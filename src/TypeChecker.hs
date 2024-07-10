{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

module TypeChecker where

import AbsLatte
import ErrM

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import qualified Data.Map as M
import Data.Maybe

--------------------------------------------------------------------------------
-- DATA TYPES
--------------------------------------------------------------------------------

data TCType = TCInt
    | TCBool
    | TCString
    | TCArray TCType
    | TCClass Ident
    | TCVoid
    | TCNull

instance Show TCType where
    show TCInt = "int"
    show TCBool = "bool"
    show TCString = "string"
    show (TCArray arrayType) = show arrayType ++ "[]"
    show (TCClass className@(Ident name)) = "class " ++ name
    show TCVoid = "void"
    show TCNull = "null"

instance Eq TCType where
    TCInt == TCInt = True
    TCBool == TCBool = True
    TCString == TCString = True
    TCArray arrayType1 == TCArray arrayType2 = arrayType1 == arrayType2
    TCClass className1 == TCClass className2 = className1 == className2
    _ == _ = False

type Loc = Int
type TCFun = (TCType, [TCType])
type Atrs = M.Map Ident TCType
type Mets = M.Map Ident TCFun

type VStore = M.Map Loc TCType -- variables
type FStore = M.Map Ident TCFun -- functions
type CStore = M.Map Ident (Ident, Atrs, Mets) -- classes
type Store = (VStore, FStore, CStore)

data ParentDef = ParentFun Ident | ParentClassMet Ident Ident | TopDef Ident

type Venv = M.Map Ident Loc
type Env = (Venv, ParentDef) -- last function / class+method ident

type TC = ExceptT String (StateT Store (Reader Env))

--------------------------------------------------------------------------------
-- HELPERS generate msg about position of error
--------------------------------------------------------------------------------

genPosInfo :: BNFC'Position -> String
genPosInfo (Just (line, column)) = "at line " ++ show line
                                ++ ", column " ++ show column ++ ": "
genPosInfo Nothing = ""

genTypeInfo :: Show t => t -> String
genTypeInfo t = "'" ++ show t ++ "'"

genTypesInfo :: Show t => [t] -> String
genTypesInfo [] = ""
genTypesInfo [t] = genTypeInfo t
genTypesInfo (t:ts) = genTypeInfo t ++ ", " ++ genTypesInfo ts

--------------------------------------------------------------------------------
-- HELPERS getters, setters
--------------------------------------------------------------------------------

getVStore :: TC VStore
getVStore = do
    (vStore, _, _) <- get
    return vStore

getFStore :: TC FStore
getFStore = do
    (_, fStore, _) <- get
    return fStore

getCStore :: TC CStore
getCStore = do
    (_, _, cStore) <- get
    return cStore

setVStore :: VStore -> TC ()
setVStore vStore = do
    (_, fStore, cStore) <- get
    put (vStore, fStore, cStore)

setFStore :: FStore -> TC ()
setFStore fStore = do
    (vStore, _, cStore) <- get
    put (vStore, fStore, cStore)

setCStore :: CStore -> TC ()
setCStore cStore = do
    (vStore, fStore, _) <- get
    put (vStore, fStore, cStore)

getFunction :: Ident -> BNFC'Position -> TC TCFun
getFunction ident@(Ident name) p = do
    fStore <- getFStore
    case M.lookup ident fStore of
        Just tcFun -> return tcFun
        Nothing -> throwError (genPosInfo p ++ "unknown function " ++ name)

addFunction :: TCType -> Ident -> [TCType] -> TC ()
addFunction tcType ident tcTypes = do
    fStore <- getFStore
    setFStore (M.insert ident (tcType, tcTypes) fStore)

getClassInfo :: Ident -> BNFC'Position -> TC (Ident, Atrs, Mets)
getClassInfo ident@(Ident name) p = do
    cStore <- getCStore
    case M.lookup ident cStore of
        Just info -> return info
        Nothing -> throwError (genPosInfo p ++ "unknown class " ++ name)

getClassParent :: Ident -> BNFC'Position -> TC Ident
getClassParent ident p = do
    (parent, _, _) <- getClassInfo ident p
    return parent

addClass :: Ident -> Ident -> Atrs -> Mets -> TC ()
addClass ident father atrs mets = do
    cStore <- getCStore
    setCStore (M.insert ident (father, atrs, mets) cStore)

addVariable :: Ident -> TCType -> BNFC'Position -> TC Env
addVariable ident tcType p = case ident of
    Ident "self" -> throwError (genPosInfo p ++ "var self is special")
    _ -> do
        vStore <- getVStore
        (vEnv, space) <- ask
        let loc = (if M.null vStore then 0 else fst (M.findMax vStore) + 1)
        setVStore (M.insert loc tcType vStore)
        return (M.insert ident loc vEnv, space)

getClassAtr :: Ident -> Ident -> BNFC'Position -> TC TCType
getClassAtr ident atr@(Ident name) p = do
    (fatherIdent, atrs, mets) <- getClassInfo ident p
    case M.lookup atr atrs of
        (Just tcType) -> return tcType
        Nothing -> case fatherIdent of
            (Ident "<class Object>") ->
                throwError (genPosInfo p ++ "no such atribute " ++ name)
            _ -> getClassAtr fatherIdent atr p

getClassMet :: Ident -> Ident -> BNFC'Position -> TC TCFun
getClassMet ident met@(Ident name) p = do
    (fatherIdent, atrs, mets) <- getClassInfo ident p
    case M.lookup met mets of
        (Just tcFun) -> return tcFun
        Nothing -> case fatherIdent of
            (Ident "<class Object>") ->
                throwError (genPosInfo p ++ "no such method " ++ name)
            _ -> getClassMet fatherIdent met p

addClassAtr :: Ident -> Ident -> TCType -> BNFC'Position -> TC ()
addClassAtr ident@(Ident name) atr tcType p = do
    cStore <- getCStore
    unLegalVoid tcType p
    case M.lookup ident cStore of
        Nothing -> throwError (genPosInfo p ++ "unknown class name " ++ name)
        (Just (fatherIdent, atrs, mets)) -> do
            case M.lookup atr atrs of
                (Just tcType) ->
                    throwError (genPosInfo p ++ "atribute already exists")
                Nothing -> setCStore (M.insert ident
                    (fatherIdent, M.insert atr tcType atrs, mets) cStore)

addClassMet :: Ident -> Ident -> TCFun -> BNFC'Position -> TC ()
addClassMet ident@(Ident name) met tcFun p = do
    cStore <- getCStore
    case M.lookup ident cStore of
        Nothing -> throwError (genPosInfo p ++ "unknown class name " ++ name)
        (Just (fatherIdent, atrs, mets)) -> do
            case M.lookup met mets of
                (Just tcFun) ->
                    throwError (genPosInfo p ++ "method aldready exists")
                Nothing -> setCStore (M.insert ident
                    (fatherIdent, atrs, M.insert met tcFun mets) cStore)

--------------------------------------------------------------------------------
-- HELPERS converters, easy checkers (try)
--------------------------------------------------------------------------------

convType :: Type -> TC TCType
convType t = case t of
    Int _ -> return TCInt
    Str _ -> return TCString
    Bool _ -> return TCBool
    TArray _ t -> do
        tcType <- convType t
        return (TCArray tcType)
    TClass _ ident -> return (TCClass ident)
    Void _ -> return TCVoid
    _ -> throwError (genPosInfo (hasPosition t) ++ "unexpected Type")

unLegalVoid :: TCType -> BNFC'Position -> TC ()
unLegalVoid tcType p = case tcType of
    TCVoid -> throwError (genPosInfo p ++ "'void' type cannot be used")
    _ -> return ()

tryFunctionName :: Ident -> FStore -> BNFC'Position -> TC ()
tryFunctionName ident@(Ident name) fStore p =
    case M.lookup ident fStore of
        Just _ -> throwError (genPosInfo p 
            ++ "multiple function's name " ++ name)
        _ -> case 'a' <= (head name) && (head name) <= 'z' of
            False -> throwError (genPosInfo p ++ "first char should be a-z")
            True -> return ()

tryClassName :: Ident -> CStore -> BNFC'Position -> TC ()
tryClassName ident@(Ident name) cStore p =
    case M.lookup ident cStore of
        Just _ -> throwError (genPosInfo p ++ "multiple class name")
        _ -> return ()

listVarUnique :: [(Ident, BNFC'Position)] -> TC ()
listVarUnique l = foldM_ (\lPref el -> do
    uniqueVarInList el lPref
    return (el:lPref)) [] l

uniqueVarInList :: (Ident, BNFC'Position) -> [(Ident, BNFC'Position)] -> TC ()
uniqueVarInList el [] = return ()
uniqueVarInList (el@(Ident name), p) ((hEl, _):t) = if el == hEl
    then throwError (genPosInfo p ++ "var duplicate " ++ name)
    else return ()

tryAncestorsDuplicate :: Ident -> Ident -> BNFC'Position -> TC ()
tryAncestorsDuplicate ident (Ident "<class Object>") p = return ()
tryAncestorsDuplicate ident identAncestor p = case ident == identAncestor of
    True -> throwError (genPosInfo p ++ "can't be ancestor itself")
    False -> do
        cStore <- getCStore
        case M.lookup identAncestor cStore of
            (Just (fatherIdent, _, _)) ->
                tryAncestorsDuplicate ident fatherIdent p
            _ -> return ()

--------------------------------------------------------------------------------
-- HELPERS init
--------------------------------------------------------------------------------

initStore :: Store
initStore = (
    M.empty,
    M.fromList [
        (Ident "printInt", (TCVoid, [TCInt])),
        (Ident "printString", (TCVoid, [TCString])),
        (Ident "error", (TCVoid, [])),
        (Ident "readInt", (TCInt, [])),
        (Ident "readString", (TCString, []))
    ],
    M.empty
    )

initEnv :: Env
initEnv = (M.empty, TopDef (Ident "<class Object>"))

buildDefs :: [Def] -> TC ()
buildDefs [] = return ()
buildDefs defs@(def:restDefs) = case def of
    DefFunc p t ident args block -> do
        fStore <- getFStore
        tryFunctionName ident fStore p
        tcType <- convType t
        tcTypes <- mapM (\(Arg _ t _) -> convType t) args
        forceIntMainSignature ident (tcType, tcTypes) p
        addFunction tcType ident tcTypes
        listVarUnique (map (\(Arg p _ ident) -> (ident, p)) args)
        buildDefs restDefs
    DefClass p ident extend features -> do
        cStore <- getCStore
        tryClassName ident cStore p
        fatherIdent <- do
            case extend of
                NoExtend _ -> return (Ident "<class Object>")
                Extend _ ident -> return ident
        addClass ident fatherIdent M.empty M.empty
        buildClass ident features
        tryAncestorsDuplicate ident fatherIdent p
        buildDefs restDefs

buildClass :: Ident -> [Feature] -> TC ()
buildClass ident [] = return ()
buildClass ident (feature:features) = case feature of
    Atribute p t [] -> buildClass ident features
    Atribute p t items@(item:rest) -> do
        tcType <- convType t
        atr <- do
            case item of -- here we cannot check expr
                (NoInit _ ident) -> return ident
                (Init _ ident _) -> return ident
        addClassAtr ident atr tcType p
        buildClass ident features
    Method p t met args _ -> do
        tcType <- convType t
        tcTypes <- mapM (\(Arg _ t _) -> convType t) args
        addClassMet ident met (tcType, tcTypes) p
        buildClass ident features
    Empty _ -> buildClass ident features

--------------------------------------------------------------------------------
-- HELPERS forcing types and other things
--------------------------------------------------------------------------------

forceIntMainExistence :: TC ()
forceIntMainExistence = do
    fStore <- getFStore
    case M.lookup (Ident "main") fStore of
        Just (TCInt, []) -> return ()
        _ -> throwError "no int main() function"

forceIntMainSignature :: Ident -> TCFun -> BNFC'Position -> TC ()
forceIntMainSignature (Ident "main") (tcType, tcTypes) p = case tcType of
    TCInt -> case tcTypes of
        [] -> return ()
        _ -> throwError (genPosInfo p ++ "main function cannot have args")
    _ -> throwError (genPosInfo p ++ "invalid main function type")
forceIntMainSignature _ _ _ = return ()

lookUpReturnInFunc :: Block -> TC Bool
lookUpReturnInFunc (Block _ []) = return False
lookUpReturnInFunc (Block p (stmt:stmts)) = do
    firstReturns <- case stmt of
        Ret _ _ -> return True
        VRet _ -> return True
        SBlock _ block -> lookUpReturnInFunc block
        While _ (ELitTrue p) stmt -> do
            mustReturn <- lookUpReturnInFunc (Block p [stmt])
            if mustReturn
                then return True
                else throwError (genPosInfo p ++ "missing return for while")
        Cond _ (ELitTrue _) stmt -> lookUpReturnInFunc (Block p [stmt])
        CondElse _ (ELitFalse p) stmt1 stmt2 -> do
            lookUpReturnInFunc (Block p [stmt2])
        CondElse _ (ELitTrue p) stmt1 stmt2 -> do
            lookUpReturnInFunc (Block p [stmt1])
        CondElse _ _ stmt1 stmt2 -> do
            maybeReturn1 <- lookUpReturnInFunc (Block p [stmt1])
            maybeReturn2 <- lookUpReturnInFunc (Block p [stmt2])
            return (maybeReturn1 && maybeReturn2)
        _ -> return False
    if firstReturns
        then return True
        else lookUpReturnInFunc (Block p stmts)

forceUniqueVarsInFunc :: [(Ident, BNFC'Position)] -> Block -> TC ()
forceUniqueVarsInFunc l (Block p []) = listVarUnique l
forceUniqueVarsInFunc l (Block p (stmt:stmts)) = do
    case stmt of
        SBlock _ block -> forceUniqueVarsInFunc [] block
        While p _ stmt -> forceUniqueVarsInFunc [] (Block p [stmt])
        Cond p _ stmt -> forceUniqueVarsInFunc [] (Block p [stmt])
        ForEach p _ _ _ stmt -> forceUniqueVarsInFunc [] (Block p [stmt])
        CondElse p _ stmt1 stmt2 -> do
            forceUniqueVarsInFunc [] (Block p [stmt1])
            forceUniqueVarsInFunc [] (Block p [stmt2])
        _ -> return ()
    newL <- case stmt of
        Decl p t items -> return (l ++
            map (\el -> case el of
               (NoInit p ident) -> (ident, p)
               (Init p ident _) -> (ident, p))
            items)
        _ -> return l
    forceUniqueVarsInFunc newL (Block p stmts)

forceTypeTCType :: TCType -> TCType -> BNFC'Position -> TC TCType
forceTypeTCType tcType forcedType p = case (forcedType, tcType) of
        (TCClass parentIdent, TCClass sonIdent) -> do
            classesMatch <- lookForAncestor sonIdent parentIdent
            if classesMatch
                then return forcedType
                else throwError (genPosInfo p ++ "classes not match")
            where
                lookForAncestor :: Ident -> Ident -> TC Bool
                lookForAncestor (Ident "<class Object>") parent = return False
                lookForAncestor son parent = if son == parent
                    then return True
                    else do
                        newSon <- getClassParent son p
                        lookForAncestor newSon parent
        _ -> if tcType == forcedType
            then return tcType
            else throwError (genPosInfo p
                            ++ "invalid type " ++ genTypeInfo tcType
                            ++ ", expected " ++ genTypeInfo forcedType)

forceExprType :: Expr -> TCType -> TC TCType
forceExprType expr forcedType = do
    t <- transExpr expr
    forceTypeTCType t forcedType (hasPosition expr)

-- absolute =, for ==,!=
forceExprOneOfTypes :: Expr -> [TCType] -> TC TCType
forceExprOneOfTypes expr forcedTypes = do
    t <- transExpr expr
    if t `elem` forcedTypes
        then return t
        else throwError (genPosInfo (hasPosition expr)
                        ++ "invalid type " ++ genTypeInfo t)

forceAppliedArgsTypes :: [Expr] -> [TCType] -> BNFC'Position -> TC ()
forceAppliedArgsTypes exprs tcTypes p = case (exprs, tcTypes) of
    ([], []) -> return ()
    (_, []) -> throwError (genPosInfo p ++ "too big number of arguments")
    ([], _) -> throwError (genPosInfo p ++ "too small number of arguments")
    (expr:exprs, forcedType:forcedTypes) -> do
        forceExprType expr forcedType
        forceAppliedArgsTypes exprs forcedTypes p
        return ()

forceExtendClassExist :: Extend -> TC ()
forceExtendClassExist (NoExtend _) = return ()
forceExtendClassExist (Extend p ident@(Ident name)) = do
    cStore <- getCStore
    case M.lookup ident cStore of
        Just _ -> return ()
        Nothing -> throwError (genPosInfo p ++ "unknown superclass " ++ name)

--------------------------------------------------------------------------------
-- TYPE CHECKER
--------------------------------------------------------------------------------

run :: Program -> Err String
run program = case runReader (runStateT (runExceptT (transProgram program))
                                                        initStore) initEnv of
    (Left err, _) -> Bad err
    (Right _, _) -> Ok "Type checker: passed."

transProgram :: Program -> TC ()
transProgram (Program p defs) = do
    -- phase 1 - init
    buildDefs defs
    forceIntMainExistence
    -- phase 2 - type checker
    transDefs defs where
        transDefs :: [Def] -> TC ()
        transDefs [] = return ()
        transDefs (def:defs) = do
            transDef def (TopDef (Ident "<class Object>"))
            transDefs defs

transDef :: Def -> ParentDef -> TC ()
transDef def parentDef = case def of
    DefFunc p t ident args block -> do
        forceUniqueVarsInFunc [] block
        tcType <- convType t
        returnOk <- lookUpReturnInFunc block
        returnOk <- case tcType of
            TCVoid -> return True
            _ -> return returnOk
        if returnOk
            then do
                env <- ask
                (venv, _) <- foldM (\tmpEnv (Arg p t ident) -> do
                        tcType <- convType t
                        unLegalVoid tcType p
                        local (const tmpEnv) (addVariable ident tcType p))
                    env args
                case parentDef of
                    (ParentClassMet cIdent ident) -> local
                        (const (venv, ParentClassMet cIdent ident))
                        (transBlock block)
                    _ -> do
                        -- TODO
                        local -- only TopDef class Object
                            (const (venv, ParentFun ident)) (transBlock block)
            else throwError (genPosInfo p ++ "missing return in function")
    DefClass _ ident extend features -> do
        forceExtendClassExist extend
        (venv, _) <- ask
        foldM_ (\tmpEnv feature -> local (const tmpEnv)
            (transFeature ident feature)) (venv, TopDef ident) features

transFeature :: Ident -> Feature -> TC Env
transFeature cIdent feature = case feature of
    Empty _ -> ask
    Atribute p t items -> do
        env <- ask
        tcType <- convType t
        unLegalVoid tcType p
        foldM (\tmpEnv item -> do
            case item of
                NoInit p ident ->
                    local (const tmpEnv) (addVariable ident tcType p)
                Init p ident expr -> do
                    forceExprType expr tcType
                    local (const tmpEnv) (addVariable ident tcType p)
            ) env items
    Method p t ident args block -> do
        transDef (DefFunc p t ident args block) (ParentClassMet cIdent ident)
        ask

transBlock :: Block -> TC ()
transBlock (Block _ stmts) = do
    env <- ask
    foldM_ (\tmpEnv stmt -> local (const tmpEnv) (transStmt stmt)) env stmts

transStmt :: Stmt -> TC Env
transStmt stmt = case stmt of

    SEmpty _ -> ask

    SBlock _ block -> do
        transBlock block
        ask

    SExp _ expr -> do
        transExpr expr
        ask

    Decl p t items -> case items of
        [] -> ask
        (var:vars) -> do
            tcType <- convType t
            unLegalVoid tcType p
            (vIdent, pv) <- case var of
                (NoInit pv ident) -> return (ident, pv)
                (Init pv ident expr) -> do
                    forceExprType expr tcType
                    return (ident, pv)
            env <- addVariable vIdent tcType pv
            local (const env) (transStmt (Decl p t vars))

    Ass _ expr1 expr2 -> do
        tcType <- transExpr expr1
        forceExprType expr2 tcType
        ask

    Incr _ expr -> do
        forceExprType expr TCInt
        ask

    Decr _ expr -> do
        forceExprType expr TCInt
        ask

    Ret p expr -> do
        env@(_, parentDef) <- ask
        tcFun <- case parentDef of
            ParentFun ident -> getFunction ident p
            ParentClassMet ident met -> getClassMet ident met p
            TopDef _ -> throwError (genPosInfo p ++ "unexpected return")
        case tcFun of
            (TCVoid, _) -> throwError (genPosInfo p ++ "only empty returns")
            (tcType, _) -> do
                forceExprType expr tcType
                ask

    VRet p -> do
        env@(_, parentDef) <- ask
        tcFun <- case parentDef of
            ParentFun ident -> getFunction ident p
            ParentClassMet ident met -> getClassMet ident met p
            TopDef _ -> throwError (genPosInfo p ++ "unexpected return")
        case tcFun of
            (TCVoid, _) -> ask
            (tcType, _) ->
                throwError (genPosInfo p ++ "empty return not match")

    While _ expr stmt -> do
        forceExprType expr TCBool
        transStmt stmt
        ask

    ForEach p t ident expr stmt -> do
        forcedType <- convType t
        array <- transExpr expr
        case array of
            TCArray tcType -> do
                forceTypeTCType tcType forcedType p
                env <- transStmt (Decl p t [NoInit p ident])
                local (const env) (transStmt stmt)
            _ -> throwError (genPosInfo (hasPosition expr) ++ "expected array")

    CondElse _ expr stmt1 stmt2 -> do
        forceExprType expr TCBool
        transStmt stmt1
        transStmt stmt2
        ask

    Cond _ expr stmt -> do
        forceExprType expr TCBool
        transStmt stmt
        ask

transExpr :: Expr -> TC TCType
transExpr expr = case expr of

    EVar p ident@(Ident name) -> case name of
        "self" -> do
            (_, parentDef) <- ask
            case parentDef of
                ParentClassMet cIdent met -> return (TCClass cIdent)
                TopDef cIdent -> return (TCClass cIdent) -- no class Object
                _ -> throwError (genPosInfo p
                                ++ "self can be called in class definition")
        _ -> do
            vStore <- getVStore
            (vEnv, _) <- ask
            case M.lookup ident vEnv of
                Nothing -> throwError (genPosInfo p ++ "var "
                                        ++ name ++ " not declared")
                Just loc -> case M.lookup loc vStore of
                    Nothing -> throwError (genPosInfo p
                                            ++ "var undef reference")
                    Just tcType -> return tcType

    -- as first check global function, then local method
    EApp p ident@(Ident name) exprs -> do
        -- check if function exists, then check
        fStore <- getFStore
        (matchedFun, tcType) <- case M.lookup ident fStore of
            Just (tcType, tcTypes) -> do
                forceAppliedArgsTypes exprs tcTypes p
                return (True, tcType)
            Nothing -> return (False, TCVoid)
        -- check if method exists, then check
        if not matchedFun
            then do
                (_, parentDef) <- ask
                case parentDef of
                    ParentClassMet pIdent met -> findMethod pIdent ident where
                        findMethod :: Ident -> Ident -> TC TCType
                        findMethod (Ident "<class Object>") met =
                            throwError (genPosInfo p
                                        ++ "unknown function/method " ++ name)
                        findMethod ident met = do
                            cStore <- getCStore
                            case M.lookup ident cStore of
                                Nothing -> throwError (genPosInfo p
                                        ++ "unknown function/method " ++ name)
                                Just (fIdent, atrs, mets) -> 
                                    case M.lookup met mets of
                                        Nothing -> findMethod fIdent met
                                        Just (tcType, tcTypes) -> do
                                            forceAppliedArgsTypes 
                                                exprs tcTypes p
                                            return tcType
                    _ -> throwError (genPosInfo p
                                    ++ "unknown function/method " ++ name)
            else return tcType

    EArrayVal p expr1 expr2 -> do
        forceExprType expr2 TCInt
        tcType <- transExpr expr1
        case tcType of
            TCArray arrayType -> return arrayType
            _ -> throwError (genPosInfo p ++ "expected array")

    EFeature p expr1 expr2 -> do
        tcType <- transExpr expr1
        case tcType of
            TCClass ident -> do
                (fatherIdent, atr, met) <- getClassInfo ident p
                case expr2 of
                    (EVar p1 atr) -> getClassAtr ident atr p1
                    (EApp p1 met exprs) -> do
                        (tcType, tcTypes) <- getClassMet ident met p1
                        forceAppliedArgsTypes exprs tcTypes p1
                        return tcType
                    _ -> throwError (genPosInfo p ++ "expected class feature")
            TCArray _ -> do
                case expr2 of
                    (EVar p1 var) -> case var of
                        (Ident "length") -> return TCInt
                        _ -> throwError (genPosInfo p ++ "need array atribute")
                    _ -> throwError (genPosInfo p ++ "unknown array feature")
            _ -> throwError (genPosInfo p ++ "uknown feature")

    ELitInt p integer -> do
        if integer > 2147483647
            then throwError (genPosInfo p ++ "too big number for 'int'")
            else return TCInt

    ELitTrue _ -> return TCBool

    ELitFalse _ -> return TCBool

    ELitString _ string -> return TCString

    ELitNull _ -> return TCNull -- no match without cast

    Cast p exprClassIdentExpected expr -> do
        case exprClassIdentExpected of 
            EVar p ident -> do
                getClassInfo ident p
                case expr of
                    (ELitNull _) -> return (TCClass ident)
                    _ -> forceExprType expr (TCClass ident)
            _ -> throwError (genPosInfo p ++ "class name expected")

    NewArray p t expr -> do
        tcType <- convType t
        case tcType of
            TCVoid -> throwError (genPosInfo p ++ "unexpected 'void' table")
            _ -> return (TCArray tcType)

    NewClass p ident -> do
        getClassInfo ident p
        return (TCClass ident)

    Neg _ expr -> forceExprType expr TCInt

    Not _ expr -> forceExprType expr TCBool

    EMul _ expr1 mulop expr2 -> do
        forceExprType expr1 TCInt
        forceExprType expr2 TCInt
        return TCInt

    EAdd _ expr1 addop expr2 -> do
        case addop of
            Plus _ -> do
                tcType <- forceExprOneOfTypes expr1 [TCInt, TCString]
                forceExprType expr2 tcType
                return tcType
            _ -> do
                forceExprType expr1 TCInt
                forceExprType expr2 TCInt
                return TCInt

    ERel _ expr1 relop expr2 -> do
        case relop of
            EQU _ -> do
                tcType <- transExpr expr1
                forceExprOneOfTypes expr2 [tcType]
                return TCBool
            NE _ -> do
                tcType <- transExpr expr1
                forceExprOneOfTypes expr2 [tcType]
                return TCBool
            _ -> do
                forceExprType expr1 TCInt
                forceExprType expr2 TCInt
                return TCBool

    EAnd _ expr1 expr2 -> do
        forceExprType expr1 TCBool
        forceExprType expr2 TCBool
        return TCBool

    EOr _ expr1 expr2 -> do
        forceExprType expr1 TCBool
        forceExprType expr2 TCBool
        return TCBool
