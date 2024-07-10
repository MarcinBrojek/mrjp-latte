{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

module LLVMCompiler where

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

data LLType = LLInt
    | LLBool
    | LLString
    | LLArray LLType
    | LLClass Ident
    | LLVoid
    | LLNull
    | LLPtr LLType -- only used for show

-- there are used registers:
-- %var-[var id]-[nr] for existing variables
-- %tmp-[nr] for temporar values from expressions
-- %class-[class name] - pointer to struct type (declared once at the begin)
-- %array-[type] - like %array-i32, pointer ti struct type {[type], size:i32}
-- %string - pointer to struct type {i8*, size:i32}

convToObject :: LLType -> LLType
convToObject llType = case llType of
    LLInt -> LLInt
    LLBool -> LLBool
    LLString -> LLPtr LLString
    LLArray llType -> LLPtr (LLArray llType)
    LLClass ident -> LLPtr (LLClass ident)
    LLVoid -> LLVoid
    LLNull -> LLNull
    LLPtr llType -> LLPtr llType

instance Show LLType where
    show LLInt = "i32"
    show LLBool = "i1"
    show LLString = genStringTypeName
    show (LLArray arrType) = genArrayTypeName arrType
    show (LLClass ident) = genClassTypeName ident
    show LLVoid = "void"
    show LLNull = "null"
    show (LLPtr llType) = show llType ++ "*"

instance Eq LLType where
    LLInt == LLInt = True
    LLBool == LLBool = True
    LLString == LLString = True
    LLArray arrayType1 == LLArray arrayType2 = arrayType1 == arrayType2
    LLClass className1 == LLClass className2 = className1 == className2
    _ == _ = False

type Loc = Int
type LLFun = (LLType, [LLType])
type Atrs = M.Map Ident LLType
type Mets = M.Map Ident LLFun

type VStore = M.Map Loc LLType -- variables
type FStore = M.Map Ident LLFun -- functions
type CStore = M.Map Ident (Ident, Atrs, Mets) -- classes

-- last function / class+method ident
data ParentDef = ParentFun Ident | ParentClassMet Ident Ident | TopDef Ident
type Venv = M.Map Ident Loc

type Reg = String
type Timestamp = Int
type RStore = M.Map Loc (Reg, Timestamp)

type LLInstrs = [LLInstr]
type LLGlobIntrs = [LLInstr] -- for global strings
type IdCounter = Int -- for regs, tmp regs and blocks - field [nr]

type Store = (VStore, FStore, CStore, RStore, LLInstrs, LLGlobIntrs, IdCounter)
type Env = (Venv, ParentDef)
type LL = ExceptT String (StateT Store (Reader Env))

type VarRTMap = (M.Map Ident (Reg, Timestamp)) -- helper type for while, cond

--------------------------------------------------------------------------------
-- HELPERS generate msg about position of error
--------------------------------------------------------------------------------

genPosInfo :: BNFC'Position -> String
genPosInfo (Just (line, column)) = "at line " ++ show line
                                ++ ", column " ++ show column ++ ": "
genPosInfo Nothing = ""

genTypeInfo :: LLType -> String
genTypeInfo t = "'" ++ showTCType t ++ "'" where
    showTCType :: LLType -> String
    showTCType llType = case llType of
        LLInt -> "int"
        LLBool -> "bool"
        LLString -> "string"
        (LLArray arrayType) -> showTCType arrayType ++ "[]"
        (LLClass className@(Ident name)) -> "class " ++ name
        LLVoid -> "void"
        _ -> "null"

genEmptyBNFCpos :: BNFC'Position
genEmptyBNFCpos = Nothing

--------------------------------------------------------------------------------
-- HELPERS Instrs
--------------------------------------------------------------------------------

data LLInstr =
      LLUndefLine String -- for some not listed below
    | LLLabel String
    | LLCallFunc Reg LLType Ident [LLType] [Reg]
    | LLCallProc Ident [LLType] [Reg]
    | LLCallMethodFunc Reg LLType Ident Ident [LLType] [Reg]
    | LLCallMethodProc Ident Ident [LLType] [Reg]
    | LLMul Reg Reg Reg
    | LLNot Reg Reg
    | LLDiv Reg Reg Reg
    | LLMod Reg Reg Reg
    | LLAdd Reg Reg Reg
    | LLSub Reg Reg Reg
    | LLOr Reg Reg Reg
    | LLAnd Reg Reg Reg
    | LLLTH Reg Reg Reg
    | LLLE Reg Reg Reg
    | LLGTH Reg Reg Reg
    | LLGE Reg Reg Reg
    | LLEQU Reg LLType Reg Reg
    | LLNE Reg LLType Reg Reg
    | LLStore LLType Reg Reg
    | LLLoad Reg LLType Reg
    | LLGetElPtr Reg LLType Reg Reg
    | LLGetElPtrAtr Reg LLType Reg Reg
    | LLDefineFunc LLType Ident [LLType] [Reg]
    | LLDefineMethod LLType Ident Ident [LLType] [Reg]
    | LLDefEnd
    | LLNewType Reg [Reg]
    | LLAlloca Reg LLType
    | LLAllocaArr Reg LLType Reg
    | LLBitcast Reg LLType Reg LLType
    | LLStrDef String Reg String
    | LLGoto String
    | LLPhi Reg LLType [Reg] [String]
    | LLVRet
    | LLRet LLType Reg
    | LLBr Reg String String
    | LLDeclExtFunc LLType Ident [LLType]
    | LLMain String
    | LLDeclMalloc
    | LLi32i64 Reg Reg

instance Show LLInstr where
    show (LLUndefLine s) = s
    show (LLLabel s) = s ++ ":"
    show (LLCallFunc r l f ls rs) = r ++ " = call " ++ show l ++ " " ++ genFunctionName f ++ "(" ++ genArgList ls rs  ++ ")"
    show (LLCallProc f ls rs) = "call void " ++ genFunctionName f ++ "(" ++ genArgList ls rs ++ ")"
    show (LLCallMethodFunc r l c m ls rs) = r ++ " = call " ++ show l ++ " " ++ genClassMethodName c m ++ "(" ++ genArgList ls rs ++ ")"
    show (LLCallMethodProc c m ls rs) = "call void " ++ genClassMethodName c m  ++ "(" ++ genArgList ls rs ++ ")"
    show (LLMul r1 r2 r3) = r1 ++ " = mul i32 " ++ r2 ++ ", " ++ r3
    show (LLNot r1 r2) = r1 ++ " = icmp eq i1 " ++ r2 ++ ", 0"
    show (LLDiv r1 r2 r3) = r1 ++ " = sdiv i32 " ++ r2 ++ ", " ++ r3
    show (LLMod r1 r2 r3) = r1 ++ " = srem i32 " ++ r2 ++ ", " ++ r3
    show (LLAdd r1 r2 r3) = r1 ++ " = add i32 " ++ r2 ++ ", " ++ r3
    show (LLSub r1 r2 r3) = r1 ++ " = sub i32 " ++ r2 ++ ", " ++ r3
    show (LLOr r1 r2 r3) = r1 ++ " = or i1 " ++ r2 ++ ", " ++ r3
    show (LLAnd r1 r2 r3) = r1 ++ " = and i1 " ++ r2 ++ ", " ++ r3
    show (LLLTH r1 r2 r3) = r1 ++ " = icmp slt i32 " ++ r2 ++ ", " ++ r3
    show (LLLE r1 r2 r3) = r1 ++ " = icmp sle i32 " ++ r2 ++ ", " ++ r3
    show (LLGTH r1 r2 r3) = r1 ++ " = icmp sgt i32 " ++ r2 ++ ", " ++ r3
    show (LLGE r1 r2 r3) = r1 ++ " = icmp sge i32 " ++ r2 ++ ", " ++ r3
    show (LLEQU r1 l r2 r3) = r1 ++ " = icmp eq " ++ show l ++ " " ++ r2 ++ ", " ++ r3
    show (LLNE r1 l r2 r3) = r1 ++ " = icmp ne " ++ show l ++ " " ++ r2 ++ ", " ++ r3
    show (LLStore l r1 r2) = "store " ++ show l ++ " " ++ r1 ++ ", " ++ show (LLPtr l) ++ " " ++ r2
    show (LLLoad r1 l r2) = r1 ++ " = load " ++ show l ++ ", " ++ show (LLPtr l) ++ " " ++ r2
    show (LLGetElPtr r1 l r2 r3) = r1 ++ " = getelementptr " ++ show l ++ ", " ++ show (LLPtr l) ++ " " ++ r2 ++ ", i32 " ++ r3
    show (LLGetElPtrAtr r1 l r2 r3) = r1 ++ " = getelementptr " ++ show l ++ ", " ++ show (LLPtr l) ++ " " ++ r2 ++ ", i32 0, i32 " ++ r3
    show (LLDefineFunc l f ls rs) = "define " ++ show l ++ " " ++ genFunctionName f ++ "(" ++ genArgList ls rs ++ ") {"
    show (LLDefineMethod l c m ls rs) = "define " ++ show l ++ " " ++ genClassMethodName c m ++ "(" ++ genArgList ls rs ++ ") {"
    show LLDefEnd = "}"
    show (LLNewType r rs) = r ++ " = type { " ++ genRegList rs ++ " }"
    
    show (LLAlloca r l) = r ++ " = alloca " ++ show l
    show (LLAllocaArr r1 l r2) = r1 ++ " = alloca " ++ show l ++ ", i32 " ++ r2

    show (LLBitcast r1 l1 r2 l2) = r1 ++ " = bitcast " ++ show l1 ++ " " ++ r2 ++ " to " ++ show l2
    show (LLStrDef g size s) = "@" ++ g ++ " = private unnamed_addr constant [" ++ size ++ " x i8] c\"" ++ s ++ "\\00\""
    show (LLGoto s) = "br label %" ++ s
    show (LLPhi r l rs ss) = r ++ " = phi " ++ show l ++ " " ++ safeTail (safeTail (concatMap (\(r, s) -> ", [ " ++ r ++ ", %" ++ s ++ " ]" ) (zip rs ss)))
    show LLVRet = "ret void"
    show (LLRet l r) = "ret " ++ show l ++ " " ++ r
    show (LLBr r lt lf) = "br i1 " ++ r ++ ", label %" ++ lt ++ ", label %" ++ lf
    show (LLDeclExtFunc l f ls) = "declare " ++ show l ++ genFunctionName f ++ "(" ++ safeTail (safeTail (concatMap (\l -> ", " ++ show l ) ls)) ++ ")"
    show (LLMain s) = s
    show LLDeclMalloc = "declare i8* @malloc(i64)"
    show (LLi32i64 r1 r2) = r1 ++ " = sext i32 " ++ r2 ++ " to i64"

showIdent :: Ident -> String
showIdent ident@(Ident name) = name

addInstr :: LLInstr -> LL ()
addInstr instr = do
    instrs <- getLLInstrs
    setLLInstrs (instr:instrs)

addGlobInstr :: LLInstr -> LL ()
addGlobInstr globInstr = do
    globInstrs <- getLLGlobInstrs
    setLLGlobInstrs (globInstr:globInstrs)

-- instrs not in reverse
addInstrs :: LLInstrs -> LL ()
addInstrs instrs = do
    lastInstrs <- getLLInstrs
    setLLInstrs (concatInstrstoReverseInstrs instrs lastInstrs) where
        concatInstrstoReverseInstrs :: LLInstrs -> LLInstrs -> LLInstrs
        concatInstrstoReverseInstrs [] instrs = instrs
        concatInstrstoReverseInstrs (instr:instrs) lastInstrs =
            concatInstrstoReverseInstrs instrs (instr:lastInstrs)

-- instrs not in reverse, without that label
takeInsUntilLabel :: String -> LL LLInstrs
takeInsUntilLabel label = do
    lastInstrs <- getLLInstrs
    (instrs, newLastInstrs) <- takeHelper label [] lastInstrs
    setLLInstrs newLastInstrs
    return instrs where
        takeHelper :: String -> LLInstrs -> LLInstrs -> LL (LLInstrs, LLInstrs)
        takeHelper l1 instrs lastInstrs = case lastInstrs of
            [] -> return (instrs, [])
            (lastInstr:rest) -> case lastInstr of
                (LLLabel l2) -> if l1 == l2
                    then return (instrs, rest)
                    else takeHelper l1 (lastInstr:instrs) rest
                _ -> takeHelper l1 (lastInstr:instrs) rest

addUnconditionalBrToLabels :: LL ()
addUnconditionalBrToLabels = do
    revInstrs <- getLLInstrs
    let instrs = foldl (\acc instr -> case instr of
            (LLLabel l) -> (LLGoto l):instr:acc
            _ -> instr:acc) [] revInstrs
    setLLInstrs (reverse instrs)

--------------------------------------------------------------------------------
-- HELPERS getters, setters on Store
--------------------------------------------------------------------------------

getVStore :: LL VStore
getVStore = do
    (vStore, _, _, _, _, _, _) <- get
    return vStore

getFStore :: LL FStore
getFStore = do
    (_, fStore, _, _, _, _, _) <- get
    return fStore

getCStore :: LL CStore
getCStore = do
    (_, _, cStore, _, _, _, _) <- get
    return cStore

getRStore :: LL RStore
getRStore = do
    (_, _, _, rStore, _, _, _) <- get
    return rStore

getLLInstrs :: LL LLInstrs
getLLInstrs = do
    (_, _, _, _, instrs, _, _) <- get
    return instrs

getLLGlobInstrs :: LL LLInstrs
getLLGlobInstrs = do
    (_, _, _, _, _, globInstrs, _) <- get
    return globInstrs

getIdCounter :: LL IdCounter
getIdCounter = do
    (_, _, _, _, _, _, idCounter) <- get
    return idCounter

setVStore :: VStore -> LL ()
setVStore vStore = do
    (_, fStore, cStore, rStore, instrs, globInstrs, idCounter) <- get
    put (vStore, fStore, cStore, rStore, instrs, globInstrs, idCounter)

setFStore :: FStore -> LL ()
setFStore fStore = do
    (vStore, _, cStore, rStore, instrs, globInstrs, idCounter) <- get
    put (vStore, fStore, cStore, rStore, instrs, globInstrs, idCounter)

setCStore :: CStore -> LL ()
setCStore cStore = do
    (vStore, fStore, _, rStore, instrs, globInstrs, idCounter) <- get
    put (vStore, fStore, cStore, rStore, instrs, globInstrs, idCounter)

setRStore :: RStore -> LL ()
setRStore rStore = do
    (vStore, fStore, cStore, _, instrs, globInstrs, idCounter) <- get
    put (vStore, fStore, cStore, rStore, instrs, globInstrs, idCounter)

setLLInstrs :: LLInstrs -> LL ()
setLLInstrs instrs = do
    (vStore, fStore, cStore, rStore, _, globInstrs, idCounter) <- get
    put (vStore, fStore, cStore, rStore, instrs, globInstrs, idCounter)

setLLGlobInstrs :: LLInstrs -> LL ()
setLLGlobInstrs globInstrs = do
    (vStore, fStore, cStore, rStore, instrs, _, idCounter) <- get
    put (vStore, fStore, cStore, rStore, instrs, globInstrs, idCounter)

setIdCounter :: IdCounter -> LL ()
setIdCounter idCounter = do
    (vStore, fStore, cStore, rStore, instrs, globInstrs, _) <- get
    put (vStore, fStore, cStore, rStore, instrs, globInstrs, idCounter)

--------------------------------------------------------------------------------
-- HELPERS getters for type names in llvm
--------------------------------------------------------------------------------

safeTail :: [a] -> [a]
safeTail sequence = case sequence of
    [] -> []
    (h:t) -> t

genStringTypeName :: Reg
genStringTypeName = "%string"

genClassTypeName :: Ident -> Reg
genClassTypeName ident = "%class-" ++ showIdent ident

genArrayTypeName :: LLType -> Reg
genArrayTypeName llType = "%array-" ++ case llType of
    LLClass ident -> safeTail (genClassTypeName ident)
    _ -> show llType

genFunctionName :: Ident -> String
genFunctionName ident = "@function_" ++ showIdent ident

genClassMethodName :: Ident -> Ident -> String
genClassMethodName identClassName identMetName =
    "@classMethod." ++ showIdent identClassName ++ "." ++ showIdent identMetName

genLabelName :: String -> IdCounter -> String
genLabelName label num = "label-" ++ label ++ "-" ++ show num

genArgList :: [LLType] -> [Reg] -> String -- like i32 x, i8 y, ..., i10 c
genArgList ls rs = safeTail (safeTail
    (concatMap (\(l, r) -> ", " ++ show l ++ " " ++ r) (zip ls rs)))

genRegList :: [Reg] -> String -- like r1, r2, r3
genRegList rs = safeTail (safeTail(concatMap (", " ++) rs))

--------------------------------------------------------------------------------
-- HELPERS getters, setters for registers, for llvm
--------------------------------------------------------------------------------

getNewId :: LL IdCounter
getNewId = do
    idCounter <- getIdCounter
    setIdCounter (idCounter + 1)
    return idCounter

setVarReg :: Ident -> Reg -> LL ()
setVarReg ident reg = do
    (venv, _) <- ask
    rStore <- getRStore
    newId <- getNewId
    let loc = fromMaybe 0 (M.lookup ident venv)
    setRStore (M.insert loc (reg, newId) rStore)

getVarReg :: Ident -> LL Reg
getVarReg ident = do
    (venv, _) <- ask
    rStore <- getRStore
    let loc = fromMaybe 0 (M.lookup ident venv)
    let (reg, timestamp) = fromMaybe ("WOLOLOLO", -1) (M.lookup loc rStore)
    return reg 

getNewVarReg :: Ident -> LL Reg
getNewVarReg ident@(Ident name) = do
    newId <- getNewId
    return ("%var-" ++ name ++ "-" ++ show newId)

getVarRegWithId :: Ident -> IdCounter -> LL Reg
getVarRegWithId ident@(Ident name) num = do
    return ("%var-" ++ name ++ "-" ++ show num)

getNewTmpReg :: LL Reg
getNewTmpReg = do
    newId <- getNewId
    return ("%tmp-" ++ show newId)

addVariableWithReg :: Ident -> LLType -> Reg -> BNFC'Position -> LL Env
addVariableWithReg ident llType reg p = case ident of
    Ident "self" -> throwError (genPosInfo p ++ "var self is special")
    _ -> do
        vStore <- getVStore
        rStore <- getRStore
        (venv, space) <- ask
        let loc = (if M.null vStore then 0 else fst (M.findMax vStore) + 1)
        setVStore (M.insert loc llType vStore)
        newId <- getNewId
        setRStore (M.insert loc (reg, newId) rStore)
        return (M.insert ident loc venv, space)

addSelfToVenv :: LLType -> Reg -> LL Venv
addSelfToVenv llType reg = do
    vStore <- getVStore
    rStore <- getRStore
    (venv, space) <- ask
    let loc = (if M.null vStore then 0 else fst (M.findMax vStore) + 1)
    setVStore (M.insert loc llType vStore)
    newId <- getNewId
    setRStore (M.insert loc (reg, newId) rStore)
    return (M.insert (Ident "self") loc venv)

getInitRegInsideStructure :: LLType -> LL Reg
getInitRegInsideStructure llType = case llType of
    LLInt -> return "0"
    LLBool -> return "false"
    LLString -> do
        strReg <- allocaObject LLString
        -- addInstr (LLAlloca strReg LLString)
        sizeReg <- getNewTmpReg
        addInstr (LLGetElPtrAtr sizeReg LLString strReg "1")
        addInstr (LLStore LLInt "0" sizeReg)
        return strReg
    LLArray _ -> return "null"
    LLClass _ -> return "null"
    _ -> getNewTmpReg

-- expected existing ident of variable
getVarLLType :: Ident -> LL LLType
getVarLLType ident = do
    (venv, _) <- ask
    vStore <- getVStore
    return (fromMaybe LLNull (M.lookup
        (fromMaybe 0 (M.lookup ident venv)) vStore))

--------------------------------------------------------------------------------
-- HELPERS getters, setters for typechecker
--------------------------------------------------------------------------------

getFunction :: Ident -> BNFC'Position -> LL LLFun
getFunction ident@(Ident name) p = do
    fStore <- getFStore
    case M.lookup ident fStore of
        Just llFun -> return llFun
        Nothing -> throwError (genPosInfo p ++ "unknown function " ++ name)

addFunction :: LLType -> Ident -> [LLType] -> LL ()
addFunction llType ident llTypes = do
    fStore <- getFStore
    setFStore (M.insert ident (llType, llTypes) fStore)

getClassInfo :: Ident -> BNFC'Position -> LL (Ident, Atrs, Mets)
getClassInfo ident@(Ident name) p = do
    cStore <- getCStore
    case M.lookup ident cStore of
        Just info -> return info
        Nothing -> throwError (genPosInfo p ++ "unknown class " ++ name)

getClassParent :: Ident -> BNFC'Position -> LL Ident
getClassParent ident p = do
    (parent, _, _) <- getClassInfo ident p
    return parent

addClass :: Ident -> Ident -> Atrs -> Mets -> LL ()
addClass ident father atrs mets = do
    cStore <- getCStore
    setCStore (M.insert ident (father, atrs, mets) cStore)

addVariable :: Ident -> LLType -> BNFC'Position -> LL Env
addVariable ident llType p = case ident of
    Ident "self" -> throwError (genPosInfo p ++ "var self is special")
    _ -> do
        vStore <- getVStore
        (venv, space) <- ask
        let loc = (if M.null vStore then 0 else fst (M.findMax vStore) + 1)
        setVStore (M.insert loc llType vStore)
        return (M.insert ident loc venv, space)

getClassAtr :: Ident -> Ident -> BNFC'Position -> LL LLType
getClassAtr ident atr@(Ident name) p = do
    (fatherIdent, atrs, mets) <- getClassInfo ident p
    case M.lookup atr atrs of
        (Just llType) -> return llType
        Nothing -> case fatherIdent of
            (Ident "<class Object>") ->
                throwError (genPosInfo p ++ "no such atribute " ++ name)
            _ -> getClassAtr fatherIdent atr p

getClassMet :: Ident -> Ident -> BNFC'Position -> LL (Ident, LLFun)
getClassMet ident met@(Ident name) p = do
    (fatherIdent, atrs, mets) <- getClassInfo ident p
    case M.lookup met mets of
        (Just llFun) -> return (ident, llFun)
        Nothing -> case fatherIdent of
            (Ident "<class Object>") ->
                throwError (genPosInfo p ++ "no such method " ++ name)
            _ -> getClassMet fatherIdent met p

addClassAtr :: Ident -> Ident -> LLType -> BNFC'Position -> LL ()
addClassAtr ident@(Ident name) atr llType p = do
    cStore <- getCStore
    unLegalVoid llType p
    case M.lookup ident cStore of
        Nothing -> throwError (genPosInfo p ++ "unknown class name " ++ name)
        (Just (fatherIdent, atrs, mets)) -> do
            case M.lookup atr atrs of
                (Just _) ->
                    throwError (genPosInfo p ++ "atribute already exists")
                Nothing -> setCStore (M.insert ident
                    (fatherIdent, M.insert atr llType atrs, mets) cStore)

addClassMet :: Ident -> Ident -> LLFun -> BNFC'Position -> LL ()
addClassMet ident@(Ident name) met llFun p = do
    cStore <- getCStore
    case M.lookup ident cStore of
        Nothing -> throwError (genPosInfo p ++ "unknown class name " ++ name)
        (Just (fatherIdent, atrs, mets)) -> do
            case M.lookup met mets of
                (Just _) ->
                    throwError (genPosInfo p ++ "method aldready exists")
                Nothing -> setCStore (M.insert ident
                    (fatherIdent, atrs, M.insert met llFun mets) cStore)

--------------------------------------------------------------------------------
-- HELPERS converters, easy checkers (try) for typechecker
--------------------------------------------------------------------------------

convType :: Type -> LL LLType
convType t = case t of
    Int _ -> return LLInt
    Str _ -> return LLString
    Bool _ -> return LLBool
    TArray _ t -> do
        llType <- convType t
        return (LLArray llType)
    TClass _ ident -> return (LLClass ident)
    Void _ -> return LLVoid
    _ -> throwError (genPosInfo (hasPosition t) ++ "unexpected Type")

unLegalVoid :: LLType -> BNFC'Position -> LL ()
unLegalVoid llType p = case llType of
    LLVoid -> throwError (genPosInfo p ++ "'void' type cannot be used")
    _ -> return ()

tryFunctionName :: Ident -> FStore -> BNFC'Position -> LL ()
tryFunctionName ident@(Ident name) fStore p =
    case M.lookup ident fStore of
        Just _ -> throwError (genPosInfo p
            ++ "multiple function's name " ++ name)
        _ -> case 'a' <= (head name) && (head name) <= 'z' of
            False -> throwError (genPosInfo p ++ "first char should be a-z")
            True -> return ()

tryClassName :: Ident -> CStore -> BNFC'Position -> LL ()
tryClassName ident@(Ident name) cStore p =
    case M.lookup ident cStore of
        Just _ -> throwError (genPosInfo p ++ "multiple class name")
        _ -> return ()

listVarUnique :: [(Ident, BNFC'Position)] -> LL ()
listVarUnique l = foldM_ (\lPref el -> do
    uniqueVarInList el lPref
    return (el:lPref)) [] l

uniqueVarInList :: (Ident, BNFC'Position) -> [(Ident, BNFC'Position)] -> LL ()
uniqueVarInList el [] = return ()
uniqueVarInList (el@(Ident name), p) ((hEl, _):t) = if el == hEl
    then throwError (genPosInfo p ++ "var duplicate " ++ name)
    else return ()

tryAncestorsDuplicate :: Ident -> Ident -> BNFC'Position -> LL ()
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
-- HELPERS for llvm
--------------------------------------------------------------------------------

-- save variables' registers out of block
genMapOfVarRT :: LL VarRTMap
genMapOfVarRT = do
    (venv, _) <- ask
    rStore <- getRStore
    return (M.map(\loc -> fromMaybe ("", -1) (M.lookup loc rStore)) venv)

-- regain variables' registers out of block
regainMapOfVarRegs :: VarRTMap -> LL ()
regainMapOfVarRegs varRTMap = do
    (venv, _) <- ask
    rStore <- getRStore
    setRStore (M.foldrWithKey (\ident rt acc -> M.insert
        (fromMaybe 0 (M.lookup ident venv)) rt acc) rStore varRTMap)

diffOfVarRegMaps :: VarRTMap -> VarRTMap -> M.Map Ident (Reg, Reg)
diffOfVarRegMaps varRTMap1 varRTMap2 =
    M.foldrWithKey (\ident (reg1, id1) acc ->
        case M.lookup ident varRTMap2 of
            Just (reg2, id2) -> if (reg1 == reg2) && (id1 == id2)
                then acc
                else M.insert ident (reg1, reg2) acc
            _ -> acc
        ) M.empty varRTMap1

-- atrs can be duplicated, from few ancestors
getListOfAtrsForClass :: Ident -> [(Ident, LLType)] -> LL [(Ident, LLType)]
getListOfAtrsForClass ident lAtrs = case ident of
    (Ident "<class Object>") -> return lAtrs
    _ -> do
        (fIdent, atrs, _) <- getClassInfo ident genEmptyBNFCpos
        getListOfAtrsForClass fIdent (M.assocs atrs ++ lAtrs)

getClassAtrNumber :: Ident -> Ident -> LL Int
getClassAtrNumber cIdent atrIdent = do
    lAtrs <- getListOfAtrsForClass cIdent []
    lAtrsIdents <- mapM (\(ident, _) -> return ident) lAtrs
    (_, index) <- foldM (\(b, ind) ident -> if b || (ident == atrIdent)
            then return (True, ind)
            else return (False, ind - 1))
        (False, length lAtrs - 1) (reverse lAtrsIdents)
    return index

allocaObject :: LLType -> LL Reg
allocaObject llType = do
    notCasted <- getNewTmpReg
    casted <- getNewTmpReg
    memory <- getNewTmpReg
    castedMemory <- getNewTmpReg

    addInstr(LLUndefLine (notCasted ++ " = getelementptr " ++ show llType ++ ", " ++ show (LLPtr llType) ++ " null, i32 1"))
    addInstr(LLUndefLine (casted ++ " = ptrtoint " ++ show (LLPtr llType) ++ " " ++ notCasted ++ " to i64"))
    addInstr(LLUndefLine (memory ++ " = call i8* @malloc(i64 " ++ casted ++ ")"))
    addInstr(LLUndefLine (castedMemory ++ " = bitcast i8* " ++ memory ++ " to " ++ show (LLPtr llType)))

    return castedMemory
    
allocaArray :: LLType -> Reg -> LL Reg
allocaArray llType size = do
    notCasted <- getNewTmpReg
    casted <- getNewTmpReg
    i64Size <- getNewTmpReg
    castedXSize <- getNewTmpReg
    memory <- getNewTmpReg
    castedMemory <- getNewTmpReg

    addInstr(LLUndefLine (notCasted ++ " = getelementptr " ++ show llType ++ ", " ++ show (LLPtr llType) ++ " null, i32 1"))
    addInstr(LLUndefLine (casted ++ " = ptrtoint " ++ show (LLPtr llType) ++ " " ++ notCasted ++ " to i64"))
    addInstr(LLi32i64 i64Size size)
    addInstr(LLUndefLine (castedXSize ++ " = mul i64 " ++ casted ++ ", " ++ i64Size))
    addInstr(LLUndefLine (memory ++ " = call i8* @malloc(i64 " ++ castedXSize ++ ")"))
    addInstr(LLUndefLine (castedMemory ++ " = bitcast i8* " ++ memory ++ " to " ++ show (LLPtr llType)))

    return castedMemory

convBVal :: (LLType, Reg) -> Bool
convBVal (LLBool, reg) = reg == "true"

convValB :: Bool -> (LLType, Reg)
convValB b = case b of
    True -> (LLBool, "true")
    False -> (LLBool, "false")

convIVal :: (LLType, Reg) -> Int
convIVal (LLInt, reg) = read reg::Int

convValI :: Int -> (LLType, Reg)
convValI i = (LLInt, show i)

isRegLit :: Reg -> Bool
isRegLit reg = head reg /= '%'

--------------------------------------------------------------------------------
-- HELPERS init
--------------------------------------------------------------------------------

initStore :: Store
initStore = (
    M.empty, 
    M.fromList [
        (Ident "printInt", (LLVoid, [LLInt])),
        (Ident "printString", (LLVoid, [LLString])),
        (Ident "error", (LLVoid, [])),
        (Ident "readInt", (LLInt, [])),
        (Ident "readString", (LLString, []))
    ], 
    M.empty, 
    M.empty, 
    [], 
    [], 
    0)

initEnv :: Env
initEnv = (M.empty, TopDef (Ident "<class Object>"))

buildDefs :: [Def] -> LL ()
buildDefs [] = return ()
buildDefs defs@(def:restDefs) = case def of
    DefFunc p t ident args block -> do
        fStore <- getFStore
        tryFunctionName ident fStore p
        llType <- convType t
        llTypes <- mapM (\(Arg _ t _) -> convType t) args
        forceIntMainSignature ident (llType, llTypes) p
        addFunction llType ident llTypes
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

buildClass :: Ident -> [Feature] -> LL ()
buildClass ident [] = return ()
buildClass ident (feature:features) = case feature of
    Atribute p t [] -> buildClass ident features
    Atribute p t items@(item:rest) -> do
        llType <- convType t
        atr <- do
            case item of -- here we cannot check expr
                (NoInit _ ident) -> return ident
                (Init _ ident _) -> return ident
        addClassAtr ident atr llType p
        buildClass ident features
    Method p t met args _ -> do
        llType <- convType t
        llTypes <- mapM (\(Arg _ t _) -> convType t) args
        addClassMet ident met (llType, llTypes) p
        buildClass ident features
    Empty _ -> buildClass ident features

genAllClassesAndArraysDefinitions :: LL ()
genAllClassesAndArraysDefinitions = do
    cStore <- getCStore
    addGlobInstr (LLNewType (genArrayTypeName LLInt) ["i32*", "i32"])
    addGlobInstr (LLNewType (genArrayTypeName LLBool) ["i1*", "i32"])
    addGlobInstr (LLNewType genStringTypeName ["i8*", "i32"])
    mapM_ addDefOfClass (M.keys cStore) where
        addDefOfClass :: Ident -> LL ()
        addDefOfClass ident = do
            listOfAtrs <- getListOfAtrsForClass ident []
            addGlobInstr
                (LLNewType (genClassTypeName ident) (map (show . convToObject . snd) listOfAtrs))
            addGlobInstr (LLNewType (genArrayTypeName (LLClass ident))
                [genClassTypeName ident ++ "*", "i32"])

genAllPredefinedFunctions :: LL ()
genAllPredefinedFunctions = do
    addGlobInstr (LLDeclExtFunc LLVoid (Ident "printInt") [LLInt])
    addGlobInstr (LLDeclExtFunc LLVoid (Ident "printString") [convToObject LLString])
    addGlobInstr (LLDeclExtFunc LLVoid (Ident "error") [])
    addGlobInstr (LLDeclExtFunc LLInt (Ident "readInt") [])
    addGlobInstr (LLDeclExtFunc (convToObject LLString) (Ident "readString") [])
    addGlobInstr (LLDeclExtFunc (convToObject LLString) (Ident "concat") [convToObject LLString, convToObject LLString])
    addGlobInstr LLDeclMalloc


genMainFunction :: LL ()
genMainFunction = do
    let main = unlines [ "define i32 @main() {"
                       , "\t%tmp-result = call i32 @function_main()"
                       , "\tret i32 %tmp-result"
                       , "}" ]
    addGlobInstr (LLMain main)

--------------------------------------------------------------------------------
-- HELPERS forcing types and other things - typechecker
--------------------------------------------------------------------------------

forceIntMainExistence :: LL ()
forceIntMainExistence = do
    fStore <- getFStore
    case M.lookup (Ident "main") fStore of
        Just (LLInt, []) -> return ()
        _ -> throwError "no int main() function"

forceIntMainSignature :: Ident -> LLFun -> BNFC'Position -> LL ()
forceIntMainSignature (Ident "main") (llType, llTypes) p = case llType of
    LLInt -> case llTypes of
        [] -> return ()
        _ -> throwError (genPosInfo p ++ "main function cannot have args")
    _ -> throwError (genPosInfo p ++ "invalid main function type")
forceIntMainSignature _ _ _ = return ()

lookUpReturnInFunc :: Block -> LL Bool
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

forceUniqueVarsInFunc :: [(Ident, BNFC'Position)] -> Block -> LL ()
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

forceTypeLLType :: LLType -> LLType -> BNFC'Position -> LL LLType
forceTypeLLType llType forcedType p = case (forcedType, llType) of
        (LLClass parentIdent, LLClass sonIdent) -> do
            classesMatch <- lookForAncestor sonIdent parentIdent
            if classesMatch
                then return forcedType
                else throwError (genPosInfo p ++ "classes not match")
            where
                lookForAncestor :: Ident -> Ident -> LL Bool
                lookForAncestor (Ident "<class Object>") parent = return False
                lookForAncestor son parent = if son == parent
                    then return True
                    else do
                        newSon <- getClassParent son p
                        lookForAncestor newSon parent
        _ -> if llType == forcedType
            then return llType
            else throwError (genPosInfo p
                            ++ "invalid type " ++ genTypeInfo llType
                            ++ ", expected " ++ genTypeInfo forcedType)

forceExprType :: Expr -> LLType -> LL (LLType, Reg)
forceExprType expr forcedType = do
    (t, reg) <- transExpr expr
    forceTypeLLType t forcedType (hasPosition expr)
    return (t, reg)

-- absolute =, for ==,!=
forceExprOneOfTypes :: Expr -> [LLType] -> LL (LLType, Reg)
forceExprOneOfTypes expr forcedTypes = do
    (t, reg) <- transExpr expr
    if t `elem` forcedTypes
        then return (t, reg)
        else throwError (genPosInfo (hasPosition expr)
                        ++ "invalid type " ++ genTypeInfo t)

forceAppArgsTypes :: [Expr] -> [LLType] -> [Reg] -> BNFC'Position -> LL [Reg]
forceAppArgsTypes exprs llTypes regs p = case (exprs, llTypes) of
    ([], []) -> return (reverse regs)
    (_, []) -> throwError (genPosInfo p ++ "too big number of arguments")
    ([], _) -> throwError (genPosInfo p ++ "too small number of arguments")
    (expr:exprs, forcedType:forcedTypes) -> do
        (t, reg) <- forceExprType expr forcedType
        forceAppArgsTypes exprs forcedTypes (reg:regs) p

forceExtendClassExist :: Extend -> LL ()
forceExtendClassExist (NoExtend _) = return ()
forceExtendClassExist (Extend p ident@(Ident name)) = do
    cStore <- getCStore
    case M.lookup ident cStore of
        Just _ -> return ()
        Nothing -> throwError (genPosInfo p ++ "unknown superclass " ++ name)

--------------------------------------------------------------------------------
-- TYPE CHECKER + LLVM COMPILER
--------------------------------------------------------------------------------

run :: Program -> Err String
run program = case runReader (runStateT (runExceptT (transProgram program))
                                                        initStore) initEnv of
    (Left err, _) -> Bad err
    (Right res, _) -> Ok res

transProgram :: Program -> LL String
transProgram (Program p defs) = do
    -- phase 0 -- prefef function
    genAllPredefinedFunctions
    -- phase 1 - init
    buildDefs defs
    forceIntMainExistence
    genAllClassesAndArraysDefinitions
    genMainFunction
    -- phase 2 - type checker + llvm compiler
    transDefs defs
    -- phase 3 - retrieve output
    addUnconditionalBrToLabels
    revInstrs <- getLLInstrs
    revGlobInstrs <- getLLGlobInstrs
    let instrs = doLCSE (reverse revInstrs) [] []
    let revOutputLines = (reverse instrs) ++ revGlobInstrs
    addWhiteSpaces revOutputLines ""

    where
        transDefs :: [Def] -> LL ()
        transDefs [] = return ()
        transDefs (def:defs) = do
            transDef def (TopDef (Ident "<class Object>"))
            transDefs defs

        addWhiteSpaces :: LLInstrs -> String -> LL String
        addWhiteSpaces [] res = return res
        addWhiteSpaces (last:instrs) res = do
            nextLine <- case last of
                (LLLabel s) -> return (show last)
                LLDefEnd -> return (show last)
                (LLStrDef g size s) -> return (show last)
                (LLDefineFunc l f ls rs) -> return (show last)
                (LLDefineMethod l c m ls rs) -> return (show last)
                (LLNewType _ _) -> return (show last)
                (LLDeclExtFunc _ _ _) -> return (show last)
                (LLMain _) -> return (show last)
                LLDeclMalloc -> return (show last)
                _ -> return ("\t" ++ show last)
            addWhiteSpaces instrs (nextLine ++ "\n" ++ res)

transDef :: Def -> ParentDef -> LL ()
transDef def parentDef = case def of
    DefFunc p t ident args block -> do
        forceUniqueVarsInFunc [] block
        llType <- convType t
        returnOk <- lookUpReturnInFunc block
        returnOk <- case llType of
            LLVoid -> return True
            _ -> return returnOk
        if returnOk
            then do
                llTypes <- mapM (\(Arg p t ident) -> convType t) args
                env <- ask
                (venv, pi) <- foldM (\tmpEnv (Arg p t ident) -> do
                        llType <- convType t
                        unLegalVoid llType p
                        varReg <- getNewVarReg ident
                        local (const tmpEnv)
                            (addVariableWithReg ident llType varReg p))
                    env args
                regs <- local (const (venv, pi)) (mapM (getVarReg . (\(Arg p t ident) -> ident)) args)
                case parentDef of
                    (ParentClassMet cIdent ident) -> do
                        selfReg <- getNewVarReg (Ident "self")
                        setVarReg (Ident "self") selfReg
                        addInstr (LLDefineMethod (convToObject llType) cIdent
                            ident ((convToObject (LLClass cIdent)):
                                (map convToObject llTypes)) (selfReg:regs))
                        local (const (venv, ParentClassMet cIdent ident)) 
                            (transBlock block)
                    _ -> do
                        addInstr (LLDefineFunc (convToObject llType) ident
                            (map convToObject llTypes) regs)
                        local (const (venv, ParentFun ident))
                            (transBlock block)
                case llType of
                    LLVoid -> addInstr (LLVRet)
                    _ -> do
                        defaultVal <- case llType of
                            LLInt -> return "0"
                            LLBool -> return "false"
                            _ -> return "null"
                        addInstr (LLRet (convToObject llType) defaultVal)
                addInstr LLDefEnd
            else throwError (genPosInfo p ++ "missing return in function")
    DefClass _ ident extend features -> do
        forceExtendClassExist extend
        selfReg <- getNewTmpReg
        venvAndSelf <- addSelfToVenv (LLClass ident) selfReg
        mapM_ (\feature -> local (const (venvAndSelf, TopDef ident))
            (transFeature ident feature)) features

transFeature :: Ident -> Feature -> LL ()
transFeature cIdent feature = case feature of
    Empty _ -> return ()
    Atribute p t items -> do
        llType <- convType t
        unLegalVoid llType p
        mapM_ (\item -> case item of
                Init p _ _ -> throwError 
                    (genPosInfo p ++ "init value for atribute not supported")
                _ -> return ()
            ) items
    Method p t ident args block -> do
        transDef (DefFunc p t ident args block) (ParentClassMet cIdent ident)

transBlock :: Block -> LL ()
transBlock (Block _ stmts) = do
    env <- ask
    foldM_ (\tmpEnv stmt -> local (const tmpEnv) (transStmt stmt)) env stmts

transStmt :: Stmt -> LL Env
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
            llType <- convType t
            unLegalVoid llType p
            (ident, pv, reg) <- case var of
                (NoInit pv ident) -> do
                    reg <- getInitRegInsideStructure llType
                    return (ident, pv, reg)
                (Init pv ident expr) -> do
                    (_, reg) <- forceExprType expr llType
                    return (ident, pv, reg)
            env <- addVariableWithReg ident llType reg pv
            local (const env) (transStmt (Decl p t vars))

    Ass p expr1 expr2 -> case expr1 of
        EVar p ident@(Ident name) ->case name of
            "self" -> throwError (genPosInfo p ++ "cannot assign to self")
            _ -> do
                vStore <- getVStore
                (venv, parentDef) <- ask
                case M.lookup ident venv of
                    Nothing -> do
                        (cIdent, isInClass) <- case parentDef of
                            ParentClassMet cIdent _ -> return (cIdent, True)
                            TopDef cIdent -> return (cIdent, True)
                            _ -> return (Ident "Void", False)
                        if not isInClass
                        then throwError (genPosInfo p ++ "var "
                                        ++ name ++ " not declared")
                        else do
                            llType <- getClassAtr cIdent ident p
                            selfReg <- getVarReg (Ident "self")
                            atrPos <- getClassAtrNumber cIdent ident
                            tmpReg <- getNewTmpReg
                            addInstr (LLGetElPtrAtr tmpReg (LLClass cIdent)
                                selfReg (show atrPos))
    
                            (rlType, reg2) <- forceExprType expr2 llType

                            case rlType == llType of
                                True -> addInstr (LLStore (convToObject llType) reg2 tmpReg)
                                False -> do
                                    castReg <- getNewTmpReg
                                    addInstr (LLBitcast castReg (convToObject rlType) reg2 (convToObject llType))
                                    addInstr (LLStore (convToObject llType) castReg tmpReg)
                            ask
                    Just loc -> case M.lookup loc vStore of
                        Nothing -> throwError (genPosInfo p 
                                                ++ "var undef reference")
                        Just llType -> do
                            varReg <- getVarReg ident

                            (rlType, reg2) <- forceExprType expr2 llType
                            case rlType == llType of
                                True -> setVarReg ident reg2
                                False -> do
                                    castReg <- getNewTmpReg
                                    addInstr (LLBitcast castReg (convToObject rlType) reg2 (convToObject llType))
                                    setVarReg ident castReg
                            ask
        EFeature p expr1 (EVar p2 atr) -> do
            (llType, reg1) <- transExpr expr1
            case llType of
                LLClass ident -> do
                    atrType <- getClassAtr ident atr p2
                    atrNumber <- getClassAtrNumber ident atr
                    atrPtrReg <- getNewTmpReg
                    addInstr (LLGetElPtrAtr atrPtrReg llType reg1 (show atrNumber))
                    (rlType, reg2) <- forceExprType expr2 atrType
                    case rlType == atrType of
                        True -> addInstr (LLStore (convToObject atrType) reg2 atrPtrReg)
                        False -> do
                            castReg <- getNewTmpReg
                            addInstr (LLBitcast castReg (convToObject rlType) reg2 (convToObject atrType))
                            addInstr (LLStore (convToObject atrType) castReg atrPtrReg)
                    ask
                _ -> throwError (genPosInfo p ++ "only class can have assignment on its attribute")
        EArrayVal p expr1 exprIndex -> do
            (llType, reg1) <- transExpr expr1
            case llType of
                LLArray arrayType -> do
                    (_, regIndex) <- forceExprType exprIndex LLInt
                    tmpReg <- getNewTmpReg
                    addInstr (LLGetElPtrAtr tmpReg llType reg1 "0")
                    arrayReg <- getNewTmpReg
                    addInstr (LLLoad arrayReg (LLPtr (convToObject arrayType)) tmpReg)
                    arrayElReg <- getNewTmpReg
                    addInstr (LLGetElPtr arrayElReg (convToObject arrayType) arrayReg regIndex)
                    (rlType, reg2) <- forceExprType expr2 arrayType
                    case rlType == arrayType of
                        True -> addInstr (LLStore (convToObject arrayType) reg2 arrayElReg)
                        False -> do
                            castReg <- getNewTmpReg
                            addInstr (LLBitcast castReg (convToObject rlType) reg2 (convToObject arrayType))
                            addInstr (LLStore (convToObject arrayType) castReg arrayElReg)
                    ask
                _ -> throwError (genPosInfo p ++ "expected array")
        _ -> throwError (genPosInfo p ++ "illegal assignment")

    Incr p expr -> case expr of
        EVar p ident@(Ident name) ->case name of
            "self" -> throwError (genPosInfo p ++ "cannot assign to self")
            _ -> do
                vStore <- getVStore
                (venv, parentDef) <- ask
                case M.lookup ident venv of
                    Nothing -> do
                        (cIdent, isInClass) <- case parentDef of
                            ParentClassMet cIdent _ -> return (cIdent, True)
                            TopDef cIdent -> return (cIdent, True)
                            _ -> return (Ident "Void", False)
                        if not isInClass
                        then throwError (genPosInfo p ++ "var "
                                        ++ name ++ " not declared")
                        else do
                            llType <- getClassAtr cIdent ident p
                            case llType of
                                LLInt -> do
                                    selfReg <- getVarReg (Ident "self")
                                    atrPos <- getClassAtrNumber cIdent ident
                                    tmpReg <- getNewTmpReg
                                    addInstr (LLGetElPtrAtr tmpReg (LLClass cIdent)
                                        selfReg (show atrPos))

                                    currentValReg <- getNewTmpReg
                                    addInstr (LLLoad currentValReg LLInt tmpReg)
                                    resReg <- getNewTmpReg
                                    addInstr (LLAdd resReg currentValReg "1")
                                    addInstr (LLStore (convToObject llType) resReg tmpReg)
                                    ask
                                _ -> throwError (genPosInfo p ++ "wrong type: expected int")
                    Just loc -> case M.lookup loc vStore of
                        Nothing -> throwError (genPosInfo p 
                                                ++ "var undef reference")
                        Just LLInt -> do
                            varReg <- getVarReg ident
                            case isRegLit varReg of
                                True -> do
                                    let val = (read varReg::Int) + 1
                                    setVarReg ident (show val)
                                False -> do
                                    resReg <- getNewTmpReg
                                    addInstr (LLAdd resReg varReg "1")
                                    setVarReg ident resReg
                            ask
                        Just _ -> throwError (genPosInfo p ++ "wrong type: expected int")
        EFeature p expr1 (EVar p2 atr) -> do
            (llType, reg1) <- transExpr expr1
            case llType of
                LLClass ident -> do
                    atrType <- getClassAtr ident atr p2
                    case atrType of
                        LLInt -> do
                            atrNumber <- getClassAtrNumber ident atr
                            atrPtrReg <- getNewTmpReg
                            addInstr (LLGetElPtrAtr atrPtrReg llType reg1 (show atrNumber))
                            
                            currentValReg <- getNewTmpReg
                            addInstr (LLLoad currentValReg LLInt atrPtrReg)
                            resReg <- getNewTmpReg
                            addInstr (LLAdd resReg currentValReg "1")
                            addInstr (LLStore (convToObject atrType) resReg atrPtrReg)
                            ask
                        _ -> throwError (genPosInfo p ++ "wrong type: expected int")
                _ -> throwError (genPosInfo p ++ "only class can have assignment on its attribute")
        EArrayVal p expr1 exprIndex -> do
            (llType, reg1) <- transExpr expr1
            case llType of
                LLArray LLInt -> do
                    (_, regIndex) <- forceExprType exprIndex LLInt
                    tmpReg <- getNewTmpReg
                    addInstr (LLGetElPtrAtr tmpReg llType reg1 "0")
                    arrayReg <- getNewTmpReg
                    addInstr (LLLoad arrayReg (LLPtr (convToObject LLInt)) tmpReg)
                    arrayElReg <- getNewTmpReg
                    addInstr (LLGetElPtr arrayElReg (convToObject LLInt) arrayReg regIndex)

                    currentValReg <- getNewTmpReg
                    addInstr (LLLoad currentValReg LLInt arrayElReg)
                    resReg <- getNewTmpReg
                    addInstr (LLAdd resReg currentValReg "1")
                    addInstr (LLStore (convToObject LLInt) resReg arrayElReg)
                    ask
                _ -> throwError (genPosInfo p ++ "expected array of integers")
        _ -> throwError (genPosInfo p ++ "illegal assignment")

    Decr p expr -> case expr of
        EVar p ident@(Ident name) ->case name of
            "self" -> throwError (genPosInfo p ++ "cannot assign to self")
            _ -> do
                vStore <- getVStore
                (venv, parentDef) <- ask
                case M.lookup ident venv of
                    Nothing -> do
                        (cIdent, isInClass) <- case parentDef of
                            ParentClassMet cIdent _ -> return (cIdent, True)
                            TopDef cIdent -> return (cIdent, True)
                            _ -> return (Ident "Void", False)
                        if not isInClass
                        then throwError (genPosInfo p ++ "var "
                                        ++ name ++ " not declared")
                        else do
                            llType <- getClassAtr cIdent ident p
                            case llType of
                                LLInt -> do
                                    selfReg <- getVarReg (Ident "self")
                                    atrPos <- getClassAtrNumber cIdent ident
                                    tmpReg <- getNewTmpReg
                                    addInstr (LLGetElPtrAtr tmpReg (LLClass cIdent)
                                        selfReg (show atrPos))

                                    currentValReg <- getNewTmpReg
                                    addInstr (LLLoad currentValReg LLInt tmpReg)
                                    resReg <- getNewTmpReg
                                    addInstr (LLSub resReg currentValReg "1")
                                    addInstr (LLStore (convToObject llType) resReg tmpReg)
                                    ask
                                _ -> throwError (genPosInfo p ++ "wrong type: expected int")
                    Just loc -> case M.lookup loc vStore of
                        Nothing -> throwError (genPosInfo p 
                                                ++ "var undef reference")
                        Just LLInt -> do
                            varReg <- getVarReg ident
                            case isRegLit varReg of
                                True -> do
                                    let val = (read varReg::Int) - 1
                                    setVarReg ident (show val)
                                False -> do
                                    resReg <- getNewTmpReg
                                    addInstr (LLSub resReg varReg "1")
                                    setVarReg ident resReg
                            ask
                        Just _ -> throwError (genPosInfo p ++ "wrong type: expected int")
        EFeature p expr1 (EVar p2 atr) -> do
            (llType, reg1) <- transExpr expr1
            case llType of
                LLClass ident -> do
                    atrType <- getClassAtr ident atr p2
                    case atrType of
                        LLInt -> do
                            atrNumber <- getClassAtrNumber ident atr
                            atrPtrReg <- getNewTmpReg
                            addInstr (LLGetElPtrAtr atrPtrReg llType reg1 (show atrNumber))
                            
                            currentValReg <- getNewTmpReg
                            addInstr (LLLoad currentValReg LLInt atrPtrReg)
                            resReg <- getNewTmpReg
                            addInstr (LLSub resReg currentValReg "1")
                            addInstr (LLStore (convToObject atrType) resReg atrPtrReg)
                            ask
                        _ -> throwError (genPosInfo p ++ "wrong type: expected int")
                _ -> throwError (genPosInfo p ++ "only class can have assignment on its attribute")
        EArrayVal p expr1 exprIndex -> do
            (llType, reg1) <- transExpr expr1
            case llType of
                LLArray LLInt -> do
                    (_, regIndex) <- forceExprType exprIndex LLInt
                    tmpReg <- getNewTmpReg
                    addInstr (LLGetElPtrAtr tmpReg llType reg1 "0")
                    arrayReg <- getNewTmpReg
                    addInstr (LLLoad arrayReg (LLPtr (convToObject LLInt)) tmpReg)
                    arrayElReg <- getNewTmpReg
                    addInstr (LLGetElPtr arrayElReg (convToObject LLInt) arrayReg regIndex)

                    currentValReg <- getNewTmpReg
                    addInstr (LLLoad currentValReg LLInt arrayElReg)
                    resReg <- getNewTmpReg
                    addInstr (LLSub resReg currentValReg "1")
                    addInstr (LLStore (convToObject LLInt) resReg arrayElReg)
                    ask
                _ -> throwError (genPosInfo p ++ "expected array of integers")
        _ -> throwError (genPosInfo p ++ "illegal assignment")

    Ret p expr -> do
        env@(_, parentDef) <- ask
        llFun <- case parentDef of
            ParentFun ident -> getFunction ident p
            ParentClassMet ident met -> do
               (_, s) <- getClassMet ident met p
               return s
            TopDef _ -> throwError (genPosInfo p ++ "unexpected return")
        case llFun of
            (LLVoid, _) -> throwError (genPosInfo p ++ "only empty returns")
            (llType, _) -> do
                (_, reg) <- forceExprType expr llType
                addInstr (LLRet (convToObject llType) reg)
                ask

    VRet p -> do
        env@(_, parentDef) <- ask
        llFun <- case parentDef of
            ParentFun ident -> getFunction ident p
            ParentClassMet ident met -> do
                (_, s) <- getClassMet ident met p
                return s
            TopDef _ -> throwError (genPosInfo p ++ "unexpected return")
        case llFun of
            (LLVoid, _) -> do
                addInstr LLVRet
                ask
            (llType, _) ->
                throwError (genPosInfo p ++ "empty return not match")

    While _ expr stmt -> do
        varMap1 <- genMapOfVarRT
        idNum <- getNewId
        addInstr (LLLabel (genLabelName "wblock" idNum))
        transStmt stmt

        varMap2 <- genMapOfVarRT
        let diffMap12 = diffOfVarRegMaps varMap1 varMap2

        let vars = M.keys diffMap12
        mapM_ (\varIdent -> do
            newTmpReg <- getNewTmpReg
            setVarReg varIdent newTmpReg) vars
        varMap3 <- genMapOfVarRT

        blockInstrs <- takeInsUntilLabel (genLabelName "wblock" idNum)
        addInstr (LLLabel (genLabelName "wblock" idNum))
        transStmt stmt
        varMap4 <- genMapOfVarRT
        let diffMap34 = diffOfVarRegMaps varMap3 varMap4
        blockInstrs <- takeInsUntilLabel (genLabelName "wblock" idNum)

        regainMapOfVarRegs varMap3
        addInstr (LLLabel (genLabelName "wstart" idNum))
        addInstr (LLGoto (genLabelName "wcond" idNum))
        addInstr (LLLabel (genLabelName "wblock" idNum))
        addInstrs blockInstrs
        addInstr (LLLabel (genLabelName "wcond" idNum))

        llTypes <- mapM getVarLLType (M.keys diffMap12)
        mapM_ (\(llType, (r1, _), (r3, r4)) -> do
            addInstr (LLPhi r3 (convToObject llType) [r1, r4] [genLabelName "wstart" idNum, genLabelName "wblock" idNum])
            ) (zip3 llTypes (M.elems diffMap12) (M.elems diffMap34))

        (_, regBool) <- forceExprType expr LLBool
        addInstr (LLBr regBool (genLabelName "wblock" idNum) (genLabelName "wend" idNum))
        addInstr (LLLabel (genLabelName "wend" idNum))
        ask

    ForEach p t ident expr stmt -> do
        newId <- getNewId
        let indIdent = Ident ("foreach_ind_" ++ show newId)
        let arrIdent = Ident ("foreach_array_" ++ show newId)
        let sizeIdent = Ident ("foreach_size_" ++ show newId)

        transBlock (Block p [
            Decl p (Int p) [(Init p indIdent) (ELitInt p 0)],
            Decl p (TArray p t) [Init p arrIdent expr],
            Decl p (Int p) [Init p sizeIdent (EFeature p (EVar p arrIdent) (EVar p (Ident "length")))],
            While p (ERel p (EVar p indIdent) (LTH p) (EVar p sizeIdent)) (SBlock p (Block p [
                Decl p t [Init p ident (EArrayVal p (EVar p arrIdent) (EVar p indIdent))],
                Incr p (EVar p indIdent),
                stmt ]))])
        ask

    CondElse _ expr stmt1 stmt2 -> do
        (_, regBool) <- forceExprType expr LLBool
        tranCondForElse regBool stmt1
        negRegBool <- getNewTmpReg
        addInstr (LLNot negRegBool regBool)
        tranCondForElse negRegBool stmt2
        ask where
            tranCondForElse :: Reg -> Stmt -> LL ()
            tranCondForElse regBool stmt = do
                varMap1 <- genMapOfVarRT
                idNum <- getNewId

                addInstr (LLLabel (genLabelName "cestart" idNum))
                addInstr (LLBr regBool (genLabelName "ceblock" idNum) (genLabelName "ceend" idNum))

                addInstr (LLLabel (genLabelName "ceblock" idNum))
                transStmt stmt
                varMap2 <- genMapOfVarRT

                let diffMap12 = diffOfVarRegMaps varMap1 varMap2
                llTypes <- mapM getVarLLType (M.keys diffMap12)
                addInstr (LLLabel (genLabelName "ceend" idNum))
                mapM_ (\(llType, (ident, (r1, r2))) -> do
                    r3 <- getNewTmpReg
                    addInstr (LLPhi r3 (convToObject llType) [r1, r2] [genLabelName "cestart" idNum, genLabelName "ceblock" idNum])
                    setVarReg ident r3) (zip llTypes (M.assocs diffMap12))

    Cond _ expr stmt -> do
        varMap1 <- genMapOfVarRT
        idNum <- getNewId

        addInstr (LLLabel (genLabelName "cstart" idNum))
        (_, regBool) <- forceExprType expr LLBool
        addInstr (LLBr regBool (genLabelName "cblock" idNum) (genLabelName "cend" idNum))

        addInstr (LLLabel (genLabelName "cblock" idNum))
        transStmt stmt
        varMap2 <- genMapOfVarRT

        let diffMap12 = diffOfVarRegMaps varMap1 varMap2
        llTypes <- mapM getVarLLType (M.keys diffMap12)
        addInstr (LLLabel (genLabelName "cend" idNum))
        mapM_ (\(llType, (ident, (r1, r2))) -> do
            r3 <- getNewTmpReg
            addInstr (LLPhi r3 (convToObject llType) [r1, r2] [genLabelName "cstart" idNum, genLabelName "cblock" idNum])
            setVarReg ident r3) (zip llTypes (M.assocs diffMap12))
        ask

transExpr :: Expr -> LL (LLType, Reg)
transExpr expr = case expr of

    EVar p ident@(Ident name) -> case name of
        "self" -> do
            (_, parentDef) <- ask
            case parentDef of
                ParentClassMet cIdent met -> do
                    selfReg <- getVarReg (Ident "self")
                    return (LLClass cIdent, selfReg)
                TopDef cIdent -> do
                    selfReg <- getVarReg (Ident "self")
                    return (LLClass cIdent, selfReg) -- no class Object
                _ -> throwError (genPosInfo p
                                ++ "self can be called in class definition")
        _ -> do
            vStore <- getVStore
            (venv, parentDef) <- ask
            case M.lookup ident venv of
                Nothing -> do
                    (cIdent, isInClass) <- case parentDef of
                        ParentClassMet cIdent _ -> return (cIdent, True)
                        TopDef cIdent -> return (cIdent, True)
                        _ -> return (Ident "Void", False)
                    if not isInClass
                        then throwError (genPosInfo p ++ "var "
                                        ++ name ++ " not declared")
                        else do
                            llType <- getClassAtr cIdent ident p
                            selfReg <- getVarReg (Ident "self")
                            atrPos <- getClassAtrNumber cIdent ident
                            tmpReg <- getNewTmpReg
                            addInstr (LLGetElPtrAtr tmpReg (LLClass cIdent)
                                selfReg (show atrPos))
                            regVal <- getNewTmpReg
                            addInstr (LLLoad regVal (convToObject llType) tmpReg)
                            return (llType, regVal)
                Just loc -> case M.lookup loc vStore of
                    Nothing -> throwError (genPosInfo p
                                            ++ "var undef reference")
                    Just llType -> do
                        varReg <- getVarReg ident
                        return (llType, varReg)

    -- as first check global function, then local method
    EApp p ident@(Ident name) exprs -> do
        -- check if function exists, then check
        fStore <- getFStore
        (matchedFun, llType, appReg) <- case M.lookup ident fStore of
            Just (llType, llTypes) -> do
                regs <- forceAppArgsTypes exprs llTypes [] p
                tmpReg <- getNewTmpReg
                case llType of
                    LLVoid -> addInstr
                        (LLCallProc ident (map convToObject llTypes) regs)
                    _ -> addInstr (LLCallFunc tmpReg (convToObject llType)
                        ident (map convToObject llTypes) regs)
                return (True, llType, tmpReg)
            Nothing -> return (False, LLVoid, "")
        -- check if method exists, then check
        if not matchedFun
            then do
                (_, parentDef) <- ask
                case parentDef of
                    ParentClassMet pIdent met -> findMethod pIdent ident where
                        findMethod :: Ident -> Ident -> LL (LLType, Reg)
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
                                        Just (llType, llTypes) -> do
                                            regs <- forceAppArgsTypes exprs llTypes [] p
                                            selfReg <- getVarReg (Ident "self")
                                            let classObj = convToObject (LLClass pIdent)
                                            castReg <-getNewTmpReg
                                            tmpReg <- getNewTmpReg

                                            let deepObj = convToObject (LLClass ident)
                                            addInstr (LLBitcast castReg classObj selfReg deepObj)
                                            
                                            case llType of
                                                LLVoid -> addInstr (LLCallMethodProc ident met (deepObj:(map convToObject llTypes)) (castReg:regs))
                                                _ -> addInstr (LLCallMethodFunc tmpReg (convToObject llType) ident met (deepObj:(map convToObject llTypes)) (castReg:regs))
                                            return (llType, tmpReg)
                    _ -> throwError (genPosInfo p
                                    ++ "unknown function/method " ++ name)
            else return (llType, appReg)

    EArrayVal p expr1 expr2 -> do
        (llType, reg1) <- transExpr expr1
        case llType of
            LLArray arrayType -> do
                (_, reg2) <- forceExprType expr2 LLInt
                tmpReg <- getNewTmpReg
                addInstr (LLGetElPtrAtr tmpReg llType reg1 "0")
                arrayReg <- getNewTmpReg
                addInstr (LLLoad arrayReg (LLPtr (convToObject arrayType)) tmpReg)
                arrayElReg <- getNewTmpReg
                addInstr (LLGetElPtr arrayElReg (convToObject arrayType) arrayReg reg2)
                valReg <- getNewTmpReg
                addInstr (LLLoad valReg (convToObject arrayType) arrayElReg)
                return (arrayType, valReg)
            _ -> throwError (genPosInfo p ++ "expected array")

    EFeature p expr1 expr2 -> do
        (llType, reg1) <- transExpr expr1
        case llType of
            LLClass ident -> do
                (fatherIdent, atr, met) <- getClassInfo ident p
                case expr2 of
                    (EVar p1 atr) -> do
                        atrType <- getClassAtr ident atr p1
                        atrNumber <- getClassAtrNumber ident atr
                        atrPtrReg <- getNewTmpReg
                        addInstr (LLGetElPtrAtr atrPtrReg llType reg1 (show atrNumber))
                        atrReg <- getNewTmpReg
                        addInstr (LLLoad atrReg (convToObject atrType) atrPtrReg)
                        return (atrType, atrReg)
                    (EApp p1 met exprs) -> do
                        (deepIdent, (llTypeFun, llTypes)) <- getClassMet ident met p1
                        regs <- forceAppArgsTypes exprs llTypes [] p1
                        tmpReg <- getNewTmpReg

                        let deepType = LLClass deepIdent
                        castReg <- getNewTmpReg
                        addInstr (LLBitcast castReg (convToObject llType) reg1 (convToObject deepType))
                        
                        case llTypeFun of
                            LLVoid -> addInstr (LLCallMethodProc deepIdent met (map convToObject (deepType:llTypes)) (castReg:regs))
                            _ -> addInstr (LLCallMethodFunc tmpReg (convToObject llTypeFun) deepIdent met (map convToObject (deepType:llTypes)) (castReg:regs))
                        return (llTypeFun, tmpReg)
                    _ -> throwError (genPosInfo p ++ "expected class feature")
            LLArray _ -> do
                case expr2 of
                    (EVar p1 var) -> case var of
                        (Ident "length") -> do
                            tmpReg <- getNewTmpReg
                            addInstr (LLGetElPtrAtr tmpReg llType reg1 "1")
                            regSize <- getNewTmpReg
                            addInstr (LLLoad regSize LLInt tmpReg)
                            return (LLInt, regSize)
                        _ -> throwError (genPosInfo p ++ "need array atribute")
                    _ -> throwError (genPosInfo p ++ "unknown array feature")
            _ -> throwError (genPosInfo p ++ "uknown feature")

    ELitInt p integer -> do
        if integer > 2147483647
            then throwError (genPosInfo p ++ "too big number for 'int'")
            else return (LLInt, show integer)

    ELitTrue _ -> return (LLBool, "true")

    ELitFalse _ -> return (LLBool, "false")

    ELitString _ string -> do
        let size = length string
        regStr <- allocaObject LLString
        -- addInstr (LLAlloca regStr LLString)
        regSize <- getNewTmpReg
        addInstr (LLGetElPtrAtr regSize LLString regStr "1")
        addInstr (LLStore LLInt (show size) regSize)
        regString <- getNewTmpReg
        addInstr (LLGetElPtrAtr regString LLString regStr "0")
        tmpGlobReg <- getNewTmpReg
        let globStrName = "glob-str-" ++ safeTail tmpGlobReg
        addInstr (LLUndefLine (tmpGlobReg ++ " = getelementptr ["
            ++ (show $ size + 1) ++ " x i8], [" ++ (show $ size + 1) ++ " x i8]* @"
            ++ globStrName ++ ", i32 0, i32 0"))
        addInstr (LLUndefLine ("store i8* " ++ tmpGlobReg ++ ", i8** " ++ regString))
        addGlobInstr (LLStrDef globStrName (show $ size + 1) string)
        return (LLString, regStr)

    ELitNull _ -> return (LLNull, "null") -- no match without cast

    Cast p exprClassIdentExpected expr -> do
        case exprClassIdentExpected of
            EVar p ident -> do
                _ <- getClassInfo ident p
                case expr of
                    (ELitNull _) -> do
                        reg <- getInitRegInsideStructure (LLClass ident)
                        return (LLClass ident, reg) 
                    _ -> do
                        (llType, reg) <- forceExprType expr (LLClass ident)
                        newTmpReg <- getNewTmpReg
                        addInstr (LLBitcast newTmpReg (convToObject llType)
                            reg (convToObject (LLClass ident)))
                        return (LLClass ident, newTmpReg)
            _ -> throwError (genPosInfo p ++ "class name expected")

    NewArray p t expr -> do
        llType <- convType t
        case llType of
            LLVoid -> throwError (genPosInfo p ++ "unexpected 'void' table")
            _ -> do
                (_, regSize) <- forceExprType expr LLInt
                arrayTypeReg <- allocaObject (LLArray llType)
                -- addInstr (LLAlloca arrayTypeReg (LLArray llType))
                regArrayPtr <- getNewTmpReg
                regSizePtr <- getNewTmpReg
                addInstr (LLGetElPtrAtr regArrayPtr (LLArray llType) arrayTypeReg "0")
                addInstr (LLGetElPtrAtr regSizePtr (LLArray llType) arrayTypeReg "1")

                arrayReg <- allocaArray (convToObject llType) regSize
                -- addInstr (LLAllocaArr arrayReg (convToObject llType) regSize)

                addInstr (LLStore (LLPtr (convToObject llType)) arrayReg regArrayPtr)
                addInstr (LLStore LLInt regSize regSizePtr)

                newId <- getNewId
                regIndex <- getNewTmpReg
                regNextIndex <- getNewTmpReg
  
                let initArrayStart = "init_array_start_" ++ show newId
                let initArrayCond = "init_array_cond_" ++ show newId
                let initArrayLoop = "init_array_loop_" ++ show newId
                let initArrayEnd = "init_array_end_" ++ show newId
                
                addInstr (LLLabel initArrayStart)
                addInstr (LLGoto initArrayCond)

                addInstr (LLLabel initArrayLoop)
                addInstr (LLAdd regNextIndex regIndex "1")
                regElemPtr <- getNewTmpReg
                addInstr (LLGetElPtr regElemPtr llType arrayReg regIndex)
                regDefaultValue <- getInitRegInsideStructure llType
                addInstr (LLStore llType regDefaultValue regElemPtr)
                
                addInstr (LLLabel initArrayCond)
                addInstr (LLPhi regIndex LLInt ["0", regNextIndex] [initArrayStart, initArrayLoop])
                regStopLoop <- getNewTmpReg
                addInstr (LLGE regStopLoop regIndex regSize)
                addInstr (LLBr regStopLoop initArrayEnd initArrayLoop)

                addInstr (LLLabel initArrayEnd)
                return (LLArray llType, arrayTypeReg)

    NewClass p ident -> do
        getClassInfo ident p
        newTmpReg <- allocaObject (LLClass ident) 
        -- addInstr (LLAlloca newTmpReg (LLClass ident))
        listOfAtrsForClass <- getListOfAtrsForClass ident []
        llTypes <- mapM (\(_, llType) -> return llType) listOfAtrsForClass
        foldM_ (\pos llType -> do
            atrReg <- getNewTmpReg
            addInstr (LLGetElPtrAtr atrReg (LLClass ident) newTmpReg (show pos))
            initReg <- getInitRegInsideStructure llType
            addInstr (LLStore (convToObject llType) initReg atrReg)
            return (pos + 1)
            ) 0 llTypes
        return (LLClass ident, newTmpReg)

    Neg _ expr -> do
        (t, reg) <- forceExprType expr LLInt
        case isRegLit reg of
            True -> return (convValI (0 - (read reg::Int)))
            False -> do
                newTmpReg <- getNewTmpReg
                addInstr (LLMul newTmpReg reg "-1")
                return (LLInt, newTmpReg)

    Not _ expr -> do
        (t, reg) <- forceExprType expr LLBool
        case isRegLit reg of
            True -> return (convValB (reg == "false"))
            False -> do
                newTmpReg <- getNewTmpReg
                addInstr (LLNot newTmpReg reg)
                return (LLBool, newTmpReg)

    EMul _ expr1 mulop expr2 -> do
        (t1, reg1) <- forceExprType expr1 LLInt
        (t2, reg2) <- forceExprType expr2 LLInt
        newTmpReg <- getNewTmpReg
        (litReg1, litReg2, bothLits) <- case (isRegLit reg1) && (isRegLit reg2) of
            True -> return (read reg1::Int, read reg2::Int, True)
            False -> return (0, 0, False)
        case (mulop, bothLits) of
            (Times _, True) -> return (convValI (litReg1 * litReg2))
            (Div _, True) -> return (convValI (div litReg1 litReg2))
            (Mod _, True) -> return (convValI (mod litReg1 litReg2))
            (Times _, _) -> do
                addInstr (LLMul newTmpReg reg1 reg2)
                return (LLInt, newTmpReg)
            (Div _, _) -> do
                addInstr (LLDiv newTmpReg reg1 reg2)
                return (LLInt, newTmpReg)
            (Mod _, _) -> do
                addInstr (LLMod newTmpReg reg1 reg2)
                return (LLInt, newTmpReg)

    EAdd _ expr1 addop expr2 -> do
        case addop of
            Plus _ -> do
                (t1, reg1) <- forceExprOneOfTypes expr1 [LLInt, LLString]
                (t2, reg2) <- forceExprType expr2 t1
                newTmpReg <- getNewTmpReg
                case (t1, (isRegLit reg1), (isRegLit reg2)) of
                    (LLString, _, _) -> do
                        addInstr (LLCallFunc newTmpReg (convToObject LLString)
                            (Ident "concat") (map convToObject [LLString, LLString]) [reg1, reg2])
                        return (t1, newTmpReg)
                    (_, True, True) -> do
                        let val1 = read reg1::Int
                        let val2 = read reg2::Int
                        return (convValI (val1 + val2))
                    _ -> do
                        addInstr (LLAdd newTmpReg reg1 reg2)
                        return (t1, newTmpReg)
            Minus _ -> do
                (t1, reg1) <- forceExprType expr1 LLInt
                (t2, reg2) <- forceExprType expr2 LLInt

                case (isRegLit reg1, isRegLit reg2) of
                    (True, True) -> do
                        let val1 = read reg1::Int
                        let val2 = read reg2::Int
                        return (convValI (val1 - val2))
                    _ -> do
                        newTmpReg <- getNewTmpReg
                        addInstr (LLSub newTmpReg reg1 reg2)
                        return (LLInt, newTmpReg)

    ERel _ expr1 relop expr2 -> do
        (llType, tmpReg1, tmpReg2) <- case relop of
            EQU _ -> do
                (t1, reg1) <- transExpr expr1
                (t2, reg2) <- forceExprOneOfTypes expr2 [t1]
                return (t1, reg1, reg2)
            NE _ -> do
                (t1, reg1) <- transExpr expr1
                (t2, reg2) <- forceExprOneOfTypes expr2 [t1]
                return (t1, reg1, reg2)
            _ -> do
                (t1, reg1) <- forceExprType expr1 LLInt
                (t2, reg2) <- forceExprType expr2 LLInt
                return (LLInt, reg1, reg2)

        case ((isRegLit tmpReg1), (isRegLit tmpReg2), llType) of
            (True, True, LLBool) -> case relop of
                EQU _ -> return (convValB (tmpReg1 == tmpReg2))
                _ -> return (convValB (tmpReg1 /= tmpReg2))
            (True, True, LLInt) -> do
                let val1 = read tmpReg1::Int
                let val2 = read tmpReg2::Int
                case relop of
                    LTH _ -> return (convValB (val1 < val2))
                    LE _ -> return (convValB (val1 <= val2))
                    GTH _ -> return (convValB (val1 > val2))
                    GE _ -> return (convValB (val1 >= val2))
                    EQU _ -> return (convValB (val1 == val2))
                    NE _ -> return (convValB (val1 /= val2))
            _ -> do
                newTmpReg <- getNewTmpReg
                case relop of
                    LTH _ -> addInstr (LLLTH newTmpReg tmpReg1 tmpReg2)
                    LE _ -> addInstr (LLLE newTmpReg tmpReg1 tmpReg2)
                    GTH _ -> addInstr (LLGTH newTmpReg tmpReg1 tmpReg2)
                    GE _ -> addInstr (LLGE newTmpReg tmpReg1 tmpReg2)
                    EQU _ -> addInstr (LLEQU newTmpReg (convToObject llType) tmpReg1 tmpReg2)
                    NE _ -> addInstr (LLNE newTmpReg (convToObject llType) tmpReg1 tmpReg2)
                return (LLBool, newTmpReg)

    EAnd _ expr1 expr2 -> do
        idNum <- getNewId
        let beginfirstL = genLabelName "beginfirstand" idNum
        let formfirstL = genLabelName "fromfirstandval" idNum
        let toSecondL = genLabelName "tosecondandval" idNum
        let fromSecondL = genLabelName "fromsecondandval" idNum
        let endL = genLabelName "andend" idNum

        addInstr (LLLabel beginfirstL)
        (t1, reg1) <- forceExprType expr1 LLBool
        addInstr (LLLabel formfirstL)
        addInstr (LLBr reg1 toSecondL endL)

        addInstr (LLLabel toSecondL)
        (t2, reg2) <- forceExprType expr2 LLBool
        addInstr (LLLabel fromSecondL)

        addInstr (LLLabel endL)
        newTmpReg <- getNewTmpReg
        addInstr (LLPhi newTmpReg LLBool [reg1, reg2] [formfirstL, fromSecondL])

        case ((isRegLit reg1), (isRegLit reg2), reg1 == "true", reg2 == "true") of
            (True, _, False, _) -> do
                takeInsUntilLabel beginfirstL
                return (LLBool, "false")
            (True, True, _, False) -> do
                takeInsUntilLabel beginfirstL
                return (LLBool, "false")
            (True, True, True, True) -> do
                takeInsUntilLabel beginfirstL
                return (LLBool, "true")
            _ -> return (LLBool, newTmpReg)

    EOr _ expr1 expr2 -> do
        idNum <- getNewId
        let beginfirstL = genLabelName "beginfirstor" idNum
        let formfirstL = genLabelName "fromfirstorval" idNum
        let toSecondL = genLabelName "tosecondorval" idNum
        let fromSecondL = genLabelName "fromsecondorval" idNum
        let endL = genLabelName "orend" idNum

        addInstr (LLLabel beginfirstL)
        (t1, reg1) <- forceExprType expr1 LLBool
        addInstr (LLLabel formfirstL)
        addInstr (LLBr reg1 endL toSecondL)

        addInstr (LLLabel toSecondL)
        (t2, reg2) <- forceExprType expr2 LLBool
        addInstr (LLLabel fromSecondL)

        addInstr (LLLabel endL)
        newTmpReg <- getNewTmpReg
        addInstr (LLPhi newTmpReg LLBool [reg1, reg2] [formfirstL, fromSecondL])

        case ((isRegLit reg1), (isRegLit reg2), reg1 == "true", reg2 == "true") of
            (True, _, True, _) -> do
                takeInsUntilLabel beginfirstL
                return (LLBool, "true")
            (True, True, _, True) -> do
                takeInsUntilLabel beginfirstL
                return (LLBool, "true")
            (True, True, False, False) -> do
                takeInsUntilLabel beginfirstL
                return (LLBool, "false")
            _ -> return (LLBool, newTmpReg)

--------------------------------------------------------------------------------
-- LLVM OPTIMIZER & IMPORTANT
--------------------------------------------------------------------------------

updateReg :: Reg -> Reg -> Reg -> Reg
updateReg oldReg newReg reg = if reg == oldReg
    then newReg
    else reg

updateRegsList :: Reg -> Reg -> [Reg] -> [Reg]
updateRegsList oldReg newReg = map (updateReg oldReg newReg)

-- oR (old register), nR (new register)
updateRegInInstr :: Reg -> Reg -> LLInstr -> LLInstr
updateRegInInstr oR nR instr = case instr of
    (LLCallFunc r l f ls rs) -> LLCallFunc r l f ls (updateRegsList oR nR rs)
    (LLCallProc f ls rs) -> LLCallProc f ls (updateRegsList oR nR rs)
    (LLCallMethodFunc r l c m ls rs) -> LLCallMethodFunc r l c m ls (updateRegsList oR nR rs)
    (LLCallMethodProc c m ls rs) -> LLCallMethodProc c m ls (updateRegsList oR nR rs)
    (LLMul r1 r2 r3) -> LLMul r1 (updateReg oR nR r2) (updateReg oR nR r3)
    (LLNot r1 r2) -> LLNot r1 (updateReg oR nR r2)
    (LLDiv r1 r2 r3) -> LLDiv r1 (updateReg oR nR r2) (updateReg oR nR r3)
    (LLMod r1 r2 r3) -> LLMod r1 (updateReg oR nR r2) (updateReg oR nR r3)
    (LLAdd r1 r2 r3) -> LLAdd r1 (updateReg oR nR r2) (updateReg oR nR r3)
    (LLSub r1 r2 r3) -> LLSub r1 (updateReg oR nR r2) (updateReg oR nR r3)
    (LLOr r1 r2 r3) -> LLOr r1 (updateReg oR nR r2) (updateReg oR nR r3)
    (LLAnd r1 r2 r3) -> LLAnd r1 (updateReg oR nR r2) (updateReg oR nR r3)
    (LLLTH r1 r2 r3) -> LLLTH r1 (updateReg oR nR r2) (updateReg oR nR r3)
    (LLLE r1 r2 r3) -> LLLE r1 (updateReg oR nR r2) (updateReg oR nR r3)
    (LLGTH r1 r2 r3) -> LLGTH r1 (updateReg oR nR r2) (updateReg oR nR r3)
    (LLGE r1 r2 r3) -> LLGE r1 (updateReg oR nR r2) (updateReg oR nR r3)
    (LLEQU r1 l r2 r3) -> LLEQU r1 l (updateReg oR nR r2) (updateReg oR nR r3)
    (LLNE r1 l r2 r3) -> LLNE r1 l (updateReg oR nR r2) (updateReg oR nR r3)
    (LLStore l r1 r2) -> LLStore l (updateReg oR nR r1) (updateReg oR nR r2)
    (LLLoad r1 l r2) -> LLLoad r1 l (updateReg oR nR r2)
    (LLGetElPtr r1 l r2 r3) -> LLGetElPtr r1 l (updateReg oR nR r2) (updateReg oR nR r3)
    (LLGetElPtrAtr r1 l r2 r3) -> LLGetElPtrAtr r1 l (updateReg oR nR r2) (updateReg oR nR r3)
    (LLAllocaArr r1 l r2) -> LLAllocaArr r1 l (updateReg oR nR r2)
    (LLBitcast r1 l1 r2 l2) -> LLBitcast r1 l1 (updateReg oR nR r2) l2
    (LLPhi r l rs ss) -> LLPhi r l (updateRegsList oR nR rs) ss
    (LLRet l r) -> LLRet l (updateReg oR nR r)
    (LLBr r lt lf) -> LLBr (updateReg oR nR r) lt lf
    (LLi32i64 r1 r2) -> LLi32i64 r1 (updateReg oR nR r2)
    _ -> instr

updateRegInInstrs :: Reg -> Reg -> LLInstrs -> LLInstrs
updateRegInInstrs oldReg newReg = map (updateRegInInstr oldReg newReg)

doesRegExistsInInstr :: Reg -> LLInstr -> Bool
doesRegExistsInInstr reg instr = case instr of
    (LLCallFunc r l f ls rs) -> elem reg rs
    (LLCallProc f ls rs) -> elem reg rs
    (LLCallMethodFunc r l c m ls rs) -> elem reg rs
    (LLCallMethodProc c m ls rs) -> elem reg rs
    (LLMul r1 r2 r3) -> reg == r2 || reg == r3
    (LLNot r1 r2) -> reg == r2
    (LLDiv r1 r2 r3) -> reg == r2 || reg == r3
    (LLMod r1 r2 r3) -> reg == r2 || reg == r3
    (LLAdd r1 r2 r3) -> reg == r2 || reg == r3
    (LLSub r1 r2 r3) -> reg == r2 || reg == r3
    (LLOr r1 r2 r3) -> reg == r2 || reg == r3
    (LLAnd r1 r2 r3) -> reg == r2 || reg == r3
    (LLLTH r1 r2 r3) -> reg == r2 || reg == r3
    (LLLE r1 r2 r3) -> reg == r2 || reg == r3
    (LLGTH r1 r2 r3) -> reg == r2 || reg == r3
    (LLGE r1 r2 r3) -> reg == r2 || reg == r3
    (LLEQU r1 l r2 r3) -> reg == r2 || reg == r3
    (LLNE r1 l r2 r3) -> reg == r2 || reg == r3
    (LLStore l r1 r2) -> reg == r1 || reg == r2
    (LLLoad r1 l r2) -> reg == r2
    (LLGetElPtr r1 l r2 r3) ->reg == r2 || reg == r3
    (LLGetElPtrAtr r1 l r2 r3) ->reg == r2 || reg == r3
    (LLAllocaArr r1 l r2) -> reg == r2
    (LLBitcast r1 l1 r2 l2) -> reg == r2
    (LLPhi r l rs ss) -> elem reg rs
    (LLRet l r) -> reg == r
    (LLBr r lt lf) -> reg == r
    (LLi32i64 r1 r2) -> reg == r2
    _ -> False

getRegFromInstr :: LLInstr -> Reg
getRegFromInstr instr = case instr of
    (LLMul r _ _) -> r
    (LLNot r _) -> r
    (LLDiv r _ _) -> r
    (LLMod r _ _) -> r
    (LLAdd r _ _) -> r
    (LLSub r _ _) -> r
    (LLOr r _ _) -> r
    (LLAnd r _ _) -> r
    (LLLTH r _ _) -> r
    (LLLE r _ _) -> r
    (LLGTH r _ _) -> r
    (LLGE r _ _) -> r
    (LLEQU r _ _ _) -> r
    (LLNE r _ _ _) -> r
    (LLGetElPtr r _ _ _) -> r
    (LLGetElPtrAtr r _ _ _) -> r
    (LLPhi r _ _ _) -> r
    _ -> "..unexpected reg"

doesRegExistsInInstrs :: Reg -> LLInstrs -> Bool
doesRegExistsInInstrs reg = foldl (\acc instr -> acc || (doesRegExistsInInstr reg instr)) False

isRightSideDuplicate :: LLInstr -> LLInstr -> (Bool, Reg, Reg)
isRightSideDuplicate lInstr rInstr = case (lInstr, rInstr) of
    (LLMul r1l r2l r3l, LLMul r1r r2r r3r) -> (r2l == r2r && r3l == r3r, r1l, r1r)
    (LLNot r1l r2l, LLNot r1r r2r) -> (r2l == r2r, r1l, r1r)
    (LLDiv r1l r2l r3l, LLDiv r1r r2r r3r) -> (r2l == r2r && r3l == r3r, r1l, r1r)
    (LLMod r1l r2l r3l, LLMod r1r r2r r3r) -> (r2l == r2r && r3l == r3r, r1l, r1r)
    (LLAdd r1l r2l r3l, LLAdd r1r r2r r3r) -> (r2l == r2r && r3l == r3r, r1l, r1r)
    (LLSub r1l r2l r3l, LLSub r1r r2r r3r) -> (r2l == r2r && r3l == r3r, r1l, r1r)
    (LLOr r1l r2l r3l, LLOr r1r r2r r3r) -> (r2l == r2r && r3l == r3r, r1l, r1r)
    (LLAnd r1l r2l r3l, LLAnd r1r r2r r3r) -> (r2l == r2r && r3l == r3r, r1l, r1r)
    (LLLTH r1l r2l r3l, LLLTH r1r r2r r3r) -> (r2l == r2r && r3l == r3r, r1l, r1r)
    (LLLE r1l r2l r3l, LLLE r1r r2r r3r) -> (r2l == r2r && r3l == r3r, r1l, r1r)
    (LLGTH r1l r2l r3l, LLGTH r1r r2r r3r) -> (r2l == r2r && r3l == r3r, r1l, r1r)
    (LLGE r1l r2l r3l, LLGE r1r r2r r3r) -> (r2l == r2r && r3l == r3r, r1l, r1r)
    (LLEQU r1l ll r2l r3l, LLEQU r1r lr r2r r3r) -> (r2l == r2r && r3l == r3r, r1l, r1r)
    (LLNE r1l ll r2l r3l, LLNE r1r lr r2r r3r) -> (r2l == r2r && r3l == r3r, r1l, r1r)
    (LLGetElPtr r1l ll r2l r3l, LLGetElPtr r1r lr r2r r3r) -> (r2l == r2r && r3l == r3r, r1l, r1r)
    (LLGetElPtrAtr r1l ll r2l r3l, LLGetElPtrAtr r1r lr r2r r3r) -> (r2l == r2r && r3l == r3r, r1l, r1r)
    (LLPhi rl ll rsl ssl, LLPhi rr lr rsr ssr) -> (rsl == rsr && ssl == ssr, rl, rr)
    _ -> (False, "", "")

isAnyDuplicate :: LLInstr -> LLInstrs -> (Bool, Reg, Reg)
isAnyDuplicate instr = foldl (\(b, r1, r2) nextInstr ->
        if b
            then (b, r1, r2)
            else isRightSideDuplicate instr nextInstr
    ) (False, "", "")

doBlockLCSE :: LLInstrs -> LLInstrs -> LLInstrs -> LLInstrs
doBlockLCSE otherInstrs [] prefInstrs = reverse prefInstrs
doBlockLCSE otherInstrs (nextInstr:restInstrs) prefInstrs = if b1 && b2
    then doBlockLCSE otherInstrs (updateRegInInstrs oldReg newReg restInstrs) prefInstrs
    else doBlockLCSE otherInstrs restInstrs (nextInstr:prefInstrs)
    where
        (b1, oldReg, newReg) = isAnyDuplicate nextInstr prefInstrs
        b2 = not (doesRegExistsInInstrs (getRegFromInstr nextInstr) otherInstrs)

-- run only on instrs without global definitions
doLCSE :: LLInstrs -> LLInstrs -> [LLInstrs] -> LLInstrs
doLCSE [] revBlockInstrs revResBlocks =
    let (prefBlocks, suffBlocks) = foldl (\(pref, suf) block ->
            (((doBlockLCSE (concat (pref ++ suf)) block []):pref), safeTail suf))
            ([], safeTail blocks) blocks
    in concat (reverse prefBlocks)
    where
        blocks = reverse ((reverse revBlockInstrs):revResBlocks)

doLCSE (instr:res) revBlockInstrs revResBlocks
    | isBeginOfBlock instr = doLCSE res [instr] revResBlocks
    | isEndOfBlock instr = case revBlockInstrs of
        [] -> doLCSE res [] revResBlocks
        _ -> doLCSE res [] ((reverse (instr:revBlockInstrs)):revResBlocks)
    | otherwise = case instr of
        LLDefEnd {} -> doLCSE res [] ([instr]:revResBlocks)
        _ -> doLCSE res (instr:revBlockInstrs) revResBlocks

    where
        isBeginOfBlock :: LLInstr -> Bool
        isBeginOfBlock LLLabel {} = True
        isBeginOfBlock LLDefineFunc {} = True
        isBeginOfBlock LLDefineMethod {} = True
        isBeginOfBlock _ = False

        isEndOfBlock :: LLInstr -> Bool
        isEndOfBlock LLGoto {} = True
        isEndOfBlock LLBr {} = True
        isEndOfBlock LLVRet {} = True
        isEndOfBlock LLRet {} = True
        isEndOfBlock _ = False
