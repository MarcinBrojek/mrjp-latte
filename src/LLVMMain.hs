{-# LANGUAGE CPP #-} -- for the LIBPATH definition

import System.Environment
import System.IO
import System.Process

import LexLatte
import ParLatte
import LLVMCompiler as LL
import ErrM

import StreamHelper as H

writeError :: String -> IO String
writeError err = do
    hPutStrLn stderr "ERROR"
    ioError (userError err)
    return ""

writeOk :: String -> IO String
writeOk str = do
    hPutStrLn stderr "OK"
    return str

main :: IO ()
main = do
    args <- getArgs
    stream <- H.tryStream args
    input <- H.getInput stream
    output <- case pProgram (myLexer input) of
        Ok tree -> case LL.run tree of
            Bad err -> writeError ("Type checker error: " ++ err)
            Ok res -> writeOk res
            _ -> writeError "Type checker error"
        Bad err -> writeError ("Parser error: " ++ err)
        _ -> writeError "Parser error"
    case stream of
        StdIO -> H.writeOutput stream output
        File filePath -> do
            llvmFile <- H.changeExtension ".lat" ".ll" filePath
            bitFile <- H.changeExtension ".lat" ".bc" filePath
            streamFile <- H.tryStream [llvmFile]
            H.writeOutput streamFile output
            callCommand ("llvm-as " ++ llvmFile ++ " -o " ++ bitFile)
            callCommand ("llvm-link -o " ++ bitFile ++ " "  ++ bitFile ++ " \"" ++ LIBPATH ++ "/lib/runtime.bc\"")
