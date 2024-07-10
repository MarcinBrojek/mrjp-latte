import System.Environment
import System.IO
import System.Process

import LexLatte
import ParLatte
import TypeChecker as TC
import ErrM

import StreamHelper as H

writeError :: String -> IO ()
writeError err = do
    hPutStrLn stderr "ERROR"
    ioError (userError err)

writeOk :: IO ()
writeOk = do
    hPutStrLn stderr "OK"
    return ()

main :: IO ()
main = do
    args <- getArgs
    stream <- H.tryStream args
    input <- H.getInput stream
    case pProgram (myLexer input) of
        Ok tree -> case TC.run tree of
            Bad err -> writeError ("Type checker error: " ++ err)
            _ -> writeOk
        Bad err -> writeError ("Parser error: " ++ err)
        _ -> writeError "Parser error"
