module StreamHelper where
import System.Environment
import System.IO
import System.FilePath.Posix as P

data Stream = StdIO | File String

tryStream :: [String] -> IO Stream
tryStream args = case args of
    [] -> return StdIO
    [filePath] -> return (File filePath)
    _ -> ioError (userError "Wrong number of arguments.")

getInput :: Stream -> IO String
getInput stream = case stream of
    StdIO -> getContents
    File filePath -> openFile filePath ReadMode >>= hGetContents

writeOutput :: Stream -> String -> IO ()
writeOutput stream output = case stream of
    StdIO -> putStr output
    File filePath -> do
        handle <- openFile filePath WriteMode
        hPutStr handle output
        hClose handle

-- changes extension in the name of file from old to new one
changeExtension :: String -> String -> String -> IO String
changeExtension oldExtension newExtension filePath = 
    case oldExtension == P.takeExtension filePath of
        True -> return (P.replaceExtension filePath newExtension)
        False -> ioError (userError "Wrong file extension")
