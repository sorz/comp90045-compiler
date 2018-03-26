{-# LANGUAGE PackageImports #-}

import Debug.Trace (trace)
import Text.Parsec (parse)
import PazLexer
import PazParser
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)

die :: String -> IO ()
die err = do
    hPutStrLn stderr err
    exitFailure

main :: IO ()
main = do
    text <- getContents
    case
        trace
             "*** Lexical analysis"
             (parse PazLexer.parseStartSymbol "(stdin)" text)
        of
        Left error ->
            die ("Lexical error:\n" ++ show error)
        Right tokens ->
            case
                trace
                    "*** Syntax analysis"
                    (parse PazParser.parseStartSymbol "(stdin)" tokens)
                of
                Left error ->
                    die ("Syntax error:\n" ++ show error)
                Right ast ->
                    putStrLn (show ast)
