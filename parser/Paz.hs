--import Debug.Trace (trace)
import Text.Parsec (parse)
import PazLexer (parseStartSymbol)
import PazParser (parseStartSymbol, printStartSymbol)
import Compiler (compileStartSymbol)
import System.IO (openFile, hGetContents, IOMode(..), hPutStrLn, stderr)
import System.Exit (exitFailure)
import System.Environment (getArgs)

-- turn off parser and lexer tracing since stage 1 is now complete
trace :: x -> y -> y
trace _ y = y

die :: String -> IO ()
die err = do
    hPutStrLn stderr err
    exitFailure

main :: IO ()
main = do
    (x : xs) <- getArgs
    case x of
        "-p" ->
            let
                (y : _) = xs
            in
                do
                    file <- openFile y ReadMode
                    text <- hGetContents file
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
                                    putStr (printStartSymbol ast)
        _ ->
            -- horrible duplication here.
            -- this is so that we don't have to use Haskell Exceptions.
            do
                file <- openFile x ReadMode
                text <- hGetContents file
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
                                putStr (compileStartSymbol ast)
