{-# LANGUAGE PackageImports #-}
-- The entry point (main) of Paz compiler program.
-- Our solution of the programming project in COMP90045,
-- the University of Melbourne.
--
-- Require following modules:
--  * PazLexer:    lexer of Paz
--  * PazParser:   synatx parser of Paz
--  * PazFormat:   pretty printing
--  * PazCompiler: Paz to Taz compiler
--
-- Written by team Placeholder, May 2018.
import Text.Parsec (parse, ParseError)
import PazLexer
import PazParser
import PazFormat
import PazCompiler
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import System.Environment (getArgs)

die :: String -> IO ()
die err = do
    hPutStrLn stderr err
    exitFailure

usage = "Usage: Paz [-p] source_file"

main :: IO()
main = do
    args <- getArgs
    case args of
        ["-p"] -> die usage
        [file] -> compile file
        ["-p", file] -> Main.prettyPrint file
        _ -> die usage


data ParseResult =
    LexcialError ParseError |
    SyntaxError ParseError |
    Ok PazParser.ASTStartSymbol

instance Show ParseResult where
    show (LexcialError err) = "Lexical error:\n" ++ show err
    show (SyntaxError err) = "Syntax error:\n" ++ show err

-- Parse AST from the source file.
getASTFromFile :: String -> IO ParseResult
getASTFromFile file = do
    text <- readFile file
    case
        trace
            "*** Lexical analysis"
            (parse PazLexer.parseStartSymbol file text)
        of
        Left err -> return (LexcialError err)
        Right tokens ->
            case
                trace
                    "*** Syntax analysis"
                    (parse PazParser.parseStartSymbol file tokens)
                of
                Left err -> return (SyntaxError err)
                Right ast -> return (Ok ast)

-- Compile the source file. Unimplemented in stage 1.
compile :: String -> IO()
compile file = do
    result <- getASTFromFile file
    case result of
        LexcialError _ -> die (show result)
        SyntaxError _ -> die (show result)
        Ok ast ->
            putStrLn (PazCompiler.compileStartSymbol ast)

-- Pretty print the source file.
prettyPrint :: String -> IO()
prettyPrint file = do
    result <- getASTFromFile file
    case result of
        LexcialError _ -> die (show result)
        SyntaxError _ -> die (show result)
        Ok ast ->
            putStrLn (PazFormat.prettyPrint ast)
