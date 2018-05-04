module PazCompiler where

import Control.Applicative
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import PazLexer as L
import PazParser as P

-- this is the entry point to the compiler from the Paz.hs driver module
compileStartSymbol :: P.ASTStartSymbol -> String
compileStartSymbol =
    compileProgram

-- the following is a suggestion for how you can maintain the symbol table,
-- the symbol information for procedures and for variables is kept separate
-- (although this isn't a requirement, they can share the same name space
-- if you wish), and put in a pair to make it easy to pass around everywhere
type Symbols =
    (
        -- for each procedure, for each formal parameter, its varness and type
        Map String [(Bool, ASTTypeDenoter)],

        -- for each variable, its varness, type, and starting slot number
        Map String (Bool, ASTTypeDenoter, Int)
        )

-- the following is a suggestion for how your compiler can be structured,
-- there is no requirement to follow this template but it shows how the
-- important information (such as current label number) can be threaded
-- through the functions that implement the various parts of the compiler
-- (the more advanced students might wish to use a state monad for this)
type LabelCounter = Int
type SlotCounter = Int
data State = State Symbols LabelCounter SlotCounter
data CodeGen a = CodeGen (State -> (a, State))

instance Functor CodeGen where
    fmap = Control.Monad.liftM

instance Applicative CodeGen where
    pure = return
    (<*>) = Control.Monad.ap

instance Monad CodeGen where
    return code = CodeGen (\state -> (code, state))
    CodeGen gen >>= f
        = CodeGen (\st0 -> let
                                (code, st1) = gen st0
                                CodeGen gen' = f code
                            in gen' st1
                    )

getState :: CodeGen State
getState = CodeGen (\st -> (st, st))

runState :: CodeGen a -> State -> a
runState (CodeGen gen) state = code where
    (code, _) = gen state

incLabelCounter :: CodeGen ()
incLabelCounter =
    CodeGen (\(State sym lc sc) -> ((), State sym (lc+1) sc))

clearSlotCounter :: CodeGen ()
clearSlotCounter =
    CodeGen (\(State sym lc _) -> ((), State sym lc 0))

nextLabel :: CodeGen String
nextLabel = do
    State _ counter _ <- getState
    incLabelCounter
    return $ "label-" ++ (show counter)

putProcedure :: String -> [(Bool, ASTTypeDenoter)] -> CodeGen ()
putProcedure id params = CodeGen (\(State (procs, vars) lc sc) ->
    let procs' = Map.insert id params procs in
        ((), State (procs', vars) lc sc)
    )

putVariable :: String -> ASTTypeDenoter -> CodeGen ()
putVariable id typ = CodeGen (\(State (procs, vars) lc sc) ->
    let vars' = Map.insert id (False, typ, sc) vars in
        ((), State (procs, vars') lc (sc+1))
    )

genOp :: String -> [String] -> CodeGen String
genOp op [] = return $ "    " ++ op ++ "\n"
genOp op args =
    return $ "    " ++ op ++ " " ++ (gen args) ++ "\n"
    where gen (a:[]) = a
          gen (a:b:xs) = a ++ ", " ++ b

compileProgram :: ASTProgram -> String
compileProgram (name, varDecls, procDecls, bodyStatement) =
    let
        state = State (Map.empty, Map.empty) 0 0
        gen = do
            precompileProcedureDeclarationPart procDecls
            slot <- compileVariableDeclarationPart varDecls
            procText <- compileProcedureDeclarationPart procDecls
            bodyText <- compileCompoundStatement bodyStatement
            return (slot, procText, bodyText)
        (slot, procText, bodyText) = runState gen state
    in
        "# program " ++ name ++
            "\n    call main\n    halt\n" ++
            procText ++
            "main:\n    push_stack_frame " ++
            show slot ++
            "\n# begin" ++
            "\n" ++
            bodyText ++
            "# end" ++
            "\n    pop_stack_frame " ++
            show slot ++
            "\n    return\n"

-- the following pre-compilation functions are intended as an example of
-- how you can walk through the AST gathering information into a symbol
-- table (by adding additional state such as the label or slot number and
-- changing the return type, you can easily modify this to compile things)
precompileProcedureDeclarationPart ::
    ASTProcedureDeclarationPart -> CodeGen ()
precompileProcedureDeclarationPart [] = return ()
precompileProcedureDeclarationPart (x : xs) = do
    precompileProcedureDeclaration x
    precompileProcedureDeclarationPart xs

precompileProcedureDeclaration ::
    ASTProcedureDeclaration -> CodeGen ()
precompileProcedureDeclaration (x0, x1, x2, x3) = do
    putProcedure x0 (precompileFormalParameterList x1)

precompileFormalParameterList ::
    ASTFormalParameterList -> [(Bool, ASTTypeDenoter)]
precompileFormalParameterList [] = []
precompileFormalParameterList (x : xs) =
    precompileFormalParameterSection x ++ precompileFormalParameterList xs

precompileFormalParameterSection ::
    ASTFormalParameterSection -> [(Bool, ASTTypeDenoter)]
precompileFormalParameterSection (isVar, [], typeDenoter) =
    []
precompileFormalParameterSection (isVar, (x : xs), typeDenoter) =
    (isVar, typeDenoter) :
        precompileFormalParameterSection (isVar, xs, typeDenoter)

-- the following is code that you have to implement
-- we've provided signatures and comments corresponding to the calls made
-- above, there is no requirement that you follow these signatures at all!

-- compile a list of variable declarations
-- takes a slot number, a symbol table and an AST fragment
-- returns the advanced slot number and the updated symbol table
compileVariableDeclarationPart ::
    ASTVariableDeclarationPart -> CodeGen Int
compileVariableDeclarationPart [] = do
    State _ _ sc <- getState
    clearSlotCounter
    return sc
compileVariableDeclarationPart (x:xs) = do
    compileVariableDeclaration x
    compileVariableDeclarationPart xs

compileVariableDeclaration ::
    ASTVariableDeclaration -> CodeGen ()
compileVariableDeclaration ([], typ) = return ()
compileVariableDeclaration ((id:ids), typ) = do
    putVariable id typ
    compileVariableDeclaration (ids, typ)

-- compile a list of procedures
-- takes a label number, a symbol table and an AST fragment
-- returns the advanced label number and the generated code
compileProcedureDeclarationPart ::
    ASTProcedureDeclarationPart -> CodeGen String
compileProcedureDeclarationPart [] = return ""
compileProcedureDeclarationPart (x : xs) =
    error "compiling procedure declarations is not yet implemented"

-- compile a list of statements
-- takes a label number, a symbol table and an AST fragment
-- returns the advanced label number and the generated code
compileCompoundStatement ::
    ASTCompoundStatement -> CodeGen String
compileCompoundStatement [] = return ""
compileCompoundStatement (x : xs) = do
    code0 <- compileStatement x
    code1 <- compileCompoundStatement xs
    return $ code0 ++ code1

compileStatement ::
    ASTStatement -> CodeGen String
compileStatement (WriteStringStatement stat) =
    compileWriteStringStatement stat
compileStatement WritelnStatement = compileWritelnStatement
compileStatement EmptyStatement = return ""
compileStatement stat =
    error "compiling statement is not yet implemented"

compileWriteStringStatement ::
    ASTWriteStringStatement -> CodeGen String
compileWriteStringStatement str = do
    c0 <- genOp "r0" ["'" ++ (concatMap repl str) ++ "'"]
    c1 <- genOp "call_builtin" ["print_string"]
    return $ c0 ++ c1
    where
        repl '\'' = "''"
        repl c = [c]

compileWritelnStatement :: CodeGen String
compileWritelnStatement = genOp "call_builtin" ["print_newline"]
