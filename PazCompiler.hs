module PazCompiler where

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
compileProgram :: ASTProgram -> String
compileProgram (name, varDecls, procDecls, bodyStatement) =
    let
        slot = 0
        symbols = (Map.empty, Map.empty)
        label = 0

        -- pre-compile the procedure declarations to create a map from
        -- procedure name to formal parameter list, then really compile
        symbols' = precompileProcedureDeclarationPart symbols procDecls

        -- the following is a bit crusty due to the awkward order of declaring
        -- variables vs. procedures (because variables are meant to be global,
        -- but aren't in Paz), hence why we use symbols' twice, not symbols''
        (slot', symbols'') =
            compileVariableDeclarationPart slot symbols' varDecls
        (label', procText) =
            compileProcedureDeclarationPart label symbols' procDecls
        (label'', bodyText) =
            compileCompoundStatement label' symbols'' bodyStatement
    in
        "# program " ++ name ++
            "\n    call main\n    halt\n" ++
            procText ++
            "main:\n    push_stack_frame " ++
            show slot' ++
            "\n# begin" ++
            "\n" ++
            bodyText ++
            "# end" ++
            "\n    pop_stack_frame " ++
            show slot' ++
            "\n    return\n"

-- the following pre-compilation functions are intended as an example of
-- how you can walk through the AST gathering information into a symbol
-- table (by adding additional state such as the label or slot number and
-- changing the return type, you can easily modify this to compile things)
precompileProcedureDeclarationPart ::
    Symbols -> ASTProcedureDeclarationPart -> Symbols
precompileProcedureDeclarationPart symbols [] =
    symbols
precompileProcedureDeclarationPart symbols (x : xs) =
    -- the following doesn't actually require a let statement, but when
    -- you start adding additional parameters and return values it might
    let
        symbols' = precompileProcedureDeclaration symbols x
        symbols'' = precompileProcedureDeclarationPart symbols' xs
    in
        symbols''

precompileProcedureDeclaration ::
    Symbols -> ASTProcedureDeclaration -> Symbols
precompileProcedureDeclaration (procSymbols, varSymbols) (x0, x1, x2, x3) =
    (Map.insert x0 (precompileFormalParameterList x1) procSymbols, varSymbols)

precompileFormalParameterList ::
    ASTFormalParameterList -> [(Bool, ASTTypeDenoter)]
precompileFormalParameterList [] =
    []
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
    Int -> Symbols -> ASTVariableDeclarationPart -> (Int, Symbols)
compileVariableDeclarationPart slot symbols [] =
    (slot, symbols)
compileVariableDeclarationPart slot symbols (x : xs) =
    error "compiling variable declarations is not yet implemented"

-- compile a list of procedures
-- takes a label number, a symbol table and an AST fragment
-- returns the advanced label number and the generated code
compileProcedureDeclarationPart ::
    Int -> Symbols -> ASTProcedureDeclarationPart -> (Int, String)
compileProcedureDeclarationPart label symbols [] =
    (label, "")
compileProcedureDeclarationPart label symbols (x : xs) =
    error "compiling procedure declarations is not yet implemented"

-- compile a list of statements
-- takes a label number, a symbol table and an AST fragment
-- returns the advanced label number and the generated code
compileCompoundStatement ::
    Int -> Symbols -> ASTCompoundStatement -> (Int, String)
compileCompoundStatement label symbols [] =
    (label, "")
compileCompoundStatement label symbols (x : xs) =
    error "compiling compound statements is not yet implemented"
