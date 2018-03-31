module PazFormat where
import PazLexer (
    ASTIdentifier,
    )
import PazParser (
    ASTProgram,
    ASTVariableDeclarationPart,
    ASTProcedureDeclarationPart,
    ASTCompoundStatement,
    )

-- Generate formatted source code of whole program from AST.
-- ASTProgram =
--   (ASTIdentifier, ASTVariableDeclarationPart,
--    ASTProcedureDeclarationPart, ASTCompoundStatement)
showProgram :: ASTProgram -> String
showProgram (id, variable, procedure, statement) =
    (showIdentifier id) ++
    (showVariableDeclarationPart variable) ++
    (showProcedureDeclarationPart procedure) ++
    (showCompoundStatement statement)

showIdentifier :: ASTIdentifier -> String
showIdentifier name =
    "program " ++ name ++ ";\n\n"

showVariableDeclarationPart :: ASTVariableDeclarationPart -> String
showVariableDeclarationPart = show -- TODO

showProcedureDeclarationPart :: ASTProcedureDeclarationPart -> String
showProcedureDeclarationPart = show -- TODO

showCompoundStatement :: ASTCompoundStatement -> String
showCompoundStatement = show -- TODO