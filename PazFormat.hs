module PazFormat where
import PazLexer (
    ASTIdentifier,
    )
import PazParser (
    ASTProgram,
    ASTVariableDeclarationPart,
    ASTProcedureDeclarationPart,
    ASTCompoundStatement,
    ASTVariableDeclaration,
    ASTIdentifierList,
    ASTTypeDenoter,
    ASTTypeIdentifier,
    ASTArrayType,
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

-- ASTVariableDeclarationPart = [ASTVariableDeclaration]
showVariableDeclarationPart :: ASTVariableDeclarationPart -> String
showVariableDeclarationPart [] = ""
showVariableDeclarationPart xs =
    "var\n" ++ (showVariableDeclarationPart' xs)

showVariableDeclarationPart' :: ASTVariableDeclarationPart -> String
showVariableDeclarationPart' [] = ""
showVariableDeclarationPart' (x:xs) =
    "    " ++ (showVariableDeclaration x) ++ "\n"
    ++ showVariableDeclarationPart' xs

-- ASTVariableDeclaration = (ASTIdentifierList, ASTTypeDenoter)
showVariableDeclaration :: ASTVariableDeclaration -> String
showVariableDeclaration (ids, typ) =
    (showIdentifierList ids) ++ ": " ++ (showTypeDenoter typ)

-- ASTIdentifierList = [ASTIdentifier]
showIdentifierList :: ASTIdentifierList -> String
showIdentifierList [] = ""
showIdentifierList (x:[]) = id x
showIdentifierList (x:xs) =
    (id x) ++ ", " ++ (showIdentifierList xs)

-- ASTTypeDenoter = OrdinaryTypeDenoter | ArrayTypeDenoter
showTypeDenoter :: ASTTypeDenoter -> String
showTypeDenoter = show -- TODO

showProcedureDeclarationPart :: ASTProcedureDeclarationPart -> String
showProcedureDeclarationPart = show -- TODO

showCompoundStatement :: ASTCompoundStatement -> String
showCompoundStatement = show -- TODO