{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

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

class PrettyPrint ast where
    prettyPrint :: ast -> String

-- Generate formatted source code of whole program from AST.
-- ASTProgram =
--   (ASTIdentifier, ASTVariableDeclarationPart,
--    ASTProcedureDeclarationPart, ASTCompoundStatement)
instance PrettyPrint ASTProgram where
    prettyPrint (id, variable, procedure, statement) =
        (prettyPrint id) ++
        (prettyPrint variable) ++
        (prettyPrint procedure) ++
        (prettyPrint statement)

instance PrettyPrint ASTIdentifier where
    prettyPrint name =
        "program " ++ name ++ ";\n\n"

-- ASTVariableDeclarationPart = [ASTVariableDeclaration]
instance PrettyPrint ASTVariableDeclarationPart where
    prettyPrint [] = ""
    prettyPrint xs =
        "var\n" ++ (showVariableDeclarationPart xs)

showVariableDeclarationPart :: ASTVariableDeclarationPart -> String
showVariableDeclarationPart [] = ""
showVariableDeclarationPart (x:xs) =
    "    " ++ (prettyPrint x) ++ "\n"
    ++ showVariableDeclarationPart xs

-- ASTVariableDeclaration = (ASTIdentifierList, ASTTypeDenoter)
instance PrettyPrint ASTVariableDeclaration where
    prettyPrint (ids, typ) =
        (prettyPrint ids) ++ ": " ++ (prettyPrint typ)

-- ASTIdentifierList = [ASTIdentifier]
instance PrettyPrint ASTIdentifierList where
    prettyPrint [] = ""
    prettyPrint (x:[]) = id x
    prettyPrint (x:xs) =
        (id x) ++ ", " ++ (prettyPrint xs)

-- ASTTypeDenoter = OrdinaryTypeDenoter | ArrayTypeDenoter
instance PrettyPrint ASTTypeDenoter where
    prettyPrint = show -- TODO

instance PrettyPrint ASTProcedureDeclarationPart where
    prettyPrint = show -- TODO

instance PrettyPrint ASTCompoundStatement where
    prettyPrint = show -- TODO