{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module PazFormat where
import PazLexer (
    ASTIdentifier,
    ASTUnsignedInteger,
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
    ASTConstant,
    ASTSubrangeType,
    ASTSign,
    Sign(..),
    TypeDenoter(..),
    TypeIdentifier(..),
    )

class PrettyPrint ast where
    prettyPrint :: ast -> String

-- Generate formatted source code of whole program from AST.
-- ASTProgram =
--   (ASTIdentifier, ASTVariableDeclarationPart,
--    ASTProcedureDeclarationPart, ASTCompoundStatement)
instance PrettyPrint ASTProgram where
    prettyPrint (id, variable, procedure, statement) =
        "program " ++ (prettyPrint id) ++ ";\n\n" ++
        (prettyPrint variable) ++
        (prettyPrint procedure) ++
        (prettyPrint statement)

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
        (prettyPrint ids) ++ ": " ++ (prettyPrint typ) ++ ";"

-- ASTIdentifierList = [ASTIdentifier]
instance PrettyPrint ASTIdentifierList where
    prettyPrint [] = ""
    prettyPrint (x:[]) = id x
    prettyPrint (x:xs) =
        (prettyPrint x) ++ ", " ++ (prettyPrint xs)

-- ASTIdentifier = String
-- Use `id` instead of `show` to avoid quotes ("").
instance PrettyPrint ASTIdentifier where
    prettyPrint = id

-- ASTTypeDenoter = OrdinaryTypeDenoter | ArrayTypeDenoter
instance PrettyPrint ASTTypeDenoter where
    prettyPrint (OrdinaryTypeDenoter x) = prettyPrint x
    prettyPrint (ArrayTypeDenoter x) = prettyPrint x

instance PrettyPrint ASTTypeIdentifier where
    prettyPrint IntegerTypeIdentifier = "integer"
    prettyPrint RealTypeIdentifier = "real"
    prettyPrint BooleanTypeIdentifier = "boolean"

instance PrettyPrint ASTArrayType where
    prettyPrint (subTyp, typId) =
        "array [" ++ (prettyPrint subTyp) ++ "] of " ++
        (prettyPrint typId)

instance PrettyPrint ASTSubrangeType where
    prettyPrint (const1, const2) =
        (prettyPrint const1) ++ ".." ++ (prettyPrint const2)

instance PrettyPrint ASTConstant where
    prettyPrint (Nothing, n) = prettyPrint n
    prettyPrint ((Just sign), n) =
        (prettyPrint sign) ++ (prettyPrint n)

instance PrettyPrint ASTUnsignedInteger where
    prettyPrint = show

instance PrettyPrint ASTSign where
    prettyPrint SignPlus = "+"
    prettyPrint SignMinus = "-"


instance PrettyPrint ASTProcedureDeclarationPart where
    prettyPrint = show -- TODO

instance PrettyPrint ASTCompoundStatement where
    prettyPrint = show -- TODO