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
    ASTProcedureDeclaration,
    ASTCompoundStatement,
    ASTVariableDeclaration,
    ASTIdentifierList,
    ASTTypeDenoter,
    ASTTypeIdentifier,
    ASTArrayType,
    ASTConstant,
    ASTSubrangeType,
    ASTSign,
    ASTFormalParameterList,
    Sign(..),
    TypeDenoter(..),
    TypeIdentifier(..),
    )

import PazParser (
    ASTExpression,
    ASTRelationalOperator,
    ASTSimpleExpression,
    ASTAddingOperator,
    ASTTerm,
    ASTMutiplayingOperator,
    ASTFactor,
    ASTVariableAccess,
    ASTIndexedVariable,
    ASTUnsignedNumber,
    ASTUnsignedConstant,
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

instance PrettyPrint ASTCompoundStatement where 
    prettyPrint = show  

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
    prettyPrint [] = ""
    prettyPrint (x:xs) =
        (prettyPrint x) ++ "\n" ++ (prettyPrint xs)

instance  PrettyPrint ASTProcedureDeclaration where
    prettyPrint = show 

-- ASTFormalParameterList = (ASTFormalParameterSection, [ASTFormalParameterSection])
instance PrettyPrint ASTFormalParameterList where
    prettyPrint (section, sections) = "(" ++ (show section)++ (show sections)++ "\n)"



-- ASTProcedureDeclaration = (ASTIdentifier, (Maybe ASTFormalParameterList), ASTVariableDeclarationPart, ASTCompoundStatement)
instance PrettyPrint ASTProcedureDeclaration where
    prettyPrint (id, param, var, stat) = 
        case param of 
            Nothing -> "procedure " ++ (prettyPrint id) ++ "(" ++ " nothing " ++ ")\n" ++ (show var) ++ (show stat) 
            Just param -> "procedure " ++ (prettyPrint id) ++ "(" ++ (show x) ++ ")\n" ++ (show var) ++ (show stat) 




-- ASTExpression = (ASTSimpleExpression, Maybe (ASTRelationalOperator, ASTSimpleExpression))
instance PrettyPrint ASTExpression where
    prettyPrint (astSimpExp1, param)= 
        case param of 
            Nothing -> astSimpExp1
            Just param ->  astSimpExp1 ++ param

-- ASTRelationalOperator = RelationalOperator
instance PrettyPrint ASTRelationalOperator where
    prettyPrint reloperator =
        case reloperator of 
            Equal -> "EQUAL" ++ "\n;"
            NotEqual -> "NOT_EQUAL" ++ "\n;"
            LessThan -> "LESS_THAN" ++ "\n;"
            GreaterThan -> "GREATER_THAN" ++ "\n;"
            LessThanOrEqual -> "LESS_THAN_OR_EQUAL" ++ "\n;"
            GreaterThanOrEqual -> "GREATER_THAN_OR_EQUAL" ++ "\n;"

-- -- ASTSimpleExpression = (Maybe Sign, ASTTerm, [(ASTAddingOperator, ASTTerm)])
instance PrettyPrint ASTSimpleExpression where
    prettyPrint (sign, astTerm1, [(opeartor, astTerm2)]) =
        case sign of 
            Nothing -> astTerm1 ++ "{" ++ opeartor ++ "}" ++ "\n;"
            Just sign -> "[" ++ sign ++ "]" ++ astTerm1 ++ "{" ++ opeartor ++ "}" ++ "\n;"

-- -- ASTAddingOperator = AddingOperator
instance PrettyPrint ASTAddingOperator where
    prettyPrint opeartor= 
        case operator of 
            Plus -> "+" ++ "\n;"
            Minus -> "-" ++ "\n;"

-- -- ASTTerm = (ASTFactor, [(ASTMutiplayingOperator, ASTFactor)])
instance PrettyPrint ASTTerm where
    prettyPrint (factor1, [(opeartor, factor2)]) =  
        factor1 ++ " {" ++ opeartor ++ " " ++ "factor2" ++ " }" ++ "\n;"

-- type ASTMutiplayingOperator = MutiplayingOperator
instance PrettyPrint ASTMutiplayingOperator where
    prettyPrint multiOperator= 
        case  multiOperator of 
            Times -> "TIMES" ++ "\n;"
            Divideby -> "DIVIDE_BY" ++ "\n;"
            Div -> "DIV" ++ "\n;"
            And -> "AND" ++ "\n;"

-- ASTFactor = Factor
instance PrettyPrint ASTFactor where
    prettyPrint factor = 
        case factor of
            UnsignedConstant ASTUnsignedConstant -> "unsigned_constant" ++ "\n;"
            VariableAccess ASTVariableAccess -> "variable_access" ++ "\n;"
            Expression ASTExpression -> "LEFT_PARENTHESIS expression RIGHT_PARENTHESIS" ++ "\n;"
            NotFactor ASTFactor -> "NOT factor" ++ "\n;"


-- -- ASTVariableAccess = VariableAccess
instance PrettyPrint ASTVariableAccess where
    prettyPrint sign =
        case sing of 
            IndexedVariable ASTIndexedVariable -> "indexed_variable" ++ "\n;"
            Identifier ASTIdentifier ->  "identifier" ++ "\n;"

-- -- ASTIndexedVariable = (ASTIdentifier, ASTExpression)
instance PrettyPrint ASTIndexedVariable where
    prettyPrint (id, astExp) = 
        id ++ " LEFT_BRACKET " ++ astExp ++ " RIGHT_BRACKET " ++ "\n;"

-- -- ASTUnsignedNumber = UnsignedNumber
instance PrettyPrint ASTUnsignedNumber where
    prettyPrint unsignedNum = 
        case  unsignedNum of 
            UnsignedInteger ASTUnsignedInteger -> "unsigned_integer" ++ "\n;"
            UnsignedReal ASTUnsignedReal -> "unsigned_real" ++ "\n;"


-- -- ASTUnsignedConstant = UnsignedConstant
instance PrettyPrint ASTUnsignedConstant where
    prettyPrint unsignedCon = 
        case unsignedCon of 
            UnsignedNumber ASTUnsignedNumber -> " unsigned_number" ++ "\n;"
            CharacterString ASTCharacterString -> " unsigned_real" ++ "\n;"








