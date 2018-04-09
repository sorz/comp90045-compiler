{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module PazFormat where
import PazLexer as L
import PazParser as P

class PrettyPrint ast where
    prettyPrint :: ast -> String

------------------
-- program section
------------------

-- Generate formatted source code of whole program from AST.
-- ASTProgram =
--   (ASTIdentifier, ASTVariableDeclarationPart,
--    ASTProcedureDeclarationPart, ASTCompoundStatement)
instance PrettyPrint P.ASTProgram where
    prettyPrint (id, variable, procedure, statement) =
        "program " ++ (prettyPrint id) ++ ";\n\n" ++
        (prettyPrint variable) ++
        (prettyPrint procedure) ++
        (prettyPrint statement)

instance PrettyPrint P.ASTProcedureDeclarationPart where
    prettyPrint [] = ""
    prettyPrint (x:xs) =
        (prettyPrint x) ++ "\n" ++ (prettyPrint xs)

--------------------
-- statement section
--------------------

instance PrettyPrint P.ASTCompoundStatement where 
    prettyPrint = show  

---------------------
-- procedures section
---------------------

-- ASTFormalParameterList = (ASTFormalParameterSection, [ASTFormalParameterSection])
instance PrettyPrint P.ASTFormalParameterList where
    prettyPrint (section, sections) = "(" ++ (show section)++ (show sections)++ "\n)"

-- ASTProcedureDeclaration = (ASTIdentifier, (Maybe ASTFormalParameterList), ASTVariableDeclarationPart, ASTCompoundStatement)
instance PrettyPrint P.ASTProcedureDeclaration where
    prettyPrint (id, param, var, stat) = 
        case param of 
            Nothing -> "procedure " ++ (prettyPrint id) ++ "(" ++ " nothing " ++ ")\n" ++ (show var) ++ (show stat) 
            Just param -> "procedure " ++ (prettyPrint id) ++ "(" ++ (show param) ++ ")\n" ++ (show var) ++ (show stat) 


----------------------
-- expressions section
----------------------

-- ASTExpression =
--     (ASTSimpleExpression,
--     Maybe (ASTRelationalOperator, ASTSimpleExpression))
instance PrettyPrint P.ASTExpression where
    prettyPrint (expr1, param) =
        case param of 
            Nothing -> prettyPrint expr1
            Just (op, expr2) ->
                (prettyPrint expr1) ++ (prettyPrint op) ++
                (prettyPrint expr2)

-- ASTRelationalOperator = RelationalOperator
instance PrettyPrint P.ASTRelationalOperator where
    prettyPrint Equal = "="
    prettyPrint NotEqual = "<>"
    prettyPrint LessThan = "<"
    prettyPrint GreaterThan = ">"
    prettyPrint LessThanOrEqual = "<="
    prettyPrint GreaterThanOrEqual = ">="

-- ASTSimpleExpression =
--    (Maybe Sign, ASTTerm,
--      [(ASTAddingOperator, ASTTerm)])
instance PrettyPrint P.ASTSimpleExpression where
    prettyPrint (sign, astTerm1, [(opeartor, astTerm2)]) =
        case sign of 
            Nothing ->
                (prettyPrint astTerm1) ++ "{" ++
                (prettyPrint opeartor) ++ "}"
            Just sign ->
                "[" ++ (prettyPrint sign) ++ "]" ++ 
                (prettyPrint astTerm1) ++ "{" ++ 
                (prettyPrint opeartor) ++ "}"

-- ASTAddingOperator = AddingOperator
instance PrettyPrint P.ASTAddingOperator where
    prettyPrint Plus = "+"
    prettyPrint Minus = "-"

-- ASTTerm = (ASTFactor, [(ASTMutiplayingOperator, ASTFactor)])
instance PrettyPrint P.ASTTerm where
    prettyPrint (factor1, [(opeartor, factor2)]) =  
        (prettyPrint factor1) ++ " {" ++
        (prettyPrint opeartor) ++ " " ++ "factor2" ++ " }" ++ "\n;"

-- type ASTMutiplayingOperator = MutiplayingOperator
instance PrettyPrint P.ASTMutiplayingOperator where
    prettyPrint P.Times = "*"
    prettyPrint P.DivideBy = "/"
    prettyPrint P.Div = "div"
    prettyPrint P.And = "and"

-- ASTFactor = Factor
instance PrettyPrint P.ASTFactor where
    prettyPrint (UnsignedConstant const) = prettyPrint const
    prettyPrint (VariableAccess var) = prettyPrint var
    prettyPrint (Expression expr) = "(" ++ (prettyPrint expr) ++ ")"
    prettyPrint (NotFactor factor) = "not " ++ (prettyPrint factor)

-- ASTVariableAccess = VariableAccess
instance PrettyPrint P.ASTVariableAccess where
    prettyPrint (IndexedVariable var) = prettyPrint var
    prettyPrint (Identifier id) = prettyPrint id

-- ASTIndexedVariable = (ASTIdentifier, ASTExpression)
instance PrettyPrint P.ASTIndexedVariable where
    prettyPrint (id, astExp) = 
        (prettyPrint id) ++ "(" ++ (prettyPrint astExp) ++ ")"

instance PrettyPrint P.ASTUnsignedNumber where
    prettyPrint (UnsignedInteger int) = prettyPrint int
    prettyPrint (UnsignedReal real) = prettyPrint real

instance PrettyPrint P.ASTUnsignedConstant where
    prettyPrint (UnsignedNumber num) = prettyPrint num
    prettyPrint (CharacterString str) = prettyPrint str

instance PrettyPrint L.ASTUnsignedReal where
    prettyPrint = show -- TODO

--------------------
-- variables section
--------------------

-- ASTVariableDeclarationPart = [ASTVariableDeclaration]
instance PrettyPrint P.ASTVariableDeclarationPart where
    prettyPrint [] = ""
    prettyPrint xs =
        "var\n" ++ (showVariableDeclarationPart xs)

showVariableDeclarationPart :: P.ASTVariableDeclarationPart -> String
showVariableDeclarationPart [] = ""
showVariableDeclarationPart (x:xs) =
    "    " ++ (prettyPrint x) ++ "\n"
    ++ showVariableDeclarationPart xs

-- ASTVariableDeclaration = (ASTIdentifierList, ASTTypeDenoter)
instance PrettyPrint P.ASTVariableDeclaration where
    prettyPrint (ids, typ) =
        (prettyPrint ids) ++ ": " ++ (prettyPrint typ) ++ ";"

-- ASTIdentifierList = [ASTIdentifier]
instance PrettyPrint P.ASTIdentifierList where
    prettyPrint [] = ""
    prettyPrint (x:[]) = id x
    prettyPrint (x:xs) =
        (prettyPrint x) ++ ", " ++ (prettyPrint xs)

-- ASTIdentifier = String
-- Use `id` instead of `show` to avoid quotes ("").
instance PrettyPrint L.ASTIdentifier where
    prettyPrint = id

-- ASTTypeDenoter = OrdinaryTypeDenoter | ArrayTypeDenoter
instance PrettyPrint P.ASTTypeDenoter where
    prettyPrint (OrdinaryTypeDenoter x) = prettyPrint x
    prettyPrint (ArrayTypeDenoter x) = prettyPrint x

instance PrettyPrint P.ASTTypeIdentifier where
    prettyPrint IntegerTypeIdentifier = "integer"
    prettyPrint RealTypeIdentifier = "real"
    prettyPrint BooleanTypeIdentifier = "boolean"

instance PrettyPrint P.ASTArrayType where
    prettyPrint (subTyp, typId) =
        "array [" ++ (prettyPrint subTyp) ++ "] of " ++
        (prettyPrint typId)

instance PrettyPrint P.ASTSubrangeType where
    prettyPrint (const1, const2) =
        (prettyPrint const1) ++ ".." ++ (prettyPrint const2)

instance PrettyPrint P.ASTConstant where
    prettyPrint (Nothing, n) = prettyPrint n
    prettyPrint ((Just sign), n) =
        (prettyPrint sign) ++ (prettyPrint n)

instance PrettyPrint L.ASTUnsignedInteger where
    prettyPrint = show

instance PrettyPrint P.ASTSign where
    prettyPrint P.SignPlus = "+"
    prettyPrint P.SignMinus = "-"
