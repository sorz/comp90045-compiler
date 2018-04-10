{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module PazFormat where
import PazLexer as L
import PazParser as P

class PrettyPrint ast where
    prettyPrint :: ast -> String

instance PrettyPrint ast => PrettyPrint (Maybe ast) where
    prettyPrint Nothing = ""
    prettyPrint (Just x) = prettyPrint x

-- statement with a ident (four space before each lines)
data Ident stat where
    Ident :: ASTStatement -> Ident stat

instance PrettyPrint (Ident stat) where
    prettyPrint (Ident stat) = unlines' (map ident lns) where
        lns = lines (prettyPrint stat)
        -- add ident to non-blank line
        ident "" = ""
        ident l = "    " ++ l
        -- like unlines, but without terminating newline
        unlines' [] = ""
        unlines' (a:[]) = a
        unlines' (a:b:xs) = a ++ "\n" ++ (unlines' (b:xs))

-- print list of ast, separated by sep
printSepBy :: PrettyPrint ast => String -> [ast] -> String
printSepBy sep (a:[]) = prettyPrint a
printSepBy sep (a:b:xs) = 
    (prettyPrint a) ++ sep ++ (printSepBy sep (b:xs))

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
        (prettyPrint procedure) ++ "\n" ++
        (prettyPrint statement) ++ "."

instance PrettyPrint P.ASTProcedureDeclarationPart where
    prettyPrint [] = ""
    prettyPrint (x:xs) =
        (prettyPrint x) ++ ";\n" ++ (prettyPrint xs)

--------------------
-- statement section
--------------------

-- ASTCompoundStatement = ASTStatementSequence = [ASTStatement]
instance PrettyPrint P.ASTCompoundStatement where 
    prettyPrint seq =
        let seq' = map Ident seq in
            "begin\n" ++ (printSepBy ";\n" seq') ++ "\nend"

instance PrettyPrint P.ASTStatement where 
    prettyPrint (AssignmentStatement s) = prettyPrint s
    prettyPrint (ProcedureStatement s) = prettyPrint s
    prettyPrint (CompoundStatement s) = prettyPrint s
    prettyPrint (IfStatement s) = prettyPrint s
    prettyPrint (WhileStatement s) = prettyPrint s
    prettyPrint (ForStatement s) = prettyPrint s
    prettyPrint EmptyStatement = ""

-- assignment statement
instance PrettyPrint P.ASTAssignmentStatement where 
    prettyPrint (left, expr) =
        (prettyPrint left) ++ " := " ++ (prettyPrint expr)

instance PrettyPrint P.AssignmentLeft where
    prettyPrint (AssignVariableAccess var) = prettyPrint var
    prettyPrint (AssignIdentifier id) = prettyPrint id

-- procedure statement (call)
instance PrettyPrint P.ASTProcedureStatement where
    prettyPrint (id, Nothing) = prettyPrint id
    prettyPrint (id, Just params) =
        (prettyPrint id) ++ "(" ++ (prettyPrint params) ++ ")"

instance PrettyPrint P.ASTActualParameterList where
    prettyPrint (a:[]) = prettyPrint a 
    prettyPrint (a:b:xs) =
        (prettyPrint a) ++ ", " ++ (prettyPrint (b:xs))

-- while/if/for statements
-- print with ident expect CompoundStatement
printWithOptionIdent :: ASTStatement -> String
printWithOptionIdent (CompoundStatement s) =
    prettyPrint (CompoundStatement s)
printWithOptionIdent s = prettyPrint (Ident s)

-- while statement
instance PrettyPrint P.ASTWhileStatement where
    prettyPrint (expr, stat) =
        "while " ++ (prettyPrint expr) ++ " do\n" ++
        (printWithOptionIdent stat)
    
-- if statement
instance PrettyPrint P.ASTIfStatement where 
    prettyPrint (expr, stat1, Nothing) =
        "if " ++ (prettyPrint expr) ++ " then\n" ++
        (printWithOptionIdent stat1)
    prettyPrint (expr, stat1, (Just stat2)) =
        (prettyPrint ((expr, stat1, Nothing) :: ASTIfStatement)) ++
        "\nelse\n" ++ (printWithOptionIdent stat2)

-- for statement
instance PrettyPrint P.ASTForStatement where 
    prettyPrint (id , expr1, to, expr2, stat) = 
          "for " ++ (prettyPrint id) ++ " := " ++ (prettyPrint expr1) ++
          (prettyPrint to) ++ (prettyPrint expr2) ++ "do" ++
          (printWithOptionIdent stat)

instance PrettyPrint P.ForDirection where
    prettyPrint ForTo = " to "
    prettyPrint ForDownTo = " downto "

---------------------
-- procedures section
---------------------

-- ASTProcedureDeclaration =
--     (ASTIdentifier, (Maybe ASTFormalParameterList),
--         ASTVariableDeclarationPart, ASTCompoundStatement)
instance PrettyPrint P.ASTProcedureDeclaration where
    prettyPrint (id, param, var, stat) =
        "\nprocedure " ++ (prettyPrint id) ++
        (prettyPrint param) ++ ";\n" ++
        (prettyPrint var) ++ (prettyPrint stat)

-- ASTFormalParameterList = [ASTFormalParameterSection]
instance PrettyPrint P.ASTFormalParameterList where
    prettyPrint params = "(" ++ (printSepBy "; " params) ++ ")"

-- ASTFormalParameterSection = (Bool, ASTIdentifierList, ASTTypeDenoter)
instance PrettyPrint P.ASTFormalParameterSection where
    prettyPrint (True, ids, typ) =
        "var " ++ (prettyPrint (False, ids, typ))
    prettyPrint (False, ids, typ) =
        (prettyPrint ids) ++ ": " ++ (prettyPrint typ)

-- ASTIdentifierList = [ASTIdentifier]
instance PrettyPrint P.ASTIdentifierList where
    prettyPrint = printSepBy ", "

----------------------
-- expressions section
----------------------

-- ASTExpression =
--     (ASTSimpleExpression,
--     Maybe (ASTRelationalOperator, ASTSimpleExpression))
instance PrettyPrint P.ASTExpression where
    prettyPrint (expr1, Nothing) = prettyPrint expr1
    prettyPrint (expr1, Just (op, expr2)) =
        (prettyPrint expr1) ++ " " ++ (prettyPrint op) ++ " " ++
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
    prettyPrint (sign, term, opTerms) =
        (prettyPrint sign) ++ (prettyPrint term) ++
        (prettyPrint opTerms)

instance PrettyPrint [(P.ASTAddingOperator, P.ASTTerm)] where
    prettyPrint [] = ""
    prettyPrint ((op, term):xs) =
        (prettyPrint op) ++ (prettyPrint term) ++
        (prettyPrint xs)

instance PrettyPrint P.ASTAddingOperator where
    prettyPrint Plus = " + "
    prettyPrint Minus = " - "
    prettyPrint Or = " or "

-- ASTTerm = (ASTFactor, [(ASTMutiplayingOperator, ASTFactor)])
instance PrettyPrint P.ASTTerm where
    prettyPrint (factor, opFactors) =
        (prettyPrint factor) ++ (prettyPrint opFactors)

instance PrettyPrint [(ASTMutiplayingOperator, ASTFactor)] where
    prettyPrint [] = ""
    prettyPrint ((op, factor):xs) =
        (prettyPrint op) ++ (prettyPrint factor) ++
        (prettyPrint xs)

instance PrettyPrint P.ASTMutiplayingOperator where
    prettyPrint P.Times = " * "
    prettyPrint P.DivideBy = " / "
    prettyPrint P.Div = " div "
    prettyPrint P.And = " and "

instance PrettyPrint P.ASTFactor where
    prettyPrint (UnsignedConstant const) = prettyPrint const
    prettyPrint (VariableAccess var) = prettyPrint var
    prettyPrint (Expression expr) = "(" ++ (prettyPrint expr) ++ ")"
    prettyPrint (NotFactor factor) = "not " ++ (prettyPrint factor)

instance PrettyPrint P.ASTVariableAccess where
    prettyPrint (IndexedVariable var) = prettyPrint var
    prettyPrint (Identifier id) = prettyPrint id

-- ASTIndexedVariable = (ASTIdentifier, ASTExpression)
instance PrettyPrint P.ASTIndexedVariable where
    prettyPrint (id, astExp) = 
        (prettyPrint id) ++ "[" ++ (prettyPrint astExp) ++ "]"

instance PrettyPrint P.ASTUnsignedNumber where
    prettyPrint (UnsignedInteger int) = prettyPrint int
    prettyPrint (UnsignedReal real) = prettyPrint real

instance PrettyPrint P.ASTUnsignedConstant where
    prettyPrint (UnsignedNumber num) = prettyPrint num
    prettyPrint (CharacterString str) =
        "'" ++ (concatMap repl (prettyPrint str)) ++ "'"
        where repl '\'' = "''"
              repl c = [c]

--------------------
-- variables section
--------------------

-- ASTVariableDeclarationPart = [ASTVariableDeclaration]
instance PrettyPrint P.ASTVariableDeclarationPart where
    prettyPrint [] = ""
    prettyPrint xs =
        "var\n" ++ (printVariableDeclarationPart xs)

printVariableDeclarationPart :: P.ASTVariableDeclarationPart -> String
printVariableDeclarationPart [] = ""
printVariableDeclarationPart (x:xs) =
    "    " ++ (prettyPrint x) ++ "\n"
    ++ printVariableDeclarationPart xs

-- ASTVariableDeclaration = (ASTIdentifierList, ASTTypeDenoter)
instance PrettyPrint P.ASTVariableDeclaration where
    prettyPrint (ids, typ) =
        (prettyPrint ids) ++ ": " ++ (prettyPrint typ) ++ ";"

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
    prettyPrint (sign, n) =
        (prettyPrint sign) ++ (prettyPrint n)

----------------
-- lexer section
-----------------

-- ASTUnsignedInteger = Integer, so we just `show` it.
instance PrettyPrint L.ASTUnsignedInteger where
    prettyPrint = show

instance PrettyPrint P.ASTSign where
    prettyPrint P.SignPlus = "+"
    prettyPrint P.SignMinus = "-"

-- ASTIdentifier = ASTDigitSequence = String
-- Use `id` instead of `show` to avoid quotes ("").
instance PrettyPrint String where
    prettyPrint = id

instance PrettyPrint L.ASTUnsignedReal where
    prettyPrint (int, Nothing, scale) =
        (prettyPrint int) ++ (prettyPrint scale)
    prettyPrint (int, Just float, scale) =
        (prettyPrint int) ++ '.':(prettyPrint float) ++ (prettyPrint scale)

instance PrettyPrint L.ASTScaleFactor where
    prettyPrint (sign, num) =
        'e':(prettyPrint sign) ++ (prettyPrint num)

instance PrettyPrint L.ASTSign where
    prettyPrint L.SignPlus = "+"
    prettyPrint L.SignMinus = "-"
