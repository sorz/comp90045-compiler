{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

-- Module PazFormat contains the implementation of pretty printing.
--
-- The module was organized as following:
--   * Definition of PrettyPrint and helper functions
--   * Program section
--   * Statement section
--   * Procedures section
--   * Expressions section
--   * Variables section
--   * Lexer section
module PazFormat where
import PazLexer as L
import PazParser as P

-- All AST types are instances of PrettyPrint.
class PrettyPrint ast where
    prettyPrint :: ast -> String

-- Default implementation for (Maybe AST).
instance PrettyPrint ast => PrettyPrint (Maybe ast) where
    prettyPrint Nothing = ""
    prettyPrint (Just x) = prettyPrint x

-- Helper to easily print a sequence of things.
-- ast1 +++ ast2 +++ "a string" is equivalent to
-- (prettyPrint ast1) ++ (prettyPrint ast2) ++ "a string"
(+++) :: PrettyPrint a => PrettyPrint b => a -> b -> String
(+++) a b = (prettyPrint a) ++ (prettyPrint b)

-- Statement with a ident (four space before each lines).
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

-- Print list of ast, separated by sep
printSepBy :: PrettyPrint ast => String -> [ast] -> String
printSepBy sep (a:[]) = prettyPrint a
printSepBy sep (a:b:xs) = 
    a +++ sep ++ (printSepBy sep (b:xs))

------------------
-- program section
------------------

-- Generate formatted source code of whole program from AST.
-- ASTProgram =
--   (ASTIdentifier, ASTVariableDeclarationPart,
--    ASTProcedureDeclarationPart, ASTCompoundStatement)
instance PrettyPrint P.ASTProgram where
    prettyPrint (id, variable, procedure, statement) =
        "program " +++ id +++ ";\n" +++
        variableText +++ procedure +++ "\n" +++
        statement +++ "." where
        -- extra blank line only if VariableDeclarationPart present
            variableText = case variable of
                [] -> ""
                otherwise -> "\n" +++ variable

instance PrettyPrint P.ASTProcedureDeclarationPart where
    prettyPrint [] = ""
    prettyPrint (x:xs) = x +++ ";\n" +++ xs

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
    prettyPrint (ReadStatement s) = "read(" +++ s +++ ")"
    prettyPrint (WriteStatement s) = "write(" +++ s +++ ")"
    prettyPrint (WritelnStatement) = "writeln"
    prettyPrint (WriteStringStatement s) =
        "write('" ++ (concatMap repl s) ++ "')"
        where repl '\'' = "''"
              repl c = [c]

-- assignment statement
instance PrettyPrint P.ASTAssignmentStatement where 
    prettyPrint (left, expr) =
        left +++ " := " +++ expr

-- procedure statement (call)
instance PrettyPrint P.ASTProcedureStatement where
    prettyPrint (id, Nothing) = prettyPrint id
    prettyPrint (id, Just params) =
        id +++ "(" +++ params +++ ")"

instance PrettyPrint P.ASTActualParameterList where
    prettyPrint (a:[]) = prettyPrint a 
    prettyPrint (a:b:xs) =
        a +++ ", " +++ (b:xs)

-- while/if/for statements
-- print with ident expect CompoundStatement
printWithOptionIdent :: ASTStatement -> String
printWithOptionIdent (CompoundStatement s) =
    prettyPrint (CompoundStatement s)
printWithOptionIdent s = prettyPrint (Ident s)

-- while statement
instance PrettyPrint P.ASTWhileStatement where
    prettyPrint (expr, stat) =
        "while " +++ expr +++ " do\n" ++
        (printWithOptionIdent stat)
    
-- if statement
instance PrettyPrint P.ASTIfStatement where 
    prettyPrint (expr, stat1, Nothing) =
        "if " +++ expr +++ " then\n" +++
        (printWithOptionIdent stat1)
    prettyPrint (expr, stat1, (Just stat2)) =
        ((expr, stat1, Nothing) :: ASTIfStatement) +++
        "\nelse\n" +++ (printWithOptionIdent stat2)

-- for statement
instance PrettyPrint P.ASTForStatement where 
    prettyPrint (id , expr1, to, expr2, stat) = 
          "for " +++ id +++ " := " +++ expr1 +++ to +++
          expr2 +++ " do\n" +++ (printWithOptionIdent stat)

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
        "\nprocedure " +++ id +++ param +++ ";\n" +++
        var +++ stat

-- ASTFormalParameterList = [ASTFormalParameterSection]
instance PrettyPrint P.ASTFormalParameterList where
    prettyPrint params = "(" ++ (printSepBy "; " params) ++ ")"

-- ASTFormalParameterSection = (Bool, ASTIdentifierList, ASTTypeDenoter)
instance PrettyPrint P.ASTFormalParameterSection where
    prettyPrint (True, ids, typ) = "var " +++ (False, ids, typ)
    prettyPrint (False, ids, typ) = ids +++ ": " +++ typ

-- ASTIdentifierList = [ASTIdentifier]
instance PrettyPrint P.ASTIdentifierList where
    prettyPrint = printSepBy ", "

----------------------
-- expressions section
----------------------

data ExprOpPriority =
    EORel | EOAdd | EOMul | EONot | EONul
    deriving (Show, Eq, Ord)

-- If the priority of expr is lower than p1, print expr inside
-- parentheses.
printExpr :: ExprOpPriority -> P.ASTExpression -> String
printExpr p1 expr
    | p1 > p2   = "(" +++ expr +++ ")"
    | otherwise = prettyPrint expr
    where
        p2 = case expr of
            RelOp _ _ _ -> EORel
            SignOp _ _  -> EOAdd
            AddOp _ _ _ -> EOAdd
            MulOp _ _ _ -> EOMul
            NotOp _     -> EONot
            otherwise   -> EONul

instance PrettyPrint P.ASTExpression where
    prettyPrint (RelOp op expr1 expr2) =
        (printExpr EOAdd expr1) +++ op +++
        (printExpr EOAdd expr2)
    prettyPrint (SignOp op expr) =
        op +++ (printExpr EOMul expr)
    prettyPrint (AddOp op expr1 expr2) =
        (printExpr EOAdd expr1) +++ op +++
        (printExpr EOMul expr2)
    prettyPrint (MulOp op expr1 expr2) =
        (printExpr EOMul expr1) +++ op +++
        (printExpr EONot expr2)
    prettyPrint (NotOp expr) =
        "not " ++ (printExpr EONot expr)
    prettyPrint (Const const) = prettyPrint const
    prettyPrint (Var var) = prettyPrint var

instance PrettyPrint P.ASTRelationalOperator where
    prettyPrint Equal = " = "
    prettyPrint NotEqual = " <> "
    prettyPrint LessThan = " < "
    prettyPrint GreaterThan = " > "
    prettyPrint LessEqual = " <= "
    prettyPrint GreaterEqual = " >= "

instance PrettyPrint P.ASTAddingOperator where
    prettyPrint Plus = " + "
    prettyPrint Minus = " - "
    prettyPrint Or = " or "

instance PrettyPrint P.ASTMutiplayingOperator where
    prettyPrint P.Times = " * "
    prettyPrint P.DivideBy = " / "
    prettyPrint P.Div = " div "
    prettyPrint P.And = " and "

instance PrettyPrint P.ASTVariableAccess where
    prettyPrint (IndexedVariable var) = prettyPrint var
    prettyPrint (Identifier id) = prettyPrint id

-- ASTIndexedVariable = (ASTIdentifier, ASTExpression)
instance PrettyPrint P.ASTIndexedVariable where
    prettyPrint (id, astExp) = 
        id +++ "[" +++ astExp +++ "]"

instance PrettyPrint P.ASTUnsignedConstant where
    prettyPrint (Boolean bool) = show bool
    prettyPrint (UnsignedInteger int) = prettyPrint int
    prettyPrint (UnsignedReal real) = prettyPrint real

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
    "    " +++ x ++ "\n"
    ++ (printVariableDeclarationPart xs)

-- ASTVariableDeclaration = (ASTIdentifierList, ASTTypeDenoter)
instance PrettyPrint P.ASTVariableDeclaration where
    prettyPrint (ids, typ) =
        ids +++ ": " +++ typ +++ ";"

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
        "array[" +++ subTyp +++ "] of " +++ typId

instance PrettyPrint P.ASTSubrangeType where
    prettyPrint (const1, const2) =
        const1 +++ ".." +++ const2

instance PrettyPrint P.ASTConstant where
    prettyPrint (sign, n) =
        sign +++ n

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
    prettyPrint = show

instance PrettyPrint L.ASTScaleFactor where
    prettyPrint (sign, num) =
        "e" +++ sign +++ num

instance PrettyPrint L.ASTSign where
    prettyPrint L.SignPlus = "+"
    prettyPrint L.SignMinus = "-"
