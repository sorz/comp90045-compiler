module PazCompiler where

import Control.Applicative
import Control.Monad
import Data.Map (
    Map,
    (!),
    (!?)
    )
import qualified Data.Map as Map
import PazLexer as L
import PazParser as P
import PazFormat (
    prettyPrintString,
    (+++)
    )

-- this is the entry point to the compiler from the Paz.hs driver module
compileStartSymbol :: P.ASTStartSymbol -> String
compileStartSymbol =
    compileProgram

-- the following is a suggestion for how your compiler can be structured,
-- there is no requirement to follow this template but it shows how the
-- important information (such as current label number) can be threaded
-- through the functions that implement the various parts of the compiler
-- (the more advanced students might wish to use a state monad for this)
data State = State
    { procedures   :: Map String [(Bool, ASTTypeDenoter)]
    , variables    :: Map String (Bool, ASTTypeDenoter, Int)
    , labelCounter :: Int
    , slotCounter  :: Int
    , regCounter   :: Int
    , code         :: String
    }
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

clearSlotCounter :: CodeGen ()
clearSlotCounter =
    CodeGen (\st -> ((), st { slotCounter = 0 }))

nextLabel :: CodeGen String
nextLabel = do
    st <- getState
    CodeGen (\st -> ((), st { labelCounter = (labelCounter st) + 1 }))
    return $ "label" ++ (show $ labelCounter st)

putLabel :: String -> CodeGen ()
putLabel l = putCode $ l ++ ":\n"

register :: Int -> String
register n = "r" ++ (show n)

nextRegister :: CodeGen String
nextRegister = do
    st <- getState
    reg <- return $ regCounter st
    CodeGen (\st -> ((), st { regCounter = reg + 1 }))
    if reg > 1023
        then error "number of registers exceeds 1023"
        else return $ register reg

resetRegisterCounter :: Int -> CodeGen ()
resetRegisterCounter c =
    CodeGen (\st -> ((), st { regCounter = c }))

putProcedure :: String -> [(Bool, ASTTypeDenoter)] -> CodeGen ()
putProcedure id params = CodeGen (\st ->
    let procs = if Map.member id (procedures st)
            then error $ "duplicated procedure " ++ id
            else Map.insert id params (procedures st) in
        ((), st { procedures = procs })
    )

-- return nth of paramter in given procedure
getProcedureParameter :: String -> Int -> CodeGen (Bool, ASTTypeDenoter)
getProcedureParameter id n = do
    st <- getState
    return $ (procedures st) ! id !! n


putVariable :: Bool -> String -> ASTTypeDenoter -> CodeGen ()
putVariable isVar id typ = CodeGen (\st ->
    let sc = slotCounter st
        n = typeSizeOf typ
        vars = if Map.member id (variables st)
            then error $ "duplicated paramaters or variables " ++ id
            else Map.insert id (isVar, typ, sc) (variables st) in
        ((), st { variables = vars, slotCounter = sc + n })
    )

getVariable :: ASTIdentifier -> CodeGen (Bool, ASTTypeDenoter, Int)
getVariable id = do
    st <- getState
    case (variables st) !? id of
        Nothing -> error $ "undefined variable or parameter " ++ id
        Just v  -> return v

clearVariables :: CodeGen ()
clearVariables =
    CodeGen (\st -> ((), st { variables = Map.empty }))

-- append code string to internal state
putCode :: String -> CodeGen ()
putCode c = CodeGen (\st -> ((), st { code = (code st) ++ c }))

-- get code string from state & clear code.
getCode :: CodeGen String
getCode = do
    st <- getState
    CodeGen (\st -> ((), st { code = "" }))
    return $ code st

-- put opcode into code
putOp :: String -> [String] -> CodeGen ()
putOp op [] = putCode $ "    " ++ op ++ "\n"
putOp op args =
    putCode $ "    " ++ op ++ " " ++ (gen args) ++ "\n"
    where gen (a:[]) = a
          gen (a:b:xs) = a ++ ", " ++ (gen $ b:xs)

putComment :: String -> CodeGen ()
putComment s = putCode $ "# " ++ s ++ "\n"

-- helper functions for AST

-- return the number of slot the variable take
typeSizeOf :: ASTTypeDenoter -> Int
typeSizeOf (OrdinaryTypeDenoter _) = 1
typeSizeOf (ArrayTypeDenoter ((lo, hi), _)) =
    if lo <= hi
        then hi - lo + 1
        else error $ "expected m <= n in array[m..n], found "
                     +++ hi ++ " and " +++ lo

primitiveType :: ASTTypeDenoter -> ASTTypeIdentifier
primitiveType (OrdinaryTypeDenoter t) = t
primitiveType (ArrayTypeDenoter (_, t)) = t

-- main function

compileProgram :: ASTProgram -> String
compileProgram (name, varDecls, procDecls, bodyStatement) =
    let
        state = State
            { procedures    = Map.empty
            , variables     = Map.empty
            , labelCounter  = 0
            , slotCounter   = 0
            , regCounter    = 0
            , code          = ""
            }
        gen = do
            -- precompile
            precompileProcedureDeclarationPart procDecls
            -- producures
            compileProcedureDeclarationPart procDecls
            procText <- getCode
            -- main producure
            slot <- compileVariableDeclarationPart varDecls
            compileCompoundStatement bodyStatement
            bodyText <- getCode
            -- return
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
compileVariableDeclarationPart :: ASTVariableDeclarationPart -> CodeGen Int
compileVariableDeclarationPart [] = do
    st <- getState
    return $ slotCounter st
compileVariableDeclarationPart (x:xs) = do
    compileVariableDeclaration x
    compileVariableDeclarationPart xs

compileVariableDeclaration :: ASTVariableDeclaration -> CodeGen ()
compileVariableDeclaration ([], typ) = return ()
compileVariableDeclaration ((id:ids), typ) = do
    putVariable False id typ
    compileVariableDeclaration (ids, typ)

-- compile a list of procedures
-- takes a label number, a symbol table and an AST fragment
-- returns the advanced label number and the generated code
compileProcedureDeclarationPart :: ASTProcedureDeclarationPart -> CodeGen Int
compileProcedureDeclarationPart [] = do
    st <- getState
    return $ slotCounter st
compileProcedureDeclarationPart (x:xs) = do
    compileProcedureDeclaration x
    compileProcedureDeclarationPart xs

compileProcedureDeclaration :: ASTProcedureDeclaration -> CodeGen ()
compileProcedureDeclaration (id, params, vars, body) = do
    putLabel id
    putComment "prologue"
    paramSlot <- compileFormalParameterList params
    totalSlot <- compileVariableDeclarationPart vars
    putOp "push_stack_frame" [show totalSlot]
    storeProcedureParamters paramSlot paramSlot

    compileCompoundStatement body

    putComment "epilogue"
    putOp "pop_stack_frame" [show totalSlot]
    putOp "return" []
    clearVariables
    clearSlotCounter

compileFormalParameterList :: ASTFormalParameterList -> CodeGen Int
compileFormalParameterList [] = do
    st <- getState
    return $ slotCounter st
compileFormalParameterList (x:xs) = do
    compileFormalParameterSection x
    compileFormalParameterList xs

compileFormalParameterSection :: ASTFormalParameterSection -> CodeGen ()
compileFormalParameterSection (isVar, [], typ) = return ()
compileFormalParameterSection (isVar, (id:ids), typ) = do
    putVariable isVar id typ
    compileFormalParameterSection (isVar, ids, typ)

storeProcedureParamters :: Int -> Int -> CodeGen ()
storeProcedureParamters 0 _ = resetRegisterCounter 0
storeProcedureParamters left total = do
    reg <- nextRegister
    putOp "store" [show $ total - left, reg]
    storeProcedureParamters (left - 1) total

-- compile a list of statements
-- takes a label number, a symbol table and an AST fragment
-- returns the advanced label number and the generated code
compileCompoundStatement :: ASTCompoundStatement -> CodeGen ()
compileCompoundStatement [] = return ()
compileCompoundStatement (x : xs) = do
    compileStatement x
    resetRegisterCounter 0
    compileCompoundStatement xs

compileStatement :: ASTStatement -> CodeGen ()
compileStatement EmptyStatement           = return ()
compileStatement WritelnStatement         = compileWritelnStatement
compileStatement (WriteStringStatement s) = compileWriteStringStatement s
compileStatement (WriteStatement s)       = compileWriteStatement s
compileStatement (ReadStatement s)        = compileReadStatement s
compileStatement (AssignmentStatement s)  = compileAssignmentStatement s
compileStatement (IfStatement s)          = compileIfStatement s
compileStatement (WhileStatement s)       = compileWhileStatement s
compileStatement (ForStatement s)         = compileForStatement s
compileStatement (ProcedureStatement s)   = compileProcedureStatement s
compileStatement (CompoundStatement s)    = do
    putComment "begin"
    compileCompoundStatement s
    putComment "end"

-- compile write statement

compileWriteStringStatement :: ASTWriteStringStatement -> CodeGen ()
compileWriteStringStatement str = do
    putComment "write string"
    putOp "string_const" ["r0", prettyPrintString str]
    putOp "call_builtin" ["print_string"]

compileWriteStatement :: ASTWriteStatement -> CodeGen ()
compileWriteStatement expr = do
    putComment "write"
    (reg, typ) <- compileExpression expr
    func <- case typ of
        IntegerTypeIdentifier -> return "print_int"
        RealTypeIdentifier    -> return "print_real"
        BooleanTypeIdentifier -> return "print_bool"
    if reg /= "r0"
        then putOp "move" ["r0", reg]
        else return ()
    putOp "call_builtin" [func]

compileWritelnStatement :: CodeGen ()
compileWritelnStatement = do
    putComment "writeln"
    putOp "call_builtin" ["print_newline"]

-- compile read statement
compileReadStatement :: ASTVariableAccess -> CodeGen ()
compileReadStatement var = do
    putComment "read"
    typ <- variableType var
    func <- return $ case typ of
        IntegerTypeIdentifier -> "read_int"
        RealTypeIdentifier    -> "read_real"
        BooleanTypeIdentifier -> "read_bool"
    putOp "call_builtin" [func]
    resetRegisterCounter 1
    storeVariable var "r0"

-- compile if statement
compileIfStatement :: ASTIfStatement -> CodeGen ()
compileIfStatement (expr, stat0, stat1) = do
    putComment "if"
    reg <- compileBooleanExpression expr
    labelElse <- nextLabel
    putOp "branch_on_false" [reg, labelElse]
    compileStatement stat0
    case stat1 of
        Nothing -> putLabel labelElse
        Just stat -> do
            labelEnd <- nextLabel
            putOp "branch_uncond" [labelEnd]
            putLabel labelElse
            compileStatement stat
            putLabel labelEnd
    putComment "end if"

-- compile while statement
compileWhileStatement :: ASTWhileStatement -> CodeGen ()
compileWhileStatement (expr, stat) = do
    putComment "while"
    labelStart <- nextLabel
    labelEnd   <- nextLabel
    putLabel labelStart
    reg <- compileBooleanExpression expr
    putOp "branch_on_false" [reg, labelEnd]
    compileStatement stat
    putOp "branch_uncond" [labelStart]
    putLabel labelEnd

-- compile for statement
compileForStatement :: ASTForStatement -> CodeGen ()
compileForStatement (i, initExpr, dir, endExpr, stat) = do
    putComment "for"
    i' <- return $ Identifier i  -- convert to ASTVariableAccess
    iVar <- return $ Var i'      -- convert to ASTExpression
    typ <- variableType i'
    if typ /= IntegerTypeIdentifier
        then error $ "expected integer in for statement, found " +++ typ
        else return ()
    (relOp, addOp) <- return $ case dir of
        ForTo     -> (LessEqual, Plus)
        ForDownTo -> (GreaterEqual, Minus)
    expr  <- return $ RelOp relOp iVar endExpr
    adder <- return $ AssignmentStatement
        (i', AddOp addOp iVar (P.Const (UnsignedInteger 1)))
    stat' <- return $ CompoundStatement $ [stat] ++ [adder]
    compileAssignmentStatement (i', initExpr)
    compileWhileStatement (expr, stat')

-- helper function for if & while compiler:
-- compile expr and panic if it's not a boolean expr.
compileBooleanExpression :: ASTExpression -> CodeGen String
compileBooleanExpression expr = do
    (reg, typ) <- compileExpression expr
    case typ of
        BooleanTypeIdentifier -> return reg
        otherwise -> error $ "expected boolean expression, found " +++ typ

-- compile procedure call

compileProcedureStatement :: ASTProcedureStatement -> CodeGen ()
compileProcedureStatement (id, params) = do
    st <- getState
    case (procedures st) !? id of
        Nothing -> error $ "call to undefined procedure " ++ id
        Just p -> if length p /= length params
            then error $ "expected " +++ length p ++
                         " parameter(s), found " +++ length params
            else compileActualParameterList id 0 params
    putOp "call" [id]

compileActualParameterList :: String -> Int ->
    ASTActualParameterList -> CodeGen ()
compileActualParameterList _ _ [] = return ()
compileActualParameterList id n (expr:params) = do
    reg <- return $ register n
    (isVar, typ) <- getProcedureParameter id n
    typ <- return $ primitiveType typ
    if isVar
        then case expr of
            Var var -> do
                typ' <- variableType var
                if typ' == typ then loadAddress reg var
                else error $ "expected " +++ typ +++ ", found " +++ typ'
            otherwise -> error "expected variable as parameter"
        else do
            resetRegisterCounter n
            (reg', typ') <- compileExpression expr
            case needCastType typ typ' of
                CastLeft  -> error "expected integer, found real"
                CastRight -> putOp "int_to_real" [reg, reg']
                NoNeed    -> if reg == reg' then return ()
                    else putOp "move" [reg, reg']
    compileActualParameterList id (n+1) params

-- compile assignment & expression

compileAssignmentStatement ::
    ASTAssignmentStatement -> CodeGen ()
compileAssignmentStatement (var, expr) = do
    putComment "assignment"
    ltype <- variableType var
    (rvalue, rtype) <- compileExpression expr
    case needCastType ltype rtype of
        CastRight -> putOp "int_to_real" [rvalue, rvalue]
        CastLeft  -> error $ "expected integer, found real"
        NoNeed    -> return ()
    storeVariable var rvalue

-- opeartions about variable access
variableType :: ASTVariableAccess -> CodeGen ASTTypeIdentifier
variableType (IndexedVariable (id, _)) = variableType (Identifier id)
variableType (Identifier id) = do
    (_, typ, _) <- getVariable id
    return $ primitiveType typ

-- store register to a variable access
storeVariable :: ASTVariableAccess -> String -> CodeGen ()
storeVariable (Identifier id) val = do
    (isVar, _, slot) <- getVariable id
    if not isVar
        then putOp "store" [show slot, val]
        else do
            addr <- nextRegister
            putOp "load" [addr, show slot]
            putOp "store_indirect" [addr, val]
storeVariable var val = do
    index <- nextRegister
    loadAddress index var
    putOp "store_indirect" [index, val]

-- load variable to register
loadVariable :: String -> ASTVariableAccess -> CodeGen ()
loadVariable reg (Identifier id) = do
    (isVar, _, slot) <- getVariable id
    putOp "load" [reg, show slot]
    if isVar
        then putOp "load_indirect" [reg, reg]
        else return ()
loadVariable reg var = do
    loadAddress reg var
    putOp "load_indirect" [reg, reg]

-- load address of a variable to register
loadAddress :: String -> ASTVariableAccess -> CodeGen ()
loadAddress reg (Identifier id) = do
    (isVar, _, slot) <- getVariable id
    if isVar
        then putOp "load" [reg, show slot]
        else putOp "load_address" [reg, show slot]
loadAddress reg (IndexedVariable (id, expr)) = do
    (index, typ) <- compileExpression expr
    if typ /= IntegerTypeIdentifier
        then error $ "expected int in array index, found " +++ typ
        else return ()
    (isVar, typ, slot) <- getVariable id
    case typ of
        (OrdinaryTypeDenoter _) ->
            error $ "expected arrary variable, found primitive " ++ id
        (ArrayTypeDenoter ((0, _), _)) ->
            return ()
        (ArrayTypeDenoter ((lo, _), _)) -> do
            lo' <- nextRegister
            putOp "int_const" [lo', show lo]
            putOp "sub_int" [index, index, lo']
    if isVar
        then putOp "load" [reg, show slot]
        else putOp "load_address" [reg, show slot]
    putOp "sub_offset" [reg, reg, index]

-- return register where the result of expression, with its type.
compileExpression :: ASTExpression -> CodeGen (String, ASTTypeIdentifier)
-- const access
compileExpression (P.Const const) = do
    reg <- nextRegister
    typ <- return $ case const of
        UnsignedInteger _ -> IntegerTypeIdentifier
        UnsignedReal    _ -> RealTypeIdentifier
        Boolean         _ -> BooleanTypeIdentifier
    case const of
        UnsignedInteger x -> putOp "int_const"  [reg, show x]
        UnsignedReal    x -> putOp "real_const" [reg, show x]
        Boolean True      -> putOp "int_const"  [reg, "1"]
        Boolean False     -> putOp "int_const"  [reg, "0"]
    return (reg, typ)
-- var access
compileExpression (Var var) = do
    typ <- variableType var
    reg <- nextRegister
    loadVariable reg var
    return (reg, typ)
-- sign op
compileExpression (SignOp sign expr) = do
    (reg, typ) <- compileExpression expr
    func <- case typ of
        IntegerTypeIdentifier -> return "neg_int"
        RealTypeIdentifier    -> return "neg_real"
        BooleanTypeIdentifier ->
            error $ "expected integer or real, found boolean"
    case sign of
        P.SignPlus  -> return ()
        P.SignMinus -> putOp func [reg, reg]
    return (reg, typ)
-- not op
compileExpression (NotOp expr) = do
    (reg, typ) <- compileExpression expr
    case typ of
        BooleanTypeIdentifier -> putOp "not" [reg, reg]
        otherwise -> error $ "expected bool, found " +++ typ
    return (reg, typ)
-- relation op
compileExpression (RelOp op expr0 expr1) = do
    (r0, r1, typ) <- unifyTypesInBinaryNumberExpr expr0 expr1
    func0 <- return $ case op of
        Equal        -> "cmp_eq_"
        NotEqual     -> "cmp_ne_"
        LessThan     -> "cmp_lt_"
        GreaterThan  -> "cmp_gt_"
        LessEqual    -> "cmp_le_"
        GreaterEqual -> "cmp_ge_"
    func1 <- return $ case typ of
        IntegerTypeIdentifier -> "int"
        RealTypeIdentifier    -> "real"
        otherwise -> error $ "expect integer or real, found boolean"
    putOp (func0 ++ func1) [r0, r0, r1]
    return (r0, BooleanTypeIdentifier)
-- adding op
compileExpression (AddOp op expr0 expr1) = do
    (r0, r1, typ) <- unifyTypesInBinaryNumberExpr expr0 expr1
    func <- return $ case (op, typ) of
        (Plus,  IntegerTypeIdentifier) -> "add_int"
        (Plus,  RealTypeIdentifier)    -> "add_real"
        (Minus, IntegerTypeIdentifier) -> "sub_int"
        (Minus, RealTypeIdentifier)    -> "sub_real"
        (Or,    BooleanTypeIdentifier) -> "or"
        otherwise -> error "unexpected types in adding operation"
    putOp func [r0, r0, r1]
    return (r0, typ)
-- mutiplaying op
compileExpression (MulOp op expr0 expr1) = do
    (r0, r1, typ) <- unifyTypesInBinaryNumberExpr expr0 expr1
    func <- case (op, typ) of
        (And,      BooleanTypeIdentifier) -> return "and"
        (Times,    IntegerTypeIdentifier) -> return "mul_int"
        (Times,    RealTypeIdentifier)    -> return "mul_real"
        (Div,      IntegerTypeIdentifier) -> return "div_int"
        (DivideBy, RealTypeIdentifier)    -> return "div_real"
        (DivideBy, IntegerTypeIdentifier) -> do
            putOp "int_to_real" [r0, r0]
            putOp "int_to_real" [r1, r1]
            return "div_real"
        otherwise -> error "unexpected types in mutiplaying operation"
    typ' <- return $ case op of
        DivideBy  -> RealTypeIdentifier
        otherwise -> typ
    putOp func [r0, r0, r1]
    return (r0, typ')

-- helper function for compileExpression:
-- evaluate two number expressions, convert one of them to real
-- on demand. return registers of results and their type.
unifyTypesInBinaryNumberExpr :: ASTExpression -> ASTExpression ->
    CodeGen (String, String, ASTTypeIdentifier)
unifyTypesInBinaryNumberExpr expr0 expr1 = do
    (r0, t0) <- compileExpression expr0
    (r1, t1) <- compileExpression expr1
    typ <- case needCastType t0 t1 of
        CastLeft  -> putOp "int_to_real" [r0, r0] >> return RealTypeIdentifier
        CastRight -> putOp "int_to_real" [r1, r1] >> return RealTypeIdentifier
        NoNeed    -> return t0
    return (r0, r1, typ)

data Cast = NoNeed | CastLeft | CastRight
needCastType :: ASTTypeIdentifier -> ASTTypeIdentifier -> Cast
needCastType IntegerTypeIdentifier IntegerTypeIdentifier = NoNeed
needCastType RealTypeIdentifier    RealTypeIdentifier    = NoNeed
needCastType BooleanTypeIdentifier BooleanTypeIdentifier = NoNeed
needCastType IntegerTypeIdentifier RealTypeIdentifier    = CastLeft
needCastType RealTypeIdentifier    IntegerTypeIdentifier = CastRight
needCastType RealTypeIdentifier    BooleanTypeIdentifier =
    error $ "expected real, found boolean"
needCastType BooleanTypeIdentifier typ =
    error $ "expected boolean, found " +++ typ
needCastType IntegerTypeIdentifier typ =
    error $ "expected integer, found " +++ typ
