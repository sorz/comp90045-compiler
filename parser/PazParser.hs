module PazParser where

--import Debug.Trace (trace)
import Text.Parsec (
    Parsec,
    SourcePos,
    choice,
    eof,
    optionMaybe,
    optional,
    parse,
    tokenPrim,
    try
    )
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (void)
import Control.Applicative (many) -- get <|> from here too if needed
import PazLexer (
    ASTLexicalToken,
    LexicalToken(..),
    ASTCharacterString,
    ASTIdentifier,
    ASTUnsignedInteger,
    ASTUnsignedReal,
    printTokenLeftBrace,
    printTokenRightBrace,
    printTokenLeftParenthesis,
    printTokenRightParenthesis,
    printTokenTimes,
    printTokenPlus,
    printTokenComma,
    printTokenMinus,
    printTokenEllipsis,
    printTokenDot,
    printTokenDivideBy,
    printTokenAssign,
    printTokenColon,
    printTokenSemicolon,
    printTokenLessThanOrEqual,
    printTokenNotEqual,
    printTokenLessThan,
    printTokenEqual,
    printTokenGreaterThanOrEqual,
    printTokenGreaterThan,
    printTokenLeftBracket,
    printTokenRightBracket,
    printTokenAnd,
    printTokenArray,
    printTokenBegin,
    printTokenBoolean,
    printTokenDiv,
    printTokenDo,
    printTokenDownTo,
    printTokenElse,
    printTokenEnd,
    printTokenFalse,
    printTokenFor,
    printTokenFunction,
    printTokenIf,
    printTokenInteger,
    printTokenNot,
    printTokenOf,
    printTokenOr,
    printTokenProcedure,
    printTokenProgram,
    printTokenRead,
    printTokenReal,
    printTokenThen,
    printTokenTo,
    printTokenTrue,
    printTokenVar,
    printTokenWhile,
    printTokenWrite,
    printTokenWriteln,
    printCharacterString,
    printTokenSingleQuote,
    printStringElement,
    printIdentifier,
    printUnsignedInteger,
    printDigitSequence,
    printUnsignedReal
    )

-- turn off lexer tracing since stage 1 is now complete
trace :: x -> y -> y
trace _ y = y

-- define a parser which parses an incoming stream of ASTLexicalToken,
-- noting that ASTLexicalToken is a pair of SourcePos and LexicalToken
type Parser = Parsec [ASTLexicalToken] ()

-- since we are not processing characters we must define "satisfy" ourself,
-- tokenPrim takes a show function, an update function, and test function
satisfy ::
     -- the following type constraint is some kind of Haskell gobbledygook,
     -- to make it work we'd have to turn on the FlexibleContents feature:
     --(Stream s m ASTLexicalToken) =>
     (LexicalToken -> Bool) -> Parser LexicalToken
satisfy f =
     tokenPrim
         (\(_, x) -> show x)
         updatePosASTLexicalToken
         (\(_, x) -> if f x then Just x else Nothing)

-- updating the source position is simply extracing it from the coming
-- ASTLexicalToken in the input stream, if there is no next parseToken then
-- use current (i.e. last) parseToken, I am sure there would be a better way
updatePosASTLexicalToken ::
    SourcePos -> ASTLexicalToken -> [ASTLexicalToken] -> SourcePos
updatePosASTLexicalToken _ _ ((pos, _) : _) = pos
updatePosASTLexicalToken _ (pos, _) [] = pos

parseTokenEof :: Parser ()
parseTokenEof = eof

-- lexical tokens section
-- these are basically wrappers that check for each kind of lexical parseToken
-- already recognized by PazLexer.hs and then regurgitate it, those
-- starting with "parseToken" throw away the ASTLexicalToken, those starting
-- with "parse" unwrap the ASTLexicalToken and then the LexicalToken

parseTokenLeftParenthesis :: Parser ()
parseTokenLeftParenthesis =
    void (
        satisfy (
            \x ->
                case x of
                    LTLeftParenthesis -> True
                    otherwise -> False
            )
        )
    
parseTokenRightParenthesis :: Parser ()
parseTokenRightParenthesis =
    void (
        satisfy (
            \x ->
                case x of
                    LTRightParenthesis -> True
                    otherwise -> False
            )
        )
    
parseTokenTimes :: Parser ()
parseTokenTimes =
    void (
        satisfy (
            \x ->
                case x of
                    LTTimes -> True
                    otherwise -> False
            )
        )
    
parseTokenPlus :: Parser ()
parseTokenPlus =
    void (
        satisfy (
            \x ->
                case x of
                    LTPlus -> True
                    otherwise -> False
            )
        )
    
parseTokenComma :: Parser ()
parseTokenComma =
    void (
        satisfy (
            \x ->
                case x of
                    LTComma -> True
                    otherwise -> False
            )
        )
    
parseTokenMinus :: Parser ()
parseTokenMinus =
    void (
        satisfy (
            \x ->
                case x of
                    LTMinus -> True
                    otherwise -> False
            )
        )
    
parseTokenEllipsis :: Parser ()
parseTokenEllipsis =
    void (
        satisfy (
            \x ->
                case x of
                    LTEllipsis -> True
                    otherwise -> False
            )
        )
    
parseTokenDot :: Parser ()
parseTokenDot =
    void (
        satisfy (
            \x ->
                case x of
                    LTDot -> True
                    otherwise -> False
            )
        )
    
parseTokenDivideBy :: Parser ()
parseTokenDivideBy =
    void (
        satisfy (
            \x ->
                case x of
                    LTDivideBy -> True
                    otherwise -> False
            )
        )
    
parseTokenAssign :: Parser ()
parseTokenAssign =
    void (
        satisfy (
            \x ->
                case x of
                    LTAssign -> True
                    otherwise -> False
            )
        )
    
parseTokenColon :: Parser ()
parseTokenColon =
    void (
        satisfy (
            \x ->
                case x of
                    LTColon -> True
                    otherwise -> False
            )
        )
    
parseTokenSemicolon :: Parser ()
parseTokenSemicolon =
    void (
        satisfy (
            \x ->
                case x of
                    LTSemicolon -> True
                    otherwise -> False
            )
        )
    
parseTokenLessThanOrEqual :: Parser ()
parseTokenLessThanOrEqual =
    void (
        satisfy (
            \x ->
                case x of
                    LTLessThanOrEqual -> True
                    otherwise -> False
            )
        )
    
parseTokenNotEqual :: Parser ()
parseTokenNotEqual =
    void (
        satisfy (
            \x ->
                case x of
                    LTNotEqual -> True
                    otherwise -> False
            )
        )
    
parseTokenLessThan :: Parser ()
parseTokenLessThan =
    void (
        satisfy (
            \x ->
                case x of
                    LTLessThan -> True
                    otherwise -> False
            )
        )
    
parseTokenEqual :: Parser ()
parseTokenEqual =
    void (
        satisfy (
            \x ->
                case x of
                    LTEqual -> True
                    otherwise -> False
            )
        )
    
parseTokenGreaterThanOrEqual :: Parser ()
parseTokenGreaterThanOrEqual =
    void (
        satisfy (
            \x ->
                case x of
                    LTGreaterThanOrEqual -> True
                    otherwise -> False
            )
        )
    
parseTokenGreaterThan :: Parser ()
parseTokenGreaterThan =
    void (
        satisfy (
            \x ->
                case x of
                    LTGreaterThan -> True
                    otherwise -> False
            )
        )
    
parseTokenLeftBracket :: Parser ()
parseTokenLeftBracket =
    void (
        satisfy (
            \x ->
                case x of
                    LTLeftBracket -> True
                    otherwise -> False
            )
        )
    
parseTokenRightBracket :: Parser ()
parseTokenRightBracket =
    void (
        satisfy (
            \x ->
                case x of
                    LTRightBracket -> True
                    otherwise -> False
            )
        )
    
parseTokenAnd :: Parser ()
parseTokenAnd =
    void (
        satisfy (
            \x ->
                case x of
                    LTAnd -> True
                    otherwise -> False
            )
        )

parseTokenArray :: Parser ()
parseTokenArray =
    void (
        satisfy (
            \x ->
                case x of
                    LTArray -> True
                    otherwise -> False
            )
        )

parseTokenBegin :: Parser ()
parseTokenBegin =
    void (
        satisfy (
            \x ->
                case x of
                    LTBegin -> True
                    otherwise -> False
            )
        )

parseTokenBoolean :: Parser ()
parseTokenBoolean =
    void (
        satisfy (
            \x ->
                case x of
                    LTBoolean -> True
                    otherwise -> False
            )
        )

parseTokenDiv :: Parser ()
parseTokenDiv =
    void (
        satisfy (
            \x ->
                case x of
                    LTDiv -> True
                    otherwise -> False
            )
        )

parseTokenDo :: Parser ()
parseTokenDo =
    void (
        satisfy (
            \x ->
                case x of
                    LTDo -> True
                    otherwise -> False
            )
        )

parseTokenDownTo :: Parser ()
parseTokenDownTo =
    void (
        satisfy (
            \x ->
                case x of
                    LTDownTo -> True
                    otherwise -> False
            )
        )

parseTokenElse :: Parser ()
parseTokenElse =
    void (
        satisfy (
            \x ->
                case x of
                    LTElse -> True
                    otherwise -> False
            )
        )

parseTokenEnd :: Parser ()
parseTokenEnd =
    void (
        satisfy (
            \x ->
                case x of
                    LTEnd -> True
                    otherwise -> False
            )
        )

parseTokenFalse :: Parser ()
parseTokenFalse =
    void (
        satisfy (
            \x ->
                case x of
                    LTFalse -> True
                    otherwise -> False
            )
        )

parseTokenFor :: Parser ()
parseTokenFor =
    void (
        satisfy (
            \x ->
                case x of
                    LTFor -> True
                    otherwise -> False
            )
        )

parseTokenFunction :: Parser ()
parseTokenFunction =
    void (
        satisfy (
            \x ->
                case x of
                    LTFunction -> True
                    otherwise -> False
            )
        )

parseTokenIf :: Parser ()
parseTokenIf =
    void (
        satisfy (
            \x ->
                case x of
                    LTIf -> True
                    otherwise -> False
            )
        )

parseTokenInteger :: Parser ()
parseTokenInteger =
    void (
        satisfy (
            \x ->
                case x of
                    LTInteger -> True
                    otherwise -> False
            )
        )

parseTokenNot :: Parser ()
parseTokenNot =
    void (
        satisfy (
            \x ->
                case x of
                    LTNot -> True
                    otherwise -> False
            )
        )

parseTokenOf :: Parser ()
parseTokenOf =
    void (
        satisfy (
            \x ->
                case x of
                    LTOf -> True
                    otherwise -> False
            )
        )

parseTokenOr :: Parser ()
parseTokenOr =
    void (
        satisfy (
            \x ->
                case x of
                    LTOr -> True
                    otherwise -> False
            )
        )

parseTokenProcedure :: Parser ()
parseTokenProcedure =
    void (
        satisfy (
            \x ->
                case x of
                    LTProcedure -> True
                    otherwise -> False
            )
        )

parseTokenProgram :: Parser ()
parseTokenProgram =
    void (
        satisfy (
            \x ->
                case x of
                    LTProgram -> True
                    otherwise -> False
            )
        )

parseTokenRead :: Parser ()
parseTokenRead =
    void (
        satisfy (
            \x ->
                case x of
                    LTRead -> True
                    otherwise -> False
            )
        )

parseTokenReal :: Parser ()
parseTokenReal =
    void (
        satisfy (
            \x ->
                case x of
                    LTReal -> True
                    otherwise -> False
            )
        )

parseTokenThen :: Parser ()
parseTokenThen =
    void (
        satisfy (
            \x ->
                case x of
                    LTThen -> True
                    otherwise -> False
            )
        )

parseTokenTo :: Parser ()
parseTokenTo =
    void (
        satisfy (
            \x ->
                case x of
                    LTTo -> True
                    otherwise -> False
            )
        )

parseTokenTrue :: Parser ()
parseTokenTrue =
    void (
        satisfy (
            \x ->
                case x of
                    LTTrue -> True
                    otherwise -> False
            )
        )

parseTokenVar :: Parser ()
parseTokenVar =
    void (
        satisfy (
            \x ->
                case x of
                    LTVar -> True
                    otherwise -> False
            )
        )

parseTokenWhile :: Parser ()
parseTokenWhile =
    void (
        satisfy (
            \x ->
                case x of
                    LTWhile -> True
                    otherwise -> False
            )
        )

parseTokenWrite :: Parser ()
parseTokenWrite =
    void (
        satisfy (
            \x ->
                case x of
                    LTWrite -> True
                    otherwise -> False
            )
        )

parseTokenWriteln :: Parser ()
parseTokenWriteln =
    void (
        satisfy (
            \x ->
                case x of
                    LTWriteln -> True
                    otherwise -> False
            )
        )

parseCharacterString :: Parser ASTCharacterString
parseCharacterString =
    do
        (LTCharacterString x) <-
            satisfy (
                \x ->
                    case x of
                        LTCharacterString _ -> True
                        otherwise -> False
                )
        return x

parseIdentifier :: Parser ASTIdentifier
parseIdentifier =
    do
        (LTIdentifier x) <-
            satisfy (
                \x ->
                    case x of
                        LTIdentifier _ -> True
                        otherwise -> False
                )
        return x

parseUnsignedInteger :: Parser ASTUnsignedInteger
parseUnsignedInteger =
    do
        (LTUnsignedInteger x) <-
            satisfy (
                \x ->
                    case x of
                        LTUnsignedInteger _ -> True
                        otherwise -> False
                )
        return x

parseUnsignedReal :: Parser ASTUnsignedReal
parseUnsignedReal =
    do
        (LTUnsignedReal x) <-
            satisfy (
                \x ->
                    case x of
                        LTUnsignedReal _ -> True
                        otherwise -> False
                )
        return x

-- end of lexical tokens section
 
type ASTStartSymbol = ASTProgram
parseStartSymbol :: Parser ASTStartSymbol
parseStartSymbol =
    trace
        "parseStartSymbol"
        (
            do
                x0 <-
                    parseProgram
                parseTokenEof
                return x0
            )

type ASTProgram = (ASTIdentifier, ASTVariableDeclarationPart, ASTProcedureDeclarationPart, ASTCompoundStatement)
parseProgram :: Parser ASTProgram
parseProgram =
    trace
        "parseProgram"
        (
            do
                parseTokenProgram
                x0 <-
                    parseIdentifier
                parseTokenSemicolon
                x1 <-
                    parseVariableDeclarationPart
                x2 <-
                    parseProcedureDeclarationPart
                x3 <-
                    parseCompoundStatement
                parseTokenDot
                return (x0, x1, x2, x3)
            )

type ASTProcedureDeclarationPart = [ASTProcedureDeclaration]
parseProcedureDeclarationPart :: Parser ASTProcedureDeclarationPart
parseProcedureDeclarationPart =
    trace
        "parseProcedureDeclarationPart"
        (
            many (
                try (
                    do
                        x0 <-
                            parseProcedureDeclaration
                        parseTokenSemicolon
                        return x0
                    )
                )
            )

type ASTStatement = Statement
data Statement =
    AssignmentStatement ASTAssignmentStatement |
    ReadStatement ASTReadStatement |
    WriteStatement ASTWriteStatement |
    WriteStringStatement ASTWriteStringStatement |
    WritelnStatement |
    ProcedureStatement ASTProcedureStatement |
    CompoundStatement ASTCompoundStatement |
    IfStatement ASTIfStatement |
    WhileStatement ASTWhileStatement |
    ForStatement ASTForStatement |
    EmptyStatement
    deriving(Show)
parseStatement :: Parser ASTStatement
parseStatement =
    trace
        "parseStatement"
        (
            choice
                [
                    try (
                        do
                            x <-
                                parseAssignmentStatement
                            return (AssignmentStatement x)
                        ),
                    try (
                        do
                            x <-
                                parseReadStatement
                            return (ReadStatement x)
                        ),
                    try (
                        do
                            x <-
                                parseWriteStatement
                            return (WriteStatement x)
                        ),
                    try (
                        do
                            x <-
                                parseWriteStringStatement
                            return (WriteStringStatement x)
                        ),
                    try (
                        do
                            x <-
                                parseWritelnStatement
                            return WritelnStatement
                        ),
                    try (
                        do
                            x <-
                                parseProcedureStatement
                            return (ProcedureStatement x)
                        ),
                    try (
                        do
                            x <-
                                parseCompoundStatement
                            return (CompoundStatement x)
                        ),
                    try (
                        do
                            x <-
                                parseIfStatement
                            return (IfStatement x)
                        ),
                    try (
                        do
                            x <-
                                parseWhileStatement
                            return (WhileStatement x)
                        ),
                    try (
                        do
                            x <-
                                parseForStatement
                            return (ForStatement x)
                        ),
                    do
                        x <-
                            parseEmptyStatement
                        return EmptyStatement
                    ]
            )

type ASTEmptyStatement = ()
parseEmptyStatement :: Parser ASTEmptyStatement
parseEmptyStatement =
    trace
        "parseEmptyStatement"
        (
            return ()
            )

type ASTAssignmentStatement = (ASTVariableAccess, ASTExpression)
parseAssignmentStatement :: Parser ASTAssignmentStatement
parseAssignmentStatement =
    trace
        "parseAssignmentStatement"
        (
            do
                x0 <-
                    parseVariableAccess
                parseTokenAssign
                x1 <-
                    parseExpression
                return (x0, x1)
            )

type ASTReadStatement = ASTVariableAccess
parseReadStatement :: Parser ASTReadStatement
parseReadStatement =
    trace
        "parseReadStatement"
        (
            do
                parseTokenRead
                parseTokenLeftParenthesis
                x0 <-
                    parseVariableAccess
                parseTokenRightParenthesis
                return x0
            )

type ASTWriteStatement = ASTExpression
parseWriteStatement :: Parser ASTWriteStatement
parseWriteStatement =
    trace
        "parseWriteStatement"
        (
            do
                parseTokenWrite
                parseTokenLeftParenthesis
                x0 <-
                    parseExpression
                parseTokenRightParenthesis
                return x0
            )

type ASTWriteStringStatement = ASTCharacterString
parseWriteStringStatement :: Parser ASTWriteStringStatement
parseWriteStringStatement =
    trace
        "parseWriteStringStatement"
        (
            do
                parseTokenWrite
                parseTokenLeftParenthesis
                x0 <-
                    parseCharacterString
                parseTokenRightParenthesis
                return x0
            )

type ASTWritelnStatement = ()
parseWritelnStatement :: Parser ASTWritelnStatement
parseWritelnStatement =
    trace
        "parseWritelnStatement"
        (
            parseTokenWriteln
            )

type ASTProcedureStatement = (ASTIdentifier, ASTActualParameterList)
parseProcedureStatement :: Parser ASTProcedureStatement
parseProcedureStatement =
    trace
        "parseProcedureStatement"
        (
            do
                x0 <-
                    parseIdentifier
                x1 <-
                    optionMaybe (
                        try (
                            parseActualParameterList
                            )
                        )
                return (
                    x0,
                    case x1 of
                        Just x -> x
                        Nothing -> []
                    )
            )

type ASTActualParameterList = [ASTExpression]
parseActualParameterList :: Parser ASTActualParameterList
parseActualParameterList =
    trace
        "parseActualParameterList"
        (
            do
                parseTokenLeftParenthesis
                x0 <-
                    parseExpression
                x1 <-
                    many (
                        try (
                            do
                                parseTokenComma
                                x0 <-
                                    parseExpression
                                return x0
                            )
                        )
                parseTokenRightParenthesis
                return (x0 : x1)
            )

type ASTCompoundStatement = ASTStatementSequence
parseCompoundStatement :: Parser ASTCompoundStatement
parseCompoundStatement =
    trace
        "parseCompoundStatement"
        (
            do
                parseTokenBegin
                x0 <-
                    parseStatementSequence
                parseTokenEnd
                return x0
            )

type ASTStatementSequence = [ASTStatement]
parseStatementSequence :: Parser ASTStatementSequence
parseStatementSequence =
    trace
        "parseStatementSequence"
        (
            do
                x0 <-
                    parseStatement
                x1 <-
                    many (
                        try (
                            do
                                parseTokenSemicolon
                                x0 <-
                                    parseStatement
                                return x0
                            )
                        )
                return (x0 : x1)
            )

type ASTIfStatement = (ASTExpression, ASTStatement, (Maybe ASTStatement))
parseIfStatement :: Parser ASTIfStatement
parseIfStatement =
    trace
        "parseIfStatement"
        (
            do
                parseTokenIf
                x0 <-
                    parseExpression
                parseTokenThen
                x1 <-
                    parseStatement
                x2 <-
                    optionMaybe (
                        try (
                            do
                                parseTokenElse
                                x0 <-
                                    parseStatement
                                return x0
                            )
                        )
                return (x0, x1, x2)
            )

type ASTWhileStatement = (ASTExpression, ASTStatement)
parseWhileStatement :: Parser ASTWhileStatement
parseWhileStatement =
    trace
        "parseWhileStatement"
        (
            do
                parseTokenWhile
                x0 <-
                    parseExpression
                parseTokenDo
                x1 <-
                    parseStatement
                return (x0, x1)
            )

type ASTForStatement = (ASTIdentifier, ASTExpression, ForDirection, ASTExpression, ASTStatement)
data ForDirection =
    ForDirectionUp |
    ForDirectionDown
    deriving(Show)
parseForStatement :: Parser ASTForStatement
parseForStatement =
    trace
        "parseForStatement"
        (
            do
                parseTokenFor
                x0 <-
                    parseIdentifier
                parseTokenAssign
                x1 <-
                    parseExpression
                x2 <-
                    choice
                        [
                            try (
                                do
                                    parseTokenTo
                                    return ForDirectionUp
                                ),
                            do
                                parseTokenDownTo
                                return ForDirectionDown
                            ]
                x3 <-
                    parseExpression
                parseTokenDo
                x4 <-
                    parseStatement
                return (x0, x1, x2, x3, x4)
            )

-- helper for expressions
data BinaryOperator =
    -- ASTRelationalOperator
    EqualOperator |
    NotEqualOperator |
    LessThanOperator |
    GreaterThanOperator |
    LessThanOrEqualOperator |
    GreaterThanOrEqualOperator |
    -- ASTAddingOperator
    PlusOperator |
    MinusOperator |
    OrOperator |
    -- ASTMultiplyingOperator
    TimesOperator |
    DivideByOperator |
    DivOperator |
    AndOperator
    deriving(Show)
-- note that in the below, ASTExpression vs. ASTSimpleExpression etc,
-- makes no difference since all are type aliases for Expression, but
-- we distinguish them according to the (unparenthesized) structure
data Expression =
    -- ASTExpression
    EqualExpression ASTSimpleExpression ASTSimpleExpression |
    NotEqualExpression ASTSimpleExpression ASTSimpleExpression |
    LessThanExpression ASTSimpleExpression ASTSimpleExpression |
    GreaterThanExpression ASTSimpleExpression ASTSimpleExpression |
    LessThanOrEqualExpression ASTSimpleExpression ASTSimpleExpression |
    GreaterThanOrEqualExpression ASTSimpleExpression ASTSimpleExpression |
    -- ASTSimpleExpression
    PlusExpression ASTSimpleExpression ASTTerm |
    MinusExpression ASTSimpleExpression ASTTerm |
    OrExpression ASTSimpleExpression ASTTerm |
    NegExpression ASTTerm |
    -- ASTTerm
    TimesExpression ASTTerm ASTFactor |
    DivideByExpression ASTTerm ASTFactor |
    DivExpression ASTTerm ASTFactor |
    AndExpression ASTTerm ASTFactor |
    -- ASTFactor
    UnsignedConstantExpression ASTUnsignedConstant |
    VariableAccessExpression ASTVariableAccess |
    NotExpression ASTFactor
    deriving(Show)
binaryExpression ::
    Expression -> BinaryOperator -> Expression -> Expression
binaryExpression lhs EqualOperator rhs =
    EqualExpression lhs rhs
binaryExpression lhs NotEqualOperator rhs =
    NotEqualExpression lhs rhs
binaryExpression lhs LessThanOperator rhs =
    LessThanExpression lhs rhs
binaryExpression lhs GreaterThanOperator rhs =
    GreaterThanExpression lhs rhs
binaryExpression lhs LessThanOrEqualOperator rhs =
    LessThanOrEqualExpression lhs rhs
binaryExpression lhs GreaterThanOrEqualOperator rhs =
    GreaterThanOrEqualExpression lhs rhs
binaryExpression lhs PlusOperator rhs =
    PlusExpression lhs rhs
binaryExpression lhs MinusOperator rhs =
    MinusExpression lhs rhs
binaryExpression lhs OrOperator rhs =
    OrExpression lhs rhs
binaryExpression lhs TimesOperator rhs =
    TimesExpression lhs rhs
binaryExpression lhs DivideByOperator rhs =
    DivideByExpression lhs rhs
binaryExpression lhs DivOperator rhs =
    DivExpression lhs rhs
binaryExpression lhs AndOperator rhs =
    AndExpression lhs rhs
binaryExpressionList ::
    Expression -> [(BinaryOperator, Expression)] -> Expression
binaryExpressionList result [] = result
binaryExpressionList result ((oper, expr) : exprs) =
    binaryExpressionList (binaryExpression result oper expr) exprs

type ASTExpression = Expression
parseExpression :: Parser ASTExpression
parseExpression =
    trace
        "parseExpression"
        (
            do
                x0 <-
                    parseSimpleExpression
                x1 <-
                    optionMaybe (
                        try (
                            do
                                x0 <-
                                    parseRelationalOperator
                                x1 <-
                                    parseSimpleExpression
                                return (x0, x1)
                            )
                        )
                return (
                    case x1 of
                        Nothing -> x0
                        Just (oper, expr) -> binaryExpression x0 oper expr
                    )
            )

type ASTRelationalOperator = BinaryOperator
parseRelationalOperator :: Parser ASTRelationalOperator
parseRelationalOperator =
    trace
        "parseRelationalOperator"
        (
            choice
                [
                    try (
                        do
                            parseTokenEqual
                            return EqualOperator
                        ),
                    try (
                        do
                            parseTokenNotEqual
                            return NotEqualOperator
                        ),
                    try (
                        do
                            parseTokenLessThan
                            return LessThanOperator
                        ),
                    try (
                        do
                            parseTokenGreaterThan
                            return GreaterThanOperator
                        ),
                    try (
                        do
                            parseTokenLessThanOrEqual
                            return LessThanOrEqualOperator
                        ),
                    do
                        parseTokenGreaterThanOrEqual
                        return GreaterThanOrEqualOperator
                    ]
            )

type ASTSimpleExpression = Expression
parseSimpleExpression :: Parser ASTSimpleExpression
parseSimpleExpression =
    trace
        "parseSimpleExpression"
        (
            do
                x0 <-
                    optionMaybe (
                        try (
                            parseSign
                            )
                        )
                x1 <-
                    parseTerm
                x2 <-
                    many (
                        try (
                            do
                                x0 <-
                                    parseAddingOperator
                                x1 <-
                                    parseTerm
                                return (x0, x1)
                            )
                        )
                return (
                    binaryExpressionList
                    (
                        case x0 of
                            Just SignMinus -> NegExpression x1
                            Nothing -> x1
                        )
                    x2
                    )
            )

type ASTAddingOperator = BinaryOperator
parseAddingOperator :: Parser ASTAddingOperator
parseAddingOperator =
    trace
        "parseAddingOperator"
        (
            choice
                [
                    try (
                        do
                            parseTokenPlus
                            return PlusOperator
                        ),
                    try (
                        do
                            parseTokenMinus
                            return MinusOperator
                        ),
                    do
                        parseTokenOr
                        return OrOperator
                    ]
            )

type ASTTerm = Expression
parseTerm :: Parser ASTTerm
parseTerm =
    trace
        "parseTerm"
        (
            do
                x0 <-
                    parseFactor
                x1 <-
                    many (
                        try (
                            do
                                x0 <-
                                    parseMultiplyingOperator
                                x1 <-
                                    parseFactor
                                return (x0, x1)
                            )
                        )
                return (binaryExpressionList x0 x1)
            )

type ASTMultiplyingOperator = BinaryOperator
parseMultiplyingOperator :: Parser ASTMultiplyingOperator
parseMultiplyingOperator =
    trace
        "parseMultiplyingOperator"
        (
            choice
                [
                    try (
                        do
                            parseTokenTimes
                            return TimesOperator
                        ),
                    try (
                        do
                            parseTokenDivideBy
                            return DivideByOperator
                        ),
                    try (
                        do
                            parseTokenDiv
                            return DivOperator
                        ),
                    do
                        parseTokenAnd
                        return AndOperator
                    ]
            )

type ASTFactor = Expression
parseFactor :: Parser ASTFactor
parseFactor =
    trace
        "parseFactor"
        (
            choice
                [
                    try (
                        do
                            x <-
                                parseUnsignedConstant
                            return (UnsignedConstantExpression x)
                        ),
                    try (
                        do
                            x <-
                                parseVariableAccess
                            return (VariableAccessExpression x)
                        ),
                    try (
                        do
                            parseTokenLeftParenthesis
                            x0 <-
                                parseExpression
                            parseTokenRightParenthesis
                            return x0
                        ),
                    do
                        x <-
                            do
                                parseTokenNot
                                x0 <-
                                    parseFactor
                                return x0
                        return (NotExpression x)
                    ]
            )

type ASTUnsignedConstant = UnsignedConstant
data UnsignedConstant =
    BooleanConstant ASTBooleanConstant |
    UnsignedInteger ASTUnsignedInteger |
    UnsignedReal ASTUnsignedReal
    deriving(Show)
parseUnsignedConstant :: Parser ASTUnsignedConstant
parseUnsignedConstant =
    trace
        "parseUnsignedConstant"
        (
            choice
                [
                    try (
                        do
                            x <-
                                parseBooleanConstant
                            return (BooleanConstant x)
                        ),
                    try (
                        do
                            x <-
                                parseUnsignedInteger
                            return (UnsignedInteger x)
                        ),
                    do
                        x <-
                            parseUnsignedReal
                        return (UnsignedReal x)
                    ]
            )

type ASTBooleanConstant = Bool
parseBooleanConstant :: Parser ASTBooleanConstant
parseBooleanConstant =
    trace
        "parseBooleanConstant"
        (
            choice
                [
                    try (
                        do
                            parseTokenFalse
                            return False
                        ),
                    do
                        parseTokenTrue
                        return True
                    ]
            )

type ASTVariableAccess = VariableAccess
data VariableAccess =
    IndexedVariableAccess ASTIndexedVariable |
    OrdinaryVariableAccess ASTIdentifier
    deriving(Show)
parseVariableAccess :: Parser ASTVariableAccess
parseVariableAccess =
    trace
        "parseVariableAccess"
        (
            choice
                [
                    try (
                        do
                            x <-
                                parseIndexedVariable
                            return (IndexedVariableAccess x)
                        ),
                    do
                        x <-
                            parseIdentifier
                        return (OrdinaryVariableAccess x)
                    ]
            )

type ASTIndexedVariable = (ASTIdentifier, ASTExpression)
parseIndexedVariable :: Parser ASTIndexedVariable
parseIndexedVariable =
    trace
        "parseIndexedVariable"
        (
            do
                x0 <-
                    parseIdentifier
                parseTokenLeftBracket
                x1 <-
                    parseExpression
                parseTokenRightBracket
                return (x0, x1)
            )

type ASTProcedureDeclaration = (ASTIdentifier, ASTFormalParameterList, ASTVariableDeclarationPart, ASTCompoundStatement)
parseProcedureDeclaration :: Parser ASTProcedureDeclaration
parseProcedureDeclaration =
    trace
        "parseProcedureDeclaration"
        (
            do
                parseTokenProcedure
                x0 <-
                    parseIdentifier
                x1 <-
                    optionMaybe (
                        try (
                            parseFormalParameterList
                            )
                        )
                parseTokenSemicolon
                x2 <-
                    parseVariableDeclarationPart
                x3 <-
                    parseCompoundStatement
                return (
                    x0,
                    (
                        case x1 of
                            Just x -> x
                            Nothing -> []
                        ),
                    x2,
                    x3
                    )
            )

type ASTFormalParameterList = [ASTFormalParameterSection]
parseFormalParameterList :: Parser ASTFormalParameterList
parseFormalParameterList =
    trace
        "parseFormalParameterList"
        (
            do
                parseTokenLeftParenthesis
                x0 <-
                    parseFormalParameterSection
                x1 <-
                    many (
                        try (
                            do
                                parseTokenSemicolon
                                x0 <-
                                    parseFormalParameterSection
                                return x0
                            )
                        )
                parseTokenRightParenthesis
                return (x0 : x1)
            )

-- presence of "var" keyword should have type "Maybe ()" according to the
-- usual system, but the generator cannot handle this because it is stupid,
-- so we manually put in a type of "Bool" which is "True" if "var" present
type ASTFormalParameterSection = (Bool, ASTIdentifierList, ASTTypeDenoter)
parseFormalParameterSection :: Parser ASTFormalParameterSection
parseFormalParameterSection =
    trace
        "parseFormalParameterSection"
        (
            do
                x0 <-
                    optionMaybe (
                        try (
                            parseTokenVar
                            )
                        )
                x1 <-
                    parseIdentifierList
                parseTokenColon
                x2 <-
                    parseTypeDenoter
                return (
                    case x0 of
                        Just () -> (True, x1, x2)
                        Nothing -> (False, x1, x2)
                    )
            )

type ASTIdentifierList = [ASTIdentifier]
parseIdentifierList :: Parser ASTIdentifierList
parseIdentifierList =
    trace
        "parseIdentifierList"
        (
            do
                x0 <-
                    parseIdentifier
                x1 <-
                    many (
                        try (
                            do
                                parseTokenComma
                                x0 <-
                                    parseIdentifier
                                return x0
                            )
                        )
                return (x0 : x1)
            )

type ASTVariableDeclarationPart = [ASTVariableDeclaration]
parseVariableDeclarationPart :: Parser ASTVariableDeclarationPart
parseVariableDeclarationPart =
    trace
        "parseVariableDeclarationPart"
        (
            do
                x <- optionMaybe (
                    try (
                        do
                            parseTokenVar
                            x0 <-
                                parseVariableDeclaration
                            parseTokenSemicolon
                            x1 <-
                                many (
                                    try (
                                        do
                                            x0 <-
                                                parseVariableDeclaration
                                            parseTokenSemicolon
                                            return x0
                                        )
                                    )
                            return (x0 : x1)
                        )
                    )
                return (
                    case x of
                        Just y -> y
                        Nothing -> []
                    )
            )

type ASTVariableDeclaration = (ASTIdentifierList, ASTTypeDenoter)
parseVariableDeclaration :: Parser ASTVariableDeclaration
parseVariableDeclaration =
    trace
        "parseVariableDeclaration"
        (
            do
                x0 <-
                    parseIdentifierList
                parseTokenColon
                x1 <-
                    parseTypeDenoter
                return (x0, x1)
            )

type ASTTypeDenoter = TypeDenoter
data TypeDenoter =
    OrdinaryTypeDenoter ASTTypeIdentifier |
    ArrayTypeDenoter ASTArrayType
    deriving(Show)
parseTypeDenoter :: Parser ASTTypeDenoter
parseTypeDenoter =
    trace
        "parseTypeDenoter"
        (
            choice
                [
                    try (
                        do
                            x <-
                                parseTypeIdentifier
                            return (OrdinaryTypeDenoter x)
                        ),
                    do
                        x <-
                            parseArrayType
                        return (ArrayTypeDenoter x)
                    ]
            )

type ASTTypeIdentifier = TypeIdentifier
data TypeIdentifier =
    IntegerTypeIdentifier |
    RealTypeIdentifier |
    BooleanTypeIdentifier
    deriving(Eq, Show)
parseTypeIdentifier :: Parser ASTTypeIdentifier
parseTypeIdentifier =
    trace
        "parseTypeIdentifier"
        (
            choice
                [
                    try (
                        do
                            parseTokenInteger
                            return IntegerTypeIdentifier
                        ),
                    try (
                        do
                            parseTokenReal
                            return RealTypeIdentifier
                        ),
                    do
                        parseTokenBoolean
                        return BooleanTypeIdentifier
                    ]
            )

type ASTArrayType = (ASTSubrangeType, ASTTypeIdentifier)
parseArrayType :: Parser ASTArrayType
parseArrayType =
    trace
        "parseArrayType"
        (
            do
                parseTokenArray
                parseTokenLeftBracket
                x0 <-
                    parseSubrangeType
                parseTokenRightBracket
                parseTokenOf
                x1 <-
                    parseTypeIdentifier
                return (x0, x1)
            )

type ASTSubrangeType = (ASTConstant, ASTConstant)
parseSubrangeType :: Parser ASTSubrangeType
parseSubrangeType =
    trace
        "parseSubrangeType"
        (
            do
                x0 <-
                    parseConstant
                parseTokenEllipsis
                x1 <-
                    parseConstant
                return (x0, x1)
            )

type ASTConstant = Int
parseConstant :: Parser ASTConstant
parseConstant =
    trace
        "parseConstant"
        (
            do
                x0 <-
                    optionMaybe (
                        try (
                            parseSign
                            )
                        )
                x1 <-
                    parseUnsignedInteger
                return (
                    case x0 of
                        Just SignMinus -> -x1
                        otherwise -> x1
                    )
            )

type ASTSign = Sign
data Sign =
    SignPlus |
    SignMinus
    deriving(Show)
parseSign :: Parser ASTSign
parseSign =
    trace
        "parseSign"
        (
            choice
                [
                    try (
                        do
                            parseTokenPlus
                            return SignPlus
                        ),
                    do
                        parseTokenMinus
                        return SignMinus
                    ]
            )

generateSpaces :: Int -> String
generateSpaces 0 = ""
generateSpaces n = ' ' : generateSpaces (n - 1)

printStartSymbol :: ASTStartSymbol -> String
printStartSymbol =
    printProgram

printProgram :: ASTProgram -> String
printProgram =
    \(x0, x1, x2, x3) ->
        printTokenProgram ++
            " " ++
            printIdentifier x0 ++
            printTokenSemicolon ++
            "\n\n" ++
            printVariableDeclarationPart x1 ++
            (
                case x1 of
                    (_ : _) ->
                        "\n"
                    [] ->
                        ""
                ) ++
            printProcedureDeclarationPart x2 ++
            printCompoundStatement 0 x3 ++
            printTokenDot ++
            "\n"

printProcedureDeclarationPart :: ASTProcedureDeclarationPart -> String
printProcedureDeclarationPart =
    concatMap (
        \x0 ->
            printProcedureDeclaration x0 ++
                printTokenSemicolon ++
                "\n\n"
        )

printStatement :: Int -> ASTStatement -> String
printStatement indent =
    \x -> (
        case x of
            AssignmentStatement x -> (
                printAssignmentStatement indent
                ) x
            ReadStatement x -> (
                printReadStatement indent
                ) x
            WriteStatement x -> (
                printWriteStatement indent
                ) x
            WriteStringStatement x -> (
                printWriteStringStatement indent
                ) x
            WritelnStatement -> (
                printWritelnStatement indent
                )
            ProcedureStatement x -> (
                printProcedureStatement indent
                ) x
            CompoundStatement x -> (
                printCompoundStatement indent
                ) x
            IfStatement x -> (
                printIfStatement indent
                ) x
            WhileStatement x -> (
                printWhileStatement indent
                ) x
            ForStatement x -> (
                printForStatement indent
                ) x
            EmptyStatement -> (
                printEmptyStatement indent
                )
        )

printEmptyStatement :: Int -> String
printEmptyStatement _ =
    ""

printAssignmentStatement :: Int -> ASTAssignmentStatement -> String
printAssignmentStatement indent =
    \(x0, x1) ->
        generateSpaces indent ++
            printVariableAccess x0 ++
            " " ++
            printTokenAssign ++
            " " ++
            printExpression x1

printReadStatement :: Int -> ASTReadStatement -> String
printReadStatement indent =
    \x0 ->
        generateSpaces indent ++
            printTokenRead ++
            printTokenLeftParenthesis ++
            printVariableAccess x0 ++
            printTokenRightParenthesis

printWriteStatement :: Int -> ASTWriteStatement -> String
printWriteStatement indent =
    \x0 ->
        generateSpaces indent ++
            printTokenWrite ++
            printTokenLeftParenthesis ++
            printExpression x0 ++
            printTokenRightParenthesis

printWriteStringStatement :: Int -> ASTWriteStringStatement -> String
printWriteStringStatement indent =
    \x0 ->
        generateSpaces indent ++
            printTokenWrite ++
            printTokenLeftParenthesis ++
            printCharacterString x0 ++
            printTokenRightParenthesis

printWritelnStatement :: Int -> String
printWritelnStatement indent =
    generateSpaces indent ++
        printTokenWriteln

printProcedureStatement :: Int -> ASTProcedureStatement -> String
printProcedureStatement indent =
    \(x0, x1) ->
        generateSpaces indent ++
            printIdentifier x0 ++
            (
                case x1 of
                    (_ : _) ->
                        printActualParameterList x1
                    [] ->
                        ""
                )

printActualParameterList :: ASTActualParameterList -> String
printActualParameterList =
    \(x0 : x1) ->
        printTokenLeftParenthesis ++
            printExpression x0 ++
            (
                concatMap (
                    \x0 ->
                        printTokenComma ++
                            " " ++
                            printExpression x0
                    )
                ) x1 ++
            printTokenRightParenthesis

printCompoundStatement :: Int -> ASTCompoundStatement -> String
printCompoundStatement indent =
    \x0 ->
        generateSpaces indent ++
            printTokenBegin ++
            "\n" ++
            printStatementSequence (indent + 4) x0 ++
            "\n" ++
            generateSpaces indent ++
            printTokenEnd

printStatementSequence :: Int -> ASTStatementSequence -> String
printStatementSequence indent =
    \(x0 : x1) ->
        printStatement indent x0 ++
            concatMap (
                \x0 ->
                    printTokenSemicolon ++
                        "\n" ++
                        printStatement indent x0
                ) x1

printIfStatement :: Int -> ASTIfStatement -> String
printIfStatement indent =
    \(x0, x1, x2) ->
        generateSpaces indent ++
            printTokenIf ++
            " " ++
            printExpression x0 ++
            " " ++
            printTokenThen ++
            "\n" ++
            printStatement
                (
                    case x1 of
                        CompoundStatement _ ->
                            indent
                        otherwise ->
                            indent + 4
                    )
                x1 ++
            (
                case x2 of
                    Just x ->
                        "\n" ++
                            generateSpaces indent ++
                            printTokenElse ++
                            "\n" ++
                            printStatement
                                (
                                    case x of
                                        CompoundStatement _ ->
                                            indent
                                        otherwise ->
                                            indent + 4
                                    )
                                x
                    Nothing ->
                        ""
                )

printWhileStatement :: Int -> ASTWhileStatement -> String
printWhileStatement indent =
    \(x0, x1) ->
        generateSpaces indent ++
            printTokenWhile ++
            " " ++
            printExpression x0 ++
            " " ++
            printTokenDo ++
            "\n" ++
            printStatement
                (
                    case x1 of
                        CompoundStatement _ ->
                            indent
                        otherwise ->
                            indent + 4
                    )
                x1

printForStatement :: Int -> ASTForStatement -> String
printForStatement indent =
    \(x0, x1, x2, x3, x4) ->
        generateSpaces indent ++
            printTokenFor ++
            " " ++
            printIdentifier x0 ++
            " " ++
            printTokenAssign ++
            " " ++
            printExpression x1 ++
            " " ++
            (
                case x2 of
                    ForDirectionUp -> (
                        printTokenTo
                        )
                    ForDirectionDown -> (
                        printTokenDownTo
                        )
                ) ++
            " " ++
            printExpression x3 ++
            " " ++
            printTokenDo ++
            "\n" ++
            printStatement
                (
                    case x4 of
                        CompoundStatement _ ->
                            indent
                        otherwise ->
                            indent + 4
                    )
                x4

-- helper for expressions
printBinaryOperator :: BinaryOperator -> String
printBinaryOperator =
    \x -> (
        case x of
            EqualOperator -> (
                printTokenEqual
                )
            NotEqualOperator -> (
                printTokenNotEqual
                )
            LessThanOperator -> (
                printTokenLessThan
                )
            GreaterThanOperator -> (
                printTokenGreaterThan
                )
            LessThanOrEqualOperator -> (
                printTokenLessThanOrEqual
                )
            GreaterThanOrEqualOperator -> (
                printTokenGreaterThanOrEqual
                )
            PlusOperator -> (
                printTokenPlus
                )
            MinusOperator -> (
                printTokenMinus
                )
            OrOperator -> (
                printTokenOr
                )
            TimesOperator -> (
                printTokenTimes
                )
            DivideByOperator -> (
                printTokenDivideBy
                )
            DivOperator -> (
                printTokenDiv
                )
            AndOperator -> (
                printTokenAnd
                )
        )

printExpression :: ASTExpression -> String
printExpression =
    \x -> (
        case x of
            EqualExpression lhs rhs ->
                printSimpleExpression lhs ++
                    " " ++
                    printBinaryOperator EqualOperator ++
                    " " ++
                    printSimpleExpression rhs
            NotEqualExpression lhs rhs ->
                printSimpleExpression lhs ++
                    " " ++
                    printBinaryOperator NotEqualOperator ++
                    " " ++
                    printSimpleExpression rhs
            LessThanExpression lhs rhs ->
                printSimpleExpression lhs ++
                    " " ++
                    printBinaryOperator LessThanOperator ++
                    " " ++
                    printSimpleExpression rhs
            GreaterThanExpression lhs rhs ->
                printSimpleExpression lhs ++
                    " " ++
                    printBinaryOperator GreaterThanOperator ++
                    " " ++
                    printSimpleExpression rhs
            LessThanOrEqualExpression lhs rhs ->
                printSimpleExpression lhs ++
                    " " ++
                    printBinaryOperator LessThanOrEqualOperator ++
                    " " ++
                    printSimpleExpression rhs
            GreaterThanOrEqualExpression lhs rhs ->
                printSimpleExpression lhs ++
                    " " ++
                    printBinaryOperator GreaterThanOrEqualOperator ++
                    " " ++
                    printSimpleExpression rhs
            otherwise ->
                printSimpleExpression x
        )

printSimpleExpression :: ASTSimpleExpression -> String
printSimpleExpression =
    \x -> (
        case x of
            PlusExpression lhs rhs ->
                printSimpleExpression lhs ++
                    " " ++
                    printBinaryOperator PlusOperator ++
                    " " ++
                    printTerm rhs
            MinusExpression lhs rhs ->
                printSimpleExpression lhs ++
                    " " ++
                    printBinaryOperator MinusOperator ++
                    " " ++
                    printTerm rhs
            OrExpression lhs rhs ->
                printSimpleExpression lhs ++
                    " " ++
                    printBinaryOperator OrOperator ++
                    " " ++
                    printTerm rhs
            NegExpression expr ->
                printSign SignMinus ++
                    printTerm expr
            otherwise ->
                printTerm x
        )

printTerm :: ASTTerm -> String
printTerm =
    \x -> (
        case x of 
            TimesExpression lhs rhs ->
                printTerm lhs ++
                    " " ++
                    printBinaryOperator TimesOperator ++
                    " " ++
                    printFactor rhs
            DivideByExpression lhs rhs ->
                printTerm lhs ++
                    " " ++
                    printBinaryOperator DivideByOperator ++
                    " " ++
                    printFactor rhs
            DivExpression lhs rhs ->
                printTerm lhs ++
                    " " ++
                    printBinaryOperator DivOperator ++
                    " " ++
                    printFactor rhs
            AndExpression lhs rhs ->
                printTerm lhs ++
                    " " ++
                    printBinaryOperator AndOperator ++
                    " " ++
                    printFactor rhs
            otherwise ->
                printFactor x
        )

printFactor :: ASTFactor -> String
printFactor =
    \x -> (
        case x of
            UnsignedConstantExpression x ->
                printUnsignedConstant x
            VariableAccessExpression x ->
                printVariableAccess x
            NotExpression x ->
                printTokenNot ++
                    " " ++
                    printFactor x
            otherwise ->
                printTokenLeftParenthesis ++
                    printExpression x ++
                    printTokenRightParenthesis
        )

printUnsignedConstant :: ASTUnsignedConstant -> String
printUnsignedConstant =
    \x -> (
        case x of
            BooleanConstant x -> (
                printBooleanConstant
                ) x
            UnsignedInteger x -> (
                printUnsignedInteger
                ) x
            UnsignedReal x -> (
                printUnsignedReal
                ) x
        )

printBooleanConstant :: ASTBooleanConstant -> String
printBooleanConstant =
    \x -> (
        case x of
            False -> (
                printTokenFalse
                )
            True -> (
                printTokenTrue
                )
        )

printVariableAccess :: ASTVariableAccess -> String
printVariableAccess =
    \x -> (
        case x of
            IndexedVariableAccess x -> (
                printIndexedVariable
                ) x
            OrdinaryVariableAccess x -> (
                printIdentifier
                ) x
        )

printIndexedVariable :: ASTIndexedVariable -> String
printIndexedVariable =
    \(x0, x1) ->
        (
            printIdentifier
            ) x0 ++
        (
            printTokenLeftBracket
            ) ++
        (
            printExpression
            ) x1 ++
        (
            printTokenRightBracket
            )

printProcedureDeclaration :: ASTProcedureDeclaration -> String
printProcedureDeclaration =
    \(x0, x1, x2, x3) ->
        printTokenProcedure ++
            " " ++
            printIdentifier x0 ++
            (
                case x1 of
                    (_ : _) ->
                        printFormalParameterList x1
                    [] ->
                        ""
                ) ++
            printTokenSemicolon ++
            "\n" ++
            printVariableDeclarationPart x2 ++
            printCompoundStatement 0 x3

printFormalParameterList :: ASTFormalParameterList -> String
printFormalParameterList =
    \(x0 : x1) ->
        printTokenLeftParenthesis ++
            printFormalParameterSection x0 ++
            (
                concatMap (
                    \x0 ->
                        printTokenSemicolon ++
                            " " ++
                            printFormalParameterSection x0
                    )
                ) x1 ++
            printTokenRightParenthesis

printFormalParameterSection :: ASTFormalParameterSection -> String
printFormalParameterSection =
    \(x0, x1, x2) ->
        (
            if x0
                then
                    printTokenVar ++
                        " "
                else
                    ""
            ) ++
            printIdentifierList x1 ++
            printTokenColon ++
            " " ++
            printTypeDenoter x2

printIdentifierList :: ASTIdentifierList -> String
printIdentifierList =
    \(x0 : x1) ->
        printIdentifier x0 ++
            concatMap (
                \x0 ->
                    printTokenComma ++
                        " " ++
                        printIdentifier x0
                ) x1

printVariableDeclarationPart :: ASTVariableDeclarationPart -> String
printVariableDeclarationPart =
    \x -> (
        case x of
            (_ : _) ->
                printTokenVar ++
                    "\n" ++
                    (
                        concatMap (
                            \x0 ->
                                printVariableDeclaration x0 ++
                                    printTokenSemicolon ++
                                    "\n"
                            ) x
                        )
            [] ->
                ""
        )

printVariableDeclaration :: ASTVariableDeclaration -> String
printVariableDeclaration =
    \(x0, x1) ->
        "    " ++
            printIdentifierList x0 ++
            printTokenColon ++
            " " ++
            printTypeDenoter x1

printTypeDenoter :: ASTTypeDenoter -> String
printTypeDenoter =
    \x -> (
        case x of
            OrdinaryTypeDenoter x -> (
                printTypeIdentifier
                ) x
            ArrayTypeDenoter x -> (
                printArrayType
                ) x
        )

printTypeIdentifier :: ASTTypeIdentifier -> String
printTypeIdentifier =
    \x -> (
        case x of
            IntegerTypeIdentifier -> (
                printTokenInteger
                )
            RealTypeIdentifier -> (
                printTokenReal
                )
            BooleanTypeIdentifier -> (
                printTokenBoolean
                )
        )

printArrayType :: ASTArrayType -> String
printArrayType =
    \(x0, x1) ->
        printTokenArray
            ++
            printTokenLeftBracket ++
            printSubrangeType x0 ++
            printTokenRightBracket ++
            " " ++
            printTokenOf ++
            " " ++
            printTypeIdentifier x1

printSubrangeType :: ASTSubrangeType -> String
printSubrangeType =
    \(x0, x1) ->
        (
            printConstant
            ) x0 ++
        (
            printTokenEllipsis
            ) ++
        (
            printConstant
            ) x1

printConstant :: ASTConstant -> String
printConstant =
    show

printSign :: ASTSign -> String
printSign =
    \x -> (
        case x of
            SignPlus -> (
                printTokenPlus
                )
            SignMinus -> (
                printTokenMinus
                )
        )
