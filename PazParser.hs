-- Module PazParser contains the parser of Paz language.
--
-- This file was provided by the course COMP90045, and
-- modified by team Placeholder in May 2018.
module PazParser where

import Text.Parsec (
    Parsec,
    SourcePos,
    choice,
    eof,
    optionMaybe,
    optional,
    parse,
    tokenPrim,
    try,
    (<|>),
    sepBy1,
    endBy1,
    lookAhead
    )
import Text.Parsec.Expr
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (
    void,
    liftM
    )
import Control.Applicative (many) -- get <|> from here too if needed
import PazLexer (
    ASTLexicalToken,
    LexicalToken(..),
    ASTCharacterString,
    ASTIdentifier,
    ASTUnsignedInteger,
    ASTUnsignedReal,
    trace
    )

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

type ASTProgram =
    (ASTIdentifier, ASTVariableDeclarationPart,
    ASTProcedureDeclarationPart, ASTCompoundStatement)
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

-- your code starts here

---------------------
-- statements section
---------------------

type ASTCompoundStatement = ASTStatementSequence
parseCompoundStatement :: Parser ASTCompoundStatement
parseCompoundStatement =
    trace
        "parseCompoundStatement"
        (
            do
                parseTokenBegin
                s <- parseStatementSequence
                parseTokenEnd
                return s
            )

type ASTStatementSequence = [ASTStatement]
parseStatementSequence :: Parser ASTStatementSequence
parseStatementSequence =
    trace
        "parseStatementSequence"
        sepBy1 parseStatement parseTokenSemicolon

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
    deriving Show
parseStatement :: Parser ASTStatement
parseStatement =
    trace
    "parseStatement"
    (
        try (liftM AssignmentStatement parseAssignmentStatement) <|>
        try (liftM ReadStatement parseReadStatement) <|>
        try (liftM WriteStatement parseWriteStatement) <|>
        try (liftM WriteStringStatement parseWriteStringStatement) <|>
        try (parseWritelnStatement >> return WritelnStatement ) <|>
        try (liftM ProcedureStatement parseProcedureStatement) <|>
        try (liftM CompoundStatement parseCompoundStatement) <|>
        try (liftM IfStatement parseIfStatement) <|>
        try (liftM WhileStatement parseWhileStatement) <|>
        try (liftM ForStatement parseForStatement) <|>
        (parseEmptyStatement >> return EmptyStatement)
        )

type ASTEmptyStatement = ()
parseEmptyStatement :: Parser ASTEmptyStatement
parseEmptyStatement =
    trace
        "parseEmptyStatement"
        lookAhead (parseTokenEnd <|> parseTokenDot <|> parseTokenSemicolon)

type ASTAssignmentStatement = (ASTVariableAccess, ASTExpression)
parseAssignmentStatement :: Parser ASTAssignmentStatement
parseAssignmentStatement =
    trace
        "parseExpression"
        (do
            x0 <- parseVariableAccess
            parseTokenAssign
            x1 <- parseExpression
            return (x0, x1)
        )

type ASTReadStatement = ASTVariableAccess
parseReadStatement :: Parser ASTReadStatement
parseReadStatement =
    trace
        "parseReadStatement"
        (do
            parseTokenRead
            parseTokenLeftParenthesis
            x0 <- parseVariableAccess
            parseTokenRightParenthesis
            return x0
        )

type ASTWriteStatement = ASTExpression
parseWriteStatement :: Parser ASTWriteStatement
parseWriteStatement =
    trace
        "parseWriteStatement"
        (do
            parseTokenWrite
            parseTokenLeftParenthesis
            x0 <- parseExpression
            parseTokenRightParenthesis
            return x0
        )

type ASTWriteStringStatement = ASTCharacterString
parseWriteStringStatement :: Parser ASTWriteStringStatement
parseWriteStringStatement =
    trace
        "parseWriteStringStatement"
        (do
            parseTokenWrite
            parseTokenLeftParenthesis
            x0 <- parseCharacterString
            parseTokenRightParenthesis
            return x0
        )

type ASTWritelnStatement = ()
parseWritelnStatement :: Parser ASTWritelnStatement
parseWritelnStatement =
    trace "parseWritelnStatement" parseTokenWriteln

type ASTIfStatement = (ASTExpression, ASTStatement, Maybe ASTStatement)
parseIfStatement :: Parser ASTIfStatement
parseIfStatement =
    trace
        "parseIfStatement"
        (do
            parseTokenIf
            x0 <- parseExpression
            parseTokenThen
            x1 <- parseStatement
            x2 <- optionMaybe (try (do
                parseTokenElse
                parseStatement
                ))
            return (x0, x1, x2)
            )

type ASTProcedureStatement = (ASTIdentifier, ASTActualParameterList)
parseProcedureStatement :: Parser ASTProcedureStatement
parseProcedureStatement = trace
    "parseProcedureStatement"
    (do
        x0 <- parseIdentifier
        x1 <- do
            try parseActualParameterList <|> return []
        return (x0, x1)
        )

type ASTActualParameterList = [ASTExpression]
parseActualParameterList :: Parser ASTActualParameterList
parseActualParameterList = trace
    "parseActualParameterList"
    (do
        parseTokenLeftParenthesis
        x <- sepBy1 parseExpression parseTokenComma
        parseTokenRightParenthesis
        return x
        )


type ASTForStatement =
    (ASTIdentifier, ASTExpression, ForDirection, ASTExpression, ASTStatement)
data ForDirection =
    ForTo | ForDownTo
    deriving Show
parseForStatement :: Parser ASTForStatement
parseForStatement = trace
    "parseForStatement"
    (do
        parseTokenFor
        x0 <- parseIdentifier
        parseTokenAssign
        x1 <- parseExpression
        x2 <-
            (parseTokenTo >> return ForTo) <|>
            (parseTokenDownTo >> return ForDownTo)
        x3 <- parseExpression
        parseTokenDo
        x4 <- parseStatement
        return (x0, x1, x2, x3, x4)
        )

type ASTWhileStatement = (ASTExpression, ASTStatement)
parseWhileStatement :: Parser ASTWhileStatement
parseWhileStatement = trace
    "parseWhileStatement"
    (do
        parseTokenWhile
        x0 <- parseExpression
        parseTokenDo
        x1 <- parseStatement
        return (x0, x1)
        )

---------------------
-- expression section
---------------------

data ASTRelationalOperator =
    Equal | NotEqual | LessThan | GreaterThan | LessEqual | GreaterEqual
    deriving Show

parseRelationalOperator :: Parser ASTRelationalOperator
parseRelationalOperator = trace
    "parseRelationalOperator"
    choice [
        parseTokenEqual              >> return Equal,
        parseTokenNotEqual           >> return NotEqual,
        parseTokenLessThan           >> return LessThan,
        parseTokenGreaterThan        >> return GreaterThan,
        parseTokenLessThanOrEqual    >> return LessEqual,
        parseTokenGreaterThanOrEqual >> return GreaterEqual
    ]

data ASTAddingOperator =
    Plus | Minus | Or
    deriving Show

data ASTMutiplayingOperator =
    Times | DivideBy | Div | And
    deriving Show

data ASTExpression =
    RelOp  ASTRelationalOperator ASTExpression ASTExpression |
    SignOp ASTSign ASTExpression |
    AddOp  ASTAddingOperator ASTExpression ASTExpression |
    MulOp  ASTMutiplayingOperator ASTExpression ASTExpression |
    NotOp  ASTExpression |
    Const  ASTUnsignedConstant |
    Var    ASTVariableAccess
    deriving Show

type UnaryExprParser = Parser (ASTExpression -> ASTExpression)
type BinaryExprParser =
    Parser (ASTExpression -> ASTExpression -> ASTExpression)

-- parse simple expr with one optional relational op
parseExpression :: Parser ASTExpression
parseExpression = trace
    "parseExpression"
    (
        (try (do
            e0 <- parseSimpleExpression
            op <- parseRelationalOperator
            e1 <- parseSimpleExpression
            return $ RelOp op e0 e1
            ))
        <|> parseSimpleExpression
    )

-- parse expr without relational ops (simple expr)
-- use Text.Parsec.Expr module to build that parser
-- ref. https://wiki.haskell.org/Parsing_a_simple_imperative_language
parseSimpleExpression :: Parser ASTExpression
parseSimpleExpression = trace
    "parseSimpleExpression"
    buildExpressionParser operatorTable parseTerm

operatorTable = [
        [ Prefix parseNotExpression                   ],
        [ Infix  parseMutiplayingExpression AssocLeft ],
        [ Prefix parseSignExpression,
          Infix  parseAddingExpression      AssocLeft ]
    ]

parseTerm :: Parser ASTExpression
parseTerm = trace
    "parseTerm"
    try (do
        parseTokenLeftParenthesis
        -- allow relational ops only here
        -- (directly within parenthesis)
        x <- parseExpression
        parseTokenRightParenthesis
        return x
        ) <|>
    try (liftM Const parseUnsignedConstant) <|>
    try (liftM Var parseVariableAccess)

parseNotExpression :: UnaryExprParser
parseNotExpression = trace
    "parseNotExpression"
    parseTokenNot >> return NotOp

parseMutiplayingExpression :: BinaryExprParser
parseMutiplayingExpression = trace
    "parseMutiplayingExpression"
    choice [
        parseTokenTimes    >> return (MulOp Times),
        parseTokenDivideBy >> return (MulOp DivideBy),
        parseTokenDiv      >> return (MulOp Div),
        parseTokenAnd      >> return (MulOp And)
        ]

parseSignExpression :: UnaryExprParser
parseSignExpression = trace
    "parseSignExpression"
    choice [
        parseTokenPlus  >> return (SignOp SignPlus),
        parseTokenMinus >> return (SignOp SignMinus)
        ]

parseAddingExpression :: BinaryExprParser
parseAddingExpression = trace
    "parseAddingExpression"
    choice [
        parseTokenPlus   >> return (AddOp Plus),
        parseTokenMinus  >> return (AddOp Minus),
        parseTokenOr     >> return (AddOp Or)
        ]

type ASTVariableAccess = VariableAccess
data VariableAccess =
    IndexedVariable ASTIndexedVariable |
    Identifier ASTIdentifier
    deriving Show

parseVariableAccess :: Parser ASTVariableAccess
parseVariableAccess = trace
    "parseVariableAccess"
    try (liftM IndexedVariable parseIndexedVariable)
    <|>  liftM Identifier parseIdentifier

type ASTIndexedVariable = (ASTIdentifier, ASTExpression)
parseIndexedVariable :: Parser ASTIndexedVariable
parseIndexedVariable = trace
    "parseIndexedVariable"
    (do
        x0 <- parseIdentifier
        parseTokenLeftBracket
        x1 <- parseExpression
        parseTokenRightBracket
        return (x0, x1)
        )

type ASTBooleanConstant = Bool
parseBooleanConstant :: Parser ASTBooleanConstant
parseBooleanConstant = trace
    "parseBooleanConstant"
    try (parseTokenFalse >> return False)
    <|> (parseTokenTrue >> return True)

type ASTUnsignedConstant = UnsignedConstant
data UnsignedConstant =
    Boolean ASTBooleanConstant |
    UnsignedInteger ASTUnsignedInteger |
    UnsignedReal ASTUnsignedReal
    deriving Show

parseUnsignedConstant :: Parser ASTUnsignedConstant
parseUnsignedConstant = trace
    "parseUnsignedConstant"
    try (liftM Boolean parseBooleanConstant)
    <|>  try (liftM UnsignedInteger parseUnsignedInteger)
    <|>  liftM UnsignedReal parseUnsignedReal

-- your code ends here

type ASTProcedureDeclaration =
    (ASTIdentifier, ASTFormalParameterList,
    ASTVariableDeclarationPart, ASTCompoundStatement)
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
                    try (parseFormalParameterList)
                    <|> (return [])
                parseTokenSemicolon
                x2 <-
                    parseVariableDeclarationPart
                x3 <-
                    parseCompoundStatement
                return (x0, x1, x2, x3)
            )

type ASTFormalParameterList = [ASTFormalParameterSection]
parseFormalParameterList :: Parser ASTFormalParameterList
parseFormalParameterList =
    trace
        "parseFormalParameterList"
        (
            do
                parseTokenLeftParenthesis
                x <- try (do
                    sepBy1 parseFormalParameterSection parseTokenSemicolon
                    )
                parseTokenRightParenthesis
                return x
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
                        Just _ -> (True, x1, x2)
                        _ -> (False, x1, x2)
                    )
            )

type ASTIdentifierList = [ASTIdentifier]
parseIdentifierList :: Parser ASTIdentifierList
parseIdentifierList =
    trace
        "parseIdentifierList"
        try (sepBy1 parseIdentifier parseTokenComma)

type ASTVariableDeclarationPart = [ASTVariableDeclaration]
parseVariableDeclarationPart :: Parser ASTVariableDeclarationPart
parseVariableDeclarationPart =
    trace
        "parseVariableDeclarationPart"
        (
            try (
                do
                    parseTokenVar
                    endBy1 parseVariableDeclaration parseTokenSemicolon
                ) <|>
                return []
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
    deriving(Show, Eq)
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
