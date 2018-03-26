module PazLexer where

import Debug.Trace (trace)
import Text.Parsec (
    SourcePos,
    anyChar,
    char,
    choice,
    eof,
    getPosition,
    noneOf,
    oneOf,
    optionMaybe,
    optional,
    satisfy,
    string,
    try
    )
import Text.Parsec.String (Parser)
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (void)
import Control.Applicative (many) -- get <|> from here too if needed

parseTokenEof :: Parser ()
parseTokenEof = eof

parseTokenWhitespaceCharacter :: Parser ()
parseTokenWhitespaceCharacter = void (oneOf "\t\n\v\f\r ")

parseTokenCommentCharacter :: Parser ()
parseTokenCommentCharacter = void (noneOf "}")

type ASTStringCharacter = Char
parseStringCharacter :: Parser ASTStringCharacter
parseStringCharacter = noneOf "'\n"

type ASTDigit = Char
parseDigit :: Parser ASTDigit
parseDigit = satisfy (\x -> isDigit x)

type ASTLetter = Char
parseLetter :: Parser ASTLetter
parseLetter = do
    x <- satisfy (\x -> isLetter x)
    return (toLower x)

type ASTStartSymbol = [ASTLexicalToken]
parseStartSymbol :: Parser ASTStartSymbol
parseStartSymbol =
    trace
        "parseStartSymbol"
        (
            do
                parseTokenWhitespace
                x0 <-
                    many (
                        try (
                            do
                                x0 <-
                                    parseLexicalToken
                                parseTokenWhitespace
                                return x0
                            )
                        )
                parseTokenEof
                return x0
            )

parseTokenWhitespace :: Parser ()
parseTokenWhitespace =
    trace
        "parseTokenWhitespace"
        (
            void (
                many (
                    choice
                        [
                            try (
                                parseTokenWhitespaceCharacter
                                ),
                            parseTokenComment
                            ]
                    )
                )
            )

parseTokenComment :: Parser ()
parseTokenComment =
    trace
        "parseTokenComment"
        (
            do
                parseTokenLeftBrace
                void (
                    many (
                        parseTokenCommentCharacter
                        )
                    )
                parseTokenRightBrace
            )

parseTokenLeftBrace :: Parser ()
parseTokenLeftBrace =
    trace
        "parseTokenLeftBrace"
        (
            void (string "{")
            )

parseTokenRightBrace :: Parser ()
parseTokenRightBrace =
    trace
        "parseTokenRightBrace"
        (
            void (string "}")
            )

type ASTLexicalToken = (SourcePos, LexicalToken)
data LexicalToken =
    LTLeftParenthesis |
    LTRightParenthesis |
    LTTimes |
    LTPlus |
    LTComma |
    LTMinus |
    LTEllipsis |
    LTDot |
    LTDivideBy |
    LTAssign |
    LTColon |
    LTSemicolon |
    LTLessThanOrEqual |
    LTNotEqual |
    LTLessThan |
    LTEqual |
    LTGreaterThanOrEqual |
    LTGreaterThan |
    LTLeftBracket |
    LTRightBracket |
    LTAnd |
    LTArray |
    LTBegin |
    LTBoolean |
    LTDiv |
    LTDo |
    LTDownTo |
    LTElse |
    LTEnd |
    LTFor |
    LTFunction |
    LTIf |
    LTInteger |
    LTNot |
    LTOf |
    LTOr |
    LTProcedure |
    LTProgram |
    LTReal |
    LTThen |
    LTTo |
    LTVar |
    LTWhile |
    LTCharacterString ASTCharacterString |
    LTIdentifier ASTIdentifier |
    LTUnsignedReal ASTUnsignedReal |
    LTUnsignedInteger ASTUnsignedInteger
    deriving(Show)
keywordToLexicalToken :: Map String LexicalToken
keywordToLexicalToken =
    Map.fromList
        [
            ("and", LTAnd),
            ("array", LTArray),
            ("begin", LTBegin),
            ("boolean", LTBoolean),
            ("div", LTDiv),
            ("do", LTDo),
            ("downto", LTDownTo),
            ("else", LTElse),
            ("end", LTEnd),
            ("for", LTFor),
            ("function", LTFunction),
            ("if", LTIf),
            ("integer", LTInteger),
            ("not", LTNot),
            ("of", LTOf),
            ("or", LTOr),
            ("procedure", LTProcedure),
            ("program", LTProgram),
            ("real", LTReal),
            ("then", LTThen),
            ("to", LTTo),
            ("var", LTVar),
            ("while", LTWhile)
        ]
parseLexicalToken :: Parser ASTLexicalToken
parseLexicalToken =
    do
        pos <- getPosition
        x <- lexicalToken
        return (
            pos,
            case x of
                LTIdentifier y ->
                    case Map.lookup y keywordToLexicalToken of
                        Just z -> z
                        Nothing -> x
                _ -> x
            )
lexicalToken :: Parser LexicalToken
lexicalToken =
    trace
        "parseLexicalToken"
        (
            choice
                [
                    try (
                        do
                            parseTokenLeftParenthesis
                            return LTLeftParenthesis
                        ),
                    try (
                        do
                            parseTokenRightParenthesis
                            return LTRightParenthesis
                        ),
                    try (
                        do
                            parseTokenTimes
                            return LTTimes
                        ),
                    try (
                        do
                            parseTokenPlus
                            return LTPlus
                        ),
                    try (
                        do
                            parseTokenComma
                            return LTComma
                        ),
                    try (
                        do
                            parseTokenMinus
                            return LTMinus
                        ),
                    try (
                        do
                            parseTokenEllipsis
                            return LTEllipsis
                        ),
                    try (
                        do
                            parseTokenDot
                            return LTDot
                        ),
                    try (
                        do
                            parseTokenDivideBy
                            return LTDivideBy
                        ),
                    try (
                        do
                            parseTokenAssign
                            return LTAssign
                        ),
                    try (
                        do
                            parseTokenColon
                            return LTColon
                        ),
                    try (
                        do
                            parseTokenSemicolon
                            return LTSemicolon
                        ),
                    try (
                        do
                            parseTokenLessThanOrEqual
                            return LTLessThanOrEqual
                        ),
                    try (
                        do
                            parseTokenNotEqual
                            return LTNotEqual
                        ),
                    try (
                        do
                            parseTokenLessThan
                            return LTLessThan
                        ),
                    try (
                        do
                            parseTokenEqual
                            return LTEqual
                        ),
                    try (
                        do
                            parseTokenGreaterThanOrEqual
                            return LTGreaterThanOrEqual
                        ),
                    try (
                        do
                            parseTokenGreaterThan
                            return LTGreaterThan
                        ),
                    try (
                        do
                            parseTokenLeftBracket
                            return LTLeftBracket
                        ),
                    try (
                        do
                            parseTokenRightBracket
                            return LTRightBracket
                        ),
                    try (
                        do
                            x <-
                                parseCharacterString
                            return (LTCharacterString x)
                        ),
                    try (
                        do
                            x <-
                                parseIdentifier
                            return (LTIdentifier x)
                        ),
                    try (
                        do
                            x <-
                                parseUnsignedReal
                            return (LTUnsignedReal x)
                        ),
                    do
                        x <-
                            parseUnsignedInteger
                        return (LTUnsignedInteger x)
                    ]
            )

parseTokenLeftParenthesis :: Parser ()
parseTokenLeftParenthesis =
    trace
        "parseTokenLeftParenthesis"
        (
            void (string "(")
            )

parseTokenRightParenthesis :: Parser ()
parseTokenRightParenthesis =
    trace
        "parseTokenRightParenthesis"
        (
            void (string ")")
            )

parseTokenTimes :: Parser ()
parseTokenTimes =
    trace
        "parseTokenTimes"
        (
            void (string "*")
            )

parseTokenPlus :: Parser ()
parseTokenPlus =
    trace
        "parseTokenPlus"
        (
            void (string "+")
            )

parseTokenComma :: Parser ()
parseTokenComma =
    trace
        "parseTokenComma"
        (
            void (string ",")
            )

parseTokenMinus :: Parser ()
parseTokenMinus =
    trace
        "parseTokenMinus"
        (
            void (string "-")
            )

parseTokenEllipsis :: Parser ()
parseTokenEllipsis =
    trace
        "parseTokenEllipsis"
        (
            void (string "..")
            )

parseTokenDot :: Parser ()
parseTokenDot =
    trace
        "parseTokenDot"
        (
            void (string ".")
            )

parseTokenDivideBy :: Parser ()
parseTokenDivideBy =
    trace
        "parseTokenDivideBy"
        (
            void (string "/")
            )

parseTokenAssign :: Parser ()
parseTokenAssign =
    trace
        "parseTokenAssign"
        (
            void (string ":=")
            )

parseTokenColon :: Parser ()
parseTokenColon =
    trace
        "parseTokenColon"
        (
            void (string ":")
            )

parseTokenSemicolon :: Parser ()
parseTokenSemicolon =
    trace
        "parseTokenSemicolon"
        (
            void (string ";")
            )

parseTokenLessThanOrEqual :: Parser ()
parseTokenLessThanOrEqual =
    trace
        "parseTokenLessThanOrEqual"
        (
            void (string "<=")
            )

parseTokenNotEqual :: Parser ()
parseTokenNotEqual =
    trace
        "parseTokenNotEqual"
        (
            void (string "<>")
            )

parseTokenLessThan :: Parser ()
parseTokenLessThan =
    trace
        "parseTokenLessThan"
        (
            void (string "<")
            )

parseTokenEqual :: Parser ()
parseTokenEqual =
    trace
        "parseTokenEqual"
        (
            void (string "=")
            )

parseTokenGreaterThanOrEqual :: Parser ()
parseTokenGreaterThanOrEqual =
    trace
        "parseTokenGreaterThanOrEqual"
        (
            void (string ">=")
            )

parseTokenGreaterThan :: Parser ()
parseTokenGreaterThan =
    trace
        "parseTokenGreaterThan"
        (
            void (string ">")
            )

parseTokenLeftBracket :: Parser ()
parseTokenLeftBracket =
    trace
        "parseTokenLeftBracket"
        (
            void (string "[")
            )

parseTokenRightBracket :: Parser ()
parseTokenRightBracket =
    trace
        "parseTokenRightBracket"
        (
            void (string "]")
            )

type ASTCharacterString = [ASTStringElement]
parseCharacterString :: Parser ASTCharacterString
parseCharacterString =
    trace
        "parseCharacterString"
        (
            do
                parseTokenSingleQuote
                x0 <-
                    many (
                        try (
                            parseStringElement
                            )
                        )
                parseTokenSingleQuote
                return x0
            )

parseTokenSingleQuote :: Parser ()
parseTokenSingleQuote =
    trace
        "parseTokenSingleQuote"
        (
            void (string "'")
            )

type ASTStringElement = Char
parseStringElement :: Parser ASTStringElement
parseStringElement =
    trace
        "parseStringElement"
        (
            choice
                [
                    try (
                        do
                            x <-
                                string "''"
                            return '\''
                        ),
                    parseStringCharacter
                    ]
            )

type ASTIdentifier = String
parseIdentifier :: Parser ASTIdentifier
parseIdentifier =
    trace
        "parseIdentifier"
        (
            do
                x0 <-
                    parseLetter
                x1 <-
                    many (
                        try (
                            choice
                                [
                                    try (
                                        parseLetter
                                        ),
                                    parseDigit
                                    ]
                            )
                        )
                return (x0 : x1)
            )

type ASTUnsignedInteger = ASTDigitSequence
parseUnsignedInteger :: Parser ASTUnsignedInteger
parseUnsignedInteger =
    trace
        "parseUnsignedInteger"
        (
            parseDigitSequence
            )

type ASTDigitSequence = [ASTDigit]
parseDigitSequence :: Parser ASTDigitSequence
parseDigitSequence =
    trace
        "parseDigitSequence"
        (
            do
                x0 <-
                    parseDigit
                x1 <-
                    many (
                        try (
                            parseDigit
                            )
                        )
                return (x0 : x1)
            )

type ASTUnsignedReal = (ASTDigitSequence, (Maybe ASTDigitSequence), (Maybe ASTScaleFactor))
parseUnsignedReal :: Parser ASTUnsignedReal
parseUnsignedReal =
    trace
        "parseUnsignedReal"
        (
            choice
                [
                    try (
                        do
                            x0 <-
                                parseDigitSequence
                            parseTokenDot
                            x1 <-
                                parseDigitSequence
                            x2 <-
                                optionMaybe (
                                    try (
                                        do
                                            parseTokenE
                                            x0 <-
                                                parseScaleFactor
                                            return x0
                                        )
                                    )
                            return (x0, Just x1, x2)
                        ),
                    do
                        x0 <-
                            parseDigitSequence
                        parseTokenE
                        x1 <-
                            parseScaleFactor
                        return (x0, Nothing, Just x1)
                    ]
            )

parseTokenE :: Parser ()
parseTokenE =
    trace
        "parseTokenE"
        (
            void (string "e")
            )

type ASTScaleFactor = ((Maybe ASTSign), ASTDigitSequence)
parseScaleFactor :: Parser ASTScaleFactor
parseScaleFactor =
    trace
        "parseScaleFactor"
        (
            do
                x0 <-
                    optionMaybe (
                        try (
                            parseSign
                            )
                        )
                x1 <-
                    parseDigitSequence
                return (x0, x1)
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
