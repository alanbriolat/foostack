> module FooStack.AsmParser where

> import Data.List (elemIndex)
> import Control.Applicative ((<$>))
> import Text.ParserCombinators.Parsec
> import qualified Text.ParserCombinators.Parsec.Token as Token
> import qualified Text.ParserCombinators.Parsec.Language as Language

> import FooStack.Asm

Let's use Parsec's useful language definition stuff!

> def = Language.emptyDef { Language.commentLine   = ";"
>                         , Language.identStart    = letter <|> char '_'
>                         , Language.identLetter   = alphaNum <|> char '_'
>                         , Language.caseSensitive = False
>                         }
> lexer = Token.makeTokenParser def

Some utility functions based on the language definition:

> integer    = Token.integer lexer
> identifier = Token.identifier lexer
> lexeme     = Token.lexeme lexer
> whiteSpace = Token.whiteSpace lexer

Let's work from the top down.  Firstly we need a single type to represent all
available code elements - there are more than just instructions!

> data Element = Op Op
>              | Label String
>              deriving (Show)

At the top level, we are parsing on a line-by-line basis:

> parseAsm :: Parser [Element]
> parseAsm = concat <$> parseLine `sepBy1` (many1 newline)

Each line consists of either a label, an operation, or both:

> parseLine :: Parser [Element]
> parseLine = concat <$> sequence [ try parseLabel <|> return []
>                                 , try parseInstruction <|> return []
>                                 ]

A label is an identifier string ending with a colon:

> parseLabel :: Parser [Element]
> parseLabel = do i <- identifier
>                 char ':'
>                 return [Label i]
>          <|> return []

> parseInstruction :: Parser [Element]
> parseInstruction = identifier >>= parseOp

> parseOp :: String -> Parser [Element]
> parseOp op = return []

Probably the most basic element: a parser for valid operands.

> parseOperand :: Parser Operand
> parseOperand = parseIdentifier <|> parseLiteral <?> "operand"
> parseIdentifier :: Parser Operand
> parseIdentifier = do modechar <- oneOf "#%@"
>                      regchar <- lexeme (oneOf registers)
>                      case regchar `elemIndex` registers of
>                           Just r -> case modechar of
>                                          '#' -> return (Reg r)
>                                          '%' -> return (Frame r)
>                                          '@' -> return (Addr r)
>                   <?> "identifier"
> -- TODO: range check
> parseLiteral :: Parser Operand
> parseLiteral = (Lit . fromIntegral) <$> integer

Parse an operation.  This returns a list of operations because some
instructions have a second argument which is a data word.

We're going to have more than just operations in an assembly source file, e.g.
labels.  We need a data type that covers everything we're going to get from
the parser.
