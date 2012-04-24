> module FooStack.AsmParser where

> import Data.List (elemIndex)
> import Data.Char (toUpper)
> import Control.Applicative ((<$>), (<*))
> import Text.ParserCombinators.Parsec hiding (label, space, spaces)

> import FooStack.Asm

Basic identifier parsing: alphanumeric plus underscore, but not starting with a
number.

> identifier :: Parser String
> identifier = do first <- (letter <|> char '_')
>                 rest <- (many $ alphaNum <|> char '_')
>                 return (first:rest)
>              <?> "identifier"

The identifier parser is sufficient for parsing instruction names; validity of
the instruction name can be sorted once we're sure we're trying to treat it as
an instruction.

> instruction :: Parser String
> instruction = identifier

The identifier parser is also sufficient for label names.  Labels are signified
by a separating colon before the code they refer to.

> label :: Parser String
> label = identifier <* (spaces >> char ':')

We're going to deal with whitespace a lot, and our definition varies a bit from
Parsec's.

> space :: Parser Char
> space = char ' ' <|> char '\t'
> spaces :: Parser [Char]
> spaces = many space
> spaces1 :: Parser [Char]
> spaces1 = many1 space

Let's work from the top down.  Firstly we need a single type to represent all
available code elements - there are more than just instructions!

> data Element = Op Op
>              | Label String
>              deriving (Show)

At the top level, we are parsing on a line-by-line basis.  Each line must
therefore have at least one newline character between it and the next line.

> parseAsm :: Parser [Element]
> parseAsm = concat <$> parseLine `sepBy` (many1 newline)

Each line consists of either a label, an operation, or both.  This gives us the
flexibility of multiple labels for the same location, e.g. a subroutine that
starts with a loop.

> parseLine :: Parser [Element]
> parseLine = concat <$> sequence [ spaces >> (try parseLabel <|> return [])
>                                 , spaces >> (try parseInstruction <|> return [])
>                                 ]

> parseLabel :: Parser [Element]
> parseLabel = (:[]) <$> Label <$> label

> parseInstruction :: Parser [Element]
> parseInstruction = instruction >> return [Op (SET (Lit 0) (Lit 0))]

 parseInstruction :: Parser [Element]
 parseInstruction = uppercase <$> identifier >>= parseOp
     where
     uppercase :: String -> String
     uppercase = map toUpper

 buildOp1 :: (Operand -> Op) -> Parser [Element]
 buildOp1 op = do a <- parseOperand
                  return [(Op (op a))]
 buildOp2 :: (Operand -> Operand -> Op) -> Parser [Element]
 buildOp2 op = do a <- parseOperand
                  b <- parseOperand
                  return [(Op (op a b))]
 parseOp :: String -> Parser [Element]
 parseOp "SET"  = buildOp2 SET
 parseOp "ADD"  = buildOp2 SET
 parseOp "SUB"  = buildOp2 SET
 parseOp "MUL"  = buildOp2 SET
 parseOp "SHL"  = buildOp2 SET
 parseOp "SHR"  = buildOp2 SET
 parseOp "AND"  = buildOp2 SET
 parseOp "BOR"  = buildOp2 SET
 parseOp "XOR"  = buildOp2 SET
 parseOp _      = fail "unknown instruction"

Probably the most basic element: a parser for valid operands.

 parseOperand :: Parser Operand
 parseOperand = parseIdentifier <|> parseLiteral <?> "operand"
 parseIdentifier :: Parser Operand
 parseIdentifier = do modechar <- oneOf "#%@"
                      regchar <- lexeme (oneOf registers)
                      case regchar `elemIndex` registers of
                           Just r -> case modechar of
                                          '#' -> return (Reg r)
                                          '%' -> return (Frame r)
                                          '@' -> return (Addr r)
                   <?> "identifier"
 -- TODO: range check
 parseLiteral :: Parser Operand
 parseLiteral = (Lit . fromIntegral) <$> integer

Parse an operation.  This returns a list of operations because some
instructions have a second argument which is a data word.

We're going to have more than just operations in an assembly source file, e.g.
labels.  We need a data type that covers everything we're going to get from
the parser.
