> module FooStack.AsmParser where

> import Data.List (elemIndex)
> import Data.Char (toUpper, toLower)
> import Control.Applicative ((<$>), (<*))
> import Text.ParserCombinators.Parsec hiding (label, space, spaces)
> import qualified Text.Parsec.Token as Token
> import qualified Text.Parsec.Language as Language

> import FooStack.Asm

We need a couple of useful things from Parsec's generic token parser.

> emptyLexer = Token.makeTokenParser Language.emptyDef
> decimal = Token.decimal emptyLexer
> hexadecimal = Token.hexadecimal emptyLexer


We're going to deal with whitespace a lot, and our definition varies a bit from
Parsec's.  Newlines are used to separate statements, so our definition of
valid whitespace inside a statement is "spaces and/or tabs".  Parsec's
definitions of these tokens have been hidden.

> space :: Parser Char
> space = char ' ' <|> char '\t' <?> "space"
> spaces :: Parser ()
> spaces = skipMany space
> spaces1 :: Parser ()
> spaces1 = skipMany1 space

We're also going to be handling things case-insensitively a lot.

> uppercase :: String -> String
> uppercase = map toUpper

Annoyingly, Parsec's "integer" parser is a "lexeme" and will gobble up our
newlines.  This means we basically have to reconstruct it ourselves.  Most of
what is here is inspired by the implementation of Parsec's "integer".

> integer :: Parser Integer
> integer =   do f <- sign
>                n <- number
>                return (f n)
>         <?> "integer"
>     where
>     sign =   (char '-' >> return negate)
>          <|> (char '+' >> return id)
>          <|> return id
>     number =   (char '0' >> hexadecimal)
>            <|> decimal

Basic identifier parsing: alphanumeric plus underscore, but not starting with a
number.  Identifiers must be at least 2 characters long, to make them
completely unambiguous compared to register references.

> identifier :: Parser String
> identifier =   do first <- (letter <|> char '_')
>                   rest <- (many1 $ alphaNum <|> char '_')
>                   return (first:rest)
>            <?> "identifier"

The identifier parser is sufficient for parsing instruction names; validity of
the instruction name can be sorted once we're sure we're trying to treat it as
an instruction.

> instruction :: Parser String
> instruction = identifier <?> "instruction"

The identifier parser is also sufficient for label names.  Labels are signified
by a separating colon before the code they refer to.

> label :: Parser String
> label = identifier <* (spaces >> char ':' >> spaces) <?> "label"

We can also re-used the identifier parser for assembler directives.  Like with
instructions, whether or not it is valid can be handled by the directive
parser.

> directive :: Parser String
> directive = char '.' >> identifier <?> "directive"

Registers must be a valid register letter.  They are stored in the assembly AST
as a lowercase character, but usage in the source should be case-insensitive.

> register :: Parser Register
> register = toLower <$> oneOf (registers ++ uppercase registers)

Comments are line comments, and start with a semicolon.  They can contain
anything, and end at the newline.

> linecomment :: Parser String
> linecomment = char ';' >> spaces >> manyTill anyChar newline <?> "comment"

Statements are lines, and are ended with a newline character.  Since line
comments always run to the end of the line, we can generalise line endings to
include an optional comment, which will be consumed and ignored.  Any leading
whitespace on the next line will also be consumed.

> lineending :: Parser ()
> lineending = spaces >> ((linecomment >> return ()) <|> (newline >> return ())) >> spaces

Let's work from the top down.  Firstly we need a single type to represent all
available code elements - there are more than just instructions!

> data Element = Op Op
>              | Label String
>              | LabelRef String
>              | UnknownDirective String
>              deriving (Show)

At the top level, we are parsing on a line-by-line basis.  Each line must
therefore have at least one newline character between it and the next line.
Using "spaces" consumes end-of-line whitespace.

> parseAsm :: Parser [Element]
> parseAsm = concat <$> parseLine `sepEndBy` lineending

Each line consists of either a lone assembler directive, or a label,
instruction, or both.  This gives us the flexibility of multiple labels for the
same location, e.g. a subroutine that starts with a loop.  It also allows
special things like ".align 8".

> parseLine :: Parser [Element]
> parseLine =   parseDirective
>           <|> concat <$> sequence [ option [] (try parseLabel)
>                                   , option [] (try parseInstruction)
>                                   ]

Here's a really basic directive element parser.  It should be fleshed out to
handle useful directives, such as ".align".

> parseDirective :: Parser [Element]
> parseDirective = (:[]) <$> UnknownDirective <$> directive

A label is just an identifier that ends with a colon.

> parseLabel :: Parser [Element]
> parseLabel = (:[]) <$> Label <$> label

An instruction is an identifier followed by some instruction-specific handling.
We make the instruction uppercase so we can treat it case-insensitively.

> parseInstruction :: Parser [Element]
> parseInstruction = uppercase <$> instruction >>= parseOp

Each valid instruction can be parsed differently, but generally speaking they
will make use of more generic operand parsing.

> parseOp :: String -> Parser [Element]
> parseOp "SET"  = parseOperands2 SET
> parseOp "ADD"  = parseOperands2 ADD
> parseOp "SUB"  = parseOperands2 SUB
> parseOp "MUL"  = parseOperands2 MUL
> parseOp "SHL"  = parseOperands2 SHL
> parseOp "SHR"  = parseOperands2 SHR
> parseOp "AND"  = parseOperands2 AND
> parseOp "BOR"  = parseOperands2 BOR
> parseOp "XOR"  = parseOperands2 XOR
> parseOp "DATA" = spaces1 >> (:[]) <$> parseData
> parseOp "LOAD" = parseOperandAndData $ EXT . LOAD
> parseOp "PUSH" = parseOperands1 $ EXT . PUSH
> parseOp "POP"  = parseOperands1 $ EXT . POP
> parseOp "PEEK" = parseOperands1 $ EXT . PEEK
> parseOp _      = fail "unknown instruction"

All of the basic instructions have a LHS operand and RHS operand.  This wraps
up this pattern as a reusable parser.  Operands are separated by a comma, and
at least one space must separate the first operand from the instruction.

> parseOperands2 :: (Operand -> Operand -> Op) -> Parser [Element]
> parseOperands2 op = do spaces1
>                        a <- parseOperandLHS
>                        spaces >> char ',' >> spaces
>                        b <- parseOperandRHS
>                        return [Op $ op a b]

Extended instruction often have a LHS operand and a full-word data operand.
This data operand will actually create an extra "DATA" instruction, but it is
much nicer to be able to write it inline in the assembly language.

> parseOperandAndData :: (Operand -> Op) -> Parser [Element]
> parseOperandAndData op = do spaces1
>                             a <- parseOperandLHS
>                             spaces >> char ',' >> spaces
>                             d <- parseData
>                             return [Op $ op a, d]

Some other extended instructions just have a LHS.

> parseOperands1 :: (Operand -> Op) -> Parser [Element]
> parseOperands1 op = spaces1 >> (:[]) <$> (Op . op) <$> parseOperandLHS

There are two kinds of operands: LHS and RHS.  LHS operands must refer to a
location - a register, frame offset or address.

TODO: make registers also case-insensitive.

> parseOperandLHS :: Parser Operand
> parseOperandLHS =   (char '@' >> Addr <$> register)
>                 <|> (char '$' >> Frame <$> register)
>                 <|> Reg <$> register
>                 <?> "operand"

RHS operands have the option of being a short (4-bit) literal.

TODO: range check literals -8 to 7.

> parseOperandRHS :: Parser Operand
> parseOperandRHS =   parseOperandLHS
>                 <|> Lit <$> fromIntegral <$> integer

"Data" elements are things that are going to take up a whole word on their own.
For example, the "LOAD a, 0xBEEF" instruction would be an (Op (LOAD (Reg 'a')))
followed by (Op (DATA 0xBEEF)).  For things like absolute jump instructions,
the value might be a label reference.  This parser allows for that, returning
a LabelRef element.  The assembler should replace these with DATA instructions
when it knows where the labels are.

TODO: range check literals -2^31 to 2^31-1.

> parseData :: Parser Element
> parseData =   Op . DATA <$> fromIntegral <$> integer
>           <|> LabelRef <$> identifier


If we run this module as a program, it will attempt to parse stdin and write
the resulting [Element] to stdout, or the error message.

> main = getContents >>= parseTest (parseAsm <* eof)
