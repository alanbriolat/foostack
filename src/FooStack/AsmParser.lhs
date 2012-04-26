> module FooStack.AsmParser where

> import Data.List (elemIndex)
> import Data.Char (toUpper, toLower)
> import Control.Applicative ((<$>), (<*))
> import Text.ParserCombinators.Parsec hiding (label, space, spaces)
> import qualified Text.Parsec.Token as Token
> import qualified Text.Parsec.Language as Language

> import FooStack.Asm


BASIC TOKEN PARSERS
===================

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
what is here is inspired by the implementation of Parsec's "integer".  For now
let's only worry about decimal and hexidecimal integers.

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

Registers must be a valid register letter.  They are stored in the assembly AST
as a lowercase character, but usage in the source should be case-insensitive.

> register :: Parser Register
> register = toLower <$> oneOf (registers ++ uppercase registers)

Basic identifier parsing: alphanumeric plus underscore, and must start with a
letter.  Identifiers must be at least 2 characters long, to make them
completely unambiguous compared to register references.  (This is a source
readability convenience, not necessary for the parser.)

> identifier :: Parser String
> identifier =   do first <- letter
>                   rest <- (many1 $ alphaNum <|> char '_')
>                   return (first:rest)
>            <?> "identifier"

The identifier parser is sufficient for parsing instruction names; validity of
the instruction name can be sorted once we're sure we're trying to treat it as
an instruction.

> instruction :: Parser String
> instruction = identifier <?> "instruction"

We can also re-used the identifier parser for assembler directives.  Like with
instructions, whether or not it is valid can be handled by the directive
parser.  Directives start with a period.

> directive :: Parser String
> directive = char '.' >> identifier <?> "directive"

A target is a named code location.  It consists of the target name followed by
a colon.  A target can either be a symbol (global name) or a label (local name).
Symbols should be globally unique, whereas labels should be unique between
symbols.  Labels are signified by a leading underscore.

The identifier parser can be reused for both symbols and labels.

> symbol :: Parser String
> symbol = identifier <?> "symbol"

> label :: Parser String
> label = char '_' >> identifier <?> "label"

> target :: Parser Element
> target =   do e <- (Label <$> label) <|> (Symbol <$> symbol)
>               spaces >> char ':' >> spaces
>               return e
>        <?> "target"

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


THE ABSTRACT SYNTAX TREE
========================

Because we're going to resolve a lot of stuff in the post-parsing step, we need
a well-annotated AST.  Luckly Parsec makes this easy with the SourcePos data
that's available.

> type AST = [ASTNode]
> type ASTNode = (SourcePos, Element)

There are several different possible source elements.

> data Element = Instruction Op
>              | Directive Directive
>              | Symbol String
>              | SymbolRef String
>              | Label String
>              | LabelRef String
>              deriving (Show)

We don't want to clutter the code with annotation boilerplate everywhere, so
let's define a helper that annotates the result of a parser with the token's
start position.

> ann :: Parser a -> Parser (SourcePos, a)
> ann parser = do pos <- getPosition
>                 token <- parser
>                 return (pos, token)


THE ASSEMBLY PARSER
===================

At the top level, we are parsing on a line-by-line basis.  Input lines are
therefore separated by one or more line endings, and as defined above line
endings include line comments.

> parseAsm :: Parser AST
> parseAsm = concat <$> parseLine `sepEndBy` lineending <* eof

Each line consists of either a lone assembler directive, or a target,
instruction, or both.  This gives us the flexibility of multiple targets for
the same location, e.g. a subroutine that starts with a loop.

> parseLine :: Parser AST
> parseLine =   parseDirective
>           <|> concat <$> sequence [ option [] (try parseTarget)
>                                   , option [] (try parseInstruction)
>                                   ]

Here's a really basic directive element parser.  It should be fleshed out to
handle useful directives.

> data Directive = UnknownDirective String
>                deriving (Show)

> parseDirective :: Parser AST
> parseDirective = (:[]) <$> ann ((Directive . UnknownDirective) <$> directive)

As described above, a target is either a symbol or a label that gives a name
to a code location.

> parseTarget :: Parser AST
> parseTarget = (:[]) <$> ann target

An instruction is an identifier followed by some instruction-specific handling.
We make the instruction uppercase so we can treat it case-insensitively.

> parseInstruction :: Parser AST
> parseInstruction = ann (uppercase <$> instruction) >>= parseOp

Each valid instruction can be parsed differently, but generally speaking they
will make use of more generic operand parsing.

> parseOp :: (SourcePos, String) -> Parser AST
> parseOp (pos, "SET" ) = parseOperands2 pos SET
> parseOp (pos, "ADD" ) = parseOperands2 pos ADD
> parseOp (pos, "SUB" ) = parseOperands2 pos SUB
> parseOp (pos, "MUL" ) = parseOperands2 pos MUL
> parseOp (pos, "SHL" ) = parseOperands2 pos SHL
> parseOp (pos, "SHR" ) = parseOperands2 pos SHR
> parseOp (pos, "AND" ) = parseOperands2 pos AND
> parseOp (pos, "BOR" ) = parseOperands2 pos BOR
> parseOp (pos, "XOR" ) = parseOperands2 pos XOR
> parseOp (pos, "DATA") = spaces1 >> (:[]) <$> parseData
> parseOp (pos, "LOAD") = parseOperandAndData pos (EXT . LOAD)
> parseOp (pos, "PUSH") = parseOperands1 pos (EXT . PUSH)
> parseOp (pos, "POP" ) = parseOperands1 pos (EXT . POP)
> parseOp (pos, "PEEK") = parseOperands1 pos (EXT . PEEK)
> parseOp _             = fail "unknown instruction"

All of the basic instructions have a LHS operand and RHS operand.  This wraps
up this pattern as a reusable parser.  Operands are separated by a comma, and
at least one space must separate the first operand from the instruction.

> parseOperands2 :: SourcePos -> (Operand -> Operand -> Op) -> Parser AST
> parseOperands2 pos op = do spaces1
>                            a <- parseOperandLHS
>                            spaces >> char ',' >> spaces
>                            b <- parseOperandRHS
>                            return [(pos, Instruction $ op a b)]

Extended instruction often have a LHS operand and a full-word data operand.
This data operand will actually create an extra instruction, but it is much
nicer to be able to write it inline in the assembly language.

> parseOperandAndData :: SourcePos -> (Operand -> Op) -> Parser AST
> parseOperandAndData pos op = do spaces1
>                                 a <- parseOperandLHS
>                                 spaces >> char ',' >> spaces
>                                 d <- parseData
>                                 return [(pos, Instruction $ op a), d]

Some other extended instructions just have a LHS.

> parseOperands1 :: SourcePos -> (Operand -> Op) -> Parser AST
> parseOperands1 pos op = do spaces1
>                            a <- parseOperandLHS
>                            return [(pos, Instruction $ op a)]

There are two kinds of operands: LHS and RHS.  LHS operands must refer to a
location - a register, frame offset or address.

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

Data elements are things that are going to take up a whole word on their own.
They can either be a literal 16-bit word or a target; in the case of a target
they will eventually take a value related to the address of the target.

TODO: range check literals -2^31 to 2^31-1.

> parseData :: Parser ASTNode
> parseData = ann (   LabelRef <$> label
>                 <|> SymbolRef <$> symbol
>                 <|> Instruction . DATA <$> fromIntegral <$> integer
>                 )


If we run this module as a program, it will attempt to parse stdin and write
the resulting AST to stdout, or the error message.

> main = do c <- getContents
>           case parse parseAsm "" c of
>             Left err  -> do putStr "parse error at "
>                             print err
>             Right x   -> mapM_ print x
>
