Things we want to be able to do:

- Basic arithmetic operations (+, -, *, /?, %?).
- Basic bitwise operations (&, |, ^, ~?, <<, >>).
- Load literal values into registers.
- Store/load values from memory.
- Move stuff between registers.
- Have enough registers!
- Stack operations (push, pop, peek).
- Control flow operations, i.e. direct jumps, relative jumps, conditional jumps.
- Support basic calling + execution conventions, i.e. stack frames.

Some things these requirments might suggest:
- Stack pointer and frame pointer.
- Argument registers to form part of the calling convention for the first n arguments.

> module FooStack.Asm (
>     Byte,
>     Word,
>     Address,
>     Register(..),
>     HalfRegister(..),
>     Instruction(..),
>     ) where

> import Data.Word (Word8, Word16)
> import Data.Binary.Put (putWord16be, Put, runPut)
> import Data.ByteString.Lazy (ByteString)
> import Data.Bits (Bits, (.|.), (.&.), shiftL, shiftR)
> import Data.List (elemIndex)
> import Data.Maybe (fromJust)
> import Data.Monoid


THE BASICS
==========

I'll often be talking about Bytes and Words.  Bytes are always 8 bits - what
else would they be?

> type Byte = Word8

The machine is 16-bit at heart, so Word means Word16.

> type Word = Word16

Memory is a 16-bit address space, with byte addressing like most architectures.
I've decided on little endian byte order when representing words as bytes, by
the hugely scientific process of reading the wiki page and picking whichever
seemed cleverer.

> type Address = Word


INSTRUCTION ENCODING INTRODUCTION
=================================

A bit of a chicken-and-egg problem: "which came first, the instructions or the
instruction encoding?"  The encoding needs to represent the operations, but it
also constrains the kinds of operations we can have.  To kick things off, I
like the idea of constant-width instructions, so I'm going to stick with that.
This doesn't seem to be the normal approach, but oh well - I feel it will be
easier to keep track of.

To make this work, I need a consistent, easy-to-decode scheme to pack opcodes
and operands into the 16-bit instruction word.  I'm going to borrow a concept
that Notch's DCPU-16 brought to my attention: splitting opcodes into nested
classes, where an opcode of 0 in the outer class signifies that an inner opcode
is present.  This lets us have a few instructions with lots of operand bits,
and lots of instructions with few operand bits, hopefully balancing the needs
for powerful and numerous instructions.

Originally I was tempted to go with a DCPU-16-style operand format, using the
same generic 6-bit operand format everywhere.  Unfortunately, this really
limits us when it comes to possible operand values, so instead the operand bits
are given meaning by the instruction they're used in.  This is a much more
flexible environment for designing instructions.


REGISTERS
=========

Registers are the staple diet of code, and they need to be well thought out.
We need to consider the kinds of things people will want to do with them, and
maybe even establish some recommended conventions for using them, as
architectures often have done.  We also need to keep in mind that we want
plenty of registers, but we need to be able to address them all.  We also don't
want too many registers; we don't gain much speed if we replace memory accesses
for variables with memory accesses for preserving lots of registers!

To make the decision for the sake of making it, I'm going to have a uniform set
of 16 16-bit registers.  These will include general purpose registers and
specific purpose registers, e.g. the program counter and stack pointer.  This
gives maximum flexibility for ways that code can affect its operation.  16-bit
registers can be addressed with a 4-bit identifier under this scheme, which
gives a lot of scope for 2- and possibly even 3-operand instructions.

As already mentioned, we need a program counter and a stack pointer no matter
what.

> data Register = PC
>               | SP

We also need some general-purpose registers.  The naming is reminiscent of 8086
general-purpose registers because they're actually going to work somewhat like
those (see below).

>               | AX
>               | BX
>               | CX
>               | DX

We're also going to want to be able to do "indirect offset" addressing of some
kind because it's so useful in higher-level languages (arrays, stack frames,
etc.), so let's provide enough base address registers to be able to do this
with reasonable scope for a good usage convention.  The naming is inspired by
having used the Z80/Z180.

>               | IX
>               | IY
>               | IZ

TODO: There should probably be more registers, I'm sure I'll probably come up
with some at a later date.

>               deriving (Eq, Enum, Show, Read)

I mentioned that the general-purpose registers were somewhat like those in the
8086.  What I meant is that I think the ability to work with individual bytes is
pretty useful, so e.g. for AX there is a corresponding AH and AL for the high
and low bytes respectively.  If we allow 4-bit byte register identifiers, then
as many as half of our word registers can be used as pairs of byte registers.

> data HalfRegister = AH | AL
>                   | BH | BL
>                   | CH | CL
>                   | DH | DL
>                   deriving (Eq, Enum, Show, Read)


THE INSTRUCTION SET
===================

Now that the really low level considerations are out of the way, we can start
thinking about the operations that need to be provided.

Mostly I'm going to make these mnemonics up on the spot, hopefully following
some kind of intelligible convention.  I'm also going to try not to reference
the encoding too much in this section, since that should mostly be a separate
concern.  The main concern is that the instruction set should fit within the
encoding.

To start with, let's have the HALT instruction.  It sits there and does nothing
whatsoever, not even incrementing the program counter.

> data Instruction = HALT

Obviously we need to be able to directly load values into registers, otherwise
we couldn't have any kind of literal value in our code!  With a 16-bit fixed
width instruction format and a 16-bit register width, we have an obvious
problem: we can't load 16-bit values.  Not to worry though, we can easily load
bytes and still have room to spare to specify where to.  Since our general-
purpose registers have byte access, we can just load the low byte and the high
byte.  LDBI sets the value of the specified byte register, having no effect on
the other half of the word register it belongs to.

>                  | LDBI HalfRegister Byte         -- Load byte immediate

Commonly we'll want to load small values into registers.  It would be an
unnecessary burden if we need to execute 2 instructions to do this.  For this
case, let's create the LDI instruction which sets the low byte of a register
to the specified value and the high byte to 0.

TODO: Sign-extend the low byte into the high byte instead?

>                  | LDI Register Byte              -- Load immediate

Another fundamental operation on registers is copying between them.  Let's
allow this for both full and half registers.

>                  | LD Register Register           -- Load
>                  | LDB HalfRegister HalfRegister  -- Load byte

Getting things to and from the stack is going to be common, so here are some
instructions for communicating between the stack and a register.  The stack
extends downwards.

>                  | PUSH Register                  -- [--SP] = r
>                  | POP Register                   -- r = [SP++]
>                  | PEEK Register                  -- r = [SP]

Next let's have some arithmetic.  I've decided to use two-operand arithmetic,
where the first operand is always overwritten with the result.  All arithmetic
operations are on two word registers.  Because a lot of arithmetic works with
smallish changes, most of these have an "immediate" version which operates on
a register and a supplied byte, and is signified by an appended "I".

TODO: Add more operators?

First we have basic addition, subtraction and negation.

>                  | ADD Register Register          -- Add
>                  | ADDI Register Byte             -- Add immediate
>                  | ADDC Register Register         -- Add with carry
>                  | SUB Register Register          -- Subtract
>                  | SUBI Register Byte             -- Subtract immediate
>                  | SUBB Register Register         -- Subtract with borrow
>                  | NEG Register                   -- Two's complement negation

Next we have bitwise combination operations.

>                  | AND Register Register          -- Bitwise AND
>                  | OR Register Register           -- Bitwise OR
>                  | XOR Register Register          -- Bitwise XOR
>                  | NOT Register                   -- One's complement negation

Shift left and shift right are very useful.  SHx is unigned shift, SSx is
signed/arithmetic shift.

>                  | SHL Register Register          -- Shift left
>                  | SHLI Register Byte             -- Shift left immediate
>                  | SHR Register Register          -- Shift right
>                  | SHRI Register Byte             -- Shift right immediate
>                  | SSL Register Register          -- Signed shift left
>                  | SSLI Register Byte             -- Signed shift left immediate
>                  | SSR Register Register          -- Signed shift right
>                  | SSRI Register Byte             -- Signed shift right immediate

We're going to have a hard time implementing control flow without any kind of
jump.  Firstly let's add absolute and relative jumps.  Relative jump values can
be positive or negative.  Relative jumps are relative to the current
instruction, not PC + 1.

>                  | JMP Register                   -- Jump, PC = r
>                  | JR Register                    -- Jump relative, PC += r

Local relative jumps are extremely common, so we should support them in the
instruction set with an "immediate" version.

>                  | JRI Byte                       -- Jump relative immediate

Conditional jumps are also essential.  For now let's create some very simple
conditional jumps, the "jump if not zero" kind.

>                  | JMPNZ Register     -- Jump if not zero
>                  | JRNZ Register      -- Jump relative if not zero
>                  | JRINZ Byte         -- Jump relative immediate if not zero

TODO: Add more instructions.

>                  deriving (Eq, Show, Read)


ENCODING
========

Everything we have defined so far needs to be able to be packed into 16-bit
instruction words.  Packing is going to require handling partial words, so
let's create a type to deal with that.  Partial words are (Chunk n x) where n
is the bit width of the chunk and x is the value.

> data Chunk = Chunk Int Word deriving (Eq, Show)
> fromChunk :: Chunk -> Word
> fromChunk (Chunk _ x) = x

Chunks can be appended together into a bigger chunk - this is how we build the
instruction word.  Appending chunks is an associative operation, and it also
has an identity value - the empty chunk.  It's a monoid!

> instance Monoid Chunk where
>     mempty = Chunk 0 0
>     mappend (Chunk xn x) (Chunk yn y) = Chunk (xn + yn) ((x `shiftL` yn) .|. y)

We need a nice way of encoding these abstract things.  How about creating a
typeclass for things that can be encoded.

> class Encodable a where
>     encode :: a -> Chunk
>     decode :: Chunk -> a

And now we can define how to encode things.  Register and half-register
references are easy, because they derive Enum.  It's worth bearing in mind that
the use of Enum means we can only ever append new register names if we want old
binaries to still work.

> instance Encodable Register where
>     encode = Chunk 4 . fromIntegral . fromEnum
>     decode = toEnum  . fromIntegral . fromChunk

> instance Encodable HalfRegister where
>     encode = Chunk 4 . fromIntegral . fromEnum
>     decode = toEnum  . fromIntegral . fromChunk

Encoding bytes is even easier!

> instance Encodable Word8 where
>     encode = Chunk 8 . fromIntegral
>     decode = fromIntegral . fromChunk

And now for the big part, encoding instructions...

> instance Encodable Instruction where
>     encode (LDI r b) = mconcat [Chunk 4 0x1, encode r, encode b]
>     -- TODO: this is a dummy setting everything unknown to HALT
>     encode _ = Chunk 16 0
>     -- TODO: this is a dummy setting everything unknown to HALT
>     decode _ = HALT


Judging by the above instructions, and earlier discussion
of the instruction encoding we have a limited number of packing patterns, and
they all seem to be based on 4-bit chunks.  Certain opcode values can signify
that there is an "extended" opcode nested inside, and the whole thing forms a
tree of possible encodings.

   |-- 0000
   |    |-- 0000
   |    |    |-- 0000
   |    |    |    `-- oooo
   |    |    `-- oooo rrrr
   |    |-- oooo rrrr rrrr
   |    `-- oooo iiiiiiii
   `-- oooo
        `-- rrrr iiiiiiii
