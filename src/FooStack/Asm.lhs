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
>     encode,
>     decode,
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

We also have a masking function to set all irrelevant bits - those above the
extent of the chunk - to 0.

> mask :: Int -> Word -> Word
> mask n x = (2^n-1) .&. x

A function to extract the value from a Chunk.

> fromChunk :: Chunk -> Word
> fromChunk (Chunk n x) = mask n x

Chunks can be appended together into a bigger chunk - this is how we build the
instruction word.  Appending chunks is an associative operation, and it also
has an identity value - the empty chunk.  It's a monoid!

> instance Monoid Chunk where
>     mempty = Chunk 0 0
>     mappend (Chunk nx x) (Chunk ny y) = Chunk n xy
>         where
>         n = nx + ny
>         xy = (x `shiftL` ny) .|. (mask ny y)

We also need to be able to cut chunks up again.  The number of bits is counted
from the MSB of the *Chunk*, not the underlying word.

> splitChunkAt :: Int -> Chunk -> (Chunk, Chunk)
> splitChunkAt n (Chunk nxy xy)
>     | n >= 0 && n <= nxy = let nx = n
>                                ny = nxy - n
>                                x = mask nx $ xy `shiftR` ny
>                                y = mask ny xy
>                            in  (Chunk nx x, Chunk ny y)
>     | otherwise          = undefined

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

And now for the big part, encoding instructions.  Let's keep the Encodable
instance simple, because the implementation is going to be quite big.

> instance Encodable Instruction where
>     encode = encodeInstruction
>     decode = decodeInstruction

The implementation of instruction encoding.

> encodeInstruction :: Instruction -> Chunk
> -- (register, immediate) instructions, only 4 bits for opcode
> encodeInstruction (LDBI r b)   = mconcat [Chunk 4 0x0, encode r, encode b]
> encodeInstruction (LDI r b)    = mconcat [Chunk 4 0x1, encode r, encode b]
> encodeInstruction (ADDI r b)   = mconcat [Chunk 4 0x2, encode r, encode b]
> encodeInstruction (SUBI r b)   = mconcat [Chunk 4 0x3, encode r, encode b]
> encodeInstruction (SHLI r b)   = mconcat [Chunk 4 0x4, encode r, encode b]
> encodeInstruction (SHRI r b)   = mconcat [Chunk 4 0x5, encode r, encode b]
> encodeInstruction (SSLI r b)   = mconcat [Chunk 4 0x6, encode r, encode b]
> encodeInstruction (SSRI r b)   = mconcat [Chunk 4 0x7, encode r, encode b]
> -- (register, register) instructions, 8-bit opcode, extend unused 4-bit
> encodeInstruction (LD ra rb)   = mconcat [Chunk 8 0xF0, encode ra, encode rb]
> encodeInstruction (LDB ra rb)  = mconcat [Chunk 8 0xF1, encode ra, encode rb]
> encodeInstruction (ADD ra rb)  = mconcat [Chunk 8 0xF2, encode ra, encode rb]
> encodeInstruction (ADDC ra rb) = mconcat [Chunk 8 0xF3, encode ra, encode rb]
> encodeInstruction (SUB ra rb)  = mconcat [Chunk 8 0xF4, encode ra, encode rb]
> encodeInstruction (SUBB ra rb) = mconcat [Chunk 8 0xF5, encode ra, encode rb]
> encodeInstruction (AND ra rb)  = mconcat [Chunk 8 0xF6, encode ra, encode rb]
> encodeInstruction (OR ra rb)   = mconcat [Chunk 8 0xF7, encode ra, encode rb]
> encodeInstruction (XOR ra rb)  = mconcat [Chunk 8 0xF8, encode ra, encode rb]
> encodeInstruction (SHL ra rb)  = mconcat [Chunk 8 0xF9, encode ra, encode rb]
> encodeInstruction (SHR ra rb)  = mconcat [Chunk 8 0xFA, encode ra, encode rb]
> encodeInstruction (SSL ra rb)  = mconcat [Chunk 8 0xFB, encode ra, encode rb]
> encodeInstruction (SSR ra rb)  = mconcat [Chunk 8 0xFC, encode ra, encode rb]
> -- (immediate) instructions, 8-bit opcode, extend unused 4-bit
> encodeInstruction (JRI b)      = mconcat [Chunk 8 0xE0, encode b]
> encodeInstruction (JRINZ b)    = mconcat [Chunk 8 0xE1, encode b]
> -- (register) instructions, 12-bit opcode, extend unused 8-bit
> encodeInstruction (PUSH r)     = mconcat [Chunk 12 0xFF0, encode r]
> encodeInstruction (POP r)      = mconcat [Chunk 12 0xFF1, encode r]
> encodeInstruction (PEEK r)     = mconcat [Chunk 12 0xFF2, encode r]
> encodeInstruction (NEG r)      = mconcat [Chunk 12 0xFF3, encode r]
> encodeInstruction (NOT r)      = mconcat [Chunk 12 0xFF4, encode r]
> encodeInstruction (JMP r)      = mconcat [Chunk 12 0xFF5, encode r]
> encodeInstruction (JR r)       = mconcat [Chunk 12 0xFF6, encode r]
> encodeInstruction (JMPNZ r)    = mconcat [Chunk 12 0xFF7, encode r]
> encodeInstruction (JRNZ r)     = mconcat [Chunk 12 0xFF8, encode r]
> -- instructions with no operand, 16-bit opcode, extend unused 12-bit
> encodeInstruction (HALT)       = Chunk 16 0xFFFF

The implementation of instruction decoding.

> decodeInstruction :: Chunk -> Instruction
> decodeInstruction = uncurry decode4 . splitChunkAt 4
> decodeArity2 :: (Encodable a, Encodable b) =>
>                 Int -> (a -> b -> Instruction) -> Chunk -> Instruction
> decodeArity2 n i c = let (a, b) = splitChunkAt n c
>                      in  i (decode a) (decode b)
> decode4 :: Chunk -> Chunk -> Instruction
> decode4 (Chunk 4 0x0)      = decodeArity2 4 LDBI
> decode4 (Chunk 4 0x1)      = decodeArity2 4 LDI
> decode4 (Chunk 4 0x2)      = decodeArity2 4 ADDI
> decode4 (Chunk 4 0x3)      = decodeArity2 4 SUBI
> decode4 (Chunk 4 0x4)      = decodeArity2 4 SHLI
> decode4 (Chunk 4 0x5)      = decodeArity2 4 SHRI
> decode4 (Chunk 4 0x6)      = decodeArity2 4 SSLI
> decode4 (Chunk 4 0x7)      = decodeArity2 4 SSRI
> decode4 opcode             = uncurry (decode8 . mappend opcode) . splitChunkAt 4
> decode8 :: Chunk -> Chunk -> Instruction
> decode8 (Chunk 8 0xF0)     = decodeArity2 4 LD
> decode8 (Chunk 8 0xF1)     = decodeArity2 4 LDB
> decode8 (Chunk 8 0xF2)     = decodeArity2 4 ADD
> decode8 (Chunk 8 0xF3)     = decodeArity2 4 ADDC
> decode8 (Chunk 8 0xF4)     = decodeArity2 4 SUB
> decode8 (Chunk 8 0xF5)     = decodeArity2 4 SUBB
> decode8 (Chunk 8 0xF6)     = decodeArity2 4 AND
> decode8 (Chunk 8 0xF7)     = decodeArity2 4 OR
> decode8 (Chunk 8 0xF8)     = decodeArity2 4 XOR
> decode8 (Chunk 8 0xF9)     = decodeArity2 4 SHL
> decode8 (Chunk 8 0xFA)     = decodeArity2 4 SHR
> decode8 (Chunk 8 0xFB)     = decodeArity2 4 SSL
> decode8 (Chunk 8 0xFC)     = decodeArity2 4 SSR
> decode8 (Chunk 8 0xE0)     = JRI . decode
> decode8 (Chunk 8 0xE1)     = JRINZ . decode
> decode8 opcode             = uncurry (decode12 . mappend opcode) . splitChunkAt 4
> decode12 :: Chunk -> Chunk -> Instruction
> decode12 (Chunk 12 0xFF0)  = PUSH . decode
> decode12 (Chunk 12 0xFF1)  = POP . decode
> decode12 (Chunk 12 0xFF2)  = PEEK . decode
> decode12 (Chunk 12 0xFF3)  = NEG . decode
> decode12 (Chunk 12 0xFF4)  = NOT . decode
> decode12 (Chunk 12 0xFF5)  = JMP . decode
> decode12 (Chunk 12 0xFF6)  = JR . decode
> decode12 (Chunk 12 0xFF7)  = JMPNZ . decode
> decode12 (Chunk 12 0xFF8)  = JRNZ . decode
> decode12 opcode            = decode16 . mappend opcode
> decode16 :: Chunk -> Instruction
> decode16 (Chunk 16 0xFFFF) = HALT
