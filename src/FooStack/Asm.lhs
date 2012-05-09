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
>     byteRegisters,
>     Instruction(..),
>     encodeOp, decodeOp,
>     ) where

> import Data.Word (Word8, Word16)
> import Data.Binary.Put (putWord16be, Put, runPut)
> import Data.ByteString.Lazy (ByteString)
> import Data.Bits ((.|.), (.&.), shiftL, shiftR)
> import Data.List (elemIndex)
> import Data.Maybe (fromJust)


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
I've borrowed the general concept of opcode encoding from what I saw in Notch's
DCPU-16: a 4-bit "basic" opcode with 12 bits of operand, with an opcode of 0
denoting an "extended" opcode which takes the next 6 bits and has 6 bits of
operand.  In fact I've decided to take it even further, so an extended opcode
of 0 signifies an opcode in the last 6 bits, with no operand.  This gives us a
total opcode space of 142: 15 with 12-bit operands, 63 with 6-bit operands and
64 with no operand.  I'm aiming for a RISC-style architecture, so this should
be plenty.

Originally I was tempted to go with a DCPU-16-style operand format, using the
same generic 6-bit operand format everywhere.  Unfortunately, this really
limits us when it comes to immediate values and available registers.  Instead
I've decided to take the approach where the operand format is dependent on the
instruction.  This gives a much more flexible environment for designing
instructions.


REGISTERS
=========

Registers are the staple diet of code, and they need to be well thought out.
We need to consider the kinds of things people will want to do with them, and
maybe even establish some recommended conventions for using them, as
architectures often have done.  We also need to keep in mind that we want
plenty of registers, but we need to be able to address them all.  We are
limited to 64 register names if we want to be able to use any pair in
two-register instructions.

I'm going to make the decision now that all registers are accessible, none
hidden.  This means it's possible to jump in a unreturnable way by setting the
program counter, as an example.  We're definitely going to want a stack pointer
and a program counter, so let's create those.

> data Register = SP
>               | PC

We'll want some general-purpose registers too.  I'm going to borrow from x86
here and let general purpose registers be accessible as 16-bit values, or as
two 8-bit values - high byte and low byte.  For register A these would be AX,
AH and AL.  To digress into instruction set concerns, it is worth noting that
we could create a "load byte" instruction with an 8-bit immediate value and 4
bits of operand remaining, which can then identify the 8-bit register target.
This limits us to 16 8-bit registers, i.e. 8 full registers that can have their
bytes addressed individually.  For now let's create 4 general-purpose registers
from A to D.

>               | AX | AH | AL
>               | BX | BH | BL
>               | CX | CH | CL
>               | DX | DH | DL

Relative indexing is a really common thing to want to do, so let's provide some
base address registers.  We'll want at least 2, to account for "global" and
"frame" pointers, because these are such common uses.  Let's have 3 so we still
have one to play with.

>               | IX | IY | IZ

I'm sure there will be more to follow as I think things up, or as I try and use
the CPU for something.

For the benefit of the assembler, these are the valid byte registers.

> byteRegisters :: [Register]
> byteRegisters = [AH, AL, BH, BL, CH, CL, DH, DL]


THE INSTRUCTION SET
===================

Now that the really low level considerations are out of the way, we can start
thinking about the operations that need to be provided.

Mostly I'm going to make these mnemonics up on the spot, hopefully following
some kind of intelligible convention.  I'm also going to try not to reference
the encoding too much in this section, since that should mostly be a separate
concern.

To start with, let's have the HALT instruction.  It sits there and does nothing
whatsoever, not even incrementing the program counter.

> data Instruction = HALT

Obviously we need to be able to get values into registers.  Using the idea
discussed earlier we can load individual bytes into byte registers.  It's the
job of the assembler to make sure the specified register is actually allowed
for this usage, since only a subset of registers are actually valid.

TODO: Restrict this in a better way.

>                  | LDBI Register Byte

Next, let's tackle copying values between registers.  We can copy values
between any two registers.  If a word register is copied to a byte register,
the top byte is dropped.  If a byte register is copied to a word register, the
top byte is set to 0.

>                  | LD Register Register

Borrowing DCPU-16's instruction format, bbbbbbaaaaaaoooo.  That is, a 4-bit
opcode and two 6-bit operands.

> data Op = EXT ExtOp               -- 0x0, extended instruction
>         | SET Operand Operand     -- 0x1, set a = b
>         | ADD Operand Operand     -- 0x2, set a = a + b, sets carry flag
>         | SUB Operand Operand     -- 0x3, set a = a - b, sets carry flag
>         | MUL Operand Operand     -- 0x4, set a = a * b, sets carry flag
>         | SHL Operand Operand     -- 0x5, set a = a << b, sets carry flag
>         | SHR Operand Operand     -- 0x6, set a = a >> b, sets carry flag
>         | AND Operand Operand     -- 0x7, set a = a & b
>         | BOR Operand Operand     -- 0x8, set a = a | b
>         | XOR Operand Operand     -- 0x9, set a = a ^ b
>         | DATA Word               -- a raw 16-bit integer
>         deriving (Show)

To achieve a larger range of available opcodes, the "extended" opcodes are
signified by a "basic" opcode of 0x0.  Extended operations also follow the
DCPU-16 format, aaaaaaoooooo0000.  That is, a 6-bit opcode and a single 6-bit
operand.

> data ExtOp = ExtReserved          -- 0x00, reserved for extra-extended opcodes
>            | LOAD Operand         -- 0x01, set a = next word
>            | PUSH Operand         -- 0x02, set [SP++] = a
>            | POP Operand          -- 0x03, set a = [--SP]
>            | PEEK Operand         -- 0x04, set a = [SP]
>            deriving (Show)

Operands need to provide several different methods for playing with data.  We
have 6 bits to play with.  If we want a reasonable number of registers (16),
that leaves us 2 bits for addressing mode.

> data Operand = Lit Word           -- 00xxxx: short literals (-8 to +7)
>              | Reg Register       -- 01xxxx: r = register r
>              | Frame Register     -- 10xxxx: $r = memory location [FP + r]
>              | Addr Register      -- 11xxxx: @r = memory location [r]
>              deriving (Show)

Define the binary representations of operands:

> encodeOperand :: Operand -> Word
> encodeOperand = encode
>     where
>     combine :: Word -> Word -> Word
>     combine mode a = ((mode .&. 0x3) `shiftL` 4) .|. (a .&. 0xF)
>     encode :: Operand -> Word
>     encode (Lit a)   = combine 0x0 a
>     encode (Reg r)   = combine 0x1 $ fromIntegral $ fromJust $ r `elemIndex` registers
>     encode (Frame r) = combine 0x2 $ fromIntegral $ fromJust $ r `elemIndex` registers
>     encode (Addr r)  = combine 0x3 $ fromIntegral $ fromJust $ r `elemIndex` registers

Define the binary representation of instructions.  To recap, the basic
instruction format is bbbbbbaaaaaaoooo.  Extended instructions are
aaaaaaoooooo0000.  The special case of a "data" operation is just a 16-bit
literal.

> encodeOp :: Op -> Word
> encodeOp = encode
>     where
>     combine :: Word -> Word -> Word -> Word
>     combine o a b = (o .&. 0xF) .|. ((a .&. 0x3F) `shiftL` 4) .|. ((b .&. 0x3F) `shiftL` 10)
>     encode :: Op -> Word
>     encode (SET a b) = combine 0x1 (encodeOperand a) (encodeOperand b)
>     encode (ADD a b) = combine 0x2 (encodeOperand a) (encodeOperand b)
>     encode (SUB a b) = combine 0x3 (encodeOperand a) (encodeOperand b)
>     encode (MUL a b) = combine 0x4 (encodeOperand a) (encodeOperand b)
>     encode (SHL a b) = combine 0x5 (encodeOperand a) (encodeOperand b)
>     encode (SHR a b) = combine 0x6 (encodeOperand a) (encodeOperand b)
>     encode (AND a b) = combine 0x7 (encodeOperand a) (encodeOperand b)
>     encode (BOR a b) = combine 0x8 (encodeOperand a) (encodeOperand b)
>     encode (XOR a b) = combine 0x9 (encodeOperand a) (encodeOperand b)
>     encode (DATA a)  = a .&. 0xFFFF
>     encode (EXT (LOAD a)) = combine 0x0 0x01 (encodeOperand a)
>     encode (EXT (PUSH a)) = combine 0x0 0x02 (encodeOperand a)
>     encode (EXT (POP  a)) = combine 0x0 0x03 (encodeOperand a)
>     encode (EXT (PEEK a)) = combine 0x0 0x04 (encodeOperand a)

Decode an operand:

> decodeOperand :: Word -> Operand
> decodeOperand = decode . split
>     where
>     split :: Word -> (Word, Word)
>     split a = ((a .&. 0x30) `shiftR` 4, a .&. 0xF)
>     decode :: (Word, Word) -> Operand
>     decode (0x0, a) = Lit a
>     decode (0x1, r) = Reg $ registers !! fromIntegral r
>     decode (0x2, r) = Frame $ registers !! fromIntegral r
>     decode (0x3, r) = Addr $ registers !! fromIntegral r

Decode an instruction.  Note that we can't decode data instructions because
there is nothing to mark them as such.  When reading a binary, we need to make
sure we handle instructions that are followed by data and read the data in.

> decodeOp :: Word -> Op
> decodeOp = decode . split
>     where
>     split :: Word -> (Word, Word, Word)
>     split inst = (inst .&. 0xF, (inst `shiftR` 4) .&. 0x3F, (inst `shiftR` 10) .&. 0x3F)
>     decode :: (Word, Word, Word) -> Op
>     decode (0x1, a, b) = SET (decodeOperand a) (decodeOperand b)
>     decode (0x2, a, b) = ADD (decodeOperand a) (decodeOperand b)
>     decode (0x3, a, b) = SUB (decodeOperand a) (decodeOperand b)
>     decode (0x4, a, b) = MUL (decodeOperand a) (decodeOperand b)
>     decode (0x5, a, b) = SHL (decodeOperand a) (decodeOperand b)
>     decode (0x6, a, b) = SHR (decodeOperand a) (decodeOperand b)
>     decode (0x7, a, b) = AND (decodeOperand a) (decodeOperand b)
>     decode (0x8, a, b) = BOR (decodeOperand a) (decodeOperand b)
>     decode (0x9, a, b) = XOR (decodeOperand a) (decodeOperand b)
>     decode (0x0, 0x01, a) = EXT (LOAD (decodeOperand a))
>     decode (0x0, 0x02, a) = EXT (PUSH (decodeOperand a))
>     decode (0x0, 0x03, a) = EXT (POP  (decodeOperand a))
>     decode (0x0, 0x04, a) = EXT (PEEK (decodeOperand a))
