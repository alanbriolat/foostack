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

> module FooStack.Asm where
> import Data.Word (Word16)
> import Data.Binary.Put (putWord16be, Put, runPut)
> import Data.ByteString.Lazy (ByteString)
> import Data.Bits ((.|.), (.&.), shiftL, shiftR)
> import Data.List (elemIndex)
> import Data.Maybe (fromJust)

A register identifier is an integer from 0 to 15.

> type Register = Char

Registers should be referred to in code by a single-letter alias.  The final 4
should be used as "argument registers", hence the separate naming.

> registers :: [Register]
> registers = "abcdefghijklwxyz"

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
>         | DATA Word16             -- a raw 16-bit integer
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

> data Operand = Lit Word16         -- 00xxxx: short literals (-8 to +7)
>              | Reg Register       -- 01xxxx: r = register r
>              | Frame Register     -- 10xxxx: $r = memory location [FP + r]
>              | Addr Register      -- 11xxxx: @r = memory location [r]
>              deriving (Show)

Define the binary representations of operands:

> encodeOperand :: Operand -> Word16
> encodeOperand = encode
>     where
>     combine :: Word16 -> Word16 -> Word16
>     combine mode a = ((mode .&. 0x3) `shiftL` 4) .|. (a .&. 0xF)
>     encode :: Operand -> Word16
>     encode (Lit a)   = combine 0x0 a
>     encode (Reg r)   = combine 0x1 $ fromIntegral $ fromJust $ r `elemIndex` registers
>     encode (Frame r) = combine 0x2 $ fromIntegral $ fromJust $ r `elemIndex` registers
>     encode (Addr r)  = combine 0x3 $ fromIntegral $ fromJust $ r `elemIndex` registers

Define the binary representation of instructions.  To recap, the basic
instruction format is bbbbbbaaaaaaoooo.  Extended instructions are
aaaaaaoooooo0000.  The special case of a "data" operation is just a 16-bit
literal.

> encodeOp :: Op -> Word16
> encodeOp = encode
>     where
>     combine :: Word16 -> Word16 -> Word16 -> Word16
>     combine o a b = (o .&. 0xF) .|. ((a .&. 0x3F) `shiftL` 4) .|. ((b .&. 0x3F) `shiftL` 10)
>     encode :: Op -> Word16
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

> decodeOperand :: Word16 -> Operand
> decodeOperand = decode . split
>     where
>     split :: Word16 -> (Word16, Word16)
>     split a = ((a .&. 0x30) `shiftR` 4, a .&. 0xF)
>     decode :: (Word16, Word16) -> Operand
>     decode (0x0, a) = Lit a
>     decode (0x1, r) = Reg $ registers !! fromIntegral r
>     decode (0x2, r) = Frame $ registers !! fromIntegral r
>     decode (0x3, r) = Addr $ registers !! fromIntegral r

Decode an instruction.  Note that we can't decode data instructions because
there is nothing to mark them as such.  When reading a binary, we need to make
sure we handle instructions that are followed by data and read the data in.

> decodeOp :: Word16 -> Op
> decodeOp = decode . split
>     where
>     split :: Word16 -> (Word16, Word16, Word16)
>     split inst = (inst .&. 0xF, (inst `shiftR` 4) .&. 0x3F, (inst `shiftR` 10) .&. 0x3F)
>     decode :: (Word16, Word16, Word16) -> Op
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
