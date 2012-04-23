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
> import Data.Binary.Put (putWord16be, Put, runPut)
> import Data.ByteString.Lazy (ByteString)
> import Data.Bits ((.|.), (.&.), shiftL)

A register identifier is an integer from 0 to 15.

> type Register = Int

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
>         | Data Int                -- a raw 16-bit integer
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

> data Operand = Lit Int            -- 00xxxx: short literals (-8 to +7)
>              | Reg Register       -- 01xxxx: value in register #a
>              | Frame Register     -- 10xxxx: memory location FP + #a
>              | Addr Register      -- 11xxxx: memory location #a
>              deriving (Show)

Define the binary opcodes:

> opcode :: Op -> Int
> opcode (EXT e)   = shiftL (extopcode e) 4
> opcode (SET _ _) = 0x1
> opcode (ADD _ _) = 0x2
> opcode (SUB _ _) = 0x3
> opcode (MUL _ _) = 0x4
> opcode (SHL _ _) = 0x5
> opcode (SHR _ _) = 0x6
> opcode (AND _ _) = 0x7
> opcode (BOR _ _) = 0x8
> opcode (XOR _ _) = 0x9

... and also for extended opcodes:

> extopcode :: ExtOp -> Int
> extopcode (LOAD _) = 0x01
> extopcode (PUSH _) = 0x02
> extopcode (POP _)  = 0x03
> extopcode (PEEK _) = 0x04

Define the binary representations of operands:

> operand :: Operand -> Int
> operand (Lit a)   = a .&. 0xF
> operand (Reg r)   = 0x10 .|. (r .&. 0xF)
> operand (Frame r) = 0x20 .|. (r .&. 0xF)
> operand (Addr r)  = 0x30 .|. (r .&. 0xF)

Define the binary representation of operations:

> op2 :: Int -> Operand -> Operand -> Int
> op2 opcode a b = opcode .|. (shiftL (operand a) 4) .|. (shiftL (operand b) 10)
> op1 :: Int -> Operand -> Int
> op1 opcode a = opcode .|. (shiftL (operand a) 10)
> op :: Op -> Int
> op o@(SET a b)      = op2 (opcode o) a b
> op o@(ADD a b)      = op2 (opcode o) a b
> op o@(SUB a b)      = op2 (opcode o) a b
> op o@(MUL a b)      = op2 (opcode o) a b
> op o@(SHL a b)      = op2 (opcode o) a b
> op o@(SHR a b)      = op2 (opcode o) a b
> op o@(AND a b)      = op2 (opcode o) a b
> op o@(BOR a b)      = op2 (opcode o) a b
> op o@(XOR a b)      = op2 (opcode o) a b
> op (Data a)         = a .&. 0xFF
> op o@(EXT (LOAD a)) = op1 (opcode o) a
> op o@(EXT (PUSH a)) = op1 (opcode o) a
> op o@(EXT (POP a))  = op1 (opcode o) a
> op o@(EXT (PEEK a)) = op1 (opcode o) a
