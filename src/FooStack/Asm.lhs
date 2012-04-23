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

A register identifier is an integer from 0 to 15.

> type Register = Int

Basic opcodes are 4 bits, and take 2 6-bit operands.  Opcode 0x0 signifies an
extended opcode (see ExtOpcode).

> data Op = EXT ExtOp               -- 0x0, extended instruction
>         | SET Operand Operand     -- 0x1, set a = b
>         | ADD Operand Operand     -- 0x2, set a = a + b, sets carry flag
>         | SUB Operand Operand     -- 0x3, set a = a - b, sets carry flag
>         | MUL Operand Operand     -- 0x4, set a = a * b, sets carry flag
>         deriving (Show)

To achieve a larger range of available opcodes, the "extended" opcodes are
signified by a "basic" opcode of 0x0.

> data ExtOp = ExtReserved          -- Reserved for extra-extended opcodes
>            | LOAD Operand         -- set a = next word
>            | PUSH Operand         -- set [SP++] = a
>            | POP Operand          -- set a = [--SP]
>            | PEEK Operand         -- set a = [SP]

Operands need to provide several different methods for playing with data.  We
have 6 bits to play with.  If we want a reasonable number of registers (16),
that leaves us 2 bits for addressing mode.

> data Operand = Lit Int            -- 00xxxx: short literals (-8 to +7)
>              | Reg Register       -- 01xxxx: value in register @a
>              | Frame Register     -- 10xxxx: memory location FP + @a
>              | Addr Register      -- 11xxxx: memory location @a
