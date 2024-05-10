## Project goal: emulate a CPU at the processor level

This project emulates the [6502](http://archive.6502.org/datasheets/rockwell_r650x_r651x.pdf) processor.

A CPU requires 4 things:
1. output to an address
2. input/output data
3. define whether output is read/write
4. take a clock as input to change state

In particular, the 6502 is capable of outputting a 16-bit address, with data being read/write 8 bits at a time. 

On its own, a CPU is not capable of doing much. We need to connect it to a *bus* (through address/data lines). This
bus spans the entire length of the available addresses, from `0x0000` to `0xFFFF`
That means we need three additional devices connected to the bus:
1. One device that only writes data to the bus (lives from `0x0000` to `0x7FFF`)
2. One device that can both read/write to/from the bus (lives from `0x8000` to `0xBFFF`)
3. One device that only reads data from the bus (lives from `0xC000` to `0xEFFF`)
4. There will be some invalid data past `0xF000`

For this project, for simplicity, there is one bus, RAM, capable of both reading and writing, covering the full
range of the bus from `0x0000` to `0xFFFF`.
* The RAM has a total size of 64kb
* It has a traditional Von-Neumann architecture, holding both variables and programs.

There are three primary registers (all 8-bit) in a 6502 CPU:
1. `A`: accumulator
2. `X`: register
3. `Y`: register
There are additional registers:
4. `STKP`: stack pointer (stack address pointer). This is an 8-bit register
5. `PC`: program counter (program bytes). This is a 16-bit register
6. `STATUS`: status register (Z? C? I?). For convenience, this is packaged as an 8-bit word

Instructions can vary in size (1, 2, or 3 bytes). This means that the CPU may need several clock cycles to handle just
one instruction. Therefore, the emulated CPU handles both size and duration, depending on the type of instruction. A
6502 has 56 different *legal* instructions, but these legal instructions can be *mutated* to change the size and duration,
depending on the instruction's argument. Illegal opcodes will not be emulated here.
The first argument of an instruction provides this information.

A sample instruction. Load the accumulator with the numeric decimal value 65. This is a 2-byte instruction, one byte
for the argument and one for the immediate data:
```asm
LDA $41
```
Another instruction, loading the accumulator with the absolute value of an address. This is a 3-byte instruction, one
for the argument and *two* for the memory address:
```asm
LDA $0105
```
One more instruction, clear the carry bit in the STATUS register, takes only one byte:
```asm
CLC
```

For any given instruction, we emulate:
1. The function itself
2. The addressing mode
3. The number of cycles needed to complete

From the 6502 datasheet, we can actually get all necessary information from the first byte. Instructions are mutated
by the *addressing mode* (e.g. accumulator addressing, immediate address, zero page addressing, etc.). These instruction
set op codes can conveniently be represented in a 16x16 matrix (256 potential instructions).

To do this, we:
1. Read a byte at the `PC`
2. `OPCODE[byte]` will give us the addressing mode and the number of cycles
3. Read any additional bytes needed to complete the instruction (0, 1, 2 or more bytes)
4. Execute the instruction
5. Wait and count clock cycles until the instruction is officially complete