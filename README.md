# Tiny RISC-V
Pipeline processor (32 bits) in riscv architecture, made in VHDL and running in the GHDL 0.33 environment. Inspired in the processor LEGv8.

- Instruction memory are not included in VHD files, it is inserted as rom.dat in testbench(Processador_pipeline_tb.vhd)
- rom.dat (Instruction Memory) have 256 KiB with a simple program writed.
- Data memory is already implemented in the processor.

## Tiny RISC-V Instruction Set

Core Instruction Formats



![insr](https://github.com/GustavsC/TRisc-V/assets/59322464/68360952-cde2-4a63-bcaa-29362287d9ac)


Instruction Set with their respective name, formats, opcodes, funct3, funct7 and description:

| Instruction  | Name | Format | Opcode | funct3 | funct 7 | Description| 
| ------------- | ------------- | ------------- | ------------- | ------------- | ------------- | ------------- | 
| ADD  | ADD  | R | 0110011 |0x0| 0x00 | rd = rs1 + rs2
| SUB  | SUB  | R |0110011|0x0| 0x20 | rd = rs1 - rs2
| OR   | OR   | R |0110011|0x6| 0x00 | rd = rs1 OR rs2
| AND  | AND  | R |0110011|0x7| 0x00 | rd = rs1 AND rs2
| SLL  | Shift Left Logical | R |0110011|0x1| 0x00 | rd = rs1 << rs2
| SRL  | Shift Right Logical | R |0110011|0x5| 0x00 | rd = rs1 >> rs2
| SRA  | Shift Right Arith | R |0110011|0x5| 0x20 | rd = rs1 >> rs2
| SLT  | Set Less Then  | R | 0110011 |0x2| 0x00 | rd = (rs1 < rs2)?1:0
| MUL  | Multiplication  | R | 0110011 |0x0| 0x01 | rd = rs1 * rs2[31:0]
| ADDI | ADD Immediate | I |0010011| 0x0 | - | rd = rs1 + imm
| ORI  | OR Immediate  | I |0010011| 0x6 | - | rd = rs1 OR imm
| ANDI | AND Immediate | I |0010011| 0x7 | - | rd = rs1 AND imm
| SLLI | Shift Left Logical Immediate | I |0010011| 0x1 | imm[5:11]=0x00 | rd = rs1 << imm[0:4]
| SRLI | Shift Right Logical Immediate | I |0010011| 0x5 | imm[5:11]=0x00 | rd = rs1 >> imm[0:4]
| SRAI | Shift Right Arith Immediate| I |0010011| 0x5 | imm[5:11]=0x20 | rd = rs1 >> imm[0:4]
| SLTI | Set Less Than Imm | I |0010011| 0x2 | - | rd = (rs1 < imm)?1:0
| LW | Load Word | I |0000011| 0x2 | - | rd = M[rs1+imm][0:31]
| SW | Store Word | S |0100011| 0x2 | - | M[rs1+imm][0:7] = rs2[0:31]
| BEQ | Branch Equal | B |1100011| 0x0 | - |if(rs1 == rs2) PC += imm
| BNE | Branch Not Equal | B |1100011| 0x1 | - | if(rs1 != rs2) PC += imm
| BLT | Branch Less Than | B |1100011| 0x4 | - | if(rs1 < rs2) PC += imm
| JAL | Jump and Link | J |1101111| - | - | rd = PC+4; PC += imm
| JALR | Jump and Link Reg | I |1100111| 0x0 | - | rd = PC+4; PC = rs1 + imm

## Processor Datapath and Unit Control flow

![Data](https://github.com/GustavsC/TRisc-V/assets/59322464/8dbe7989-f8bf-48a5-b7a8-c94ac7c26856)
Datapath retired from: Figure 4.65 - Computer Organization and Design by David A Patterson and John L Hennessy


Signals from the Control Unit and their respective effects within the data flow

![Control](https://github.com/GustavsC/TRisc-V/assets/59322464/126f21f5-369d-4a0e-ba5a-6296152780b3)
Table retired from: Figure 4.48 - Computer Organization and Design by David A Patterson and John L Hennessy

