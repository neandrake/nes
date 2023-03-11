//! Definitions for Processor Status Flags, Addressing Modes, and Instructions.

/// Processor Status Bit: Carry Flag
pub const C: u8 = 0b0000_0001;
/// Processor Status Bit: Zero Flag
pub const Z: u8 = 0b0000_0010;
/// Processor Status Bit: Interrupt Disable
pub const I: u8 = 0b0000_0100;
/// Processor Status Bit: Decimal Mode Flag
pub const D: u8 = 0b0000_1000;
/// Processor Status Bit: Break Command
pub const B: u8 = 0b0001_0000;
/// Processor Status Bit: Overflow Flag
pub const V: u8 = 0b0100_0000;
/// Processor Status Bit: Negative Flag
pub const N: u8 = 0b1000_0000;

/// Addressing Modes
///
/// Indicates how memory locations are addressed.
pub enum AddressingMode {
    /// The instruction implicity indicates source/destination and no arguments
    /// are specified for the instruction.
    Implicit,
    /// The instruction acts on the accumulator register.
    Accumulator,
    /// The instruction has arguments which are directly specified constants.
    Immediate,
    /// Takes an 8bit address operand. This limits the instruction to addressing
    /// only the first 256 bytes of memory where the most significant byte of
    /// the address is always zero. In this mode only the least significant
    /// byte of the address is held in the instruction making it shorter by one
    /// byte.
    ZeroPage,
    /// The address to be accessed by an instruction is calculated by taking
    /// the 8bit address from the operand and adding the current value of the
    /// X register to it. Addition wraps if the value exceeds 0xFF.
    ZeroPageX,
    /// The address to be accessed by an instruction is calculated by taking
    /// the 8bit address from the operand and adding the current value of the
    /// Y register to it. Addition wraps it the value exceeds 0xFF.
    ZeroPageY,
    /// Used by branch instructions which contain an 8bit operand that is added
    /// to the program counter AFTER the program counter has been incremented
    /// for the branch instruction (and operands). Due to this the taret
    /// instruction location must be within -126 to +129 bytes of the branch.
    Relative,
    /// Instructions contain one 16bit operand to identify the target location.
    Absolute,
    /// Instructions contain one 16bit opernd which is added with the X register
    /// to calculate the absolute address to be used.
    AbsoluteX,
    /// Instructions contain one 16bit opernd which is added with the Y register
    /// to calculate the absolute address to be used.
    AbsoluteY,
    /// Only used by the JMP instruction. The instruction has one 16bit operand
    /// which identifies the location of the least significant byte of another
    /// 16bit memory address which is the real target of the instruction.
    Indirect,
    /// Usually used in conjunction with a table of address held on zero page.
    /// The address of the table is taken from the instruction 16bit operand
    /// and the X register added to it (with zero page wrap around) to give
    /// the location of the least signficant byte of the target address.
    IndirectX,
    /// The operand contains the zero page location of the least significant
    /// byte of the 16bit address. The Y register is added to this value
    /// to generate the actual target address for operation.
    IndirectY,
}

/// Instruction Opcode
pub struct Op {
    /// Array of the different addressing modes this instruction operates in.
    /// This array length and order of items matches that of `opcodes`,
    /// `args`, `cycles`, `pg_pen`, and `br_pen`.
    pub addrmodes: &'static [AddressingMode],

    /// The opcodes for this instruction. Each entry corresponds to an opcode
    /// for a different addressing mode. See `addrmodes`.
    pub opcodes:  &'static [u8],

    /// The number of arguments for this instruction. Each entry corresponds
    /// to an opcode for a different addressing mode. See `addrmodes`.
    pub opbytes:  &'static [u8],

    /// The base number of cycles this instruction takes to execute. Each entry
    /// corresponds to an opcode for a different addressing mode. See
    /// `addrmodes`.
    pub cycles:   &'static [u8],

    /// The number of cycles added to the base if a page boundary is crossed.
    /// Each entry corresponds to an opcode for a different addressing mode.
    /// See `addrmodes`.
    pub pg_cyc:   &'static [u8],

    /// The number of cycles added to the base if a branch is executed.
    /// Each entry corresponds to an opcode for a different addressing mode.
    /// See `addrmodes`.
    pub br_cyc:   &'static [u8],

    /// The Processor Status bits that are affected by this instruction.
    pub status: u8,
}

/// All instructions
pub static INSTRUCTIONS: &'static [&'static Op] = &[
    &ADC, &AND, &ASL, &BCC, &BCS, &BEQ, &BIT, &BMI, &BNE, &BPL, &BRK, &BVC, &BVS, &CLC,
    &CLD, &CLI, &CLV, &CMP, &CPX, &CPY, &DEC, &DEX, &DEY, &EOR, &INC, &INX, &INY, &JMP,
    &JSR, &LDA, &LDX, &LDY, &LSR, &NOP, &ORA, &PHA, &PHP, &PLA, &PLP, &ROL, &ROR, &RTI,
    &RTS, &SBC, &SEC, &SED, &SEI, &STA, &STX, &STY, &TAX, &TAY, &TSX, &TXA, &TXS, &TYA,
];


/// Add with Carry
///
/// This instruction adds the contents of a memory location to the accumulator
/// together with the carry bit. If overflow occurs the carry bit is set, this
/// enables multiple byte addition to be performed.
///
/// Processor Status after use:
/// Carry Flag: Set if overflow in bit 7
/// Zero Flag: Set if A = 0
/// Overflow Flag: Set if sign bit is incorrect
/// Negative Flag: Set if bit 7 set
pub static ADC: Op = Op {
    addrmodes: &[
        AddressingMode::Immediate,
        AddressingMode::ZeroPage,
        AddressingMode::ZeroPageX,
        AddressingMode::Absolute,
        AddressingMode::AbsoluteX,
        AddressingMode::AbsoluteY,
        AddressingMode::IndirectX,
        AddressingMode::IndirectY,
    ],
            // Immd,  ZPg, ZPgX,  Abs, AbsX, AbsY, IndX, IndY
    opcodes: &[0x69, 0x65, 0x75, 0x6D, 0x7D, 0x79, 0x61, 0x71],
    opbytes: &[   2,    2,    2,    3,    3,    3,    2,    2],
    cycles:  &[   2,    3,    4,    4,    4,    4,    6,    5],
    pg_cyc:  &[   0,    0,    0,    0,    1,    1,    0,    1],
    br_cyc:  &[   0,    0,    0,    0,    0,    0,    0,    0],
    status: C | Z | V | N,
};

/// Logical AND
///
/// A logical AND is performed, bit by bit, on the accumulator contents using
/// the contents of a byte of memory.
///
/// Processor Status after use:
/// Zero Flag: Set if A = 0
/// Negative Flag: Set if bit 7 set
pub static AND: Op = Op {
    addrmodes: &[
        AddressingMode::Immediate,
        AddressingMode::ZeroPage,
        AddressingMode::ZeroPageX,
        AddressingMode::Absolute,
        AddressingMode::AbsoluteX,
        AddressingMode::AbsoluteY,
        AddressingMode::IndirectX,
        AddressingMode::IndirectY,
    ],
            // Immd,  ZPg, ZpgX,  Abs, AbsX, AbsY, IndX, IndY
    opcodes: &[0x29, 0x25, 0x35, 0x2D, 0x3D, 0x39, 0x21, 0x31],
    opbytes: &[    2,   2,    2,    3,    3,    3,    2,    2],
    cycles:  &[    2,   3,    4,    4,    4,    4,    6,    5],
    pg_cyc:  &[    0,   0,    0,    0,    1,    1,    0,    1],
    br_cyc:  &[    0,   0,    0,    0,    0,    0,    0,    0],
    status: Z | N,
};

/// Arithmetic Shift Left
///
/// This operation shifts all the bits of the accumulator or memory contents
/// one bit left. Bit 0 is set to 0 and bit 7 is placed in the carry flag. The
/// effect of this operation is to multiply the memory contents by 2 (ignoring
/// 2's complement considerations), setting the carry if the result will not
/// fit in 8 bits.
///
/// Processor Status after use:
/// Carry Flag: Set to contents of old bit 7
/// Zero Flag: Set if A = 0
/// Negative Flag: Set if bit 7 of the result is set
pub static ASL: Op = Op {
    addrmodes: &[
        AddressingMode::Accumulator,
        AddressingMode::ZeroPage,
        AddressingMode::ZeroPageX,
        AddressingMode::Absolute,
        AddressingMode::AbsoluteX,
    ],
            // Accm,  ZPg, ZPgX,  Abs, AbsX
    opcodes: &[0x0A, 0x06, 0x16, 0x0E, 0x1E],
    opbytes: &[   1,    2,    2,    3,    3],
    cycles:  &[   2,    5,    6,    6,    7],
    pg_cyc:  &[   0,    0,    0,    0,    0],
    br_cyc:  &[   0,    0,    0,    0,    0],
    status: C | Z | N,
};

/// Branch if Carry Clear
///
/// If the carry flag is clear then add the relative displacement to the
/// program counter to cause a branch to a new location.
///
/// Processor Status after use:
/// [processor status unaffected]
pub static BCC: Op = Op {
    addrmodes: &[
        AddressingMode::Relative,
    ],
            //  Rel
    opcodes: &[0x90],
    opbytes: &[   2],
    cycles:  &[   2],
    pg_cyc:  &[   2],
    br_cyc:  &[   1],
    status: 0,
};

/// Branch if Carry Set
///
/// If the carry flag is set then add the relative displacement to the program
/// counter to cause a branch to a new location.
///
/// Processor Status after use:
/// [processor status unaffected]
pub static BCS: Op = Op {
    addrmodes: &[
        AddressingMode::Relative,
    ],
            //  Rel
    opcodes: &[0xB0],
    opbytes: &[   2],
    cycles:  &[   2],
    pg_cyc:  &[   2],
    br_cyc:  &[   1],
    status: 0,
};

/// Branch if Equal
///
/// If the zero flag is set then add the relative displacement to the program
/// counter to cause a branch to a new location.
///
/// Processor Status after use:
/// [processor status unaffected]
pub static BEQ: Op = Op {
    addrmodes: &[
        AddressingMode::Relative,
    ],
            //  Rel
    opcodes: &[0xF0],
    opbytes: &[   2],
    cycles:  &[   2],
    pg_cyc:  &[   2],
    br_cyc:  &[   1],
    status: 0,
};

/// Bit Test
///
/// This instructions is used to test if one or more bits are set in a target
/// memory location. The mask pattern in A is ANDed with the value in memory
/// to set or clear the zero flag, but the result is not kept. Bits 7 and 6 of
/// the value from memory are copied into the N and V flags.
///
/// Processor Status after use:
/// Zero Flag: Set if the result of the AND is zero
/// Overflow Flag: Set to bit 6 of the memory value
/// Negative Flag: Set to bit 7 of the memory value
pub static BIT: Op = Op {
    addrmodes: &[
        AddressingMode::ZeroPage,
        AddressingMode::Absolute,
    ],
            //  ZPg,  Abs
    opcodes: &[0x24, 0x2C],
    opbytes: &[   2,    3],
    cycles:  &[   3,    4],
    pg_cyc:  &[   0,    0],
    br_cyc:  &[   0,    0],
    status: Z | V | N,
};

/// Branch if Minus
///
/// If the negative flag is set then add the relative displacement to the
/// program counter to cause a branch to a new location.
///
/// Processor Status after use:
/// [processor status unaffected]
pub static BMI: Op = Op {
    addrmodes: &[
        AddressingMode::Relative,
    ],
            //  Rel
    opcodes: &[0x30],
    opbytes: &[   2],
    cycles:  &[   2],
    pg_cyc:  &[   2],
    br_cyc:  &[   1],
    status: 0,
};

/// Branch if Not Equal
///
/// If the zero flag is clear then add the relative displacement to the program
/// counter to cause a branch to a new location.
///
/// Processor Status after use:
/// [processor status unaffected]
pub static BNE: Op = Op {
    addrmodes: &[
        AddressingMode::Relative,
    ],
            //  Rel
    opcodes: &[0xD0],
    opbytes: &[   2],
    cycles:  &[   2],
    pg_cyc:  &[   2],
    br_cyc:  &[   1],
    status: 0,
};

/// Branch if Positive
///
/// If the negative flag is clear then add the relative displacement to the
/// program counter to cause a branch to a new location.
///
/// Processor Status after use:
/// [processor status unaffected]
pub static BPL: Op = Op {
    addrmodes: &[
        AddressingMode::Relative,
    ],
            //  Rel
    opcodes: &[0x10],
    opbytes: &[   2],
    cycles:  &[   2],
    pg_cyc:  &[   2],
    br_cyc:  &[   1],
    status: 0,
};

/// Force Interrupt
///
/// The BRK instruction forces the generation of an interrupt request. The
/// program counter and processor status are pushed on the stack then the IRQ
/// interrupt vector at $FFFE/F is loaded into the PC and the break flag in the
/// status set to one.
///
/// Processor Status after use:
/// Break Command: Set to 1
pub static BRK: Op = Op {
    addrmodes: &[
        AddressingMode::Implicit,
    ],
            // Impl
    opcodes: &[0x00],
    opbytes: &[   1],
    cycles:  &[   7],
    pg_cyc:  &[   0],
    br_cyc:  &[   0],
    status: B,
};

/// Branch if Overflow Clear
///
/// If the overflow flag is clear then add the relative displacement to the
/// program counter to cause a branch to a new location.
///
/// Processor Status after use:
/// [processor status unaffected]
pub static BVC: Op = Op {
    addrmodes: &[
        AddressingMode::Relative,
    ],
            //  Rel
    opcodes: &[0x50],
    opbytes: &[   2],
    cycles:  &[   2],
    pg_cyc:  &[   2],
    br_cyc:  &[   1],
    status: 0,
};

/// Branch if Overflow Set
///
/// If the overflow flag is set then add the relative displacement to the
/// program counter to cause a branch to a new location.
///
/// Processor Status after use:
/// [processor status unaffected]
pub static BVS: Op = Op {
    addrmodes: &[
        AddressingMode::Relative,
    ],
            //  Rel
    opcodes: &[0x70],
    opbytes: &[   2],
    cycles:  &[   2],
    pg_cyc:  &[   2],
    br_cyc:  &[   1],
    status: 0,
};


/// Clear Carry Flag
///
/// Set the carry flag to zero.
///
/// Processor Status after use:
/// Carry Flag: Set to 0
pub static CLC: Op = Op {
    addrmodes: &[
        AddressingMode::Implicit,
    ],
            // Impl
    opcodes: &[0x18],
    opbytes: &[   1],
    cycles:  &[   2],
    pg_cyc:  &[   0],
    br_cyc:  &[   0],
    status: C,
};

/// Clear Decimal Mode
///
/// Sets the decimal mode flag to zero.
///
/// Processor Status after use:
/// Decimal Mode Flag: Set to 0
pub static CLD: Op = Op {
    addrmodes: &[
        AddressingMode::Implicit,
    ],
            // Impl
    opcodes: &[0xD8],
    opbytes: &[   1],
    cycles:  &[   2],
    pg_cyc:  &[   0],
    br_cyc:  &[   0],
    status: D,
};

/// Clear Interrupt Disable
///
/// Clears the interrupt disable flag allowing normal interrupt requests to be
/// serviced.
///
/// Processor Status after use:
/// Interrupt Disable: Set to 0
pub static CLI: Op = Op {
    addrmodes: &[
        AddressingMode::Implicit,
    ],
            // Impl
    opcodes: &[0x58],
    opbytes: &[   1],
    cycles:  &[   2],
    pg_cyc:  &[   0],
    br_cyc:  &[   0],
    status: I,
};

/// Clear Overflow Flag
///
/// Clears the overflow flag.
///
/// Processor Status after use:
/// Overflow Flag: Set to 0
pub static CLV: Op = Op {
    addrmodes: &[
        AddressingMode::Implicit,
    ],
            // Impl
    opcodes: &[0xB8],
    opbytes: &[   1],
    cycles:  &[   2],
    pg_cyc:  &[   0],
    br_cyc:  &[   0],
    status: V,
};

/// Compare
///
/// This instruction compares the contents of the accumulator with another
/// memory held value and sets the zero and carry flags as appropriate.
///
/// Processor Status after use:
/// Carry Flag: Set if A >= M
/// Zero Flag: Set if A = M
/// Negative Flag: Set if bit 7 of the result is set
pub static CMP: Op = Op {
    addrmodes: &[
        AddressingMode::Immediate,
        AddressingMode::ZeroPage,
        AddressingMode::ZeroPageX,
        AddressingMode::Absolute,
        AddressingMode::AbsoluteX,
        AddressingMode::AbsoluteY,
        AddressingMode::IndirectX,
        AddressingMode::IndirectY,
    ],
            // Immd,  ZPg, ZpgX,  Abs, AbsX, AbsY, IndX, IndY
    opcodes: &[0xC9, 0xC5, 0xD5, 0xCD, 0xDD, 0xD9, 0xC1, 0xD1],
    opbytes: &[   2,    2,    2,    3,    3,    3,    2,    2],
    cycles:  &[   2,    3,    4,    4,    4,    4,    6,    5],
    pg_cyc:  &[   0,    0,    0,    0,    1,    1,    0,    1],
    br_cyc:  &[   0,    0,    0,    0,    0,    0,    0,    0],
    status: C | Z | N,
};

/// Comapre X Register
///
/// This instruction compares the contents of the X register with another
/// memory held value and sets the zero and carry flags as appropriate.
///
/// Processor Status after use:
/// Carry Flag: Set if X >= M
/// Zero Flag: Set if X = M
/// Negative Flag: Set if bit 7 of the result is set
pub static CPX: Op = Op {
    addrmodes: &[
        AddressingMode::Immediate,
        AddressingMode::ZeroPage,
        AddressingMode::Absolute,
    ],
            // Immd,  ZPg,  Abs
    opcodes: &[0xE0, 0xE4, 0xEC],
    opbytes: &[   2,    2,    3],
    cycles:  &[   2,    3,    4],
    pg_cyc:  &[   0,    0,    0],
    br_cyc:  &[   0,    0,    0],
    status: C | Z | N,
};

/// Compare Y Register
///
/// This instruction compares the contents of the Y register with another
/// memory held value and sets the zero and carry flags as appropriate.
///
/// Processor Status after use:
/// Carry Flag: Set if Y >= M
/// Zero Flag: Set if Y = M
/// Negative Flag: Set if bit 7 of the result is set
pub static CPY: Op = Op {
    addrmodes: &[
        AddressingMode::Immediate,
        AddressingMode::ZeroPage,
        AddressingMode::Absolute,
    ],
            // Immd,  ZPg,  Abs
    opcodes: &[0xC0, 0xC4, 0xCC],
    opbytes: &[   2,    2,    3],
    cycles:  &[   2,    3,    4],
    pg_cyc:  &[   0,    0,    0],
    br_cyc:  &[   0,    0,    0],
    status: C | Z | N,
};

/// Decrement Memory
///
/// Subtracts one from the value held at a specified memory location setting
/// the zero and negative flags as appropriate.
///
/// Processor Status after use:
/// Zero Flag: Set if result is zero
/// Negative Flag: Set if bit 7 of the result is set
pub static DEC: Op = Op {
    addrmodes: &[
        AddressingMode::ZeroPage,
        AddressingMode::ZeroPageX,
        AddressingMode::Absolute,
        AddressingMode::AbsoluteX,
    ],
            //  ZPg, ZPgX,  Abs, AbsX
    opcodes: &[0xC6, 0xD6, 0xCE, 0xDE],
    opbytes: &[   2,    2,    3,    3],
    cycles:  &[   5,    6,    6,    7],
    pg_cyc:  &[   0,    0,    0,    0],
    br_cyc:  &[   0,    0,    0,    0],
    status: Z | N,
};

/// Decrement X Register
///
/// Subtracts one from the X register setting the zero and negative flags as
/// appropriate.
///
/// Processor Status after use:
/// Zero Flag: Set if X is zero
/// Negative Flag: Set if bit 7 of X is set
pub static DEX: Op = Op {
    addrmodes: &[
        AddressingMode::Implicit,
    ],
            // Impl
    opcodes: &[0xCA],
    opbytes: &[   1],
    cycles:  &[   2],
    pg_cyc:  &[   0],
    br_cyc:  &[   0],
    status: Z | N,
};

/// Decrement Y Register
///
/// Subtracts one from the Y register setting the zero and negative flags as
/// appropriate.
///
/// Processor Status after use:
/// Zero Flag: Set if Y is zero
/// Negative Flag: Set if bit 7 of Y is set
pub static DEY: Op = Op {
    addrmodes: &[
        AddressingMode::Implicit,
    ],
            // Impl
    opcodes: &[0x88],
    opbytes: &[   1],
    cycles:  &[   2],
    pg_cyc:  &[   0],
    br_cyc:  &[   0],
    status: Z | N,
};

/// Exclusive OR
///
/// An exclusive OR is performed, bit by bit, on the accumulator contents using
/// the contents of a byte of memory.
///
/// Processor Status after use:
/// Zero Flag: Set if A is zero
/// Negative Flag: Set if bit 7 is set
pub static EOR: Op = Op {
    addrmodes: &[
        AddressingMode::Immediate,
        AddressingMode::ZeroPage,
        AddressingMode::ZeroPageX,
        AddressingMode::Absolute,
        AddressingMode::AbsoluteX,
        AddressingMode::AbsoluteY,
        AddressingMode::IndirectX,
        AddressingMode::IndirectY,
    ],
            // Immd,  ZPg, ZPgX,  Abs, AbsX, AbsY, IndX, IndY
    opcodes: &[0x40, 0x45, 0x55, 0x4D, 0x5D, 0x59, 0x41, 0x51],
    opbytes: &[   2,    2,    2,    3,    3,    3,    2,    2],
    cycles:  &[   2,    3,    4,    4,    4,    4,    6,    5],
    pg_cyc:  &[   0,    0,    0,    0,    1,    1,    0,    1],
    br_cyc:  &[   0,    0,    0,    0,    0,    0,    0,    0],
    status: Z | N,
};

/// Increment Memory
///
/// Adds one to the value held at a specified memory location setting the zero
/// and negative flags as appropriate.
///
/// Processor Status after use:
/// Zero Flag: Set if result is zero
/// Negative Flag: Set if bit 7 of the result is set
pub static INC: Op = Op {
    addrmodes: &[
        AddressingMode::ZeroPage,
        AddressingMode::ZeroPageX,
        AddressingMode::Absolute,
        AddressingMode::AbsoluteX,
    ],
            //  ZPg, ZPgX,  Abs, AbsX
    opcodes: &[0xE6, 0xF6, 0xEE, 0xFE],
    opbytes: &[   2,    2,    3,    3],
    cycles:  &[   5,    6,    6,    7],
    pg_cyc:  &[   0,    0,    0,    0],
    br_cyc:  &[   0,    0,    0,    0],
    status: Z | N,
};

/// Increment X Register
///
/// Adds one to the X register setting the zero and negative flags as
/// appropriate.
///
/// Processor Status after use:
/// Zero Flag: Set if X is zero
/// Negative Flag: Set if bit 7 of X is set
pub static INX: Op = Op {
    addrmodes: &[
        AddressingMode::Implicit,
    ],
            // Impl
    opcodes: &[0xE8],
    opbytes: &[   1],
    cycles:  &[   2],
    pg_cyc:  &[   0],
    br_cyc:  &[   0],
    status: Z | N,
};

/// Increment Y Register
///
/// Adds one to the Y register setting the zero and negative flags as
/// appropriate.
///
/// Processor Status after use:
/// Zero Flag: Set if Y is zero
/// Negative Flag: Set if bit 7 of Y is set
pub static INY: Op = Op {
    addrmodes: &[
        AddressingMode::Implicit,
    ],
            // Impl
    opcodes: &[0xC8],
    opbytes: &[   1],
    cycles:  &[   2],
    pg_cyc:  &[   0],
    br_cyc:  &[   0],
    status: Z | N,
};

/// Jump
///
/// Sets the program counter to the address specified by the operand.
///
/// Processor Status after use:
/// [processor status is unaffected]
pub static JMP: Op = Op {
    addrmodes: &[
        AddressingMode::Absolute,
        AddressingMode::Indirect,
    ],
            //  Abs,  Ind
    opcodes: &[0x4C, 0x6C],
    opbytes: &[   3,    3],
    cycles:  &[   3,    5],
    pg_cyc:  &[   0,    0],
    br_cyc:  &[   0,    0],
    status: 0,
};

/// Jump to Subroutine
///
/// The JSR instruction pushes the address (minus one) of the return point on
/// to the stack and then sets the program counter to the target memory address.
///
/// Processor Status after use:
/// [processor status is unaffected]
pub static JSR: Op = Op {
    addrmodes: &[
        AddressingMode::Absolute,
    ],
            //  Abs
    opcodes: &[0x20],
    opbytes: &[   3],
    cycles:  &[   6],
    pg_cyc:  &[   0],
    br_cyc:  &[   0],
    status: 0,
};

/// Load Accumulator
///
/// Loads a byte of memory into the accumulator setting the zero and negative
/// flags as appropriate.
///
/// Processor Status after use:
/// Zero Flag: Set if A = 0
/// Negative Flag: Set if bit 7 of A is set
pub static LDA: Op = Op {
    addrmodes: &[
        AddressingMode::Immediate,
        AddressingMode::ZeroPage,
        AddressingMode::ZeroPageX,
        AddressingMode::Absolute,
        AddressingMode::AbsoluteX,
        AddressingMode::AbsoluteY,
        AddressingMode::IndirectX,
        AddressingMode::IndirectY,
    ],
            // Immd,  ZPg, ZPgX,  Abs, AbsX, AbsY, IndX, IndY
    opcodes: &[0xA9, 0xA5, 0xB5, 0xAD, 0xBD, 0xB9, 0xA1, 0xB1],
    opbytes: &[   2,    2,    2,    3,    3,    3,    2,    2],
    cycles:  &[   2,    3,    4,    4,    4,    4,    6,    5],
    pg_cyc:  &[   0,    0,    0,    0,    1,    1,    0,    1],
    br_cyc:  &[   0,    0,    0,    0,    0,    0,    0,    0],
    status: Z | N,
};

/// Load X Register
///
/// Loads a byte of memory into the X register setting the zero and negative
/// flags as appropriate.
///
/// Processor Status after use:
/// Zero Flag: Set if X = 0
/// Negative Flag: Set if bit 7 of X is set
pub static LDX: Op = Op {
    addrmodes: &[
        AddressingMode::Immediate,
        AddressingMode::ZeroPage,
        AddressingMode::ZeroPageY,
        AddressingMode::Absolute,
        AddressingMode::AbsoluteY,
    ],
            // Immd,  ZPg, ZPgY,  Abs, AbsY
    opcodes: &[0xA2, 0xA6, 0xB6, 0xAE, 0xBE],
    opbytes: &[   2,    2,    2,    3,    3],
    cycles:  &[   2,    3,    4,    4,    4],
    pg_cyc:  &[   0,    0,    0,    0,    1],
    br_cyc:  &[   0,    0,    0,    0,    0],
    status: Z | N,
};

/// Load Y Register
///
/// Loads a byte of memory into the Y register setting the zero and negative
/// flags as appropriate.
///
/// Processor Status after use:
/// Zero Flag: Set if Y = 0
/// Negative Flag: Set if bit 7 of Y is set
pub static LDY: Op = Op {
    addrmodes: &[
        AddressingMode::Immediate,
        AddressingMode::ZeroPage,
        AddressingMode::ZeroPageX,
        AddressingMode::Absolute,
        AddressingMode::AbsoluteX,
    ],
            // Immd,  ZPg, ZPgX,  Abs, AbsX
    opcodes: &[0xA0, 0xA4, 0xB4, 0xAC, 0xBC],
    opbytes: &[   2,    2,    2,    3,    3],
    cycles:  &[   2,    3,    4,    4,    4],
    pg_cyc:  &[   0,    0,    0,    0,    1],
    br_cyc:  &[   0,    0,    0,    0,    0],
    status: Z | N,
};

/// Logical Shift Right
///
/// Each of the bits in A or M is shift one place to the right. The bit that
/// was in bit 0 is shifted into the carry flag. Bit 7 is set to zero.
///
/// Processor Status after use:
/// Carry Flag: Set to contents of old bit 0
/// Zero Flag: Set if result = 0
/// Negative Flag: Set if bit 7 of the result is set
pub static LSR: Op = Op {
    addrmodes: &[
        AddressingMode::Accumulator,
        AddressingMode::ZeroPage,
        AddressingMode::ZeroPageX,
        AddressingMode::Absolute,
        AddressingMode::AbsoluteX,
    ],
            //  Acc,  ZPg, ZPgX,  Abs, AbsX
    opcodes: &[0x4A, 0x46, 0x56, 0x4E, 0x5E],
    opbytes: &[   1,    2,    2,    3,    3],
    cycles:  &[   2,    5,    6,    6,    7],
    pg_cyc:  &[   0,    0,    0,    0,    0],
    br_cyc:  &[   0,    0,    0,    0,    0],
    status: C | Z | N,
};

/// No Operation
///
/// The NOP instruction causes no changes to the processor other than the
/// normal incrementing of the program counter to the next instruction.
///
/// Processor Status after use:
/// [processer status unaffected]
pub static NOP: Op = Op {
    addrmodes: &[
        AddressingMode::Implicit,
    ],
            // Impl
    opcodes: &[0xEA],
    opbytes: &[   1],
    cycles:  &[   2],
    pg_cyc:  &[   0],
    br_cyc:  &[   0],
    status: 0,
};

/// Logical Inclusive OR
///
/// An inclusive OR is performed, bit by bit, on the accumulator contents using
/// the contents of a byte of memory.
///
/// Processor Status after use:
/// Zero Flag: Set if A = 0,
/// Negative Flag: Set if bit 7 set
pub static ORA: Op = Op {
    addrmodes: &[
        AddressingMode::Immediate,
        AddressingMode::ZeroPage,
        AddressingMode::ZeroPageX,
        AddressingMode::Absolute,
        AddressingMode::AbsoluteX,
        AddressingMode::AbsoluteY,
        AddressingMode::IndirectX,
        AddressingMode::IndirectY,
    ],
            // Immd,  ZPg, ZPgX,  Abs, AbsX, AbsY, IndX, IndY
    opcodes: &[0x09, 0x05, 0x15, 0x0D, 0x1D, 0x19, 0x01, 0x11],
    opbytes: &[   2,    2,    2,    3,    3,    3,    2,    2],
    cycles:  &[   2,    3,    4,    4,    4,    4,    6,    5],
    pg_cyc:  &[   0,    0,    0,    0,    1,    1,    0,    1],
    br_cyc:  &[   0,    0,    0,    0,    0,    0,    0,    0],
    status: Z | N,
};

/// Push Accumulator
///
/// Pushes a copy of the accumulator on to the stack.
///
/// Processor Status after use:
/// [processor status unaffected]
pub static PHA: Op = Op {
    addrmodes: &[
        AddressingMode::Implicit,
    ],
            // Impl
    opcodes: &[0x48],
    opbytes: &[   1],
    cycles:  &[   3],
    pg_cyc:  &[   0],
    br_cyc:  &[   0],
    status: 0,
};

/// Push Processor Status
///
/// Pushes a copy of the status flags on to the stack.
///
/// Processor Status after use:
/// [processor status unaffected]
pub static PHP: Op = Op {
    addrmodes: &[
        AddressingMode::Implicit,
    ],
            // Impl
    opcodes: &[0x08],
    opbytes: &[   1],
    cycles:  &[   3],
    pg_cyc:  &[   0],
    br_cyc:  &[   0],
    status: 0,
};

/// Pull Accumulator
///
/// Pulls an 8 bit value from the stack and into the accumulator. The zero and
/// negative flags are set as appropriate.
///
/// Processor Status after use:
/// Zero Flag: Set if A = 0,
/// Negative Flag: Set if bit 7 of A is set
pub static PLA: Op = Op {
    addrmodes: &[
        AddressingMode::Implicit,
    ],
            // Impl
    opcodes: &[0x68],
    opbytes: &[   1],
    cycles:  &[   4],
    pg_cyc:  &[   0],
    br_cyc:  &[   0],
    status: Z | N,
};

/// Pull Processor Status
///
/// Pulls an 8 bit value from the stack and into the processor flags. The flags
/// will take on new states as determined by the value pulled.
///
/// Processor Status after use:
/// Carry Flag: Set from stack
/// Zero Flag: Set from stack
/// Interrupt Disable: Set from stack
/// Decimal Mode Flag: Set from stack
/// Break Command: Set from stack
/// Overflow Flag: Set from stack
/// Negative Flag: Set from stack
pub static PLP: Op = Op {
    addrmodes: &[
        AddressingMode::Implicit,
    ],
            // Impl
    opcodes: &[0x28],
    opbytes: &[   1],
    cycles:  &[   4],
    pg_cyc:  &[   0],
    br_cyc:  &[   0],
    status: C | Z | I | D | B | V | N,
};

/// Rotate Left
///
/// Move each of the bits in either A or M one place to the left. Bit 0 is
/// filled with the current value of the carry flag whilst the old bit 7
/// becomes the new carry flag value.
///
/// Processor Status after use:
/// Carry Flag: Set to contents of old bit 7
/// Zero Flag: Set if A = 0
/// Negative Flag: Set if bit 7 of the result is set
pub static ROL: Op = Op {
    addrmodes: &[
        AddressingMode::Accumulator,
        AddressingMode::ZeroPage,
        AddressingMode::ZeroPageX,
        AddressingMode::Absolute,
        AddressingMode::AbsoluteX,
    ],
            // Accm,  ZPg, ZPgX,  Abs, AbsX
    opcodes: &[0x2A, 0x26, 0x36, 0x2E, 0x3E],
    opbytes: &[   1,    2,    2,    3,    3],
    cycles:  &[   2,    5,    6,    6,    7],
    pg_cyc:  &[   0,    0,    0,    0,    0],
    br_cyc:  &[   0,    0,    0,    0,    0],
    status: C | Z | N,
};

/// Rotate Right
///
/// Move each of the bits in either A or M one place to the right. Bit 7 is
/// filled with the current value of the carry flag whilst the old bit 0
/// becomes the new carry flag value.
///
/// Processor Status after use:
/// Carry Flag: Set to contents of old bit 0
/// Zero Flag: Set if A = 0
/// Negative Flag: Set if bit 7 of the result is set
pub static ROR: Op = Op {
    addrmodes: &[
        AddressingMode::Accumulator,
        AddressingMode::ZeroPage,
        AddressingMode::ZeroPageX,
        AddressingMode::Absolute,
        AddressingMode::AbsoluteX,
    ],
            // Accm,  ZPg, ZPgX,  Abs, AbsX
    opcodes: &[0x6A, 0x66, 0x76, 0x6E, 0x7E],
    opbytes: &[   1,    2,    2,    3,    3],
    cycles:  &[   2,    5,    6,    6,    7],
    pg_cyc:  &[   0,    0,    0,    0,    0],
    br_cyc:  &[   0,    0,    0,    0,    0],
    status: C | Z | N,
};

/// Return from Interrupt
///
/// The RTI instruction is used at the end of an interrupt processing routine.
/// It pulls the processor flags from the stack followed by the program counter.
///
/// Processor Status after use:
/// Carry Flag: Set from stack
/// Zero Flag: Set from stack
/// Interrupt Disable: Set from stack
/// Decimal Mode Flag: Set from stack
/// Break Comand: Set from stack
/// Overflow Flag: Set from stack
/// Negative Flag: Set from stack
pub static RTI: Op = Op {
    addrmodes: &[
        AddressingMode::Implicit,
    ],
            // Impl
    opcodes: &[0x40],
    opbytes: &[   1],
    cycles:  &[   6],
    pg_cyc:  &[   0],
    br_cyc:  &[   0],
    status: C | Z | I | D | B | V | N,
};

/// Return from Subroutine
///
/// The RTS instruction is used at the end of a subroutine to return to the
/// calling routine. It pulls the program counter (minus one) from the stack.
///
/// Processor Status after use:
/// [processor status unaffected]
pub static RTS: Op = Op {
    addrmodes: &[
        AddressingMode::Implicit,
    ],
            // Impl
    opcodes: &[0x60],
    opbytes: &[   1],
    cycles:  &[   6],
    pg_cyc:  &[   0],
    br_cyc:  &[   0],
    status: 0,
};

/// Subtract with Carry
///
/// This instruction subtracts the contents of a memory location to the
/// accumulator together with the not of the carry bit. If overflow occurs the
/// carry bit is clear, this enables multiple byte subtraction to be performed.
///
/// Processor Status after use:
/// Carry Flag: Claer if overflow in bit 7
/// Zero Flag: Set if A = 0
/// Overflow Flag: Set if sign bit is incorrect
/// Negative Flag: Set if bit 7 set
pub static SBC: Op = Op {
    addrmodes: &[
        AddressingMode::Immediate,
        AddressingMode::ZeroPage,
        AddressingMode::ZeroPageX,
        AddressingMode::Absolute,
        AddressingMode::AbsoluteX,
        AddressingMode::AbsoluteY,
        AddressingMode::IndirectX,
        AddressingMode::IndirectY,
    ],
            // Immd,  ZPg, ZPgX,  Abs, AbsX, AbsY, IndX, IndY
    opcodes: &[0xE9, 0xE5, 0xF5, 0xED, 0xFD, 0xF9, 0xE1, 0xF1],
    opbytes: &[   2,    2,    2,    3,    3,    3,    2,    2],
    cycles:  &[   2,    3,    4,    4,    4,    4,    6,    5],
    pg_cyc:  &[   0,    0,    0,    0,    1,    1,    0,    1],
    br_cyc:  &[   0,    0,    0,    0,    0,    0,    0,    0],
    status: C | Z | V | N,
};

/// Set Carry Flag
///
/// Set the carry flag to one.
///
/// Processor Status after use:
/// Carry Flag: Set to 1
pub static SEC: Op = Op {
    addrmodes: &[
        AddressingMode::Implicit,
    ],
            // Impl
    opcodes: &[0x3B],
    opbytes: &[   1],
    cycles:  &[   2],
    pg_cyc:  &[   0],
    br_cyc:  &[   0],
    status: C,
};

/// Set Decimal Flag
///
/// Set the decimal mode flag to one.
///
/// Processor Status after use:
/// Decimal Mode Flag: Set to 1
pub static SED: Op = Op {
    addrmodes: &[
        AddressingMode::Implicit,
    ],
            // Impl
    opcodes: &[0xF8],
    opbytes: &[   1],
    cycles:  &[   2],
    pg_cyc:  &[   0],
    br_cyc:  &[   0],
    status: D,
};

/// Set Interrupt Disable
///
/// Set the interrupt disable flag to one.
///
/// Processor Status after use:
/// Interrupt Disable: Set to 1
pub static SEI: Op = Op {
    addrmodes: &[
        AddressingMode::Implicit,
    ],
            // Impl
    opcodes: &[0x78],
    opbytes: &[   1],
    cycles:  &[   2],
    pg_cyc:  &[   0],
    br_cyc:  &[   0],
    status: D,
};

/// Store Accumulator
///
/// Stores the contents of the accumulator into memory.
///
/// Processor Status after use:
/// [processor status unaffecte]
pub static STA: Op = Op {
    addrmodes: &[
        AddressingMode::ZeroPage,
        AddressingMode::ZeroPageX,
        AddressingMode::Absolute,
        AddressingMode::AbsoluteX,
        AddressingMode::AbsoluteY,
        AddressingMode::IndirectX,
        AddressingMode::IndirectY,
    ],
            //  ZPg, ZPgX,  Abs, AbsX, AbsY, IndX, IndY
    opcodes: &[0x85, 0x95, 0x8D, 0x9D, 0x99, 0x81, 0x91],
    opbytes: &[   2,    2,    3,    3,    3,    2,    2],
    cycles:  &[   3,    4,    4,    5,    5,    6,    6],
    pg_cyc:  &[   0,    0,    0,    0,    0,    0,    0],
    br_cyc:  &[   0,    0,    0,    0,    0,    0,    0],
    status: 0,
};

/// Store X Register
///
/// Stores the contents of the X register into memory.
///
/// Processor Status after use:
/// [processor status unaffected]
pub static STX: Op = Op {
    addrmodes: &[
        AddressingMode::ZeroPage,
        AddressingMode::ZeroPageY,
        AddressingMode::Absolute,
    ],
            //  ZPg, ZPgY,  Abs
    opcodes: &[0x86, 0x96, 0x8E],
    opbytes: &[   2,    2,    3],
    cycles:  &[   3,    4,    4],
    pg_cyc:  &[   0,    0,    0],
    br_cyc:  &[   0,    0,    0],
    status: 0,
};

/// Store Y Register
///
/// Stores the contents of the Y register into memory.
///
/// Processor Status after use:
/// [processor status unaffected]
pub static STY: Op = Op {
    addrmodes: &[
        AddressingMode::ZeroPage,
        AddressingMode::ZeroPageX,
        AddressingMode::Absolute,
    ],
            //  ZPg, ZPgX,  Abs
    opcodes: &[0x84, 0x94, 0x8C],
    opbytes: &[   2,    2,    3],
    cycles:  &[   3,    4,    4],
    pg_cyc:  &[   0,    0,    0],
    br_cyc:  &[   0,    0,    0],
    status: 0,
};

/// Transfer Accumulator to X
///
/// Copies the current contents of the accumulator into the X register and set
/// the zero and negative flags as appropriate.
///
/// Processor Status after use:
/// Zero Flag: Set if X = 0
/// Negative Flag: Set if bit 7 of X is set
pub static TAX: Op = Op {
    addrmodes: &[
        AddressingMode::Implicit,
    ],
            // Impl
    opcodes: &[0xAA],
    opbytes: &[   1],
    cycles:  &[   2],
    pg_cyc:  &[   0],
    br_cyc:  &[   0],
    status: Z | N,
};

/// Transfer Accumulator to Y
///
/// Copies the current contents of the accumulator into the Y register and set
/// the zero and negative flags as appropriate.
///
/// Processor Status after use:
/// Zero Flag: Set if Y = 0
/// Negative Flag: Set if bit 7 of Y is set
pub static TAY: Op = Op {
    addrmodes: &[
        AddressingMode::Implicit,
    ],
            // Impl
    opcodes: &[0xA8],
    opbytes: &[   1],
    cycles:  &[   2],
    pg_cyc:  &[   0],
    br_cyc:  &[   0],
    status: Z | N,
};

/// Transfer Stack Pointer to X
///
/// Copies the current contents of the stack register into the X register and
/// sets the zero and negative flags as appropriate.
///
/// Processor Status after use:
/// Zero Flag: Set if X = 0
/// Negative Flag: Set if bit 7 of X is set
pub static TSX: Op = Op {
    addrmodes: &[
        AddressingMode::Implicit,
    ],
            // Impl
    opcodes: &[0xBA],
    opbytes: &[   1],
    cycles:  &[   2],
    pg_cyc:  &[   0],
    br_cyc:  &[   0],
    status: Z | N,
};

/// Transfer X to Accumulator
///
/// Copies the current contents of the X register into the accumulator and sets
/// the zero and negative flags as appropriate.
///
/// Processor Status after use:
/// Zero Flag: Set if A = 0
/// Negative Flag: Set if bit 7 of A is set
pub static TXA: Op = Op {
    addrmodes: &[
        AddressingMode::Implicit,
    ],
            // Impl
    opcodes: &[0x8A],
    opbytes: &[   1],
    cycles:  &[   2],
    pg_cyc:  &[   0],
    br_cyc:  &[   0],
    status: Z | N,
};

/// Transfer X to Stack Pointer
///
/// Copies the current contents of the X register into the stack register.
///
/// Processor Status after use:
/// [processor status unaffected]
pub static TXS: Op = Op {
    addrmodes: &[
        AddressingMode::Implicit,
    ],
            // Impl
    opcodes: &[0x9A],
    opbytes: &[   1],
    cycles:  &[   2],
    pg_cyc:  &[   0],
    br_cyc:  &[   0],
    status: 0,
};


/// Transfer Y to Accumulator
///
/// Copies the current contents of the Y register into the accumulator and sets
/// the zero and negative flags as appropriate.
///
/// Processor Status after use:
/// Zero Flag: Set if A = 0
/// Negative Flag: Set if bit 7 of A is set
pub static TYA: Op = Op {
    addrmodes: &[
        AddressingMode::Implicit,
    ],
            // Impl
    opcodes: &[0x98],
    opbytes: &[   1],
    cycles:  &[   2],
    pg_cyc:  &[   0],
    br_cyc:  &[   0],
    status: Z | N,
};

