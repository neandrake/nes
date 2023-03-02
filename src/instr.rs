
pub const C: u8 = 0b0000_0001;
pub const Z: u8 = 0b0000_0010;
pub const I: u8 = 0b0000_0100;
pub const D: u8 = 0b0000_1000;
pub const B: u8 = 0b0001_0000;
pub const V: u8 = 0b0100_0000;
pub const N: u8 = 0b1000_0000;

pub enum AddressingMode {
    Implicit,
    Accumulator,
    Immediate,
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
    Relative,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    Indirect,
    IndirectX,
    IndirectY,
}

/// Instruction Opcode
pub struct Op {
    pub addrmodes: &'static [AddressingMode],
    pub opcodes:  &'static [u8],
    pub args:     &'static [u8],
    pub cycles:   &'static [u8],
    pub pg_pen:   &'static [u8],
    pub br_pen:   &'static [u8],
    pub status: u8,
}

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
    args:    &[   1,    1,    1,    2,    2,    2,    1,    1],
    cycles:  &[   2,    3,    4,    4,    4,    4,    6,    5],
    pg_pen:  &[   0,    0,    0,    0,    1,    1,    0,    1],
    br_pen:  &[   0,    0,    0,    0,    0,    0,    0,    0],
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
    args:    &[    1,   1,    1,    2,    2,    2,    1,    1],
    cycles:  &[    2,   3,    4,    4,    4,    4,    6,    5],
    pg_pen:  &[    0,   0,    0,    0,    1,    1,    0,    1],
    br_pen:  &[    0,   0,    0,    0,    0,    0,    0,    0],
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
    args:    &[   0,    1,    1,    2,    2],
    cycles:  &[   2,    5,    6,    6,    7],
    pg_pen:  &[   0,    0,    0,    0,    0],
    br_pen:  &[   0,    0,    0,    0,    0],
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
    args:    &[   1],
    cycles:  &[   2],
    pg_pen:  &[   2],
    br_pen:  &[   1],
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
    args:    &[   1],
    cycles:  &[   2],
    pg_pen:  &[   2],
    br_pen:  &[   1],
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
    args:    &[   1],
    cycles:  &[   2],
    pg_pen:  &[   2],
    br_pen:  &[   1],
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
    args:    &[   1,    2],
    cycles:  &[   3,    4],
    pg_pen:  &[   0,    0],
    br_pen:  &[   0,    0],
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
    args:    &[   1],
    cycles:  &[   2],
    pg_pen:  &[   2],
    br_pen:  &[   1],
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
    args:    &[   1],
    cycles:  &[   2],
    pg_pen:  &[   2],
    br_pen:  &[   1],
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
    args:    &[   1],
    cycles:  &[   2],
    pg_pen:  &[   2],
    br_pen:  &[   1],
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
    args:    &[   0],
    cycles:  &[   7],
    pg_pen:  &[   0],
    br_pen:  &[   0],
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
    args:    &[   1],
    cycles:  &[   2],
    pg_pen:  &[   2],
    br_pen:  &[   1],
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
    args:    &[   1],
    cycles:  &[   2],
    pg_pen:  &[   2],
    br_pen:  &[   1],
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
    args:    &[   0],
    cycles:  &[   2],
    pg_pen:  &[   0],
    br_pen:  &[   0],
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
    args:    &[   0],
    cycles:  &[   2],
    pg_pen:  &[   0],
    br_pen:  &[   0],
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
    args:    &[   0],
    cycles:  &[   2],
    pg_pen:  &[   0],
    br_pen:  &[   0],
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

    opcodes: &[0xB8],
    args:    &[   0],
    cycles:  &[   2],
    pg_pen:  &[   0],
    br_pen:  &[   0],
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
    args:    &[   1,    1,    1,    2,    2,    2,    1,    1],
    cycles:  &[   2,    3,    4,    4,    4,    4,    6,    5],
    pg_pen:  &[   0,    0,    0,    0,    1,    1,    0,    1],
    br_pen:  &[   0,    0,    0,    0,    0,    0,    0,    0],
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
    args:    &[   1,    1,    2],
    cycles:  &[   2,    3,    4],
    pg_pen:  &[   0,    0,    0],
    br_pen:  &[   0,    0,    0],
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
    args:    &[   1,    1,    2],
    cycles:  &[   2,    3,    4],
    pg_pen:  &[   0,    0,    0],
    br_pen:  &[   0,    0,    0],
    status: C | Z | N,
};

