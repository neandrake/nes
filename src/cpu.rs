
pub trait Mem {
    fn write_u8(&mut self, addr: u16, value: u8);

    fn read_u8(&self, addr: u16) -> u8;

    fn write_u16(&mut self, addr: u16, value: u16);

    fn read_u16(&self, addr: u16) -> u16;
}

pub struct CPU {
    pc: u16,
    sp: u8,
    ac: u8,
    rx: u8,
    ry: u8,
    ps: u8,

    ram: [u8; 0xFFFF],
}

impl CPU {
    pub fn execute(&mut self, op: u16) -> bool {
        match op {
            0xA9 => {
            },
            0x00 => {
                return false;
            },
            _ => todo!()
        }
        true
    }
}

impl Mem for CPU {
    fn write_u8(&mut self, addr: u16, value: u8) {
        let addr: usize = addr.into();
        self.ram[addr] = value;
    }

    fn read_u8(&self, addr: u16) -> u8 {
        let addr: usize = addr.into();
        self.ram[addr]
    }

    fn write_u16(&mut self, addr: u16, value: u16) {
        let addr: usize = addr.into();
        let bytes: [u8; 2] = value.to_le_bytes();
        self.ram[addr] = bytes[0];
        self.ram[addr+1] = bytes[1];
    }

    fn read_u16(&self, addr: u16) -> u16 {
        let addr: usize = addr.into();
        let bytes: [u8; 2] = [self.ram[addr], self.ram[addr+1]];
        u16::from_le_bytes(bytes)
    }
}
