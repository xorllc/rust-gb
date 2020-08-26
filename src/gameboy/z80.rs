use std::num::Wrapping;
use super::memory_bus::MemoryBus;
use std::ops::{AddAssign, Deref};
use std::hint::unreachable_unchecked;
use std::borrow::Cow;
use std::str::FromStr;
use std::io::Write;

/**
  @note This Z80 is a bit different from Zilog Z80, and is modified by Sharp and Nintendo to cut price
*/
pub struct Z80 {
    pub emi: bool,

    pub a: u8,
    pub b: u8,
    pub c: u8,
    pub d: u8,
    pub e: u8,
    pub f: u8,
    pub h: u8,
    pub l: u8,

    pub sp: u16,
    pub pc: u16,
}

#[derive(Debug)]
pub enum Reg {
    A, B, C, D, E, H, L,
    AF, BC, DE, HL, SP, PC,
}

#[derive(Debug)]
enum Condition {
    Z, NZ, C, NC, ALWAYS
}

#[derive(Debug)]
enum Index {
    Reg16(Reg),
    Reg16Inc(Reg),
    Reg16Dec(Reg),
    Imm16,
    Reg8(Reg),
    Imm8,
}

#[derive(Debug)]
enum Operand {
    Reg8(Reg),
    Imm8,
    Index8(Index),
    HighIndex8(Index),
    Offset8,
    Reg16(Reg),
    Imm16,
    Index16(Index),
}

#[derive(Debug)]
enum Instruction {
    // Arithmetic & Logic
    ADC(Operand),
    ADD(Operand, Operand),
    AND(Operand),
    CP(Operand),
    DEC(Operand),
    INC(Operand),
    OR(Operand),
    SBC(Operand),
    SUB(Operand),
    XOR(Operand),

    // Non-prefixed Bit Operations
    RLCA,
    RRCA,
    RLA,
    RRA,

    // Prefixed instructions
    BIT(u8, Operand),
    RES(u8, Operand),
    SET(u8, Operand),
    SWAP(Operand),
    RL(Operand),
    RLC(Operand),
    RR(Operand),
    RRC(Operand),
    SLA(Operand),
    SRA(Operand),
    SRL(Operand),

    // Memory
    LD(Operand, Operand),
    PUSH(Operand),
    POP(Operand),

    // Jumps
    CALL(Condition, Operand),
    JP(Condition, Operand),
    JR(Condition, Operand),
    RET(Condition),
    RETI,
    RST(u16),

    // Miscellaneous
    CCF,
    CPL,
    DAA,
    DI,
    EI,
    HALT,
    NOP,
    SCF,
    STOP,
    INVALID,
    PREFIX
}

impl Z80 where {
    pub fn read_reg8(&self, reg: &Reg) -> u8 {
        let data = match reg {
            Reg::A => self.a,
            Reg::B => self.b,
            Reg::C => self.c,
            Reg::D => self.d,
            Reg::E => self.e,
            Reg::H => self.h,
            Reg::L => self.l,
            _ => panic!("Tried to load 8-bit register with Reg16")
        };
        data
    }

    pub fn read_reg16(&self, reg: &Reg) -> u16 {
        let data = match reg {
            Reg::AF => ((self.a as u16) << 8 | self.f as u16),
            Reg::BC => ((self.b as u16) << 8 | self.c as u16),
            Reg::DE => ((self.d as u16) << 8 | self.e as u16),
            Reg::HL => ((self.h as u16) << 8 | self.l as u16),
            Reg::SP => self.sp,
            Reg::PC => self.pc,
            _ => panic!("Tried to load 16-bit register with Reg8")
        };
        data
    }

    pub fn write_reg8(&mut self, reg: &Reg, data: u8) {
        match reg {
            Reg::A => self.a = data,
            Reg::B => self.b = data,
            Reg::C => self.c = data,
            Reg::D => self.d = data,
            Reg::E => self.e = data,
            Reg::H => self.h = data,
            Reg::L => self.l = data,
            _ => panic!()
        }
    }

    pub fn write_reg16(&mut self, reg: &Reg, data: u16) {
        match reg {
            Reg::AF => {
                self.a = ((data & 0xFF00) >> 8) as u8;
                self.f = (data & 0xF0) as u8;
            },
            Reg::BC => {
                self.b = ((data & 0xFF00) >> 8) as u8;
                self.c = (data & 0xFF) as u8;
            },
            Reg::DE => {
                self.d = ((data & 0xFF00) >> 8) as u8;
                self.e = (data & 0xFF) as u8;
            },
            Reg::HL => {
                self.h = ((data & 0xFF00) >> 8) as u8;
                self.l = (data & 0xFF) as u8;
            },
            Reg::SP => self.sp = data,
            Reg::PC => self.pc = data,
            _ => panic!()
        }
    }

    fn is_16bit(&self, op: &Operand) -> bool {
        match op {
            Operand::Reg8(_) => false,
            Operand::Imm8 => false,
            Operand::Index8(_) => false,
            Operand::HighIndex8(_) => false,
            Operand::Offset8 => false,
            Operand::Reg16(_) => true,
            Operand::Imm16 => true,
            Operand::Index16(_) => true,
        }
    }

    fn resolve_index(&mut self, bus: &mut MemoryBus, idx: &Index) -> u16 {
        match idx {
            Index::Reg16(r) => self.read_reg16(&r),
            Index::Reg16Inc(r) => {
                let addr = self.read_reg16(&r);
                self.write_reg16(&r, self.read_reg16(&r).wrapping_add(1));
                addr
            },
            Index::Reg16Dec(r) => {
                let addr = self.read_reg16(&r);
                self.write_reg16(&r, self.read_reg16(&r).wrapping_sub(1));
                addr
            },
            Index::Imm16 => self.fetch_u16(bus),
            Index::Reg8(r) => {
                0xFF00 | self.read_reg8(&r) as u16
            },
            Index::Imm8 => {
                0xFF00 | self.fetch_u8(bus) as u16
            }
        }
    }

    fn read_op8(&mut self, bus: &mut MemoryBus, op: &Operand) -> u8 {
        match op {
            Operand::Reg8(r) => self.read_reg8(&r),
            Operand::Imm8 => self.fetch_u8(bus),
            Operand::Index8(addr) => {
                let addr = self.resolve_index(bus, addr);
                bus.read_u8(addr)
            },
            Operand::HighIndex8(addr) => {
                let addr = self.resolve_index(bus, addr);
                bus.read_u8(addr)
            },
            Operand::Offset8 => { panic!("Offset should be treated specially!"); },
            _ => { panic!("Tried to load 16-bit operand with op8!"); }
        }
    }

    fn read_op16(&mut self, bus: &mut MemoryBus, op: &Operand) -> u16 {
        match op {
            Operand::Reg16(r) => self.read_reg16(&r),
            Operand::Imm16 => self.fetch_u16(bus),
            Operand::Index16(addr) => {
                let addr = self.resolve_index(bus, addr);
                bus.read_u16(addr)
            },
            _ => { panic!("Tried to read 8-bit operand with Op16!"); }
        }
    }

    fn write_op8(&mut self, bus: &mut MemoryBus, op: &Operand, data: u8) {
        match op {
            Operand::Reg8(r) => self.write_reg8(&r, data),
            Operand::Index8(addr) => {
                let addr = self.resolve_index(bus, addr);
                bus.write_u8(addr, data)
            },
            Operand::HighIndex8(addr) => {
                let addr = self.resolve_index(bus, addr);
                bus.write_u8(addr, data)
            },
            Operand::Imm8 => panic!("Unable to write to immediate!"),
            Operand::Offset8 => panic!("Unable to write to offset!"),
            _ => { panic!("Tried to store 16-bit operand with op8!"); }
        }
    }

    fn write_op16(&mut self, bus: &mut MemoryBus, op: &Operand, data: u16) {
        match op {
            Operand::Reg16(r) => self.write_reg16(&r, data),
            Operand::Index16(addr) => {
                let addr = self.resolve_index(bus, addr);
                bus.write_u16(addr, data)
            },
            Operand::Imm16 => panic!("Unable to write to immediate!"),
            _ => panic!("Tried to read 8-bit operand with Op16!")
        }
    }

    fn set_flags(&mut self, z: bool, n: bool, h: bool, c: bool) {
        if z {
            self.f |= 0b10000000;
        }
        if n {
            self.f |= 0b01000000;
        }
        if h {
            self.f |= 0b00100000;
        }
        if c {
            self.f |= 0b00010000;
        }
    }

    fn clear_flags(&mut self, z: bool, n: bool, h: bool, c: bool) {
        let mut mask = 0xF0u8;
        if z {
            mask &= 0b01111111;
        }
        if n {
            mask &= 0b10111111;
        }
        if h {
            mask &= 0b11011111;
        }
        if c {
            mask &= 0b11101111;
        }
        self.f &= mask;
    }

    fn read_flags(&self, z: bool, n: bool, h: bool, c: bool) -> bool{
        if z {
            (self.f & 0b10000000) != 0
        }
        else if n {
            (self.f & 0b01000000) != 0
        }
        else if h {
            (self.f & 0b00100000) != 0
        }
        else {
            (self.f & 0b00010000) != 0
        }
    }

    fn fetch_u8(&mut self, bus: &mut MemoryBus) -> u8 {
        let data = bus.read_u8(self.pc);
        self.pc = self.pc.wrapping_add(1);
        data
    }

    fn fetch_u16(&mut self, bus: &mut MemoryBus) -> u16 {
        let data = bus.read_u16(self.pc);
        self.pc = self.pc.wrapping_add(2);
        data
    }

    fn check_cond(&self, cond: &Condition) -> bool {
        match cond {
            Condition::Z => {
                self.read_flags(true, false, false, false) == true
            },
            Condition::C => {
                self.read_flags(false, false, false, true) == true
            },
            Condition::NZ => {
                self.read_flags(true, false, false, false) == false
            },
            Condition::NC => {
                self.read_flags(false, false, false, true) == false
            },
            Condition::ALWAYS => true,
        }
    }

    fn push_u8(&mut self, bus: &mut MemoryBus, data: u8) {
        self.sp = self.sp.wrapping_sub(1);
        bus.write_u8(self.sp, data);
    }

    fn push_u16(&mut self, bus: &mut MemoryBus, data: u16) {
        let b0 = (data & 0xFF) as u8;
        let b1 = ((data >> 8) & 0xFF) as u8;
        self.push_u8(bus, b1);
        self.push_u8(bus, b0);
    }

    fn pop_u8(&mut self, bus: &mut MemoryBus) -> u8 {
        let data = bus.read_u8(self.sp);
        self.sp = self.sp.wrapping_add(1);
        data
    }

    fn pop_u16(&mut self, bus: &mut MemoryBus) -> u16 {
        let b0 = self.pop_u8(bus) as u16;
        let b1 = self.pop_u8(bus) as u16;
        (b1 << 8) | b0
    }

    fn handle_interrupt(&mut self, bus: &mut MemoryBus) -> bool {
        if self.emi {
            let mut int_vector: u16 = 0xFFFF;
            let interrupt = bus.interrupt_enable & bus.interrupt_flag;

            if interrupt & 1 != 0 {
                bus.interrupt_flag &= 0b1111_1110;
                int_vector = 0x40;
            } else if interrupt & (1 << 1) != 0 {
                bus.interrupt_flag &= 0b1111_1101;
                int_vector = 0x48;
            } else if interrupt & (1 << 2) != 0 {
                bus.interrupt_flag &= 0b1111_1011;
                int_vector = 0x50;
            }

            if int_vector == 0xFFFF {
                false
            }
            else {
                self.emi = false;
                self.push_u16(bus, self.pc);
                self.pc = int_vector;
                bus.tick(4); //TODO: Check actual cycles taken
                true
            }
        }
        else { false }
    }

    pub fn new() -> Z80 {
        Z80 {
            emi: false,
            a: 0,
            b: 0,
            c: 0,
            d: 0,
            e: 0,
            f: 0,
            h: 0,
            l: 0,
            sp: 0,
            pc: 0,
        }
    }

    pub fn disassembly(&self, bus: &mut MemoryBus) -> String {
        let opcode = bus.read_u8(self.pc);
        let (_, ins) = decode_ins(opcode);
        format!("{:#06x} [AF: {:04x}  BC: {:04x}  DE: {:04x}  HL: {:04x}  SP: {:04x}  LY: {:02x}  LCDC: {:02x}] {:02x}{:02x}{:02x}{:02x} {:?}",
                self.pc,
                self.read_reg16(&Reg::AF),
                self.read_reg16(&Reg::BC),
                self.read_reg16(&Reg::DE),
                self.read_reg16(&Reg::HL),
                self.read_reg16(&Reg::SP),
                bus.read_u8(0xFF44),
                bus.read_u8(0xFF40),
                bus.read_u8(self.pc),
                bus.read_u8(self.pc+1),
                bus.read_u8(self.pc+2),
                bus.read_u8(self.pc+3), ins)
    }

    pub fn tick(&mut self, bus: &mut MemoryBus) {
        let mut pending_turn_on_emi = false;
        let opcode = self.fetch_u8(bus);
        let (cycle, ins) = decode_ins(opcode);

        // Decode prefix
        let (cycle, ins) = match ins {
            Instruction::PREFIX => {
                let op = self.fetch_u8(bus);
                decode_prefix(op)
            },
            x => (cycle, x)
        };

        // Cycle can be changed on conditional jump
        let mut cycle = cycle;

        // Execute it
        match ins {
            // Arithmetic & Logic
            Instruction::ADC(x) => {
                let from_carry = if self.read_flags(false, false, false, true) {1u8} else {0u8};
                let data = self.read_op8(bus, &x);
                let half_carry = (self.a & 0xF) + (data & 0xF) + from_carry > 0xF;
                let (value, carry) = self.a.overflowing_add(data);
                let (value, second_carry) = value.overflowing_add(from_carry);
                self.a = value;
                self.clear_flags(true, true, true, true);
                self.set_flags(self.a == 0, false, half_carry, carry || second_carry);
            },
            Instruction::ADD(y, x) => {
                if opcode == 0x09 || opcode == 0x19 || opcode == 0x29 || opcode == 0x39 {
                    let src = self.read_op16(bus, &y);
                    let data = self.read_op16(bus, &x);
                    let half_carry = (((src & 0xFFF) + (data & 0xFFF)) & 0x1000) != 0;
                    let (value, carry) = src.overflowing_add(data);
                    self.write_op16(bus, &y, value);
                    self.clear_flags(false, true, true, true);
                    self.set_flags(false, false, half_carry, carry);
                }
                else if opcode == 0xE8 {
                    let src = self.read_op16(bus, &y) as u16;
                    let data = self.fetch_u8(bus) as i8 as i16 as u16;
                    let half_carry = (src & 0xF).wrapping_add(data & 0xF) & 0xF0 != 0;
                    let carry = (src & 0xFF).wrapping_add(data & 0xFF) & 0xFF00 != 0;
                    let value = src.wrapping_add(data);
                    self.write_op16(bus, &y, value);
                    self.clear_flags(true, true, true, true);
                    self.set_flags(false, false, half_carry, carry);
                }
                else {
                    let src = self.read_op8(bus, &y);
                    let data = self.read_op8(bus, &x);
                    let half_carry = (((src & 0xF) + (data & 0xF)) & 0x10) != 0;
                    let (value, carry) = src.overflowing_add(data);
                    self.write_op8(bus, &y, value);
                    self.clear_flags(true, true, true, true);
                    self.set_flags(value == 0, false, half_carry, carry);
                }
            },
            Instruction::AND(x) => {
                let data = self.read_op8(bus, &x);
                let value = self.a & data;
                self.a = value;
                self.clear_flags(true, true, true, true);
                self.set_flags(self.a == 0, false, true, false);
            },
            Instruction::CP(x) => {
                let data = self.read_op8(bus, &x);
                let half_carry = (((self.a & 0xF).wrapping_sub(data & 0xF)) & 0x10) != 0;
                let (value, carry) = self.a.overflowing_sub(data);
                self.clear_flags(true, false, true, true);
                self.set_flags(value == 0, true, half_carry, carry);
            },
            Instruction::DEC(x) => {
                if opcode & 0x0F == 0x0B {
                    let data = self.read_op16(bus, &x);
                    let value = data.wrapping_sub(1);
                    self.write_op16(bus, &x, value);
                }
                else {
                    let data = self.read_op8(bus, &x);
                    let half_carry = ((data & 0xF).wrapping_sub(1) & 0x10) != 0;
                    let value = data.wrapping_sub(1);
                    self.write_op8(bus, &x, value);
                    self.clear_flags(true, true, true, false);
                    self.set_flags(value == 0, true, half_carry, false);
                }
            },
            Instruction::INC(x) => {
                if opcode & 0x0F == 0x03 {
                    let data = self.read_op16(bus, &x);
                    let value = data.wrapping_add(1);
                    self.write_op16(bus, &x, value);
                }
                else {
                    let data = self.read_op8(bus, &x);
                    let half_carry = (((data & 0xF) + 1) & 0x10) != 0;
                    let value = data.wrapping_add(1);
                    self.write_op8(bus, &x, value);
                    self.clear_flags(true, true, true, false);
                    self.set_flags(value == 0, false, half_carry, false);
                }
            },
            Instruction::OR(x) => {
                let data = self.read_op8(bus, &x);
                let value = self.a | data;
                self.a = value;
                self.clear_flags(true, true, true, true);
                self.set_flags(value == 0, false, false, false);
            },
            Instruction::SBC(x) => {
                let from_carry = if self.read_flags(false, false, false, true) {1u8} else {0u8};
                let data = self.read_op8(bus, &x);
                let half_carry = (self.a & 0xF) < (data & 0xF) + from_carry;
                let (value, carry) = self.a.overflowing_sub(data);
                let (value, second_carry) = value.overflowing_sub(from_carry);
                self.a = value;
                self.clear_flags(true, true, true, true);
                self.set_flags(value == 0, true, half_carry, carry || second_carry);
            },
            Instruction::SUB(x) => {
                let data = self.read_op8(bus, &x);
                let half_carry = ((self.a & 0xF).wrapping_sub(data & 0xF) & 0x10) != 0;
                let (value, carry) = self.a.overflowing_sub(data);
                self.a = value;
                self.clear_flags(true, true, true, true);
                self.set_flags(value == 0, true, half_carry, carry);
            },
            Instruction::XOR(x) => {
                let data = self.read_op8(bus, &x);
                let value = self.a ^ data;
                self.a = value;
                self.clear_flags(true, true, true, true);
                self.set_flags(value == 0, false, false, false);
            },

            // Non-prefixed Bit Operations
            Instruction::RLCA => {
                let new_carry = self.a & 0x80 != 0;
                let value = self.a.rotate_left(1);
                self.a = value;
                self.clear_flags(true, true, true, true);
                self.set_flags(false, false, false, new_carry);
            },
            Instruction::RRCA => {
                let new_carry = self.a & 1 != 0;
                let value = self.a.rotate_right(1);
                self.a = value;
                self.clear_flags(true, true, true, true);
                self.set_flags(false, false, false, new_carry);
            },
            Instruction::RLA => {
                let new_carry = self.a & 0x80 != 0;
                let old_carry = self.read_flags(false, false, false, true);
                let value = (self.a << 1) | if old_carry {1} else {0};
                self.a = value;
                self.clear_flags(true, true, true, true);
                self.set_flags(false, false, false, new_carry);
            },
            Instruction::RRA => {
                let new_carry = self.a & 1 != 0;
                let old_carry = self.read_flags(false, false, false, true);
                let value = (self.a >> 1) | if old_carry {0x80} else {0};
                self.a = value;
                self.clear_flags(true, true, true, true);
                self.set_flags(false, false, false, new_carry);
            }

            // Prefixed instructions
            Instruction::BIT(y, x) => {
                let data = self.read_op8(bus, &x);
                let value = data & y;
                self.clear_flags(true, true, true, false);
                self.set_flags(value == 0, false, true, false);
            },
            Instruction::RES(y, x) => {
                let data = self.read_op8(bus, &x);
                let value = data & (!y);
                self.write_op8(bus, &x, value);
            }
            Instruction::SET(y, x) => {
                let data = self.read_op8(bus, &x);
                let value = data | y;
                self.write_op8(bus, &x, value);
            },
            Instruction::SWAP(x) => {
                let data = self.read_op8(bus, &x);
                let value = ((data & 0x0F) << 4) | ((data & 0xF0) >> 4);
                self.write_op8(bus, &x, value);
                self.clear_flags(true, true, true, true);
                self.set_flags(value == 0, false, false, false);
            },
            Instruction::RL(x) => {
                let data = self.read_op8(bus, &x);
                let new_carry = data & 0x80 != 0;
                let old_carry = self.read_flags(false, false, false, true);
                let value = (data << 1) | if old_carry {1} else {0};
                self.write_op8(bus, &x, value);
                self.clear_flags(true, true, true, true);
                self.set_flags(value == 0, false, false, new_carry);
            },
            Instruction::RLC(x) => {
                let data = self.read_op8(bus, &x);
                let new_carry = data & 0x80 != 0;
                let value = data.rotate_left(1);
                self.write_op8(bus, &x, value);
                self.clear_flags(true, true, true, true);
                self.set_flags(value == 0, false, false, new_carry);
            },
            Instruction::RR(x) => {
                let data = self.read_op8(bus, &x);
                let new_carry = data & 1 != 0;
                let old_carry = self.read_flags(false, false, false, true);
                let value = (data >> 1) | if old_carry {0x80} else {0};
                self.write_op8(bus, &x, value);
                self.clear_flags(true, true, true, true);
                self.set_flags(value == 0, false, false, new_carry);
            },
            Instruction::RRC(x) => {
                let data = self.read_op8(bus, &x);
                let new_carry = data & 1 != 0;
                let value = data.rotate_right(1);
                self.write_op8(bus, &x, value);
                self.clear_flags(true, true, true, true);
                self.set_flags(value == 0, false, false, new_carry);
            },
            Instruction::SLA(x) => {
                let data = self.read_op8(bus, &x);
                let new_carry = data & 0x80 != 0;
                let value = (data << 1) & 0xFE;
                self.write_op8(bus, &x, value);
                self.clear_flags(true, true, true, true);
                self.set_flags(value == 0, false, false, new_carry);
            },
            Instruction::SRA(x) => {
                let data = self.read_op8(bus, &x);
                let new_carry = data & 1 != 0;
                let value = ((data >> 1) & 0x7F) | (data & 0x80);
                self.write_op8(bus, &x, value);
                self.clear_flags(true, true, true, true);
                self.set_flags(value == 0, false, false, new_carry);
            },
            Instruction::SRL(x) => {
                let data = self.read_op8(bus, &x);
                let new_carry = data & 1 != 0;
                let value = (data >> 1) & 0x7F;
                self.write_op8(bus, &x, value);
                self.clear_flags(true, true, true, true);
                self.set_flags(value == 0, false, false, new_carry);
            },

            // Memory
            Instruction::LD(y, x) => {
                if opcode == 0xF8 {
                    let data = self.fetch_u8(bus) as i8 as i16 as u16;
                    let value = self.sp.wrapping_add(data);
                    let carry = (self.sp & 0xFF) + (data & 0xFF) > 0xFF;
                    let half_carry = (self.sp & 0xF) + (data & 0xF) > 0xF;
                    self.write_op16(bus, &y, value);
                    self.clear_flags(true, true, true, true);
                    self.set_flags(false, false, half_carry, carry);
                }
                else if self.is_16bit(&y) {
                    let data = self.read_op16(bus, &x);
                    self.write_op16(bus, &y, data);
                }
                else {
                    let data = self.read_op8(bus, &x);
                    self.write_op8(bus, &y, data);
                }
            },
            Instruction::PUSH(x) => {
                let data = self.read_op16(bus, &x);
                self.push_u16(bus, data);
            },
            Instruction::POP(x) => {
                let data = self.pop_u16(bus);
                self.write_op16(bus, &x, data);
            },

            // Jumps
            Instruction::CALL(cond, x) => {
                let cond = self.check_cond(&cond);
                let addr = self.read_op16(bus, &x);
                if cond {
                    cycle = 24;
                    self.push_u16(bus, self.pc);
                    self.pc = addr;
                }
                else {
                    cycle = 12;
                }
            },
            Instruction::JP(cond, x) => {
                let cond = self.check_cond(&cond);
                let data = self.read_op16(bus, &x);
                if cond {
                    self.pc = data;
                }
                if cycle == 0 {
                    cycle = if cond {16} else {12};
                }
            },
            Instruction::JR(cond, x) => {
                let cond = self.check_cond(&cond);
                let offset = self.fetch_u8(bus) as i8 as i16;
                if cond {
                    cycle = 12;
                    self.pc = self.pc.wrapping_add(offset as u16);
                }
                else {
                    cycle = 8;
                }
            },
            Instruction::RET(cond) => {
                let cond = self.check_cond(&cond);
                if cond {
                    let addr = self.pop_u16(bus);
                    self.pc = addr;
                }
                if cycle == 0 {
                    cycle = if cond {20} else {8};
                }
            },
            Instruction::RETI => {
                let addr = self.pop_u16(bus);
                self.pc = addr;
                pending_turn_on_emi = true;
            },
            Instruction::RST(v) => {
                self.push_u16(bus, self.pc);
                self.pc = v;
            },

            // Miscellaneous
            Instruction::CCF => {
                let value = self.read_flags(false, false, false, true);
                if value {
                    self.clear_flags(false, true, true, true);
                } else {
                    self.clear_flags(false, true, true, false);
                    self.set_flags(false, false, false, true);
                }
            },
            Instruction::CPL => {
                self.a = !self.a;
                self.set_flags(false, true, true, false);
            }
            Instruction::DAA => {
                let mut modifier: u16 = 0;
                let flag_c = self.read_flags(false, false, false, true);
                let flag_h = self.read_flags(false, false, true, false);
                let flag_n = self.read_flags(false, true, false, false);

                if flag_c {
                    modifier = 0x60;
                }

                if flag_h || (!flag_n && (self.a & 0x0F) > 9) {
                    modifier |= 0x06;
                }

                if flag_c || (!flag_n && self.a > 0x99) {
                    modifier |= 0x60;
                }

                let value = if !flag_n {
                    self.a.wrapping_add(modifier as u8)
                } else {
                    self.a.wrapping_sub(modifier as u8)
                };

                self.clear_flags(true, false, true, true);
                self.set_flags(value == 0, false, false, ((modifier << 2) & 0x100) != 0);
                self.a = value;
            },
            Instruction::DI => {
                self.emi = false;
            },
            Instruction::EI => {
                pending_turn_on_emi = true;
            },
            Instruction::HALT => {
                bus.tick(cycle as i32);
                cycle = 0;
                if self.emi {
                    while bus.interrupt_enable & bus.interrupt_flag == 0 {
                        bus.tick(4);
                    }
                }
            },
            Instruction::NOP => {
                // No operation
            },
            Instruction::SCF => {
                self.clear_flags(false, true, true, true);
                self.set_flags(false, false, false, true);
            },
            Instruction::STOP => {
                let _value = self.fetch_u8(bus);
                if self.emi {
                    bus.tick(cycle as i32);
                    cycle = 0;
                    while !self.handle_interrupt(bus) {
                        bus.tick(4);
                    }
                }
            },
            Instruction::INVALID => {
                panic!(format!("Invalid opcode: {:#04x} on {:04x}!", opcode, self.pc));
            },
            Instruction::PREFIX => unreachable!()
        }

        bus.tick(cycle as i32);
        self.handle_interrupt(bus);

        if self.emi == false && pending_turn_on_emi {
            self.emi = true;
        }
    }
}

fn decode_ins(opcode: u8) -> (u8, Instruction) {
    match opcode {
        0x00 => (4, Instruction::NOP),
        0x01 => (12, Instruction::LD(Operand::Reg16(Reg::BC), Operand::Imm16)),
        0x02 => (8, Instruction::LD(Operand::Index8(Index::Reg16(Reg::BC)), Operand::Reg8(Reg::A))),
        0x03 => (8, Instruction::INC(Operand::Reg16(Reg::BC))),
        0x04 => (4, Instruction::INC(Operand::Reg8(Reg::B))),
        0x05 => (4, Instruction::DEC(Operand::Reg8(Reg::B))),
        0x06 => (8, Instruction::LD(Operand::Reg8(Reg::B), Operand::Imm8)),
        0x07 => (4, Instruction::RLCA),
        0x08 => (20, Instruction::LD(Operand::Index16(Index::Imm16), Operand::Reg16(Reg::SP))),
        0x09 => (8, Instruction::ADD(Operand::Reg16(Reg::HL), Operand::Reg16(Reg::BC))),
        0x0A => (8, Instruction::LD(Operand::Reg8(Reg::A), Operand::Index8(Index::Reg16(Reg::BC)))),
        0x0B => (8, Instruction::DEC(Operand::Reg16(Reg::BC))),
        0x0C => (4, Instruction::INC(Operand::Reg8(Reg::C))),
        0x0D => (4, Instruction::DEC(Operand::Reg8(Reg::C))),
        0x0E => (8, Instruction::LD(Operand::Reg8(Reg::C), Operand::Imm8)),
        0x0F => (4, Instruction::RRCA),

        0x10 => (4, Instruction::STOP),
        0x11 => (12, Instruction::LD(Operand::Reg16(Reg::DE), Operand::Imm16)),
        0x12 => (8, Instruction::LD(Operand::Index8(Index::Reg16(Reg::DE)), Operand::Reg8(Reg::A))),
        0x13 => (8, Instruction::INC(Operand::Reg16(Reg::DE))),
        0x14 => (4, Instruction::INC(Operand::Reg8(Reg::D))),
        0x15 => (4, Instruction::DEC(Operand::Reg8(Reg::D))),
        0x16 => (8, Instruction::LD(Operand::Reg8(Reg::D), Operand::Imm8)),
        0x17 => (4, Instruction::RLA),
        0x18 => (12, Instruction::JR(Condition::ALWAYS, Operand::Offset8)),
        0x19 => (8, Instruction::ADD(Operand::Reg16(Reg::HL), Operand::Reg16(Reg::DE))),
        0x1A => (8, Instruction::LD(Operand::Reg8(Reg::A), Operand::Index8(Index::Reg16(Reg::DE)))),
        0x1B => (8, Instruction::DEC(Operand::Reg16(Reg::DE))),
        0x1C => (4, Instruction::INC(Operand::Reg8(Reg::E))),
        0x1D => (4, Instruction::DEC(Operand::Reg8(Reg::E))),
        0x1E => (8, Instruction::LD(Operand::Reg8(Reg::E), Operand::Imm8)),
        0x1F => (4, Instruction::RRA),

        0x20 => (0, Instruction::JR(Condition::NZ, Operand::Offset8)),
        0x21 => (12, Instruction::LD(Operand::Reg16(Reg::HL), Operand::Imm16)),
        0x22 => (8, Instruction::LD(Operand::Index8(Index::Reg16Inc(Reg::HL)), Operand::Reg8(Reg::A))),
        0x23 => (8, Instruction::INC(Operand::Reg16(Reg::HL))),
        0x24 => (4, Instruction::INC(Operand::Reg8(Reg::H))),
        0x25 => (4, Instruction::DEC(Operand::Reg8(Reg::H))),
        0x26 => (8, Instruction::LD(Operand::Reg8(Reg::H), Operand::Imm8)),
        0x27 => (4, Instruction::DAA),
        0x28 => (0, Instruction::JR(Condition::Z, Operand::Offset8)),
        0x29 => (8, Instruction::ADD(Operand::Reg16(Reg::HL), Operand::Reg16(Reg::HL))),
        0x2A => (8, Instruction::LD(Operand::Reg8(Reg::A), Operand::Index8(Index::Reg16Inc(Reg::HL)))),
        0x2B => (8, Instruction::DEC(Operand::Reg16(Reg::HL))),
        0x2C => (4, Instruction::INC(Operand::Reg8(Reg::L))),
        0x2D => (4, Instruction::DEC(Operand::Reg8(Reg::L))),
        0x2E => (8, Instruction::LD(Operand::Reg8(Reg::L), Operand::Imm8)),
        0x2F => (4, Instruction::CPL),

        0x30 => (0, Instruction::JR(Condition::NC, Operand::Offset8)),
        0x31 => (12, Instruction::LD(Operand::Reg16(Reg::SP), Operand::Imm16)),
        0x32 => (8, Instruction::LD(Operand::Index8(Index::Reg16Dec(Reg::HL)), Operand::Reg8(Reg::A))),
        0x33 => (8, Instruction::INC(Operand::Reg16(Reg::SP))),
        0x34 => (12, Instruction::INC(Operand::Index8(Index::Reg16(Reg::HL)))),
        0x35 => (12, Instruction::DEC(Operand::Index8(Index::Reg16(Reg::HL)))),
        0x36 => (12, Instruction::LD(Operand::Index8(Index::Reg16(Reg::HL)), Operand::Imm8)),
        0x37 => (4, Instruction::SCF),
        0x38 => (0, Instruction::JR(Condition::C, Operand::Offset8)),
        0x39 => (8, Instruction::ADD(Operand::Reg16(Reg::HL), Operand::Reg16(Reg::SP))),
        0x3A => (8, Instruction::LD(Operand::Reg8(Reg::A), Operand::Index8(Index::Reg16Dec(Reg::HL)))),
        0x3B => (8, Instruction::DEC(Operand::Reg16(Reg::SP))),
        0x3C => (4, Instruction::INC(Operand::Reg8(Reg::A))),
        0x3D => (4, Instruction::DEC(Operand::Reg8(Reg::A))),
        0x3E => (8, Instruction::LD(Operand::Reg8(Reg::A), Operand::Imm8)),
        0x3F => (4, Instruction::CCF),

        0x40 => (4, Instruction::LD(Operand::Reg8(Reg::B), Operand::Reg8(Reg::B))),
        0x41 => (4, Instruction::LD(Operand::Reg8(Reg::B), Operand::Reg8(Reg::C))),
        0x42 => (4, Instruction::LD(Operand::Reg8(Reg::B), Operand::Reg8(Reg::D))),
        0x43 => (4, Instruction::LD(Operand::Reg8(Reg::B), Operand::Reg8(Reg::E))),
        0x44 => (4, Instruction::LD(Operand::Reg8(Reg::B), Operand::Reg8(Reg::H))),
        0x45 => (4, Instruction::LD(Operand::Reg8(Reg::B), Operand::Reg8(Reg::L))),
        0x46 => (8, Instruction::LD(Operand::Reg8(Reg::B), Operand::Index8(Index::Reg16(Reg::HL)))),
        0x47 => (4, Instruction::LD(Operand::Reg8(Reg::B), Operand::Reg8(Reg::A))),
        0x48 => (4, Instruction::LD(Operand::Reg8(Reg::C), Operand::Reg8(Reg::B))),
        0x49 => (4, Instruction::LD(Operand::Reg8(Reg::C), Operand::Reg8(Reg::C))),
        0x4A => (4, Instruction::LD(Operand::Reg8(Reg::C), Operand::Reg8(Reg::D))),
        0x4B => (4, Instruction::LD(Operand::Reg8(Reg::C), Operand::Reg8(Reg::E))),
        0x4C => (4, Instruction::LD(Operand::Reg8(Reg::C), Operand::Reg8(Reg::H))),
        0x4D => (4, Instruction::LD(Operand::Reg8(Reg::C), Operand::Reg8(Reg::L))),
        0x4E => (8, Instruction::LD(Operand::Reg8(Reg::C), Operand::Index8(Index::Reg16(Reg::HL)))),
        0x4F => (4, Instruction::LD(Operand::Reg8(Reg::C), Operand::Reg8(Reg::A))),

        0x50 => (4, Instruction::LD(Operand::Reg8(Reg::D), Operand::Reg8(Reg::B))),
        0x51 => (4, Instruction::LD(Operand::Reg8(Reg::D), Operand::Reg8(Reg::C))),
        0x52 => (4, Instruction::LD(Operand::Reg8(Reg::D), Operand::Reg8(Reg::D))),
        0x53 => (4, Instruction::LD(Operand::Reg8(Reg::D), Operand::Reg8(Reg::E))),
        0x54 => (4, Instruction::LD(Operand::Reg8(Reg::D), Operand::Reg8(Reg::H))),
        0x55 => (4, Instruction::LD(Operand::Reg8(Reg::D), Operand::Reg8(Reg::L))),
        0x56 => (8, Instruction::LD(Operand::Reg8(Reg::D), Operand::Index8(Index::Reg16(Reg::HL)))),
        0x57 => (4, Instruction::LD(Operand::Reg8(Reg::D), Operand::Reg8(Reg::A))),
        0x58 => (4, Instruction::LD(Operand::Reg8(Reg::E), Operand::Reg8(Reg::B))),
        0x59 => (4, Instruction::LD(Operand::Reg8(Reg::E), Operand::Reg8(Reg::C))),
        0x5A => (4, Instruction::LD(Operand::Reg8(Reg::E), Operand::Reg8(Reg::D))),
        0x5B => (4, Instruction::LD(Operand::Reg8(Reg::E), Operand::Reg8(Reg::E))),
        0x5C => (4, Instruction::LD(Operand::Reg8(Reg::E), Operand::Reg8(Reg::H))),
        0x5D => (4, Instruction::LD(Operand::Reg8(Reg::E), Operand::Reg8(Reg::L))),
        0x5E => (8, Instruction::LD(Operand::Reg8(Reg::E), Operand::Index8(Index::Reg16(Reg::HL)))),
        0x5F => (4, Instruction::LD(Operand::Reg8(Reg::E), Operand::Reg8(Reg::A))),

        0x60 => (4, Instruction::LD(Operand::Reg8(Reg::H), Operand::Reg8(Reg::B))),
        0x61 => (4, Instruction::LD(Operand::Reg8(Reg::H), Operand::Reg8(Reg::C))),
        0x62 => (4, Instruction::LD(Operand::Reg8(Reg::H), Operand::Reg8(Reg::D))),
        0x63 => (4, Instruction::LD(Operand::Reg8(Reg::H), Operand::Reg8(Reg::E))),
        0x64 => (4, Instruction::LD(Operand::Reg8(Reg::H), Operand::Reg8(Reg::H))),
        0x65 => (4, Instruction::LD(Operand::Reg8(Reg::H), Operand::Reg8(Reg::L))),
        0x66 => (8, Instruction::LD(Operand::Reg8(Reg::H), Operand::Index8(Index::Reg16(Reg::HL)))),
        0x67 => (4, Instruction::LD(Operand::Reg8(Reg::H), Operand::Reg8(Reg::A))),
        0x68 => (4, Instruction::LD(Operand::Reg8(Reg::L), Operand::Reg8(Reg::B))),
        0x69 => (4, Instruction::LD(Operand::Reg8(Reg::L), Operand::Reg8(Reg::C))),
        0x6A => (4, Instruction::LD(Operand::Reg8(Reg::L), Operand::Reg8(Reg::D))),
        0x6B => (4, Instruction::LD(Operand::Reg8(Reg::L), Operand::Reg8(Reg::E))),
        0x6C => (4, Instruction::LD(Operand::Reg8(Reg::L), Operand::Reg8(Reg::H))),
        0x6D => (4, Instruction::LD(Operand::Reg8(Reg::L), Operand::Reg8(Reg::L))),
        0x6E => (8, Instruction::LD(Operand::Reg8(Reg::L), Operand::Index8(Index::Reg16(Reg::HL)))),
        0x6F => (4, Instruction::LD(Operand::Reg8(Reg::L), Operand::Reg8(Reg::A))),

        0x70 => (8, Instruction::LD(Operand::Index8(Index::Reg16(Reg::HL)), Operand::Reg8(Reg::B))),
        0x71 => (8, Instruction::LD(Operand::Index8(Index::Reg16(Reg::HL)), Operand::Reg8(Reg::C))),
        0x72 => (8, Instruction::LD(Operand::Index8(Index::Reg16(Reg::HL)), Operand::Reg8(Reg::D))),
        0x73 => (8, Instruction::LD(Operand::Index8(Index::Reg16(Reg::HL)), Operand::Reg8(Reg::E))),
        0x74 => (8, Instruction::LD(Operand::Index8(Index::Reg16(Reg::HL)), Operand::Reg8(Reg::H))),
        0x75 => (8, Instruction::LD(Operand::Index8(Index::Reg16(Reg::HL)), Operand::Reg8(Reg::L))),
        0x76 => (4, Instruction::HALT),
        0x77 => (8, Instruction::LD(Operand::Index8(Index::Reg16(Reg::HL)), Operand::Reg8(Reg::A))),
        0x78 => (4, Instruction::LD(Operand::Reg8(Reg::A), Operand::Reg8(Reg::B))),
        0x79 => (4, Instruction::LD(Operand::Reg8(Reg::A), Operand::Reg8(Reg::C))),
        0x7A => (4, Instruction::LD(Operand::Reg8(Reg::A), Operand::Reg8(Reg::D))),
        0x7B => (4, Instruction::LD(Operand::Reg8(Reg::A), Operand::Reg8(Reg::E))),
        0x7C => (4, Instruction::LD(Operand::Reg8(Reg::A), Operand::Reg8(Reg::H))),
        0x7D => (4, Instruction::LD(Operand::Reg8(Reg::A), Operand::Reg8(Reg::L))),
        0x7E => (8, Instruction::LD(Operand::Reg8(Reg::A), Operand::Index8(Index::Reg16(Reg::HL)))),
        0x7F => (4, Instruction::LD(Operand::Reg8(Reg::A), Operand::Reg8(Reg::A))),

        0x80 => (4, Instruction::ADD(Operand::Reg8(Reg::A), Operand::Reg8(Reg::B))),
        0x81 => (4, Instruction::ADD(Operand::Reg8(Reg::A), Operand::Reg8(Reg::C))),
        0x82 => (4, Instruction::ADD(Operand::Reg8(Reg::A), Operand::Reg8(Reg::D))),
        0x83 => (4, Instruction::ADD(Operand::Reg8(Reg::A), Operand::Reg8(Reg::E))),
        0x84 => (4, Instruction::ADD(Operand::Reg8(Reg::A), Operand::Reg8(Reg::H))),
        0x85 => (4, Instruction::ADD(Operand::Reg8(Reg::A), Operand::Reg8(Reg::L))),
        0x86 => (8, Instruction::ADD(Operand::Reg8(Reg::A), Operand::Index8(Index::Reg16(Reg::HL)))),
        0x87 => (4, Instruction::ADD(Operand::Reg8(Reg::A), Operand::Reg8(Reg::A))),
        0x88 => (4, Instruction::ADC(Operand::Reg8(Reg::B))),
        0x89 => (4, Instruction::ADC(Operand::Reg8(Reg::C))),
        0x8A => (4, Instruction::ADC(Operand::Reg8(Reg::D))),
        0x8B => (4, Instruction::ADC(Operand::Reg8(Reg::E))),
        0x8C => (4, Instruction::ADC(Operand::Reg8(Reg::H))),
        0x8D => (4, Instruction::ADC(Operand::Reg8(Reg::L))),
        0x8E => (8, Instruction::ADC(Operand::Index8(Index::Reg16(Reg::HL)))),
        0x8F => (4, Instruction::ADC(Operand::Reg8(Reg::A))),

        0x90 => (4, Instruction::SUB(Operand::Reg8(Reg::B))),
        0x91 => (4, Instruction::SUB(Operand::Reg8(Reg::C))),
        0x92 => (4, Instruction::SUB(Operand::Reg8(Reg::D))),
        0x93 => (4, Instruction::SUB(Operand::Reg8(Reg::E))),
        0x94 => (4, Instruction::SUB(Operand::Reg8(Reg::H))),
        0x95 => (4, Instruction::SUB(Operand::Reg8(Reg::L))),
        0x96 => (8, Instruction::SUB(Operand::Index8(Index::Reg16(Reg::HL)))),
        0x97 => (4, Instruction::SUB(Operand::Reg8(Reg::A))),
        0x98 => (4, Instruction::SBC(Operand::Reg8(Reg::B))),
        0x99 => (4, Instruction::SBC(Operand::Reg8(Reg::C))),
        0x9A => (4, Instruction::SBC(Operand::Reg8(Reg::D))),
        0x9B => (4, Instruction::SBC(Operand::Reg8(Reg::E))),
        0x9C => (4, Instruction::SBC(Operand::Reg8(Reg::H))),
        0x9D => (4, Instruction::SBC(Operand::Reg8(Reg::L))),
        0x9E => (8, Instruction::SBC(Operand::Index8(Index::Reg16(Reg::HL)))),
        0x9F => (4, Instruction::SBC(Operand::Reg8(Reg::A))),

        0xA0 => (4, Instruction::AND(Operand::Reg8(Reg::B))),
        0xA1 => (4, Instruction::AND(Operand::Reg8(Reg::C))),
        0xA2 => (4, Instruction::AND(Operand::Reg8(Reg::D))),
        0xA3 => (4, Instruction::AND(Operand::Reg8(Reg::E))),
        0xA4 => (4, Instruction::AND(Operand::Reg8(Reg::H))),
        0xA5 => (4, Instruction::AND(Operand::Reg8(Reg::L))),
        0xA6 => (8, Instruction::AND(Operand::Index8(Index::Reg16(Reg::HL)))),
        0xA7 => (4, Instruction::AND(Operand::Reg8(Reg::A))),
        0xA8 => (4, Instruction::XOR(Operand::Reg8(Reg::B))),
        0xA9 => (4, Instruction::XOR(Operand::Reg8(Reg::C))),
        0xAA => (4, Instruction::XOR(Operand::Reg8(Reg::D))),
        0xAB => (4, Instruction::XOR(Operand::Reg8(Reg::E))),
        0xAC => (4, Instruction::XOR(Operand::Reg8(Reg::H))),
        0xAD => (4, Instruction::XOR(Operand::Reg8(Reg::L))),
        0xAE => (8, Instruction::XOR(Operand::Index8(Index::Reg16(Reg::HL)))),
        0xAF => (4, Instruction::XOR(Operand::Reg8(Reg::A))),

        0xB0 => (4, Instruction::OR(Operand::Reg8(Reg::B))),
        0xB1 => (4, Instruction::OR(Operand::Reg8(Reg::C))),
        0xB2 => (4, Instruction::OR(Operand::Reg8(Reg::D))),
        0xB3 => (4, Instruction::OR(Operand::Reg8(Reg::E))),
        0xB4 => (4, Instruction::OR(Operand::Reg8(Reg::H))),
        0xB5 => (4, Instruction::OR(Operand::Reg8(Reg::L))),
        0xB6 => (8, Instruction::OR(Operand::Index8(Index::Reg16(Reg::HL)))),
        0xB7 => (4, Instruction::OR(Operand::Reg8(Reg::A))),
        0xB8 => (4, Instruction::CP(Operand::Reg8(Reg::B))),
        0xB9 => (4, Instruction::CP(Operand::Reg8(Reg::C))),
        0xBA => (4, Instruction::CP(Operand::Reg8(Reg::D))),
        0xBB => (4, Instruction::CP(Operand::Reg8(Reg::E))),
        0xBC => (4, Instruction::CP(Operand::Reg8(Reg::H))),
        0xBD => (4, Instruction::CP(Operand::Reg8(Reg::L))),
        0xBE => (8, Instruction::CP(Operand::Index8(Index::Reg16(Reg::HL)))),
        0xBF => (4, Instruction::CP(Operand::Reg8(Reg::A))),

        0xC0 => (0, Instruction::RET(Condition::NZ)),
        0xC1 => (12, Instruction::POP(Operand::Reg16(Reg::BC))),
        0xC2 => (0, Instruction::JP(Condition::NZ, Operand::Imm16)),
        0xC3 => (16, Instruction::JP(Condition::ALWAYS, Operand::Imm16)),
        0xC4 => (0, Instruction::CALL(Condition::NZ, Operand::Imm16)),
        0xC5 => (16, Instruction::PUSH(Operand::Reg16(Reg::BC))),
        0xC6 => (8, Instruction::ADD(Operand::Reg8(Reg::A), Operand::Imm8)),
        0xC7 => (16, Instruction::RST(0x00)),
        0xC8 => (0, Instruction::RET(Condition::Z)),
        0xC9 => (16, Instruction::RET(Condition::ALWAYS)),
        0xCA => (0, Instruction::JP(Condition::Z, Operand::Imm16)),
        0xCB => (0, Instruction::PREFIX),
        0xCC => (0, Instruction::CALL(Condition::Z, Operand::Imm16)),
        0xCD => (24, Instruction::CALL(Condition::ALWAYS, Operand::Imm16)),
        0xCE => (8, Instruction::ADC(Operand::Imm8)),
        0xCF => (16, Instruction::RST(0x08)),

        0xD0 => (0, Instruction::RET(Condition::NC)),
        0xD1 => (12, Instruction::POP(Operand::Reg16(Reg::DE))),
        0xD2 => (0, Instruction::JP(Condition::NC, Operand::Imm16)),
        0xD3 => (0, Instruction::INVALID),
        0xD4 => (0, Instruction::CALL(Condition::NC, Operand::Imm16)),
        0xD5 => (16, Instruction::PUSH(Operand::Reg16(Reg::DE))),
        0xD6 => (8, Instruction::SUB(Operand::Imm8)),
        0xD7 => (16, Instruction::RST(0x10)),
        0xD8 => (0, Instruction::RET(Condition::C)),
        0xD9 => (16, Instruction::RETI),
        0xDA => (0, Instruction::JP(Condition::C, Operand::Imm16)),
        0xDB => (0, Instruction::INVALID),
        0xDC => (0, Instruction::CALL(Condition::C, Operand::Imm16)),
        0xDD => (0, Instruction::INVALID),
        0xDE => (8, Instruction::SBC(Operand::Imm8)),
        0xDF => (16, Instruction::RST(0x18)),

        0xE0 => (12, Instruction::LD(Operand::HighIndex8(Index::Imm8), Operand::Reg8(Reg::A))),
        0xE1 => (12, Instruction::POP(Operand::Reg16(Reg::HL))),
        0xE2 => (8, Instruction::LD(Operand::HighIndex8(Index::Reg8(Reg::C)), Operand::Reg8(Reg::A))),
        0xE3 => (0, Instruction::INVALID),
        0xE4 => (0, Instruction::INVALID),
        0xE5 => (16, Instruction::PUSH(Operand::Reg16(Reg::HL))),
        0xE6 => (8, Instruction::AND(Operand::Imm8)),
        0xE7 => (16, Instruction::RST(0x20)),
        0xE8 => (16, Instruction::ADD(Operand::Reg16(Reg::SP), Operand::Offset8)),
        0xE9 => (4, Instruction::JP(Condition::ALWAYS, Operand::Reg16(Reg::HL))),
        0xEA => (16, Instruction::LD(Operand::Index8(Index::Imm16), Operand::Reg8(Reg::A))),
        0xEB => (0, Instruction::INVALID),
        0xEC => (0, Instruction::INVALID),
        0xED => (0, Instruction::INVALID),
        0xEE => (8, Instruction::XOR(Operand::Imm8)),
        0xEF => (16, Instruction::RST(0x28)),

        0xF0 => (12, Instruction::LD(Operand::Reg8(Reg::A), Operand::HighIndex8(Index::Imm8))),
        0xF1 => (12, Instruction::POP(Operand::Reg16(Reg::AF))),
        0xF2 => (8, Instruction::LD(Operand::Reg8(Reg::A), Operand::HighIndex8(Index::Reg8(Reg::C)))),
        0xF3 => (4, Instruction::DI),
        0xF4 => (0, Instruction::INVALID),
        0xF5 => (16, Instruction::PUSH(Operand::Reg16(Reg::AF))),
        0xF6 => (8, Instruction::OR(Operand::Imm8)),
        0xF7 => (16, Instruction::RST(0x30)),
        0xF8 => (12, Instruction::LD(Operand::Reg16(Reg::HL), Operand::Offset8)),
        0xF9 => (8, Instruction::LD(Operand::Reg16(Reg::SP), Operand::Reg16(Reg::HL))),
        0xFA => (16, Instruction::LD(Operand::Reg8(Reg::A), Operand::Index8(Index::Imm16))),
        0xFB => (4, Instruction::EI),
        0xFC => (0, Instruction::INVALID),
        0xFD => (0, Instruction::INVALID),
        0xFE => (8, Instruction::CP(Operand::Imm8)),
        0xFF => (16, Instruction::RST(0x38)),

        _ => { unsafe { unreachable_unchecked(); } }
    }
}

// https://gb-archive.github.io/salvage/decoding_gbz80_opcodes/Decoding%20Gamboy%20Z80%20Opcodes.html
fn decode_prefix(opcode: u8) -> (u8, Instruction) {
    let mode = (opcode & 0b11000000) >> 6;
    let y = (opcode & 0b00111000) >> 3;
    let z = opcode & 0b00000111;

    let (cycle, reg) = match z {
        0 => (8, Operand::Reg8(Reg::B)),
        1 => (8, Operand::Reg8(Reg::C)),
        2 => (8, Operand::Reg8(Reg::D)),
        3 => (8, Operand::Reg8(Reg::E)),
        4 => (8, Operand::Reg8(Reg::H)),
        5 => (8, Operand::Reg8(Reg::L)),
        6 => (12, Operand::Index8(Index::Reg16(Reg::HL))),
        7 => (8, Operand::Reg8(Reg::A)),
        _ => unreachable!()
    };

    let ins = match mode {
        0 => {
            match y {
                0 => Instruction::RLC(reg),
                1 => Instruction::RRC(reg),
                2 => Instruction::RL(reg),
                3 => Instruction::RR(reg),
                4 => Instruction::SLA(reg),
                5 => Instruction::SRA(reg),
                6 => Instruction::SWAP(reg),
                7 => Instruction::SRL(reg),
                _ => unreachable!()
            }
        },
        1 => {
            Instruction::BIT(1 << y, reg)
        },
        2 => Instruction::RES(1 << y, reg),
        3 => Instruction::SET(1 << y, reg),
        _ => unreachable!()
    };

    (cycle, ins)
}
