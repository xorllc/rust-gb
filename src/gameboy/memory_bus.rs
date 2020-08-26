use super::cartridge::Cartridge;
use super::z80::Z80;
use super::ppu::PPU;
use super::joypad::Joypad;
use super::gb_timer::GBTimer;
use super::sound::Sound;
use std::num::Wrapping;
use std::sync::atomic::{AtomicBool, Ordering};

pub struct MemoryBus {
    pub flag_using_bios: bool,
    pub interrupt_enable: u8,
    pub interrupt_flag: u8,
    pub cartridge: Box<dyn Cartridge>,
    pub joypad: Joypad,
    pub timer: GBTimer,
    pub sound: Sound,
    pub ppu: PPU,
    pub hiram: [u8; 127],
    pub wram: [[u8; 4096]; 2],
    pub bios: Vec<u8>,
}

impl MemoryBus {
    pub fn new(cartridge: Box<dyn Cartridge>, ppu: PPU, joypad: Joypad, sound: Sound, bios: Vec<u8>) -> MemoryBus {
        let hiram = [0; 127];
        let wram = [[0; 4096]; 2];
        MemoryBus {
            flag_using_bios: true,
            cartridge,
            timer: GBTimer::new(),
            joypad,
            sound,
            ppu,
            bios,
            interrupt_enable: 0,
            interrupt_flag: 0,
            hiram,
            wram,
        }
    }

    pub fn skip_bios(&mut self, cpu: &mut Z80) {
        cpu.pc = 0x0100;
        cpu.sp = 0xFFFE;
        cpu.a = 0x01; cpu.f = 0xB0;
        cpu.b = 0x00; cpu.c = 0x13;
        cpu.d = 0x00; cpu.e = 0xD8;
        cpu.h = 0x01; cpu.l = 0x4D;
        const MEM_CONTENT: [u8; 62] = [
            0x05, 0x00, 0x06, 0x00, 0x07, 0x00, 0x10, 0xBF,
            0x12, 0xF3, 0x14, 0xBF, 0x16, 0x3F, 0x17, 0x00,
            0x19, 0xBF, 0x1A, 0x7F, 0x1A, 0x7F, 0x1B, 0xFF,
            0x1C, 0x9F, 0x1E, 0xBF, 0x20, 0xFF, 0x21, 0x00,
            0x22, 0x00, 0x23, 0xBF, 0x24, 0x77, 0x25, 0xF3,
            0x26, 0xF1, 0x40, 0x91, 0x42, 0x00, 0x43, 0x00,
            0x45, 0x00, 0x47, 0xFC, 0x48, 0xFF, 0x49, 0xFF,
            0x4A, 0x00, 0x4B, 0x00, 0xFF, 0x00
        ];
        for i in (0..62).step_by(2) {
            self.write_u8(0xFF00 | MEM_CONTENT[i] as u16, MEM_CONTENT[i+1]);
        }
        // Disable audio
        for i in 0xFF10..0xFF26 {
            self.write_u8(i, 0);
        }
        self.flag_using_bios = false;
    }

    pub fn tick(&mut self, cycles: i32) {
        for _ in 0..cycles {
            self.interrupt_flag |= self.timer.tick();
            self.interrupt_flag |= self.joypad.tick();
        }
        self.interrupt_flag |= self.ppu.tick(cycles);
        self.interrupt_flag |= self.sound.tick(cycles);
    }

    pub fn read_u8(&mut self, addr: u16) -> u8 {
        if self.flag_using_bios && addr < 0x0100 {
            return self.bios[addr as usize];
        }

        if addr < 0x8000 {
            self.cartridge.read_u8(addr)
        } else if addr < 0xA000 {
            self.ppu.vram[(addr - 0x8000) as usize]
        } else if addr < 0xC000 {
            self.cartridge.read_u8(addr)
        } else if addr < 0xD000 {
            self.wram[0][(addr - 0xC000) as usize]
        } else if addr < 0xE000 {
            self.wram[1][(addr - 0xD000) as usize]
        } else if addr < 0xFE00 {
            // Mirror WRAM
            self.read_u8(addr - (0xE000 - 0xC000))
        } else if addr < 0xFEA0 {
            self.ppu.oam[(addr - 0xFE00) as usize]
        } else if addr < 0xFF00 {
            // Invalid (but not open bus)
            0x00
        } else if addr < 0xFF80 {
            self.read_io(addr)
        } else if addr < 0xFFFF {
            self.hiram[(addr - 0xFF80) as usize]
        } else /* addr == 0xFFFF */ {
            self.interrupt_enable
        }
    }

    pub fn write_u8(&mut self, addr: u16, data: u8) {
        /*if addr == 0xff80 && data != self.read_u8(addr) {
            println!("Write: ff80={:02x}", data);
        }*/

        if addr < 0x8000 {
            self.cartridge.write_u8(addr, data);
        } else if addr < 0xA000 {
            self.ppu.vram[(addr - 0x8000) as usize] = data;
        } else if addr < 0xC000 {
            self.cartridge.write_u8(addr, data);
        } else if addr < 0xD000 {
            self.wram[0][(addr - 0xC000) as usize] = data;
        } else if addr < 0xE000 {
            self.wram[1][(addr - 0xD000) as usize] = data;
        } else if addr < 0xFE00 {
            // Mirror WRAM
            self.write_u8(addr - (0xE000 - 0xC000), data);
        } else if addr < 0xFEA0 {
            self.ppu.oam[(addr - 0xFE00) as usize] = data;
        } else if addr < 0xFF00 {
            // Invalid
        } else if addr < 0xFF80 {
            self.write_io(addr, data);
        } else if addr < 0xFFFF {
            self.hiram[(addr - 0xFF80) as usize] = data;
        } else /* addr == 0xFFFF */ {
            self.interrupt_enable = data;
            //println!("Interrupt enable: {:02x}", data);
        }
    }

    pub fn read_u16(&mut self, addr: u16) -> u16 {
        let b0 = self.read_u8(addr) as u16;
        let b1 = self.read_u8(addr + 1) as u16;
        (b1 << 8) | b0
    }

    pub fn write_u16(&mut self, addr: u16, data: u16) {
        let addr = Wrapping(addr);
        let b0 = (data & 0x00FF) as u8;
        let b1 = ((data & 0xFF00) >> 8) as u8;
        self.write_u8(addr.0, b0);
        self.write_u8((addr + Wrapping(1)).0, b1);
    }

    fn read_io(&mut self, addr: u16) -> u8 {
        match addr {
            0xFF00 => self.joypad.read(),
            0xFF01 => {
                // serial data
                0xff
            },
            0xFF02 => {
                // serial control
                0
            },
            0xFF04..=0xFF07 => self.timer.read(addr),
            0xFF0F => self.interrupt_flag,
            0xFF10..=0xFF3F => self.sound.read(addr),
            0xFF40..=0xFF4B => self.ppu.read(addr),
            0xFF4F => { /* GBC only */ 0xFF },
            0xFF70 => { /* GBC only */ 0xFF },
            0xFF7F => 0,
            x => {
                println!("IO: Read from {:#06x}", addr);
                0xFF
            }
        }
    }

    fn write_io(&mut self, addr: u16, data: u8) {
        match addr {
            0xFF00 => self.joypad.write(data),
            0xFF01 => {
                //print!("{}", data as char);
                // serial data
            },
            0xFF02 => {
                // serial control
            },
            0xFF04..=0xFF07 => self.timer.write(addr, data),
            0xFF0F => self.interrupt_flag = data,
            0xFF10..=0xFF3F => self.sound.write(addr, data),
            0xFF46 => {
                let base_addr = (data as u16) << 8;
                for i in 0..0xA0 {
                    let value = self.read_u8(base_addr + i);
                    self.write_u8(0xFE00 + i, value);
                }
            },
            0xFF40..=0xFF4B => self.ppu.write(addr, data),
            0xFF50 => self.flag_using_bios = false,
            0xFF7F => {},
            _ => {
                if addr != 0xFF25 {
                    //println!("IO: Write to {:#06x} = {:#04x}", addr, data);
                }
            }
        }
    }
}