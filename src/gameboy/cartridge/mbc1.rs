use super::Cartridge;
use super::SaveFile;
use std::path::Path;

pub struct MBC1 {
    ram_enable: bool,
    rom_bank: i32,
    bank_selector: u8,
    banking_mode_switch: bool,

    rom: Vec<u8>,
    ram: SaveFile,
}

impl MBC1 {
    pub fn from_content(content: Vec<u8>, path: &Path) -> Box<dyn Cartridge> {
        let ram_capacity = [0, 2, 8, 32, 128, 64][content[0x0149] as usize] * 1024;

        Box::new(MBC1 {
            ram_enable: false,
            rom_bank: 1,
            bank_selector: 0,
            banking_mode_switch: false,

            rom: content,
            ram: SaveFile::from_rom_path(path, ram_capacity),
        })
    }
}

impl Cartridge for MBC1 {
    fn read_u8(&mut self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x3FFF => self.rom[addr as usize],
            0x4000..=0x9FFF => {
                let additional_bank = if !self.banking_mode_switch {
                    self.bank_selector as usize
                } else {
                    0
                };
                let rom_bank = self.rom_bank as usize | (additional_bank << 5);
                let base_addr = rom_bank as usize * 0x4000;
                let offset = addr as usize - 0x4000;
                self.rom[base_addr + offset]
            }
            0xA000..=0xBFFF => {
                let offset = addr as usize - 0xA000;
                if self.banking_mode_switch {
                    let base_addr = self.bank_selector as usize * 0x2000;
                    self.ram.read(base_addr + offset)
                } else {
                    self.ram.read(offset)
                }
            }
            _ => {
                println!("Unknown MBC1 read from {:04x}", addr);
                0xFF
            }
        }
    }

    fn write_u8(&mut self, addr: u16, data: u8) {
        match addr {
            0x0000..=0x1FFF => {
                self.ram_enable = data & 0xF == 0xA;
            }
            0x2000..=0x3FFF => {
                self.rom_bank = (data & 0x1F) as i32;
                if self.rom_bank == 0 {
                    self.rom_bank += 1;
                }
            }
            0x4000..=0x5FFF => {
                self.bank_selector = data;
            }
            0x6000..=0x7FFF => {
                self.banking_mode_switch = (data & 1) != 0;
            }
            0xA000..=0xBFFF => {
                if self.ram_enable {
                    let offset = addr as usize - 0xA000;
                    if self.banking_mode_switch {
                        let base_addr = self.bank_selector as usize * 0x2000;
                        self.ram.write(base_addr + offset, data);
                    } else {
                        self.ram.write(offset, data);
                    }
                }
            }
            _ => {
                println!("Unknown MBC1 write to {:04x}={:02x}", addr, data);
            }
        }
    }
}
