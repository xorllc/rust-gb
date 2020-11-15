use super::Cartridge;
use super::SaveFile;
use std::path::Path;

pub struct MBC3 {
    flag_ram_enabled: bool,
    rom_bank: u16,
    ram_bank: u8,

    rom: Vec<u8>,
    ram: SaveFile,
}

impl MBC3 {
    pub fn from_content(content: Vec<u8>, path: &Path) -> Box<dyn Cartridge> {
        println!("Warning: MBC3 implementation is incomplete!");
        let ram_capacity = [0, 2, 8, 32, 128, 64][content[0x0149] as usize] * 1024;

        Box::new(MBC3 {
            flag_ram_enabled: false,
            rom_bank: 1,
            ram_bank: 0,

            rom: content,
            ram: SaveFile::from_rom_path(path, ram_capacity),
        })
    }
}

impl Cartridge for MBC3 {
    fn read_u8(&mut self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x3FFF => self.rom[addr as usize],
            0x4000..=0x7FFF => {
                let base_addr = self.rom_bank as usize * 0x4000;
                let offset = addr as usize - 0x4000;
                self.rom[base_addr + offset]
            }
            0xA000..=0xBFFF => {
                let base_addr = self.ram_bank as usize * 0x2000;
                let offset = addr as usize - 0xA000;
                self.ram.read(base_addr + offset)
            }
            _ => {
                println!("Unknown MBC3 read from {:04x}", addr);
                0xFF
            }
        }
    }

    fn write_u8(&mut self, addr: u16, data: u8) {
        match addr {
            0x0000..=0x1FFF => {
                self.flag_ram_enabled = (data & 0x0F) == 0x0A;
            }
            0x2000..=0x3FFF => {
                self.rom_bank = data as u16;
                if self.rom_bank == 0 {
                    self.rom_bank = 1;
                }
            }
            0x4000..=0x5FFF => {
                if data <= 3 {
                    self.ram_bank = data;
                }
            }
            0xA000..=0xBFFF => {
                let base_addr = self.ram_bank as usize * 0x2000;
                let offset = addr as usize - 0xA000;
                self.ram.write(base_addr + offset, data);
            }
            _ => {
                println!("Unknown MBC3 write to {:04x}={:02x}", addr, data);
            }
        }
    }
}
