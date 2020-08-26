use super::Cartridge;

pub struct ROM {
    _rom: [u8; 32768]
}

impl ROM {
    pub fn from_content(content: Vec<u8>) -> Box<dyn Cartridge> {
        if content.len() != 32768 {
            panic!("Size not match!");
        }

        let mut rom: [u8; 32768] = [0; 32768];
        for i in 0..32768 {
            rom[i] = content[i];
        }

        Box::new(ROM {
            _rom: rom
        })
    }
}

impl Cartridge for ROM {
    fn read_u8(&mut self, addr: u16) -> u8 {
        let addr = addr as usize;

        if addr < self._rom.len() {
            self._rom[addr]
        }
        else {
            0xFF
        }
    }

    fn write_u8(&mut self, addr: u16, data: u8) {
    }
}