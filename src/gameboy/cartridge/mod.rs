mod rom;
mod mbc1;
mod mbc3;
mod save_file;

use save_file::SaveFile;
use std::{fs, io};
use std::path::PathBuf;

pub trait Cartridge {
    fn read_u8(&mut self, addr: u16) -> u8;
    fn write_u8(&mut self, addr: u16, data: u8);
}

pub fn open(path: &str) -> io::Result<Box<dyn Cartridge>> {
    let path = PathBuf::from(path);
    let content = fs::read(&path)?;
    validate(&content);

    let mut cartridge;

    match content[0x0147] {
        0x00 => {
            cartridge = rom::ROM::from_content(content)
        },
        0x01..=0x03 => {
            cartridge = mbc1::MBC1::from_content(content, &path);
        },
        0x0F..=0x13 => {
            cartridge = mbc3::MBC3::from_content(content, &path);
        }
        _ => {
            println!("Unknown mapper: {:02x}", content[0x0147]);
            cartridge = mbc3::MBC3::from_content(content, &path);
        }
    }

    Ok(cartridge)
}

fn validate(content: &Vec<u8>) -> bool {
    let mut title = Vec::new();
    for i in 0..11 {
        let ch = content[i + 0x0134];
        if ch == 0 { break }
        title.push(ch);
    }

    let title = String::from_utf8_lossy(&title);

    println!("== Start of ROM data validation ==");
    println!("Title: {}", title);

    if content[0x014A] == 0 {
        println!("Region: Japanese");
    }
    else {
        println!("Region: Oversea");
    }

    println!("Mask ROM revision: {}", content[0x014C]);

    let mut header_checksum: u8 = 0;
    for i in 0x0134..0x014D {
        header_checksum = header_checksum.wrapping_sub(content[i]).wrapping_sub(1);
    }

    if header_checksum != content[0x014D] {
        println!("Header checksum failed!");
        return false;
    }

    let mut rom_checksum: u16 = 0;
    for i in 0..content.len() {
        if i == 0x14E || i == 0x14F { continue }
        rom_checksum = rom_checksum.wrapping_add(content[i] as u16);
    }

    if rom_checksum != (content[0x14E] as u16) << 8 | (content[0x14F] as u16) {
        println!("File checksum failed! (A real gameboy would not care)");
    }

    println!("== ROM data validation successful ==");

    true
}
