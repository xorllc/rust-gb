use std::path::{Path, PathBuf};
use std::fs::{File, OpenOptions};
use std::io::{Seek, SeekFrom, Read, Write};
use std::sync::mpsc::Sender;
use std::time::Duration;
use memmap::{Mmap, MmapMut, MmapOptions};

pub struct SaveFile {
    path: PathBuf,
    capacity: usize,
    data: Option<MmapMut>
}

impl SaveFile {
    pub fn new<T: AsRef<Path>>(path: T, capacity: usize) -> SaveFile {
        if capacity == 0 {
            return SaveFile {
                path: path.as_ref().to_owned(),
                data: None,
                capacity
            }
        }
        let mut file = OpenOptions::new()
            .read(true).write(true).truncate(false).create(true)
            .open(&path).unwrap();

        file.set_len(capacity as u64).unwrap();

        let data = unsafe {
            Mmap::map(&file).unwrap().make_mut().unwrap()
        };

        SaveFile {
            path: path.as_ref().to_owned(),
            data: Some(data),
            capacity
        }
    }

    pub fn from_rom_path<T: AsRef<Path>>(path: T, capacity: usize) -> SaveFile {
        let mut path = path.as_ref().to_owned();
        let filename = path.file_name().unwrap().to_str().unwrap().to_owned();
        let mut iter = filename.rsplitn(2, '.');
        let ext = iter.next().unwrap();
        let name = match iter.next() {
            Some(x) => {
                let mut y = x.to_owned();
                y.push_str(".sav");
                y
            },
            None => {
                let mut y = ext.to_owned();
                y.push_str(".sav");
                y
            }
        };
        path.pop();
        path.push(name);

        Self::new(path, capacity)
    }

    pub fn read(&mut self, addr: usize) -> u8 {
        if addr >= self.capacity {
            return 0xFF;
        }
        if let Some(mmap) = &self.data {
            mmap[addr]
        } else {
            0xFF
        }
    }

    pub fn write(&mut self, addr: usize, data: u8) {
        if addr >= self.capacity {
            return;
        }
        if let Some(mmap) = &mut self.data {
            mmap[addr] = data;
        }
    }
}
