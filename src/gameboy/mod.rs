mod cartridge;
mod memory_bus;
mod ppu;
mod sound;
mod z80;
mod joypad;
mod gb_timer;

use memory_bus::MemoryBus;
use z80::Z80;
use ppu::PPU;
use sdl2::{Sdl, EventPump};
use std::sync::atomic::Ordering;
use joypad::Joypad;
use std::rc::Rc;
use std::sync::Mutex;
use sdl2::keyboard::Keycode;
use std::ops::{Deref, DerefMut};
use std::thread::sleep;
use std::time::Duration;
use std::sync::mpsc::{SyncSender, Receiver};
use self::joypad::KeyState;
use crate::gameboy::z80::Reg;
use crate::gameboy::sound::Sound;

pub struct Gameboy {
    mem: MemoryBus,
    cpu: Z80,

    joypad_mutex: Rc<Mutex<KeyState>>,
}

impl Gameboy {
    pub fn new(sdl: &Sdl, rom_path: &str) -> Gameboy {
        let cart = cartridge::open(rom_path).unwrap();
        let bios = std::fs::read("res/bios.bin").unwrap_or_else(|_| {Vec::new()});
        let bios_len = bios.len();

        let ppu = PPU::new(sdl);
        let joypad = Joypad::new();
        let sound = Sound::new(sdl);
        let joypad_mutex = joypad.keys.clone();

        let mut mem = memory_bus::MemoryBus::new(cart, ppu, joypad, sound, bios);
        let mut cpu = Z80::new();
        mem.skip_bios(&mut cpu);

        Gameboy {
            mem,
            cpu,
            joypad_mutex,
        }
    }

    /// Run until the next vblank interrupt
    pub fn advance_frame(&mut self, event_pump: &EventPump) {
        {
            let keys: Vec<Keycode> = event_pump.keyboard_state()
                .pressed_scancodes()
                .filter_map(Keycode::from_scancode)
                .collect();
            let mut target = self.joypad_mutex.lock().unwrap();
            target.start = keys.contains(&Keycode::Return);
            target.select = keys.contains(&Keycode::LShift);
            target.a = keys.contains(&Keycode::Z);
            target.b = keys.contains(&Keycode::X);
            target.up = keys.contains(&Keycode::Up);
            target.down = keys.contains(&Keycode::Down);
            target.left = keys.contains(&Keycode::Left);
            target.right = keys.contains(&Keycode::Right);
        }

        loop {
            self.cpu.tick(&mut self.mem);
            if self.mem.ppu.reached_vblank {
                self.mem.ppu.reached_vblank = false;
                break;
            }
        }

        self.mem.ppu.show_frame(event_pump);
    }

    /// Run a single instruction
    pub fn advance_instruction(&mut self) {
        self.cpu.tick(&mut self.mem);
    }
}