use std::ops::Deref;
use std::rc::Rc;
use std::sync::Mutex;

#[derive(Debug)]
pub struct KeyState {
    pub up: bool,
    pub down: bool,
    pub left: bool,
    pub right: bool,
    pub start: bool,
    pub select: bool,
    pub a: bool,
    pub b: bool,
}

pub struct Joypad {
    pub keys: Rc<Mutex<KeyState>>,
    selector: u8,
    prev_keys: u8,
}

impl Joypad {
    pub fn new() -> Joypad {
        Joypad {
            keys: Rc::new(Mutex::new(KeyState {
                up: false,
                down: false,
                left: false,
                right: false,
                start: false,
                select: false,
                a: false,
                b: false,
            })),
            selector: 0xff,
            prev_keys: 0,
        }
    }

    pub fn tick(&self) -> u8 {
        let mut interrupt = 0;
        if (!self.read() & 0xF) & self.prev_keys != 0 {
            interrupt |= 16;
        }

        interrupt
    }

    pub fn read(&self) -> u8 {
        let mut val: u8 = 0;
        let keys = self.keys.lock().unwrap();
        if self.selector & 0x20 == 0 {
            //val |= 1 << 5;
            if keys.start {
                val |= 1 << 3
            }
            if keys.select {
                val |= 1 << 2
            }
            if keys.a {
                val |= 1 << 1
            }
            if keys.b {
                val |= 1
            }
        }
        if self.selector & 0x10 == 0 {
            //val |= 1 << 4;
            if keys.down {
                val |= 1 << 3
            }
            if keys.up {
                val |= 1 << 2
            }
            if keys.left {
                val |= 1 << 1
            }
            if keys.right {
                val |= 1
            }
        }

        !val
    }

    pub fn write(&mut self, data: u8) {
        self.selector = data;
    }
}
