use std::hint::unreachable_unchecked;

pub struct GBTimer {
    div: u16,
    timer: u8,
    timer_modulo: u8,
    timer_control: u8,
}

impl GBTimer {
    pub fn new() -> GBTimer {
        GBTimer {
            div: 0,
            timer: 0,
            timer_modulo: 0,
            timer_control: 0,
        }
    }

    pub fn tick(&mut self) -> u8 {
        let mut interrupt = 0;

        let prev_div = self.div;
        self.div = self.div.wrapping_add(1);

        // Calculate timer
        let timer_prev = prev_div & self.get_timer_clock_mode() != 0;
        let timer_now = self.div & self.get_timer_clock_mode() != 0;
        let timer_signal_prev = self.is_timer_enabled() && timer_prev;
        let timer_signal_now = self.is_timer_enabled() && timer_now;
        if timer_signal_prev && !timer_signal_now {
            let (value, carry) = self.timer.overflowing_add(1);
            self.timer = value;
            if carry {
                self.timer = self.timer_modulo;
                interrupt |= 4;
            }
        }

        interrupt
    }

    pub fn read(&mut self, addr: u16) -> u8 {
        match addr {
            0xFF04 => (self.div >> 8) as u8,
            0xFF05 => self.timer,
            0xFF06 => self.timer_modulo,
            0xFF07 => self.timer_control,
            _ => {
                unreachable!();
            }
        }
    }

    pub fn write(&mut self, addr: u16, data: u8) {
        match addr {
            0xFF04 => self.div = 0,
            0xFF05 => self.timer = data,
            0xFF06 => self.timer_modulo = data,
            0xFF07 => self.timer_control = data & 7,
            _ => {
                unreachable!();
            }
        }
    }

    fn get_timer_clock_mode(&self) -> u16 {
        match self.timer_control & 3 {
            0 => 1 << 9,
            1 => 1 << 3,
            2 => 1 << 5,
            3 => 1 << 7,
            _ => {
                unreachable!();
            }
        }
    }

    fn is_timer_enabled(&self) -> bool {
        self.timer_control & 4 != 0
    }
}
