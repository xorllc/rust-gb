use sdl2::{Sdl, VideoSubsystem, EventPump};
use sdl2::video::Window;
use sdl2::rect::Rect;
use sdl2::surface::Surface;
use sdl2::pixels::PixelFormatEnum;
use sdl2::render::{Canvas, WindowCanvas};
use super::memory_bus::MemoryBus;
use std::time::Instant;
use std::sync::atomic::{AtomicBool, Ordering};

#[repr(C, packed(1))]
#[derive(Copy, Clone, Debug)]
pub struct Pixel {
    r: u8,
    g: u8,
    b: u8,
}

#[derive(Copy, Clone, Debug)]
enum PPUMode {
    ScanOAM = 2,
    ReadVRAM = 3,
    HBlank = 0,
    VBlank = 1,
}

impl PartialEq for PPUMode {
    fn eq(&self, other: &Self) -> bool {
        *self as i32 == *other as i32
    }
}

#[derive(Copy, Clone, Debug)]
struct ObjectProp {
    y: u8,
    x: u8,
    tile: u8,
    flags: u8,
}

impl ObjectProp {
    fn flag_priority(&self) -> bool {
        self.flags & 0x80 != 0
    }
    fn flag_flip_y(&self) -> bool {
        self.flags & 0x40 != 0
    }
    fn flag_flip_x(&self) -> bool {
        self.flags & 0x20 != 0
    }
    fn flag_palette(&self) -> bool {
        self.flags & 0x10 != 0
    }
}

pub struct PPU {
    video: VideoSubsystem,
    window: Window,
    scan_x: i32,
    ppu_mode: PPUMode,
    flag_lyc_interrupt: bool,
    flag_oam_interrupt: bool,
    flag_vblank_interrupt: bool,
    flag_hblank_interrupt: bool,
    holding_lcd_interrupt: bool,
    bgp: u8,
    obp0: u8,
    obp1: u8,
    ppu_control: u8,
    lyc: u8,
    scanline: u8,
    scy: u8, scx: u8,
    wy: u8, wx: u8,
    pub reached_vblank: bool,
    pub oam: [u8; 160],
    pub vram: [u8; 8192],
    fb: [[Pixel; 160]; 144],
}

impl PPU {
    pub fn new(sdl: &Sdl) -> PPU {
        let video = sdl.video().unwrap();
        let window = video.window("A Rust Gameboy Emulator", 160*2, 144*2)
            .allow_highdpi().build().unwrap();

        PPU {
            video,
            window,
            ppu_mode: PPUMode::ReadVRAM,
            scanline: 0,
            scan_x: 0,
            flag_lyc_interrupt: false,
            flag_oam_interrupt: false,
            flag_hblank_interrupt: false,
            flag_vblank_interrupt: false,
            holding_lcd_interrupt: false,
            bgp: 0,
            obp0: 0,
            obp1: 0,
            ppu_control: 0,
            lyc: 0,
            scy: 0, scx: 0,
            wy: 0, wx: 0,
            reached_vblank: false,
            oam: [0; 160],
            vram: [0; 8192],
            fb: [[Pixel {r: 0, g: 0, b: 0}; 160]; 144]
        }
    }

    fn calculate_mode(&self) -> PPUMode {
        if self.scanline < 144 {
            if self.scan_x < 80 { PPUMode::ScanOAM }
            else if self.scan_x < 169 { PPUMode::ReadVRAM }
            else { PPUMode::HBlank }
        }
        else { PPUMode::VBlank }
    }

    pub fn tick(&mut self, cycles: i32) -> u8 {
        // Too many cycles; split
        if cycles > 40 {
            let batch_cnt = cycles / 40;
            let remainder = cycles % 40;
            let mut interrupt = 0;
            for _ in 0..batch_cnt {
                interrupt |= self.tick(40);
            }
            interrupt |= self.tick(remainder);
            return interrupt;
        }

        if self.ppu_control & 0x80 == 0 {
            self.scanline = 0;
            self.scan_x = 0;
            return 0;
        }

        self.scan_x += cycles;
        while self.scan_x >= 456 {
            self.scan_x -= 456;
            self.scanline += 1;
            if self.scanline == 154 {
                self.scanline = 0;
            }
        }

        let mut interrupt = 0;

        let new_mode = self.calculate_mode();
        if new_mode != self.ppu_mode {
            match new_mode {
                PPUMode::ScanOAM => {},
                PPUMode::ReadVRAM => {
                    if self.reached_vblank {
                        panic!("VBlank was ignored by frontend!");
                    }
                    self.draw_line();
                },
                PPUMode::HBlank => {},
                PPUMode::VBlank => {
                    interrupt |= 1;
                    self.reached_vblank = true;
                },
            }
            self.ppu_mode = new_mode;
        }

        let mut lcd_stat_int = false;
        match self.ppu_mode {
            PPUMode::ScanOAM => {
                if self.flag_oam_interrupt || (self.flag_lyc_interrupt && self.scanline == self.lyc) {
                    lcd_stat_int = true;
                }
            },
            PPUMode::ReadVRAM => {}
            PPUMode::HBlank => {
                if self.flag_hblank_interrupt {
                    lcd_stat_int = true;
                }
            },
            PPUMode::VBlank => {
                if self.flag_vblank_interrupt {
                    lcd_stat_int = true;
                }
            }
        }
        if lcd_stat_int && !self.holding_lcd_interrupt {
            interrupt |= 2;
        }
        self.holding_lcd_interrupt = lcd_stat_int;

        interrupt
    }

    pub fn show_frame(&mut self, event_pump: &EventPump) {
        let mut data = unsafe {
            let arr: [u8; 3*160*144] = std::mem::transmute(self.fb);
            arr
        };

        let mut win = self.window.surface(&event_pump).unwrap();
        let fb = Surface::from_data(&mut data, 160, 144, 160*3,
                                    PixelFormatEnum::RGB24).unwrap();

        let src = Rect::new(0, 0, 160, 144);
        let dst = Rect::new(0, 0, 160*2, 144*2);
        fb.blit_scaled(src, &mut win, dst).unwrap();

        win.finish().unwrap();

        // Clear fb
        for i in 0..144 {
            for j in 0..160 {
                self.fb[i][j] = Self::shade2pix(0);
            }
        }
    }

    pub fn read(&mut self, addr: u16) -> u8 {
        match addr {
            0xFF40 => self.ppu_control,
            0xFF41 => self.read_stat(),
            0xFF42 => self.scy,
            0xFF43 => self.scx,
            0xFF44 => self.scanline,
            0xFF45 => self.lyc,
            0xFF47 => self.bgp,
            0xFF48 => self.obp0,
            0xFF49 => self.obp1,
            0xFF4A => self.wy,
            0xFF4B => self.wx,
            _ => {
                println!("Unknown PPU read: {:04x}", addr);
                0xFF
            }
        }
    }

    pub fn write(&mut self, addr: u16, data: u8) {
        match addr {
            0xFF40 => self.ppu_control = data,
            0xFF41 => self.write_stat(data),
            0xFF42 => self.scy = data,
            0xFF43 => self.scx = data,
            0xFF44 => {},
            0xFF45 => self.lyc = data,
            0xFF47 => self.bgp = data,
            0xFF48 => self.obp0 = data,
            0xFF49 => self.obp1 = data,
            0xFF4A => self.wy = data,
            0xFF4B => self.wx = data,
            _ => {
                println!("Unknown PPU write: {:04x}={:02x}", addr, data);
            }
        }
    }

    fn read_stat(&self) -> u8 {
        let mut value: u8 = 0;
        if self.flag_lyc_interrupt { value |= 1 << 6 }
        if self.flag_oam_interrupt { value |= 1 << 5 }
        if self.flag_vblank_interrupt { value |= 1 << 4 }
        if self.flag_hblank_interrupt { value |= 1 << 3 }
        if self.lyc == self.scanline { value |= 1 << 2 }
        value |= match self.ppu_mode {
            PPUMode::HBlank => 0,
            PPUMode::VBlank => 1,
            PPUMode::ScanOAM => 2,
            PPUMode::ReadVRAM => 3,
        };

        value
    }

    fn write_stat(&mut self, data: u8) {
        self.flag_lyc_interrupt = if data & (1 << 6) != 0 {true} else {false};
        self.flag_oam_interrupt = if data & (1 << 5) != 0 {true} else {false};
        self.flag_hblank_interrupt = if data & (1 << 4) != 0 {true} else {false};
        self.flag_vblank_interrupt = if data & (1 << 3) != 0 {true} else {false};
    }

    fn shade2pix(shade: u8) -> Pixel {
        const PIXEL_BLACK: Pixel = Pixel {r: 0x08, g: 0x18, b: 0x20};
        const PIXEL_DARK: Pixel = Pixel {r: 0x34, g: 0x68, b: 0x56};
        const PIXEL_LIGHT: Pixel = Pixel {r: 0x88, g: 0xc0, b: 0x70};
        const PIXEL_WHITE: Pixel = Pixel {r: 0xe0, g: 0xf8, b: 0xd0};
        match shade {
            0 => PIXEL_WHITE,
            1 => PIXEL_LIGHT,
            2 => PIXEL_DARK,
            3 => PIXEL_BLACK,
            _ => unreachable!()
        }
    }

    fn fetch_tile_line(&self, offset: usize, line: i32) -> [u8; 8] {
        if line < 0 || line > 7 {
            println!("Line overflow while fetching! (line={})", line);
            return [0; 8];
        }
        let mut tile: [u8; 8] = [0; 8];
        let line_offset = (line * 2) as usize;
        for i in 0..2 {
            let modifier = if i % 2 == 0 { 1 } else { 2 };
            let mut val = self.vram[offset + line_offset + i];
            for j in 0..8 {
                tile[j] |= if (val & 0x80) != 0 { modifier } else { 0 };
                val <<= 1;
            }
        }

        tile
    }

    fn fetch_tile_bg(&self, num: u8, line: i32) -> [u8; 8] {
        let mode = self.ppu_control & (1 << 4) != 0;
        let base = if mode {
            // $8000 Mode
            //(num as usize) * 16
            num as usize * 16
        } else {
            // $8800 Mode
            (0x1000 + (num as i8 as i32) * 16) as usize
        };

        self.fetch_tile_line(base, line)
    }

    fn fetch_bg(&self, y: i32, x: i32) -> u8 {
        let tile_y = y / 8;
        let tile_x = x / 8;
        debug_assert!(0 <= tile_y && tile_y < 32 && 0 <= tile_x && tile_x < 32);
        if self.ppu_control & (1 << 3) == 0 {
            self.vram[0x1800 + (tile_y as usize * 32) + tile_x as usize]
        }
        else {
            self.vram[0x1C00 + (tile_y as usize * 32) + tile_x as usize]
        }
    }

    fn fetch_win(&self, y: i32, x: i32) -> u8 {
        let tile_y = y / 8;
        let tile_x = x / 8;
        debug_assert!(0 <= tile_y && tile_y < 32 && 0 <= tile_x && tile_x < 32);
        if self.ppu_control & (1 << 6) == 0 {
            self.vram[0x1800 + (tile_y as usize * 32) + tile_x as usize]
        }
        else {
            self.vram[0x1C00 + (tile_y as usize * 32) + tile_x as usize]
        }
    }

    fn fetch_tile_obj(&self, num: u8, line: i32) -> [u8; 8] {
        let base = (num as usize) * 16;
        if self.ppu_control & (1 << 2) == 0 {
            self.fetch_tile_line(base, line)
        }
        else {
            if line < 8 {
                self.fetch_tile_line(base, line)
            }
            else {
                let tall_base = (num.wrapping_add(1) as usize) * 16;
                self.fetch_tile_line(tall_base, line - 8)
            }
        }
    }

    fn fetch_obj(&self, y: i32) -> Vec<ObjectProp> {
        let tall_obj = if self.ppu_control & (1 << 2) == 0 {false} else {true};
        let mut arr = vec![];
        for i in 0..40 {
            let base_addr = i * 4;
            let obj_y = self.oam[base_addr] as i32 - (if tall_obj {0} else {8});
            let top_y = self.oam[base_addr] as i32 - 16;
            if y < top_y || obj_y <= y {
                continue;
            }

            arr.push(ObjectProp {
                y: self.oam[base_addr],
                x: self.oam[base_addr + 1],
                tile: self.oam[base_addr + 2],
                flags: self.oam[base_addr + 3]
            });
        }

        arr.sort_by_key(|a| {a.x});
        // PPU renders at most 10 sprites
        if arr.len() > 10 {
            arr.truncate(10);
        }

        arr
    }

    fn draw_bg(&mut self, color_line: &mut [u8]) {
        if self.ppu_control & 1 != 0 {
            let y = self.scanline.wrapping_add(self.scy) as i32;
            for i in 0..160 {
                let x = self.scx.wrapping_add(i as u8) as i32;
                let tile_num = self.fetch_bg(y, x);
                let tile = self.fetch_tile_bg(tile_num, y % 8);
                let color = tile[x as usize % 8];
                color_line[i] = color;
                let shade = (self.bgp >> (color * 2)) & 3;
                self.fb[self.scanline as usize][i] = Self::shade2pix(shade);
            }
        }
    }

    fn draw_win(&mut self, color_line: &mut [u8]) {
        if self.ppu_control & (1 << 5) != 0 && self.ppu_control & 1 != 0 {
            let y = self.scanline as i32 - self.wy as i32;
            if 0 <= y {
                let x_start = self.wx as usize - 7;
                for i in x_start..160 {
                    let x = i as i32 - (self.wx as i32 - 7);
                    let tile_num = self.fetch_win(y, x);
                    let tile = self.fetch_tile_bg(tile_num, y % 8);
                    let color = tile[x as usize % 8];
                    color_line[i] = color;
                    let shade = (self.bgp >> (color * 2)) & 3;
                    self.fb[self.scanline as usize][i] = Self::shade2pix(shade);
                }
            }
        }
    }

    fn draw_obj(&mut self, color_line: &mut [u8]) {
        if self.ppu_control & 2 != 0 {
            let obj_height = if self.ppu_control & (1 << 2) == 0 {8} else {16};
            let objs = self.fetch_obj(self.scanline as i32);
            for i in 0..160 {
                for obj in objs.iter().rev() {
                    let mut tile_x = i as i32 - obj.x as i32 + 8;
                    if tile_x < 0 || 8 <= tile_x {
                        continue;
                    }
                    let mut line = 16 - (obj.y as i32 - self.scanline as i32);

                    if obj.flag_flip_x() {
                        tile_x = 7 - tile_x;
                    }
                    if obj.flag_flip_y() {
                        line = obj_height - line - 1;
                    }

                    let tile = self.fetch_tile_obj(obj.tile, line);
                    let color = tile[tile_x as usize];

                    let shade = if obj.flag_palette() {
                        (self.obp1 >> (color * 2)) & 3
                    } else {
                        (self.obp0 >> (color * 2)) & 3
                    };

                    if (!obj.flag_priority() && color != 0) || (obj.flag_priority() && color_line[i] == 0){
                        color_line[i] = color;
                        self.fb[self.scanline as usize][i] = Self::shade2pix(shade);
                    }
                }
            }
        }
    }

    fn draw_line(&mut self) {
        let mut color_line = [0; 160];
        self.draw_bg(&mut color_line);
        self.draw_win(&mut color_line);
        self.draw_obj(&mut color_line);
    }
}