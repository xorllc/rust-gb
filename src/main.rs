mod gameboy;

use std::thread::sleep;
use std::time::Duration;
use sdl2::event::Event;
use std::env;

fn main() {
    let sdl = sdl2::init().unwrap();
    let mut event_pump = sdl.event_pump().unwrap();

    let args = env::args().collect::<Vec<String>>();
    let file_path = match args.len() {
        1 => {
            let file_name = std::fs::read_dir(".").ok().and_then(|x| {
                x.filter(|x| {
                    x.as_ref().ok().and_then(|x| {
                        x.file_name().to_str().and_then(|x| {
                            Some(x.ends_with(".gb") || x.ends_with(".gbc"))
                        })
                    }).unwrap_or(false)
                }).next().and_then(|x| {
                    x.ok().and_then(|x| x.file_name().to_str().map(|x| x.to_owned()))
                })
            });
            match file_name {
                Some(x) => x,
                None => {
                    let rust_gb = "rust-gb".to_owned();
                    println!("Usage: {} [rom file]", args.get(0).unwrap_or(&rust_gb));
                    return;
                }
            }
        },
        2 => args[1].as_str().to_owned(),
        _ => {
            let rust_gb = "rust-gb".to_owned();
            println!("Usage: {} [rom file]", args.get(0).unwrap_or(&rust_gb));
            return;
        }
    };

    let mut gameboy = gameboy::Gameboy::new(&sdl, &file_path);
    'app: loop {
        for ev in event_pump.poll_iter() {
            match ev {
                Event::Quit {..} => {
                    break 'app;
                }
                _ => {}
            }
        }
        gameboy.advance_frame(&event_pump);
    }
}
