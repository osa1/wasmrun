#![allow(unused)]
#![allow(clippy::too_many_arguments)]

use libwasmrun::{exec, syntax, HostFunDecl, Mem, MemAddr, Runtime, ValueType};

use std::rc::Rc;

use pixels::{Pixels, SurfaceTexture};
use winit::dpi::LogicalSize;
use winit::event::{Event, VirtualKeyCode};
use winit::event_loop::{ControlFlow, EventLoopBuilder};
use winit::window::WindowBuilder;
use winit_input_helper::WinitInputHelper;

const SCALE: f64 = 4.0;

fn main() {
    let module_path = std::env::args()
        .nth(1)
        .unwrap_or_else(|| panic!("USAGE: w4wr <program>"));

    let module = syntax::deserialize_file(module_path).unwrap();

    let mut rt = Runtime::new();

    let mem_addr = rt.allocate_mem(Mem::new(1, Some(1)));

    let w4state = W4State { mem: mem_addr };

    let _env_module_addr = rt.allocate_host_module(
        "env".to_string(),
        vec![
            (
                "tone".to_string(),
                HostFunDecl {
                    arg_tys: vec![
                        ValueType::I32, // frequency
                        ValueType::I32, // duration
                        ValueType::I32, // volume
                        ValueType::I32, // flags
                    ],
                    ret_tys: vec![],
                    fun: Rc::new(move |rt, _mem_addr| {
                        let flags = rt.pop_i32()?;
                        let volume = rt.pop_i32()?;
                        let duration = rt.pop_i32()?;
                        let frequency = rt.pop_i32()?;
                        w4state.tone(frequency, duration, volume, flags, rt);
                        Ok(vec![])
                    }),
                },
            ),
            (
                "blit".to_string(),
                HostFunDecl {
                    arg_tys: vec![
                        ValueType::I32, // spritePtr
                        ValueType::I32, // x
                        ValueType::I32, // y
                        ValueType::I32, // width
                        ValueType::I32, // height
                        ValueType::I32, // flags
                    ],
                    ret_tys: vec![],
                    fun: Rc::new(move |rt, _mem_addr| {
                        let flags = rt.pop_i32()?;
                        let height = rt.pop_i32()?;
                        let width = rt.pop_i32()?;
                        let y = rt.pop_i32()?;
                        let x = rt.pop_i32()?;
                        let sprite_ptr = rt.pop_i32()?;
                        w4state.blit(flags, height, width, y, x, sprite_ptr, rt);
                        Ok(vec![])
                    }),
                },
            ),
            (
                "textUtf16".to_string(),
                HostFunDecl {
                    arg_tys: vec![
                        ValueType::I32, // str
                        ValueType::I32, // byteLength
                        ValueType::I32, // x
                        ValueType::I32, // y
                    ],
                    ret_tys: vec![],
                    fun: Rc::new(move |rt, _mem_addr| {
                        let y = rt.pop_i32()?;
                        let x = rt.pop_i32()?;
                        let byte_length = rt.pop_i32()?;
                        let str = rt.pop_i32()?;
                        w4state.text_utf16(str, byte_length, x, y, rt);
                        Ok(vec![])
                    }),
                },
            ),
            (
                "rect".to_string(),
                HostFunDecl {
                    arg_tys: vec![
                        ValueType::I32, // x
                        ValueType::I32, // y
                        ValueType::I32, // width
                        ValueType::I32, // height
                    ],
                    ret_tys: vec![],
                    fun: Rc::new(move |rt, _mem_addr| {
                        let height = rt.pop_i32()?;
                        let width = rt.pop_i32()?;
                        let y = rt.pop_i32()?;
                        let x = rt.pop_i32()?;
                        w4state.rect(x, y, width, height, rt);
                        Ok(vec![])
                    }),
                },
            ),
        ],
        vec![("memory".to_string(), mem_addr)],
    );

    let module_addr = exec::instantiate(&mut rt, module).unwrap();

    let start_fun_idx = rt.get_module(module_addr).get_start().unwrap();
    let update_fun_idx = rt
        .get_module(module_addr)
        .get_exported_fun_idx("update")
        .unwrap();

    //
    // Create window.
    //

    let event_loop = EventLoopBuilder::<()>::with_user_event().build();
    let event_loop_proxy = event_loop.create_proxy();

    std::thread::spawn(move || loop {
        std::thread::sleep(std::time::Duration::from_millis(16));
        if event_loop_proxy.send_event(()).is_err() {
            break;
        }
    });

    let mut input = WinitInputHelper::new();
    let screen_width = 160f64 * SCALE;
    let screen_height = 160f64 * SCALE;
    let window = {
        let size = LogicalSize::new(screen_width, screen_height);
        WindowBuilder::new()
            .with_title("w4wr")
            .with_inner_size(size)
            .with_min_inner_size(size)
            .build(&event_loop)
            .unwrap()
    };

    let mut pixels = {
        let window_size = window.inner_size();
        let surface_texture = SurfaceTexture::new(window_size.width, window_size.height, &window);
        Pixels::new(screen_width as u32, screen_height as u32, surface_texture).unwrap()
    };

    exec::invoke(&mut rt, module_addr, start_fun_idx).unwrap();
    exec::finish(&mut rt).unwrap();

    event_loop.run(move |event, _, control_flow| {
        if let Event::RedrawRequested(_) = event {
            w4state.draw(pixels.frame_mut(), &mut rt);
            if let Err(err) = pixels.render() {
                eprintln!("pixels.render error: {}", err);
                *control_flow = ControlFlow::Exit;
                return;
            }
        }

        if let Event::UserEvent(()) = event {
            println!("tick");
            exec::invoke(&mut rt, module_addr, update_fun_idx);
            exec::finish(&mut rt);
            window.request_redraw();
            return;
        }

        // Handle input events.
        if input.update(&event) {
            // Close events.
            if input.key_pressed(VirtualKeyCode::Escape) || input.close_requested() {
                *control_flow = ControlFlow::Exit;
                return;
            }

            // Resize the window.
            if let Some(size) = input.window_resized() {
                if let Err(err) = pixels.resize_surface(size.width, size.height) {
                    eprintln!("pixels.resize_surface error: {}", err);
                    *control_flow = ControlFlow::Exit;
                    return;
                }
            }

            // Update internal state and request a redraw.
            // world.update();
            window.request_redraw();
        }
    });

    // loop {
    //     exec::invoke(&mut rt, module_addr, update_fun_idx).unwrap();
    //     exec::finish(&mut rt).unwrap();
    // }
}

const PALETTE_ADDR: u32 = 0x4; // 16 bytes
const DRAW_COLORS_ADDR: u32 = 0x14; // 2 bytes
const GAMEPADS_ADDR: u32 = 0x16; // 4 bytes
const MOUSE_X_ADDR: u32 = 0x1A; // 2 bytes
const MOUSE_Y_ADDR: u32 = 0x1C; // 2 bytes
const MOUSE_BUTTONS_ADDR: u32 = 0x1e; // 1 byte
const SYSTEM_FLAGS_ADDR: u32 = 0x1f; // 1 byte
const NETPLAY_ADDR: u32 = 0x20; // 1 byte
const FRAMEBUFFER_ADDR: u32 = 0xA0; // 6,400 bytes
const PROGRAM_MEM_ADDR: u32 = 0x19A0; // 58,976 bytes

#[derive(Debug, Clone, Copy)]
struct W4State {
    mem: MemAddr,
}

impl W4State {
    /// Returns indices into palette for draw colors.
    fn get_draw_color_indices(&self, rt: &Runtime) -> (u8, u8, u8, u8) {
        let mem = rt.get_mem(self.mem);
        let bytes = mem.load_16_le(DRAW_COLORS_ADDR).unwrap();
        (
            (bytes & 0b00001111) as u8,
            ((bytes >> 4) & 0b00001111) as u8,
            ((bytes >> 8) & 0b00001111) as u8,
            ((bytes >> 12) & 0b00001111) as u8,
        )
    }

    /// Indexes palette. Returns (r, g, b) channels.
    fn index_palette(&self, idx: u8, rt: &Runtime) -> (u8, u8, u8) {
        let mem = rt.get_mem(self.mem);
        let idx = PALETTE_ADDR + (u32::from(idx) * 4);
        let palette = mem.load_32_le(idx).unwrap();
        (
            (palette & 0xFF) as u8,
            ((palette >> 8) & 0xFF) as u8,
            ((palette >> 16) & 0xFF) as u8,
        )
    }

    fn rect(&self, x: i32, y: i32, width: i32, height: i32, rt: &mut Runtime) {
        println!("rect");

        let x = x as u32;
        let y = y as u32;
        let width = width as u32;
        let height = height as u32;

        // Framebuffer is 160x160, contains 2 bits per pixel for colors 0-3.
        let buffer_start_bit = (y * 320) + (x * 2);
        let size_bits = (height * 320) + (width * 2);

        let whole_bytes_start = buffer_start_bit.div_ceil(8);
        let whole_bytes_end = (buffer_start_bit + size_bits) / 8;

        let mem = rt.get_mem_mut(self.mem);

        // Copy whole bytes.
        mem.fill(
            whole_bytes_start,
            whole_bytes_end - whole_bytes_start,
            0b01010101,
        )
        .unwrap();

        // Update bits of the first byte.
        let first_byte_idx = buffer_start_bit / 8;
        if first_byte_idx != whole_bytes_start {
            let old_byte = mem[first_byte_idx];

            // Bit index will always be 2, 4, or 6. Example:
            // Bit index 6 => ((old_byte & 0b00111111) | 0b01000000)
            const BIT_IDX_MASKS: [u8; 3] = [0b00000011, 0b00001111, 0b00111111];
            const FILLED_BIT_PATTERNS: [u8; 3] = [0b01010100, 0b01010000, 0b01000000];

            let bit_index = (((buffer_start_bit % 8) / 2) - 1) as usize;
            let new_byte = (old_byte & BIT_IDX_MASKS[bit_index]) | FILLED_BIT_PATTERNS[bit_index];

            mem[first_byte_idx] = new_byte;
        }

        // Update bits of the last byte.
        let last_byte_idx = (buffer_start_bit + size_bits).div_ceil(8);
        if last_byte_idx != whole_bytes_end {
            let old_byte = mem[last_byte_idx];

            // Similar to the first byte.
            // Bit index 6 => ((old_byte & 0b11000000) | 0b00010101)
            const BIT_IDX_MASKS: [u8; 3] = [0b11111100, 0b11110000, 0b11000000];
            const FILLED_BIT_PATTERNS: [u8; 3] = [0b00000001, 0b00000101, 0b00010101];

            let bit_index = ((((buffer_start_bit + size_bits) % 8) / 2) - 1) as usize;
            let new_byte = (old_byte & BIT_IDX_MASKS[bit_index]) | FILLED_BIT_PATTERNS[bit_index];
            mem[last_byte_idx] = new_byte;
        }
    }

    fn blit(
        &self,
        sprite_addr: i32,
        x: i32,
        y: i32,
        width: i32,
        height: i32,
        flags: i32,
        rt: &mut Runtime,
    ) {
        let format_2bpp = flags & 0b1 != 0;
        let flip_x = flags & 0b10 != 0;
        let flip_y = flags & 0b100 != 0;
        let rotate = flags & 0b1000 != 0;

        // TODO: Apply flags.

        println!("blit");
    }

    fn tone(&self, frequency: i32, duration: i32, volume: i32, flags: i32, rt: &mut Runtime) {
        println!("tone");
    }

    fn text_utf16(&self, str_addr: i32, byte_length: i32, x: i32, y: i32, rt: &mut Runtime) {
        println!("textUtf16");
    }

    fn draw(&self, frame: &mut [u8], rt: &mut Runtime) {
        let mem = rt.get_mem(self.mem);
    }
}
