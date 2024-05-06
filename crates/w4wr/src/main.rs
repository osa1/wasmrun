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

    // Two pages instead of one to allow font data. See below.
    let mem_addr = rt.allocate_mem(Mem::new(2, Some(2)));

    // Initialize palette and draw colors. Reference:
    // https://github.com/aduros/wasm4/blob/0dff7ad4e6c7b28b87a6555bea8574e5aa748e27/runtimes/native/src/runtime.c#L50-L55
    //
    // We also write the font data to the main memory, after the first 64 KiB, to be able to allow
    // blitting from font data to the framebuffer.
    //
    // TODO: This means a buggy program (that uses more than 64 KiB of Wasm memory) can corrupt the
    // font data. Is there a better way to implement this?
    {
        let mem = rt.get_mem_mut(mem_addr);
        mem.store_32_le(PALETTE_ADDR, 0xe0f8cf).unwrap();
        mem.store_32_le(PALETTE_ADDR + 4, 0x86c06c).unwrap();
        mem.store_32_le(PALETTE_ADDR + 8, 0x306850).unwrap();
        mem.store_32_le(PALETTE_ADDR + 12, 0x071821).unwrap();

        mem.store_8(DRAW_COLORS_ADDR, 0x03).unwrap();
        mem.store_8(DRAW_COLORS_ADDR + 1, 0x03).unwrap();

        mem.range_mut(FONT_ADDR, FONT_ADDR + FONT.len() as u32)
            .copy_from_slice(&FONT);
    }

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
                        let frequency = rt.get_local(0)?.expect_i32();
                        let duration = rt.get_local(1)?.expect_i32();
                        let volume = rt.get_local(2)?.expect_i32();
                        let flags = rt.get_local(3)?.expect_i32();
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
                        let sprite_ptr = rt.get_local(0)?.expect_i32();
                        let x = rt.get_local(1)?.expect_i32();
                        let y = rt.get_local(2)?.expect_i32();
                        let width = rt.get_local(3)?.expect_i32();
                        let height = rt.get_local(4)?.expect_i32();
                        let flags = rt.get_local(5)?.expect_i32();
                        w4state.blit(sprite_ptr, x, y, width, height, flags, rt);
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
                        let str = rt.get_local(0)?.expect_i32();
                        let byte_length = rt.get_local(1)?.expect_i32();
                        let x = rt.get_local(2)?.expect_i32();
                        let y = rt.get_local(3)?.expect_i32();
                        w4state.text_utf16(str, byte_length, x, y, rt);
                        Ok(vec![])
                    }),
                },
            ),
            (
                "text".to_string(),
                HostFunDecl {
                    arg_tys: vec![
                        ValueType::I32, // str
                        ValueType::I32, // x
                        ValueType::I32, // y
                    ],
                    ret_tys: vec![],
                    fun: Rc::new(move |rt, _mem_addr| {
                        let str = rt.get_local(0)?.expect_i32();
                        let x = rt.get_local(1)?.expect_i32();
                        let y = rt.get_local(2)?.expect_i32();
                        w4state.text(str, x, y, rt);
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
                        let x = rt.get_local(0)?.expect_i32();
                        let y = rt.get_local(1)?.expect_i32();
                        let width = rt.get_local(2)?.expect_i32();
                        let height = rt.get_local(3)?.expect_i32();
                        w4state.rect(x, y, width, height, rt);
                        Ok(vec![])
                    }),
                },
            ),
        ],
        vec![("memory".to_string(), mem_addr)],
    );

    let module_addr = exec::instantiate(&mut rt, module).unwrap();

    let start_fun_idx = rt.get_module(module_addr).get_start();
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
        Pixels::new(160, 160, surface_texture).unwrap()
    };

    if let Some(start_fun_idx) = start_fun_idx {
        exec::invoke(&mut rt, module_addr, start_fun_idx).unwrap();
        exec::finish(&mut rt).unwrap();
    }

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
            exec::invoke(&mut rt, module_addr, update_fun_idx).unwrap();
            exec::finish(&mut rt).unwrap();
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
            window.request_redraw();
        }
    });
}

const PALETTE_ADDR: u32 = 0x4; // 16 bytes
const DRAW_COLORS_ADDR: u32 = 0x14; // 2 bytes
const _GAMEPADS_ADDR: u32 = 0x16; // 4 bytes
const _MOUSE_X_ADDR: u32 = 0x1A; // 2 bytes
const _MOUSE_Y_ADDR: u32 = 0x1C; // 2 bytes
const _MOUSE_BUTTONS_ADDR: u32 = 0x1e; // 1 byte
const _SYSTEM_FLAGS_ADDR: u32 = 0x1f; // 1 byte
const _NETPLAY_ADDR: u32 = 0x20; // 1 byte
const FRAMEBUFFER_ADDR: u32 = 0xA0; // 6,400 bytes
const _PROGRAM_MEM_ADDR: u32 = 0x19A0; // 58,976 bytes
const FONT_ADDR: u32 = 65536; // Starts from the second page, right after the WASM-4 memory.

#[derive(Debug, Clone, Copy)]
struct W4State {
    mem: MemAddr,
}

impl W4State {
    /// Returns indices into palette for draw colors.
    fn _get_draw_color_indices(&self, rt: &Runtime) -> (u8, u8, u8, u8) {
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
    fn _index_palette(&self, idx: u8, rt: &Runtime) -> (u8, u8, u8) {
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

        let sprite_addr = sprite_addr as u32;
        let x = x as u32;
        let y = y as u32;
        let width = width as u32;
        let height = height as u32;

        if format_2bpp {
            self.blit_2bpp(sprite_addr, x, y, width, height, flip_x, flip_y, rotate, rt);
        } else {
            self.blit_1bpp(
                sprite_addr,
                x,
                y,
                width,
                height,
                0,     // sprite_x
                0,     // sprite_y
                width, // stride
                flip_x,
                flip_y,
                rotate,
                rt,
            );
        }
    }

    fn blit_1bpp(
        &self,
        sprite_addr: u32,
        framebuffer_x: u32,
        framebuffer_y: u32,
        width: u32,
        height: u32,
        sprite_x0: u32,
        sprite_y0: u32,
        stride: u32,
        _flip_x: bool,
        _flip_y: bool,
        _rotate: bool,
        rt: &mut Runtime,
    ) {
        let mut sprite_x = sprite_x0;
        let mut sprite_y = sprite_y0;

        let width = std::cmp::min(width, stride);

        let mem = rt.get_mem_mut(self.mem);

        loop {
            let sprite_bit_offset = (sprite_y * stride) + sprite_x;
            let sprite_byte_offset = sprite_bit_offset / 8;
            let sprite_bit_index = sprite_bit_offset % 8;
            let sprite_byte = mem[sprite_addr + sprite_byte_offset];

            // Sprite bit 1 = color 1, 0 = color 0 (background)
            let sprite_bit = (sprite_byte & (1 << sprite_bit_index)) == 0;

            let framebuffer_pos = ((framebuffer_y + (sprite_y - sprite_y0)) * 160)
                + (framebuffer_x + (sprite_x - sprite_x0));
            let framebuffer_byte_offset = FRAMEBUFFER_ADDR + ((framebuffer_pos * 2) / 8);
            let framebuffer_bit_offset = (framebuffer_pos % 4) * 2;
            let framebuffer_byte = mem[framebuffer_byte_offset];

            let framebuffer_new_byte = (framebuffer_byte & !(0b11 << framebuffer_bit_offset))
                | (u8::from(sprite_bit) << framebuffer_bit_offset);

            mem[framebuffer_byte_offset] = framebuffer_new_byte;

            sprite_x += 1;
            if sprite_x - sprite_x0 == width {
                sprite_x = sprite_x0;
                sprite_y += 1;
                if sprite_y - sprite_y0 == height {
                    break;
                }
            }
        }
    }

    fn blit_2bpp(
        &self,
        _sprite_addr: u32,
        _x: u32,
        _y: u32,
        _width: u32,
        _height: u32,
        _flip_x: bool,
        _flip_y: bool,
        _rotate: bool,
        _rt: &mut Runtime,
    ) {
        println!("NOT IMPLEMENTED: blit_2bpp");
    }

    fn tone(&self, _frequency: i32, _duration: i32, _volume: i32, _flags: i32, _rt: &mut Runtime) {
        println!("NOT IMPLEMENTED: tone");
    }

    fn text_utf16(&self, str_addr: i32, byte_length: i32, x0: i32, y0: i32, rt: &mut Runtime) {
        let mut str_addr = str_addr as u32;
        let mut byte_length = byte_length as u32;
        let x0 = x0 as u32;
        let y0 = y0 as u32;

        let mut x = x0;
        let mut y = y0;

        while byte_length != 0 {
            let char_code_point = rt.get_mem(self.mem).load_16_le(str_addr).unwrap();
            let char = char::decode_utf16([char_code_point])
                .next()
                .unwrap()
                .unwrap();

            if char == '\n' {
                x = x0;
                y += 8;
            } else if char as u32 >= 32 {
                self.blit_1bpp(
                    FONT_ADDR,                // sprite_addr
                    x,                        // framebuffer_x
                    y,                        // framebuffer_y
                    8,                        // width
                    8,                        // height
                    0,                        // sprite_x
                    ((char as u32) - 32) * 8, // sprite_y
                    8,                        // stride
                    false,                    // flip_x
                    false,                    // flip_y
                    false,                    // rotate
                    rt,
                );
                x += 8;
            } else {
                x += 8;
            }
            str_addr += 2;
            byte_length -= 2;
        }
    }

    fn text(&self, str_addr: i32, x0: i32, y0: i32, rt: &mut Runtime) {
        let mut str_addr = str_addr as u32;
        let x0 = x0 as u32;
        let y0 = y0 as u32;

        let mut char = rt.get_mem(self.mem)[str_addr];

        let mut x = x0;
        let mut y = y0;

        while char != 0 {
            if char == b'\n' {
                x = x0;
                y += 8;
            } else if char >= 32 {
                self.blit_1bpp(
                    FONT_ADDR,                // sprite_addr
                    x,                        // framebuffer_x
                    y,                        // framebuffer_y
                    8,                        // width
                    8,                        // height
                    0,                        // sprite_x
                    u32::from(char - 32) * 8, // sprite_y
                    8,                        // stride
                    false,                    // flip_x
                    false,                    // flip_y
                    false,                    // rotate
                    rt,
                );
                x += 8;
            } else {
                x += 8;
            }
            str_addr += 1;
            char = rt.get_mem(self.mem)[str_addr];
        }
    }

    fn draw(&self, frame: &mut [u8], rt: &mut Runtime) {
        let mem = rt.get_mem(self.mem);

        let mut frame_offset = 0;
        for frame_byte in mem.range(FRAMEBUFFER_ADDR, FRAMEBUFFER_ADDR + 6400) {
            let color = frame_byte & 0b11;
            let (r, g, b) = self.get_palette_color(color, rt);

            frame[frame_offset] = r;
            frame[frame_offset + 1] = g;
            frame[frame_offset + 2] = b;
            frame[frame_offset + 3] = 0xFF;
            frame_offset += 4;

            let color = (frame_byte & 0b1100) >> 2;
            let (r, g, b) = self.get_palette_color(color, rt);

            frame[frame_offset] = r;
            frame[frame_offset + 1] = g;
            frame[frame_offset + 2] = b;
            frame[frame_offset + 3] = 0xFF;
            frame_offset += 4;

            let color = (frame_byte & 0b110000) >> 4;
            let (r, g, b) = self.get_palette_color(color, rt);

            frame[frame_offset] = r;
            frame[frame_offset + 1] = g;
            frame[frame_offset + 2] = b;
            frame[frame_offset + 3] = 0xFF;
            frame_offset += 4;

            let color = (frame_byte & 0b11000000) >> 6;
            let (r, g, b) = self.get_palette_color(color, rt);

            frame[frame_offset] = r;
            frame[frame_offset + 1] = g;
            frame[frame_offset + 2] = b;
            frame[frame_offset + 3] = 0xFF;
            frame_offset += 4;
        }

        assert_eq!(frame_offset, frame.len());
    }

    /// Get (r, g, b) of `color` from the palette.
    fn get_palette_color(&self, color: u8, rt: &Runtime) -> (u8, u8, u8) {
        let mem = rt.get_mem(self.mem);
        let rgb = mem.load_32_le(PALETTE_ADDR + u32::from(color)).unwrap();
        (
            ((rgb & 0xFF0000) >> 16) as u8,
            ((rgb & 0xFF00) >> 8) as u8,
            (rgb & 0xFF) as u8,
        )
    }

    // For debugging.
    #[allow(unused)]
    fn dump_framebuffer(&self, rt: &Runtime) {
        use std::fmt::Write;

        let mem = rt.get_mem(self.mem);

        // 2 bits per pixel = 4 pixels per byte.
        for y in 0..160 {
            let mut buffer = String::with_capacity(160 * 4);
            for x in 0..(160 / 4) {
                let byte_offset = ((y * 160) / 4) + x;
                let byte = mem[FRAMEBUFFER_ADDR + byte_offset];
                let byte = byte.swap_bytes();
                write!(&mut buffer, "{:08b}", byte).unwrap();
            }
            println!("{}", buffer);
        }
    }
}

/// 8x8 ASCII sprites in 1bpp format.
///
/// First character is ' ' (SPACE). Blit from this using 8 pixels as stride, 0 as X, and
/// `(char code - 32) * 8` as Y.
///
/// Source:
/// https://github.com/aduros/wasm4/blob/0dff7ad4e6c7b28b87a6555bea8574e5aa748e27/runtimes/native/src/framebuffer.c#L8
#[allow(unused)]
const FONT: [u8; 1792] = [
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xc7, 0xc7, 0xc7, 0xcf, 0xcf, 0xff, 0xcf, 0xff,
    0x93, 0x93, 0x93, 0xff, 0xff, 0xff, 0xff, 0xff, 0x93, 0x01, 0x93, 0x93, 0x93, 0x01, 0x93, 0xff,
    0xef, 0x83, 0x2f, 0x83, 0xe9, 0x03, 0xef, 0xff, 0x9d, 0x5b, 0x37, 0xef, 0xd9, 0xb5, 0x73, 0xff,
    0x8f, 0x27, 0x27, 0x8f, 0x25, 0x33, 0x81, 0xff, 0xcf, 0xcf, 0xcf, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xf3, 0xe7, 0xcf, 0xcf, 0xcf, 0xe7, 0xf3, 0xff, 0x9f, 0xcf, 0xe7, 0xe7, 0xe7, 0xcf, 0x9f, 0xff,
    0xff, 0x93, 0xc7, 0x01, 0xc7, 0x93, 0xff, 0xff, 0xff, 0xe7, 0xe7, 0x81, 0xe7, 0xe7, 0xff, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xcf, 0xcf, 0x9f, 0xff, 0xff, 0xff, 0x81, 0xff, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xcf, 0xcf, 0xff, 0xfd, 0xfb, 0xf7, 0xef, 0xdf, 0xbf, 0x7f, 0xff,
    0xc7, 0xb3, 0x39, 0x39, 0x39, 0x9b, 0xc7, 0xff, 0xe7, 0xc7, 0xe7, 0xe7, 0xe7, 0xe7, 0x81, 0xff,
    0x83, 0x39, 0xf1, 0xc3, 0x87, 0x1f, 0x01, 0xff, 0x81, 0xf3, 0xe7, 0xc3, 0xf9, 0x39, 0x83, 0xff,
    0xe3, 0xc3, 0x93, 0x33, 0x01, 0xf3, 0xf3, 0xff, 0x03, 0x3f, 0x03, 0xf9, 0xf9, 0x39, 0x83, 0xff,
    0xc3, 0x9f, 0x3f, 0x03, 0x39, 0x39, 0x83, 0xff, 0x01, 0x39, 0xf3, 0xe7, 0xcf, 0xcf, 0xcf, 0xff,
    0x87, 0x3b, 0x1b, 0x87, 0x61, 0x79, 0x83, 0xff, 0x83, 0x39, 0x39, 0x81, 0xf9, 0xf3, 0x87, 0xff,
    0xff, 0xcf, 0xcf, 0xff, 0xcf, 0xcf, 0xff, 0xff, 0xff, 0xcf, 0xcf, 0xff, 0xcf, 0xcf, 0x9f, 0xff,
    0xf3, 0xe7, 0xcf, 0x9f, 0xcf, 0xe7, 0xf3, 0xff, 0xff, 0xff, 0x01, 0xff, 0x01, 0xff, 0xff, 0xff,
    0x9f, 0xcf, 0xe7, 0xf3, 0xe7, 0xcf, 0x9f, 0xff, 0x83, 0x01, 0x39, 0xf3, 0xc7, 0xff, 0xc7, 0xff,
    0x83, 0x7d, 0x45, 0x55, 0x41, 0x7f, 0x83, 0xff, 0xc7, 0x93, 0x39, 0x39, 0x01, 0x39, 0x39, 0xff,
    0x03, 0x39, 0x39, 0x03, 0x39, 0x39, 0x03, 0xff, 0xc3, 0x99, 0x3f, 0x3f, 0x3f, 0x99, 0xc3, 0xff,
    0x07, 0x33, 0x39, 0x39, 0x39, 0x33, 0x07, 0xff, 0x01, 0x3f, 0x3f, 0x03, 0x3f, 0x3f, 0x01, 0xff,
    0x01, 0x3f, 0x3f, 0x03, 0x3f, 0x3f, 0x3f, 0xff, 0xc1, 0x9f, 0x3f, 0x31, 0x39, 0x99, 0xc1, 0xff,
    0x39, 0x39, 0x39, 0x01, 0x39, 0x39, 0x39, 0xff, 0x81, 0xe7, 0xe7, 0xe7, 0xe7, 0xe7, 0x81, 0xff,
    0xf9, 0xf9, 0xf9, 0xf9, 0xf9, 0x39, 0x83, 0xff, 0x39, 0x33, 0x27, 0x0f, 0x07, 0x23, 0x31, 0xff,
    0x9f, 0x9f, 0x9f, 0x9f, 0x9f, 0x9f, 0x81, 0xff, 0x39, 0x11, 0x01, 0x01, 0x29, 0x39, 0x39, 0xff,
    0x39, 0x19, 0x09, 0x01, 0x21, 0x31, 0x39, 0xff, 0x83, 0x39, 0x39, 0x39, 0x39, 0x39, 0x83, 0xff,
    0x03, 0x39, 0x39, 0x39, 0x03, 0x3f, 0x3f, 0xff, 0x83, 0x39, 0x39, 0x39, 0x21, 0x33, 0x85, 0xff,
    0x03, 0x39, 0x39, 0x31, 0x07, 0x23, 0x31, 0xff, 0x87, 0x33, 0x3f, 0x83, 0xf9, 0x39, 0x83, 0xff,
    0x81, 0xe7, 0xe7, 0xe7, 0xe7, 0xe7, 0xe7, 0xff, 0x39, 0x39, 0x39, 0x39, 0x39, 0x39, 0x83, 0xff,
    0x39, 0x39, 0x39, 0x11, 0x83, 0xc7, 0xef, 0xff, 0x39, 0x39, 0x29, 0x01, 0x01, 0x11, 0x39, 0xff,
    0x39, 0x11, 0x83, 0xc7, 0x83, 0x11, 0x39, 0xff, 0x99, 0x99, 0x99, 0xc3, 0xe7, 0xe7, 0xe7, 0xff,
    0x01, 0xf1, 0xe3, 0xc7, 0x8f, 0x1f, 0x01, 0xff, 0xc3, 0xcf, 0xcf, 0xcf, 0xcf, 0xcf, 0xc3, 0xff,
    0x7f, 0xbf, 0xdf, 0xef, 0xf7, 0xfb, 0xfd, 0xff, 0x87, 0xe7, 0xe7, 0xe7, 0xe7, 0xe7, 0x87, 0xff,
    0xc7, 0x93, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x01,
    0xef, 0xf7, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x83, 0xf9, 0x81, 0x39, 0x81, 0xff,
    0x3f, 0x3f, 0x03, 0x39, 0x39, 0x39, 0x83, 0xff, 0xff, 0xff, 0x81, 0x3f, 0x3f, 0x3f, 0x81, 0xff,
    0xf9, 0xf9, 0x81, 0x39, 0x39, 0x39, 0x81, 0xff, 0xff, 0xff, 0x83, 0x39, 0x01, 0x3f, 0x83, 0xff,
    0xf1, 0xe7, 0x81, 0xe7, 0xe7, 0xe7, 0xe7, 0xff, 0xff, 0xff, 0x81, 0x39, 0x39, 0x81, 0xf9, 0x83,
    0x3f, 0x3f, 0x03, 0x39, 0x39, 0x39, 0x39, 0xff, 0xe7, 0xff, 0xc7, 0xe7, 0xe7, 0xe7, 0x81, 0xff,
    0xf3, 0xff, 0xe3, 0xf3, 0xf3, 0xf3, 0xf3, 0x87, 0x3f, 0x3f, 0x31, 0x03, 0x07, 0x23, 0x31, 0xff,
    0xc7, 0xe7, 0xe7, 0xe7, 0xe7, 0xe7, 0x81, 0xff, 0xff, 0xff, 0x03, 0x49, 0x49, 0x49, 0x49, 0xff,
    0xff, 0xff, 0x03, 0x39, 0x39, 0x39, 0x39, 0xff, 0xff, 0xff, 0x83, 0x39, 0x39, 0x39, 0x83, 0xff,
    0xff, 0xff, 0x03, 0x39, 0x39, 0x03, 0x3f, 0x3f, 0xff, 0xff, 0x81, 0x39, 0x39, 0x81, 0xf9, 0xf9,
    0xff, 0xff, 0x91, 0x8f, 0x9f, 0x9f, 0x9f, 0xff, 0xff, 0xff, 0x83, 0x3f, 0x83, 0xf9, 0x03, 0xff,
    0xe7, 0xe7, 0x81, 0xe7, 0xe7, 0xe7, 0xe7, 0xff, 0xff, 0xff, 0x39, 0x39, 0x39, 0x39, 0x81, 0xff,
    0xff, 0xff, 0x99, 0x99, 0x99, 0xc3, 0xe7, 0xff, 0xff, 0xff, 0x49, 0x49, 0x49, 0x49, 0x81, 0xff,
    0xff, 0xff, 0x39, 0x01, 0xc7, 0x01, 0x39, 0xff, 0xff, 0xff, 0x39, 0x39, 0x39, 0x81, 0xf9, 0x83,
    0xff, 0xff, 0x01, 0xe3, 0xc7, 0x8f, 0x01, 0xff, 0xf3, 0xe7, 0xe7, 0xcf, 0xe7, 0xe7, 0xf3, 0xff,
    0xe7, 0xe7, 0xe7, 0xe7, 0xe7, 0xe7, 0xe7, 0xff, 0x9f, 0xcf, 0xcf, 0xe7, 0xcf, 0xcf, 0x9f, 0xff,
    0xff, 0xff, 0x8f, 0x45, 0xe3, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x93, 0x93, 0xff,
    0x83, 0x29, 0x29, 0x11, 0x29, 0x29, 0x83, 0xff, 0x83, 0x39, 0x09, 0x11, 0x21, 0x39, 0x83, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    0x83, 0x11, 0x21, 0x7d, 0x21, 0x11, 0x83, 0xff, 0x83, 0x11, 0x09, 0x7d, 0x09, 0x11, 0x83, 0xff,
    0x83, 0x11, 0x39, 0x55, 0x11, 0x11, 0x83, 0xff, 0x83, 0x11, 0x11, 0x55, 0x39, 0x11, 0x83, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xe7, 0xff, 0xe7, 0xe7, 0xc7, 0xc7, 0xc7, 0xff,
    0xef, 0x83, 0x29, 0x2f, 0x29, 0x83, 0xef, 0xff, 0xc3, 0x99, 0x9f, 0x03, 0x9f, 0x9f, 0x01, 0xff,
    0xff, 0xa5, 0xdb, 0xdb, 0xdb, 0xa5, 0xff, 0xff, 0x99, 0x99, 0xc3, 0x81, 0xe7, 0x81, 0xe7, 0xff,
    0xe7, 0xe7, 0xe7, 0xff, 0xe7, 0xe7, 0xe7, 0xff, 0xc3, 0x99, 0x87, 0xdb, 0xe1, 0x99, 0xc3, 0xff,
    0x93, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xc3, 0xbd, 0x66, 0x5e, 0x5e, 0x66, 0xbd, 0xc3,
    0x87, 0xc3, 0x93, 0xc3, 0xff, 0xff, 0xff, 0xff, 0xff, 0xc9, 0x93, 0x27, 0x93, 0xc9, 0xff, 0xff,
    0xff, 0xff, 0x81, 0xf9, 0xf9, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xc3, 0xbd, 0x46, 0x5a, 0x46, 0x5a, 0xbd, 0xc3, 0x83, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xef, 0xd7, 0xef, 0xff, 0xff, 0xff, 0xff, 0xff, 0xe7, 0xe7, 0x81, 0xe7, 0xe7, 0xff, 0x81, 0xff,
    0xc7, 0xf3, 0xe7, 0xc3, 0xff, 0xff, 0xff, 0xff, 0xc3, 0xe7, 0xf3, 0xc7, 0xff, 0xff, 0xff, 0xff,
    0xf7, 0xef, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x33, 0x33, 0x33, 0x33, 0x09, 0x3f,
    0xc1, 0x95, 0xb5, 0x95, 0xc1, 0xf5, 0xf5, 0xff, 0xff, 0xff, 0xff, 0xcf, 0xcf, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xf7, 0xcf, 0xe7, 0xc7, 0xe7, 0xc3, 0xff, 0xff, 0xff, 0xff,
    0xc7, 0x93, 0x93, 0xc7, 0xff, 0xff, 0xff, 0xff, 0xff, 0x27, 0x93, 0xc9, 0x93, 0x27, 0xff, 0xff,
    0xbd, 0x3b, 0xb7, 0xad, 0xd9, 0xb1, 0x7d, 0xff, 0xbd, 0x3b, 0xb7, 0xa9, 0xdd, 0xbb, 0x71, 0xff,
    0x1d, 0xbb, 0xd7, 0x2d, 0xd9, 0xb1, 0x7d, 0xff, 0xc7, 0xff, 0xc7, 0x9f, 0x39, 0x01, 0x83, 0xff,
    0xdf, 0xef, 0xc7, 0x93, 0x39, 0x01, 0x39, 0xff, 0xf7, 0xef, 0xc7, 0x93, 0x39, 0x01, 0x39, 0xff,
    0xc7, 0x93, 0xc7, 0x93, 0x39, 0x01, 0x39, 0xff, 0xcb, 0xa7, 0xc7, 0x93, 0x39, 0x01, 0x39, 0xff,
    0x93, 0xff, 0xc7, 0x93, 0x39, 0x01, 0x39, 0xff, 0xef, 0xd7, 0xc7, 0x93, 0x39, 0x01, 0x39, 0xff,
    0xc1, 0x87, 0x27, 0x21, 0x07, 0x27, 0x21, 0xff, 0xc3, 0x99, 0x3f, 0x3f, 0x99, 0xc3, 0xf7, 0xcf,
    0xdf, 0xef, 0x01, 0x3f, 0x03, 0x3f, 0x01, 0xff, 0xf7, 0xef, 0x01, 0x3f, 0x03, 0x3f, 0x01, 0xff,
    0xc7, 0x93, 0x01, 0x3f, 0x03, 0x3f, 0x01, 0xff, 0x93, 0xff, 0x01, 0x3f, 0x03, 0x3f, 0x01, 0xff,
    0xef, 0xf7, 0x81, 0xe7, 0xe7, 0xe7, 0x81, 0xff, 0xf7, 0xef, 0x81, 0xe7, 0xe7, 0xe7, 0x81, 0xff,
    0xe7, 0xc3, 0x81, 0xe7, 0xe7, 0xe7, 0x81, 0xff, 0x99, 0xff, 0x81, 0xe7, 0xe7, 0xe7, 0x81, 0xff,
    0x87, 0x93, 0x99, 0x09, 0x99, 0x93, 0x87, 0xff, 0xcb, 0xa7, 0x19, 0x09, 0x01, 0x21, 0x31, 0xff,
    0xdf, 0xef, 0x83, 0x39, 0x39, 0x39, 0x83, 0xff, 0xf7, 0xef, 0x83, 0x39, 0x39, 0x39, 0x83, 0xff,
    0xc7, 0x93, 0x83, 0x39, 0x39, 0x39, 0x83, 0xff, 0xcb, 0xa7, 0x83, 0x39, 0x39, 0x39, 0x83, 0xff,
    0x93, 0xff, 0x83, 0x39, 0x39, 0x39, 0x83, 0xff, 0xff, 0xbb, 0xd7, 0xef, 0xd7, 0xbb, 0xff, 0xff,
    0x83, 0x39, 0x31, 0x29, 0x19, 0x39, 0x83, 0xff, 0xdf, 0xef, 0x39, 0x39, 0x39, 0x39, 0x83, 0xff,
    0xf7, 0xef, 0x39, 0x39, 0x39, 0x39, 0x83, 0xff, 0xc7, 0x93, 0xff, 0x39, 0x39, 0x39, 0x83, 0xff,
    0x93, 0xff, 0x39, 0x39, 0x39, 0x39, 0x83, 0xff, 0xf7, 0xef, 0x99, 0x99, 0xc3, 0xe7, 0xe7, 0xff,
    0x3f, 0x03, 0x39, 0x39, 0x39, 0x03, 0x3f, 0xff, 0xc3, 0x99, 0x99, 0x93, 0x99, 0x89, 0x93, 0xff,
    0xdf, 0xef, 0x83, 0xf9, 0x81, 0x39, 0x81, 0xff, 0xf7, 0xef, 0x83, 0xf9, 0x81, 0x39, 0x81, 0xff,
    0xc7, 0x93, 0x83, 0xf9, 0x81, 0x39, 0x81, 0xff, 0xcb, 0xa7, 0x83, 0xf9, 0x81, 0x39, 0x81, 0xff,
    0x93, 0xff, 0x83, 0xf9, 0x81, 0x39, 0x81, 0xff, 0xef, 0xd7, 0x83, 0xf9, 0x81, 0x39, 0x81, 0xff,
    0xff, 0xff, 0x83, 0xe9, 0x81, 0x2f, 0x83, 0xff, 0xff, 0xff, 0x81, 0x3f, 0x3f, 0x81, 0xf7, 0xcf,
    0xdf, 0xef, 0x83, 0x39, 0x01, 0x3f, 0x83, 0xff, 0xf7, 0xef, 0x83, 0x39, 0x01, 0x3f, 0x83, 0xff,
    0xc7, 0x93, 0x83, 0x39, 0x01, 0x3f, 0x83, 0xff, 0x93, 0xff, 0x83, 0x39, 0x01, 0x3f, 0x83, 0xff,
    0xdf, 0xef, 0xff, 0xc7, 0xe7, 0xe7, 0x81, 0xff, 0xf7, 0xef, 0xff, 0xc7, 0xe7, 0xe7, 0x81, 0xff,
    0xc7, 0x93, 0xff, 0xc7, 0xe7, 0xe7, 0x81, 0xff, 0x93, 0xff, 0xc7, 0xe7, 0xe7, 0xe7, 0x81, 0xff,
    0x9b, 0x87, 0x67, 0x83, 0x39, 0x39, 0x83, 0xff, 0xcb, 0xa7, 0x03, 0x39, 0x39, 0x39, 0x39, 0xff,
    0xdf, 0xef, 0x83, 0x39, 0x39, 0x39, 0x83, 0xff, 0xf7, 0xef, 0x83, 0x39, 0x39, 0x39, 0x83, 0xff,
    0xc7, 0x93, 0x83, 0x39, 0x39, 0x39, 0x83, 0xff, 0xcb, 0xa7, 0x83, 0x39, 0x39, 0x39, 0x83, 0xff,
    0x93, 0xff, 0x83, 0x39, 0x39, 0x39, 0x83, 0xff, 0xff, 0xe7, 0xff, 0x81, 0xff, 0xe7, 0xff, 0xff,
    0xff, 0xff, 0x83, 0x31, 0x29, 0x19, 0x83, 0xff, 0xdf, 0xef, 0x39, 0x39, 0x39, 0x39, 0x81, 0xff,
    0xf7, 0xef, 0x39, 0x39, 0x39, 0x39, 0x81, 0xff, 0xc7, 0x93, 0xff, 0x39, 0x39, 0x39, 0x81, 0xff,
    0x93, 0xff, 0x39, 0x39, 0x39, 0x39, 0x81, 0xff, 0xf7, 0xef, 0x39, 0x39, 0x39, 0x81, 0xf9, 0x83,
    0x3f, 0x3f, 0x03, 0x39, 0x39, 0x03, 0x3f, 0x3f, 0x93, 0xff, 0x39, 0x39, 0x39, 0x81, 0xf9, 0x83,
];
