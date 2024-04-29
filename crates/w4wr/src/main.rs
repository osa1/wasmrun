use libwasmrun::{exec, syntax, HostFunDecl, Mem, Runtime, ValueType};

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
                    fun: Rc::new(|_rt, _mem_addr| {
                        println!("tone");
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
                    fun: Rc::new(|_rt, _mem_addr| {
                        println!("blit");
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
                    fun: Rc::new(|_rt, _mem_addr| {
                        println!("textUtf16");
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
                    fun: Rc::new(|_rt, _mem_addr| {
                        println!("rect");
                        Ok(vec![])
                    }),
                },
            ),
        ],
        vec![("memory".to_string(), mem_addr)],
    );

    let module_addr = exec::instantiate(&mut rt, module).unwrap();

    let start_fun_idx = rt.get_module(module_addr).get_start().unwrap();
    let _update_fun_idx = rt
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
        // Draw the current frame
        if let Event::RedrawRequested(_) = event {
            // world.draw(pixels.frame_mut());
            if let Err(err) = pixels.render() {
                eprintln!("pixels.render error: {}", err);
                *control_flow = ControlFlow::Exit;
                return;
            }
        }

        if let Event::UserEvent(()) = event {
            println!("tick");
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
