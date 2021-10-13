open Graphics
open Printf

let width = 500
let height = 500

let zoom = ref 4.0
let ax = ref 0.0
let ay = ref 0.0
let quality = ref 60

let iteration_count x0 y0 =
    let rec iter' x y n =
        if n = !quality then 255 else
        if x *. x +. y *. y > 4.0
        then n * 256 / !quality
        else iter' (x *. x -. y *. y +. x0) (2.0 *. x *. y +. y0) (succ n)
    in
    iter' 0.0 0.0 0

let redraw () =
    let zoom_c = !zoom /. float width in
    let offs_x = !ax -. !zoom /. 2.0 in
    let offs_y = !ay -. !zoom /. 2.0 in
    for j = 0 to width - 1 do
        for k = 0 to height - 1 do
            let ca = zoom_c *. float j +. offs_x in
            let cb = zoom_c *. float k +. offs_y in
            let c = iteration_count ca cb in
            set_color (rgb c c c);
            plot j k;
        done;
    done;
    synchronize ()

let main () =
    let window_size = sprintf " %dx%d" width height in
    open_graph window_size;

    auto_synchronize false;

    let event_loop () =
        redraw ();
    in event_loop () ;
      ignore(read_key())

let _ = main ()
