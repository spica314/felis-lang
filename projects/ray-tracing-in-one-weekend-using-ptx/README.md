# ray-tracing-in-one-weekend-using-ptx

[Ray Tracing in One Weekend](https://raytracing.github.io/) implementation using Felis and PTX.

The project keeps the PTX kernel path compiled from Felis with `#compile_ptx`.
The executable currently writes the PPM stream on the host side, while the PTX
path mirrors the same ray/background color math as the GPU rendering surface
continues to grow.

## Current Stage

This project implements chapter 4.2, "Sending Rays Into the Scene", through the
book's `image.2` result. It writes a 400x225 `P3` image to standard output with
the ray-direction sky gradient from white to blue.

## Build and Render

```sh
cargo run -p neco-rs -- build projects/ray-tracing-in-one-weekend-using-ptx
projects/ray-tracing-in-one-weekend-using-ptx/ray-tracing-in-one-weekend-using-ptx-bin/.neco/main > image.ppm
```

## References

- <https://raytracing.github.io/>
