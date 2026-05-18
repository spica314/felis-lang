# ray-tracing-in-one-weekend-using-ptx

[Ray Tracing in One Weekend](https://raytracing.github.io/) implementation using Felis and PTX.

The project keeps the PTX kernel path compiled from Felis with `#compile_ptx`.
The executable currently writes the PPM stream on the host side, while the PTX
path mirrors the same ray/background color math as the GPU rendering surface
continues to grow.

## Current Stage

This project implements chapter 12, "Positionable Camera", through the book's
Image 21 result. It renders the scene with Lambertian, fuzzy metal, and a
hollow dielectric sphere built from an outer glass sphere plus an inner air
bubble, viewed from a positionable camera with `lookfrom = (-2, 2, 1)`,
`lookat = (0, 0, -1)`, `vup = (0, 1, 0)`, and `vfov = 20`. The PTX kernel
renders a 400x225 `P3` image with 100 samples per pixel and gamma-corrected
output.

## Build and Render

```sh
cargo run -p neco-rs -- build projects/ray-tracing-in-one-weekend-using-ptx
projects/ray-tracing-in-one-weekend-using-ptx/ray-tracing-in-one-weekend-using-ptx-bin/.neco/main > image.ppm
```

## References

- <https://raytracing.github.io/>
