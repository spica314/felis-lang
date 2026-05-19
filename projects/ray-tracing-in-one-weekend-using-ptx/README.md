# ray-tracing-in-one-weekend-using-ptx

[Ray Tracing in One Weekend](https://raytracing.github.io/) implementation using Felis and PTX.

The project keeps the PTX kernel path compiled from Felis with `#compile_ptx`.
The executable currently writes the PPM stream on the host side, while the PTX
path mirrors the same ray/background color math as the GPU rendering surface
continues to grow.

## Current Stage

This project implements chapter 14, "Where Next?", through the book's Image 23
final scene. It renders the procedural ground and random small-sphere field
with Lambertian, fuzzy metal, and dielectric materials, plus the three large
final-scene spheres. The camera uses `lookfrom = (13, 2, 3)`,
`lookat = (0, 0, 0)`, `vup = (0, 1, 0)`, `vfov = 20`,
`defocus_angle = 0.6`, and `focus_dist = 10`. The PTX kernel renders a 1600x900
`P3` image with 100 samples per pixel and gamma-corrected output.

## Build and Render

```sh
cargo run -p neco-rs -- build projects/ray-tracing-in-one-weekend-using-ptx
projects/ray-tracing-in-one-weekend-using-ptx/ray-tracing-in-one-weekend-using-ptx-bin/.neco/main > image.ppm
```

## References

- <https://raytracing.github.io/>
