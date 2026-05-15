# ray-tracing-in-one-weekend-using-ptx

[Ray Tracing in One Weekend](https://raytracing.github.io/) implementation using Felis and PTX.

The project keeps the PTX kernel path compiled from Felis with `#compile_ptx`.

## Current Stage

This project implements the PPM image output from chapter 2.2 of
_Ray Tracing in One Weekend_. It writes a 256x256 `P3` image to standard output,
with red increasing from left to right and green increasing from top to bottom.

## Build and Render

```sh
cargo run -p neco-rs -- build projects/ray-tracing-in-one-weekend-using-ptx
projects/ray-tracing-in-one-weekend-using-ptx/ray-tracing-in-one-weekend-using-ptx-bin/.neco/main > image.ppm
```

## References

- <https://raytracing.github.io/>
