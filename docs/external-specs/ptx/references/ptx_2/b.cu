extern "C" {
    __global__ void f(long *rs, long *gs, long *bs) {
        long thread_id = blockIdx.x * blockDim.x + threadIdx.x;

        long h = 256;
        long w = 256;
        long x = thread_id % w;
        long y = thread_id / w;

        float r = (float)x / (float)w;
        float g = (float)y / (float)h;
        float b = 0.2f;

        long ir = (long)(r * 255.99f);
        long ig = (long)(g * 255.99f);
        long ib = (long)(b * 255.99f);

        rs[thread_id] = ir;
        gs[thread_id] = ig;
        bs[thread_id] = ib;
    }
}
