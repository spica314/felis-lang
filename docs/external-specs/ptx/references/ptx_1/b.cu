extern "C" {
    __global__ void f(int *xs) {
        xs[0] = 42;
    }
}
