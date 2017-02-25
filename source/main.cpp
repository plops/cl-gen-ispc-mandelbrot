#include "mandelbrot_ispc.h"
int main() {
  {
    unsigned int height = 512;
    float x0 = (-2.e+0);
    float x1 = (1.e+0);
    float y0 = (-1.e+0);
    float y1 = (1.e+0);
    int *buf(new int[(768 * height)]);

    ispc::mandelbrot_ispc(x0, y0, x1, y1, height, buf);
    return 0;
  }
}
