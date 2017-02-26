#include <fstream>
#include <algorithm>
#include <type_traits>
#include <iostream>
#include "mandelbrot_ispc.h"
int main() {
  {
    const unsigned int width = 768;
    const unsigned int height = 512;
    float x0 = (-2.e+0);
    float x1 = (1.e+0);
    float y0 = (-1.e+0);
    float y1 = (1.e+0);
    static int buf[(width * height)] __attribute__((aligned(64)));

    for (int i = 0; (i < 100); i += 1) {
      ispc::mandelbrot_ispc(x0, y0, x1, y1, buf);
    }

    return 0;
  }
}
