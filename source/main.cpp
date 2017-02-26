#include <fstream>
#include <algorithm>
#include <type_traits>
#include <iostream>
#include "mandelbrot_ispc.h"
int main() {
  {
    const unsigned int width = 768;
    unsigned int height = 512;
    float x0 = (-2.e+0);
    float x1 = (1.e+0);
    float y0 = (-1.e+0);
    float y1 = (1.e+0);
    int *buf(new int[(768 * height)]);

    if ((nullptr == buf)) {
      (std::cout << "error getting aligned buffer");
    }

    ispc::mandelbrot_ispc(x0, y0, x1, y1, height, buf);
    {
      std::ofstream f(
          "/dev/shm/test.pgm",
          (std::ofstream::out | std::ofstream::binary | std::ofstream::trunc));
      unsigned char *bufu8(new unsigned char[(768 * height)]);

      for (int i = 0; (i < (width * height)); i += 1) {
        bufu8[i] = std::min(255, std::max(0, buf[i]));
      }

      (f << "P5\n" << width << " " << height << "\n255\n");
      f.write(reinterpret_cast<char *>(bufu8), (width * height));
    }

    return 0;
  }
}
