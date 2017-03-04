#include <fstream>
#include <algorithm>
#include <type_traits>
#include <iostream>
#include "mandelbrot_ispc.h"
#include <stdint.h>
#include <tbb/tbb.h>
extern "C" {
void ISPCInstrument(const char *fn, const char *note, int line, uint64_t mask) {
  (std::cout << fn << ":" << line << " - " << note << ", 0x" << std::hex << mask
             << std::endl);
}
} // extern "C"

uint64_t rdtsc() {
  {
    uint32_t low;
    uint32_t high;

    __asm__ __volatile__("xorl %%eax,%%eax \n cpuid" ::
                             : "%rax", "%rbx", "%rcx", "%rdx");
    __asm__ __volatile__("rdtsc" : "=a"(low), "=d"(high));
    return ((static_cast<uint64_t>(high) << 32) | low);
  }
}

int main() {
  {
    const unsigned int width = 512;
    const unsigned int height = 512;
    float x0 = (-2.e+0);
    float x1 = (1.e+0);
    float y0 = (-1.e+0);
    float y1 = (1.e+0);
    float dx = ((x1 - x0) * ((1.e+0) / 512));
    float dy = ((y1 - y0) * ((1.e+0) / 512));
    static int buf[(32 + (width * height))] __attribute__((aligned(64)));

    for (unsigned int i = 0; (i < 100); i += 1) {
      {

        tbb::parallel_for(
            tbb::blocked_range2d<int, int>(0, 512, 4, 0, 512, 512),
            [=](const tbb::blocked_range2d<int, int> &r) {
              ispc::mandelbrot_ispc(x0, y0, dx, dy, buf, r.rows().begin(),
                                    r.cols().begin(), r.rows().end(),
                                    r.cols().end());
            });
      }
    }

    {
      std::ofstream f(
          "/dev/shm/test.pgm",
          (std::ofstream::out | std::ofstream::binary | std::ofstream::trunc));
      unsigned char *bufu8(new unsigned char[(512 * height)]);

      for (unsigned int i = 0; (i < (width * height)); i += 1) {
        bufu8[i] = std::min(255, std::max(0, buf[i]));
      }

      (f << "P5\n" << width << " " << height << "\n255\n");
      f.write(reinterpret_cast<char *>(bufu8), (width * height));
    }

    return 0;
  }
}
