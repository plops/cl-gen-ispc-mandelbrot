#include <fstream>
#include <algorithm>
#include <type_traits>
#include <iostream>
#include "mandelbrot_ispc.h"
#include <stdint.h>
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
    const unsigned int width = 768;
    const unsigned int height = 512;
    float x0 = (-2.e+0);
    float x1 = (1.e+0);
    float y0 = (-1.e+0);
    float y1 = (1.e+0);
    static int buf[(32 + (width * height))] __attribute__((aligned(64)));

    for (int i = 0; (i < 1); i += 1) {
      {
        auto start = rdtsc();

        ispc::mandelbrot_ispc(x0, y0, x1, y1, buf);
        (std::cout << "mcycles: "
                   << ((rdtsc() - start) / ((1.024e+3) * (1.024e+3)))
                   << std::endl);
      }
    }

    return 0;
  }
}
