#include "mandelbrot_ispc.h"
#include <algorithm>
#include <cpucounters.h>
#include <fstream>
#include <fstream>
#include <iostream>
#include <sched.h>
#include <sstream>
#include <stdint.h>
#include <sys/sysinfo.h>
#include <tbb/tbb.h>
#include <type_traits>
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

static void sys_file_write(std::string fn, std::string str) {
  {
    std::ofstream f(fn);

    (f << str);
  }
}

static void cpu_frequency_set(unsigned int cpu, unsigned int freq_hz) {
  {
    std::ostringstream fn;
    std::ostringstream out;

    (fn << "/sys/devices/system/cpu/cpu" << cpu << "/cpufreq/scaling_governor");
    (out << "performance");
    sys_file_write(fn.str(), out.str());

    (fn << "/sys/devices/system/cpu/cpu" << cpu << "/cpufreq/scaling_min_freq");
    (out << freq_hz);
    sys_file_write(fn.str(), out.str());

    (fn << "/sys/devices/system/cpu/cpu" << cpu << "/cpufreq/scaling_max_freq");
    (out << freq_hz);
    sys_file_write(fn.str(), out.str());
  }
}

static void cpu_frequencies_print(unsigned int n) {
  for (unsigned int i = 0; (i < n); i += 1) {
    {
      std::ostringstream os;

      (os << "/sys/devices/system/cpu/cpu" << i << "/cpufreq/cpuinfo_cur_freq");
      {
        std::ifstream f(os.str());
        std::string line;

        std::getline(f, line);
        (std::cout << "processor " << i << " runs at " << line << " Hz"
                   << std::endl);
      }
    }
  }
}

int main() {
  {
    const int number_threads = 4;

    for (unsigned int i = 0; (i < number_threads); i += 1) {
      cpu_frequency_set(i, 3600000);
    }

    {
      cpu_set_t cpu_mask;

      CPU_ZERO(&cpu_mask);
      for (unsigned int i = 0; (i < number_threads); i += 1) {
        CPU_SET(i, &cpu_mask);
      }

      {
        int err = sched_setaffinity(getpid(), sizeof(cpu_mask), &cpu_mask);

        if ((0 != err)) {
          (std::cout << "setaffinity error" << std::endl);
        }

        (std::cout << "tried to use " << number_threads
                   << " tbb worker threads. "
                      "tbb::task_scheduler_init::default_num_threads="
                   << tbb::task_scheduler_init::default_num_threads()
                   << " tbb::tbb_thread::hardware_concurrency="
                   << static_cast<int>(tbb::tbb_thread::hardware_concurrency())
                   << std::endl);

        cpu_frequencies_print(number_threads);
      }
    }

    {
      PCM *m = PCM::getInstance();
      const unsigned int width = 512;
      const unsigned int height = 512;
      float x0 = (-2.e+0);
      float x1 = (1.e+0);
      float y0 = (-1.e+0);
      float y1 = (1.e+0);
      float dx = ((x1 - x0) * ((1.e+0) / 512));
      float dy = ((y1 - y0) * ((1.e+0) / 512));
      tbb::task_scheduler_init tbb_init(number_threads);
      static int buf[(32 + (width * height))] __attribute__((aligned(64)));

      {
        auto ret = m->program(PCM::DEFAULT_EVENTS, nullptr);

        switch (ret) {
        case 0: {
          (std::cout << "pcm init successfull" << std::endl);

          break;
        }
        case 1: {
          (std::cout << "pcm init msr access denied, try running with sudo"
                     << std::endl);

          break;
        }
        case 2: {
          (std::cout << "pcm init pmu busy" << std::endl);

          m->resetPMU();
          ret = m->program();
          break;
        }
        default: {
          (std::cout << "pcm init unknown error" << std::endl);

          break;
        }
        }
      }

      {
        SystemCounterState sstate_before = getSystemCounterState();

        for (unsigned int i = 0; (i < 1000); i += 1) {
          {

            tbb::parallel_for(
                tbb::blocked_range2d<int, int>(0, 512, 2, 0, 512, 512),
                [=](const tbb::blocked_range2d<int, int> &r) {
                  ispc::mandelbrot_ispc(x0, y0, dx, dy, buf, r.rows().begin(),
                                        r.cols().begin(), r.rows().end(),
                                        r.cols().end());
                });
          }
        }

        {
          SystemCounterState sstate_after = getSystemCounterState();

          (std::cout << "instr-retir = "
                     << getInstructionsRetired(sstate_before, sstate_after)
                     << std::endl);

          (std::cout << "instr/clock = " << getIPC(sstate_before, sstate_after)
                     << std::endl);

          (std::cout << "invaria-tsc = "
                     << getInvariantTSC(sstate_before, sstate_after)
                     << std::endl);

          (std::cout << "l2hit-ratio = "
                     << getL2CacheHitRatio(sstate_before, sstate_after)
                     << std::endl);

          (std::cout << "l3hit-ratio = "
                     << getL3CacheHitRatio(sstate_before, sstate_after)
                     << std::endl);

          m->cleanup();
        }

        cpu_frequencies_print(number_threads);
      }

      return 0;
    }
  }
}
