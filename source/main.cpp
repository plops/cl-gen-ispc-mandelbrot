#include "mandelbrot_ispc.h"
#include <algorithm>
#include <cpucounters.h>
#include <fstream>
#include <tbb/tbb.h>
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

static void cpu_setaffinity(const unsigned int number_threads) {
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

      (std::cout
       << "tried to use " << number_threads
       << " tbb worker threads. tbb::task_scheduler_init::default_num_threads="
       << tbb::task_scheduler_init::default_num_threads()
       << " tbb::tbb_thread::hardware_concurrency="
       << static_cast<int>(tbb::tbb_thread::hardware_concurrency())
       << std::endl);

      cpu_frequencies_print(number_threads);
    }
  }
}

static void pcm_init(PCM *m, int count = 3) {
  if ((0 < count)) {
    {
      auto ret = m->program(PCM::DEFAULT_EVENTS, nullptr);

      switch (ret) {
      case PCM::Success: {
        (std::cout << "pcm init successfull" << std::endl);

        break;
      }
      case PCM::MSRAccessDenied: {
        (std::cout << "pcm init msr access denied, try running with sudo"
                   << std::endl);

        break;
      }
      case PCM::PMUBusy: {
        (std::cout << "pcm init pmu busy" << std::endl);

        m->resetPMU();
        // count indicates how many times we tried to reset;
        pcm_init(m, (count - 1));
        break;
      }
      case PCM::UnknownError: {
        (std::cout << "pcm init unknown error" << std::endl);

        break;
      }
      }
    }
  }
}

static void pcm_print(PCM *m, SystemCounterState &before) {
  {
    SystemCounterState after = getSystemCounterState();

    (std::cout << "getBytesReadFromEDC  = "
               << getBytesReadFromEDC(before, after) << std::endl);

    (std::cout << "getBytesReadFromMC   = " << getBytesReadFromMC(before, after)
               << std::endl);

    (std::cout << "getBytesWrittenToEDC = "
               << getBytesWrittenToEDC(before, after) << std::endl);

    (std::cout << "getBytesWrittenToMC  = "
               << getBytesWrittenToMC(before, after) << std::endl);

    (std::cout << "getConsumedEnergy    = " << getConsumedEnergy(before, after)
               << std::endl);

    (std::cout << "getCycles            = " << getCycles(before, after)
               << std::endl);

    (std::cout << "getDRAMConsumedEnergy = "
               << getDRAMConsumedEnergy(before, after) << std::endl);

    (std::cout << "getIORequestBytesFromMC = "
               << getIORequestBytesFromMC(before, after) << std::endl);

    (std::cout << "getInstructionsRetired = "
               << getInstructionsRetired(before, after) << std::endl);

    (std::cout << "getInvariantTSC      = " << getInvariantTSC(before, after)
               << std::endl);

    (std::cout << "getL2CacheHits       = " << getL2CacheHits(before, after)
               << std::endl);

    (std::cout << "getL2CacheMisses     = " << getL2CacheMisses(before, after)
               << std::endl);

    (std::cout << "getL3CacheHits       = " << getL3CacheHits(before, after)
               << std::endl);

    (std::cout << "getL3CacheHitsNoSnoop = "
               << getL3CacheHitsNoSnoop(before, after) << std::endl);

    (std::cout << "getL3CacheHitsSnoop  = "
               << getL3CacheHitsSnoop(before, after) << std::endl);

    (std::cout << "getL3CacheMisses     = " << getL3CacheMisses(before, after)
               << std::endl);

    (std::cout << "getLocalMemoryBW     = " << getLocalMemoryBW(before, after)
               << std::endl);

    (std::cout << "getRefCycles         = " << getRefCycles(before, after)
               << std::endl);

    (std::cout << "getRemoteMemoryBW    = " << getRemoteMemoryBW(before, after)
               << std::endl);

    (std::cout << "getL2CacheHitRatio   = " << getL2CacheHitRatio(before, after)
               << std::endl);

    (std::cout << "getL3CacheHitRatio   = " << getL3CacheHitRatio(before, after)
               << std::endl);

    (std::cout << "getCoreIPC           = " << getCoreIPC(before, after)
               << std::endl);

    (std::cout << "getTotalExecUsage    = " << getTotalExecUsage(before, after)
               << std::endl);

    (std::cout << "getQPItoMCTrafficRatio = "
               << getQPItoMCTrafficRatio(before, after) << std::endl);

    (std::cout << "getConsumedJoules    = " << getConsumedJoules(before, after)
               << std::endl);

    (std::cout << "getDRAMConsumedJoules = "
               << getDRAMConsumedJoules(before, after) << std::endl);

    (std::cout << "getIPC               = " << getIPC(before, after)
               << std::endl);

    (std::cout << "getExecUsage         = " << getExecUsage(before, after)
               << std::endl);

    (std::cout << "getAverageFrequency  = "
               << getAverageFrequency(before, after) << std::endl);

    (std::cout << "getActiveAverageFrequency = "
               << getActiveAverageFrequency(before, after) << std::endl);

    (std::cout << "getRelativeFrequency = "
               << getRelativeFrequency(before, after) << std::endl);

    (std::cout << "getActiveRelativeFrequency = "
               << getActiveRelativeFrequency(before, after) << std::endl);

    (std::cout << "getCyclesLostDueL3CacheMisses = "
               << getCyclesLostDueL3CacheMisses(before, after) << std::endl);

    (std::cout << "getCyclesLostDueL2CacheMisses = "
               << getCyclesLostDueL2CacheMisses(before, after) << std::endl);

    (std::cout << "getL2CacheHitRatio   = " << getL2CacheHitRatio(before, after)
               << std::endl);

    (std::cout << "getL3CacheHitRatio   = " << getL3CacheHitRatio(before, after)
               << std::endl);

    (std::cout << "getPCUFrequency      = " << m->getPCUFrequency()
               << std::endl);

    (std::cout << "getMaxIPC            = " << m->getMaxIPC() << std::endl);

    (std::cout << "getJoulesPerEnergyUnit = " << m->getJoulesPerEnergyUnit()
               << std::endl);

    (std::cout << "getNominalFrequency  = " << m->getNominalFrequency()
               << std::endl);

    (std::cout << "getQPILinksPerSocket = " << m->getQPILinksPerSocket()
               << std::endl);

    (std::cout << "getPCUFrequency      = " << m->getPCUFrequency()
               << std::endl);
  }
}

int main() {
  {
    const int number_threads = 8;

    for (unsigned int i = 0; (i < number_threads); i += 1) {
      // set the cpus to the same frequency;
      cpu_frequency_set(i, 2000000);
    }

    cpu_setaffinity(number_threads);
    {
      PCM *m = PCM::getInstance();
      const unsigned int width = 512;
      const unsigned int height = 512;
      float x0 = (-2.e+0f);
      float x1 = (1.e+0f);
      float y0 = (-1.e+0f);
      float y1 = (1.e+0f);
      float dx = ((x1 - x0) * ((1.e+0f) / 512));
      float dy = ((y1 - y0) * ((1.e+0f) / 512));
      tbb::task_scheduler_init tbb_init(number_threads);
      static int buf[(32 + (width * height))] __attribute__((aligned(64)));

      pcm_init(m);
      {
        SystemCounterState sstate_before = getSystemCounterState();

        for (unsigned int i = 0; (i < 100); i += 1) {
          {

            tbb::parallel_for(
                tbb::blocked_range2d<int, int>(0, 512, 2, 0, 512, 64),
                [=](const tbb::blocked_range2d<int, int> &r) {
                  ispc::mandelbrot_ispc(x0, y0, dx, dy, buf, r.rows().begin(),
                                        r.cols().begin(), r.rows().end(),
                                        r.cols().end());
                });
          }
        }

        pcm_print(m, sstate_before);
        m->cleanup();
        cpu_frequencies_print(number_threads);
      }

      return 0;
    }
  }
}
