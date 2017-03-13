# Performance monitor on Intel CPU


https://software.intel.com/en-us/articles/intel-performance-counter-monitor#calling_pcm

cd ~/src/
git clone https://github.com/opcm/pcm

L3 cache is global and shared by all cores and counters are not tracking the specific thread's IP

set affinity or pin your threads to specific logical CPU


taskset

isolcpus=0,1

# References

https://technicalandstuff.wordpress.com/2015/05/15/using-intels-pcm-in-linux-and-inside-c/

https://www.youtube.com/watch?v=h2QEM1HpFgg Roofline Analysis in Intel Advisor 2017