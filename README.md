# Performance monitor on Intel CPU


https://software.intel.com/en-us/articles/intel-performance-counter-monitor#calling_pcm
```
cd ~/src/
git clone https://github.com/opcm/pcm
cd pcm
make
cd pcm.so
make
```

- L3 cache is global and shared by all cores and counters are not tracking the specific thread's IP

- set affinity or pin your threads to specific logical CPU


- taskset

- /etc/default/grub: `GRUB_CMDLINE_LINUX="isolcpus=0,1,2,3"`

# References

https://technicalandstuff.wordpress.com/2015/05/15/using-intels-pcm-in-linux-and-inside-c/

https://www.youtube.com/watch?v=h2QEM1HpFgg Roofline Analysis in Intel Advisor 2017

https://jeffamstutz.io/2017/02/17/thread-parallelism-part-3-a-very-brief-look-at-performance/

https://overcast.fm/+EN2YtNcSc Software Defined Visualization with Jeff Amstutz (starts at 12:16)