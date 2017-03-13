#CXX=/opt/intel/compilers_and_libraries/linux/bin/intel64/icc
#CXX=clang++
CXX=g++
CXXFLAGS=-g -O3  -fstack-protector-strong -fident -fno-lto -fasynchronous-unwind-tables -Wall -Wextra -pedantic-errors -Wsign-promo -Wnon-virtual-dtor -Winit-self -Wno-deprecated-declarations -pipe -D_FILE_OFFSET_BITS=64 -march=native   -ffast-math --std=c++11

#--std=gnu++1z

#--std=c++11

# pcm needs to be compiled with g++, clang++ gives this error: /home/martin/src/pcm/types.h:298:9: error: anonymous types declared in an anonymous union are an extension
CXXINC = -I/home/martin/src/pcm

# --std=gnu++1y
source/main: source/main.cpp source/mandelbrot_ispc.o
	$(CXX) $(CXXFLAGS) -o source/main source/mandelbrot_ispc.o source/main.cpp -ltbb $(CXXINC)

source/mandelbrot_ispc.o: source/mandelbrot.ispc
	ispc -g -O3   --opt=fast-math source/mandelbrot.ispc -o source/mandelbrot_ispc.o -h  source/mandelbrot_ispc.h  --target=avx2-i32x16 --opt=force-aligned-memory


# --opt=fast-masked-vload --opt=force-aligned-memory

#--target=avx2-i32x16


#  --instrument


# --target=sse2-i32x4
#--target=sse4-i32x4


#--cpu=penryn 

clean:
	rm source/mandelbrot_ispc.o source/main

cache: source/mandelbrot.cachegrind

source/cache.out: source/main
	time valgrind --tool=cachegrind --cachegrind-out-file=source/cache.out --branch-sim=yes source/main 

source/mandelbrot.cachegrind: source/cache.out
	cg_annotate source/cache.out  /home/martin/stage/cl-gen-ispc-mandelbrot/source//mandelbrot.ispc > source/mandelbrot.cachegrind
#  --branch-sim=yes

aps: source/main
	echo 0 | sudo tee  /proc/sys/kernel/nmi_watchdog
	echo 0 | sudo tee  /proc/sys/kernel/kptr_restrict
	~/big/APS_2017_lin_478468/aps.sh source/main

