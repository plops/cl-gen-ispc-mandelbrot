#CXX=/opt/intel/compilers_and_libraries/linux/bin/intel64/icc
CXX=clang++
CXXFLAGS=-g -O3  -fstack-protector-strong -fident -fno-lto -fasynchronous-unwind-tables -Wall -Wextra -pedantic-errors -Wsign-promo -Wnon-virtual-dtor -Winit-self -Wno-deprecated-declarations -pipe -D_FILE_OFFSET_BITS=64 -march=native  --std=gnu++1y -ffast-math
source/main: source/main.cpp source/mandelbrot_ispc.o
	$(CXX) $(CXXFLAGS) -o source/main source/mandelbrot_ispc.o source/main.cpp -ltbb

source/mandelbrot_ispc.o: source/mandelbrot.ispc
	ispc -g -O3   --opt=fast-math source/mandelbrot.ispc -o source/mandelbrot_ispc.o -h  source/mandelbrot_ispc.h  --target=sse4-i32x4 --opt=force-aligned-memory


# --opt=fast-masked-vload --opt=force-aligned-memory

#--target=avx2-i32x16


#  --instrument


# --target=sse2-i32x4
#--target=sse4-i32x4


#--cpu=penryn 

clean:
	rm source/mandelbrot_ispc.o source/main
