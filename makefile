source/main: source/main.cpp source/mandelbrot_ispc.o
	g++ -O3 -o source/main source/mandelbrot_ispc.o source/main.cpp -std=c++1y

source/mandelbrot_ispc.o: source/mandelbrot.ispc
	ispc -O3   --opt=fast-math source/mandelbrot.ispc -o source/mandelbrot_ispc.o -h  source/mandelbrot_ispc.h --opt=fast-masked-vload --opt=force-aligned-memory --target=avx2-i32x16 # --target=sse4-i32x4 --instrument


# --target=sse2-i32x4
#--target=sse4-i32x4


#--cpu=penryn 

clean:
	rm source/mandelbrot_ispc.o source/main
