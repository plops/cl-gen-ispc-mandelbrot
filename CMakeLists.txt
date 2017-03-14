cmake_minimum_required(VERSION 3.6)
project(cl_gen_ispc_mandelbrot)
SET(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/common/cmake ${CMAKE_MODULE_PATH})
INCLUDE(ispc)
set(CMAKE_CXX_STANDARD 11)
add_custom_command(OUTPUT mandelbrot_ispc.o
                   COMMAND ispc -g --target=sse4-i32x4 ${CMAKE_SOURCE_DIR}/source/mandelbrot.ispc -o mandelbrot_ispc.o
                   DEPENDS source/mandelbrot.ispc)
set(SOURCE_FILES
        source/main.cpp
        source/mandelbrot.ispc
        source/mandelbrot_ispc.h)
#ADD_ISPC_LIBRARY(mandelbrot source/mandelbrot.ispc)
include_directories(source)
include_directories(/home/martin/src/tbb-2017_U5/include)
include_directories(/home/martin/src/pcm)
link_directories(/home/martin/src/pcm/pcm.so)
add_executable(cl_gen_ispc_mandelbrot ${SOURCE_FILES} mandelbrot_ispc.o)
target_link_libraries(cl_gen_ispc_mandelbrot tbb)
target_link_libraries(cl_gen_ispc_mandelbrot pcm)
