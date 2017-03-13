(push :ispc *features*) ;; for now i have to open cp.lisp and compile it again with C-c C-k, so that foreach works

(push :pcm *features*)
#+nil
(delete :pcm *features*)

(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload :cl-cpp-generator))

(in-package :cl-cpp-generator)

(defmacro e (&body body)
  `(statements (<< "std::cout" ,@(loop for e in body collect
                                      (cond ((stringp e) `(string ,e))
                                            (t e))) "std::endl")))




(let ((max-iterations 256)
      (width 64)
      (height 32)
      (grain-rows 512)
      (grain-cols 2))
  (progn
   (defparameter *main-cpp-filename*  (merge-pathnames "stage/cl-gen-ispc-mandelbrot/source/main.cpp"
						       (user-homedir-pathname)))
  
   (with-open-file (s *main-cpp-filename*
		      :direction :output
		      :if-exists :supersede
		      :if-does-not-exist :create)
     (emit-cpp
      :str s
      :clear-env t
      :code
      `(with-compilation-unit
	   (include <fstream>)
	 (include <algorithm>)
	 (include <type_traits>)
	 (include <iostream>)
	 (include "mandelbrot_ispc.h")
	 (include <stdint.h>)
	 (include <tbb/tbb.h>)
	 (include <cpucounters.h>)
	 (extern-c
	  (function (ISPCInstrument ((fn :type "const char*")
				     (note :type "const char*")
				     (line :type int)
				     (mask :type uint64_t)) void)
		    (macroexpand (e fn (string ":") line (string " - ") note (string ", 0x") |STD::HEX| mask ))))

	 ;;  this is the tile code from intel ospray 
	 ;;    https://github.com/ospray/ospray/blob/master/ospray/fb/Tile.h (apache license)
	 ;; void *aligned_alloc( size_t alignment, size_t size ); //	<stdlib.h>	(since C11)
	 ;; // use allocator from tbb?
	 ;; https://www.threadingbuildingblocks.org/tutorial-intel-tbb-scalable-memory-allocator

	 ;;  In some of these allocators, threads must compete for
	 ;;  access to a single shared pool in a way that allows only
	 ;;  one thread to allocate at a time.

	 ;; false sharing is when two threads access different words
	 ;; that share the same cache line.  Use the class
	 ;; cache_aligned_allocator<T> to always allocate on a cache
	 ;; line. Two objects allocated by cache_aligned_allocator are
	 ;; guaranteed to not have false sharing.
	 ;; std::vector<int, cache_aligned_allocator<int> >;

	 ;; scalable_allocation_mode(TBBMALLOC_USE_HUGE_PAGES, 1)
	 ;; enables the use of huge pages by the allocator if
	 ;; supported for the operating system,

	 ;; #include "tbb/scalable_allocator.h"
	 ;; 	   void* alignedMalloc(size_t size, size_t align) 
	 ;;   {
	 ;;     assert((align & (align-1)) == 0);
	 ;; //#if defined(TASKING_TBB) // FIXME: have to disable this for now as the TBB allocator itself seems to access some uninitialized value when using valgrind
	 ;; //    return scalable_aligned_malloc(size,align);
	 ;; //#else
	 ;;     return _mm_malloc(size,align);
	 ;; //#endif
	 ;;   }
  
	 ;;   void alignedFree(void* ptr) 
	 ;;   {
	 ;; //#if defined(TASKING_TBB)
	 ;; //    scalable_aligned_free(ptr);
	 ;; //#else
	 ;;     _mm_free(ptr);
	 ;; //#endif
	 ;; }
	 ;; 	 struct OSPRAY_SDK_INTERFACE __aligned(64) Tile {
	 ;;     // make sure this tile is 64-byte aligned when alloc'ed
	 ;;   void* operator new(size_t size) { return alignedMalloc(size); }       
	 ;;   void operator delete(void* ptr) { alignedFree(ptr); }      
	 ;;   void* operator new[](size_t size) { return alignedMalloc(size); }  
	 ;;   void operator delete[](void* ptr) { alignedFree(ptr); } 
	 ;;     // 'red' component; in float.
	 ;;     float r[TILE_SIZE*TILE_SIZE];
	 ;;     // 'green' component; in float.
	 ;;     float g[TILE_SIZE*TILE_SIZE];
	 ;;     // 'blue' component; in float.
	 ;;     float b[TILE_SIZE*TILE_SIZE];
	 ;;     // 'alpha' component; in float.
	 ;;     float a[TILE_SIZE*TILE_SIZE];
	 ;;     // 'depth' component; in float.
	 ;;     float z[TILE_SIZE*TILE_SIZE];
	 ;;     region2i region; /*!< screen region that this corresponds to */
	 ;;     vec2i    fbSize; /*!< total frame buffer size, for the camera */
	 ;;     vec2f    rcp_fbSize;
	 ;;     int32    generation;
	 ;;     int32    children;
	 ;;     int32    accumID; //!< how often has been accumulated into this tile
	 ;;     Tile() {}
	 ;;     Tile(const vec2i &tile, const vec2i &fbsize, const int32 accumId)
	 ;;       : fbSize(fbsize),
	 ;;         rcp_fbSize(rcp(vec2f(fbsize))),
	 ;;         generation(0),
	 ;;         children(0),
	 ;;         accumID(accumId)
	 ;;     {
	 ;;       region.lower = tile * TILE_SIZE;
	 ;;       region.upper = ospcommon::min(region.lower + TILE_SIZE, fbsize);
	 ;;     }
	 ;; };

	 
	 (function (rdtsc () uint64_t)
		   (let ((low :type uint32_t)
			 (high :type uint32_t))
		     (raw "__asm__ __volatile__ (\"xorl %%eax,%%eax \\n cpuid\"
::: \"%rax\", \"%rbx\", \"%rcx\", \"%rdx\" )")
		     (raw "__asm__ __volatile__ (\"rdtsc\" : \"=a\" (low), \"=d\" (high)) ")
		     (return (|\|| (<< (funcall static_cast<uint64_t> high) 32)
				   low))))
	 (function (main ()
			 int)
		   (let (#+pcm (m :type "PCM*" :init (funcall "PCM::getInstance"))
			 (width :type "const unsigned int" :init ,width)
			 (height :type "const unsigned int" :init ,height)
			 (x0 :type float :init -2.0)
			 (x1 :type float :init 1.0)
			 (y0 :type float :init -1.0)
			 (y1 :type float :init 1.0)
			 (dx :type float :init (* (- x1 x0) (/ 1.0 ,width )))
			 (dy :type float :init (* (- y1 y0) (/ 1.0 ,height)))
			 ;; https://software.intel.com/en-us/articles/data-alignment-to-assist-vectorization
			 ;; buf should be aligned to 64 byte boundary
			 ((aref buf (+ 32 (* width height))) :type "static int" :extra (raw "__attribute__((aligned(64)))"))
			 #+nil (buf :type "int*" ;"std::unique_ptr< int >"
				    :init (funcall reinterpret_cast<int*> (funcall aligned_alloc 1024  (/ (* 1024 (* ,width height))	 1024)))
					;:ctor (new (aref int (* ,width height)))
				    ))
		     #+nil (if (== nullptr buf)
			       (<< "std::cout" (string "error getting aligned buffer")))

		     #+pcm (let ((ret :init (funcall m->program "PCM::DEFAULT_EVENTS" nullptr)))
		       (case ret
			 (0 (macroexpand (e "pcm init successfull")))
			 (1 (macroexpand (e "pcm init msr access denied, try running with sudo")))
			 (2 (macroexpand (e "pcm init pmu busy"))
			    (funcall m->resetPMU)
			    (setf ret (funcall m->program)))
			 (t (macroexpand (e "pcm init unknown error")))))

		     (let (#+pcm (sstate_before :type SystemCounterState :init (funcall getSystemCounterState)))
		       
		       (dotimes (i
				  100000)
			 #+nil (funcall "ispc::mandelbrot_ispc"
				  x0 y0
				  dx dy
				  buf
				  0 0
				  height
				  width
				  )
			 (let ()#+nil ((start :init (funcall rdtsc)))
				    #+nil (funcall "ispc::mandelbrot_ispc" x0 y0 x1 y1 #+nil width #+nil height buf)
				    (funcall "tbb::parallel_for"
						   (funcall "tbb::blocked_range2d<int,int>"
							    0 ,width ,grain-cols
							    0 ,height ,grain-rows)
						   (lambda (((r :type "const tbb::blocked_range2d<int,int>&")) :captures ("="))
						     ;; x0 y0 dx dy o rs cs re ce
						     #+nil (macroexpand (e "(" (funcall (slot-value (funcall  r.rows) begin))
									   " " (funcall (slot-value (funcall  r.cols) begin))
									   " " (funcall (slot-value (funcall  r.rows) end))
									   " " (funcall (slot-value (funcall  r.cols) end))
									   ")"
									   ))

						     (funcall "ispc::mandelbrot_ispc"
							      x0 y0
							      dx dy
							      buf
							      (funcall (slot-value (funcall  r.rows) begin))
							      (funcall (slot-value (funcall  r.cols) begin))
							      (funcall (slot-value (funcall  r.rows) end))
							      (funcall (slot-value (funcall  r.cols) end))
							      )))
				    #+nil (macroexpand (e "mcycles: " (/ (- (funcall rdtsc) start)
									 (* 1024.0 1024.0))))))
		       #+pcm (let ((sstate_after :type SystemCounterState :init (funcall getSystemCounterState)))
			 (macroexpand (e "instr per clock " (funcall getIPC sstate_before sstate_after)

					 " l3 cache hit ratio: " (funcall getL3CacheHitRatio sstate_before
									  sstate_after)))
			 (funcall m->cleanup))
		       )
		     #+nil (let ((f :type "std::ofstream" :ctor (comma-list
								 (string "/dev/shm/test.pgm")
								 (|\|| "std::ofstream::out"
								       "std::ofstream::binary"
								       "std::ofstream::trunc"))
				    )
				 (bufu8 :type "unsigned char*"  :ctor (new (aref "unsigned char" (* ,width height)))
					))
			     (dotimes (i (* width height))
			       (setf (aref bufu8 i) (funcall "std::min" 255 (funcall "std::max" 0 (aref buf i)))))
			     (<< f (string "P5\\n") width (string " ")  height  (string "\\n255\\n"))
		
			     (funcall f.write (funcall reinterpret_cast<char*> bufu8) (* width height)))
		     (return 0))))))
   (sb-ext:run-program "/usr/bin/clang-format" (list "-i" (namestring *main-cpp-filename*))))

  (progn
   (defparameter *main-ispc-filename*  (merge-pathnames "stage/cl-gen-ispc-mandelbrot/source/mandelbrot.ispc"
						       (user-homedir-pathname)))
  
   (with-open-file (s *main-ispc-filename*
		      :direction :output
		      :if-exists :supersede
		      :if-does-not-exist :create)
     (emit-cpp
      :str s
      :clear-env t
      :code
      `(with-compilation-unit
	 
	   (function (mandel ((c_re :type float)
			      (c_im :type float)
			      ;(count :type int)
			      )
			     "static inline int")
	    (let ((z_re :type float :init c_re)
		  (z_im :type float :init c_im)
		  ;(count :type "const int" :init ,max-iterations)
		  (ret :type int :init 0))
	      (dotimes (i ,max-iterations ;count
			 )
		(let ((re2 :type float :init (* z_re z_re))
		      (im2 :type float :init (* z_im z_im)))
		  (if (< 4.0 (+ re2 im2))
		      (statements
		       (setf ret i)
		       (break)))
		  (let ((new_re :type float :init (- (* z_re z_re) (* z_im z_im)))

			(new_im :type float :init (* 2.0 z_re z_im)))
		    (setf z_re (+ c_re new_re)
			  z_im (+ c_im new_im)))))
	      (return ret)))
	 ;; x0 y0 dx dy o rs cs re ce
	 (function (mandelbrot_ispc ((x0 :type "uniform float")
				     (y0 :type "uniform float")

				     (dx :type "uniform float")
				     (dy :type "uniform float")
					;(width :type "uniform int")
				     ;(height :type "uniform int")
				     ;(max_iterations :type "uniform int")
				     (output[] :type "uniform int")
				     (output_row_start :type "uniform int")
				     (output_col_start :type "uniform int")
				     (output_row_end :type "uniform int")
				     (output_col_end :type "uniform int"))
				    "export void")
		   (for ((j output_row_start :type "uniform int") (< j output_row_end) (+= j 1))
			(let ((y :type float :init (+ y0 (* j dy)))
			      (i0 :type "unsigned int" :init (* j ,width)))
			  (foreach (i output_col_start output_col_end)
				  (let ((x :type float :init (+ x0 (* i dx)))
					(index :type int :init (+ i i0))
					)
				    (;statements ;
				     setf (aref output index)
					  (funcall mandel x y #+nil max_iterations))))))))))
   (sb-ext:run-program "/usr/bin/clang-format" (list "-i" (namestring *main-ispc-filename*)))))



  #+nil
((dx :type float :init (* (- x1 x0) (/ 1.0 ,width)))
 (dy :type float :init (* (- y1 y0) (/ 1.0 ,height))))

