(push :ispc *features*) ;; for now i have to open cp.lisp and compile it again with C-c C-k, so that foreach works


(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload :cl-cpp-generator))

(in-package :cl-cpp-generator)

(defmacro e (&body body)
  `(statements (<< "std::cout" ,@(loop for e in body collect
                                      (cond ((stringp e) `(string ,e))
                                            (t e))) "std::endl")))


(let ((max-iterations 256)
      (width 512)
      (height 512)
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
	 (extern-c
	  (function (ISPCInstrument ((fn :type "const char*")
				     (note :type "const char*")
				     (line :type int)
				     (mask :type uint64_t)) void)
		    (macroexpand (e fn (string ":") line (string " - ") note (string ", 0x") |STD::HEX| mask ))))
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
	    (let ((width :type "const unsigned int" :init ,width)
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
	      
	      (dotimes (i
			 5000)
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

