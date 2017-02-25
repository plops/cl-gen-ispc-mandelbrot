(push :ispc *features*) ;; for now i have to open cp.lisp and compile it again with C-c C-k, so that foreach works


(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload :cl-cpp-generator))

(in-package :cl-cpp-generator)


(let ((max-iterations 256)
      (width 768))
  (progn
   (defparameter *main-cpp-filename*  (merge-pathnames "stage/cl-gen-ispc/source/main.cpp"
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
	   (include "mandelbrot_ispc.h")
	   (function (main ()
			   int)
	    (let (;(width :type "unsigned int" :init 768)
		  (height :type "unsigned int" :init 512)
		  (x0 :type float :init -2.0)
		  (x1 :type float :init 1.0)
		  (y0 :type float :init -1.0)
		  (y1 :type float :init 1.0)
		  (buf :type "int*" ;"std::unique_ptr< int >"
		       :ctor (new (aref int (* ,width height)))
		       ))
	      (funcall "ispc::mandelbrot_ispc" x0 y0 x1 y1 #+nil width height buf)
	      (return 0))))))
   (sb-ext:run-program "/usr/bin/clang-format" (list "-i" (namestring *main-cpp-filename*))))

  (progn
   (defparameter *main-ispc-filename*  (merge-pathnames "stage/cl-gen-ispc/source/mandelbrot.ispc"
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
		  (count :type "const int" :init ,max-iterations)
		  (ret :type int :init 0))
	      (dotimes (i count)
		(let ((re2 :type float :init (* z_re z_re))
		      (im2 :type float :init (* z_im z_im)))
		  (if (< 4.0 (+ re2 im2))
		      (statements
		       (setf ret i)
		       (break)))
		  (let ((new_re :type float :init (- re2 im2))

			(new_im :type float :init (* 2.0 z_re z_im)))
		    (setf z_re (+ c_re new_re)
			  z_im (+ c_im new_im)))))
	      (return ret)))
	 (function (mandelbrot_ispc ((x0 :type "uniform float")
				     (y0 :type "uniform float")
				     (x1 :type "uniform float")
				     (y1 :type "uniform float")
				     ;(width :type "uniform int")
				     (height :type "uniform int")
				     ;(max_iterations :type "uniform int")
				     (output[] :type "uniform int"))
				    "export void")
		   (let ((dx :type float :init (/ (- x1 x0) ,width))
			 (dy :type float :init (/ (- y1 y0) height))
			 )
		     (for ((j 0 :type "uniform int") (< j height) (+= j 1))
		      (foreach (i 0 ,width)
			       (let ((x :type float :init (+ x0 (* i dx)))
				     (y :type float :init (+ y0 (* i dy)))
				     (index :type int :init (+ i (* j ,width)))
				     )
				 (setf (aref output index) (funcall mandel x y #+nil max_iterations))))))))))
   (sb-ext:run-program "/usr/bin/clang-format" (list "-i" (namestring *main-ispc-filename*)))))





