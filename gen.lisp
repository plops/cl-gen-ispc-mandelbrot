(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload :cl-cpp-generator))

(in-package :cl-cpp-generator)



(defmacro function-prefix (prefix &body body)
  `(with-compilation-unit
       ,@(mapcar
	  (lambda (x)
	    (if (eq 'function (first x))
		(destructuring-bind (fun_ (name params &optional ret &key ctor specifier)
					  &rest function-body) x
		  `(function (,(intern (string-upcase (format nil "~a_~a" prefix name)))
			       ,params ,ret :ctor ,ctor :specifier ,specifier)
			     ,@function-body))
		x)) body)))


(progn
  (defparameter *main-cpp-filename*  (merge-pathnames "stage/cl-gen-ispc/source/main.ispc"
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
	 
       )))
  (sb-ext:run-program "/usr/bin/clang-format" (list "-i" (namestring *main-cpp-filename*))))





