;; Copyright (c) 2009, 2010 Vitaly Mayatskikh <v.mayatskih@gmail.com>
;;
;; This file is part of CL-ZMQ.
;;
;; Vitaly Mayatskikh grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :zeromq)

(defparameter *retry-on-eintr* t
  "Whether cl-zmq should retry automatically on EINTR error. This should
  typically be the case, however it is possible that the user code wants to
  handle this error ")

(define-condition error-base (error)
  ((errno :reader error-errno :initarg :argument))
  (:report (lambda (condition stream)
             (write-string (convert-from-foreign
                            (%strerror (error-errno condition))
                            :string)
                           stream))))

;; for compatibility with older code that might use the `error-again' accessor
(defun error-again (err)
  (error-errno err))

(define-condition error-again (error-base)
  ())

(define-condition error-eintr (error-again)
  ())

(defmacro defcfun* (name-and-options return-type &body args)
  (let* ((c-name (car name-and-options))
         (l-name (cadr name-and-options))
         (n-name (cffi::format-symbol t "%~A" l-name))
         (name (list c-name n-name))

         (docstring (when (stringp (car args)) (pop args)))
         (ret (gensym))
         (retry (gensym))
         (errno (gensym)))
    (loop with opt
       for i in args
       unless (consp i) do (setq opt t)
       else
       collect i into args*
       and if (not opt) collect (car i) into names
       else collect (car i) into opts
       and collect (list (car i) 0) into opts-init
       end
       finally (return
                 `(progn
                    (defcfun ,name ,return-type
                      ,@args*)
                    (defun ,l-name (,@names ,@(when opts-init `(&optional ,@opts-init)))
                      ,docstring
                      (let (,ret)
                        (tagbody ,retry
                           (setf ,ret (,n-name ,@names ,@opts))
                           (if ,(if (eq return-type :pointer)
                                    `(zerop (pointer-address ,ret))
                                    `(not (zerop ,ret)))
                               (let ((,errno (errno)))
                                 (cond
                                   #-windows
                                   ((eq ,errno isys:ewouldblock) (error 'error-again :argument ,errno))
                                   ((eq ,errno isys:eintr)
                                    (unless *retry-on-eintr*
                                      (cerror "Retry" 'error-eintr :argument ,errno))
                                    (go ,retry))
                                   (t (error (convert-from-foreign (%strerror ,errno) :string)))))))
                        ,ret)))))))
