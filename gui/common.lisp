(in-package :gui)

(defparameter *elements* (make-hash-table))

(defmacro defelem (name &body elem)
  `(progn (defparameter ,name ,@elem) 
         (setf (gethash ',name *elements*) ,name)))

(defun get-ident ()
  #'(lambda (elem)
      (list elem)))

(defun get-elem (name)
  #'(lambda (elem)
      (synth-all :content 
		 (remove-if-not #'(lambda (prop) 
				    (eql name (synth :name prop)))
				(synth :props elem)))))

(defun compose-filter (f g)
  #'(lambda (elem)
      (let ((temps (funcall f elem))) ;;temps is a list of elem
	(apply #'append (mapcar #'(lambda (temp) 
				    (funcall g temp))
				temps)))))

(defun compose-filters (&rest filters)
  #'(lambda (elem)
      (funcall (reduce #'compose-filter filters) elem)))

;; (vert nil 
;;       (navbar nav (link home) (link nested))
;;       (alt nil (vert nil 
;;                      (panel panel-test 
;;                             (label nil)
;;                             (label nil))
;;                      (button test)
;;                      (table table ))))
