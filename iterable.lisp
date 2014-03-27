;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage iterable
  (:use :common-lisp)
  (:export
   #:iterable
   #:finished-iterating-p
   #:current-iterator
   #:reset-iteration
   #:resettable-p
   #:root
   #:next
   #:current
   #:future
   #:move
   #:with-fresh-iteration))

(defclass iterable ()
  ((finished-iterating-p
    :accessor finished-iterating-p
    :initform nil
    :documentation "Has the iterator finished its iteration?")
   (current-iterator
    :initform nil
    :accessor current-iterator
    :documentation "This is the current iterator's pointer"))
  (:documentation "The iterable class is an implementation of a
  sequence abstraction, popularized by Clojure. It is suitable to
  implement over infinite sized data structures or lazy data
  structures.

An imperative example:

  (loop
    until (finished-iterating-p data)
    do
       (some stuff with (current data))
       (next data))

And a functional example

  (defun some-iter (pred seq)
     (unless (finished-iterating-p seq)
        (or (funcall pred (current seq))
           (some-iter pred (future seq)))))

"))

(defgeneric reset-iteration (seq)
  (:documentation "Reset the current and future to the point of
  root. Reset-iteration should not be destructive to the data stream;
  see RESETTABLE-P to check."))

(defmethod reset-iteration :before ((iterable iterable))
  (setf (finished-iterating-p iterable) nil))

(defgeneric resettable-p (iterable)
  (:documentation "Is the iterable resettable? This should be t for
  file and memory data structures; network based or stream data
  structures should be nil. The default for the iterable hierarchy is t"))

(defmethod resettable-p ((iterable iterable))
  t)

(defgeneric root (seq)
  (:documentation "Head of the iterator. This is typically only useful
  for data sets which can be `rewound` "))

(defgeneric next (seq)
  (:documentation "Set the iterable to the next item, to be then
  available as `current`"))

(defgeneric current  (seq)
  (:documentation "Accessible value of the current 'top' of the
  iterator. This could be the current file pointer, or perhaps the
  current element in an infinite data stream"))

(defgeneric future (seq)
  (:documentation "Elements of the iteration visible from the `current`
  point"))

(defgeneric move (seq)
  (:documentation "A stepper function; get data, move to the next
  element. Returns the data gotten from current. "))

(defmethod move ((iter iterable))
  "A default move method"
  (let ((data (current iter)))
    (next iter)
    data))

(defmacro with-fresh-iteration ((iterator) &body body)
  "Pushes the current iteration point and the finished iteration state
and puts iterator into a start point state; completion or error pops
the state and returns to the calling iteration state."
  (let ((current-iteration (gensym))
        (finished-iterating (gensym)))

   `(let ((,current-iteration (current-iterator ,iterator))
          (,finished-iterating (finished-iterating-p ,iterator)))

      (unless (resettable-p ,iterator)
          (error "~a is not resettable; fresh iterations not possible" ,iterator))

      (reset-iteration ,iterator)
      (unwind-protect
           (progn
             ,@body)
        (setf (current-iterator ,iterator) ,current-iteration)
        (setf (finished-iterating-p ,iterator) ,finished-iterating)))))
