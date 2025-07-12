(defpackage lisp-fsrs.test
  (:use #:c2cl #:parachute #:local-time #:fsrs)
  (:import-from #:alexandria #:define-constant)
  (:import-from #:fsrs #:seconds-days #:scheduler-maximum-interval-days #:+minimum-stability+)
  (:nicknames #:fsrs.test))

(in-package #:lisp-fsrs.test)

(define-constant +default-parameters+
    (coerce
     '(0.2172 1.1771 3.2602 16.1507 7.0114 0.57 2.0966 0.0069 1.5261
       0.112 1.0178 1.849 0.1133 0.3127 2.2934 0.2191 3.0004 0.7536
       0.3332 0.1437 0.2)
     'parameters)
  :test #'equalp)

(define-constant +test-ratings+ '(:good :good :good :good :good :good :again :again :good :good :good :good :good) :test #'equal)

(declaim (ftype (function (single-float single-float) (values boolean)) float=))
(defun float= (a b)
  (< (abs (- a b)) 0.001))

(defun float-list-equal (a b)
  (when (= (length a) (length b))
    (every #'float= a b)))

(define-test test-lisp-fsrs)

(define-test test-review-card :parent test-lisp-fsrs
  (loop :with scheduler := (make-scheduler :parameters +default-parameters+ :enable-fuzzing-p nil)
        :for now := (encode-timestamp 0 0 30 12 29 11 2022 :timezone +utc-zone+) :then (card-due card)
        :for rating :in +test-ratings+
        :for card := (nth-value 0 (scheduler-review-card scheduler (or card (make-card)) rating now))
        :for ivl := (seconds-days (timestamp-difference (card-due card) now))
        :collect ivl :into ivl-history
        :finally (is equal '(0 4 14 45 135 372 0 0 2 5 10 20 40) ivl-history)))

(define-test test-memo-state :parent test-lisp-fsrs
  (loop :with scheduler := (make-scheduler :parameters +default-parameters+)
        :for ivl :in '(0 0 1 3 8 21)
        :for now := (timestamp+ (or now (encode-timestamp 0 0 30 12 29 11 2022 :timezone +utc-zone+)) ivl :day)
        :for rating :in '(:again :good :good :good :good :good)
        :for card := (nth-value 0 (scheduler-review-card scheduler (or card (make-card)) rating now))
        :finally
           (setf card (nth-value 0 (scheduler-review-card scheduler card :good now)))
           (is float= 49.4472 (card-stability card))
           (is float= 6.8271 (card-difficulty card))))

(defgeneric equals (a b) (:method (a b) (cl:equalp a b)))

(defmethod equals ((a timestamp) (b timestamp)) (timestamp= a b))

(defmethod equals ((a structure-object) (b structure-object))
  (let ((class (class-of a)))
    (when (eq class (class-of b))
      (loop :for slot :in (class-slots class)
            :for slot-name := (slot-definition-name slot)
            :always (equals (slot-value a slot-name) (slot-value b slot-name))))))

(define-test test-custom-scheduler-args :parent test-lisp-fsrs
  (loop :with scheduler := (make-scheduler :desired-retention 0.9
                                           :maximum-interval '(36500 :day)
                                           :enable-fuzzing-p nil
                                           :parameters +default-parameters+)
        :for rating :in +test-ratings+
        :for now := (encode-timestamp 0 0 30 12 29 11 2022 :timezone +utc-zone+) :then (card-due card)
        :for card := (nth-value 0 (scheduler-review-card scheduler (or card (make-card)) rating now))
        :collect (seconds-days (timestamp-difference (card-due card) (card-last-review card))) :into ivl-history
        :finally (is equal '(0 4 14 45 135 372 0 0 2 5 10 20 40) ivl-history))
  (let* ((parameters2 (coerce '(0.1456 0.4186 1.1104 4.1315 5.2417 1.3098 0.8975 0.0010 1.5674 0.0567 0.9661 2.0275 0.1592 0.2446 1.5071 0.2272 2.8755 1.234 0.56789 0.1437 0.2) 'parameters))
         (desired-retention2 0.85)
         (maximum-interval2 3650)
         (scheduler2 (make-scheduler :parameters parameters2 :desired-retention desired-retention2 :maximum-interval (list maximum-interval2 :day))))
    (is equalp parameters2 (scheduler-parameters scheduler2))
    (is = desired-retention2 (scheduler-desired-retention scheduler2))
    (is = maximum-interval2 (scheduler-maximum-interval-days scheduler2))))

(define-test test-retrievability :parent test-lisp-fsrs
  (let ((scheduler (make-scheduler)) (card (make-card)))
    (is eq :learning (card-state card))
    (is = 0.0 (scheduler-card-retrievability scheduler card))

    (setf card (nth-value 0 (scheduler-review-card scheduler card :good)))
    (is eq :learning (card-state card))
    (is >= 0.0 (scheduler-card-retrievability scheduler card))
    (is <= 1.0 (scheduler-card-retrievability scheduler card))

    (setf card (nth-value 0 (scheduler-review-card scheduler card :good)))
    (is eq :review (card-state card))
    (is >= 0.0 (scheduler-card-retrievability scheduler card))
    (is <= 1.0 (scheduler-card-retrievability scheduler card))

    (setf card (nth-value 0 (scheduler-review-card scheduler card :again)))
    (is eq :relearning (card-state card))
    (is >= 0.0 (scheduler-card-retrievability scheduler card))
    (is <= 1.0 (scheduler-card-retrievability scheduler card))))

(define-test test-good-learning-steps :parent test-lisp-fsrs
  (let ((scheduler (make-scheduler))
        (created-at (now))
        (card (make-card)))
    (is eq :learning (card-state card))
    (is = 0 (card-step card))

    (setf card (nth-value 0 (scheduler-review-card scheduler card :good (card-due card))))
    (is eq :learning (card-state card))
    (is = 1 (card-step card))
    (is = 6 (round (timestamp-difference (card-due card) created-at) 100))

    (setf card (nth-value 0 (scheduler-review-card scheduler card :good (card-due card))))
    (is eq :review (card-state card))
    (true (null (card-step card)))
    (is >= 24 (round (timestamp-difference (card-due card) created-at) 3600))))

(define-test test-again-learning-steps :parent test-lisp-fsrs
  (let* ((scheduler (make-scheduler))
         (created-at (now))
         (card (make-card)))
    (is eq :learning (card-state card))
    (is = 0 (card-step card))

    (setf card (nth-value 0 (scheduler-review-card scheduler card :again (card-due card))))
    (is eq :learning (card-state card))
    (is = 0 (card-step card))
    (is = 6 (round (timestamp-difference (card-due card) created-at) 10))))

(define-test test-hard-learning-steps :parent test-lisp-fsrs
  (let* ((scheduler (make-scheduler))
         (created-at (now))
         (card (make-card)))
    (is eq :learning (card-state card))
    (is = 0 (card-step card))

    (setf card (nth-value 0 (scheduler-review-card scheduler card :hard (card-due card))))
    (is eq :learning (card-state card))
    (is = 0 (card-step card))
    (is = 33 (round (timestamp-difference (card-due card) created-at) 10))))

(define-test test-easy-learning-steps :parent test-lisp-fsrs
  (let* ((scheduler (make-scheduler))
         (created-at (now))
         (card (make-card)))
    (is eq :learning (card-state card))
    (is = 0 (card-step card))

    (setf card (nth-value 0 (scheduler-review-card scheduler card :easy (card-due card))))
    (is eq :review (card-state card))
    (true (null (card-step card)))
    (is >= 1 (round (timestamp-difference (card-due card) created-at) 86400))))

(define-test test-review-state :parent test-lisp-fsrs
  (let* ((scheduler (make-scheduler :enable-fuzzing-p nil))
         (card (make-card)))
    (setf card (nth-value 0 (scheduler-review-card scheduler card :good (card-due card))))
    (setf card (nth-value 0 (scheduler-review-card scheduler card :good (card-due card))))
    (is eq :review (card-state card))
    (true (null (card-step card)))

    (let ((prev-due (card-due card)))
      (setf card (nth-value 0 (scheduler-review-card scheduler card :good (card-due card))))
      (is eq :review (card-state card))
      (is >= 24 (round (timestamp-difference (card-due card) prev-due) 3600)))

    (let ((prev-due (card-due card)))
      (setf card (nth-value 0 (scheduler-review-card scheduler card :again (card-due card))))
      (is eq :relearning (card-state card))
      (is = 10 (round (timestamp-difference (card-due card) prev-due) 60)))))

(define-test test-relearning :parent test-lisp-fsrs
  (let* ((scheduler (make-scheduler :enable-fuzzing-p nil))
         (card (make-card)))
    (setf card (nth-value 0 (scheduler-review-card scheduler card :good (card-due card))))
    (setf card (nth-value 0 (scheduler-review-card scheduler card :good (card-due card))))
    (setf card (nth-value 0 (scheduler-review-card scheduler card :good (card-due card))))
    (let ((prev-due (card-due card)))
      (setf card (nth-value 0 (scheduler-review-card scheduler card :again (card-due card))))
      (is eq :relearning (card-state card))
      (is = 0 (card-step card))
      (is = 10 (round (timestamp-difference (card-due card) prev-due) 60)))

    (let ((prev-due (card-due card)))
      (setf card (nth-value 0 (scheduler-review-card scheduler card :again (card-due card))))
      (is eq :relearning (card-state card))
      (is = 0 (card-step card))
      (is = 10 (round (timestamp-difference (card-due card) prev-due) 60)))

    (let ((prev-due (card-due card)))
      (setf card (nth-value 0 (scheduler-review-card scheduler card :good (card-due card))))
      (is eq :review (card-state card))
      (true (null (card-step card)))
      (is >= 24 (round (timestamp-difference (card-due card) prev-due) 3600)))))

(define-test test-no-learning-steps :parent test-lisp-fsrs
  (let ((scheduler (make-scheduler :learning-steps nil)))
    (true (null (scheduler-learning-steps scheduler)))
    (let ((card (make-card)))
      (setf card (nth-value 0 (scheduler-review-card scheduler card :again)))
      (is eq :review (card-state card))
      (is >= 1 (seconds-days (timestamp-difference (card-due card) (card-last-review card)))))))

(define-test test-no-relearning-steps :parent test-lisp-fsrs
  (let ((scheduler (make-scheduler :relearning-steps nil)))
    (true (null (scheduler-relearning-steps scheduler)))
    (let ((card (make-card)))
      (setf card (nth-value 0 (scheduler-review-card scheduler card :good)))
      (is eq :learning (card-state card))
      (setf card (nth-value 0 (scheduler-review-card scheduler card :good (card-due card))))
      (is eq :review (card-state card))
      (setf card (nth-value 0 (scheduler-review-card scheduler card :again (card-due card))))
      (is eq :review (card-state card))
      (is >= 1 (seconds-days (timestamp-difference (card-due card) (card-last-review card)))))))

(define-test test-one-card-multiple-schedulers :parent test-lisp-fsrs
  (let* ((scheduler-two-learning (make-scheduler :learning-steps '((1 :minute) (10 :minute))))
         (scheduler-one-learning (make-scheduler :learning-steps '((1 :minute))))
         (scheduler-no-learning (make-scheduler :learning-steps nil))
         (scheduler-two-relearning (make-scheduler :relearning-steps '((1 :minute) (10 :minute))))
         (scheduler-one-relearning (make-scheduler :relearning-steps '((1 :minute))))
         (scheduler-no-relearning (make-scheduler :relearning-steps nil))
         (card (make-card)))
    (is = 2 (length (scheduler-learning-steps scheduler-two-learning)))
    (setf card (nth-value 0 (scheduler-review-card scheduler-two-learning card :good)))
    (is eq :learning (card-state card))
    (is = 1 (card-step card))

    (is = 1 (length (scheduler-learning-steps scheduler-one-learning)))
    (setf card (nth-value 0 (scheduler-review-card scheduler-one-learning card :again)))
    (is eq :learning (card-state card))
    (is = 0 (card-step card))

    (true (null (scheduler-learning-steps scheduler-no-learning)))
    (setf card (nth-value 0 (scheduler-review-card scheduler-no-learning card :hard)))
    (is eq :review (card-state card))
    (true (null (card-step card)))

    (is = 2 (length (scheduler-relearning-steps scheduler-two-relearning)))
    (setf card (nth-value 0 (scheduler-review-card scheduler-two-relearning card :again)))
    (is eq :relearning (card-state card))
    (is = 0 (card-step card))

    (setf card (nth-value 0 (scheduler-review-card scheduler-two-relearning card :good)))
    (is eq :relearning (card-state card))
    (is = 1 (card-step card))

    (is = 1 (length (scheduler-relearning-steps scheduler-one-relearning)))
    (setf card (nth-value 0 (scheduler-review-card scheduler-one-relearning card :again)))
    (is eq :relearning (card-state card))
    (is = 0 (card-step card))

    (true (null (scheduler-relearning-steps scheduler-no-relearning)))
    (setf card (nth-value 0 (scheduler-review-card scheduler-no-relearning card :hard)))
    (is eq :review (card-state card))
    (true (null (card-step card)))))

(define-test test-maximum-interval :parent test-lisp-fsrs
  (loop :with scheduler := (make-scheduler :maximum-interval '(100 :day))
        :repeat 10
        :for now := (now) :then (card-due card)
        :for card := (nth-value 0 (scheduler-review-card scheduler (or card (make-card)) :easy now))
        :do (is <= 100 (seconds-days (timestamp-difference (card-due card) (card-last-review card))))))

(define-test test-stability-lower-bound :parent test-lisp-fsrs
  (loop :with scheduler := (make-scheduler)
        :repeat 1000
        :for now := (now) :then (timestamp+ (card-due card) 1 :day)
        :for card := (nth-value 0 (scheduler-review-card scheduler (or card (make-card)) :again now))
        :do (is >= +minimum-stability+ (card-stability card))))

(define-test test-scheduler-parameter-validation :parent test-lisp-fsrs
  (of-type scheduler (make-scheduler :parameters +default-parameters+))

  (let ((params (copy-seq +default-parameters+)))
    (setf (aref params 6) 100.0)
    (fail (make-scheduler :parameters params)))

  (let ((params (copy-seq +default-parameters+)))
    (setf (aref params 10) -42.0)
    (fail (make-scheduler :parameters params)))

  (let ((params (copy-seq +default-parameters+)))
    (setf (aref params 0) 0.0)
    (setf (aref params 3) 101.0)
    (fail (make-scheduler :parameters params)))

  (fail (make-scheduler :parameters #()))

  (let ((params (make-array (1- (length +default-parameters+))
                            :element-type 'single-float)))
    (fail (make-scheduler :parameters params)))

  (let ((params (make-array (+ (length +default-parameters+) 3)
                            :element-type 'single-float)))
    (fail (make-scheduler :parameters params))))
