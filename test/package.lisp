(defpackage lisp-fsrs.test
  (:use #:c2cl #:parachute #:local-time #:fsrs)
  (:import-from #:alexandria #:define-constant)
  (:import-from #:fsrs #:weights #:scheduler-repeat)
  (:nicknames #:fsrs.test))

(in-package #:lisp-fsrs.test)

(define-constant +test-w+
    (coerce
     '(0.4197 1.1869 3.0412 15.2441 7.1434 0.6477 1.0007 0.0674 1.6597 0.1712 1.1178 2.0225 0.0904 0.3025 2.1214 0.2498 2.9466 0.4891 0.6468)
     'weights)
  :test #'equalp)

(define-constant +test-ratings+ '(:good :good :good :good :good :good :again :again :good :good :good :good :good) :test #'equal)

(declaim (ftype (function (single-float single-float) (values boolean)) float=))
(defun float= (a b)
  (< (abs (- a b)) 0.001))

(define-test test-lisp-fsrs)

(define-test test-review-card :parent test-lisp-fsrs
  (loop :with scheduler := (make-scheduler :weights +test-w+)
        :for now := (encode-timestamp 0 0 30 12 29 11 2022 :timezone +utc-zone+) :then (card-due card)
        :for rating :in +test-ratings+
        :for card := (nth-value 0 (scheduler-review-card scheduler (or card (make-card)) rating now))
        :for ivl := (card-scheduled-days card)
        :collect ivl :into ivl-history
        :finally (is equal '(0 4 17 62 198 563 0 0 9 27 74 190 457) ivl-history)))

(define-test test-memo-state :parent test-lisp-fsrs
  (loop :with scheduler := (make-scheduler :weights +test-w+)
        :for rating :in '(nil :again :good :good :good :good :good)
        :for ivl :in '(nil 0 0 1 3 8 21)
        :for card := (make-card) :then (scheduling-info-card (getf scheduling-cards rating))
        :for now := (encode-timestamp 0 0 30 12 29 11 2022 :timezone +utc-zone+) :then (timestamp+ now ivl :day)
        :for scheduling-cards := (scheduler-repeat scheduler card now)
        :finally
           (is float= 71.4554 (card-stability (scheduling-info-card (getf scheduling-cards :good))))
           (is float= 5.0976 (card-difficulty (scheduling-info-card (getf scheduling-cards :good))))))

(define-test test-repeat-default-arg :parent test-lisp-fsrs
  (let* ((scheduler (make-scheduler))
         (card-object (make-card))
         (scheduling-cards (scheduler-repeat scheduler card-object))
         (card-rating :good)
         (card-object (scheduling-info-card (getf scheduling-cards card-rating)))
         (due (card-due card-object))
         (time_delta (timestamp-difference due (now))))
    (is > 500 time_delta)))

(define-test test-datetime :parent test-lisp-fsrs
  (let ((scheduler (make-scheduler))
        (card (make-card)))
    (is timestamp<= (now) (card-due card))
    (let* ((scheduling-cards (scheduler-repeat scheduler card (now)))
           (card (scheduling-info-card (getf scheduling-cards :good))))
      (is timestamp<= (card-due card) (card-last-review card)))))

(defgeneric equals (a b) (:method (a b) (cl:equalp a b)))

(defmethod equals ((a timestamp) (b timestamp)) (timestamp= a b))

(defmethod equals ((a structure-object) (b structure-object))
  (let ((class (class-of a)))
    (when (eq class (class-of b))
      (loop :for slot :in (class-slots class)
            :for slot-name := (slot-definition-name slot)
            :always (equals (slot-value a slot-name) (slot-value b slot-name))))))

(define-test test-card-serialize :parent test-lisp-fsrs
  (enable-read-macros)
  (let* ((scheduler (make-scheduler))
         (card (make-card))
         (card-string (write-to-string card :readably t))
         (copied-card (read-from-string card-string)))
    (is equals card copied-card)
    (let* ((scheduling-cards (scheduler-repeat scheduler card))
           (repeated-card (scheduling-info-card (getf scheduling-cards :good)))
           (repeated-card-string (write-to-string repeated-card :readably t))
           (copied-repeat-card (read-from-string repeated-card-string)))
      (is equals repeated-card copied-repeat-card))))

(define-test test-review-log-serialize :parent test-lisp-fsrs
  (enable-read-macros)
  (let* ((scheduler (make-scheduler))
         (card (make-card))
         (scheduling-cards (scheduler-repeat scheduler card))
         (rating :again)
         (card (scheduling-info-card (getf scheduling-cards rating)))
         (review-log (scheduling-info-review-log (getf scheduling-cards rating)))
         (review-log-string (write-to-string review-log :readably t))
         (copied-review-log (read-from-string review-log-string)))
    (is equals review-log copied-review-log)
    (let* ((scheduling-cards (scheduler-repeat scheduler card))
           (rating :good)
           (card (scheduling-info-card (getf scheduling-cards rating)))
           (next-review-log (scheduling-info-review-log (getf scheduling-cards rating)))
           (next-review-log-string (write-to-string next-review-log :readably t))
           (copied-next-review-log (read-from-string next-review-log-string)))
      (declare (ignore card))
      (is equals next-review-log copied-next-review-log))))

(define-test test-custom-scheduler-args :parent test-lisp-fsrs
  (loop :with scheduler := (make-scheduler
                            :weights (coerce
                                      '(0.4197
                                        1.1869
                                        3.0412
                                        15.2441
                                        7.1434
                                        0.6477
                                        1.0007
                                        0.0674
                                        1.6597
                                        0.1712
                                        1.1178
                                        2.0225
                                        0.0904
                                        0.3025
                                        2.1214
                                        0.2498
                                        2.9466
                                        0.0
                                        0.6468)
                                      'weights)
                            :request-retention 0.9
                            :maximum-interval 36500)
        :for now := (encode-timestamp 0 0 30 12 29 11 2022 :timezone +utc-zone+) :then (card-due card)
        :for rating :in +test-ratings+
        :for card := (nth-value 0 (scheduler-review-card scheduler (or card (make-card)) rating now))
        :for ivl := (card-scheduled-days card)
        :collect ivl :into ivl-history
        :finally (is equal ivl-history '(0 3 13 50 163 473 0 0 12 34 91 229 541)))
  (let* ((weights-2 (coerce
                     '(0.1456
                       0.4186
                       1.1104
                       4.1315
                       5.2417
                       1.3098
                       0.8975
                       0.0000
                       1.5674
                       0.0567
                       0.9661
                       2.0275
                       0.1592
                       0.2446
                       1.5071
                       0.2272
                       2.8755
                       1.234
                       5.6789)
                     'weights))
         (request-retention-2 0.85)
         (maximum-interval-2 3650)
         (f2 (make-scheduler
              :weights weights-2
              :request-retention request-retention-2
              :maximum-interval maximum-interval-2)))
    (with-accessors ((p scheduler-parameters)) f2
      (is equals weights-2 (parameters-weights p))
      (is = request-retention-2 (parameters-request-retention p))
      (is = maximum-interval-2 (parameters-maximum-interval p)))))

(define-test test-retrievability :parent test-lisp-fsrs
  (let* ((scheduler (make-scheduler))
         (card (make-card))
         (retrievability (card-retrievability card)))
    (is eq :new (card-state card))
    (is = 0 retrievability)
    (setf card (nth-value 0 (scheduler-review-card scheduler card :good))
          retrievability (card-retrievability card))
    (is eq :learning (card-state card))
    (true (<= 0 (card-retrievability card) 1))
    (setf card (nth-value 0 (scheduler-review-card scheduler card :good))
          retrievability (card-retrievability card))
    (is eq :review (card-state card))
    (true (<= 0 (card-retrievability card) 1))
    (setf card (nth-value 0 (scheduler-review-card scheduler card :again))
          retrievability (card-retrievability card))
    (is eq :relearning (card-state card))
    (true (<= 0 (card-retrievability card) 1))))

(defun float-list-equal (a b)
  (when (= (length a) (length b))
    (every #'float= a b)))

(define-test test-long-term-scheduler :parent test-lisp-fsrs
  (loop :with scheduler := (make-scheduler :weights +test-w+ :enable-short-term-p nil)
        :for now := (encode-timestamp 0 0 30 12 29 11 2022 :timezone +utc-zone+) :then (card-due card)
        :for rating :in +test-ratings+
        :for card := (nth-value 0 (scheduler-review-card scheduler (or card (make-card)) rating now))
        :for ivl := (card-scheduled-days card)
        :collect ivl :into ivl-history
        :collect (card-stability card) :into stabilities
        :collect (card-difficulty card) :into difficulties
        :finally
           (is equal '(3 13 48 155 445 1158 17 3 9 27 74 190 457) ivl-history)
           (is float-list-equal '(3.0412 13.0913 48.1585 154.9373 445.0556 1158.0778 16.6306 2.9888 9.4633 26.9474 73.9723 189.7037 457.4379) stabilities)
           (is float-list-equal '(4.4909 4.2666 4.0575 3.8624 3.6804 3.5108 5.219 6.8122 6.4314 6.0763 5.7452 5.4363 5.1483) difficulties)))
