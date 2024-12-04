(in-package #:lisp-fsrs)

(deftype state ()
  '(member :new :learning :review :relearning))

(deftype rating ()
  '(member :again :hard :good :easy))

(declaim (ftype (function (rating) (integer 1 4)) rating-index)
         (inline rating-index))
(defun rating-index (rating)
  (ecase rating
    (:again 1)
    (:hard 2)
    (:good 3)
    (:easy 4)))

(defstruct review-log
  (rating :again :type rating)
  (scheduled-days 0 :type non-negative-fixnum)
  (elapsed-days 0 :type non-negative-fixnum)
  (review (now) :type timestamp)
  (state :new :type state))

(defstruct card
  (due (now) :type timestamp)
  (stability 0.0 :type non-negative-single-float)
  (difficulty 0.0 :type non-negative-single-float)
  (elapsed-days 0 :type non-negative-fixnum)
  (scheduled-days 0 :type non-negative-fixnum)
  (repeats 0 :type non-negative-fixnum)
  (lapses 0 :type non-negative-fixnum)
  (state :new :type state)
  (last-review nil :type (or timestamp null)))

(declaim (ftype (function ((or fixnum double-float)) (values fixnum)) seconds-days))
(defun seconds-days (secs)
  (nth-value 0 (truncate secs #.local-time::+seconds-per-day+)))

(declaim (type single-float +decay+ +factor+))
(defconstant +decay+ -0.5)
(defconstant +factor+ (1- (expt 0.9 (/ +decay+))))

(declaim (ftype (function (card &optional timestamp) (values (single-float 0.0 1.0))) card-retrievability))
(defun card-retrievability (self &optional (now (now)))
  (if (member (card-state self) '(:learning :review :relearning))
      (let ((elapsed-days (max 0 (seconds-days (timestamp-difference now (card-last-review self))))))
        (expt (1+ (/ (* +factor+ elapsed-days) (card-stability self))) +decay+))
      0.0))

(defstruct scheduling-info
  (card (make-card) :type card)
  (review-log (make-review-log) :type review-log))

(defstruct (scheduling-cards (:constructor %make-scheduling-cards))
  (again (make-card) :type card)
  (hard (make-card) :type card)
  (good (make-card) :type card)
  (easy (make-card) :type card))

(declaim (ftype (function (&key (:card card) (:again card) (:hard card) (:good card) (:easy card)) (values scheduling-cards)) make-scheduling-cards))
(defun make-scheduling-cards (&key (card (make-card)) (again (copy-card card)) (hard (copy-card card)) (good (copy-card card)) (easy (copy-card card)))
  (%make-scheduling-cards :again again :hard hard :good good :easy easy))

(declaim (ftype (function (scheduling-cards card timestamp) (values list)) scheduling-cards-record-log))
(defun scheduling-cards-record-log (self card now)
  (let ((again (scheduling-cards-again self))
        (hard (scheduling-cards-hard self))
        (good (scheduling-cards-good self))
        (easy (scheduling-cards-easy self)))
    (list :again (make-scheduling-info
                  :card again
                  :review-log (make-review-log
                               :rating :again
                               :scheduled-days (card-scheduled-days again)
                               :elapsed-days (card-elapsed-days card)
                               :review now
                               :state (card-state card)))
          :hard (make-scheduling-info
                 :card hard
                 :review-log (make-review-log
                              :rating :hard
                              :scheduled-days (card-scheduled-days hard)
                              :elapsed-days (card-elapsed-days card)
                              :review now
                              :state (card-state card)))
          :good (make-scheduling-info
                 :card good
                 :review-log (make-review-log
                              :rating :good
                              :scheduled-days (card-scheduled-days good)
                              :elapsed-days (card-elapsed-days card)
                              :review now
                              :state (card-state card)))
          :easy (make-scheduling-info
                 :card easy
                 :review-log (make-review-log
                              :rating :easy
                              :scheduled-days (card-scheduled-days easy)
                              :elapsed-days (card-elapsed-days card)
                              :review now
                              :state (card-state card))))))
