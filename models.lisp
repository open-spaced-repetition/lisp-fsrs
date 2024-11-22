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
  (scheduled-days 0 :type fixnum)
  (elapsed-days 0 :type fixnum)
  (review (now) :type timestamp)
  (state :new :type state))

(defstruct card
  (due (now) :type timestamp)
  (stability 0.0 :type single-float)
  (difficulty 0.0 :type single-float)
  (elapsed-days 0 :type fixnum)
  (scheduled-days 0 :type fixnum)
  (repeats 0 :type fixnum)
  (lapses 0 :type fixnum)
  (state :new :type state)
  (last-review nil :type (or timestamp null)))

(declaim (ftype (function ((or fixnum double-float)) (values fixnum)) seconds-days)
         (inline seconds-days))
(defun seconds-days (secs)
  (nth-value 0 (truncate secs #.local-time::+seconds-per-day+)))

(declaim (type single-float +decay+ +factor+))
(defconstant +decay+ -0.5)
(defconstant +factor+ (1- (expt 0.9 (/ +decay+))))

(declaim (ftype (function (card &optional timestamp) (values single-float)) card-retrievability))
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

(declaim (ftype (function (scheduling-cards state)) scheduling-cards-update-state))
(defun scheduling-cards-update-state (self state)
  (let ((again (scheduling-cards-again self))
        (hard (scheduling-cards-hard self))
        (good (scheduling-cards-good self))
        (easy (scheduling-cards-easy self)))
    (ecase state
      (:new
       (setf (card-state again) :learning
             (card-state hard) :learning
             (card-state good) :learning
             (card-state easy) :review))
      ((:learning :relearning)
       (setf (card-state again) state
             (card-state hard) state
             (card-state good) :review
             (card-state easy) :review))
      (:review
       (setf (card-state again) :relearning
             (card-state hard) :review
             (card-state good) :review
             (card-state easy) :review)
       (incf (card-lapses again))))))

(declaim (ftype (function (scheduling-cards timestamp fixnum fixnum fixnum)) scheduling-cards-schedule))
(defun scheduling-cards-schedule (self now hard-interval good-interval easy-interval)
  (let ((again (scheduling-cards-again self))
        (hard (scheduling-cards-hard self))
        (good (scheduling-cards-good self))
        (easy (scheduling-cards-easy self)))
    (setf (card-scheduled-days again) 0
          (card-scheduled-days hard) hard-interval
          (card-scheduled-days good) good-interval
          (card-scheduled-days easy) easy-interval
          (card-due again) (timestamp+ now 5 :minute)
          (card-due hard) (if (plusp hard-interval)
                              (timestamp+ now hard-interval :day)
                              (timestamp+ now 10 :minute))
          (card-due good) (timestamp+ now good-interval :day)
          (card-due easy) (timestamp+ now easy-interval :day))))

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

(define-constant +weights-default+
    (coerce
     '(0.4072 1.1829 3.1262 15.4722 7.2102 0.5316 1.0651 0.0234 1.616 0.1544 1.0824 1.9813 0.0953 0.2975 2.2042 0.2407 2.9466 0.5034 0.6567)
     '(simple-array single-float (19)))
  :test #'equalp)

(defstruct parameters
  (request-retention 0.9 :type single-float)
  (maximum-interval 36500 :type fixnum)
  (weights +weights-default+ :type (simple-array single-float (19))))
