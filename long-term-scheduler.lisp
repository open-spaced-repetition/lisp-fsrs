(in-package #:lisp-fsrs)

(defstruct (long-term-scheduler (:include scheduler)))

(declaim (ftype (function (long-term-scheduler scheduling-cards state)) long-term-scheduler-update-state))
(defun long-term-scheduler-update-state (self cards state)
  (declare (ignore self state))
  (let ((again (scheduling-cards-again cards))
        (hard (scheduling-cards-hard cards))
        (good (scheduling-cards-good cards))
        (easy (scheduling-cards-easy cards)))
    (setf (card-state again) :review
          (card-state hard) :review
          (card-state good) :review
          (card-state easy) :review)))

(declaim (ftype (function (long-term-scheduler scheduling-cards timestamp non-negative-fixnum non-negative-fixnum non-negative-fixnum non-negative-fixnum)) long-term-scheduler-schedule))
(defun long-term-scheduler-schedule (self cards now again-interval hard-interval good-interval easy-interval)
  (declare (ignore self))
  (let ((again (scheduling-cards-again cards))
        (hard (scheduling-cards-hard cards))
        (good (scheduling-cards-good cards))
        (easy (scheduling-cards-easy cards)))
    (setf (card-scheduled-days again) again-interval
          (card-scheduled-days hard) hard-interval
          (card-scheduled-days good) good-interval
          (card-scheduled-days easy) easy-interval
          (card-due again) (timestamp+ now again-interval :day)
          (card-due hard) (timestamp+ now hard-interval :day)
          (card-due good) (timestamp+ now good-interval :day)
          (card-due easy) (timestamp+ now easy-interval :day))))

(defmethod scheduler-repeat ((self long-term-scheduler) card &optional (now (now)))
  (let ((card (copy-card card))
        (parameters (scheduler-parameters self)))
    (setf (card-elapsed-days card) (if (eq (card-state card) :new) 0 (seconds-days (timestamp-difference now (card-last-review card))))
          (card-last-review card) now)
    (incf (card-repeats card))
    (let ((s (make-scheduling-cards :card card)))
      (long-term-scheduler-update-state self s (card-state card))
      (let ((again (scheduling-cards-again s))
            (hard (scheduling-cards-hard s))
            (good (scheduling-cards-good s))
            (easy (scheduling-cards-easy s)))
        (if (eq (card-state card) :new)
            (scheduler-init-ds self s)
            (scheduler-next-ds self s card))
        (let* ((again-interval (parameters-next-interval parameters (card-stability again)))
               (hard-interval (parameters-next-interval parameters (card-stability hard)))
               (good-interval (parameters-next-interval parameters (card-stability good)))
               (easy-interval (parameters-next-interval parameters (card-stability easy)))
               (again-interval (min again-interval hard-interval))
               (hard-interval (max hard-interval (1+ again-interval)))
               (good-interval (max good-interval (1+ hard-interval)))
               (easy-interval (max easy-interval (1+ good-interval))))
          (long-term-scheduler-schedule self s now again-interval hard-interval good-interval easy-interval)))
      (scheduling-cards-record-log s card now))))
