(in-package #:lisp-fsrs)

(defstruct (basic-scheduler (:include scheduler)))

(declaim (ftype (function (basic-scheduler scheduling-cards state)) basic-scheduler-update-state))
(defun basic-scheduler-update-state (self cards state)
  (declare (ignore self))
  (let ((again (scheduling-cards-again cards))
        (hard (scheduling-cards-hard cards))
        (good (scheduling-cards-good cards))
        (easy (scheduling-cards-easy cards)))
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

(declaim (ftype (function (basic-scheduler scheduling-cards timestamp non-negative-fixnum non-negative-fixnum non-negative-fixnum non-negative-fixnum)) basic-scheduler-schedule))
(defun basic-scheduler-schedule (self cards now again-interval hard-interval good-interval easy-interval)
  (declare (ignore self again-interval))
  (let ((again (scheduling-cards-again cards))
        (hard (scheduling-cards-hard cards))
        (good (scheduling-cards-good cards))
        (easy (scheduling-cards-easy cards)))
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

(defmethod scheduler-repeat ((self basic-scheduler) card &optional (now (now)))
  (let ((card (copy-card card))
        (parameters (scheduler-parameters self)))
    (setf (card-elapsed-days card) (if (eq (card-state card) :new) 0 (seconds-days (timestamp-difference now (card-last-review card))))
          (card-last-review card) now)
    (incf (card-repeats card))
    (let ((s (make-scheduling-cards :card card)))
      (basic-scheduler-update-state self s (card-state card))
      (let ((again (scheduling-cards-again s))
            (hard (scheduling-cards-hard s))
            (good (scheduling-cards-good s))
            (easy (scheduling-cards-easy s)))
        (ecase (card-state card)
          (:new
           (scheduler-init-ds self s)
           (setf (card-due again) (timestamp+ now 1 :minute)
                 (card-due hard) (timestamp+ now 5 :minute)
                 (card-due good) (timestamp+ now 10 :minute))
           (let ((easy-interval (parameters-next-interval parameters (card-stability easy))))
             (setf (card-scheduled-days easy) easy-interval
                   (card-due easy) (timestamp+ now easy-interval :day))))
          ((:learning :relearning)
           (scheduler-next-ds self s card)
           (let* ((hard-interval 0)
                  (good-interval (parameters-next-interval parameters (card-stability good)))
                  (easy-interval (max (parameters-next-interval parameters (card-stability easy)) (1+ good-interval))))
             (basic-scheduler-schedule self s now 0 hard-interval good-interval easy-interval)))
          (:review
           (scheduler-next-ds self s card)
           (let* ((hard-interval (parameters-next-interval parameters (card-stability hard)))
                  (good-interval (parameters-next-interval parameters (card-stability good)))
                  (hard-interval (min hard-interval good-interval))
                  (good-interval (max good-interval (1+ hard-interval)))
                  (easy-interval (max (parameters-next-interval parameters (card-stability easy)) (1+ good-interval))))
             (basic-scheduler-schedule self s now 0 hard-interval good-interval easy-interval)))))
      (scheduling-cards-record-log s card now))))
