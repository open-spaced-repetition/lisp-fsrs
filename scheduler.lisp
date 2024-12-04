(in-package #:lisp-fsrs)

(defstruct (scheduler (:constructor nil))
  (parameters (make-parameters) :type parameters))

(defun make-scheduler (&rest
                         args
                       &key
                         (parameters
                          (progn
                            (remf args :parameters)
                            (remf args :enable-short-term-p)
                            (apply #'make-parameters args)))
                         (enable-short-term-p t)
                       &allow-other-keys)
  (funcall (if enable-short-term-p 'make-basic-scheduler 'make-long-term-scheduler) :parameters parameters))

(declaim (ftype (function (scheduler scheduling-cards)) scheduler-init-ds))
(defun scheduler-init-ds (self s &aux (parameters (scheduler-parameters self)))
  (let ((again (scheduling-cards-again s))
        (hard (scheduling-cards-hard s))
        (good (scheduling-cards-good s))
        (easy (scheduling-cards-easy s)))
    (setf (card-difficulty again) (parameters-init-difficulty parameters :again)
          (card-stability again) (parameters-init-stability parameters :again)
          (card-difficulty hard) (parameters-init-difficulty parameters :hard)
          (card-stability hard) (parameters-init-stability parameters :hard)
          (card-difficulty good) (parameters-init-difficulty parameters :good)
          (card-stability good) (parameters-init-stability parameters :good)
          (card-difficulty easy) (parameters-init-difficulty parameters :easy)
          (card-stability easy) (parameters-init-stability parameters :easy))))

(declaim (ftype (function (scheduler scheduling-cards card)) scheduler-next-ds))
(defun scheduler-next-ds (self s card &aux (parameters (scheduler-parameters self)))
  (let* ((last-d (card-difficulty card))
         (last-s (card-stability card))
         (interval (card-elapsed-days card))
         (retrievability (parameters-forgetting-curve parameters interval last-s))
         (state (card-state card)))
    (let ((again (scheduling-cards-again s))
          (hard (scheduling-cards-hard s))
          (good (scheduling-cards-good s))
          (easy (scheduling-cards-easy s)))
      (setf (card-difficulty again) (parameters-next-difficulty parameters last-d :again)
            (card-difficulty hard) (parameters-next-difficulty parameters last-d :hard)
            (card-difficulty good) (parameters-next-difficulty parameters last-d :good)
            (card-difficulty easy) (parameters-next-difficulty parameters last-d :easy))
      (ecase state
        ((:learning :relearning)
         (setf (card-stability again) (parameters-short-term-stability parameters last-s :again)
               (card-stability hard) (parameters-short-term-stability parameters last-s :hard)
               (card-stability good) (parameters-short-term-stability parameters last-s :good)
               (card-stability easy) (parameters-short-term-stability parameters last-s :easy)))
        (:review
         (setf (card-stability again) (parameters-next-forget-stability parameters last-d last-s retrievability)
               (card-stability hard) (parameters-next-recall-stability parameters last-d last-s retrievability :hard)
               (card-stability good) (parameters-next-recall-stability parameters last-d last-s retrievability :good)
               (card-stability easy) (parameters-next-recall-stability parameters last-d last-s retrievability :easy)))))))

(defgeneric scheduler-repeat (self card &optional now))

(declaim (ftype (function (scheduler card rating &optional timestamp) (values card review-log)) scheduler-review-card))
(defun scheduler-review-card (self card rating &optional (now (now)))
  (let* ((scheduling-cards (scheduler-repeat self card now))
         (scheduling-info (getf scheduling-cards rating))
         (card (scheduling-info-card scheduling-info))
         (review-log (scheduling-info-review-log scheduling-info)))
    (values card review-log)))
