(in-package #:lisp-fsrs)

(defstruct (scheduler (:constructor nil))
  "Abstract base type for FSRS scheduling strategies.

PARAMETERS holds model configuration and weights."
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
  "Create concrete scheduler instance with specified parameters.

PARAMETERS can override default model weights and settings.
ENABLE-SHORT-TERM-P selects between learning phase strategies. Other
ARGS is passed to MAKE-PARAMETERS as given.

Returns BASIC-SCHEDULER if true, LONG-TERM-SCHEDULER otherwise."
  (funcall (if enable-short-term-p 'make-basic-scheduler 'make-long-term-scheduler) :parameters parameters))

(declaim (ftype (function (scheduler scheduling-cards)) scheduler-init-ds))
(defun scheduler-init-ds (self s)
  "Initialize difficulty/stability for new cards in SCHEDULING-CARDS.

SELF is SCHEDULER instance. S contains card variants to initialize.

Sets values for all rating paths using PARAMETERS."
  (let ((parameters (scheduler-parameters self)))
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
            (card-stability easy) (parameters-init-stability parameters :easy)))))

(declaim (ftype (function (scheduler scheduling-cards card)) scheduler-next-ds))
(defun scheduler-next-ds (self s card)
  "Update difficulty/stability for existing cards after review.

SELF is SCHEDULER instance. S contains card variants to update. CARD
is previous state before review.

Applies different stability formulas for learning/review phases."
  (let ((parameters (scheduler-parameters self)))
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
                 (card-stability easy) (parameters-next-recall-stability parameters last-d last-s retrievability :easy))))))))

(defgeneric scheduler-repeat (self card &optional now)
  (:documentation
   "Generate scheduling options after card review.

SELF is SCHEDULER instance. CARD is item being rescheduled.
NOW is current timestamp.

Returns updated SCHEDULING-CARDS with new intervals and states."))

(declaim (ftype (function (scheduler card rating &optional timestamp) (values card review-log)) scheduler-review-card))
(defun scheduler-review-card (self card rating &optional (now (now)))
  "Process user review rating and update card accordingly.

SELF is SCHEDULER instance. CARD is item being reviewed.
RATING is user's response (:again/:hard/:good/:easy). NOW is review
timestamp.

Returns updated CARD and REVIEW-LOG for record-keeping."
  (let* ((scheduling-cards (scheduler-repeat self card now))
         (scheduling-info (getf scheduling-cards rating))
         (card (scheduling-info-card scheduling-info))
         (review-log (scheduling-info-review-log scheduling-info)))
    (values card review-log)))
