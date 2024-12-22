;;; fsrs.el --- Emacs Lisp Package for FSRS -*- lexical-binding: t -*-

;;; Commentary:

;; This file was auto-transpiled from package.lisp, models.lisp, parameters.lisp, scheduler.lisp, basic-scheduler.lisp, long-term-scheduler.lisp.

;;; Code:

(require 'cl-lib)

(require 'parse-time)

(cl-deftype fsrs-timestamp nil 'string)

(cl-defun fsrs-now (&optional time) (format-time-string "%FT%TZ" time "UTC0"))

(cl-defun fsrs-timestamp-difference (time-a time-b)
 (- (time-to-seconds (parse-iso8601-time-string time-a))
    (time-to-seconds (parse-iso8601-time-string time-b))))

(cl-defun fsrs-timestamp+ (time amount unit)
 (fsrs-now
  (+ (time-to-seconds (parse-iso8601-time-string time))
     (* amount
        (cl-ecase unit (:sec 1) (:minute 60) (:hour 3600) (:day 86400))))))

(cl-deftype fsrs-state nil '(member :new :learning :review :relearning))

(cl-declaim (ftype (function (fsrs-state) (integer 0 3)) fsrs-state-integer)
 (inline fsrs-state-integer))

(cl-defun fsrs-state-integer (fsrs-state)
 (cl-ecase fsrs-state (:new 0) (:learning 1) (:review 2) (:relearning 3)))

(cl-deftype fsrs-rating nil '(member :again :hard :good :easy))

(cl-declaim (ftype (function (fsrs-rating) (integer 1 4)) fsrs-rating-integer)
 (inline fsrs-rating-integer))

(cl-defun fsrs-rating-integer (fsrs-rating)
 (cl-ecase fsrs-rating (:again 1) (:hard 2) (:good 3) (:easy 4)))

(cl-defstruct (fsrs-review-log) (rating :again :type fsrs-rating)
 (scheduled-days 0 :type fixnum) (elapsed-days 0 :type fixnum)
 (review (fsrs-now) :type fsrs-timestamp) (state :new :type fsrs-state))

(cl-defstruct (fsrs-card) (due (fsrs-now) :type fsrs-timestamp)
 (stability 0.0 :type float) (difficulty 0.0 :type float)
 (elapsed-days 0 :type fixnum) (scheduled-days 0 :type fixnum)
 (repeats 0 :type fixnum) (lapses 0 :type fixnum) (state :new :type fsrs-state)
 (last-review nil :type (or fsrs-timestamp null)))

(cl-declaim (ftype (function ((or fixnum float)) fixnum) fsrs-seconds-days))

(cl-defun fsrs-seconds-days (secs) (cl-nth-value 0 (cl-truncate secs 86400)))

(cl-declaim (type float fsrs-decay fsrs-factor))

(defconst fsrs-decay -0.5)

(defconst fsrs-factor (1- (expt 0.9 (/ fsrs-decay))))

(cl-declaim
 (ftype (function (fsrs-card &optional fsrs-timestamp) (float 0.0 1.0))
  fsrs-card-retrievability))

(cl-defun fsrs-card-retrievability (self &optional (fsrs-now (fsrs-now)))
 (if (cl-member (fsrs-card-state self) '(:learning :review :relearning))
     (let ((elapsed-days
            (max 0
                 (fsrs-seconds-days
                  (fsrs-timestamp-difference fsrs-now
                   (fsrs-card-last-review self))))))
       (expt (1+ (/ (* fsrs-factor elapsed-days) (fsrs-card-stability self)))
             fsrs-decay))
     0.0))

(cl-defstruct (fsrs-scheduling-info) (card (make-fsrs-card) :type fsrs-card)
 (review-log (make-fsrs-review-log) :type fsrs-review-log))

(cl-defstruct (fsrs-scheduling-cards (:constructor %make-scheduling-cards))
 (again (make-fsrs-card) :type fsrs-card)
 (hard (make-fsrs-card) :type fsrs-card)
 (good (make-fsrs-card) :type fsrs-card)
 (easy (make-fsrs-card) :type fsrs-card))

(cl-declaim
 (ftype
  (function
   (&key (:card fsrs-card) (:again fsrs-card) (:hard fsrs-card)
    (:good fsrs-card) (:easy fsrs-card))
   fsrs-scheduling-cards)
  make-fsrs-scheduling-cards))

(cl-defun make-fsrs-scheduling-cards
 (&key (card (make-fsrs-card)) (again (copy-fsrs-card card))
  (hard (copy-fsrs-card card)) (good (copy-fsrs-card card))
  (easy (copy-fsrs-card card)))
 (%make-scheduling-cards :again again :hard hard :good good :easy easy))

(cl-declaim
 (ftype (function (fsrs-scheduling-cards fsrs-card fsrs-timestamp) list)
  fsrs-scheduling-cards-record-log))

(cl-defun fsrs-scheduling-cards-record-log (self fsrs-card fsrs-now)
 (let ((again (fsrs-scheduling-cards-again self))
       (hard (fsrs-scheduling-cards-hard self))
       (good (fsrs-scheduling-cards-good self))
       (easy (fsrs-scheduling-cards-easy self)))
   (list :again
         (make-fsrs-scheduling-info :card again :review-log
          (make-fsrs-review-log :rating :again :scheduled-days
           (fsrs-card-scheduled-days again) :elapsed-days
           (fsrs-card-elapsed-days fsrs-card) :review fsrs-now :state
           (fsrs-card-state fsrs-card)))
         :hard
         (make-fsrs-scheduling-info :card hard :review-log
          (make-fsrs-review-log :rating :hard :scheduled-days
           (fsrs-card-scheduled-days hard) :elapsed-days
           (fsrs-card-elapsed-days fsrs-card) :review fsrs-now :state
           (fsrs-card-state fsrs-card)))
         :good
         (make-fsrs-scheduling-info :card good :review-log
          (make-fsrs-review-log :rating :good :scheduled-days
           (fsrs-card-scheduled-days good) :elapsed-days
           (fsrs-card-elapsed-days fsrs-card) :review fsrs-now :state
           (fsrs-card-state fsrs-card)))
         :easy
         (make-fsrs-scheduling-info :card easy :review-log
          (make-fsrs-review-log :rating :easy :scheduled-days
           (fsrs-card-scheduled-days easy) :elapsed-days
           (fsrs-card-elapsed-days fsrs-card) :review fsrs-now :state
           (fsrs-card-state fsrs-card))))))

(cl-deftype fsrs-weights nil 'vector)

(defconst fsrs-weights-default
 (cl-coerce
  '(0.4072 1.1829 3.1262 15.4722 7.2102 0.5316 1.0651 0.0234 1.616 0.1544
    1.0824 1.9813 0.0953 0.2975 2.2042 0.2407 2.9466 0.5034 0.6567)
  'vector))

(cl-declaim (type float fsrs-decay fsrs-factor))

(defconst fsrs-decay -0.5)

(defconst fsrs-factor (1- (expt 0.9 (/ fsrs-decay))))

(cl-defstruct (fsrs-parameters) (request-retention 0.9 :type float)
 (maximum-interval 36500 :type fixnum)
 (weights fsrs-weights-default :type fsrs-weights)
 (decay fsrs-decay :type float) (factor fsrs-factor :type float))

(cl-declaim
 (ftype (function (fsrs-parameters fixnum float) (float 0.0 1.0))
  fsrs-parameters-forgetting-curve))

(cl-defun fsrs-parameters-forgetting-curve (self elapsed-days stability)
 (expt (1+ (/ (* (fsrs-parameters-factor self) elapsed-days) stability))
       (fsrs-parameters-decay self)))

(cl-declaim
 (ftype (function (fsrs-parameters fsrs-rating) float)
  fsrs-parameters-init-stability))

(cl-defun fsrs-parameters-init-stability
 (self fsrs-rating &aux (w (fsrs-parameters-weights self))
  (r (fsrs-rating-integer fsrs-rating)))
 (max (aref w (1- r)) 0.1))

(cl-declaim
 (ftype (function (fsrs-parameters fsrs-rating) float)
  fsrs-parameters-init-difficulty))

(cl-defun fsrs-parameters-init-difficulty
 (self fsrs-rating &aux (w (fsrs-parameters-weights self))
  (r (fsrs-rating-integer fsrs-rating)))
 (min (max (1+ (- (aref w 4) (exp (* (aref w 5) (1- r))))) 1.0) 10.0))

(defconst fsrs-most-negative-fixnum-float
 (cl-coerce most-negative-fixnum 'float))

(defconst fsrs-most-positive-fixnum-float
 (cl-coerce most-positive-fixnum 'float))

(cl-deftype fsrs-fixnum-float nil
 (list 'float fsrs-most-negative-fixnum-float fsrs-most-positive-fixnum-float))

(cl-declaim
 (ftype (function (fsrs-parameters float) fixnum)
  fsrs-parameters-next-interval))

(cl-defun fsrs-parameters-next-interval (self s)
 (let ((new-interval
        (* (/ s (fsrs-parameters-factor self))
           (1-
            (expt (fsrs-parameters-request-retention self)
                  (/ (fsrs-parameters-decay self)))))))
   (cl-declare (type fsrs-fixnum-float new-interval))
   (min (max (cl-nth-value 0 (cl-round new-interval)) 1)
        (fsrs-parameters-maximum-interval self))))

(cl-declaim
 (ftype (function (fsrs-parameters float float) float)
  fsrs-parameters-mean-reversion))

(cl-defun fsrs-parameters-mean-reversion
 (self init current &aux (w (fsrs-parameters-weights self)))
 (+ (* (aref w 7) init) (* (- 1.0 (aref w 7)) current)))

(cl-declaim
 (ftype (function (fsrs-parameters float fsrs-rating) float)
  fsrs-parameters-next-difficulty))

(cl-defun fsrs-parameters-next-difficulty
 (self d fsrs-rating &aux (w (fsrs-parameters-weights self))
  (r (fsrs-rating-integer fsrs-rating)))
 (let ((next-d (- d (* (aref w 6) (- r 3)))))
   (min
    (max
     (fsrs-parameters-mean-reversion self
      (fsrs-parameters-init-difficulty self :easy) next-d)
     1.0)
    10.0)))

(cl-declaim
 (ftype (function (fsrs-parameters float fsrs-rating) float)
  fsrs-parameters-short-term-stability))

(cl-defun fsrs-parameters-short-term-stability
 (self stability fsrs-rating &aux (w (fsrs-parameters-weights self))
  (r (fsrs-rating-integer fsrs-rating)))
 (* stability (exp (* (aref w 17) (+ (- r 3) (aref w 18))))))

(cl-declaim
 (ftype
  (function (fsrs-parameters float float (float 0.0 1.0) fsrs-rating) float)
  fsrs-parameters-next-recall-stability))

(cl-defun fsrs-parameters-next-recall-stability
 (self d s r fsrs-rating &aux (w (fsrs-parameters-weights self)))
 (let ((hard-penalty
        (if (eq fsrs-rating :hard)
            (aref w 15)
            1.0))
       (easy-bonus
        (if (eq fsrs-rating :easy)
            (aref w 16)
            1.0)))
   (* s
      (1+
       (* (exp (aref w 8)) (- 11.0 d) (expt s (- (aref w 9)))
          (1- (exp (* (- 1.0 r) (aref w 10)))) hard-penalty easy-bonus)))))

(cl-declaim
 (ftype (function (fsrs-parameters float float (float 0.0 1.0)) float)
  fsrs-parameters-next-forget-stability))

(cl-defun fsrs-parameters-next-forget-stability
 (self d s r &aux (w (fsrs-parameters-weights self)))
 (* (aref w 11) (expt d (- (aref w 12))) (1- (expt (1+ s) (aref w 13)))
    (exp (* (- 1 r) (aref w 14)))))

(cl-defstruct (fsrs-scheduler (:constructor nil))
 (parameters (make-fsrs-parameters) :type fsrs-parameters))

(cl-defun make-fsrs-scheduler
 (&rest args &key
  (parameters
   (progn
    (cl-remf args :parameters)
    (cl-remf args :enable-short-term-p)
    (apply #'make-fsrs-parameters args)))
  (enable-short-term-p t) &allow-other-keys)
 (funcall
  (if enable-short-term-p
      'make-fsrs-basic-scheduler
      'make-fsrs-long-term-scheduler)
  :parameters parameters))

(cl-declaim
 (ftype #'(fsrs-scheduler fsrs-scheduling-cards) fsrs-scheduler-init-ds))

(cl-defun fsrs-scheduler-init-ds
 (self s &aux (fsrs-parameters (fsrs-scheduler-parameters self)))
 (let ((again (fsrs-scheduling-cards-again s))
       (hard (fsrs-scheduling-cards-hard s))
       (good (fsrs-scheduling-cards-good s))
       (easy (fsrs-scheduling-cards-easy s)))
   (setf (fsrs-card-difficulty again)
           (fsrs-parameters-init-difficulty fsrs-parameters :again)
         (fsrs-card-stability again)
           (fsrs-parameters-init-stability fsrs-parameters :again)
         (fsrs-card-difficulty hard)
           (fsrs-parameters-init-difficulty fsrs-parameters :hard)
         (fsrs-card-stability hard)
           (fsrs-parameters-init-stability fsrs-parameters :hard)
         (fsrs-card-difficulty good)
           (fsrs-parameters-init-difficulty fsrs-parameters :good)
         (fsrs-card-stability good)
           (fsrs-parameters-init-stability fsrs-parameters :good)
         (fsrs-card-difficulty easy)
           (fsrs-parameters-init-difficulty fsrs-parameters :easy)
         (fsrs-card-stability easy)
           (fsrs-parameters-init-stability fsrs-parameters :easy))))

(cl-declaim
 (ftype #'(fsrs-scheduler fsrs-scheduling-cards fsrs-card)
  fsrs-scheduler-next-ds))

(cl-defun fsrs-scheduler-next-ds
 (self s fsrs-card &aux (fsrs-parameters (fsrs-scheduler-parameters self)))
 (let* ((last-d (fsrs-card-difficulty fsrs-card))
        (last-s (fsrs-card-stability fsrs-card))
        (interval (fsrs-card-elapsed-days fsrs-card))
        (retrievability
         (fsrs-parameters-forgetting-curve fsrs-parameters interval last-s))
        (fsrs-state (fsrs-card-state fsrs-card)))
   (let ((again (fsrs-scheduling-cards-again s))
         (hard (fsrs-scheduling-cards-hard s))
         (good (fsrs-scheduling-cards-good s))
         (easy (fsrs-scheduling-cards-easy s)))
     (setf (fsrs-card-difficulty again)
             (fsrs-parameters-next-difficulty fsrs-parameters last-d :again)
           (fsrs-card-difficulty hard)
             (fsrs-parameters-next-difficulty fsrs-parameters last-d :hard)
           (fsrs-card-difficulty good)
             (fsrs-parameters-next-difficulty fsrs-parameters last-d :good)
           (fsrs-card-difficulty easy)
             (fsrs-parameters-next-difficulty fsrs-parameters last-d :easy))
     (cl-ecase fsrs-state
      ((:learning :relearning)
       (setf (fsrs-card-stability again)
               (fsrs-parameters-short-term-stability fsrs-parameters last-s
                :again)
             (fsrs-card-stability hard)
               (fsrs-parameters-short-term-stability fsrs-parameters last-s
                :hard)
             (fsrs-card-stability good)
               (fsrs-parameters-short-term-stability fsrs-parameters last-s
                :good)
             (fsrs-card-stability easy)
               (fsrs-parameters-short-term-stability fsrs-parameters last-s
                :easy)))
      (:review
       (setf (fsrs-card-stability again)
               (fsrs-parameters-next-forget-stability fsrs-parameters last-d
                last-s retrievability)
             (fsrs-card-stability hard)
               (fsrs-parameters-next-recall-stability fsrs-parameters last-d
                last-s retrievability :hard)
             (fsrs-card-stability good)
               (fsrs-parameters-next-recall-stability fsrs-parameters last-d
                last-s retrievability :good)
             (fsrs-card-stability easy)
               (fsrs-parameters-next-recall-stability fsrs-parameters last-d
                last-s retrievability :easy)))))))

(cl-defgeneric scheduler-repeat (self fsrs-card &optional fsrs-now))

(cl-declaim
 (ftype
  (function (fsrs-scheduler fsrs-card fsrs-rating &optional fsrs-timestamp)
   (cl-values fsrs-card fsrs-review-log))
  fsrs-scheduler-review-card))

(cl-defun fsrs-scheduler-review-card
 (self fsrs-card fsrs-rating &optional (fsrs-now (fsrs-now)))
 (let* ((fsrs-scheduling-cards (scheduler-repeat self fsrs-card fsrs-now))
        (fsrs-scheduling-info (cl-getf fsrs-scheduling-cards fsrs-rating))
        (fsrs-card (fsrs-scheduling-info-card fsrs-scheduling-info))
        (fsrs-review-log
         (fsrs-scheduling-info-review-log fsrs-scheduling-info)))
   (cl-values fsrs-card fsrs-review-log)))

(cl-defstruct (fsrs-basic-scheduler (:include fsrs-scheduler)))

(cl-declaim
 (ftype #'(fsrs-basic-scheduler fsrs-scheduling-cards fsrs-state)
  fsrs-basic-scheduler-update-state))

(cl-defun fsrs-basic-scheduler-update-state (self cards fsrs-state)
 (ignore self)
 (let ((again (fsrs-scheduling-cards-again cards))
       (hard (fsrs-scheduling-cards-hard cards))
       (good (fsrs-scheduling-cards-good cards))
       (easy (fsrs-scheduling-cards-easy cards)))
   (cl-ecase fsrs-state
    (:new
     (setf (fsrs-card-state again) :learning
           (fsrs-card-state hard) :learning
           (fsrs-card-state good) :learning
           (fsrs-card-state easy) :review))
    ((:learning :relearning)
     (setf (fsrs-card-state again) fsrs-state
           (fsrs-card-state hard) fsrs-state
           (fsrs-card-state good) :review
           (fsrs-card-state easy) :review))
    (:review
     (setf (fsrs-card-state again) :relearning
           (fsrs-card-state hard) :review
           (fsrs-card-state good) :review
           (fsrs-card-state easy) :review)
     (cl-incf (fsrs-card-lapses again))))))

(cl-declaim
 (ftype
  #'(fsrs-basic-scheduler fsrs-scheduling-cards fsrs-timestamp fixnum fixnum
     fixnum fixnum)
  fsrs-basic-scheduler-schedule))

(cl-defun fsrs-basic-scheduler-schedule
 (self cards fsrs-now again-interval hard-interval good-interval easy-interval)
 (ignore self again-interval)
 (let ((again (fsrs-scheduling-cards-again cards))
       (hard (fsrs-scheduling-cards-hard cards))
       (good (fsrs-scheduling-cards-good cards))
       (easy (fsrs-scheduling-cards-easy cards)))
   (setf (fsrs-card-scheduled-days again) 0
         (fsrs-card-scheduled-days hard) hard-interval
         (fsrs-card-scheduled-days good) good-interval
         (fsrs-card-scheduled-days easy) easy-interval
         (fsrs-card-due again) (fsrs-timestamp+ fsrs-now 5 :minute)
         (fsrs-card-due hard)
           (if (cl-plusp hard-interval)
               (fsrs-timestamp+ fsrs-now hard-interval :day)
               (fsrs-timestamp+ fsrs-now 10 :minute))
         (fsrs-card-due good) (fsrs-timestamp+ fsrs-now good-interval :day)
         (fsrs-card-due easy) (fsrs-timestamp+ fsrs-now easy-interval :day))))

(cl-defmethod scheduler-repeat
 ((self fsrs-basic-scheduler) fsrs-card &optional (fsrs-now (fsrs-now)))
 (let ((fsrs-card (copy-fsrs-card fsrs-card))
       (fsrs-parameters (fsrs-scheduler-parameters self)))
   (setf (fsrs-card-elapsed-days fsrs-card)
           (if (eq (fsrs-card-state fsrs-card) :new)
               0
               (fsrs-seconds-days
                (fsrs-timestamp-difference fsrs-now
                 (fsrs-card-last-review fsrs-card))))
         (fsrs-card-last-review fsrs-card) fsrs-now)
   (cl-incf (fsrs-card-repeats fsrs-card))
   (let ((s (make-fsrs-scheduling-cards :card fsrs-card)))
     (fsrs-basic-scheduler-update-state self s (fsrs-card-state fsrs-card))
     (let ((again (fsrs-scheduling-cards-again s))
           (hard (fsrs-scheduling-cards-hard s))
           (good (fsrs-scheduling-cards-good s))
           (easy (fsrs-scheduling-cards-easy s)))
       (cl-ecase (fsrs-card-state fsrs-card)
        (:new (fsrs-scheduler-init-ds self s)
         (setf (fsrs-card-due again) (fsrs-timestamp+ fsrs-now 1 :minute)
               (fsrs-card-due hard) (fsrs-timestamp+ fsrs-now 5 :minute)
               (fsrs-card-due good) (fsrs-timestamp+ fsrs-now 10 :minute))
         (let ((easy-interval
                (fsrs-parameters-next-interval fsrs-parameters
                 (fsrs-card-stability easy))))
           (setf (fsrs-card-scheduled-days easy) easy-interval
                 (fsrs-card-due easy)
                   (fsrs-timestamp+ fsrs-now easy-interval :day))))
        ((:learning :relearning) (fsrs-scheduler-next-ds self s fsrs-card)
         (let* ((hard-interval 0)
                (good-interval
                 (fsrs-parameters-next-interval fsrs-parameters
                  (fsrs-card-stability good)))
                (easy-interval
                 (max
                  (fsrs-parameters-next-interval fsrs-parameters
                   (fsrs-card-stability easy))
                  (1+ good-interval))))
           (fsrs-basic-scheduler-schedule self s fsrs-now 0 hard-interval
            good-interval easy-interval)))
        (:review (fsrs-scheduler-next-ds self s fsrs-card)
         (let* ((hard-interval
                 (fsrs-parameters-next-interval fsrs-parameters
                  (fsrs-card-stability hard)))
                (good-interval
                 (fsrs-parameters-next-interval fsrs-parameters
                  (fsrs-card-stability good)))
                (hard-interval (min hard-interval good-interval))
                (good-interval (max good-interval (1+ hard-interval)))
                (easy-interval
                 (max
                  (fsrs-parameters-next-interval fsrs-parameters
                   (fsrs-card-stability easy))
                  (1+ good-interval))))
           (fsrs-basic-scheduler-schedule self s fsrs-now 0 hard-interval
            good-interval easy-interval)))))
     (fsrs-scheduling-cards-record-log s fsrs-card fsrs-now))))

(cl-defstruct (fsrs-long-term-scheduler (:include fsrs-scheduler)))

(cl-declaim
 (ftype #'(fsrs-long-term-scheduler fsrs-scheduling-cards fsrs-state)
  fsrs-long-term-scheduler-update-state))

(cl-defun fsrs-long-term-scheduler-update-state (self cards fsrs-state)
 (ignore self fsrs-state)
 (let ((again (fsrs-scheduling-cards-again cards))
       (hard (fsrs-scheduling-cards-hard cards))
       (good (fsrs-scheduling-cards-good cards))
       (easy (fsrs-scheduling-cards-easy cards)))
   (setf (fsrs-card-state again) :review
         (fsrs-card-state hard) :review
         (fsrs-card-state good) :review
         (fsrs-card-state easy) :review)))

(cl-declaim
 (ftype
  #'(fsrs-long-term-scheduler fsrs-scheduling-cards fsrs-timestamp fixnum
     fixnum fixnum fixnum)
  fsrs-long-term-scheduler-schedule))

(cl-defun fsrs-long-term-scheduler-schedule
 (self cards fsrs-now again-interval hard-interval good-interval easy-interval)
 (ignore self)
 (let ((again (fsrs-scheduling-cards-again cards))
       (hard (fsrs-scheduling-cards-hard cards))
       (good (fsrs-scheduling-cards-good cards))
       (easy (fsrs-scheduling-cards-easy cards)))
   (setf (fsrs-card-scheduled-days again) again-interval
         (fsrs-card-scheduled-days hard) hard-interval
         (fsrs-card-scheduled-days good) good-interval
         (fsrs-card-scheduled-days easy) easy-interval
         (fsrs-card-due again) (fsrs-timestamp+ fsrs-now again-interval :day)
         (fsrs-card-due hard) (fsrs-timestamp+ fsrs-now hard-interval :day)
         (fsrs-card-due good) (fsrs-timestamp+ fsrs-now good-interval :day)
         (fsrs-card-due easy) (fsrs-timestamp+ fsrs-now easy-interval :day))))

(cl-defmethod scheduler-repeat
 ((self fsrs-long-term-scheduler) fsrs-card &optional (fsrs-now (fsrs-now)))
 (let ((fsrs-card (copy-fsrs-card fsrs-card))
       (fsrs-parameters (fsrs-scheduler-parameters self)))
   (setf (fsrs-card-elapsed-days fsrs-card)
           (if (eq (fsrs-card-state fsrs-card) :new)
               0
               (fsrs-seconds-days
                (fsrs-timestamp-difference fsrs-now
                 (fsrs-card-last-review fsrs-card))))
         (fsrs-card-last-review fsrs-card) fsrs-now)
   (cl-incf (fsrs-card-repeats fsrs-card))
   (let ((s (make-fsrs-scheduling-cards :card fsrs-card)))
     (fsrs-long-term-scheduler-update-state self s (fsrs-card-state fsrs-card))
     (let ((again (fsrs-scheduling-cards-again s))
           (hard (fsrs-scheduling-cards-hard s))
           (good (fsrs-scheduling-cards-good s))
           (easy (fsrs-scheduling-cards-easy s)))
       (if (eq (fsrs-card-state fsrs-card) :new)
           (fsrs-scheduler-init-ds self s)
           (fsrs-scheduler-next-ds self s fsrs-card))
       (let* ((again-interval
               (fsrs-parameters-next-interval fsrs-parameters
                (fsrs-card-stability again)))
              (hard-interval
               (fsrs-parameters-next-interval fsrs-parameters
                (fsrs-card-stability hard)))
              (good-interval
               (fsrs-parameters-next-interval fsrs-parameters
                (fsrs-card-stability good)))
              (easy-interval
               (fsrs-parameters-next-interval fsrs-parameters
                (fsrs-card-stability easy)))
              (again-interval (min again-interval hard-interval))
              (hard-interval (max hard-interval (1+ again-interval)))
              (good-interval (max good-interval (1+ hard-interval)))
              (easy-interval (max easy-interval (1+ good-interval))))
         (fsrs-long-term-scheduler-schedule self s fsrs-now again-interval
          hard-interval good-interval easy-interval)))
     (fsrs-scheduling-cards-record-log s fsrs-card fsrs-now))))

(provide 'fsrs)
;;; fsrs.el ends here
