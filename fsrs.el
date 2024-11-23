;;; fsrs.el --- Emacs Lisp Package for FSRS -*- lexical-binding: t -*-

;;; Commentary:

;; This file was auto-transpiled from package.lisp, models.lisp, fsrs.lisp.

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

(cl-deftype fsrs-rating nil '(member :again :hard :good :easy))

(cl-declaim (ftype (function (fsrs-rating) (integer 1 4)) fsrs-rating-index)
 (inline fsrs-rating-index))

(cl-defun fsrs-rating-index (fsrs-rating)
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
 (ftype #'(fsrs-scheduling-cards fsrs-state)
  fsrs-scheduling-cards-update-state))

(cl-defun fsrs-scheduling-cards-update-state (self fsrs-state)
 (let ((again (fsrs-scheduling-cards-again self))
       (hard (fsrs-scheduling-cards-hard self))
       (good (fsrs-scheduling-cards-good self))
       (easy (fsrs-scheduling-cards-easy self)))
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
 (ftype #'(fsrs-scheduling-cards fsrs-timestamp fixnum fixnum fixnum)
  fsrs-scheduling-cards-schedule))

(cl-defun fsrs-scheduling-cards-schedule
 (self fsrs-now hard-interval good-interval easy-interval)
 (let ((again (fsrs-scheduling-cards-again self))
       (hard (fsrs-scheduling-cards-hard self))
       (good (fsrs-scheduling-cards-good self))
       (easy (fsrs-scheduling-cards-easy self)))
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

(defconst fsrs-weights-default
 (cl-coerce
  '(0.4072 1.1829 3.1262 15.4722 7.2102 0.5316 1.0651 0.0234 1.616 0.1544
    1.0824 1.9813 0.0953 0.2975 2.2042 0.2407 2.9466 0.5034 0.6567)
  'vector))

(cl-defstruct (fsrs-parameters) (request-retention 0.9 :type float)
 (maximum-interval 36500 :type fixnum)
 (weights fsrs-weights-default :type vector))

(cl-defstruct (fsrs (:constructor %make-fsrs))
 (parameters (make-fsrs-parameters) :type fsrs-parameters))

(cl-defun make-fsrs
 (&rest args &key
  (parameters
   (progn (cl-remf args :parameters) (apply #'make-fsrs-parameters args)))
  &allow-other-keys)
 (%make-fsrs :parameters parameters))

(cl-declaim (ftype (function (fsrs) vector) fsrs-pw) (inline fsrs-pw))

(cl-defun fsrs-pw (self) (fsrs-parameters-weights (fsrs-parameters self)))

(cl-declaim (ftype (function (fsrs fsrs-rating) float) fsrs-init-stability))

(cl-defun fsrs-init-stability
 (self fsrs-rating &aux (w (fsrs-pw self)) (r (fsrs-rating-index fsrs-rating)))
 (max (aref w (1- r)) 0.1))

(cl-declaim (ftype (function (fsrs fsrs-rating) float) fsrs-init-difficulty))

(cl-defun fsrs-init-difficulty
 (self fsrs-rating &aux (w (fsrs-pw self)) (r (fsrs-rating-index fsrs-rating)))
 (min (max (1+ (- (aref w 4) (exp (* (aref w 5) (1- r))))) 1.0) 10.0))

(cl-declaim
 (ftype (function (fsrs fixnum float) (float 0.0 1.0)) fsrs-forgetting-curve))

(cl-defun fsrs-forgetting-curve (self elapsed-days stability) (ignore self)
 (expt (1+ (/ (* fsrs-factor elapsed-days) stability)) fsrs-decay))

(defconst fsrs-most-negative-fixnum-float
 (cl-coerce most-negative-fixnum 'float))

(defconst fsrs-most-positive-fixnum-float
 (cl-coerce most-positive-fixnum 'float))

(cl-deftype fsrs-fixnum-float nil
 (list 'float fsrs-most-negative-fixnum-float fsrs-most-positive-fixnum-float))

(cl-declaim (ftype (function (fsrs float) fixnum) fsrs-next-interval))

(cl-defun fsrs-next-interval (self s &aux (p (fsrs-parameters self)))
 (let ((new-interval
        (* (/ s fsrs-factor)
           (1- (expt (fsrs-parameters-request-retention p) (/ fsrs-decay))))))
   (cl-declare (type fsrs-fixnum-float new-interval))
   (min (max (cl-nth-value 0 (cl-round new-interval)) 1)
        (fsrs-parameters-maximum-interval p))))

(cl-declaim
 (ftype (function (fsrs float fsrs-rating) float) fsrs-short-term-stability))

(cl-defun fsrs-short-term-stability
 (self stability fsrs-rating &aux (w (fsrs-pw self))
  (r (fsrs-rating-index fsrs-rating)))
 (* stability (exp (* (aref w 17) (+ (- r 3) (aref w 18))))))

(cl-declaim (ftype (function (fsrs float float) float) fsrs-mean-reversion))

(cl-defun fsrs-mean-reversion (self init current &aux (w (fsrs-pw self)))
 (+ (* (aref w 7) init) (* (- 1.0 (aref w 7)) current)))

(cl-declaim
 (ftype (function (fsrs float fsrs-rating) float) fsrs-next-difficulty))

(cl-defun fsrs-next-difficulty
 (self d fsrs-rating &aux (w (fsrs-pw self))
  (r (fsrs-rating-index fsrs-rating)))
 (let ((next-d (- d (* (aref w 6) (- r 3)))))
   (min
    (max (fsrs-mean-reversion self (fsrs-init-difficulty self :easy) next-d)
         1.0)
    10.0)))

(cl-declaim
 (ftype (function (fsrs float float (float 0.0 1.0) fsrs-rating) float)
  fsrs-next-recall-stability))

(cl-defun fsrs-next-recall-stability
 (self d s r fsrs-rating &aux (w (fsrs-pw self)))
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
 (ftype (function (fsrs float float (float 0.0 1.0)) float)
  fsrs-next-forget-stability))

(cl-defun fsrs-next-forget-stability (self d s r &aux (w (fsrs-pw self)))
 (* (aref w 11) (expt d (- (aref w 12))) (1- (expt (1+ s) (aref w 13)))
    (exp (* (- 1 r) (aref w 14)))))

(cl-declaim (ftype #'(fsrs fsrs-scheduling-cards) fsrs-init-ds))

(cl-defun fsrs-init-ds (self s)
 (let ((again (fsrs-scheduling-cards-again s))
       (hard (fsrs-scheduling-cards-hard s))
       (good (fsrs-scheduling-cards-good s))
       (easy (fsrs-scheduling-cards-easy s)))
   (setf (fsrs-card-difficulty again) (fsrs-init-difficulty self :again)
         (fsrs-card-stability again) (fsrs-init-stability self :again)
         (fsrs-card-difficulty hard) (fsrs-init-difficulty self :hard)
         (fsrs-card-stability hard) (fsrs-init-stability self :hard)
         (fsrs-card-difficulty good) (fsrs-init-difficulty self :good)
         (fsrs-card-stability good) (fsrs-init-stability self :good)
         (fsrs-card-difficulty easy) (fsrs-init-difficulty self :easy)
         (fsrs-card-stability easy) (fsrs-init-stability self :easy))))

(cl-declaim
 (ftype #'(fsrs fsrs-scheduling-cards float float (float 0.0 1.0) fsrs-state)
  fsrs-next-ds))

(cl-defun fsrs-next-ds (self s last-d last-s retrievability fsrs-state)
 (let ((again (fsrs-scheduling-cards-again s))
       (hard (fsrs-scheduling-cards-hard s))
       (good (fsrs-scheduling-cards-good s))
       (easy (fsrs-scheduling-cards-easy s)))
   (setf (fsrs-card-difficulty again) (fsrs-next-difficulty self last-d :again)
         (fsrs-card-difficulty hard) (fsrs-next-difficulty self last-d :hard)
         (fsrs-card-difficulty good) (fsrs-next-difficulty self last-d :good)
         (fsrs-card-difficulty easy) (fsrs-next-difficulty self last-d :easy))
   (cl-ecase fsrs-state
    ((:learning :relearning)
     (setf (fsrs-card-stability again)
             (fsrs-short-term-stability self last-s :again)
           (fsrs-card-stability hard)
             (fsrs-short-term-stability self last-s :hard)
           (fsrs-card-stability good)
             (fsrs-short-term-stability self last-s :good)
           (fsrs-card-stability easy)
             (fsrs-short-term-stability self last-s :easy)))
    (:review
     (setf (fsrs-card-stability again)
             (fsrs-next-forget-stability self last-d last-s retrievability)
           (fsrs-card-stability hard)
             (fsrs-next-recall-stability self last-d last-s retrievability
              :hard)
           (fsrs-card-stability good)
             (fsrs-next-recall-stability self last-d last-s retrievability
              :good)
           (fsrs-card-stability easy)
             (fsrs-next-recall-stability self last-d last-s retrievability
              :easy))))))

(cl-declaim
 (ftype (function (fsrs fsrs-card &optional fsrs-timestamp) list) fsrs-repeat))

(cl-defun fsrs-repeat (self fsrs-card &optional (fsrs-now (fsrs-now)))
 (let ((fsrs-card (copy-fsrs-card fsrs-card)))
   (setf (fsrs-card-elapsed-days fsrs-card)
           (if (eq (fsrs-card-state fsrs-card) :new)
               0
               (fsrs-seconds-days
                (fsrs-timestamp-difference fsrs-now
                 (fsrs-card-last-review fsrs-card))))
         (fsrs-card-last-review fsrs-card) fsrs-now)
   (cl-incf (fsrs-card-repeats fsrs-card))
   (let ((s (make-fsrs-scheduling-cards :card fsrs-card)))
     (fsrs-scheduling-cards-update-state s (fsrs-card-state fsrs-card))
     (let ((again (fsrs-scheduling-cards-again s))
           (hard (fsrs-scheduling-cards-hard s))
           (good (fsrs-scheduling-cards-good s))
           (easy (fsrs-scheduling-cards-easy s)))
       (cl-ecase (fsrs-card-state fsrs-card)
        (:new (fsrs-init-ds self s)
         (setf (fsrs-card-due again) (fsrs-timestamp+ fsrs-now 1 :minute)
               (fsrs-card-due hard) (fsrs-timestamp+ fsrs-now 5 :minute)
               (fsrs-card-due good) (fsrs-timestamp+ fsrs-now 10 :minute))
         (let ((easy-interval
                (fsrs-next-interval self (fsrs-card-stability easy))))
           (setf (fsrs-card-scheduled-days easy) easy-interval
                 (fsrs-card-due easy)
                   (fsrs-timestamp+ fsrs-now easy-interval :day))))
        ((:learning :relearning)
         (let* ((interval (fsrs-card-elapsed-days fsrs-card))
                (last-d (fsrs-card-difficulty fsrs-card))
                (last-s (fsrs-card-stability fsrs-card))
                (retrievability (fsrs-forgetting-curve self interval last-s)))
           (fsrs-next-ds self s last-d last-s retrievability
            (fsrs-card-state fsrs-card))
           (let* ((hard-interval 0)
                  (good-interval
                   (fsrs-next-interval self (fsrs-card-stability good)))
                  (easy-interval
                   (max (fsrs-next-interval self (fsrs-card-stability easy))
                        (1+ good-interval))))
             (fsrs-scheduling-cards-schedule s fsrs-now hard-interval
              good-interval easy-interval))))
        (:review
         (let* ((interval (fsrs-card-elapsed-days fsrs-card))
                (last-d (fsrs-card-difficulty fsrs-card))
                (last-s (fsrs-card-stability fsrs-card))
                (retrievability (fsrs-forgetting-curve self interval last-s)))
           (fsrs-next-ds self s last-d last-s retrievability
            (fsrs-card-state fsrs-card))
           (let* ((hard-interval
                   (fsrs-next-interval self (fsrs-card-stability hard)))
                  (good-interval
                   (fsrs-next-interval self (fsrs-card-stability good)))
                  (hard-interval (min hard-interval good-interval))
                  (good-interval (max good-interval (1+ hard-interval)))
                  (easy-interval
                   (max (fsrs-next-interval self (fsrs-card-stability easy))
                        (1+ good-interval))))
             (fsrs-scheduling-cards-schedule s fsrs-now hard-interval
              good-interval easy-interval))))))
     (fsrs-scheduling-cards-record-log s fsrs-card fsrs-now))))

(cl-declaim
 (ftype
  (function (fsrs fsrs-card fsrs-rating &optional fsrs-timestamp)
   (cl-values fsrs-card fsrs-review-log))
  fsrs-review-card))

(cl-defun fsrs-review-card
 (self fsrs-card fsrs-rating &optional (fsrs-now (fsrs-now)))
 (let* ((fsrs-scheduling-cards (fsrs-repeat self fsrs-card fsrs-now))
        (fsrs-scheduling-info (cl-getf fsrs-scheduling-cards fsrs-rating))
        (fsrs-card (fsrs-scheduling-info-card fsrs-scheduling-info))
        (fsrs-review-log
         (fsrs-scheduling-info-review-log fsrs-scheduling-info)))
   (cl-values fsrs-card fsrs-review-log)))

(provide 'fsrs)
;;; fsrs.el ends here
