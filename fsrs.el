;;; fsrs.el --- Emacs Lisp Package for FSRS -*- lexical-binding: t -*-

;; Copyright (C) 2025 Open Spaced Repetition

;; Author: Open Spaced Repetition
;; Maintainer: Open Spaced Repetition
;; Version: 5.0
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/open-spaced-repetition/lisp-fsrs
;; Keywords: tools

;; This file is not part of GNU Emacs.

;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;; this software and associated documentation files (the "Software"), to deal in
;; the Software without restriction, including without limitation the rights to
;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is furnished to do
;; so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; FSRS is a spaced repetition algorithm that optimizes review scheduling
;; by adapting to individual memory patterns, outperforming SM-2.

;;; Code:

(require 'cl-lib)

(require 'cl-generic)

(require 'parse-time)

(cl-deftype fsrs-timestamp nil "ISO 8601 UTC timestamp string type.

Represents time values in `YYYY-MM-DDTHH:MM:SSZ' format. Used
throughout FSRS for all date/time tracking related to card scheduling
and review logging."
 'string)

(cl-defun fsrs-now (&optional time)
 "Get current UTC time as FSRS-TIMESTAMP string.

When TIME is non-nil (accepts time value or nil), format that instead
of current time. Returns string formatted according to ISO 8601 with
UTC timezone."
 (format-time-string "%FT%TZ" time "UTC0"))

(cl-defun fsrs-timestamp-difference (time-a time-b)
 "Calculate difference between two timestamps in seconds.

TIME-A and TIME-B must both be FSRS-TIMESTAMP strings. Returns
floating-point number representing TIME-A minus TIME-B in seconds.
Handles ISO 8601 parsing."
 (- (time-to-seconds (parse-iso8601-time-string time-a))
    (time-to-seconds (parse-iso8601-time-string time-b))))

(cl-defun fsrs-timestamp+ (time amount unit)
 "Create new FSRS-TIMESTAMP by adding time units.

TIME is base FSRS-TIMESTAMP string. AMOUNT is number of units to add. UNIT
is one of :sec/:minute/:hour/:day keyword specifying time unit.
Returns new ISO 8601 string calculated by adding AMOUNT × UNIT's
seconds to TIME."
 (fsrs-now
  (+ (time-to-seconds (parse-iso8601-time-string time))
     (* amount
        (cl-ecase unit (:sec 1) (:minute 60) (:hour 3600) (:day 86400))))))

(cl-deftype fsrs-state nil "Learning phase progression state.
Possible values: :new, :learning, :review, or :relearning."
 '(member :new :learning :review :relearning))

(cl-declaim (ftype (function (fsrs-state) (integer 0 3)) fsrs-state-integer)
 (inline fsrs-state-integer))

(cl-defun fsrs-state-integer (fsrs-state)
 "Convert FSRS-STATE to an integer (0-3)."
 (cl-ecase fsrs-state (:new 0) (:learning 1) (:review 2) (:relearning 3)))

(cl-deftype fsrs-rating nil "User response rating for memory recall success.
Possible values: :again (failed), :hard, :good, or :easy."
 '(member :again :hard :good :easy))

(cl-declaim (ftype (function (fsrs-rating) (integer 1 4)) fsrs-rating-integer)
 (inline fsrs-rating-integer))

(cl-defun fsrs-rating-integer (fsrs-rating)
 "Convert FSRS-RATING to an integer (1-4)."
 (cl-ecase fsrs-rating (:again 1) (:hard 2) (:good 3) (:easy 4)))

(cl-defstruct
 (fsrs-review-log (:copier fsrs-copy-review-log)
  (:constructor fsrs-make-review-log))
 "Record of individual review event.

RATING is user's response.
SCHEDULED-DAYS was planned interval.
ELAPSED-DAYS was actual days since last review.
REVIEW is timestamp of event.
STATE was card state before review."
 (rating :again :type fsrs-rating) (scheduled-days 0 :type fixnum)
 (elapsed-days 0 :type fixnum) (review (fsrs-now) :type fsrs-timestamp)
 (state :new :type fsrs-state))

(cl-defstruct
 (fsrs-card (:copier fsrs-copy-card) (:constructor fsrs-make-card))
 "Represents a memorization item with scheduling state and memory metrics.

DUE contains next review timestamp.
STABILITY measures memory retention strength (higher = more stable).
DIFFICULTY reflects item complexity (1.0-10.0 scale).
ELAPSED-DAYS tracks days since last actual review.
SCHEDULED-DAYS shows originally planned interval length.
REPEATS counts total successful review attempts.
LAPSES counts times item was forgotten and reset.
STATE tracks current learning phase (:new/:learning/:review/:relearning).
LAST-REVIEW stores timestamp of most recent review or NIL if new."
 (due (fsrs-now) :type fsrs-timestamp) (stability 0.0 :type float)
 (difficulty 0.0 :type float) (elapsed-days 0 :type fixnum)
 (scheduled-days 0 :type fixnum) (repeats 0 :type fixnum)
 (lapses 0 :type fixnum) (state :new :type fsrs-state)
 (last-review nil :type (or fsrs-timestamp null)))

(cl-declaim (ftype (function ((or fixnum float)) fixnum) fsrs-seconds-days))

(cl-defun fsrs-seconds-days (secs)
 "Convert SECONDS to integer days by truncating fractional part.
SECS can be fixnum or double-float.

Returns non-negative fixnum."
 (cl-nth-value 0 (cl-truncate secs 86400)))

(cl-declaim (type float fsrs-decay fsrs-factor))

(defconst fsrs-decay -0.5 "Decay exponent for forgetting curve calculation.")

(defconst fsrs-factor (1- (expt 0.9 (/ fsrs-decay)))
 "Precomputed scaling factor for interval calculations.")

(cl-declaim
 (ftype (function (fsrs-card &optional fsrs-timestamp) (float 0.0 1.0))
  fsrs-card-retrievability))

(cl-defun fsrs-card-retrievability (self &optional (fsrs-now (fsrs-now)))
 "Calculate current recall probability for CARD.

SELF is FSRS-CARD instance. FSRS-NOW is optional current timestamp.

Returns single-float between 0.0 (forgotten) and 1.0 (fresh)."
 (if (cl-member (fsrs-card-state self) '(:learning :review :relearning))
     (let ((elapsed-days
            (max 0
                 (fsrs-seconds-days
                  (fsrs-timestamp-difference fsrs-now
                   (fsrs-card-last-review self))))))
       (expt (1+ (/ (* fsrs-factor elapsed-days) (fsrs-card-stability self)))
             fsrs-decay))
     0.0))

(cl-defstruct
 (fsrs-scheduling-info (:copier fsrs-copy-scheduling-info)
  (:constructor fsrs-make-scheduling-info))
 "Container for card scheduling options and review metadata.

CARD holds modified card state for specific rating choice.
REVIEW-LOG records hypothetical review log entry for that choice."
 (card (fsrs-make-card) :type fsrs-card)
 (review-log (fsrs-make-review-log) :type fsrs-review-log))

(cl-defstruct
 (fsrs-scheduling-cards (:copier fsrs-copy-scheduling-cards)
  (:constructor fsrs--make-scheduling-cards))
 "Holds four possible scheduling paths based on user rating.

AGAIN path for failed recall.
HARD path for difficult recall.
GOOD path for successful recall.
EASY path for effortless recall."
 (again (fsrs-make-card) :type fsrs-card)
 (hard (fsrs-make-card) :type fsrs-card)
 (good (fsrs-make-card) :type fsrs-card)
 (easy (fsrs-make-card) :type fsrs-card))

(cl-declaim
 (ftype
  (function
   (&key (:card fsrs-card) (:again fsrs-card) (:hard fsrs-card)
    (:good fsrs-card) (:easy fsrs-card))
   fsrs-scheduling-cards)
  fsrs-make-scheduling-cards))

(cl-defun fsrs-make-scheduling-cards
 (&key (card (fsrs-make-card)) (again (fsrs-copy-card card))
  (hard (fsrs-copy-card card)) (good (fsrs-copy-card card))
  (easy (fsrs-copy-card card)))
 "Create FSRS-SCHEDULING-CARDS with cloned CARD variants.
Each keyword argument lets override default card copies for
ratings (EASY GOOD HARD AGAIN)."
 (fsrs--make-scheduling-cards :again again :hard hard :good good :easy easy))

(cl-declaim
 (ftype (function (fsrs-scheduling-cards fsrs-card fsrs-timestamp) list)
  fsrs-scheduling-cards-record-log))

(cl-defun fsrs-scheduling-cards-record-log (self fsrs-card fsrs-now)
 "Generate plist of FSRS-SCHEDULING-INFO entries for all rating paths.

SELF is FSRS-SCHEDULING-CARDS instance. FSRS-CARD is original card being
scheduled. FSRS-NOW is current timestamp for log entries.

Returns (:again INFO :hard INFO :good INFO :easy INFO) plist."
 (let ((again (fsrs-scheduling-cards-again self))
       (hard (fsrs-scheduling-cards-hard self))
       (good (fsrs-scheduling-cards-good self))
       (easy (fsrs-scheduling-cards-easy self)))
   (list :again
         (fsrs-make-scheduling-info :card again :review-log
          (fsrs-make-review-log :rating :again :scheduled-days
           (fsrs-card-scheduled-days again) :elapsed-days
           (fsrs-card-elapsed-days fsrs-card) :review fsrs-now :state
           (fsrs-card-state fsrs-card)))
         :hard
         (fsrs-make-scheduling-info :card hard :review-log
          (fsrs-make-review-log :rating :hard :scheduled-days
           (fsrs-card-scheduled-days hard) :elapsed-days
           (fsrs-card-elapsed-days fsrs-card) :review fsrs-now :state
           (fsrs-card-state fsrs-card)))
         :good
         (fsrs-make-scheduling-info :card good :review-log
          (fsrs-make-review-log :rating :good :scheduled-days
           (fsrs-card-scheduled-days good) :elapsed-days
           (fsrs-card-elapsed-days fsrs-card) :review fsrs-now :state
           (fsrs-card-state fsrs-card)))
         :easy
         (fsrs-make-scheduling-info :card easy :review-log
          (fsrs-make-review-log :rating :easy :scheduled-days
           (fsrs-card-scheduled-days easy) :elapsed-days
           (fsrs-card-elapsed-days fsrs-card) :review fsrs-now :state
           (fsrs-card-state fsrs-card))))))

(cl-deftype fsrs-weights nil
 "Array type containing 19 single-floats for FSRS parameters." 'vector)

(defconst fsrs-weights-default
 (cl-coerce
  '(0.4072 1.1829 3.1262 15.4722 7.2102 0.5316 1.0651 0.0234 1.616 0.1544
    1.0824 1.9813 0.0953 0.2975 2.2042 0.2407 2.9466 0.5034 0.6567)
  'vector))

(cl-declaim (type float fsrs-decay fsrs-factor))

(defconst fsrs-decay -0.5)

(defconst fsrs-factor (1- (expt 0.9 (/ fsrs-decay))))

(cl-defstruct
 (fsrs-parameters (:copier fsrs-copy-parameters)
  (:constructor fsrs-make-parameters))
 "Container for FSRS algorithm configuration parameters.

REQUEST-RETENTION specifies target retention probability (0.0-1.0).
MAXIMUM-INTERVAL sets upper bound (in days) for scheduling intervals.
WEIGHTS stores algorithm coefficients.
DECAY contains stability decay exponent for forgetting curve.
FACTOR is precomputed constant derived from DECAY for interval scaling."
 (request-retention 0.9 :type float) (maximum-interval 36500 :type fixnum)
 (weights fsrs-weights-default :type fsrs-weights)
 (decay fsrs-decay :type float) (factor fsrs-factor :type float))

(cl-declaim
 (ftype (function (fsrs-parameters fixnum float) (float 0.0 1.0))
  fsrs-parameters-forgetting-curve))

(cl-defun fsrs-parameters-forgetting-curve (self elapsed-days stability)
 "Calculate retention probability given elapsed days and stability.

SELF is FSRS-PARAMETERS instance. ELAPSED-DAYS is time since last review.
STABILITY is memory stability value.

Returns single-float between 0.0 and 1.0 representing recall probability."
 (expt (1+ (/ (* (fsrs-parameters-factor self) elapsed-days) stability))
       (fsrs-parameters-decay self)))

(cl-declaim
 (ftype (function (fsrs-parameters fsrs-rating) float)
  fsrs-parameters-init-stability))

(cl-defun fsrs-parameters-init-stability (self fsrs-rating)
 "Compute initial stability after first review.

SELF is FSRS-PARAMETERS instance. FSRS-RATING is user's response
quality (:again/:hard/:good/:easy).

Returns non-negative single-float stability value."
 (let ((w (fsrs-parameters-weights self)) (r (fsrs-rating-integer fsrs-rating)))
   (max (aref w (1- r)) 0.1)))

(cl-declaim
 (ftype (function (fsrs-parameters fsrs-rating) float)
  fsrs-parameters-init-difficulty))

(cl-defun fsrs-parameters-init-difficulty (self fsrs-rating)
 "Determine initial difficulty for new material.

SELF is FSRS-PARAMETERS instance. FSRS-RATING is first response rating.

Returns single-float between 1.0 and 10.0 representing item
difficulty."
 (let ((w (fsrs-parameters-weights self)) (r (fsrs-rating-integer fsrs-rating)))
   (min (max (1+ (- (aref w 4) (exp (* (aref w 5) (1- r))))) 1.0) 10.0)))

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
 "Calculate next review interval in days based on stability.

SELF is FSRS-PARAMETERS instance. S is current stability value.

Returns non-negative fixnum days until next review."
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

(cl-defun fsrs-parameters-mean-reversion (self init current)
 "Apply mean reversion to difficulty estimates.

SELF is FSRS-PARAMETERS instance. INIT is initial difficulty value. CURRENT
is raw difficulty estimate.

Returns adjusted single-float difficulty value."
 (let ((w (fsrs-parameters-weights self)))
   (+ (* (aref w 7) init) (* (- 1.0 (aref w 7)) current))))

(cl-declaim
 (ftype (function (fsrs-parameters float fsrs-rating) float)
  fsrs-parameters-next-difficulty))

(cl-defun fsrs-parameters-next-difficulty (self d fsrs-rating)
 "Update difficulty after user rating.

SELF is FSRS-PARAMETERS instance. D is previous difficulty value.
FSRS-RATING is user's response rating.

Returns adjusted single-float between 1.0 and 10.0."
 (let ((w (fsrs-parameters-weights self)) (r (fsrs-rating-integer fsrs-rating)))
   (let ((next-d (- d (* (aref w 6) (- r 3)))))
     (min
      (max
       (fsrs-parameters-mean-reversion self
        (fsrs-parameters-init-difficulty self :easy) next-d)
       1.0)
      10.0))))

(cl-declaim
 (ftype (function (fsrs-parameters float fsrs-rating) float)
  fsrs-parameters-short-term-stability))

(cl-defun fsrs-parameters-short-term-stability (self stability fsrs-rating)
 "Adjust short-term stability for cards in learning/relearning state.

SELF is a FSRS-PARAMETERS instance containing weight coefficients.
STABILITY is a non-negative single float representing current memory
stability. FSRS-RATING indicates user's recall correctness.

Returns adjusted stability as non-negative single float."
 (let ((w (fsrs-parameters-weights self)) (r (fsrs-rating-integer fsrs-rating)))
   (* stability (exp (* (aref w 17) (+ (- r 3) (aref w 18)))))))

(cl-declaim
 (ftype
  (function (fsrs-parameters float float (float 0.0 1.0) fsrs-rating) float)
  fsrs-parameters-next-recall-stability))

(cl-defun fsrs-parameters-next-recall-stability (self d s r fsrs-rating)
 "Calculate new stability after successful recall.

SELF is a FSRS-PARAMETERS instance containing weight coefficients. D is a
non-negative single float representing days since last review. S is a
non-negative single float representing current memory stability. R is
a single float between 0.0-1.0 representing memory retrievability.
FSRS-RATING indicates user's answer difficulty.

Returns new stability as non-negative single float."
 (let ((w (fsrs-parameters-weights self)))
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
            (1- (exp (* (- 1.0 r) (aref w 10)))) hard-penalty easy-bonus))))))

(cl-declaim
 (ftype (function (fsrs-parameters float float (float 0.0 1.0)) float)
  fsrs-parameters-next-forget-stability))

(cl-defun fsrs-parameters-next-forget-stability (self d s r)
 "Compute new stability after forgetting a card.

SELF is a FSRS-PARAMETERS instance containing weight coefficients. D is a
non-negative single float representing days elapsed since last review.
S is a non-negative single float representing previous memory
stability. R is a single float between 0.0-1.0 representing memory
retrievability.

Returns recalculated stability as non-negative single float."
 (let ((w (fsrs-parameters-weights self)))
   (* (aref w 11) (expt d (- (aref w 12))) (1- (expt (1+ s) (aref w 13)))
      (exp (* (- 1 r) (aref w 14))))))

(cl-defstruct (fsrs-scheduler (:copier fsrs-copy-scheduler) (:constructor nil))
 "Abstract base type for FSRS scheduling strategies.

PARAMETERS holds model configuration and weights."
 (parameters (fsrs-make-parameters) :type fsrs-parameters))

(cl-defun fsrs-make-scheduler
 (&rest args &key
  (parameters
   (progn
    (cl-remf args :parameters)
    (cl-remf args :enable-short-term-p)
    (apply #'fsrs-make-parameters args)))
  (enable-short-term-p t) &allow-other-keys)
 "Create concrete scheduler instance with specified parameters.

PARAMETERS can override default model weights and settings.
ENABLE-SHORT-TERM-P selects between learning phase strategies. Other
ARGS is passed to FSRS-MAKE-PARAMETERS as given.

Returns FSRS-BASIC-SCHEDULER if true, FSRS-LONG-TERM-SCHEDULER otherwise."
 (funcall
  (if enable-short-term-p
      'fsrs-make-basic-scheduler
      'fsrs-make-long-term-scheduler)
  :parameters parameters))

(cl-declaim
 (ftype #'(fsrs-scheduler fsrs-scheduling-cards) fsrs-scheduler-init-ds))

(cl-defun fsrs-scheduler-init-ds (self s)
 "Initialize difficulty/stability for new cards in SCHEDULING-CARDS.

SELF is FSRS-SCHEDULER instance. S contains card variants to initialize.

Sets values for all rating paths using PARAMETERS."
 (let ((fsrs-parameters (fsrs-scheduler-parameters self)))
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
             (fsrs-parameters-init-stability fsrs-parameters :easy)))))

(cl-declaim
 (ftype #'(fsrs-scheduler fsrs-scheduling-cards fsrs-card)
  fsrs-scheduler-next-ds))

(cl-defun fsrs-scheduler-next-ds (self s fsrs-card)
 "Update difficulty/stability for existing cards after review.

SELF is FSRS-SCHEDULER instance. S contains card variants to update. FSRS-CARD
is previous state before review.

Applies different stability formulas for learning/review phases."
 (let ((fsrs-parameters (fsrs-scheduler-parameters self)))
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
                  last-s retrievability :easy))))))))

(cl-defgeneric fsrs-scheduler-repeat (self fsrs-card &optional fsrs-now)
 (:documentation "Generate scheduling options after card review.

SELF is FSRS-SCHEDULER instance. FSRS-CARD is item being rescheduled.
FSRS-NOW is current timestamp.

Returns updated FSRS-SCHEDULING-CARDS with new intervals and states."))

(cl-declaim
 (ftype
  (function (fsrs-scheduler fsrs-card fsrs-rating &optional fsrs-timestamp)
   (cl-values fsrs-card fsrs-review-log))
  fsrs-scheduler-review-card))

(cl-defun fsrs-scheduler-review-card
 (self fsrs-card fsrs-rating &optional (fsrs-now (fsrs-now)))
 "Process user review rating and update card accordingly.

SELF is FSRS-SCHEDULER instance. FSRS-CARD is item being reviewed.
FSRS-RATING is user's response (:again/:hard/:good/:easy). FSRS-NOW is review
timestamp.

Returns updated FSRS-CARD and FSRS-REVIEW-LOG for record-keeping."
 (let* ((fsrs-scheduling-cards (fsrs-scheduler-repeat self fsrs-card fsrs-now))
        (fsrs-scheduling-info (cl-getf fsrs-scheduling-cards fsrs-rating))
        (fsrs-card (fsrs-scheduling-info-card fsrs-scheduling-info))
        (fsrs-review-log
         (fsrs-scheduling-info-review-log fsrs-scheduling-info)))
   (cl-values fsrs-card fsrs-review-log)))

(cl-defstruct
 (fsrs-basic-scheduler (:copier fsrs-copy-basic-scheduler)
  (:constructor fsrs-make-basic-scheduler) (:include fsrs-scheduler))
 "Implements FSRS's short-term learning phase strategy.

Handles :new/:learning/:relearning states with graduated intervals.
Prioritizes minimum viable intervals (minutes) for failed cards
and progressive day-based scheduling for successful recalls.")

(cl-declaim
 (ftype #'(fsrs-basic-scheduler fsrs-scheduling-cards fsrs-state)
  fsrs-basic-scheduler-update-state))

(cl-defun fsrs-basic-scheduler-update-state (self cards fsrs-state)
 "Update card states for short-term scheduling strategy.

SELF is FSRS-BASIC-SCHEDULER instance. CARDS is FSRS-SCHEDULING-CARDS container.
FSRS-STATE is current learning phase of original card.

Sets :review state for successful paths and :relearning for failures."
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
 "Assign due dates and intervals for all rating paths.

SELF is FSRS-BASIC-SCHEDULER instance. CARDS contains scheduling options to
modify. FSRS-NOW is current timestamp basis for due dates. EASY-INTERVAL,
GOOD-INTERVAL, HARD-INTERVAL, and AGAIN-INTERVAL specify days until
next review for each rating."
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

(cl-defmethod fsrs-scheduler-repeat
 ((self fsrs-basic-scheduler) fsrs-card &optional (fsrs-now (fsrs-now)))
 "Generate next scheduling options after FSRS-BASIC-SCHEDULER review.

SELF is FSRS-BASIC-SCHEDULER instance. FSRS-CARD is item being rescheduled.
FSRS-NOW is current timestamp.

Handles state transitions and interval calculations for learning
phases. Returns updated FSRS-SCHEDULING-CARDS with new intervals and logs."
 (let ((fsrs-card (fsrs-copy-card fsrs-card))
       (fsrs-parameters (fsrs-scheduler-parameters self)))
   (setf (fsrs-card-elapsed-days fsrs-card)
           (if (eq (fsrs-card-state fsrs-card) :new)
               0
               (fsrs-seconds-days
                (fsrs-timestamp-difference fsrs-now
                 (fsrs-card-last-review fsrs-card))))
         (fsrs-card-last-review fsrs-card) fsrs-now)
   (cl-incf (fsrs-card-repeats fsrs-card))
   (let ((s (fsrs-make-scheduling-cards :card fsrs-card)))
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

(cl-defstruct
 (fsrs-long-term-scheduler (:copier fsrs-copy-long-term-scheduler)
  (:constructor fsrs-make-long-term-scheduler) (:include fsrs-scheduler))
 "Optimizes FSRS for long-term retention at the expense of learning phases.

Forces all cards into :review state immediately, using geometrically
increasing intervals to maximize memory stability. Suitable for advanced
learners prioritizing efficiency over initial memorization.")

(cl-declaim
 (ftype #'(fsrs-long-term-scheduler fsrs-scheduling-cards fsrs-state)
  fsrs-long-term-scheduler-update-state))

(cl-defun fsrs-long-term-scheduler-update-state (self cards fsrs-state)
 "Force all scheduling paths to :review state.

SELF is FSRS-LONG-TERM-SCHEDULER instance. CARDS is FSRS-SCHEDULING-CARDS
container.

Ignores original FSRS-STATE to prioritize long-term retention strategy."
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
 "Schedule all ratings as day-based intervals in progression.

SELF is FSRS-LONG-TERM-SCHEDULER instance. CARDS contains scheduling
options to modify. FSRS-NOW is basis for due date calculations.

Ensures EASY-INTERVAL > GOOD-INTERVAL > HARD-INTERVAL > AGAIN-INTERVAL
for spaced progression."
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

(cl-defmethod fsrs-scheduler-repeat
 ((self fsrs-long-term-scheduler) fsrs-card &optional (fsrs-now (fsrs-now)))
 "Generate long-term focused scheduling options.

SELF is FSRS-LONG-TERM-SCHEDULER instance. FSRS-CARD is item being rescheduled.
FSRS-NOW is current timestamp.

Maintains all cards in review state with geometrically increasing
intervals. Returns FSRS-SCHEDULING-CARDS with logarithmic interval
progression."
 (let ((fsrs-card (fsrs-copy-card fsrs-card))
       (fsrs-parameters (fsrs-scheduler-parameters self)))
   (setf (fsrs-card-elapsed-days fsrs-card)
           (if (eq (fsrs-card-state fsrs-card) :new)
               0
               (fsrs-seconds-days
                (fsrs-timestamp-difference fsrs-now
                 (fsrs-card-last-review fsrs-card))))
         (fsrs-card-last-review fsrs-card) fsrs-now)
   (cl-incf (fsrs-card-repeats fsrs-card))
   (let ((s (fsrs-make-scheduling-cards :card fsrs-card)))
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
