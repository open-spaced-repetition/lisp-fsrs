;;; fsrs.el --- Free Spaced Repetition Scheduler -*- lexical-binding: t -*-

;; Copyright (C) 2025 Open Spaced Repetition

;; Author: Open Spaced Repetition
;; Maintainer: Open Spaced Repetition
;; Version: 6.0
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

;; FSRS (Free Spaced Repetition Scheduler) is a spaced repetition
;; algorithm that optimizes review scheduling by adapting to individual
;; memory patterns, outperforming traditional algorithms like SM-2.

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

(cl-deftype fsrs-state nil "Learning phase progression state."
 '(member :learning :review :relearning))

(cl-declaim (ftype (function (fsrs-state) (integer 0 3)) fsrs-state-integer)
 (inline fsrs-state-integer))

(cl-defun fsrs-state-integer (fsrs-state)
 "Convert FSRS-STATE to an integer (0-3)."
 (cl-ecase fsrs-state (:new 0) (:learning 1) (:review 2) (:relearning 3)))

(cl-deftype fsrs-rating nil "User response rating for memory recall success."
 '(member :again :hard :good :easy))

(cl-declaim (ftype (function (fsrs-rating) (integer 1 4)) fsrs-rating-integer)
 (inline fsrs-rating-integer))

(cl-defun fsrs-rating-integer (fsrs-rating)
 "Convert FSRS-RATING to an integer (1-4).

FSRS-RATING is the user's response quality (:again/:hard/:good/:easy)."
 (cl-ecase fsrs-rating (:again 1) (:hard 2) (:good 3) (:easy 4)))

(cl-deftype fsrs-parameters nil
 "Array type containing 21 single-floats for FSRS parameters." 'vector)

(defconst fsrs-default-parameters
 (cl-coerce
  '(0.2172 1.1771 3.2602 16.1507 7.0114 0.57 2.0966 0.0069 1.5261 0.112 1.0178
    1.849 0.1133 0.3127 2.2934 0.2191 3.0004 0.7536 0.3332 0.1437 0.2)
  'vector))

(defconst fsrs-lower-bounds-parameters
 (cl-coerce
  '(0.001 0.001 0.001 0.001 1.0 0.001 0.001 0.001 0.0 0.0 0.001 0.001 0.001
    0.001 0.0 0.0 1.0 0.0 0.0 0.0 0.1)
  'vector))

(defconst fsrs-upper-bounds-parameters
 (cl-coerce
  '(100.0 100.0 100.0 100.0 10.0 4.0 4.0 0.75 4.5 0.8 3.5 5.0 0.25 0.9 4.0 1.0
    6.0 2.0 2.0 0.8 0.8)
  'vector))

(defconst fsrs-minimum-difficulty 1.0 "Minimum allowed difficulty value (1.0).")

(defconst fsrs-maximum-difficulty 10.0
 "Maximum allowed difficulty value (10.0).")

(defconst fsrs-minimum-stability 0.001
 "Minimum allowed stability value (0.001).")

(defconst fsrs-fuzz-ranges
 '(((2.5 :day) (7.0 :day) 0.15) ((7.0 :day) (20.0 :day) 0.1)
   ((20.0 :day) nil 0.05)))

(cl-deftype fsrs-difficulty nil
 "Single-float type representing item difficulty (1.0-10.0)."
 (list 'float fsrs-minimum-difficulty fsrs-maximum-difficulty))

(cl-deftype fsrs-stability nil
 "Single-float type representing memory stability (>= 0.001)."
 (list 'float fsrs-minimum-stability))

(cl-deftype fsrs-retrievability nil
 "Single-float type representing recall probability (0.0-1.0)."
 '(float 0.0 1.0))

(cl-defstruct
 (fsrs-card (:copier fsrs-copy-card) (:constructor fsrs-make-card))
 "Represents a memorization item with scheduling state and memory metrics.

CARD-ID is the unique identifier for the card.
STATE is the current learning phase (:learning/:review/:relearning).
STEP is the current position in learning/relearning steps.
STABILITY is the memory retention strength (higher = more stable).
DIFFICULTY is the item complexity (1.0-10.0 scale).
DUE is the timestamp for next review.
LAST-REVIEW is the timestamp of most recent review or NIL if new."
 (card-id 0 :type fixnum) (state :learning :type fsrs-state)
 (step 0 :type (or fixnum null)) (stability nil :type (or float null))
 (difficulty nil :type (or float null)) (due (fsrs-now) :type fsrs-timestamp)
 (last-review nil :type (or fsrs-timestamp null)))

(cl-defstruct
 (fsrs-review-log (:copier fsrs-copy-review-log)
  (:constructor fsrs-make-review-log))
 "Record of individual review event.

CARD-ID is the identifier of the reviewed card.
RATING is the user's response quality (:again/:hard/:good/:easy).
REVIEW-DATETIME is the timestamp when review occurred.
REVIEW-DURATION is the time spent reviewing in seconds or NIL."
 (card-id 0 :type fixnum) (rating :again :type fsrs-rating)
 (review-datetime (fsrs-now) :type fsrs-timestamp)
 (review-duration nil :type (or fixnum null)))

(defconst fsrs-time-units
 '((:sec . 1) (:minute . 60) (:hour . 3600) (:day . 86400)))

(cl-deftype fsrs-timespan nil
 "Cons type representing a duration with multiple time units." 'cons)

(cl-declaim
 (ftype (function (fsrs-timespan) (or fixnum float)) fsrs-timespan-seconds))

(cl-defun fsrs-timespan-seconds (fsrs-timespan)
 "Convert FSRS-TIMESPAN to total seconds.

FSRS-TIMESPAN is a cons list of (AMOUNT UNIT) pairs."
 (cl-loop for (amount unit) on fsrs-timespan by #'cddr sum
  (* amount (alist-get unit fsrs-time-units nil nil #'eql))))

(cl-declaim (ftype (function (fsrs-timespan) float) fsrs-timespan-days))

(cl-defun fsrs-timespan-days (fsrs-timespan)
 "Convert FSRS-TIMESPAN to total days.

FSRS-TIMESPAN is a cons list of (AMOUNT UNIT) pairs."
 (/ (fsrs-timespan-seconds fsrs-timespan) 86400.0))

(cl-declaim (ftype (function (fixnum) fsrs-timespan) fsrs-seconds-timespan))

(cl-defun fsrs-seconds-timespan (seconds)
 "Convert SECONDS to timespan with multiple units.

SECONDS is the total duration to convert."
 (cl-loop for (unit . amount) in (reverse fsrs-time-units) for (part remainder)
  = (cl-multiple-value-list (cl-truncate (or remainder seconds) amount)) unless
  (zerop part) nconc (list part unit) until (zerop remainder)))

(cl-declaim (ftype (function (&rest t) fsrs-timespan) fsrs-make-timespan))

(cl-defun fsrs-make-timespan (&rest args)
 "Create timespan from alternating UNIT AMOUNT pairs.

ARGS is a plist of time units and amounts."
 (cl-loop for (unit amount) on args by #'cddr nconc (list amount unit)))

(cl-declaim
 (ftype (function (fsrs-timespan (or float fixnum)) fsrs-timespan)
  fsrs-timespan*))

(cl-defun fsrs-timespan* (fsrs-timespan factor)
 "Multiply FSRS-TIMESPAN by FACTOR.

FSRS-TIMESPAN is the duration to scale. FACTOR is the multiplier."
 (fsrs-seconds-timespan
  (cl-nth-value 0
   (cl-truncate (* (fsrs-timespan-seconds fsrs-timespan) factor)))))

(cl-declaim
 (ftype (function (fsrs-timespan &optional fsrs-timestamp) fsrs-timestamp)
  fsrs-timespan-apply))

(cl-defun fsrs-timespan-apply
 (fsrs-timespan &optional (fsrs-timestamp (fsrs-now)))
 "Apply FSRS-TIMESPAN to FSRS-TIMESTAMP.

FSRS-TIMESPAN is the duration to add. FSRS-TIMESTAMP is the base time."
 (cl-loop with result = fsrs-timestamp for (amount unit) on fsrs-timespan by
  #'cddr do (setf result (fsrs-timestamp+ result amount unit)) finally
  (cl-return result)))

(cl-defmacro fsrs-define-timespan-operator (operator)
 "Define a timespan operator function that applying OPERATOR to timespans.

OPERATOR is the arithmetic function to apply to timespan values. The generated
function will convert timespans to seconds, apply OPERATOR, then convert back to
timespan format."
 (let ((args (make-symbol (symbol-name 'args))))
   (list 'cl-defun
         (intern
          (cl-concatenate 'string (symbol-name 'fsrs-timespan)
           (if (> (length (symbol-name operator)) 2)
               "-"
               "")
           (symbol-name operator)))
         (list '&rest args)
         (cl-concatenate 'string "Apply " (symbol-name operator) " to TIMESPANs.

ARGS is the list of timespan values to operate on. Each timespan is converted to
seconds before applying the operation, with the result converted back to
timespan format.")
         (list 'fsrs-seconds-timespan
               (list 'apply (list 'cl-function operator)
                     (list 'cl-mapcar
                           (list 'cl-function 'fsrs-timespan-seconds) args))))))

(fsrs-define-timespan-operator +)

(fsrs-define-timespan-operator -)

(fsrs-define-timespan-operator min)

(fsrs-define-timespan-operator max)

(cl-declaim (ftype (function ((or fixnum float)) fixnum) fsrs-seconds-days))

(cl-defun fsrs-seconds-days (secs)
 "Convert SECS to integer days by truncating fractional part.

SECS can be fixnum or double-float."
 (cl-nth-value 0 (cl-truncate secs 86400)))

(cl-defstruct
 (fsrs-scheduler (:copier fsrs-copy-scheduler)
  (:constructor fsrs--make-scheduler))
 "Container for FSRS scheduling configuration and parameters.

PARAMETERS is the array of FSRS algorithm weights and coefficients.
DESIRED-RETENTION is the target probability of successful recall (0.0-1.0).
LEARNING-STEPS is the list of time intervals for initial learning phase.
RELEARNING-STEPS is the list of time intervals for relearning phase.
MAXIMUM-INTERVAL is the upper bound for scheduling intervals.
ENABLE-FUZZING-P is the flag to randomize intervals within bounds."
 (parameters fsrs-default-parameters :type fsrs-parameters)
 (desired-retention 0.9 :type float)
 (learning-steps '((1 :minute) (10 :minute)) :type list)
 (relearning-steps '((10 :minute)) :type list)
 (maximum-interval '(36500 :day) :type fsrs-timespan)
 (enable-fuzzing-p t :type boolean))

(cl-declaim
 (ftype (function (fsrs-scheduler) positive-fixnum)
  fsrs-scheduler-maximum-interval-days))

(cl-defun fsrs-scheduler-maximum-interval-days (fsrs-scheduler)
 "Get maximum interval in days from FSRS-SCHEDULER.

FSRS-SCHEDULER is the FSRS scheduler instance."
 (cl-destructuring-bind (n day)
  (fsrs-scheduler-maximum-interval fsrs-scheduler) (cl-assert (eq day :day)) n))

(cl-declaim (ftype #'(fsrs-parameters) fsrs-scheduler-validate-parameters))

(cl-defun fsrs-scheduler-validate-parameters (fsrs-parameters)
 "Validate FSRS-PARAMETERS against bounds.

FSRS-PARAMETERS is the FSRS parameter array to check."
 (cl-loop for p across fsrs-parameters for lower across
  fsrs-lower-bounds-parameters for upper across fsrs-upper-bounds-parameters do
  (cl-assert (<= lower p upper))))

(cl-defun fsrs-make-scheduler (&rest args)
 "Create scheduler instance with specified configuration.

ARGS can override default parameters and settings."
 (let ((fsrs-scheduler (apply #'fsrs--make-scheduler args)))
   (fsrs-scheduler-validate-parameters
    (fsrs-scheduler-parameters fsrs-scheduler))
   fsrs-scheduler))

(cl-declaim
 (ftype
  (function (fsrs-scheduler fsrs-card &optional fsrs-timestamp)
   fsrs-retrievability)
  fsrs-scheduler-card-retrievability))

(cl-defun fsrs-scheduler-card-retrievability
 (fsrs-scheduler fsrs-card &optional (current-time (fsrs-now)))
 "Calculate current recall probability for FSRS-CARD.

FSRS-SCHEDULER is the scheduling configuration. FSRS-CARD is the item
to evaluate. CURRENT-TIME is the optional timestamp to use as now."
 (unless (fsrs-card-last-review fsrs-card)
   (cl-return-from fsrs-scheduler-card-retrievability 0.0))
 (let* ((elapsed-days
         (max 0
              (fsrs-seconds-days
               (fsrs-timestamp-difference current-time
                (fsrs-card-last-review fsrs-card)))))
        (decay (- (aref (fsrs-scheduler-parameters fsrs-scheduler) 20)))
        (factor (- (expt 0.9 (/ 1 decay)) 1))
        (fsrs-retrievability
         (expt (1+ (/ (* factor elapsed-days) (fsrs-card-stability fsrs-card)))
               decay)))
   fsrs-retrievability))

(cl-declaim
 (ftype (function (fsrs-scheduler float) fsrs-difficulty)
  fsrs-scheduler-clamp-difficulty))

(cl-defun fsrs-scheduler-clamp-difficulty (fsrs-scheduler fsrs-difficulty)
 "Clamp FSRS-DIFFICULTY to valid range (1.0-10.0).

FSRS-SCHEDULER is unused. FSRS-DIFFICULTY is the value to clamp."
 (ignore fsrs-scheduler)
 (max fsrs-minimum-difficulty (min fsrs-maximum-difficulty fsrs-difficulty)))

(cl-declaim
 (ftype (function (fsrs-scheduler float) fsrs-stability)
  fsrs-scheduler-clamp-stability))

(cl-defun fsrs-scheduler-clamp-stability (fsrs-scheduler fsrs-stability)
 "Clamp FSRS-STABILITY to minimum value (0.001).

FSRS-SCHEDULER is unused. FSRS-STABILITY is the value to clamp."
 (ignore fsrs-scheduler) (max fsrs-minimum-stability fsrs-stability))

(cl-declaim
 (ftype (function (fsrs-scheduler fsrs-rating) fsrs-stability)
  fsrs-scheduler-initial-stability))

(cl-defun fsrs-scheduler-initial-stability (fsrs-scheduler fsrs-rating)
 "Compute initial stability after first review.

FSRS-SCHEDULER contains the parameter weights. FSRS-RATING is the user's
response."
 (let ((fsrs-stability
        (aref (fsrs-scheduler-parameters fsrs-scheduler)
              (1- (fsrs-rating-integer fsrs-rating)))))
   (fsrs-scheduler-clamp-stability fsrs-scheduler fsrs-stability)))

(cl-declaim
 (ftype (function (fsrs-scheduler fsrs-rating) fsrs-difficulty)
  fsrs-scheduler-initial-difficulty))

(cl-defun fsrs-scheduler-initial-difficulty (fsrs-scheduler fsrs-rating)
 "Compute initial difficulty after first review for FSRS-RATING.

FSRS-SCHEDULER contains the parameter weights. FSRS-RATING is the user's
response quality."
 (let ((fsrs-difficulty
        (1+
         (- (aref (fsrs-scheduler-parameters fsrs-scheduler) 4)
            (exp
             (* (aref (fsrs-scheduler-parameters fsrs-scheduler) 5)
                (1- (fsrs-rating-integer fsrs-rating))))))))
   (fsrs-scheduler-clamp-difficulty fsrs-scheduler fsrs-difficulty)))

(cl-declaim
 (ftype (function (fsrs-scheduler fsrs-stability) fixnum)
  fsrs-scheduler-next-interval))

(cl-defun fsrs-scheduler-next-interval (fsrs-scheduler fsrs-stability)
 "Calculate next review interval in days for given FSRS-STABILITY.

FSRS-SCHEDULER contains scheduling parameters. FSRS-STABILITY is the memory
strength."
 (let* ((decay (- (aref (fsrs-scheduler-parameters fsrs-scheduler) 20)))
        (factor (1- (expt 0.9 (/ 1 decay))))
        (interval
         (* (/ fsrs-stability factor)
            (1-
             (expt (fsrs-scheduler-desired-retention fsrs-scheduler)
                   (/ decay))))))
   (min (max (cl-nth-value 0 (cl-round interval)) 1)
        (fsrs-scheduler-maximum-interval-days fsrs-scheduler))))

(cl-declaim
 (ftype (function (fsrs-scheduler fsrs-stability fsrs-rating) fsrs-stability)
  fsrs-scheduler-short-term-stability))

(cl-defun fsrs-scheduler-short-term-stability
 (fsrs-scheduler fsrs-stability fsrs-rating)
 "Calculate short-term stability adjustment after reviewing with FSRS-RATING.

FSRS-SCHEDULER contains model parameters. FSRS-STABILITY is the current memory
strength. FSRS-RATING is the user's response quality (:again/:hard/:good/:easy)."
 (let* ((increase
         (*
          (exp
           (* (aref (fsrs-scheduler-parameters fsrs-scheduler) 17)
              (+ (- (fsrs-rating-integer fsrs-rating) 3)
                 (aref (fsrs-scheduler-parameters fsrs-scheduler) 18))))
          (expt fsrs-stability
                (- (aref (fsrs-scheduler-parameters fsrs-scheduler) 19)))))
        (new-stability
         (* fsrs-stability
            (if (cl-member fsrs-rating '(:good :easy))
                (max increase 1.0)
                increase))))
   (fsrs-scheduler-clamp-stability fsrs-scheduler new-stability)))

(cl-declaim
 (ftype (function (fsrs-scheduler fsrs-difficulty fsrs-rating) fsrs-difficulty)
  fsrs-scheduler-next-difficulty))

(cl-defun fsrs-scheduler-next-difficulty
 (fsrs-scheduler fsrs-difficulty fsrs-rating)
 "Calculate next difficulty level after reviewing with FSRS-RATING.

FSRS-SCHEDULER contains model parameters. FSRS-DIFFICULTY is the current item
complexity. FSRS-RATING is the user's response quality
 (:again/:hard/:good/:easy)."
 (let* ((linear-damping
         (* (/ (- 10.0 fsrs-difficulty) 9.0)
            (-
             (* (aref (fsrs-scheduler-parameters fsrs-scheduler) 6)
                (- (fsrs-rating-integer fsrs-rating) 3)))))
        (mean-reversion
         (+
          (* (aref (fsrs-scheduler-parameters fsrs-scheduler) 7)
             (fsrs-scheduler-initial-difficulty fsrs-scheduler :easy))
          (* (- 1 (aref (fsrs-scheduler-parameters fsrs-scheduler) 7))
             (+ fsrs-difficulty linear-damping)))))
   (fsrs-scheduler-clamp-difficulty fsrs-scheduler mean-reversion)))

(cl-declaim
 (ftype
  (function (fsrs-scheduler fsrs-difficulty fsrs-stability fsrs-retrievability)
   fsrs-stability)
  fsrs-scheduler-next-forget-stability))

(cl-defun fsrs-scheduler-next-forget-stability
 (fsrs-scheduler fsrs-difficulty fsrs-stability fsrs-retrievability)
 "Calculate stability after forgetting during review.

FSRS-SCHEDULER contains model parameters. FSRS-DIFFICULTY is the item
complexity. FSRS-STABILITY is the current memory strength.
FSRS-RETRIEVABILITY is the recall probability."
 (let ((long-term
        (* (aref (fsrs-scheduler-parameters fsrs-scheduler) 11)
           (expt fsrs-difficulty
                 (- (aref (fsrs-scheduler-parameters fsrs-scheduler) 12)))
           (1-
            (expt (1+ fsrs-stability)
                  (aref (fsrs-scheduler-parameters fsrs-scheduler) 13)))
           (exp
            (* (- 1 fsrs-retrievability)
               (aref (fsrs-scheduler-parameters fsrs-scheduler) 14)))))
       (short-term
        (/ fsrs-stability
           (exp
            (* (aref (fsrs-scheduler-parameters fsrs-scheduler) 17)
               (aref (fsrs-scheduler-parameters fsrs-scheduler) 18))))))
   (min long-term short-term)))

(cl-declaim
 (ftype
  (function
   (fsrs-scheduler fsrs-difficulty fsrs-stability fsrs-retrievability
    fsrs-rating)
   fsrs-stability)
  fsrs-scheduler-next-recall-stability))

(cl-defun fsrs-scheduler-next-recall-stability
 (fsrs-scheduler fsrs-difficulty fsrs-stability fsrs-retrievability
  fsrs-rating)
 "Calculate stability after successful recall with FSRS-RATING.

FSRS-SCHEDULER contains model parameters. FSRS-DIFFICULTY is the item
complexity. FSRS-STABILITY is the current memory strength.
FSRS-RETRIEVABILITY is the recall probability. FSRS-RATING is the user's
response quality (:again/:hard/:good/:easy)."
 (let* ((hard-penalty
         (if (eq fsrs-rating :hard)
             (aref (fsrs-scheduler-parameters fsrs-scheduler) 15)
             1.0))
        (easy-bonus
         (if (eq fsrs-rating :easy)
             (aref (fsrs-scheduler-parameters fsrs-scheduler) 16)
             1.0))
        (new-stability
         (* fsrs-stability
            (1+
             (* (exp (aref (fsrs-scheduler-parameters fsrs-scheduler) 8))
                (- 11.0 fsrs-difficulty)
                (expt fsrs-stability
                      (- (aref (fsrs-scheduler-parameters fsrs-scheduler) 9)))
                (1-
                 (exp
                  (* (- 1.0 fsrs-retrievability)
                     (aref (fsrs-scheduler-parameters fsrs-scheduler) 10))))
                hard-penalty easy-bonus)))))
   (fsrs-scheduler-clamp-stability fsrs-scheduler new-stability)))

(cl-declaim
 (ftype (function (fsrs-scheduler fsrs-timespan) fsrs-timespan)
  fsrs-scheduler-fuzzed-interval))

(cl-defun fsrs-scheduler-fuzzed-interval (fsrs-scheduler interval)
 "Apply random fuzzing to INTERVAL based on fuzz ranges.

FSRS-SCHEDULER contains fuzzing configuration. INTERVAL is the base timespan."
 (let ((days (fsrs-timespan-days interval)))
   (cond ((< days 2.5) interval)
         (t
          (let* ((delta
                  (cl-loop for (start end factor) in fsrs-fuzz-ranges sum
                   (* factor
                      (max 0
                           (min (or (when end (fsrs-timespan-days end)) days)
                                days)
                           (fsrs-timespan-days start)))))
                 (min-ivl
                  (max 2
                       (min (cl-nth-value 0 (cl-round (- days delta)))
                            (fsrs-scheduler-maximum-interval-days
                             fsrs-scheduler))))
                 (max-ivl
                  (min (cl-nth-value 0 (cl-round (+ days delta)))
                       (fsrs-scheduler-maximum-interval-days fsrs-scheduler)))
                 (fuzzed-days
                  (min
                   (cl-nth-value 0
                    (cl-round (+ min-ivl (cl-random (- max-ivl min-ivl -1)))))
                   (fsrs-scheduler-maximum-interval-days fsrs-scheduler))))
            (fsrs-make-timespan :day fuzzed-days))))))

(cl-declaim
 (ftype
  (function
   (fsrs-scheduler fsrs-card fsrs-rating &optional fsrs-timestamp
    (or null fixnum))
   (cl-values fsrs-card fsrs-review-log))
  fsrs-scheduler-review-card))

(cl-defun fsrs-scheduler-review-card
 (fsrs-scheduler fsrs-card fsrs-rating &optional (review-time (fsrs-now))
  review-duration)
 "Process FSRS-CARD review with FSRS-RATING and update scheduling state.

FSRS-SCHEDULER contains configuration parameters. FSRS-CARD is the item being
reviewed. FSRS-RATING is the user's response quality (:again/:hard/:good/:easy).
REVIEW-TIME is the optional timestamp of review. REVIEW-DURATION is the optional
duration."
 (let* ((fsrs-card (fsrs-copy-card fsrs-card))
        (days-since-last
         (when (fsrs-card-last-review fsrs-card)
           (fsrs-seconds-days
            (fsrs-timestamp-difference review-time
             (fsrs-card-last-review fsrs-card)))))
        (fsrs-scheduler-next-interval
         (cl-ecase (fsrs-card-state fsrs-card)
          (:learning
           (cond
            ((and (null (fsrs-card-stability fsrs-card))
                  (null (fsrs-card-difficulty fsrs-card)))
             (setf (fsrs-card-stability fsrs-card)
                     (fsrs-scheduler-initial-stability fsrs-scheduler
                      fsrs-rating)
                   (fsrs-card-difficulty fsrs-card)
                     (fsrs-scheduler-initial-difficulty fsrs-scheduler
                      fsrs-rating)))
            ((and days-since-last (< days-since-last 1))
             (setf (fsrs-card-stability fsrs-card)
                     (fsrs-scheduler-short-term-stability fsrs-scheduler
                      (fsrs-card-stability fsrs-card) fsrs-rating)
                   (fsrs-card-difficulty fsrs-card)
                     (fsrs-scheduler-next-difficulty fsrs-scheduler
                      (fsrs-card-difficulty fsrs-card) fsrs-rating)))
            (t
             (setf (fsrs-card-stability fsrs-card)
                     (fsrs-scheduler-next-recall-stability fsrs-scheduler
                      (fsrs-card-difficulty fsrs-card)
                      (fsrs-card-stability fsrs-card)
                      (fsrs-scheduler-card-retrievability fsrs-scheduler
                       fsrs-card review-time)
                      fsrs-rating)
                   (fsrs-card-difficulty fsrs-card)
                     (fsrs-scheduler-next-difficulty fsrs-scheduler
                      (fsrs-card-difficulty fsrs-card) fsrs-rating))))
           (cond
            ((or (null (fsrs-scheduler-learning-steps fsrs-scheduler))
                 (and
                  (>= (fsrs-card-step fsrs-card)
                      (length (fsrs-scheduler-learning-steps fsrs-scheduler)))
                  (cl-member fsrs-rating '(:hard :good :easy))))
             (setf (fsrs-card-state fsrs-card) :review
                   (fsrs-card-step fsrs-card) nil)
             (let ((days
                    (fsrs-scheduler-next-interval fsrs-scheduler
                     (fsrs-card-stability fsrs-card))))
               (fsrs-make-timespan :day days)))
            (t
             (cl-ecase fsrs-rating
              (:again
               (nth (setf (fsrs-card-step fsrs-card) 0)
                    (fsrs-scheduler-learning-steps fsrs-scheduler)))
              (:hard
               (cond
                ((and (= (fsrs-card-step fsrs-card) 0)
                      (=
                       (length (fsrs-scheduler-learning-steps fsrs-scheduler))
                       1))
                 (fsrs-timespan*
                  (nth 0 (fsrs-scheduler-learning-steps fsrs-scheduler)) 1.5))
                ((and (= (fsrs-card-step fsrs-card) 0)
                      (>=
                       (length (fsrs-scheduler-learning-steps fsrs-scheduler))
                       2))
                 (fsrs-timespan*
                  (fsrs-timespan+
                   (nth 0 (fsrs-scheduler-learning-steps fsrs-scheduler))
                   (nth 1 (fsrs-scheduler-learning-steps fsrs-scheduler)))
                  (/ 2.0)))
                (t
                 (nth (fsrs-card-step fsrs-card)
                      (fsrs-scheduler-learning-steps fsrs-scheduler)))))
              (:good
               (if (= (1+ (fsrs-card-step fsrs-card))
                      (length (fsrs-scheduler-learning-steps fsrs-scheduler)))
                   (let ((days
                          (fsrs-scheduler-next-interval fsrs-scheduler
                           (fsrs-card-stability fsrs-card))))
                     (setf (fsrs-card-state fsrs-card) :review
                           (fsrs-card-step fsrs-card) nil)
                     (fsrs-make-timespan :day days))
                   (nth (cl-incf (fsrs-card-step fsrs-card))
                        (fsrs-scheduler-learning-steps fsrs-scheduler))))
              (:easy
               (setf (fsrs-card-state fsrs-card) :review
                     (fsrs-card-step fsrs-card) nil)
               (let ((days
                      (fsrs-scheduler-next-interval fsrs-scheduler
                       (fsrs-card-stability fsrs-card))))
                 (fsrs-make-timespan :day days)))))))
          (:review
           (if (and days-since-last (< days-since-last 1))
               (setf (fsrs-card-stability fsrs-card)
                       (fsrs-scheduler-short-term-stability fsrs-scheduler
                        (fsrs-card-stability fsrs-card) fsrs-rating)
                     (fsrs-card-difficulty fsrs-card)
                       (fsrs-scheduler-next-difficulty fsrs-scheduler
                        (fsrs-card-difficulty fsrs-card) fsrs-rating))
               (setf (fsrs-card-stability fsrs-card)
                       (if (eq fsrs-rating :again)
                           (fsrs-scheduler-next-forget-stability fsrs-scheduler
                            (fsrs-card-difficulty fsrs-card)
                            (fsrs-card-stability fsrs-card)
                            (fsrs-scheduler-card-retrievability fsrs-scheduler
                             fsrs-card review-time))
                           (fsrs-scheduler-next-recall-stability fsrs-scheduler
                            (fsrs-card-difficulty fsrs-card)
                            (fsrs-card-stability fsrs-card)
                            (fsrs-scheduler-card-retrievability fsrs-scheduler
                             fsrs-card review-time)
                            fsrs-rating))
                     (fsrs-card-difficulty fsrs-card)
                       (fsrs-scheduler-next-difficulty fsrs-scheduler
                        (fsrs-card-difficulty fsrs-card) fsrs-rating)))
           (cl-ecase fsrs-rating
            (:again
             (if (null (fsrs-scheduler-relearning-steps fsrs-scheduler))
                 (let ((days
                        (fsrs-scheduler-next-interval fsrs-scheduler
                         (fsrs-card-stability fsrs-card))))
                   (fsrs-make-timespan :day days))
                 (nth
                  (setf (fsrs-card-state fsrs-card) :relearning
                        (fsrs-card-step fsrs-card) 0)
                  (fsrs-scheduler-relearning-steps fsrs-scheduler))))
            ((:hard :good :easy)
             (let ((days
                    (fsrs-scheduler-next-interval fsrs-scheduler
                     (fsrs-card-stability fsrs-card))))
               (fsrs-make-timespan :day days)))))
          (:relearning
           (if (and days-since-last (< days-since-last 1))
               (setf (fsrs-card-stability fsrs-card)
                       (fsrs-scheduler-short-term-stability fsrs-scheduler
                        (fsrs-card-stability fsrs-card) fsrs-rating)
                     (fsrs-card-difficulty fsrs-card)
                       (fsrs-scheduler-next-difficulty fsrs-scheduler
                        (fsrs-card-difficulty fsrs-card) fsrs-rating))
               (setf (fsrs-card-stability fsrs-card)
                       (fsrs-scheduler-next-recall-stability fsrs-scheduler
                        (fsrs-card-difficulty fsrs-card)
                        (fsrs-card-stability fsrs-card)
                        (fsrs-scheduler-card-retrievability fsrs-scheduler
                         fsrs-card review-time)
                        fsrs-rating)
                     (fsrs-card-difficulty fsrs-card)
                       (fsrs-scheduler-next-difficulty fsrs-scheduler
                        (fsrs-card-difficulty fsrs-card) fsrs-rating)))
           (cond
            ((or (null (fsrs-scheduler-relearning-steps fsrs-scheduler))
                 (and
                  (>= (fsrs-card-step fsrs-card)
                      (length
                       (fsrs-scheduler-relearning-steps fsrs-scheduler)))
                  (cl-member fsrs-rating '(:hard :good :easy))))
             (setf (fsrs-card-state fsrs-card) :review
                   (fsrs-card-step fsrs-card) nil)
             (let ((days
                    (fsrs-scheduler-next-interval fsrs-scheduler
                     (fsrs-card-stability fsrs-card))))
               (fsrs-make-timespan :day days)))
            (t
             (cl-ecase fsrs-rating
              (:again
               (nth (setf (fsrs-card-step fsrs-card) 0)
                    (fsrs-scheduler-relearning-steps fsrs-scheduler)))
              (:hard
               (cond
                ((and (= (fsrs-card-step fsrs-card) 0)
                      (=
                       (length
                        (fsrs-scheduler-relearning-steps fsrs-scheduler))
                       1))
                 (fsrs-timespan*
                  (nth 0 (fsrs-scheduler-relearning-steps fsrs-scheduler))
                  1.5))
                ((and (= (fsrs-card-step fsrs-card) 0)
                      (>=
                       (length
                        (fsrs-scheduler-relearning-steps fsrs-scheduler))
                       2))
                 (fsrs-timespan*
                  (fsrs-timespan+
                   (nth 0 (fsrs-scheduler-relearning-steps fsrs-scheduler))
                   (nth 1 (fsrs-scheduler-relearning-steps fsrs-scheduler)))
                  (/ 2.0)))
                (t
                 (nth (fsrs-card-step fsrs-card)
                      (fsrs-scheduler-relearning-steps fsrs-scheduler)))))
              (:good
               (if (= (1+ (fsrs-card-step fsrs-card))
                      (length
                       (fsrs-scheduler-relearning-steps fsrs-scheduler)))
                   (let ((days
                          (fsrs-scheduler-next-interval fsrs-scheduler
                           (fsrs-card-stability fsrs-card))))
                     (setf (fsrs-card-state fsrs-card) :review
                           (fsrs-card-step fsrs-card) nil)
                     (fsrs-make-timespan :day days))
                   (nth (cl-incf (fsrs-card-step fsrs-card))
                        (fsrs-scheduler-relearning-steps fsrs-scheduler))))
              (:easy
               (setf (fsrs-card-state fsrs-card) :review
                     (fsrs-card-step fsrs-card) nil)
               (let ((days
                      (fsrs-scheduler-next-interval fsrs-scheduler
                       (fsrs-card-stability fsrs-card))))
                 (fsrs-make-timespan :day days))))))))))
   (when
       (and (fsrs-scheduler-enable-fuzzing-p fsrs-scheduler)
            (eq (fsrs-card-state fsrs-card) :review))
     (setf fsrs-scheduler-next-interval
             (fsrs-scheduler-fuzzed-interval fsrs-scheduler
              fsrs-scheduler-next-interval)))
   (setf (fsrs-card-due fsrs-card)
           (fsrs-timespan-apply fsrs-scheduler-next-interval review-time)
         (fsrs-card-last-review fsrs-card) review-time)
   (cl-values fsrs-card
    (fsrs-make-review-log :card-id (fsrs-card-card-id fsrs-card) :rating
     fsrs-rating :review-datetime review-time :review-duration
     review-duration))))

(provide 'fsrs)
;;; fsrs.el ends here
