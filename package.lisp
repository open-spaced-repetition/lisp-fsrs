(defpackage lisp-fsrs
  (:use #:cl #:alexandria #:local-time)
  (:nicknames #:fsrs)
  (:export
   #:state
   #:rating
   #:parameters
   #:card
   #:make-card
   #:card-p
   #:copy-card
   #:card-card-id
   #:card-state
   #:card-step
   #:card-stability
   #:card-difficulty
   #:card-due
   #:card-last-review
   #:review-log
   #:make-review-log
   #:review-log-p
   #:copy-review-log
   #:review-log-card-id
   #:review-log-rating
   #:review-log-review-datetime
   #:review-log-review-duration
   #:scheduler
   #:make-scheduler
   #:scheduler-p
   #:copy-scheduler
   #:scheduler-parameters
   #:scheduler-desired-retention
   #:scheduler-learning-steps
   #:scheduler-relearning-steps
   #:scheduler-maximum-interval
   #:scheduler-enable-fuzzing-p
   #:scheduler-review-card
   #:scheduler-card-retrievability))

(in-package #:lisp-fsrs)

(deftype state ()
  "Learning phase progression state."
  '(member :learning :review :relearning))

(declaim (ftype (function (state) (integer 0 3)) state-integer)
         (inline state-integer))
(defun state-integer (state)
  "Convert STATE to an integer (0-3)."
  (ecase state
    (:new 0)
    (:learning 1)
    (:review 2)
    (:relearning 3)))

(deftype rating ()
  "User response rating for memory recall success."
  '(member :again :hard :good :easy))

(declaim (ftype (function (rating) (integer 1 4)) rating-integer)
         (inline rating-integer))
(defun rating-integer (rating)
  "Convert RATING to an integer (1-4).

RATING is the user's response quality (:again/:hard/:good/:easy)."
  (ecase rating
    (:again 1)
    (:hard 2)
    (:good 3)
    (:easy 4)))

(deftype parameters ()
  "Array type containing 21 single-floats for FSRS parameters."
  '(simple-array single-float (21)))

(define-constant +default-parameters+
    (coerce
     '(0.2172 1.1771 3.2602 16.1507 7.0114 0.57 2.0966 0.0069 1.5261
       0.112 1.0178 1.849 0.1133 0.3127 2.2934 0.2191 3.0004 0.7536
       0.3332 0.1437 0.2)
     'parameters)
  :test #'equalp
  :documentation "Default weight values for FSRS parameters.")

(define-constant +lower-bounds-parameters+
    (coerce
     '(0.001 0.001 0.001 0.001 1.0 0.001 0.001 0.001 0.0 0.0 0.001
       0.001 0.001 0.001 0.0 0.0 1.0 0.0 0.0 0.0 0.1)
     'parameters)
  :test #'equalp
  :documentation "Lower bounds for FSRS parameter values.")

(define-constant +upper-bounds-parameters+
    (coerce
     '(100.0 100.0 100.0 100.0 10.0 4.0 4.0 0.75 4.5 0.8 3.5 5.0 0.25
       0.9 4.0 1.0 6.0 2.0 2.0 0.8 0.8)
     'parameters)
  :test #'equalp
  :documentation "Upper bounds for FSRS parameter values.")

(defconstant +minimum-difficulty+ 1.0
  "Minimum allowed difficulty value (1.0).")

(defconstant +maximum-difficulty+ 10.0
  "Maximum allowed difficulty value (10.0).")

(defconstant +minimum-stability+ 0.001
  "Minimum allowed stability value (0.001).")

(define-constant +fuzz-ranges+
    '(((2.5 :day) (7.0 :day) 0.15)
      ((7.0 :day) (20.0 :day) 0.1)
      ((20.0 :day) nil 0.05))
  :test #'equal
  :documentation "Fuzz factor ranges for interval randomization.")

(deftype difficulty ()
  "Single-float type representing item difficulty (1.0-10.0)."
  (list 'single-float +minimum-difficulty+ +maximum-difficulty+))

(deftype stability ()
  "Single-float type representing memory stability (>= 0.001)."
  (list 'single-float +minimum-stability+))

(deftype retrievability ()
  "Single-float type representing recall probability (0.0-1.0)."
  '(single-float 0.0 1.0))

(defstruct card
  "Represents a memorization item with scheduling state and memory metrics.

CARD-ID is the unique identifier for the card.
STATE is the current learning phase (:learning/:review/:relearning).
STEP is the current position in learning/relearning steps.
STABILITY is the memory retention strength (higher = more stable).
DIFFICULTY is the item complexity (1.0-10.0 scale).
DUE is the timestamp for next review.
LAST-REVIEW is the timestamp of most recent review or NIL if new."
  (card-id 0 :type fixnum)
  (state :learning :type state)
  (step 0 :type (or fixnum null))
  (stability nil :type (or single-float null))
  (difficulty nil :type (or single-float null))
  (due (now) :type timestamp)
  (last-review nil :type (or timestamp null)))

(defstruct review-log
  "Record of individual review event.

CARD-ID is the identifier of the reviewed card.
RATING is the user's response quality (:again/:hard/:good/:easy).
REVIEW-DATETIME is the timestamp when review occurred.
REVIEW-DURATION is the time spent reviewing in seconds or NIL."
  (card-id 0 :type fixnum)
  (rating :again :type rating)
  (review-datetime (now) :type timestamp)
  (review-duration nil :type (or fixnum null)))

(define-constant +time-units+ '((:sec . 1) (:minute . 60) (:hour . 3600) (:day . 86400))
  :test #'equal
  :documentation "Time unit conversion factors in seconds.")

(deftype timespan ()
  "Cons type representing a duration with multiple time units."
  'cons)

(declaim (ftype (function (timespan) (values (or fixnum single-float))) timespan-seconds))
(defun timespan-seconds (timespan)
  "Convert TIMESPAN to total seconds.

TIMESPAN is a cons list of (AMOUNT UNIT) pairs."
  (loop :for (amount unit) :on timespan by #'cddr
        :sum (* amount (assoc-value +time-units+ unit))))

(declaim (ftype (function (timespan) (values single-float)) timespan-days))
(defun timespan-days (timespan)
  "Convert TIMESPAN to total days.

TIMESPAN is a cons list of (AMOUNT UNIT) pairs."
  (/ (timespan-seconds timespan) #.(coerce local-time::+seconds-per-day+ 'single-float)))

(declaim (ftype (function (fixnum) (values timespan)) seconds-timespan))
(defun seconds-timespan (seconds)
  "Convert SECONDS to timespan with multiple units.

SECONDS is the total duration to convert."
  (loop for (unit . amount) in (reverse +time-units+)
        for (part remainder) = (multiple-value-list (truncate (or remainder seconds) amount))
        unless (zerop part)
          nconc (list part unit)
        until (zerop remainder)))

(declaim (ftype (function (&rest t) (values timespan)) make-timespan))
(defun make-timespan (&rest args)
  "Create timespan from alternating UNIT AMOUNT pairs.

ARGS is a plist of time units and amounts."
  (loop for (unit amount) on args by #'cddr
        nconc (list amount unit)))

(declaim (ftype (function (timespan (or single-float fixnum)) (values timespan)) timespan*))
(defun timespan* (timespan factor)
  "Multiply TIMESPAN by FACTOR.

TIMESPAN is the duration to scale. FACTOR is the multiplier."
  (seconds-timespan (nth-value 0 (truncate (* (timespan-seconds timespan) factor)))))

(declaim (ftype (function (timespan &optional timestamp) (values timestamp)) timespan-apply))
(defun timespan-apply (timespan &optional (timestamp (now)))
  "Apply TIMESPAN to TIMESTAMP.

TIMESPAN is the duration to add. TIMESTAMP is the base time."
  (loop :with result := timestamp
        :for (amount unit) :on timespan :by #'cddr
        :do (setf result (timestamp+ result amount unit))
        :finally (return result)))

(defmacro define-timespan-operator (operator)
  "Define a timespan operator function that applying OPERATOR to timespans.

OPERATOR is the arithmetic function to apply to timespan values. The generated
function will convert timespans to seconds, apply OPERATOR, then convert back to
timespan format."
  (let ((args (make-symbol (symbol-name 'args))))
    (list 'defun (intern (concatenate 'string
                                      (symbol-name 'timespan)
                                      (if (> (length (symbol-name operator)) 2) "-" "")
                                      (symbol-name operator)))
          (list '&rest args)
          (concatenate 'string "Apply " (symbol-name operator) " to TIMESPANs.

ARGS is the list of timespan values to operate on. Each timespan is converted to
seconds before applying the operation, with the result converted back to
timespan format.")
          (list 'seconds-timespan (list 'apply (list 'function operator)
                                        (list 'mapcar (list 'function 'timespan-seconds) args))))))

(define-timespan-operator +)
(define-timespan-operator -)
(define-timespan-operator min)
(define-timespan-operator max)

(declaim (ftype (function ((or fixnum double-float)) (values fixnum)) seconds-days))
(defun seconds-days (secs)
  "Convert SECS to integer days by truncating fractional part.

SECS can be fixnum or double-float."
  (nth-value 0 (truncate secs #.local-time::+seconds-per-day+)))

(defstruct (scheduler (:constructor %make-scheduler))
  "Container for FSRS scheduling configuration and parameters.

PARAMETERS is the array of FSRS algorithm weights and coefficients.
DESIRED-RETENTION is the target probability of successful recall (0.0-1.0).
LEARNING-STEPS is the list of time intervals for initial learning phase.
RELEARNING-STEPS is the list of time intervals for relearning phase.
MAXIMUM-INTERVAL is the upper bound for scheduling intervals.
ENABLE-FUZZING-P is the flag to randomize intervals within bounds."
  (parameters +default-parameters+ :type parameters)
  (desired-retention 0.9 :type single-float)
  (learning-steps '((1 :minute) (10 :minute)) :type list)
  (relearning-steps '((10 :minute)) :type list)
  (maximum-interval '(36500 :day) :type timespan)
  (enable-fuzzing-p t :type boolean))

(declaim (ftype (function (scheduler) (values positive-fixnum)) scheduler-maximum-interval-days))
(defun scheduler-maximum-interval-days (scheduler)
  "Get maximum interval in days from SCHEDULER.

SCHEDULER is the FSRS scheduler instance."
  (destructuring-bind (n day) (scheduler-maximum-interval scheduler)
    (assert (eq day :day))
    n))

(declaim (ftype (function (scheduler) (values single-float single-float)) scheduler-factor-decay))
(defun scheduler-factor-decay (scheduler)
  "Calculate decay and factor from scheduler parameters.

SCHEDULER is the FSRS scheduler instance containing parameter weights.
Returns two values as single-floats: factor and decay."
  (let ((decay (- (aref (scheduler-parameters scheduler) 20))))
    (values (1- (expt 0.9 (/ decay))) decay)))

(declaim (ftype (function (parameters)) scheduler-validate-parameters))
(defun scheduler-validate-parameters (parameters)
  "Validate PARAMETERS against bounds.

PARAMETERS is the FSRS parameter array to check."
  (loop :for p :across parameters
        :for lower :across +lower-bounds-parameters+
        :for upper :across +upper-bounds-parameters+
        :do (assert (<= lower p upper))))

(declaim (ftype (function (&rest t) (values scheduler)) make-scheduler))
(defun make-scheduler (&rest args)
  "Create scheduler instance with specified configuration.

ARGS can override default parameters and settings."
  (let ((scheduler (apply #'%make-scheduler args)))
    (scheduler-validate-parameters (scheduler-parameters scheduler))
    scheduler))

(declaim (ftype (function (scheduler card &optional timestamp) (values retrievability)) scheduler-card-retrievability))
(defun scheduler-card-retrievability (scheduler card &optional (current-time (now)))
  "Calculate current recall probability for CARD.

SCHEDULER is the scheduling configuration. CARD is the item
to evaluate. CURRENT-TIME is the optional timestamp to use as now."
  (unless (card-last-review card) (return-from scheduler-card-retrievability 0.0))
  (let ((elapsed-days (max 0 (seconds-days (timestamp-difference current-time (card-last-review card))))))
    (multiple-value-bind (factor decay) (scheduler-factor-decay scheduler)
      (expt (1+ (/ (* factor elapsed-days) (card-stability card))) decay))))

(declaim (ftype (function (scheduler single-float) (values difficulty)) scheduler-clamp-difficulty))
(defun scheduler-clamp-difficulty (scheduler difficulty)
  "Clamp DIFFICULTY to valid range (1.0-10.0).

SCHEDULER is unused. DIFFICULTY is the value to clamp."
  (declare (ignore scheduler))
  (max +minimum-difficulty+ (min +maximum-difficulty+ difficulty)))

(declaim (ftype (function (scheduler single-float) (values stability)) scheduler-clamp-stability))
(defun scheduler-clamp-stability (scheduler stability)
  "Clamp STABILITY to minimum value (0.001).

SCHEDULER is unused. STABILITY is the value to clamp."
  (declare (ignore scheduler))
  (max +minimum-stability+ stability))

(declaim (ftype (function (scheduler rating) (values stability)) scheduler-initial-stability))
(defun scheduler-initial-stability (scheduler rating)
  "Compute initial stability after first review.

SCHEDULER contains the parameter weights. RATING is the user's
response."
  (let ((stability (aref (scheduler-parameters scheduler) (1- (rating-integer rating)))))
    (scheduler-clamp-stability scheduler stability)))

(declaim (ftype (function (scheduler rating) (values difficulty)) scheduler-initial-difficulty))
(defun scheduler-initial-difficulty (scheduler rating)
  "Compute initial difficulty after first review for RATING.

SCHEDULER contains the parameter weights. RATING is the user's
response quality."
  (let ((difficulty (1+ (- (aref (scheduler-parameters scheduler) 4)
                           (exp (* (aref (scheduler-parameters scheduler) 5)
                                   (1- (rating-integer rating))))))))
    (scheduler-clamp-difficulty scheduler difficulty)))

(declaim (ftype (function (scheduler stability) (values fixnum)) scheduler-next-interval))
(defun scheduler-next-interval (scheduler stability)
  "Calculate next review interval in days for given STABILITY.

SCHEDULER contains scheduling parameters. STABILITY is the memory
strength."
  (multiple-value-bind (factor decay) (scheduler-factor-decay scheduler)
    (let ((interval (* (/ stability factor) (1- (expt (scheduler-desired-retention scheduler) (/ decay))))))
      (min (max (nth-value 0 (round interval)) 1) (scheduler-maximum-interval-days scheduler)))))

(declaim (ftype (function (scheduler stability rating) (values stability)) scheduler-short-term-stability))
(defun scheduler-short-term-stability (scheduler stability rating)
  "Calculate short-term stability adjustment after reviewing with RATING.

SCHEDULER contains model parameters. STABILITY is the current memory
strength. RATING is the user's response quality (:again/:hard/:good/:easy)."
  (let* ((increase (* (exp (* (aref (scheduler-parameters scheduler) 17)
                              (+ (- (rating-integer rating) 3)
                                 (aref (scheduler-parameters scheduler) 18))))
                      (expt stability (- (aref (scheduler-parameters scheduler) 19)))))
         (new-stability (* stability (if (member rating '(:good :easy)) (max increase 1.0) increase))))
    (scheduler-clamp-stability scheduler new-stability)))

(declaim (ftype (function (scheduler difficulty rating) (values difficulty)) scheduler-next-difficulty))
(defun scheduler-next-difficulty (scheduler difficulty rating)
  "Calculate next difficulty level after reviewing with RATING.

SCHEDULER contains model parameters. DIFFICULTY is the current item
complexity. RATING is the user's response quality
 (:again/:hard/:good/:easy)."
  (let* ((linear-damping (* (/ (- 10.0 difficulty) 9.0)
                            (- (* (aref (scheduler-parameters scheduler) 6)
                                  (- (rating-integer rating) 3)))))
         (mean-reversion (+ (* (aref (scheduler-parameters scheduler) 7)
                               (scheduler-initial-difficulty scheduler :easy))
                            (* (- 1 (aref (scheduler-parameters scheduler) 7))
                               (+ difficulty linear-damping)))))
    (scheduler-clamp-difficulty scheduler mean-reversion)))

(declaim (ftype (function (scheduler difficulty stability retrievability) (values stability)) scheduler-next-forget-stability))
(defun scheduler-next-forget-stability (scheduler difficulty stability retrievability)
  "Calculate stability after forgetting during review.

SCHEDULER contains model parameters. DIFFICULTY is the item
complexity. STABILITY is the current memory strength.
RETRIEVABILITY is the recall probability."
  (let ((long-term (* (aref (scheduler-parameters scheduler) 11)
                      (expt difficulty (- (aref (scheduler-parameters scheduler) 12)))
                      (1- (expt (1+ stability) (aref (scheduler-parameters scheduler) 13)))
                      (exp (* (- 1 retrievability) (aref (scheduler-parameters scheduler) 14)))))
        (short-term (/ stability (exp (* (aref (scheduler-parameters scheduler) 17)
                                         (aref (scheduler-parameters scheduler) 18))))))
    (min long-term short-term)))

(declaim (ftype (function (scheduler difficulty stability retrievability rating) (values stability)) scheduler-next-recall-stability))
(defun scheduler-next-recall-stability (scheduler difficulty stability retrievability rating)
  "Calculate stability after successful recall with RATING.

SCHEDULER contains model parameters. DIFFICULTY is the item
complexity. STABILITY is the current memory strength.
RETRIEVABILITY is the recall probability. RATING is the user's
response quality (:again/:hard/:good/:easy)."
  (let* ((hard-penalty (if (eq rating :hard) (aref (scheduler-parameters scheduler) 15) 1.0))
         (easy-bonus (if (eq rating :easy) (aref (scheduler-parameters scheduler) 16) 1.0))
         (new-stability (* stability
                           (1+ (* (exp (aref (scheduler-parameters scheduler) 8))
                                  (- 11.0 difficulty)
                                  (expt stability (- (aref (scheduler-parameters scheduler) 9)))
                                  (1- (exp (* (- 1.0 retrievability) (aref (scheduler-parameters scheduler) 10))))
                                  hard-penalty easy-bonus)))))
    (scheduler-clamp-stability scheduler new-stability)))

(declaim (ftype (function (scheduler timespan) (values timespan)) scheduler-fuzzed-interval))
(defun scheduler-fuzzed-interval (scheduler interval)
  "Apply random fuzzing to INTERVAL based on fuzz ranges.

SCHEDULER contains fuzzing configuration. INTERVAL is the base timespan."
  (let ((days (timespan-days interval)))
    (cond
      ((< days 2.5) interval)
      (t (let* ((delta (loop :for (start end factor) :in +fuzz-ranges+
                             :sum (* factor (max 0 (min (or (when end (timespan-days end)) days) days) (timespan-days start)))))
                (min-ivl (max 2 (min (nth-value 0 (round (- days delta))) (scheduler-maximum-interval-days scheduler))))
                (max-ivl (min (nth-value 0 (round (+ days delta))) (scheduler-maximum-interval-days scheduler)))
                (fuzzed-days (min (nth-value 0 (round (+ min-ivl (random (- max-ivl min-ivl -1)))))
                                  (scheduler-maximum-interval-days scheduler))))
           (make-timespan :day fuzzed-days))))))

(declaim (ftype (function (scheduler card rating &optional timestamp (or null fixnum)) (values card review-log)) scheduler-review-card))
(defun scheduler-review-card (scheduler card rating &optional (review-time (now)) review-duration)
  "Process CARD review with RATING and update scheduling state.

SCHEDULER contains configuration parameters. CARD is the item being
reviewed. RATING is the user's response quality (:again/:hard/:good/:easy).
REVIEW-TIME is the optional timestamp of review. REVIEW-DURATION is the optional
duration."
  (let* ((card (copy-card card))
         (days-since-last (when (card-last-review card) (seconds-days (timestamp-difference review-time (card-last-review card)))))
         (scheduler-next-interval
           (ecase (card-state card)
             (:learning
              (cond
                ((and (null (card-stability card)) (null (card-difficulty card)))
                 (setf (card-stability card) (scheduler-initial-stability scheduler rating)
                       (card-difficulty card) (scheduler-initial-difficulty scheduler rating)))
                ((and days-since-last (< days-since-last 1))
                 (setf (card-stability card) (scheduler-short-term-stability scheduler (card-stability card) rating)
                       (card-difficulty card) (scheduler-next-difficulty scheduler (card-difficulty card) rating)))
                (t (setf (card-stability card) (scheduler-next-recall-stability scheduler (card-difficulty card) (card-stability card)
                                                                                (scheduler-card-retrievability scheduler card review-time)
                                                                                rating)
                         (card-difficulty card) (scheduler-next-difficulty scheduler (card-difficulty card) rating))))
              (cond
                ((or (null (scheduler-learning-steps scheduler))
                     (and (>= (card-step card) (length (scheduler-learning-steps scheduler)))
                          (member rating '(:hard :good :easy))))
                 (let ((days (scheduler-next-interval scheduler (card-stability card))))
                   (setf (card-state card) :review
                         (card-step card) nil)
                   (make-timespan :day days)))
                (t (ecase rating
                     (:again
                      (nth (setf (card-step card) 0) (scheduler-learning-steps scheduler)))
                     (:hard
                      (cond
                        ((and (= (card-step card) 0) (= (length (scheduler-learning-steps scheduler)) 1))
                         (timespan* (nth 0 (scheduler-learning-steps scheduler)) 1.5))
                        ((and (= (card-step card) 0) (>= (length (scheduler-learning-steps scheduler)) 2))
                         (timespan* (timespan+ (nth 0 (scheduler-learning-steps scheduler))
                                               (nth 1 (scheduler-learning-steps scheduler)))
                                    (/ 2.0)))
                        (t (nth (card-step card) (scheduler-learning-steps scheduler)))))
                     (:good
                      (if (= (1+ (card-step card)) (length (scheduler-learning-steps scheduler)))
                          (let ((days (scheduler-next-interval scheduler (card-stability card))))
                            (setf (card-state card) :review
                                  (card-step card) nil)
                            (make-timespan :day days))
                          (nth (incf (card-step card)) (scheduler-learning-steps scheduler))))
                     (:easy
                      (let ((days (scheduler-next-interval scheduler (card-stability card))))
                        (setf (card-state card) :review
                              (card-step card) nil)
                        (make-timespan :day days)))))))
             (:review
              (if (and days-since-last (< days-since-last 1))
                  (setf (card-stability card) (scheduler-short-term-stability scheduler (card-stability card) rating)
                        (card-difficulty card) (scheduler-next-difficulty scheduler (card-difficulty card) rating))
                  (setf (card-stability card) (if (eq rating :again)
                                                  (scheduler-next-forget-stability scheduler (card-difficulty card) (card-stability card)
                                                                                   (scheduler-card-retrievability scheduler card review-time))
                                                  (scheduler-next-recall-stability scheduler (card-difficulty card) (card-stability card)
                                                                                   (scheduler-card-retrievability scheduler card review-time)
                                                                                   rating))
                        (card-difficulty card) (scheduler-next-difficulty scheduler (card-difficulty card) rating)))
              (ecase rating
                (:again
                 (if (null (scheduler-relearning-steps scheduler))
                     (let ((days (scheduler-next-interval scheduler (card-stability card))))
                       (make-timespan :day days))
                     (nth (setf (card-state card) :relearning (card-step card) 0)
                          (scheduler-relearning-steps scheduler))))

                ((:hard :good :easy)
                 (let ((days (scheduler-next-interval scheduler (card-stability card))))
                   (make-timespan :day days)))))
             (:relearning
              (if (and days-since-last (< days-since-last 1))
                  (setf (card-stability card) (scheduler-short-term-stability scheduler (card-stability card) rating)
                        (card-difficulty card) (scheduler-next-difficulty scheduler (card-difficulty card) rating))
                  (setf (card-stability card) (scheduler-next-recall-stability scheduler (card-difficulty card) (card-stability card)
                                                                               (scheduler-card-retrievability scheduler card review-time)
                                                                               rating)
                        (card-difficulty card) (scheduler-next-difficulty scheduler (card-difficulty card) rating)))
              (cond
                ((or (null (scheduler-relearning-steps scheduler))
                     (and (>= (card-step card) (length (scheduler-relearning-steps scheduler)))
                          (member rating '(:hard :good :easy))))
                 (let ((days (scheduler-next-interval scheduler (card-stability card))))
                   (setf (card-state card) :review
                         (card-step card) nil)
                   (make-timespan :day days)))
                (t
                 (ecase rating
                   (:again
                    (nth (setf (card-step card) 0) (scheduler-relearning-steps scheduler)))
                   (:hard
                    (cond
                      ((and (= (card-step card) 0) (= (length (scheduler-relearning-steps scheduler)) 1))
                       (timespan* (nth 0 (scheduler-relearning-steps scheduler)) 1.5))
                      ((and (= (card-step card) 0) (>= (length (scheduler-relearning-steps scheduler)) 2))
                       (timespan* (timespan+ (nth 0 (scheduler-relearning-steps scheduler))
                                             (nth 1 (scheduler-relearning-steps scheduler)))
                                  (/ 2.0)))
                      (t (nth (card-step card) (scheduler-relearning-steps scheduler)))))

                   (:good
                    (if (= (1+ (card-step card)) (length (scheduler-relearning-steps scheduler)))
                        (let ((days (scheduler-next-interval scheduler (card-stability card))))
                          (setf (card-state card) :review
                                (card-step card) nil)
                          (make-timespan :day days))
                        (nth (incf (card-step card)) (scheduler-relearning-steps scheduler))))
                   (:easy
                    (let ((days (scheduler-next-interval scheduler (card-stability card))))
                      (setf (card-state card) :review
                            (card-step card) nil)
                      (make-timespan :day days))))))))))
    (when (and (scheduler-enable-fuzzing-p scheduler) (eq (card-state card) :review))
      (setf scheduler-next-interval (scheduler-fuzzed-interval scheduler scheduler-next-interval)))
    (setf (card-due card) (timespan-apply scheduler-next-interval review-time)
          (card-last-review card) review-time)
    (values card (make-review-log :card-id (card-card-id card)
                                  :rating rating
                                  :review-datetime review-time
                                  :review-duration review-duration))))
