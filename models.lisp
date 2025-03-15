(in-package #:lisp-fsrs)

(deftype state ()
  "Learning phase progression state.
Possible values: :new, :learning, :review, or :relearning."
  '(member :new :learning :review :relearning))

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
  "User response rating for memory recall success.
Possible values: :again (failed), :hard, :good, or :easy."
  '(member :again :hard :good :easy))

(declaim (ftype (function (rating) (integer 1 4)) rating-integer)
         (inline rating-integer))
(defun rating-integer (rating)
  "Convert RATING to an integer (1-4)."
  (ecase rating
    (:again 1)
    (:hard 2)
    (:good 3)
    (:easy 4)))

(defstruct review-log
  "Record of individual review event.

RATING is user's response.
SCHEDULED-DAYS was planned interval.
ELAPSED-DAYS was actual days since last review.
REVIEW is timestamp of event.
STATE was card state before review."
  (rating :again :type rating)
  (scheduled-days 0 :type non-negative-fixnum)
  (elapsed-days 0 :type non-negative-fixnum)
  (review (now) :type timestamp)
  (state :new :type state))

(defstruct card
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
  (due (now) :type timestamp)
  (stability 0.0 :type non-negative-single-float)
  (difficulty 0.0 :type non-negative-single-float)
  (elapsed-days 0 :type non-negative-fixnum)
  (scheduled-days 0 :type non-negative-fixnum)
  (repeats 0 :type non-negative-fixnum)
  (lapses 0 :type non-negative-fixnum)
  (state :new :type state)
  (last-review nil :type (or timestamp null)))

(declaim (ftype (function ((or fixnum double-float)) (values fixnum)) seconds-days))
(defun seconds-days (secs)
  "Convert SECONDS to integer days by truncating fractional part.
SECS can be fixnum or double-float.

Returns non-negative fixnum."
  (nth-value 0 (truncate secs #.local-time::+seconds-per-day+)))

(declaim (type single-float +decay+ +factor+))
(defconstant +decay+ -0.5 "Decay exponent for forgetting curve calculation.")
(defconstant +factor+ (1- (expt 0.9 (/ +decay+))) "Precomputed scaling factor for interval calculations.")

(declaim (ftype (function (card &optional timestamp) (values (single-float 0.0 1.0))) card-retrievability))
(defun card-retrievability (self &optional (now (now)))
  "Calculate current recall probability for CARD.

SELF is CARD instance. NOW is optional current timestamp.

Returns single-float between 0.0 (forgotten) and 1.0 (fresh)."
  (if (member (card-state self) '(:learning :review :relearning))
      (let ((elapsed-days (max 0 (seconds-days (timestamp-difference now (card-last-review self))))))
        (expt (1+ (/ (* +factor+ elapsed-days) (card-stability self))) +decay+))
      0.0))

(defstruct scheduling-info
  "Container for card scheduling options and review metadata.

CARD holds modified card state for specific rating choice.
REVIEW-LOG records hypothetical review log entry for that choice."
  (card (make-card) :type card)
  (review-log (make-review-log) :type review-log))

(defstruct (scheduling-cards (:constructor %make-scheduling-cards))
  "Holds four possible scheduling paths based on user rating.

AGAIN path for failed recall.
HARD path for difficult recall.
GOOD path for successful recall.
EASY path for effortless recall."
  (again (make-card) :type card)
  (hard (make-card) :type card)
  (good (make-card) :type card)
  (easy (make-card) :type card))

(declaim (ftype (function (&key (:card card) (:again card) (:hard card) (:good card) (:easy card)) (values scheduling-cards)) make-scheduling-cards))
(defun make-scheduling-cards (&key (card (make-card)) (again (copy-card card)) (hard (copy-card card)) (good (copy-card card)) (easy (copy-card card)))
  "Create SCHEDULING-CARDS with cloned CARD variants.
Each keyword argument lets override default card copies for
ratings (EASY GOOD HARD AGAIN)."
  (%make-scheduling-cards :again again :hard hard :good good :easy easy))

(declaim (ftype (function (scheduling-cards card timestamp) (values list)) scheduling-cards-record-log))
(defun scheduling-cards-record-log (self card now)
  "Generate plist of SCHEDULING-INFO entries for all rating paths.

SELF is SCHEDULING-CARDS instance. CARD is original card being
scheduled. NOW is current timestamp for log entries.

Returns (:again INFO :hard INFO :good INFO :easy INFO) plist."
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
