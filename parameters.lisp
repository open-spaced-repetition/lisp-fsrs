(in-package #:lisp-fsrs)

(deftype weights ()
  "Array type containing 19 single-floats for FSRS parameters."
  '(simple-array single-float (19)))

(define-constant +weights-default+
    (coerce
     '(0.4072 1.1829 3.1262 15.4722 7.2102 0.5316 1.0651 0.0234 1.616 0.1544 1.0824 1.9813 0.0953 0.2975 2.2042 0.2407 2.9466 0.5034 0.6567)
     'weights)
  :test #'equalp
  :documentation "Default weight values for FSRS parameters.")

(declaim (type single-float +decay+ +factor+))
(defconstant +decay+ -0.5)
(defconstant +factor+ (1- (expt 0.9 (/ +decay+))))

(defstruct parameters
  "Container for FSRS algorithm configuration parameters.

REQUEST-RETENTION specifies target retention probability (0.0-1.0).
MAXIMUM-INTERVAL sets upper bound (in days) for scheduling intervals.
WEIGHTS stores algorithm coefficients.
DECAY contains stability decay exponent for forgetting curve.
FACTOR is precomputed constant derived from DECAY for interval scaling."
  (request-retention 0.9 :type non-negative-single-float)
  (maximum-interval 36500 :type non-negative-fixnum)
  (weights +weights-default+ :type weights)
  (decay +decay+ :type single-float)
  (factor +factor+ :type single-float))

(declaim (ftype (function (parameters non-negative-fixnum non-negative-single-float) (values (single-float 0.0 1.0))) parameters-forgetting-curve))
(defun parameters-forgetting-curve (self elapsed-days stability)
  "Calculate retention probability given elapsed days and stability.

SELF is PARAMETERS instance. ELAPSED-DAYS is time since last review.
STABILITY is memory stability value.

Returns single-float between 0.0 and 1.0 representing recall probability."
  (expt (1+ (/ (* (parameters-factor self) elapsed-days) stability)) (parameters-decay self)))

(declaim (ftype (function (parameters rating) (values non-negative-single-float)) parameters-init-stability))
(defun parameters-init-stability (self rating)
  "Compute initial stability after first review.

SELF is PARAMETERS instance. RATING is user's response
quality (:again/:hard/:good/:easy).

Returns non-negative single-float stability value."
  (let ((w (parameters-weights self)) (r (rating-integer rating)))
    (max (aref w (1- r)) 0.1)))

(declaim (ftype (function (parameters rating) (values non-negative-single-float)) parameters-init-difficulty))
(defun parameters-init-difficulty (self rating)
  "Determine initial difficulty for new material.

SELF is PARAMETERS instance. RATING is first response rating.

Returns single-float between 1.0 and 10.0 representing item
difficulty."
  (let ((w (parameters-weights self)) (r (rating-integer rating)))
    (min (max (1+ (- (aref w 4) (exp (* (aref w 5) (1- r))))) 1.0) 10.0)))

(defconstant most-negative-fixnum-float (coerce most-negative-fixnum 'single-float))
(defconstant most-positive-fixnum-float (coerce most-positive-fixnum 'single-float))
(deftype fixnum-float () (list 'single-float most-negative-fixnum-float most-positive-fixnum-float))

(declaim (ftype (function (parameters non-negative-single-float) (values non-negative-fixnum)) parameters-next-interval))
(defun parameters-next-interval (self s)
  "Calculate next review interval in days based on stability.

SELF is PARAMETERS instance. S is current stability value.

Returns non-negative fixnum days until next review."
  (let ((new-interval (* (/ s (parameters-factor self)) (1- (expt (parameters-request-retention self) (/ (parameters-decay self)))))))
    (declare (type fixnum-float new-interval))
    (min (max (nth-value 0 (round new-interval)) 1) (parameters-maximum-interval self))))

(declaim (ftype (function (parameters non-negative-single-float single-float) (values single-float)) parameters-mean-reversion))
(defun parameters-mean-reversion (self init current)
  "Apply mean reversion to difficulty estimates.

SELF is PARAMETERS instance. INIT is initial difficulty value. CURRENT
is raw difficulty estimate.

Returns adjusted single-float difficulty value."
  (let ((w (parameters-weights self)))
    (+ (* (aref w 7) init) (* (- 1.0 (aref w 7)) current))))

(declaim (ftype (function (parameters non-negative-single-float rating) (values non-negative-single-float)) parameters-next-difficulty))
(defun parameters-next-difficulty (self d rating)
  "Update difficulty after user rating.

SELF is PARAMETERS instance. D is previous difficulty value.
RATING is user's response rating.

Returns adjusted single-float between 1.0 and 10.0."
  (let ((w (parameters-weights self)) (r (rating-integer rating)))
    (let ((next-d (- d (* (aref w 6) (- r 3)))))
      (min (max (parameters-mean-reversion self (parameters-init-difficulty self :easy) next-d) 1.0) 10.0))))

(declaim (ftype (function (parameters non-negative-single-float rating) (values non-negative-single-float)) parameters-short-term-stability))
(defun parameters-short-term-stability (self stability rating)
  "Adjust short-term stability for cards in learning/relearning state.

SELF is a PARAMETERS instance containing weight coefficients.
STABILITY is a non-negative single float representing current memory
stability. RATING indicates user's recall correctness.

Returns adjusted stability as non-negative single float."
  (let ((w (parameters-weights self)) (r (rating-integer rating)))
    (* stability (exp (* (aref w 17) (+ (- r 3) (aref w 18)))))))

(declaim (ftype (function (parameters non-negative-single-float non-negative-single-float (single-float 0.0 1.0) rating) (values non-negative-single-float)) parameters-next-recall-stability))
(defun parameters-next-recall-stability (self d s r rating)
  "Calculate new stability after successful recall.

SELF is a PARAMETERS instance containing weight coefficients. D is a
non-negative single float representing days since last review. S is a
non-negative single float representing current memory stability. R is
a single float between 0.0-1.0 representing memory retrievability.
RATING indicates user's answer difficulty.

Returns new stability as non-negative single float."
  (let ((w (parameters-weights self)))
    (let ((hard-penalty (if (eq rating :hard) (aref w 15) 1.0))
          (easy-bonus (if (eq rating :easy) (aref w 16) 1.0)))
      (* s (1+ (* (exp (aref w 8))
                  (- 11.0 d)
                  (expt s (- (aref w 9)))
                  (1- (exp (* (- 1.0 r) (aref w 10))))
                  hard-penalty
                  easy-bonus))))))

(declaim (ftype (function (parameters non-negative-single-float non-negative-single-float (single-float 0.0 1.0)) (values non-negative-single-float)) parameters-next-forget-stability))
(defun parameters-next-forget-stability (self d s r)
  "Compute new stability after forgetting a card.

SELF is a PARAMETERS instance containing weight coefficients. D is a
non-negative single float representing days elapsed since last review.
S is a non-negative single float representing previous memory
stability. R is a single float between 0.0-1.0 representing memory
retrievability.

Returns recalculated stability as non-negative single float."
  (let ((w (parameters-weights self)))
    (* (aref w 11)
       (expt d (- (aref w 12)))
       (1- (expt (1+ s) (aref w 13)))
       (exp (* (- 1 r) (aref w 14))))))
