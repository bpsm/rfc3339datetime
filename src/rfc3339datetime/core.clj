(ns rfc3339datetime.core
  (:import [java.util Calendar Date GregorianCalendar TimeZone]
           [java.sql Timestamp]))

(declare parse-timestamp validated)

;;;
;;; parse method
;;;

(defn parse
  "Parse a string containing an RFC3339-like like timestamp.

The function constructor is called with the following arguments.

                min  max           default
                ---  ------------  -------
  years          0          9'999      N/A (s must provide years)
  months         1             12        1
  days           1             31        1 (actual max days depends
  hours          0             23        0  on month and year)
  minutes        0             59        0
  seconds        0             60        0 (though 60 is only valid             
  nanoseconds    0    999'999'999        0  when minutes is 59)
  offset-sign   -1              1        0
  offset-hours   0             23        0
  offset-minutes 0             59        0

These are all integers and will be non-nil. (The listed defaults
will be passed if the corresponding field is not present in s.)

Grammar (of s):

  date-fullyear   = 4DIGIT
  date-month      = 2DIGIT  ; 01-12
  date-mday       = 2DIGIT  ; 01-28, 01-29, 01-30, 01-31 based on
                            ; month/year
  time-hour       = 2DIGIT  ; 00-23
  time-minute     = 2DIGIT  ; 00-59
  time-second     = 2DIGIT  ; 00-58, 00-59, 00-60 based on leap second
                            ; rules
  time-secfrac    = '.' 1*DIGIT
  time-numoffset  = ('+' / '-') time-hour ':' time-minute
  time-offset     = 'Z' / time-numoffset

  time-part       = time-hour [ ':' time-minute [ ':' time-second 
                    [time-secfrac] [time-offset] ] ]

  timestamp       = date-year [ '-' date-month [ '-' date-mday 
                    [ 'T' time-part ] ] ]

Unlike RFC3339:

  - we only consdier timestamp (was 'date-time')
    (removed: 'full-time', 'full-date')
  - timestamp can elide trailing components
  - time-offset is optional

Though time-offset is syntactically optional, a missing time-offset
will be treated as if the time-offset zero (+00:00) had been
specified.
"
  [constructor s]
  (parse-timestamp (validated constructor) s))

;;;
;;; constructor functions
;;;

(defn construct-string
  "Construct an RFC3339 compatible string"
  [years months days hours minutes seconds nanoseconds
   offset-sign offset-hours offset-minutes]
  (format "%04d-%02d-%02dT%02d:%02d:%02d.%09d%s%02d:%02d"
          years months days hours minutes seconds
          nanoseconds (if (neg? offset-sign) "-" "+")
          offset-hours offset-minutes))

(defn construct-calendar
  "Construct a java.util.Calendar, which preserves, preserving the timezone
offset, but truncating the subsecond fraction to milliseconds."
  ^GregorianCalendar
  [years months days hours minutes seconds nanoseconds
   offset-sign offset-hours offset-minutes]
  (doto (GregorianCalendar. years (dec months) days hours minutes seconds)
    (.set Calendar/MILLISECOND (/ nanoseconds 1000000))
    (.setTimeZone (TimeZone/getTimeZone
                   (format "GMT%s%02d:%02d"
                           (if (neg? offset-sign) "-" "+")
                           offset-hours offset-minutes)))))

(defn construct-date
  "Construct a java.util.Date, which expresses the original instant as
milliseconds since the epoch, GMT."
  [years months days hours minutes seconds nanoseconds
   offset-sign offset-hours offset-minutes]
  (.getTime (construct-calendar years months days
                                hours minutes seconds nanoseconds
                                offset-sign offset-hours offset-minutes)))

(defn construct-timestamp
  "Construct a java.sql.Timestamp, which has nanosecond precision."
  [years months days hours minutes seconds nanoseconds
   offset-sign offset-hours offset-minutes]
  (doto (Timestamp.
         (.getTimeInMillis
          (construct-calendar years months days
                              hours minutes seconds nanoseconds
                              offset-sign offset-hours offset-minutes)))
    (.setNanos nanoseconds)))


;;;
;;; convert various parse results back to rfc3339 datetime strings
;;;

(defn substring
  ([^String s ^long begin-index ^long end-index]
     (.substring s begin-index end-index))
  ([^String s ^long begin-index]
     (.substring s begin-index)))

(defn fixup-offset [s]
  (let [x (- (count s) 2)]
    (str (substring s 0 x) ":" (substring s x))))

(def calendar->rfc3339
  (comp fixup-offset
        (partial format "%1$tFT%1$tT.%1$tL%1$tz")))

(def date->rfc3339 calendar->rfc3339)

(def timestamp->rfc3339
  (comp fixup-offset
        (partial format "%1$tFT%1$tT.%1$tN%1$tz")))

;;;
;;; parser implementation
;;;

(defmacro assoc-if [map test key val]
  `(let [map# ~map]
     (if ~test (assoc map# ~key ~val) map#)))

(defn parse-int [^String s]
  (Long/parseLong s))

(defmacro fail [msg]
  `(throw (RuntimeException. ~msg)))

(defn zero-fill-right [^String s width]
  (cond (= width (count s)) s
        (< width (count s)) (substring s 0 width)
        :else (loop [b (StringBuilder. s)]
                (if (< (.length b) width)
                  (recur (.append b \0))
                  (.toString b)))))

(def parse-timestamp
  (let [re #"(\d\d\d\d)(?:-(\d\d)(?:-(\d\d)(?:[T](\d\d)(?::(\d\d)(?::(\d\d)(?:[.](\d+))?)?)?)?)?)?(?:[Z]|([-+])(\d\d):(\d\d))?"]
    (fn [constructor ^CharSequence cs]
      (if-let [[_ years months days hours minutes seconds fraction
                offset-sign offset-hours offset-minutes]
               (re-matches re cs)]
        (constructor
         (parse-int years)
         (if-not months   1 (parse-int months))
         (if-not days     1 (parse-int days))
         (if-not hours    0 (parse-int hours))
         (if-not minutes  0 (parse-int minutes))
         (if-not seconds  0 (parse-int seconds))
         (if-not fraction 0 (parse-int (zero-fill-right fraction 9)))
         (cond (= "-" offset-sign) -1
               (= "+" offset-sign)  1
               :else                0)
         (if-not offset-hours   0 (parse-int offset-hours))
         (if-not offset-minutes 0 (parse-int offset-minutes)))
        (fail (str "Unrecognized date/time syntax: " cs))))))

;;;
;;; Verification of Extra-Grammatical Restrictions from RFC3339
;;;

(defmacro divisible? [num div] `(zero? (mod ~num ~div)))
(defmacro indivisible? [num div] `(not (divisible? ~num ~div)))

(defn leap-year? [year]
  (and (divisible? year 4)
       (or (indivisible? year 100)
           (divisible? year 400))))

(def days-in-month
  (let [dim [nil 31 28 31 30 31 30 31 31 30 31 30 31]]
    (fn [month leap-year?]
      (+ (dim month)
         (if (and (= month 2) leap-year?) 1 0)))))

(defmacro verify
  ([test msg] `(when-not ~test (fail ~msg)))
  ([test] `(verify ~test ~(str "failed: " (pr-str test)))))

(defn validated
  "Middleware to which validates inputs before passing them on to
constructor."
  [constructor]
  (fn [years months days hours minutes seconds nanoseconds
       offset-sign offset-hours offset-minutes]
    (verify (<= 1 months 12))
    (verify (<= 1 days (days-in-month months (leap-year? years))))
    (verify (<= 0 hours 23))
    (verify (<= 0 minutes 59))
    (verify (<= 0 seconds (if (= minutes 59) 60 59)))
    (verify (<= 0 nanoseconds 999999999))
    (verify (<= -1 offset-sign 1))
    (verify (<= -23 offset-hours 23))
    (verify (<= -59 offset-minutes 59))
    (constructor years months days hours minutes seconds nanoseconds
                 offset-sign offset-hours offset-minutes)))


