(ns rfc3339datetime.core)

;;; Parsing of time, date and date-time "inspired by" RFC3339
;;; Unlike RFC3339:
;;;
;;; - date, time and date-time may be missing least significant
;;;   fields (truncated)
;;; - time offset is optional
;;; - time offset may appear in date (which has no time component)
;;;
;;; please see the docstring of parse for details.

(defmacro fail [msg]
  `(throw (RuntimeException. ~msg)))

(defmacro assoc-if [map test key val]
  `(let [map# ~map]
     (if ~test (assoc map# ~key ~val) map#)))

(defn str->int [^String s]
  (Long/parseLong s))

(defn str->double [^String s]
  (Double/parseDouble s))

(def parse-time-offset
  (let [re #"([Z])|([-+])(\d\d):(\d\d)"]
    (fn [^CharSequence cs]
      (if-let [[_ z sign hours minutes] (re-matches re cs)]
        (cond z            {:hours 0
                            :minutes 0}
              (= sign "+") {:hours (str->int hours)
                            :minutes (str->int minutes)}
              :else        {:hours (- (str->int hours))
                            :minutes (- (str->int minutes))})
        (fail (str "Unable to parse time-offset '" cs "'"))))))

(def parse-date
  (let [re #"(\d\d\d\d)(?:-(\d\d)(?:-(\d\d))?)?([Z]|[-+]\d\d:\d\d)?"]
    (fn [^CharSequence cs]
      (when-let [[_ years months days offset] (re-matches re cs)]
        (-> {:years (str->int years)}
            (assoc-if months :months (str->int months))
            (assoc-if days :days (str->int days))
            (assoc :offset (if offset
                             (parse-time-offset offset)
                             {:hours 0 :minutes 0})))))))

(def parse-time
  (let [re #"(\d\d)(?::(\d\d)(?::(\d\d)(?:[.](\d+))?)?)?([Z]|[-+]\d\d:\d\d)?"]
    (fn [^CharSequence cs]
      (when-let [[_ hours minutes seconds fraction offset] (re-matches re cs)]
        (-> {:hours (str->int hours)}
            (assoc-if minutes :minutes (str->int minutes))
            (assoc-if seconds :seconds (str->int seconds))
            (assoc-if fraction :fraction (str->double (str "0." fraction)))
            (assoc :offset (if offset
                             (parse-time-offset offset)
                             {:hours 0 :minutes 0})))))))

(def parse-date-time
  (let [re #"(\d\d\d\d)-(\d\d)-(\d\d)[T](\d\d)(?::(\d\d)(?::(\d\d)(?:[.](\d+))?)?)?([Z]|[-+]\d\d:\d\d)?"]
    (fn [^CharSequence cs]
      (when-let [[_ years months days hours minutes seconds fraction offset]
                 (re-matches re cs)]
        (-> {:years (str->int years)
             :months (str->int months)
             :days (str->int days)
             :hours (str->int hours)}
            (assoc-if minutes :minutes (str->int minutes))
            (assoc-if seconds :seconds (str->int seconds))
            (assoc-if fraction :fraction (str->double (str "0." fraction)))
            (assoc :offset (if offset
                             (parse-time-offset offset)
                             {:hours 0 :minutes 0})))))))

;;; Verification of Extra-Grammatical Restrictions from RFC3339

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
  ([pre test msg] `(when ~pre (when-not ~test (fail ~msg))))
  ([pre test] `(verify ~pre ~test ~(str "failed: " (pr-str test)))))

(defn verify-parsed
  [{:keys [years months days hours minutes seconds fraction offset]
    :as parsed}]
  (verify true (or years hours))
  (verify years (->> (if years
                       [fraction seconds minutes hours days months years]
                       [fraction seconds minutes hours])
                     (drop-while nil?)
                     (not-any? nil?))
          "Only trailing fields may be omitted.")
  (verify months (<= 1 months 12))
  (verify days (<= 1 days (days-in-month months (leap-year? years))))
  (verify hours (<= 0 hours 23))
  (verify minutes (<= 0 minutes 59))
  (verify seconds (<= 0 seconds (if (= minutes 59) 60 59)))
  (when-let [{offset-hours :hours offset-minutes :minutes} offset]
    (verify true (<= -23 offset-hours 23))
    (verify true (<= -59 offset-minutes 59)))
  parsed)

;;; parse method

(defn parse
  "Parse a string containing an RFC3339-like like time, date or date-time.

A map is returned containing at most the following fields:

  :years    an integer between 0 and 9999
  :months   an integer between 0 and 12
  :days     an integer between 0 and 31
            (actual maximum depends on current month and year)
  :hours    an integer between 0 and 23
  :minutes  an integer between 0 and 59
  :seconds  an integer between 0 and 60,
            (though 60 is only possible when minutes is 59)
  :fraction an non-negative double less than 1
  :offset   the time zone offset, which is itself a map:
   :hours   an integer between -23 and 23
   :minutes an integer between -59 and 59
   (hours and minutes always have the same sign.)

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

  partial-time    = time-hour [ ':' time-minute [ ':' time-second 
                    [time-secfrac] ] ]
  full-time       = partial-time [ time-offset ]

  partial-date    = date-fullyear [ '-' date-month [ '-' date-mday ] ]
  full-date       = partial-date [ time-offset ]

  date-time       = date-fullyear '-' date-month '-' date-mday 'T' full-time

Unlike RFC3339:

  - date, time and date-time may be missing least significant
    fields (truncated)
  - time offset is optional
  - time offset may appear in date (which has no time component)
"
  [s]
  (if-let [parsed (or (parse-date-time s)
                      (parse-date s)
                      (parse-time s))]
    (verify-parsed parsed)
    (fail (str "Unrecognized date/time syntax: " s))))

