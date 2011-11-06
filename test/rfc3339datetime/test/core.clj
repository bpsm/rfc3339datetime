(ns rfc3339datetime.test.core
  (:use [rfc3339datetime.core])
  (:use [clojure.test]))

(deftest good-cases
  (testing "eliding trailing fields, omitting time zone offset"
    (are [p s] (= p (parse construct-string s))
         "2001-01-01T00:00:00.000000000+00:00" "2001"
         "2001-02-01T00:00:00.000000000+00:00" "2001-02"
         "2001-02-03T00:00:00.000000000+00:00" "2001-02-03"
         "2001-02-03T04:00:00.000000000+00:00" "2001-02-03T04"
         "2001-02-03T04:05:00.000000000+00:00" "2001-02-03T04:05"
         "2001-02-03T04:05:06.000000000+00:00" "2001-02-03T04:05:06"
         "2001-02-03T04:05:06.987654321+00:00" "2001-02-03T04:05:06.987654321"))
  (testing "various time zone offsets"
    (are [p s] (= p (parse construct-string s))
         "2001-01-01T00:00:00.000000000+00:00" "2001Z"
         "2001-02-01T00:00:00.000000000+07:08" "2001-02+07:08"
         "2001-02-01T00:00:00.000000000-11:17" "2001-02-11:17")))

(deftest leap-year
  (testing "feburary 29 is forbidden if not leap year"
    (is (thrown? RuntimeException (parse construct-string "1900-02-29")))
    (is (thrown? RuntimeException (parse construct-string "1999-02-29"))))
  (testing "february 29 is allowed on leap years"
    (are [p s] (= p (parse construct-string s))
         "2004-02-29T00:00:00.000000000+00:00" "2004-02-29"
         "2000-02-29T00:00:00.000000000+00:00" "2000-02-29")))

(deftest leap-seconds
  (testing "leap second is forbidden if minutes is not 59"
    (is (thrown? RuntimeException
                 (parse construct-string "2000-01-02T00:58:60"))))
  (testing "leap second is allowed if minutes is 59"
    (is (= "1999-11-30T00:59:60.000000000+00:00"
           (parse construct-string "1999-11-30T00:59:60")))))

(deftest restrictions
  (are [s] (thrown? RuntimeException (parse construct-string s))
       "12000-02-03T04:05:06.987654321-07:08"
       "2000-13-03T04:05:06.987654321-07:08"
       "2004-02-30T04:05:06.987654321-07:08"
       "2004-09-31T04:05:06.987654321-07:08"
       "2004-12-32T04:05:06.987654321-07:08"
       "2000-02-03T24:05:06.987654321-07:08"
       "2000-02-03T04:60:06.987654321-07:08"
       "2000-02-03T04:05:60.987654321-07:08"
       "2000-02-03T04:59:61.987654321-07:08"
       "2000-02-03T04:05:06.987654321-24:08"
       "2000-02-03T04:05:06.987654321-07:60"
       "2000-12-03t04:05:06.987654321-07:08"
       "2000-12-03T04:05:06.987654321z"))
       
  


