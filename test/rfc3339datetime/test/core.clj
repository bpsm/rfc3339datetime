(ns rfc3339datetime.test.core
  (:use [rfc3339datetime.core])
  (:use [clojure.test]))

(deftest good-cases
  (are [s p] (= p (parse s))
       "2001" {:years 2001}
       "2001-02" {:years 2001 :months 2}
       "2001-02-03" {:years 2001 :months 2 :days 3}
       "2001-02-03T04" {:years 2001 :months 2 :days 3 :hours 4}
       "2001-02-03T04:05" {:years 2001 :months 2 :days 3 :hours 4 :minutes 5}
       "2001-02-03t04:05:06" {:years 2001 :months 2 :days 3 :hours 4
                              :minutes 5 :seconds 6}
       "2001-02-03T04:05:06.987654321" {:years 2001 :months 2 :days 3 :hours 4
                                        :minutes 5 :seconds 6
                                        :fraction 0.987654321}
       "2001Z" {:years 2001
                :offset {:hours 0 :minutes 0}}
       "2001-02+07:08" {:years 2001 :months 2
                        :offset {:hours 7 :minutes 8}}
       "2001-02-03-07:08" {:years 2001 :months 2 :days 3
                           :offset {:hours -7 :minutes -8}}
       "2001-02-03T04+00:00" {:years 2001 :months 2 :days 3 :hours 4
                              :offset {:hours 0 :minutes 0}}
       "2001-02-03T04:05Z" {:years 2001 :months 2 :days 3 :hours 4 :minutes 5
                            :offset {:hours 0 :minutes 0}}
       "2001-02-03t04:05:06Z" {:years 2001 :months 2 :days 3 :hours 4
                               :minutes 5 :seconds 6
                               :offset {:hours 0 :minutes 0}}
       "2001-02-03T04:05:06.987654321-07:08" {:years 2001 :months 2 :days 3
                                              :hours 4 :minutes 5 :seconds 6
                                              :fraction 0.987654321
                                              :offset {:hours -7 :minutes -8}}
       "04" {:hours 4}
       "04:05" {:hours 4 :minutes 5}
       "04:05:06" {:hours 4 :minutes 5 :seconds 6}
       "04:05:06.123456789" {:hours 4 :minutes 5 :seconds 6
                             :fraction 0.123456789}
       "04:05-07:08" {:hours 4 :minutes 5 :offset {:hours -7 :minutes -8}}))

(deftest leap-year
  (is (thrown? RuntimeException (parse "1900-02-29")))
  (is (thrown? RuntimeException (parse "1999-02-29")))
  (is (= {:years 2004 :months 2 :days 29} (parse "2004-02-29"))))

(deftest leap-seconds
  (is (thrown? RuntimeException (parse "00:58:60")))
  (is (= {:hours 0 :minutes 59 :seconds 60} (parse "00:59:60"))))

(deftest restrictions
  (are [s] (thrown? RuntimeException (parse s))
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
       "2000-02-03T04:05:06.987654321-07:60"))



