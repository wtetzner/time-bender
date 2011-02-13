;;   Copyright (c) 2011 Walter Tetzner. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns org.bovinegenius.time-bender
  (:use (clojure.contrib def))
  (:import (org.joda.time DateTime Period PeriodType Instant)
           (org.joda.time.format DateTimeFormat)
           (java.util Calendar Locale Date)))

(defvar *locale* nil
  "The locale to use.")

(defn locale
  "Get the current locale."
  []
  (or *locale* (Locale/getDefault)))

(defprotocol Datable
  "A protocol to convert to and from Joda Time DateTime objects."
  (as-date-time [self] "Convert to Joda Time DateTime.")
  (from-date-time [self date-time] "Convert from Joda Time DateTime."))

(defn parser
  "Returns a function that takes a string and parses it into a
DateTime using the given format string."
  [format]
  (let [formatter (DateTimeFormat/forPattern format)]
    (fn [string]
      (.parseDateTime formatter string))))

(defn parse
  "Parse a string into a DateTime."
  [string format]
  ((parser format) string))

(defn formatter
  "Returns a function that takes a Datable and formats it into a
string using the given format string."
  [format]
  (let [formatter (DateTimeFormat/forPattern format)]
    (fn [date]
      (.print formatter (as-date-time date)))))

(defn format-date
  "Format the given Datable to the specified format string."
  [date format]
  ((formatter format) date))

(extend Date
  Datable
  {:as-date-time (fn [self]
                   (-> self .getTime DateTime.))
   :from-date-time (fn [self date-time]
                     (let [new-date (.clone self)]
                       (.setTime new-date (.getMillis date-time))
                       new-date))})

(extend DateTime
  Datable
  {:as-date-time identity
   :from-date-time (fn [self date-time]
                     date-time)})

(extend Instant
  Datable
  {:as-date-time (fn [self]
                   (.toDateTimeISO self))
   :from-date-time (fn [self date-time]
                     (Instant. (.getMillis date-time)))})

(extend Calendar
  Datable
  {:as-date-time (fn [self]
                   (as-date-time (.getTime self)))
   :from-date-time (fn [self date-time]
                     (-> self .clone (.setTimeInMillis (.getMillis date-time))))})

(defn- locale?
  "Answers whether or not the given object is a java.util.Locale."
  [obj]
  (instance? Locale obj))

(defn before?
  "Answers whether or not the first Datable is before the second."
  [date1 date2]
  (.isBefore (as-date-time date1) (as-date-time date2)))

(defn after?
  "Answers whether or not the first Datable is after the second."
  [date1 date2]
  (.isAfter (as-date-time date1) (as-date-time date2)))

(defn before-now?
  "Answers whether or not the given Datable is before now."
  [date]
  (.isBeforeNow (as-date-time date)))

(defn after-now?
  "Answers whether or not the given Datable is after now."
  [date]
  (.isAfterNow (as-date-time date)))

(defn now?
  "Answers whether or not the given Datable represents now."
  [date]
  (.isEqualNow (as-date-time date)))

(defn leap-year?
  "Answers if the given Datable is a leap year."
  [date]
  (-> date as-date-time .year .isLeap))

(defn century-of-era
  "Get or set the century of the era of the given Datable."
  ([date]
     (-> date as-date-time .centuryOfEra .get))
  ([date century]
     (from-date-time date (-> date as-date-time
                              (.withCenturyOfEra century)))))

(defn day-of-month
  "Get or set the day of month of the given Datable."
  ([date]
     (-> date as-date-time .dayOfMonth .get))
  ([date day]
     (from-date-time date (-> date as-date-time (.withDayOfMonth day)))))

(defvar- weekdays
  {:sunday Calendar/SUNDAY
   :monday Calendar/MONDAY
   :tuesday Calendar/TUESDAY
   :wednesday Calendar/WEDNESDAY
   :thursday Calendar/THURSDAY
   :friday Calendar/FRIDAY
   :saturday Calendar/SATURDAY}
  "Map of weekday names to integers.")

(defvar- weekday-order
  [:sunday
   :monday
   :tuesday
   :wednesday
   :thursday
   :friday
   :saturday]
  "Weekday names in order.")

(defvar- weekday-reverse-order
  (vec (reverse weekday-order))
  "Weekday names in reverse order.")

(defvar int->weekday
  (into {} (map (fn [[weekday integer]]
                  [integer weekday])
                weekdays))
  "Map of integers to weekday names (as keywords).")

(defn weekday->int
  "Convert a day to an integer."
  [day]
  (cond (number? day) (int day)
        (or (keyword? day)
            (symbol? day)) (weekdays (-> day name
                                         .toLowerCase keyword))
            :else (weekdays (-> day str .toLowerCase keyword))))

(defn day-of-week
  "Get or set the day of week of the given Datable. Returns the day as one of the following:
  :sunday
  :monday
  :tuesday
  :wednesday
  :thursday
  :friday
  :saturday"
  {:arglists '([date] [date locale] [date day] [date day locale])}
  ([date]
     (day-of-week date (locale)))
  ([date day]
     (if (locale? day) (-> date as-date-time (.toCalendar day)
                           (.get Calendar/DAY_OF_WEEK) int->weekday)
           (day-of-week date day (locale))))
  ([date day locale]
     (let [calendar (-> date as-date-time (.toCalendar locale) .clone)]
       (.set calendar Calendar/DAY_OF_WEEK (weekday->int day))
       (from-date-time date (as-date-time calendar)))))

(defn first-day-of-week
  "Answer the weekday that is the start of the week for the given
Datable."
  ([date]
     (first-day-of-week date (locale)))
  ([date locale]
     (-> date as-date-time (.toCalendar locale)
         .getFirstDayOfWeek int->weekday)))

(defn last-day-of-week
  "Answer the weekday that is the end of the week for the given
Datable."
  ([date]
     (last-day-of-week date (locale)))
  ([date locale]
     (let [first-day (first-day-of-week date locale)]
       (->> (cycle weekday-reverse-order)
            (drop-while (partial not= first-day))
            rest
            first))))

(defn start-of-week
  "Answer the day that is the start of the 'current' week of the given
Datable."
  ([date]
     (start-of-week date (locale)))
  ([date locale]
     (day-of-week date (first-day-of-week date locale) locale)))

(defn end-of-week
  "Answer the day that is the start of the 'current' week of the given
Datable."
  ([date]
     (end-of-week date (locale)))
  ([date locale]
     (day-of-week date (last-day-of-week date locale) locale)))

(defn days-between
  "Return a range of days, either taking a start date and end date, or
a start date and a number of days."
  {:arglists '([start end] [start num-days])}
  [start end]
  (if (number? end)
    (map (fn [num]
           (from-date-time start (.plusDays (as-date-time start) num)))
         (range end))
    (days-between start (-> (Period. (as-date-time start) (as-date-time end)
                                     (PeriodType/days))
                            .getDays))))

(defn days-in-week
  "Get a sequence of days in the 'current' week of the given Datable."
  ([date]
     (days-in-week date (locale)))
  ([date locale]
     (days-between (start-of-week date locale) 7)))

(defn day-of-month
  "Get or set the day of month of the given Datable."
  ([date]
     (-> date as-date-time .dayOfMonth .get))
  ([date day]
     (from-date-time date (-> date as-date-time
                              (.withDayOfMonth day)))))

(defn start-of-month
  "Answer the day that is the start of the 'current' month of the
given Datable."
  [date]
  (day-of-month date 1))

(defn number-of-days-in-month
  "Answer the number of days in the 'current' month of the given
Datable."
  [date]
  (-> date as-date-time .dayOfMonth .getMaximumValue))

(defn end-of-month
  "Answer the day that is the end of the 'current' month of the
given Datable."
  [date]
  (day-of-month date (number-of-days-in-month date)))

(defn first-day-of-month
  "Answer the weekday that is the start of the month for the given
Datable."
  ([date]
     (first-day-of-month date (locale)))
  ([date locale]
     (day-of-week (start-of-month date) locale)))

(defn last-day-of-month
  "Answer the weekday that is the end of the month for the given
Datable."
  ([date]
     (last-day-of-month date (locale)))
  ([date locale]
     (day-of-week (end-of-month date) locale)))

(defn days-in-month
  "Get a sequence of days in the 'current' month of the given Datable."
  [date]
  (days-between (start-of-month date) (number-of-days-in-month date)))

(defn day-of-year
  "Get or set the day of year of the given Datable."
  ([date]
     (-> date as-date-time .dayOfYear .get))
  ([date day]
     (from-date-time date (-> date as-date-time
                              (.withDayOfYear day)))))

(defn start-of-year
  "Answer the day that is the start of the 'current' year of the
given Datable."
  [date]
  (day-of-year date 1))

(defn number-of-days-in-year
  "Answer the number of days in the 'current' year of the given
Datable."
  [date]
  (-> date as-date-time .dayOfYear .getMaximumValue))

(defn end-of-year
  "Answer the day that is the end of the 'current' year of the
given Datable."
  [date]
  (day-of-year date (number-of-days-in-year date)))

(defn first-day-of-year
  "Answer the weekday that is the start of the year for the given
Datable."
  ([date]
     (first-day-of-year date (locale)))
  ([date locale]
     (day-of-week (start-of-year date) locale)))

(defn last-day-of-year
  "Answer the weekday that is the start of the year for the given
Datable."
  ([date]
     (last-day-of-year date (locale)))
  ([date locale]
     (day-of-week (end-of-year date) locale)))

(defn days-in-year
  "Get a sequence of days in the 'current' year of the given Datable."
  [date]
  (days-between (start-of-year date) (number-of-days-in-year date)))

(defn era
  "Get or set the era of the given Datable."
  ([date]
     (-> date as-date-time .era .get))
  ([date era]
     (from-date-time date (-> date as-date-time (.withEra era)))))

(defn hour-of-day
  "Get or set the hour of day for the given Datable."
  ([date]
     (-> date as-date-time .hourOfDay .get))
  ([date hour]
     (from-date-time date (-> date as-date-time (.withHourOfDay hour)))))


(defn millis-of-second
  "Get or set the millis of second of the given Datable."
  ([date]
     (-> date as-date-time .millisOfSecond .get))
  ([date millis]
     (from-date-time date (-> date as-date-time
                              (.withMillisOfSecond millis)))))

(defn minute-of-hour
  "Get or set the minute of hour of the given Datable."
  ([date]
     (-> date as-date-time .minuteOfHour .get))
  ([date minute]
     (from-date-time date (-> date as-date-time
                              (.withMinuteOfHour minute)))))

(defn second-of-minute
  "Get or set the second of minute of the given Datable."
  ([date]
     (-> date as-date-time .secondOfMinute .get))
  ([date second]
     (from-date-time date (-> date as-date-time
                              (.withSecondOfMinute second)))))
