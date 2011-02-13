(ns org.bovinegenius.date-time
  (:use (clojure.contrib def))
  (:import (org.joda.time DateTime)
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

(extend Date
  Datable
  {:as-date-time (fn [self]
                   (-> self .getTime DateTime.))
   :from-date-time (fn [self date-time]
                     (-> date-time .getMillis Date.))})

(extend DateTime
  Datable
  {:as-date-time identity
   :from-date-time (fn [self date-time]
                     date-time)})

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
       (as-date-time calendar))))

(defn first-day-of-week
  "Answer the weekday that is the start of the week for the given
Datable."
  ([date]
     (start-of-week date (locale)))
  ([date locale]
     (-> date as-date-time (.toCalendar locale)
         .getFirstDayOfWeek int->weekday)))

(defn start-of-week
  "Answer the day that is the start of the 'current' week of the given
Datable."
  ([date]
     (start-of-week date (locale)))
  ([date locale]
     (day-of-week date (first-day-of-week date locale) locale)))
