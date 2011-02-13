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

(extend java.util.Date
  Datable
  {:as-date-time (fn [self]
                   (-> self .getTime DateTime.))
   :from-date-time (fn [self date-time]
                     (-> date-time .getMillis java.util.Date.))})

(extend DateTime
  Datable
  {:as-date-time identity
   :from-date-time (fn [self date-time]
                     date-time)})

(extend java.util.Calendar
  Datable
  {:as-date-time (fn [self]
                   (as-date-time (.getTime self)))
   :from-date-time (fn [self date-time]
                     (-> self .clone (.setTimeInMillis (.getMillis date-time))))})


