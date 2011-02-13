Time Bender
-----------

Library for manipulating dates and times. The functions provided work on several different types:
*java.util.Date
*java.util.Calendar
*org.joda.time.DateTime
*org.joda.time.Instance
*java.sql.Date
*java.sql.Timestamp

Usage
-----

The functions provieded allow for manipulating many types of dates.

Examples:

To set the day of month of a Joda Time DateTime:
    (day-of-month (org.joda.time.DateTime. 2011 3 14 12 0 0 0) 15)
    => #<DateTime 2011-03-15T12:00:00.000-04:00>

the same for a java.util.Date:
    (day-of-month (java.util.Date. 2011 3 14) 15)
    => #<Date Sat Apr 15 00:00:00 EDT 3911>

License
-------

Copyright (C) 2011 Walter Tetzner. All rights reserved.

The use and distribution terms for this software are covered by the
Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
which can be found in the file epl-v10.html at the root of this distribution.
By using this software in any fashion, you are agreeing to be bound by
the terms of this license.

You must not remove this notice, or any other, from this software.
