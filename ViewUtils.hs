module ViewUtils where

{---
 - ViewUtils Module
 -
 - Your guess is as good as mine why this is its own thing
 - gotta get the month I guess
 -
 - Shaun Kerr
 -}

import Timestamp

showMonth :: Integer -> String
showMonth 1 = "January"
showMonth 2 = "February"
showMonth 3 = "March"
showMonth 4 = "April"
showMonth 5 = "May"
showMonth 6 = "June"
showMonth 7 = "July"
showMonth 8 = "August"
showMonth 9 = "September"
showMonth 10 = "October"
showMonth 11 = "November"
showMonth 12 = "December"