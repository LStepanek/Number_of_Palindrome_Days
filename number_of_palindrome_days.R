###############################################################################
###############################################################################
###############################################################################

## Today, October 6, 2016, is considered to be a unique day since the date
## format in terms of "DDMMYYYY" is palindrome, i. e. sequence of characters
## which reads the same backward or forward.

## Note that if the format of the day could become palindromic, then we omit
## the leading zero. The date format results in the term "DMMYYYY" then.

## How many palindrome days there were after the birth of Christ, i. e. since
## January 1, 0000?

## ----------------------------------------------------------------------------

###############################################################################

## I am generating all the dates since January 1, 0000 till today -------------

my_dates <- format(seq.Date(
from = as.Date("0000-01-01"), to = as.Date("2016-10-06"), by = "day"
), format = "%Y-%m-%d")

my_dates_strings <- format(seq.Date(
from = as.Date("0000-01-01"), to = as.Date("2016-10-06"), by = "day"
), format = "%d%m%Y")


## ----------------------------------------------------------------------------

###############################################################################

## helper function ------------------------------------------------------------

isPalindrome <- function(my_text){

# '''
# returns TRUE, if string "my_text" is palindrome, i. e. is read the same
# backward or forward; it returns FALSE otherwise
# if the format of the day could become palindromic, then the leading zero
# is omitted
# '''

if(nchar(my_text) == 0|nchar(my_text) == 1){

return(TRUE)

}else{

if(substr(my_text, 1, 1) == substr(my_text, nchar(my_text), nchar(my_text))){

isPalindrome(substr(my_text, 2, nchar(my_text) - 1))

}else{return(FALSE)}

}

}


## ----------------------------------------------------------------------------

###############################################################################

## let's analyze all the dates whether they are palindromic -------------------

is_palindrome_day <- unlist(lapply(
my_dates_strings,
function(x){
ifelse(
isPalindrome(x),
TRUE,
if(substr(x, 1, 1) == "0"){isPalindrome(substr(x, 2, nchar(x)))}else{FALSE}
)
}
))


## ----------------------------------------------------------------------------

###############################################################################

## which days are palindrome days and what about their number -----------------

my_dates[is_palindrome_day]
length(which(is_palindrome_day))    # 319


## ----------------------------------------------------------------------------

###############################################################################

## let's save these days in format YYYY-MM-DD ---------------------------------

write.csv(
x = as.data.frame(
cbind(
"date (YYYY-MM-DD)" = my_dates[is_palindrome_day],
"date ((D)DMMYYYY)" = my_dates_strings[is_palindrome_day]
)
),
row.names = FALSE,
file = "palindrome_days.csv"
)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################





