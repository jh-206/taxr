#************************#
#                        #
#  Utility Functions     #
#                        #
#************************#


# Missing Function, returns true if value is NA, equal to 0, or empty string "" ~~~~~~~~~~~
isMissing <- function(x) {
  ifelse(is.na(x) | x == 0 | x == "", T, F)
}

# Convert Missing to NA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
zeroToNA <- function(x) {
  ifelse(isMissing(x), NA, x)
}

# Percent Format ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as_percent <- function(x, digits = 1, format = "f", x100 = T, ...) {
  if(x100){x = 100*x}
  paste0(formatC(x, format = format, digits = digits, ...), "%")
}

# Number Format ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
numFormat <- function(num) {
  prettyNum(num, big.mark=",")
}

# Function return False if outside range of vector ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
outsideRange <- function(x, vec) {
  x < min(vec) | x > max(vec)
}

# Wrapper for round function ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Intended use w p-values

pRound <- function(x, digits = 3) {

  ret <- round(x, digits = digits)

  if(ret == 0) {
    ret <- paste0("less than 0.", stringr::str_pad("", pad = "0", width = digits - 1), 1)
  }

  return(ret)
}

#' Reporting format for Dates
#'
#' @description Formats dates in the form: "%m/%d/%Y".
#' @param d Vector of either date or character type, formatted "%Y-%m-%d".
#' @return Character vector of formatted dates.
dateFormat <- function(d) {

  d <- format(strptime(d,format = "%Y-%m-%d"),"%m/%d/%Y")

  return(d)
}
