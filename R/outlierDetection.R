#***********************************#
#                                   #
# Outlier Detection Methods         #
#                                   #
#***********************************#

#' Test for Outliers
#'
#' @description Identify outliers in numeric vector.
#' @param x Numeric vector of values
#' @param method String indicating method of outlier detection. Values supported are \code{iqr} and \code{ptile}.
#' @param na.rm Logical value indicating whether NA values should be removed.
#' @details Outlier detection methods include inner quartile range test and a simple upper and lower percentile identification.
#' @return Logical vector of values
#'
#' @export
isOutlier <- function(x, method = "iqr", na.rm = F, ...) {
  if(!method %in% c("iqr", "ptile")){
    stop(paste0("Method '", method, "' not supported."))
  }

  ret <- switch(method,
                "iqr" = iqrOutlier(x, na.rm = na.rm, ...),
                "ptile" = ptileOutlier(x, na.rm = na.rm, ...)
  )

  return(ret)
}

#' @describeIn isOutlier Innerquartile range method for identifying outliers.
#' @param val Numeric value which is multiplied by IQR to identify outlier boundary.
iqrOutlier <- function(x, val = 3, na.rm = F, ...) {
  # Method to detect outliers using Inner Quartile Range criteria
  # Default standard for outlier is outside 3*IQR, user can change factor (1.5 also used)
  # Input: vector of values, Optional: val factor
  # Output: arranged vector of outlier values

  # Extract non-outlier range
  x.range <- c(summary(x)["1st Qu."] - val * IQR(x, na.rm = na.rm), summary(x)["3rd Qu."] + val * IQR(x, na.rm = na.rm))
  # Return logical vector
  return(outsideRange(x, vec = x.range))
}

#' @describeIn isOutlier Inner percentile method for identifying outliers.
#' @param inner_perc Numeric value between 0 and 1 indicating inner percentile of data.
ptileOutlier <- function(x, inner_perc = .995, na.rm = F, ...) {

  if(outsideRange(inner_perc, c(0, 1))){
    stop("inner_perc must be between 0 and 1.")
  }

  bounds <- quantile(x, c((1 - inner_perc)/2, (1 + inner_perc)/2), na.rm = na.rm)

  ret <- outsideRange(x, bounds)

  return(ret)
}


