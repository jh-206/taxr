#' Calculate Coefficient of Dispersion (COD)
#'
#' @description  COD is defined as the average percent deviation from the median. In the tax assessment context, the
#' COD is a standard metric to assess "horizontal equity", or whether properties of the same market value are assessed
#' at similar levels.
#' @param r A numeric vector of values. In the ratio study context, this would be a vector of Assessed-to-Sale Ratio (ASR) values.
#' @param na.rm Logical value indicating whether NA values should be removed.
#' @return Numeric vector of length one, indicating the COD of \code{r}.
#' @examples
#'
#' cod(runif(10))
#'
#' @export

cod <- function(r, na.rm = F) {

  # Check NA action
  if(na.rm) r <- na.omit(r)

  # Calculate Median
  M <- median(r)

  # Return COD
  return(mean(abs(r - M)) / M)
}

#' Calculate Coefficient of Price-Related Bias (PRB)
#'
#' @description The coefficient of price-related bias (PRB) is a measure of veritical equity, and is calculated by
#' regressing the percentage difference from the median ratio on percentage differences in value.
#' @details The PRB is a standard metric to assess "vertical equity", or whether high and low value properties
#' are assessed at similar levels. It is obtained by regressing. Negative values are
#' indicative of assessment \emph{regressivity}, while positive values are indicative of assessment \emph{progressivity}.
#' The PRB is generally preferred to the PRD, since the latter is known to be upwardly biased.
#' @param assessed A numeric vector of assessment values.
#' @param saleprice A numeric vector of sale prices.
#' @param ci Confidence level for interval.
#' @param na.rm Logical value indicating whether NA values should be removed.
#' @return A named numeric vector with the PRB value and high and low bounds.
#'
#' @export
prb <- function(assessed, saleprice, ci = .95, na.rm = F) {
  if(na.rm) {
    inds <- union(which(is.na(assessed)), which(is.na(saleprice)))
    if(length(inds) >0){
      assessed <- assessed[-inds]
      saleprice <- saleprice[-inds]
    }
  }

  asr <- assessed / saleprice

  val = .5 * (assessed / median(asr)) + .5*saleprice

  ln_val = log(val)/0.693

  diff_val = (asr - median(asr)) / median(asr)

  if(all(is.na(diff_val))) return(NA)

  fit <- lm(diff_val ~ ln_val)

  ret_val <- coef(fit)[2]

  if(!is.null(ci)) {
    ret_val <- c(ret_val, confint(fit, level = ci)[2,])
    names(ret_val)[1] = 'prb_val'
  }

  return(ret_val)
}

#' Calculate Price-Related Differential
#'
#' @description The price-related differential (PRD) is a measure of veritical equity, and is the mean divided by
#' the weighted mean.
#' @details The PRD is a measure of vertical equity. It is known to have a slight upward bias. For this reason,
#' the PRB is a regarded as a more reliable metric. PRD values above 1.03 are indicative of assessment \emph{regressivity},
#' while values below 0.98 is indicative of assessment \emph{progressivity}.
#' @param assessed A numeric vector of assessment values.
#' @param saleprice A numeric vector of sale prices.
#' @param na.rm Logical value indicating whether NA values should be removed.
#' @return Numeric vector of length one.
#'
#' @export
prd <- function(assessed, saleprice, na.rm = FALSE) {

  if(na.rm) {
    inds <- union(which(is.na(assessed)), which(is.na(saleprice)))
    if(length(inds) >0){
      assessed <- assessed[-inds]
      saleprice <- saleprice[-inds]
    }
  }

  # Calculate Means
  r.mean <- mean(assessed / saleprice)
  a.mean <- mean(assessed)
  s.mean <- mean(saleprice)

  # Return PRD
  return(r.mean / (a.mean / s.mean))
}

#' Calcualte Weighted Mean
#' @description The weighted mean, defined as the sum of appraised values divided by the sum of sale prices.
#' @param assessed A numeric vector of assessment values.
#' @param saleprice A numeric vector of sale prices.
#' @param na.rm Logical value indicating whether NA values should be removed.
#' @return Numeric vector of length one.
#'
#' @export
weightedMean <- function(assessed, saleprice, na.rm = F) {
  if(na.rm) {
    assessed = na.omit(assessed)
    saleprice = na.omit(saleprice)
  }

  return(sum(assessed) / sum(saleprice))
}
