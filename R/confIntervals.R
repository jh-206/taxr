#' Calculate Bootstrapped Confidence Interval
#'
#' @description Computes confidence interval calculations for the mean and median, both based on bootstrap resampling.
#' @param x Vector of numeric values.
#' @param y (Optional) vector of numeric values.
#' @param nboot Number indicating number of bootstrap iterations.
#' @param alpha Numeric value indicates confidence level, i.e. the interval covers \code{1 - alpha}.
#' @param FUN Function to evaluate.
#' @param na.rm Logical indicating whether to drop missing values.
#' @return List with four numeric vectors: the mean and median values and their respective confidence intervals.
#' @export
robust_ci <- function(x, y = NULL, nboot = 100, alpha = .05, FUN = NULL, na.rm = F) {
  # Calculate nonparametric confidence intervals using bootstrapping for given function

  if(is.null(y)) {
    # Omit na
    if(na.rm) {
      x <- na.omit(x)
    }

    est <- FUN(x)
    if(length(est) != 1 | !is.numeric(est)){
      stop('Error: Argument `FUN` must return numeric vector of length 1')
    }

    n <- length(x)
    vals <- rep(NA, nboot)

    for(i in 1:nboot) {
      temp_x <- x[sample(1:n, replace = T)]
      vals[i] = FUN(temp_x)
    }

    return(c('Estimate' = est, quantile(vals, alpha/2), quantile(vals, 1-alpha/2)))
  } else {
    if(na.rm) {
      inds <- union(which(is.na(x)), which(is.na(y)))
      if(length(inds) >0){
        x <- x[-inds]
        y <- y[-inds]
      }
    }

    est <- FUN(x, y, na.rm = na.rm)

    n <- length(x)
    vals <- rep(NA, nboot)

    for(i in 1:nboot) {
      inds <- sample(1:n, replace = T)
      temp_x <- x[inds]
      temp_y <- y[inds]
      vals[i] = FUN(temp_x, temp_y, na.rm = na.rm)
    }

    return(c('Estimate' = est, quantile(vals, alpha/2), quantile(vals, 1-alpha/2)))
  }

}

#' Calculate Theoretical Confidence Interval
#'
#' @description Computes confidence interval calculations for the mean and median. Mean interval based on T test; median interval
#' based on binomial distribution.
#' @param r Vector of numeric Values
#' @param alpha Numeric value indicates confidence level, i.e. the interval covers \code{1 - alpha}.
#' @param na.rm Logical option indicating whethermissing values should be removed.
#' @return List with four numeric vectors: the mean and median values and their respective confidence intervals.
calc_ci <- function(r, alpha = .05, na.rm = F) {

  if(na.rm) r <- na.omit(r)

  ci_ls <- list(
    'Median Value' = median(r),
    'Median Confidence Interval' = sort(r)[qbinom(c(alpha/2,1-alpha/2), length(r), 0.5)],
    'Mean Value' = mean(r),
    'Mean Confidence Interval' = as.vector(t.test(r, conf.level = 1-alpha)$conf.int)
  )

  attr(ci_ls, 'details') <- 'Mean: T test. Median: Binomial distribution'

  return(ci_ls)
}



