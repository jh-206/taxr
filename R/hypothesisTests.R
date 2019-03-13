#***********************************#
#                                   #
# Hypothesis Testing Fucntions      #
#                                   #
#***********************************#

# Normality Test ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  normTest <- function(x, method = "shapiro") {
    
    if(!method %in% c("shapiro", "ad", "lillie", "pearson")){
      warning(paste0("Method '", method, "' not supported."))
      return(NA)
    }
    
    ret <- switch(method,
                  "shapiro" = shapiro.test(x),
                  "ad" = nortest::ad.test(x),
                  "lillie" = nortest::lillie.test(x),
                  "pearson" = nortest::pearson.test(x)
                  )
    
    return(ret)
    
  }
  
  
# Test Level For Value ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  levelTest <- function(x, method = "t", target = 100, ok_bounds = c(90, 110), ...) {
    
    if(!method %in% c("t", "wilcox")){
      warning(paste0("Method '", method, "' not supported."))
      return(NA)
    }
    
    ret <- switch(method,
                  "t" = tWrap(x, mu = target, target = target, ok_bounds = ok_bounds, ...),
                  "wilcox" = wilcox.test(x, mu = target, ...)
                  )
    
    return(ret)
  }
  
  # Wrapper for t test, includes logical if conf int is within set bounds
  tWrap <- function(x, mu, ok_bounds, ...) {
    ret <- t.test(x, mu = mu, ...)
    
    within_bounds <- outsideRange(tt$conf.int, ok_bounds)
    
    ret$"within_bounds" <- ifelse(sum(within_bounds) == 0, T, F)
    
    return(ret)
  }
  
  
  
