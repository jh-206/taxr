#***********************************#
#                                   #
# Standard ASR Stats Function     #
#                                   #
#***********************************#

#' Perform Ratio Study
#'
#' @description Perform Ratio Study
#' @param df Dataframe with standardized names
#' @param property_class Class of properties to be analyzed, i.e. analysis title.
#' @return Dataframe of standard ratio study statstics
#'
#' @export
#' @importFrom magrittr "%>%"
ratioStats <- function(df, property_class = NULL) {

  level_ci <- calc_ci(df$asr)
  prb_val <- round(prb(df$assessed, df$saleprice), 3)
  cod_ci <- robust_ci(df$asr, nboot = 1000, FUN = cod) %>% round(3)
  prd_ci <- robust_ci(df$assessed, df$saleprice, nboot = 1000, FUN = prd) %>% round(3)

  tab <- df %>%
    dplyr::summarise(
      'Number of observations' = n(),
      'Total appraised value' = sum(assessed),
      'Total sale price' = sum(saleprice),
      'Average appraised value' = mean(assessed),
      'Average sale price' = mean(saleprice),
      'Mean ASR' = mean(asr),
      'Mean ASR 95% CI' = paste(round(level_ci$`Mean Confidence Interval`, 3), collapse = ' to '),
      'Median ASR' = median(asr),
      'Median ASR 95% CI' = paste(round(level_ci$`Median Confidence Interval`, 3), collapse = ' to '),
      'Weighted mean ASR' = weightedMean(assessed, saleprice),
      'Price-related differential (PRD)' = prd(assessed, saleprice),
      'PRD 95% CI' = paste(prd_ci[2:3], collapse = ' to '),
      'Price-related bias (PRB)' = prb_val[1],
      'PRB 95% CI' = paste(prb_val[2:3], collapse = ' to '),
      'Coefficient of dispersion (COD)' = cod_ci['Estimate'],
      'COD 95% CI' = paste(cod_ci[2:3], collapse = ' to ')
    )


  tab <- tab %>%
    dplyr::mutate_at(.vars = c('Mean ASR', 'Median ASR', 'Weighted mean ASR', 'Price-related differential (PRD)',
                        'Coefficient of dispersion (COD)'), dplyr::funs(round(., 3)))

  # Add Qualitative Info
  tab <- tab %>%
    dplyr::mutate(
      'Sale Date Range' = paste(round(as.numeric(max(as.Date(df$saledate)) - min(as.Date(df$saledate))) / 365.25, 1), 'years'),
      'Sale Date Period' = paste(dateFormat(min(df$saledate)), 'to', dateFormat(max(df$saledate)))
    )

  if(!is.null(property_class)) {
    tab <- tab %>%
      dplyr::mutate('Property Class' = property_class)
  }

  return(tab)
}

#' Format Ratio Study Output
#'
#' @param tab Dataframe, output of \cod{ratioStats}
#' @return Dataframe of formatted ratio study statistics
#' @export
formatRatioStats <- function(tab) {
  tab %>%
    dplyr::mutate_at(.vars = c('Total appraised value', 'Total sale price',
                        'Average appraised value', 'Average sale price'), dplyr::funs(round(., -3))) %>%
    dplyr::mutate_at(.vars = c('Number of observations', 'Total appraised value', 'Total sale price',
                        'Average appraised value', 'Average sale price'), numFormat)
}


