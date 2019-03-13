#' Standardize Ratio Study Variables
#'
#' @description Rename Columns
#' @param df Dataframe of values
#' @param assessed Character string indicating column in \code{df} that is to be renamed \code{assessed}.
#' @param saleprice Character string indicating column in \code{df} that is to be renamed \code{saleprice}.
#' @param uid Character string indicating column in \code{df} that is to be renamed \code{uid}, i.e. unique ID.
#' @param saledate Character string indicating column in \code{df} that is to be renamed \code{saledate}.
#' @param living_area Character string indicating column in \code{df} that is to be renamed \code{living_area}.
#' @param lot_area Character string indicating column in \code{df} that is to be renamed \code{lot_area}.
#' @param asr Character string indicating column in \code{df} that is to be renamed \code{asr}, i.e. assessed to sale ratio.
#' @param x Character string indicating column in \code{df} that is to be renamed \code{x}, i.e. longitude coordinate.
#' @param y Character string indicating column in \code{df} that is to be renamed \code{y}, i.e. latitude coordinate.
#' @param calc_asr Logical indicating whether to calculate the assessed to value ratio (asr).
#'
#' @return Dataframe with renamed columns
#'
#' @export
standardize <- function(df, assessed, saleprice = NULL, uid = NULL, saledate = NULL, living_area = NULL, lot_area = NULL, asr = NULL, x = NULL, y = NULL, calc_asr = T) {
  stopifnot('data.frame' %in% class(df))

  # Extract user entered params
  arg <- as.list(match.call())
  names_ls <- arg[!names(arg) %in% c('', 'df')]

  # Ensure character class
  names_cl <- lapply(names_ls, class) %>% unlist()
  if(!all(names_cl == 'character'))
    stop(paste0('Error: Input for column names must be character class. Inputs of wrong class: ', paste(names(names_cl)[names_cl != 'character'], collapse = ', ')))

  # Rename dataframe
  names_ls <- lapply(names_ls, as.name)

  df <- df %>%
    dplyr::rename_(.dots = names_ls)

  df <- typeStandardization(df)

  # Calculate ASR
  if(calc_asr & !'asr' %in% names(df) & 'saleprice' %in% names(df)) {
    df <- df %>%
      dplyr::mutate(asr = assessed / saleprice)
  }

  return(df)
}

#' Standardize Ratio Study Variable Types
#'
#' @param df Dataframe output of \code{standardize}
typeStandardization <- function(df) {
  num_cols <- c('assessed', 'saleprice', 'x', 'y', 'asr', 'living_area', 'lot_area')
  num_cols <- num_cols[num_cols %in% names(df)]

  df <- df %>%
    dplyr::mutate_at(num_cols, zeroToNA) %>%
    dplyr::mutate_at(num_cols, as.numeric)
  return(df)
}
