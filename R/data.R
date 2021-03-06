#' Tax Assessment Data for 2018 Sales in King County, Washington
#'
#' A dataset containing tax assessment values, sales information, and property characteristics for a sample of 2018
#' sales in King County, Washington.
#'
#' @format A data frame with 1000 rows and 9 variables:
#' \describe{
#'   \item{PIN}{parcel identification number, unique identifier}
#'   \item{Address}{physical address of property}
#'   \item{SqFtTotLiving}{total living area in square feet}
#'   \item{BathCount}{number of bathrooms, including partial}
#'   \item{YrBuilt}{year built of property}
#'   \item{SalePrice}{sale price in dollars}
#'   \item{DocumentDate}{date sale document recorded in county. Note this might diverge from the actual sale date, but will be used in examples for simplicity.}
#'   \item{ApprVal}{tax assessed value in dollars}
#'   \item{TaxYr}{tax assessment year}
#' }
#' @source \url{https://info.kingcounty.gov/assessor/DataDownload/default.aspx}
"ex_tax_data"
