#' LGD (Loss Given Default) outcomes and estimates of 500 fictitious loans
#'
#' A randomly generated dataset containing 500 historically realised LGDs, the LTV (Loan to Value),
#' LGD Estimates and LTV bin (pools)
#'
#' @format A data frame with 500 rows and 4 variables:
#' \describe{
#'   \item{REAL_LGD}{realised LGD value}
#'   \item{LGD_PREDICTION}{the model's estimated LGD value}
#'   \item{LTV}{the loans loan-to-value}
#'   \item{LTV_BIN}{LTV's corresponding bin}
#' }
"df_lgd"
