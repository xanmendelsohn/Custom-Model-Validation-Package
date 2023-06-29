#' Format backtesting matrix
#'
#' @name fct_custom_color_tile_backtesting
#'
#' @description Function defines format for matrix with color scale.
#'
#' @param ... ('numeric')
#'
#' @return format
#' @export
fct_custom_color_tile_backtesting <- function (...)
{

  formattable::formatter("span",
                         style = function(x) formattable::style(display = "block",
                                                                padding = "0 4px",
                                                                `border-radius` = "4px",
                                                                `background-color` = formattable::csscolor(formattable::gradient(c(c(0.0,0.1),pmin(abs(as.numeric(x)),0.1)),
                                                                                                                                 ...))[-(1:2)]))
}
