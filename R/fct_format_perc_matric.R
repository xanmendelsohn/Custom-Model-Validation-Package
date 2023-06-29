#' Apply format to backtesting matrix
#'
#' @name fct_format_perc_matrix
#'
#' @description Function applies color scale format to matrixes with percent entries.
#'
#' @param df ('tbl')
#' @param title ('character')
#'
#' @return kable
#' @export

utils::globalVariables(c("title"))

fct_format_perc_matrix <- function(df,title="Tab:")
{
  out <- df %>%
    tibble::tibble() %>%
    dplyr::mutate_if(is.numeric, function(x) {fct_custom_color_tile_backtesting("transparent", '#ffcc33', alpha = FALSE)(round(as.double(x),2))}) %>%
    knitr::kable(escape = F, caption = title) %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
    kableExtra::row_spec(0, bold = T, background = '#ffcc33')

  return(out)
}
