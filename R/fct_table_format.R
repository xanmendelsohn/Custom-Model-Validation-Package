#' Format output tables in Markdown
#'
#' @name fct_table_format
#'
#' @description Function formats output tables to be presented in a HTML Markdown.
#'
#' @param df ('tbl')
#' @param title ('character')
#'
#' @return kable
#' @export

utils::globalVariables(c("title"))

fct_table_format <- function(df,title="Tab:",digits=3,highlight.var=NULL,highlight.x=NULL,align="lrrrrrrrrrr"){

  if (!is.null(highlight.var) & is.integer(highlight.var)) highlight.var <- colnames(df)[highlight.var]

  out <- df %>%
    tibble::tibble() %>%
    dplyr::mutate_if(is.double, ~ round(.,digits)) %>%
    dplyr::mutate_at(dplyr::vars(highlight.var), formattable::formatter("span",
                                                          style = x ~ formattable::style(display = "block",
                                                                                         padding = "0 4px",
                                                                                         `border-radius` = "4px",
                                                                                         `background-color` = ifelse(abs(x)>=highlight.x, "#ffcc33", "transparent"))))  %>%
    knitr::kable(align = align, escape = TRUE, caption = title) %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
    kableExtra::row_spec(0, bold = T, background = '#ffcc33')

  if (dim(df)[2] > 9){
    out %>% kableExtra::scroll_box(width = "900px")
  } else {
    out
  }
}
