#' Concentration and Homogeneity analysis for pool models
#'
#' @name calc_concentation_homogeneity
#'
#' @description This function performs a Concentration and Homogeneity analysis for pool models.
#' Outputs include a summary table depicting mean, standard deviation and confidence intervalls of the mean
#' ("SUMMARY_TABLE"); the normalized Herfindahl Index ("HERFINDAHL_INDEX"); Boxplots per pool ("BOXPLOT");
#' Violin Plot per pool ("VIOLIN_PLOT") and a histogram depicting distribution across pools.
#'
#' @param data_df ('tbl') Dataframe containing all variables listed below.
#' @param lgd_pred_var ('character') Name of the variable/column containing model predictions.
#' @param lgd_real_var ('character') Name of the variable/column containing realised LGD.
#' @param grade_var ('character') Name of the discrete variable indicating the grade/pools.
#' @param alpha ('numeric') Confidence level 1-alpha/2 is diplayed in SUMMARY_TABLE
#'
#' @return tibble
#' @export
#' @examples
#' conhom <- calc_concentation_homogeneity(data_df=df_lgd,lgd_pred_var=LGD_PREDICTION,
#' lgd_real_var=REAL_LGD,grade_var=LTV_BIN,alpha = 0.1)

utils::globalVariables(c("MEAN_LGD_REAL", "SD_LGD_REAL", "CI_LOWER_LGD_REAL", "CI_UPPER_LGD_REAL",
                         "GRADE_POOL","rel_n", "n", "N"))

calc_concentation_homogeneity <- function(data_df,
                                          lgd_pred_var,
                                          lgd_real_var,
                                          grade_var=NaN,
                                          alpha = 0.1) {
  #browser()
  lgd_pred_var = rlang::as_name(rlang::enquo(lgd_pred_var))
  lgd_real_var = rlang::as_name(rlang::enquo(lgd_real_var))
  grade_var = rlang::as_name(rlang::enquo(grade_var))

  if (any(is.na(lgd_pred_var))) stop(paste("NAs in ", lgd_real_var))
  if (any(is.na(lgd_real_var))) stop(paste("NAs in ", lgd_pred_var))
  if (any(is.na(data_df[[grade_var]]))) stop(paste("NAs in ", grade_var))

  data_df <- data_df %>% dplyr::mutate(lgd_real=as.numeric(!!rlang::sym(lgd_real_var)),
                                lgd_pred=as.numeric(!!rlang::sym(lgd_pred_var)),
                                grade_bucket=factor(!!rlang::sym(grade_var)))

  result <- list()

  SUMMARY_TABLE <- data_df %>%
    dplyr::group_by(grade_bucket) %>%
    dplyr::summarise(N=dplyr::n(),
              MEAN_LGD_REAL = mean(lgd_real),
              SD_LGD_REAL = stats::sd(lgd_real),
              CI_LOWER_LGD_REAL = MEAN_LGD_REAL - stats::qt(1-alpha/2, N-1) * SD_LGD_REAL/sqrt(N),
              CI_UPPER_LGD_REAL = MEAN_LGD_REAL + stats::qt(1-alpha/2, N-1) * SD_LGD_REAL/sqrt(N)
    ) %>%
    dplyr::rename(GRADE_POOL=grade_bucket)

  result[["SUMMARY_TABLE"]] <- SUMMARY_TABLE

  nrow <- nrow(SUMMARY_TABLE)
  HERFINDAHL_INDEX <-  SUMMARY_TABLE %>%
    dplyr::summarise('normalised Herfindahl Index'= ((sum((N/sum(N))^2)-1/nrow)/(1-1/nrow)))

  result[["HERFINDAHL_INDEX"]] <- HERFINDAHL_INDEX

  BOXPLOT <- ggplot2::ggplot(data_df, ggplot2::aes(x=grade_bucket, y=lgd_real, fill=grade_bucket)) +
    ggplot2::geom_boxplot() +
    ggplot2::scale_fill_discrete(name = grade_var) +
    ggplot2::xlab(grade_var) +
    ggplot2::ylab(lgd_pred_var) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1), legend.position = "none")+
    ggplot2::ggtitle(paste("Distribution LGD real.: ", grade_var, sep=""))

  result[["BOXPLOT"]] <- BOXPLOT

  VIOLIN_PLOT <- ggplot2::ggplot(data_df %>% dplyr::filter(lgd_real<1), ggplot2::aes(x=grade_bucket, y=lgd_real, fill=grade_bucket)) +
    ggplot2::geom_violin()+
    ggplot2::scale_fill_discrete(name = grade_var) +
    ggplot2::xlab(grade_var) +
    ggplot2::ylab(lgd_pred_var) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1), legend.position = "none")+
    ggplot2::ggtitle(paste("Distribution LGD real.: ", grade_var, sep=""))

  result[["VIOLIN_PLOT"]] <- VIOLIN_PLOT

  n_tot <- nrow(data_df)

  HISTOGRAM <- data_df %>%
    dplyr::group_by(grade_bucket) %>%
    dplyr::summarize(n = dplyr::n()) %>%
    dplyr::mutate(rel_n = n/n_tot) %>%
    dplyr::select(grade_bucket, rel_n) %>%
    ggplot2::ggplot(ggplot2::aes(x = grade_bucket, y = rel_n))  +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(), fill = "#ffcc33") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 75, hjust = 1)) +
    ggplot2::xlab(grade_var) +
    ggplot2::ylab("n(% of total)") +
    ggplot2::ggtitle(paste0("Relative distribution on ", grade_var,"."))

  result[["HISTOGRAM"]] <- HISTOGRAM

  return(result)
}
