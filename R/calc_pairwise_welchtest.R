#' Welch's T-Test for all pool pairs
#'
#' @name calc_pairwise_welchtest
#'
#' @description This function performs Welch's T-Test for the realized values of each pool pair and returns an matrix (tibble) of p-values.
#'
#' @param data_df ('tbl') Dataframe containing all variables listed below.
#' @param lgd_pred_var ('character') Name of the variable/column containing model predictions.
#' @param lgd_real_var ('character') Name of the variable/column containing realised LGD.
#' @param grade_var ('character') Name of the discrete variable indicating the grade/pools.
#'
#' @return tibble
#' @export
#' @examples
#' calc_pairwise_welchtest(data_df=df_lgd,lgd_pred_var=LGD_PREDICTION,
#' lgd_real_var=REAL_LGD,grade_var=LTV_BIN)

utils::globalVariables(c("lgd_real", "lgd_pred", "grade_bucket1",
                         "n1", "mean1", "sd1", "tval", "dof", "p.value",
                         "grade", "mean2", "n2", "sd", "sd2", "grade_bucket2"))

calc_pairwise_welchtest <- function(data_df,
                                    lgd_pred_var,
                                    lgd_real_var,
                                    grade_var
) {

  lgd_pred_var = rlang::as_name(rlang::enquo(lgd_pred_var))
  lgd_real_var = rlang::as_name(rlang::enquo(lgd_real_var))
  grade_var = rlang::as_name(rlang::enquo(grade_var))

  if (any(is.na(lgd_pred_var))) stop(paste("NAs in ", lgd_real_var))
  if (any(is.na(lgd_real_var))) stop(paste("NAs in ", lgd_pred_var))
  if (any(is.na(data_df[[grade_var]]))) stop(paste("NAs in ", grade_var))

  data_df <- data_df %>%
    dplyr::mutate(lgd_real=as.numeric(!!rlang::sym(lgd_real_var)),
                  lgd_pred=as.numeric(!!rlang::sym(lgd_pred_var)),
                  grade_bucket1=factor(!!rlang::sym(grade_var))
  )

  df1 <- data_df %>%
    dplyr::arrange(grade_bucket1) %>%
    dplyr::group_by(grade_bucket1) %>%
    dplyr::summarize(n1 = dplyr::n(),
                     mean1 = mean(lgd_real,na.rm=T),
                     sd1 = sd(lgd_real,na.rm=T)) %>%
    (function(x) x %>% dplyr::left_join(x %>% dplyr::select(grade_bucket2=grade_bucket1,n2=n1,mean2=mean1,sd2=sd1),by=character())) %>%
    dplyr::mutate(tval = (mean1-mean2)/sqrt(sd1^2/n1 + sd2^2/n2),
                  dof = (sd1^2/n1 + sd2^2/n2)^2/(sd1^4/(n1^2*(n1-1)) + sd2^4/(n2^2*(n2-1))),
                  p.value = 2*stats::pt(-abs(tval),df=dof,lower.tail = T) #Welch test: unequal variance
    ) %>%
    dplyr::select(grade_bucket1,grade_bucket2,p.value) %>%
    tidyr::pivot_wider(names_from = "grade_bucket2", values_from = "p.value") %>%
    dplyr::rename(grade = grade_bucket1)

  return(df1)
}
