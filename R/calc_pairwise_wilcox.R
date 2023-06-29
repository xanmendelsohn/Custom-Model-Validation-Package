#' Paired Samples Wilcoxon Test for all pool pairs
#'
#' @name calc_pairwise_wilcox
#'
#' @description This function performs a Paired Samples Wilcoxon Test for the realized values of each pool pair and returns an matrix (tibble) of p-values.
#' Alternative Hypothesis: Pools have significantly different means.
#'
#' @param data_df ('tbl') Dataframe containing all variables listed below.
#' @param lgd_pred_var ('character') Name of the variable/column containing model predictions.
#' @param lgd_real_var ('character') Name of the variable/column containing realised LGD.
#' @param grade_var ('character') Name of the discrete variable indicating the grade/pools.
#'
#' @return tibble
#' @export
#' @examples
#' calc_pairwise_wilcox(data_df=df_lgd,lgd_pred_var=LGD_PREDICTION,
#' lgd_real_var=REAL_LGD,grade_var=LTV_BIN)

utils::globalVariables(c("KEY", "P_Value", "contains", "data", "data_left",
                         "data_right", "grade_bucket_left","grade_bucket_right"))

calc_pairwise_wilcox <- function(data_df,
                                 lgd_pred_var,
                                 lgd_real_var,
                                 grade_var
) {

  #browser()
  lgd_pred_var = rlang::as_name(rlang::enquo(lgd_pred_var))
  lgd_real_var = rlang::as_name(rlang::enquo(lgd_real_var))
  grade_var = rlang::as_name(rlang::enquo(grade_var))

  if (any(is.na(lgd_pred_var))) stop(paste("NAs in ", lgd_real_var))
  if (any(is.na(lgd_real_var))) stop(paste("NAs in ", lgd_pred_var))
  if (any(is.na(data_df[[grade_var]]))) stop(paste("NAs in ", grade_var))

  data_df <- data_df %>%
    dplyr::mutate(lgd_real=as.numeric(!!rlang::sym(lgd_real_var)),
                  lgd_pred=as.numeric(!!rlang::sym(lgd_pred_var)),
                  grade_bucket=factor(!!rlang::sym(grade_var))
  )

  wilxi_p <- function(x){
    p <- stats::wilcox.test(lgd_real ~ grade_bucket, data = x, paired = FALSE)$p.value
    return(p)
  }

  tmp1l <- data_df %>%
    dplyr::select(lgd_real, grade_bucket) %>%
    dplyr::group_by(grade_bucket) %>%
    tidyr::nest() %>%
    dplyr::rename(data_left = data, grade_bucket_left = grade_bucket)
  tmp1r <- data_df %>%
    dplyr::select(lgd_real, grade_bucket) %>%
    dplyr::group_by(grade_bucket) %>%
    tidyr::nest() %>%
    dplyr::rename(data_right = data, grade_bucket_right = grade_bucket)

  tmp2 <- tmp1l %>%
    dplyr::left_join(tmp1r, by = character()) %>%
    dplyr::mutate(KEY = paste(grade_bucket_left, "-", grade_bucket_right))
  tmp3l <- tmp2 %>%
    dplyr::select(-contains("right")) %>%
    dplyr::rename(data = data_left, grade_bucket = grade_bucket_left) %>%
    dplyr::mutate(grade_bucket = paste("1: ",grade_bucket))
  tmp3r <- tmp2 %>%
    dplyr::select(-contains("left")) %>%
    dplyr::rename(data = data_right, grade_bucket = grade_bucket_right) %>%
    dplyr::mutate(grade_bucket = paste("2: ",grade_bucket))

  PAIRWISE_WILCOXTEST_RESULT <- rbind(tmp3l, tmp3r) %>%
    tidyr::unnest(cols = c(data)) %>%
    dplyr::select(KEY, grade_bucket, lgd_real) %>%
    dplyr::group_by(KEY) %>%
    tidyr::nest() %>%
    dplyr::mutate(P_Value = as.numeric(purrr::map(data,~wilxi_p(.)))) %>%
    dplyr::select(-data) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(grade_bucket = sub("\\-.*", "", KEY)) %>%
    dplyr::mutate(grade_bucket_right = sub(".*\\-", "", KEY)) %>%
    dplyr::select(-KEY) %>%
    tidyr::pivot_wider(names_from = grade_bucket_right, values_from = P_Value)

  return(PAIRWISE_WILCOXTEST_RESULT)
}
