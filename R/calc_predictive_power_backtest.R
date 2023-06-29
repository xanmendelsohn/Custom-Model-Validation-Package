#' Predictive Power Test (T-Test on given level)
#'
#' @name calc_predictive_power_backtest
#'
#' @description This function is designed to compare the realization and predictions
#' of a continuous variable (LGD) on a given level (overall, pools or bins).
#' "P_CONS" (the p-value of the conservativity test) corresponds to the alternative hypothesis the real value is larger than the predicted value.
#' "P_CONS_TOL" corresponds to the same test as "P_CONS" but after shifting the prediction up by the tolerance level (e.g. 2.5%).
#' "P_PROG" (the p-value of the progressiveness test) corresponds to the alternative hypothesis the real value is larger than the predicted value.
#' "P_PROG_TOL" corresponds to the same test as "P_PROG" but after shifting the prediction down by the tolerance level (e.g. 2.5%).
#'
#' @param data_df ('tbl') Dataframe containing all variables listed below.
#' @param lgd_pred_var (`character`) Name of the variable/column containing model predictions.
#' @param lgd_real_var (`character`) Name of the variable/column containing realized LGD.
#' @param grade_var (`character`) Name of the discrete variable indicating the grade/pools.
#' @param tol ('numeric') tolerance level applied to t-test.
#'
#' @return tibble
#' @export
#' @examples
#' calc_predictive_power_backtest(df_lgd,lgd_pred_var=LGD_PREDICTION,lgd_real_var=REAL_LGD)

utils::globalVariables(c("lgd_real", "lgd_pred", "grade_bucket",
                         "MEAN_LGD_PRED", "MEAN_LGD_REAL", "N", "T", "STD_ERR_REAL_DIFF",
                         "P_CONS", "P_CONS_TOL", "P_PROG", "P_PROG_TOL", "DT", "T_cons_tol",
                         "T_prog_tol", "BT_indicator", "Backtesting"))

calc_predictive_power_backtest <- function(data_df,
                                           lgd_pred_var,
                                           lgd_real_var,
                                           grade_var=NaN,
                                           tol = 0.025) {
  #browser()
  if(!missing(grade_var)) {

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

    TOO_FEW_OBERVATIONS <- data_df %>%
      dplyr::group_by(grade_bucket) %>%
      dplyr::summarise(N = n()) %>%
      dplyr::filter(N<10)
    if(nrow(TOO_FEW_OBERVATIONS)>1){
      data_df <- data_df %>%
        dplyr::left_join(TOO_FEW_OBERVATIONS) %>%
        dplyr::filter(is.na(N)) %>%
        dplyr::select(-N)

      result <- TOO_FEW_OBERVATIONS
    }

    PRED_POWER_BACKTEST <- data_df %>%
      dplyr::group_by(grade_bucket) %>%
      dplyr::summarise(MEAN_LGD_PRED = mean(lgd_pred, na.rm =TRUE),
                MEAN_LGD_REAL = mean(lgd_real, na.rm =TRUE),
                N = dplyr::n(),
                T = stats::t.test(lgd_real,lgd_pred)$statistic,
                STD_ERR_REAL_DIFF = stats::t.test(lgd_real,lgd_pred)$stderr,
                P_CONS = stats::t.test(lgd_real,lgd_pred,alternative = 'greater')$p.value,
                P_CONS_TOL = stats::t.test(lgd_real,lgd_pred+tol,alternative = 'greater')$p.value,
                P_PROG = stats::t.test(lgd_real,lgd_pred,alternative = 'less')$p.value,
                P_PROG_TOL = stats::t.test(lgd_real,lgd_pred-tol,alternative = 'less')$p.value) %>%
      dplyr::mutate(DT=tol/STD_ERR_REAL_DIFF,
             T_cons_tol = T - DT,
             T_prog_tol = T + DT) %>%
      dplyr::mutate(BT_indicator= dplyr::case_when(T==''~ 0,
                                     P_CONS_TOL<0.05~-2,
                                     P_CONS<0.05~-1,
                                     P_PROG_TOL<0.05~2,
                                     P_PROG<0.05~1,
                                     TRUE ~ 0),
                    Backtesting= dplyr::case_when(T==''~ 'o',
                                    P_CONS_TOL<0.05~'--',
                                    P_CONS<0.05~'-',
                                    P_PROG_TOL<0.05~'++',
                                    P_PROG<0.05~'+',
                                    TRUE ~ 'o'))

    result <- PRED_POWER_BACKTEST

  } else {

    lgd_pred_var = rlang::as_name(rlang::enquo(lgd_pred_var))
    lgd_real_var = rlang::as_name(rlang::enquo(lgd_real_var))

    if (any(is.na(lgd_pred_var))) stop(paste("NAs in ", lgd_real_var))
    if (any(is.na(lgd_real_var))) stop(paste("NAs in ", lgd_pred_var))

    data_df <- data_df %>%
      dplyr::mutate(lgd_real=as.numeric(!!rlang::sym(lgd_real_var)),
                    lgd_pred=as.numeric(!!rlang::sym(lgd_pred_var))
    )

    PRED_POWER_BACKTEST <- data_df %>%
      dplyr::summarise(MEAN_LGD_PRED = mean(lgd_pred, na.rm =TRUE),
                MEAN_LGD_REAL = mean(lgd_real, na.rm =TRUE),
                N = dplyr::n(),
                T = stats::t.test(lgd_real,lgd_pred)$statistic,
                STD_ERR_REAL_DIFF = stats::t.test(lgd_real,lgd_pred)$stderr,
                P_CONS = stats::t.test(lgd_real,lgd_pred,alternative = 'greater')$p.value,
                P_CONS_TOL = stats::t.test(lgd_real,lgd_pred+tol,alternative = 'greater')$p.value,
                P_PROG = stats::t.test(lgd_real,lgd_pred,alternative = 'less')$p.value,
                P_PROG_TOL = stats::t.test(lgd_real,lgd_pred-tol,alternative = 'less')$p.value) %>%
      dplyr::mutate(DT=tol/STD_ERR_REAL_DIFF,
             T_cons_tol = T - DT,
             T_prog_tol = T + DT) %>%
      dplyr::mutate(BT_indicator= dplyr::case_when(T==''~ 0,
                                     P_CONS_TOL<0.05~-2,
                                     P_CONS<0.05~-1,
                                     P_PROG_TOL<0.05~2,
                                     P_PROG<0.05~1,
                                     TRUE ~ 0),
                    Backtesting= dplyr::case_when(T==''~ 'o',
                                    P_CONS_TOL<0.05~'--',
                                    P_CONS<0.05~'-',
                                    P_PROG_TOL<0.05~'++',
                                    P_PROG<0.05~'+',
                                    TRUE ~ 'o'))

    result <- PRED_POWER_BACKTEST

  }

  return(result)
}
