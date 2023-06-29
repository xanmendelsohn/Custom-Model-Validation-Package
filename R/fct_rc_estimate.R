#' Retail Construction Estimation/Calibration function
#'
#' @name fct_rc_estimate
#'
#' @description The function performs the Retail Construction LGD MOdel estimate/calibration, i.e. it performs an isotonic regression over the collateralization quota and further differentiates uncollaterlaized observations via SEASON buckets.
#'
#' @param data_df ('tbl') Dataframe containing the realized LGD, the coll. quota, a variable with coll. quota buckets and a variable with season buckets. NO MISSING VALUES
#' @param var_vec ('vector') Vector of the form c("SEASON_BU","COLL_QUOTA_BU"), i.e. first the season bucket variable thereafter the coll. quota bucket.
#' @param lgd_real_var ('character') Name of the variable/column containing realised LGDs.
#' @param coll_quota_var ('character') Name of the variable/column containing the collateralization quota.
#'
#' @return ('tibble') The output table includes the LGD estimate as LGD_PRED
#' @export

utils::globalVariables(c("Npreg", "LGD_PRED.tmp", "LGD_PRED", "LGD_PRED1", "LGD_PRED2", "REAL", "PRED", "LGD_REAL.tmp", "V1", "LGD_REAL", "COLL_QUOTA"))

fct_rc_estimate <- function(data_df, lgd_real_var = LGD_REAL, coll_quota_var = COLL_QUOTA, var_vec = c("SEASON_BU","COLL_QUOTA_BU")){

  #browser()
  lgd_real_str = rlang::as_name(rlang::enquo(lgd_real_var))
  coll_quota_str = rlang::as_name(rlang::enquo(coll_quota_var))

  var1 <- rlang::sym((var_vec[1]))
  var2 <- rlang::sym((var_vec[2]))

  #isotonic regression over coll quota
  formula2 <- stats::as.formula(paste(rlang::as_name(rlang::enquo(lgd_real_var)) , " ~ 0 + ", var_vec[2], sep=""))
  fit2 <- stats::lm(formula2, data_df)
  data_df$LGD_PRED2 <- stats::predict(fit2, data_df)

  t2.tmp <- data_df %>%
    dplyr::group_by(!!var2) %>%
    dplyr::summarise(Npreg= n(), LGD_PRED.tmp= mean(LGD_PRED2, na.rm=T )) %>%
    dplyr::ungroup()

  t2 <- t2.tmp %>%
    dplyr::mutate(LGD_PRED = Iso::pava(LGD_PRED.tmp,w=Npreg, decreasing = T))

  i2 <- data_df %>%
    dplyr::left_join(t2) %>%
    dplyr::filter(dplyr::coalesce(!!rlang::sym(coll_quota_str),0) != 0) %>%
    dplyr::select(-LGD_PRED.tmp, -Npreg, -LGD_PRED2)

  #determine adjustment factor to fix quantification level for uncollateralized observations
  UNCOLL_TARGET <- data_df %>%
    dplyr::left_join(t2) %>%
    dplyr::filter(dplyr::coalesce(!!rlang::sym(coll_quota_str),0) == 0) %>%
    dplyr::summarise(REAL = mean(!!rlang::sym(lgd_real_str),na.rm=T), PRED = mean(LGD_PRED,na.rm=T)) %>%
    t() %>%
    tibble::as_tibble() %>%
    dplyr::pull(V1)

  ADJ_FACTOR <- UNCOLL_TARGET[2]/UNCOLL_TARGET[1]

  #isotonic regression over season
  dat1 <- data_df %>%
    dplyr::filter(dplyr::coalesce(!!rlang::sym(coll_quota_str),0) == 0) %>%
    dplyr::select(-LGD_PRED2) %>%
    dplyr::mutate(LGD_REAL.tmp = !!rlang::sym(lgd_real_str)*ADJ_FACTOR)

  formula1 <- stats::as.formula(paste("LGD_REAL.tmp" , " ~ 0 + ", var_vec[1], sep=""))
  fit1 <- stats::lm(formula1, dat1)
  dat1$LGD_PRED1 <- stats::predict(fit1, dat1)

  t1.tmp <- dat1 %>%
    dplyr::group_by(!!var1) %>%
    dplyr::summarise(Npreg= n(), LGD_PRED.tmp= mean(LGD_PRED1, na.rm=T )) %>%
    dplyr::ungroup()

  t1 <- t1.tmp %>%
    dplyr::mutate(LGD_PRED = Iso::pava(LGD_PRED.tmp,w=Npreg, decreasing = T))

  i1 <- dat1 %>%
    dplyr::left_join(t1) %>%
    dplyr::select(-LGD_PRED.tmp, -Npreg, -LGD_PRED1, -LGD_REAL.tmp)

  o <- rbind(i1,i2)

  return(o)

}

