#' Retail Construction Missing Treatment and Grades
#'
#' @name fct_rc_grades
#'
#' @description The function adds the Retail Construction LGD Model's buckets and grades to a dataframe containing a COLL_QUOTA variable and a SEASON_REL variable
#'
#' @param data_df ('tbl') Dataframe containing a COLL_QUOTA variable and a SEASON_REL variable
#' @param coll_quota_var ('character') Name of the variable/column containing the relative SEASON.
#' @param coll_quota_var ('character') Name of the variable/column containing the collateralization quota.
#'
#' @return ('tibble') The output table includes the SEASON Buckets (SEASON_BU), COLL_QUOTA Buckets (COLL_QUOTA_BU), and the GRADES (GRADES) variable
#' @export

utils::globalVariables(c("SEASON_REL.tmp", "COLL_QUOTA.tmp", "GRADES.tmp","COLL_QUOTA_BU", "SEASON_BU", "GRADES", "COLL_QUOTA", "SEASON_REL"))

fct_rc_grades <- function(data_df, coll_quota_var = COLL_QUOTA, season_var = SEASON_REL){

  cuts_coll_quot <- c(0.00, 0.00, 0.10, 0.20, 0.30, 0.40, 0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.90, 0.95, 1.00, 1.10, 1.20, 1.50, 2.00,  Inf)
  cuts_season <- c(-Inf, 0.25, 0.75,  Inf)

  o <- data_df %>%
    dplyr::as_tibble() %>%
    #missing treatment
    dplyr::mutate(COLL_QUOTA.tmp = dplyr::coalesce(!!rlang::sym(rlang::as_name(rlang::enquo(coll_quota_var))),0),
                  SEASON_REL.tmp = dplyr::coalesce(!!rlang::sym(rlang::as_name(rlang::enquo(season_var))),0)) %>%
    dplyr::mutate(GRADES.tmp = as.character(santoku::chop(COLL_QUOTA.tmp, breaks=cuts_coll_quot))) %>%
    #coll quota buckets
    dplyr::mutate(COLL_QUOTA_BU = santoku::chop(COLL_QUOTA.tmp, breaks=cuts_coll_quot) %>% factor(ordered = T)) %>%
    #season buckets
    dplyr::mutate(SEASON_BU = santoku::chop(SEASON_REL.tmp, breaks=cuts_season) %>% factor(ordered = T)) %>%
    dplyr::mutate(SEASON_BU = dplyr::case_when(COLL_QUOTA.tmp == 0 ~ SEASON_BU, T ~ factor(NA, ordered = T))) %>%
    #GRADES variable
    dplyr::mutate(GRADES = factor(dplyr::case_when(COLL_QUOTA.tmp == 0 ~ paste("CQ:[0] SEASON:", as.character(santoku::chop(SEASON_REL.tmp, breaks=cuts_season))) , T ~ paste("CQ:",GRADES.tmp))))

  return(o)

}
