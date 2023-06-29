## code to prepare `DATASET` dataset goes here

df_lgd <- tibble::tibble(REAL_LGD = stats::runif(n = 500, min = 0, max = 1)) %>%
  dplyr::mutate(
    LGD_PREDICTION = dplyr::case_when(REAL_LGD < 0.5 ~ 0.25, T~0.75),
    LTV = dplyr::case_when(REAL_LGD <= 0.5 ~ stats::runif(n(), min = 0.5, max = 1),
                           data.table::between(REAL_LGD, 0.5,1) ~ stats::runif(n(), min = 0, max = 0.5),
                           T ~ 0)
  ) %>%
  dplyr::mutate(LTV_BIN = santoku::chop(LTV, breaks = c(0.25,0.5,0.75)))

usethis::use_data(df_lgd, overwrite = TRUE)
