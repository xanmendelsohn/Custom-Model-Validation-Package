#' Somers' D and Kendall's Tau B with conf. Intervalls
#'
#' @name calc_somersd_stats
#'
#' @description This is a function which calculates the Somers' D and Kendall's Tau B statistic efficiently with bootstrapped confidence intervals. 500 bootstrap iterations are performed and the 80% confidence level is used by default.
#'
#' @param x ('vector') Vector or trivial dataframe containing realised LGD.
#' @param y ('vector') Vector or trivial dataframe containing predicted LGD.
#' @param conf_level ('numeric') confidence level as double.
#' @param num_boot_samples ('numeric') number of iterations used in bootstrap estimation of confidence bounds.
#'
#' @return list
#' @export
#' @examples
#' calc_somersd_stats(x = c(0.1,0.2,0.3,0.4,0.5,0.6), y= c(0.2,0.2,0.2,0.2,0.3,0.3))

utils::globalVariables(c("n", "fac1", "fac2"))

calc_somersd_stats <- function(x, y, conf_level = 0.80, num_boot_samples = 500, return_boot_sample = F) {

  #Remove NAs
  filter <- is.na(x) | is.na(y)
  x <- round(x[!filter],6)
  y <- round(y[!filter],6)

  calc_tau_b <- function(x, y) {
    #Remove NAs
    filter <- is.na(x) | is.na(y)
    x <- x[!filter]
    y <- y[!filter]
    #Kendall's Tau A
    t_xy <- pcaPP::cor.fk(x, y)
    t_xx <- pcaPP::cor.fk(x, x)

    return ((t_xy/t_xx))
  }

  corr_factor <- sqrt((tibble::tibble(x) %>% dplyr::count(x) %>%
                         dplyr::summarize(fac1 = sum(n)*(sum(n)-1) -sum(n*(n-1))) %>%
                         dplyr::pull(fac1))/ (tibble::tibble(y) %>% dplyr::count(y) %>%
                         dplyr::summarize(fac2 = sum(n)*(sum(n)-1) -sum(n*(n-1))) %>%
                         dplyr::pull(fac2)))

  tau_b <- calc_tau_b(x,y)
  somersd <- tau_b/corr_factor

  if (num_boot_samples == 0) {

    return(list(tau_b = tau_b,
                somersd = somersd))
  }

  tau_b_func <- function(data, indices) {
    d <- data[indices,] # allows boot to select sample
    return(calc_tau_b(d[,1], d[,2]))
  }

  boot_result <- boot::boot(data=data.frame(x,y), statistic=tau_b_func,
                      R=num_boot_samples)

  ci <- boot::boot.ci(boot_result, conf=conf_level, type="norm")

  if (return_boot_sample) {
    boot_sample <- boot_result$t
  } else {
    boot_sample <- NULL
  }


  SUMMARY_SOMERSD <- data.frame(SomersD = somersd,
                                CI_LEVEL = ci$normal[1],
                                CI_LOWER = ci$normal[2]/corr_factor,
                                CI_UPPER = ci$normal[3]/corr_factor)

  SUMMARY_TAU_B <- data.frame(KendallsTauB = tau_b,
                              CI_LEVEL = ci$normal[1],
                              CI_LOWER = ci$normal[2],
                              CI_UPPER = ci$normal[3])

  return(list(
    tau_b = tau_b,
    somersd = somersd,
    SUMMARY_TAU_B = SUMMARY_TAU_B,
    SUMMARY_SOMERSD = SUMMARY_SOMERSD,
    boot_sample = boot_sample
  ))

}

