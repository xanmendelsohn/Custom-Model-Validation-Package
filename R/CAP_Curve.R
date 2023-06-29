#' Cumulative Accuracy Profile Plot
#'
#' @name CAP_Curve
#'
#' @description This is a function which plots Cumulative Accuracy Profile for given realisations and estimates of a continuous variable.
#'
#' @param sample ('tbl') Dataframe containing variable and realised LGD.
#' @param var_target ('character') Name of variable containing realised LGD.
#' @param var_pred ('character') Name of variable containing predicted LGD or risk driver.
#' @param title ('character') Title displayed above plot after "CAP Curve".
#' @param file ('character') Folder path to save output image under the name given in title.
#'
#' @return CAP Curve image
#' @export
#' @examples
#' df <- tibble::tibble(x = c(0.1,0.2,0.3,0.4,0.5,0.6), y= c(0.2,0.2,0.2,0.2,0.3,0.3))
#' CAP_Curve(df, y, x, title="Toets")

utils::globalVariables(c("X", "Y", "n", "l", "Y_tmp", "desc", "x", "y", "name", "value", "variable"))

CAP_Curve <- function(sample, var_pred, var_target, title, file=NA_character_){

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

  var_pred = rlang::as_name(rlang::enquo(var_pred))
  var_target = rlang::as_name(rlang::enquo(var_target))

  sample <- sample %>%
    tibble::as_tibble() %>%
    dplyr::select(!! var_pred, !! var_target) %>%
    dplyr::rename(X = !! var_pred) %>%
    dplyr::rename(Y = !! var_target) %>%
    dplyr::mutate(X=round(X,6), Y=round(Y,6)) %>%
    tidyr::drop_na()

  tau_b <- calc_tau_b(sample %>% dplyr::select(X),
                      sample %>% dplyr::select(Y))

  tau_b <- round(tau_b, 3)

  total <- sum(sample$Y)
  nrow <- nrow(sample)

  PERFECT_CURVE_tmp <- sample %>%
    dplyr::mutate(l=1) %>%
    dplyr::group_by(Y) %>%
    dplyr::summarise(Y_tmp = sum(Y), n=sum(l)) %>%
    dplyr::select(Y, n, Y_tmp)

  PERFECT_CURVE <- PERFECT_CURVE_tmp %>%
    dplyr::arrange(dplyr::desc(Y)) %>%
    dplyr::mutate(x = as.numeric(cumsum(n)/nrow), y = as.numeric(cumsum(Y_tmp)/total))%>%
    dplyr::select(x, y) %>%
    dplyr::add_row(x = 0, y = 0) %>%
    dplyr::mutate(variable= "perfect model")

  VAR_CURVE_tmp <- sample %>%
    dplyr::mutate(l=1) %>%
    dplyr::group_by(X) %>%
    dplyr::summarise(Y_tmp = sum(Y), n=sum(l)) %>%
    dplyr::select(X, n, Y_tmp)

  VAR_CURVE <- VAR_CURVE_tmp %>%
    dplyr::arrange(dplyr::desc(X)) %>%
    dplyr::mutate(x = as.numeric(cumsum(n)/nrow), y = as.numeric(cumsum(Y_tmp)/total)) %>%
    dplyr::select(x, y) %>%
    dplyr::add_row(x = 0, y = 0) %>%
    dplyr::mutate(variable= "predictor")

  RANDOM_CURVE <- tibble::enframe(c(0,1)) %>%
    dplyr::mutate(x=name-1, y=value, variable="random model") %>%
    dplyr::select(x,y, variable)

  plot_data <- rbind(PERFECT_CURVE, VAR_CURVE, RANDOM_CURVE)

  if (!missing(file)) {
    chart_title <- substitute(paste("Kendall's Tau B = ", tau_b, sep=" "))

    ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_line(ggplot2::aes(color = variable, linetype = variable)) +
      ggplot2::ylab("cumulative proportion of total") +
      ggplot2::xlab("observations ordered by variable") +
      ggplot2::ggtitle(chart_title)

    image <- paste(file,title,".png", sep = "")
    ggplot2::ggsave(image)

  } else {
    chart_title <- substitute(paste("Kendall's Tau B = ", tau_b, " \n ",title, sep=" "))

    ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_line(ggplot2::aes(color = variable, linetype = variable)) +
      ggplot2::ylab("cumulative proportion of total") +
      ggplot2::xlab("observations ordered by variable") +
      ggplot2::ggtitle(chart_title)

  }

}
