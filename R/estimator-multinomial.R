#' Multinomial time-series model.
#'
#' Takes a list of times, classes and counts, e.g. a COGUK variant like data set
#' with time, (multinomial) class (e.g. variant) and count being the count in
#' that time period. Fits a quadratic B-spline on time to the proportion of the
#' data using `nnet::multinom`, with approx one degree of freedom per class and per
#' `window` units of the time series.
#'
#' Additional groupings are treated as distinct proportions models.
#'
#' @iparam d A multi-class count input dataframe
#' @param ... not used
#' @param window a number of data points between knots, smaller values result in
#'   less smoothing, large value in more.
#' @param frequency the temporal density of the output estimates.
#' @param predict result a prediction. If false we return the model.
#' @param .progress show a CLI progress bar
#'
#' @return a new dataframe with `time` (as a time period), `class`, and `proportion.0.5`, or a model object
#' @export
#' @concept models
#' @examples
#'
#' data = test_poisson_rt_2class
#' tmp = data %>% ggoutbreak::multinomial_nnet_model(window=14)
#'
#' if (interactive()) {
#'   plot_multinomial(tmp, date_labels="%b %y")
#' }
#'
multinomial_nnet_model = function(d = i_multinomial_input, ..., window = 14, frequency = "1 day", predict = TRUE, .progress=interactive()) { #, output_unit="1 day") {

  env = rlang::current_env()
  if (.progress) cli::cli_progress_bar("multinomial (nnet)", total = dplyr::n_groups(d), .envir = env)

  modelled = interfacer::igroup_process(d, function(d, ..., window, frequency, predict) {

    # remove zero count time points as these crash the algorithm
    tmp2 = d %>% dplyr::group_by(time) %>% dplyr::filter(sum(count)>0) %>% dplyr::ungroup()
    tmp2 = tmp2 %>% dplyr::select(time,class,count) %>%
      tidyr::pivot_wider(names_from = class, values_from = count, values_fill = 0)

    response = tmp2 %>% dplyr::select(-dplyr::any_of(c("time"))) %>% as.matrix()
    predictor = tmp2 %>% dplyr::pull(time)
    data = tibble::tibble(prob=response,time=predictor)

    output_times = date_seq.time_period(d$time, period = frequency)
    df = .df_from_window(window, timeseries = d, classes = ncol(response))
    model = nnet::multinom(prob ~ splines::ns(time, df = df), Hess = TRUE,data = data)


    if (!predict) return(tibble::tibble(proportion = list(model)))
    new_data = tibble::tibble(time = output_times)

    #TODO: get the confidence intervals from the spline.
    # This will need to be done with resampling and a spaghetti plot.
    # browser()
    # Gets the basis model:
    # new_matrix = stats::model.matrix(~ splines::ns(time, df = df), data=new_data)
    # betahat = t(rbind(0, stats::coef(model))) # model coefficients, with explicit zero row added for reference category & transposed
    # # transform works rowwise: in another log link type thing it could just be applied I think.
    # preds = t(apply(new_matrix %*% betahat, MARGIN = 1,FUN=.softmax))
    # colnames(preds) = colnames(response)

    preds2 = stats::predict(model,newdata = new_data,type = "probs")
    probs = dplyr::bind_cols(new_data, as.data.frame(preds2))
    probs = probs %>%
      tidyr::pivot_longer(cols = -time, names_to = "class", values_to = "proportion.0.5") %>%
      dplyr::mutate(class = factor(class,levels=levels(d$class))) %>%
      dplyr::group_by(class)

    # return(interfacer::ireturn(probs, i_multinomial_proportion_model))

    if (.progress) cli::cli_progress_update(.envir = env)

    return(probs)

  })

  if (.progress) cli::cli_progress_done()

  return(modelled)


}


.softmax <- function(par){
  n.par <- length(par)
  par1 <- sort(par, decreasing = TRUE)
  Lk <- par1[1]
  for (k in 1:(n.par-1)) {
    Lk <- max(par1[k+1], Lk) + log1p(exp(-abs(par1[k+1] - Lk)))
  }
  val <- exp(par - Lk)
  return(val)
}
