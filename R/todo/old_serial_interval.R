## Serial Interval ----

#' Generate a set of serial interval distributions using EpiEstim
#'
#' @param mean_of_mean the mean of the si mean posteriors
#' @param sd_of_mean the sd of the si mean posteriors
#' @param mean_of_sd the mean of the si sd posteriors
#' @param sd_of_sd the sd of the si sd posteriors
#' @param days length of desired SI distribution
#' @param boots number of bootstraps to generate
#' @keywords internal
#'
#' @return A dataframe containing the following columns:
#'
#' * boot (anything) - a bootstrap identifier
#' * time (positive_double) - the end of the time period (in days)
#' * probability (proportion) - the probability of infection between previous time period until `time`
#'
#' Grouped by: boot (exactly).
#'
#' @examples
#' # N.B. EpiEstim cannot reconstruct an accurate set of distributions that
#' # match the inputs parameters due to truncation of the SI distribution.
#'
#' if (interactive()) {
#'
#' ggplot2::ggplot(
#'     .epiestim_si(5,1,3,1,boots = 100, days = 14),
#'     ggplot2::aes(x=time,ymin=probability,ymax=probability,group=time)
#'   )+ggplot2::geom_errorbar(alpha=0.2)
#'
#' .epiestim_si(5,1,3,1,boots = 1000, days = 14) %>%
#'   dplyr::summarise(
#'     E = sum(probability*time),
#'     E2 = sum(probability*time^2)
#'   ) %>%
#'   dplyr::summarise(
#'     mean_of_mean = mean(E), sd_of_mean = stats::sd(E),
#'     mean_of_sd = mean(sqrt(E2 - E^2)),
#'     sd_of_sd = stats::sd(sqrt(E2 - E^2))
#'   )
#'
#' }
#'
#' # This becomes less of an issue with larger SI days parameter.
.epiestim_si = function(
    mean_of_mean, sd_of_mean, mean_of_sd, sd_of_sd,
    days = 14, boots = 100, seed = Sys.time()
) {
  withr::with_seed(seed, {
    mean = stats::rlnorm(boots,
                         meanlog = log(mean_of_mean/ sqrt(sd_of_mean^2/mean_of_mean^2+1)),
                         sdlog = sqrt(log(sd_of_mean^2/mean_of_mean^2+1))
    )
    sd = stats::rgamma(boots,
                       shape = mean_of_sd^2/sd_of_sd^2,
                       scale = sd_of_sd^2/mean_of_sd # this should maybe hard limited to 2 - as SD is chisq distributed and chisq is gamma with scale 2 (or rate 0.5).
    )
    return(
      lapply(1:boots, function(b) {
        tibble::tibble(
          boot = b,
          probability = EpiEstim::discr_si(1:days, mean[b], sd[b]),
          time = 1:days
        ) %>% dplyr::mutate(
          probability = ifelse(time==days, 1-sum(probability[1:(days-1)]), probability)
        )
      }) %>%
        dplyr::bind_rows() %>%
        dplyr::group_by(boot)
    )
  })
}
