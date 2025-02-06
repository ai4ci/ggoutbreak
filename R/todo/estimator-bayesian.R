# alphaPrior = 1
# betaPrior = 2
# tau = 6
#
# testEst4 = testTs %>%
#   dplyr::group_by(subgroup) %>%
#   dplyr::mutate(
#     alpha1 = slider::slide_dbl(value, sum, .after = 2*tau, .complete = TRUE ) + alphaPrior,
#     alpha2 = slider::slide_dbl(value, sum, .before = 2*tau, .complete = TRUE ) + alphaPrior,
#     alpha0 = slider::slide_dbl(value, sum, .before = tau, .after=tau, .complete = TRUE ) + alphaPrior,
#     beta0 = 2*tau+1+betaPrior,
#     betap_mean = alpha1/(alpha2-1),
#     betap_variance = alpha1*(alpha1+alpha2-1)/((alpha2-2)*(alpha2-1)^2),
#     nb_size = alpha0,
#     nb_prob = beta0 / (beta0+1),
#     # https://stats.stackexchange.com/questions/57715/expected-value-and-variance-of-loga
#     # taylor exansion for log
#     Growth.mean = 1/(2*tau)*(log(betap_mean) - betap_variance/(2*betap_mean^2)),
#     Growth.variance = 1/(2*tau)*(betap_variance/(betap_mean^2))
#   ) %>%
#   dplyr::mutate(
#     Growth.quantile.0.025 = 1/(2*tau)*log(extraDistr::qbetapr(p=0.025, alpha1, alpha2)),
#     Growth.quantile.0.05 = 1/(2*tau)*log(extraDistr::qbetapr(p=0.05, alpha1, alpha2)),
#     Growth.quantile.0.25 = 1/(2*tau)*log(extraDistr::qbetapr(p=0.25, alpha1, alpha2)),
#     Growth.quantile.0.5 = 1/(2*tau)*log(extraDistr::qbetapr(p=0.5, alpha1, alpha2)),
#     Growth.quantile.0.75 = 1/(2*tau)*log(extraDistr::qbetapr(p=0.75, alpha1, alpha2)),
#     Growth.quantile.0.95 = 1/(2*tau)*log(extraDistr::qbetapr(p=0.95, alpha1, alpha2)),
#     Growth.quantile.0.975 = 1/(2*tau)*log(extraDistr::qbetapr(p=0.975, alpha1, alpha2))
#   ) %>%
#   dplyr::mutate(
#     Est.mean = alpha0/beta0,
#     Est.variance = alpha0/(beta0^2)*(2*tau+2+beta0),
#     Est.quantile.0.025 = stats::qnbinom(p=0.025, nb_size, nb_prob),
#     Est.quantile.0.05 = stats::qnbinom(p=0.05, nb_size, nb_prob),
#     Est.quantile.0.25 = stats::qnbinom(p=0.25, nb_size, nb_prob),
#     Est.quantile.0.5 = stats::qnbinom(p=0.5, nb_size, nb_prob),
#     Est.quantile.0.75 = stats::qnbinom(p=0.75, nb_size, nb_prob),
#     Est.quantile.0.95 = stats::qnbinom(p=0.95, nb_size, nb_prob),
#     Est.quantile.0.975 = stats::qnbinom(p=0.975, nb_size, nb_prob),
#   )
#
# ggplot2::ggplot(testEst4 %>% dplyr::filter(), ggplot2::aes(x=date, group=subgroup))+
#   ggplot2::geom_line(aes(y=Growth.mean,colour="Beta prime"))+
#   ggplot2::geom_ribbon(aes(ymin=Growth.quantile.0.025,ymax=Growth.quantile.0.975,fill="Beta prime"),alpha=0.1)+
#   ggplot2::geom_line(aes(y=growth_rate,colour="reference"))+
#   ggplot2::coord_cartesian(ylim=c(-0.1,0.1))
#
# ggplot2::ggplot(testEst4 %>% dplyr::filter(TRUE), ggplot2::aes(x=date, group=subgroup))+
#   ggplot2::geom_ribbon(aes(ymin=Est.quantile.0.025,ymax=Est.quantile.0.975,fill="Beta prime"),alpha=0.1)+
#   ggplot2::geom_point(aes(y=value,colour="reference"))+
#   ggplot2::geom_line(aes(y=Est.mean,colour="Beta prime"))
