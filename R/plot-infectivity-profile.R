

#' Plot an infectivity profile
#'
#' @iparam ip A long format infectivity profile
#' @param alpha the alpha value of the bootstrap lines
#' @param ... passed onto geom_segment for controlling line thickness, alpha etc.
#'
#' @return an ggplot object
#' @export
#' @concept vis
#'
#' @examples
#' if(interactive()) {
#'   plot_ip(ganyani_ip,alpha=0.1)
#' }
plot_ip = function(ip = i_empirical_ip, alpha=0.1, ...) {

  ip = interfacer::ivalidate(ip, .imap = interfacer::imapper(a0 = pmax(tau-0.5,0), a1=tau+0.5))

  ip_summ = ip %>% summarise_ip()


  ggplot2::ggplot(ip)+
    ggplot2::geom_segment(mapping=ggplot2::aes(x=a0,xend=a1,y=probability,yend=probability, group=boot), alpha=alpha, ...)+
    ggplot2::geom_segment(data=ip_summ, mapping=ggplot2::aes(x=a0,xend=a1,y=probability,yend=probability), colour="blue")+
    ggplot2::geom_segment(data=ip_summ, mapping=ggplot2::aes(x=a0,xend=a0,y=dplyr::lag(probability,default=0),yend=probability), colour="blue")+
    {if (min(ip_summ$a0) < 0) ggplot2::geom_vline(xintercept=0,colour="grey20") else NULL}+
    ggplot2::scale_x_continuous(breaks=integer_breaks(...))+
    ggplot2::xlab("tau (days)")
}
