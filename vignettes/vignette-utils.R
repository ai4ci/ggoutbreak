# Set general theme
ggplot2::theme_set(ggplot2::theme_bw(base_size = 8)+
   ggplot2::theme(
     axis.text.x.bottom = ggplot2::element_text(angle=30,vjust = 1,hjust=1),
     axis.text.x.top = ggplot2::element_text(angle=30,vjust = 0,hjust=0),
     legend.key.size = ggplot2::unit(0.1,"inches")
   )
)


# .test_data()
# ggplot2::ggplot(.test_data(),ggplot2::aes(x=date,y=count))+ggplot2::geom_point()+ggplot2::scale_y_continuous(trans="log1p")
.test_data = function(
    changes = tibble::tibble(time = c(0,20,40,60,80), r = c(0.1,0,-0.1,0,0.1)),
    initial = 100,
    duration = 104,
    seed=100
) {
  set.seed(seed)
  tibble::tibble(time = 0:duration) %>%
    dplyr::left_join(changes, by="time") %>%
    tidyr::fill(r) %>%
    dplyr::mutate(rate = initial*exp(cumsum(r))) %>%
    dplyr::mutate(
      count = stats::rpois(dplyr::n(),rate),
      denom = max(count)*2,
      time = as.time_period(time, unit = "7 days",start_date = "2020-01-01")
    )
}

# .test_multinomial()
.test_multinomial = function(
    changes = tibble::tibble(
      time = c(0,20,40,60,80),
      variant1 = c(0.1,0,-0.1,0,0.1),
      variant2 = c(0.15,0.05,-0.05,-0.01,0.05),
      variant3 = c(0,0.05,-0.05,+0.05,-0.05),
    ), initial=c(100,1,100), ...) {
  cols = setdiff(colnames(changes),"time")
  out = tibble::tibble()
  i = 1
  for (col in cols) {
    out = dplyr::bind_rows(
      out,
      .test_data(changes = changes %>% dplyr::select(time, r=!!col), initial=initial[[i]], ...) %>%
        dplyr::mutate(class = col, time=as.numeric(time)) %>% tibble::as_tibble()
    )
  }

  out = out %>%
    dplyr::mutate(time = as.time_period(time, unit = "7 days",start_date = "2020-01-01")) %>%
    dplyr::group_by(time) %>%
    # The relative growth rate of one variant is the weighted average growth rate of the other variants (excluding current one)
    dplyr::mutate(
      proportion = rate/sum(rate),
      proportion.obs = count/sum(count),
      relative.r = r - (sum(r*rate)-r*rate)/(sum(rate)-rate)
    ) %>%
    dplyr::ungroup()

  return(out)
}


.multinomial_waves = function(profile, waves=5, delay=20) {
  sim_len = length(profile+(waves-1)*delay)
  times = 1:sim_len
  tibble::tibble(
    time = as.time_period(times, unit = "7 days",start_date = "2020-01-01")
  )
}


.download = function(url,filename) {
  download_dir = rappdirs::user_cache_dir("ggoutbreak")
  if (!fs::dir_exists(download_dir)) fs::dir_create(download_dir)
  raw_lineages = fs::path(download_dir, filename)
  if (fs::file_exists(raw_lineages)) {
    tmp = fs::file_info(raw_lineages)
    if (tmp$change_time < as.POSIXct(Sys.Date()-7)) unlink(raw_lineages)
  }
  if (!fs::file_exists(raw_lineages)) {
    downloader::download(url,raw_lineages)
  }
  return(raw_lineages)
}
