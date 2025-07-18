# Set general theme
ggplot2::theme_set(
  ggplot2::theme_bw(base_size = 8) +
    ggplot2::theme(
      axis.text.x.bottom = ggplot2::element_text(
        angle = 30,
        vjust = 1,
        hjust = 1
      ),
      axis.text.x.top = ggplot2::element_text(
        angle = 30,
        vjust = 0,
        hjust = 0,
        size = 6
      ),
      legend.key.size = ggplot2::unit(0.1, "inches")
    )
)

.download = function(url, filename) {
  download_dir = rappdirs::user_cache_dir("ggoutbreak")
  if (!fs::dir_exists(download_dir)) {
    fs::dir_create(download_dir)
  }
  raw_lineages = fs::path(download_dir, filename)
  if (fs::file_exists(raw_lineages)) {
    tmp = fs::file_info(raw_lineages)
    if (tmp$change_time < as.POSIXct(Sys.Date() - 7)) unlink(raw_lineages)
  }
  if (!fs::file_exists(raw_lineages)) {
    downloader::download(url, raw_lineages)
  }
  return(raw_lineages)
}

.no_X = function() {
  return(ggplot2::theme(
    axis.text.x.bottom = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_blank()
  ))
}

.no_Y = function() {
  return(ggplot2::theme(
    axis.text.y.left = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank()
  ))
}

# source(here::here("R/standalone-test-utils.R"))
