
# setup package internal pin board function ----
.pin_board = .singleton({
  folder = system.file("pkgdown/assets/data/", package = "ggoutbreak")
  if (dir.exists(folder)) {
    pins::board_folder(folder)
  } else {
    pins::board_url("https://ai4ci.github.io/ggoutbreak/data/")
  }
})

# package external data access functions ----


#' @export
england_covid_poisson_age_stratified = function() {pins::pin_read(.pin_board(),"england_covid_poisson_age_stratified")}


#' @export
england_covid_proportion_age_stratified = function() {pins::pin_read(.pin_board(),"england_covid_proportion_age_stratified")}


#' @export
test_poisson_rt = function() {pins::pin_read(.pin_board(),"test_poisson_rt")}
