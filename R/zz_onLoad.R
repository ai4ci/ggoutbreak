# First add `run_on_load()` to your `.onLoad()` hook,
# then use `on_load()` anywhere in your package
# .onLoad <- function(lib, pkg) {
#   rlang::run_on_load()
# }

.onLoad <- function(libname, pkgname) {
  make_gamma_ip <<- .memoise(make_gamma_ip)
}
