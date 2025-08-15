# ---
# repo: ai4c1/ggoutbreak
# file: standalone-memoise.R
# last-updated: '2025-08-15'
# license: https://unlicense.org
# imports: rlang
# ---

.memoise_cache = rlang::new_environment(
  data = list(fns = list(), memoised = list())
)


#' Memoise a function with an in memory cache
#'
#' @param fn a self contained function. This cannot reference anything outside
#'   of its own parameters, and should be deterministic.
#'
#' @returns a function that takes the
#' @noRd
#'
#' @examples
#' lmmem = .memoise(stats::lm)
#' lmmem2 = .memoise(stats::lm)
#' identical(lmmem, lmmem2)
#'
#' result1 = lmmem(Petal.Width ~ Petal.Length, iris)
#' result2 = lmmem2(Petal.Width ~ Petal.Length, iris)
#'
#' # attr(result1,"cache")
#' # attr(result2,"cache")
#'
#' result2
#' identical(result1,result2)
#'
#' # memoise a purrr style lambda:
#' plus = .memoise(~ .x + .y)
#'
#' tmp = .memoise(function(x,y) {z})
.memoise = function(fn) {
  # have we memoised this function before?
  fn = rlang::as_function(fn)
  matches = sapply(.memoise_cache$fns, identical, fn)
  if (any(matches)) {
    return(.memoise_cache$memoised[matches][[1]])
  }
  # no? lets define a memoised function generator just for this function.

  # first lets check it is self contained:
  maybe_globals = setdiff(all.vars(rlang::fn_body(fn)), names(formals(fn)))
  if (length(maybe_globals) > 0) {
    maybe_globals = paste0(maybe_globals, collapse = ",")
    rlang::warn(
      sprintf(
        "memoised function is not self contained and references global: %s",
        maybe_globals
      ),
      .internal = TRUE,
      .frequency = "once",
      .frequency_id = sprintf("memoise %s", maybe_globals)
    )
  }

  mem_fn_fn = function() {
    fn = fn
    cache = rlang::new_environment(data = list(items = list()))
    # The parent of the global environment is the package search stack. We
    # wont have to fully qualify package functions, but no data will be available.
    exec_env = rlang::env_parent(rlang::global_env())
    rlang::fn_env(fn) <- exec_env
    return(function(...) {
      dots = rlang::list2(...)
      hsh = rlang::hash(dots)
      cached = cache$items[[hsh]]
      if (!is.null(cached)) {
        if (identical(cached, "!!NULL!!")) {
          return(NULL)
        }
        return(cached)
      }
      result = try(fn(...), silent = TRUE)
      if (is.null(result)) {
        cache$items[[hsh]] = "!!NULL!!"
        # structure("!!NULL!!", cache = "hit")
      } else {
        cache$items[[hsh]] = result
        # structure(result, cache = "hit")
      }
      # return(structure(result, cache = "miss"))
      return(result)
    })
  }
  mem_fn = mem_fn_fn()
  .memoise_cache$fns[[length(.memoise_cache$fns) + 1]] = fn
  .memoise_cache$memoised[[length(.memoise_cache$memoised) + 1]] = mem_fn
  return(mem_fn)
}

#' Unmemoise a function and drop its cache
#'
#' @param fn a self contained function. This cannot reference anything outside
#'   of its own parameters, and should be deterministic.
#'
#' @returns TRUE if function was found FALSE otherwise
#' @noRd
#'
#' @examples
#' lmmem = .memoise(stats::lm)
#' .unmemoise(stats::lm)
.unmemoise = function(fn) {
  # have we memoised this function before?
  fn = rlang::as_function(fn)
  matches = sapply(.memoise_cache$fns, identical, fn)
  if (any(matches)) {
    .memoise_cache$memoised = .memoise_cache$memoised[!matches]
    .memoise_cache$fns = .memoise_cache$fns[!matches]
    return(TRUE)
  } else {
    return(FALSE)
  }
}
