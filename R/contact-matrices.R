# experimental / non functional

#' transition matrix is a left stochastic square matrix
#' https://en.wikipedia.org/wiki/Stochastic_matrix#:~:text=In%20mathematics%2C%20a%20stochastic%20matrix,substitution%20matrix%2C%20or%20Markov%20matrix.
#' @param m columns are input, rows are output, must be named
#' @noRd
#' @examples
#' m = matrix(c(
#'   0.5,0.4,0,
#'   0.3,0.5,0.2,
#'   0.2,0.1,0.8
#'   ), nrow=3, byrow = TRUE, dimnames =
#'   list(c("child","adult","elderly"),c("child","adult","elderly"))
#' )
#' lookup = .matrix_to_long(m)
.matrix_to_long = function(m) {
  m = apply(m, MARGIN = 2, function(x) x / sum(x))
  dplyr::tibble(
    input = .as_factor(as.vector(sapply(colnames(m), rep, dim(m)[1]))),
    probability = as.vector(m),
    output = .as_factor(rep(rownames(m), dim(m)[2]))
  ) %>%
    dplyr::filter(probability > 0)
}


#' https://en.wikipedia.org/wiki/Sinkhorn%27s_theorem
#' @param m square matrix
#' @noRd
#' @examples
#' m = matrix(c(
#'   50,100,30,
#'   20,200,30,
#'   10,100,100
#'   ), nrow=3, byrow = TRUE, dimnames =
#'   list(c("child","adult","elderly"),c("child","adult","elderly"))
#' )
#' .sinkhorn_knopp(m)
.sinkhorn_knopp = function(m, tol = sqrt(.Machine$double.eps)) {
  while (
    any(abs(apply(m, MARGIN = 1, sum) - 1) > tol) ||
      any(abs(apply(m, MARGIN = 2, sum) - 1) > tol)
  ) {
    m = apply(m, MARGIN = 1, function(x) x / sum(x))
    m = apply(m, MARGIN = 2, function(x) x / sum(x))
  }
  return(m)
}

.sample_from_matrix = function(input, m) {
  m = apply(m, MARGIN = 2, function(x) x / sum(x))
  cum_m = apply(m, MARGIN = 2, cumsum)
  to_test = cum_m[, input]
  against = stats::runif(input)
  which = apply(to_test <= against, MARGIN = 2, sum) + 1
  rownames(m)[which]
}

.sample_from_long = function(input, lookup) {
  lookup = lookup %>%
    dplyr::group_by(input) %>%
    dplyr::mutate(
      probability = probability / sum(probability),
      cum = cumsum(probability)
    )
  dplyr::tibble(
    id = seq_along(input),
    input = input,
    test = stats::runif(input)
  ) %>%
    dplyr::left_join(lookup, by = "input", relationship = "many-to-many") %>%
    dplyr::mutate(
      cum = ifelse(is.na(cum), 1, cum),
      diff = test - cum
    ) %>%
    dplyr::filter(diff < 0) %>%
    dplyr::group_by(id) %>%
    dplyr::filter(diff == max(diff)) %>%
    dplyr::pull(output)
}

# Double pareto log normal degree distribution in contact network
# very heavy heterogenous
# COMIX 1 & 2 have very heavy tail. POLYMOD did not.
# N nodes age_group partitioned
# degree distribution for each age_group from?
# sample the degree for individual to get stubs (floating edges)
# choose targets based on contact matrix

# Stochastic block model - poisson degree distribution

# Does high degree distribution in one node correlate to high degree distribution in connected nodes
# index cases randomly chosen with probability based on degree
# increased heterogenerity always increases finite sze
# k = inf~poisson; 1~geometic; secondary case dispersion for COVID-19 was 0.4
# social contact data website
