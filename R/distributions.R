#-------------------------------------------------------------------------------
# Distributions not in base R
#-------------------------------------------------------------------------------

# Dirichlet --------------------------------------------------------
rdirichlet <- function(n, alpha) {
  if (any(alpha <= 0)) {
    stop("Dirichlet concentration parameter alpha must be > 0.")
  }

  if (n == 1) {
    y <- purrr::map_dbl(alpha, ~ rgamma(n, shape = .x, scale = 1))
    return(y / sum(y))
  } else {
    return(
      purrr::map(alpha, ~ rgamma(
        n, shape = .x, scale = 1
      )) %>%
        do.call(what = cbind, args = .) %>%
        sweep(
          MARGIN = 1,
          STATS = rowSums(.),
          FUN = `/`
        )
    )
  }
}

ddirichlet <- function(x, alpha, log = FALSE) {
  # currently assumes alpha must be a vector, but we may have multiple realizations
  # of the dirichlet distribution here at which to evaluate the density
  if (is.vector(x)) {
    if (length(x) != length(alpha)) {
      stop("x must have the same dimension of alpha to evaluate dirichlet density.")
    }
  } else if (is.matrix(x)) {
    if (ncol(x) != length(alpha)) {
      stop("x must have the same dimension of alpha to evaluate dirichlet density.")
    }
  }

  if (any(x <= 0)) {
    stop("Dirichlet distribution only valid on positive support.")
  }
  if (any(alpha <= 0)) {
    stop("Dirichlet concentration parameter alpha must be > 0.")
  }


  if (is.vector(x)) {
    # scale so that x sums to 1
    x <- x / sum(x)

    norm_const <- base::lgamma(sum(alpha)) - sum(base::lgamma(alpha))

    ldensity <- norm_const + sum((alpha - 1) * log(x))
    if (log) {
      return(ldensity)
    } else {
      return(exp(ldensity))
    }

  } else {
    x <- sweep(x,
               MARGIN = 1,
               STATS = rowSums(x),
               FUN = `/`)
    ldensity <- purrr::array_branch(x, 1) %>%
      purrr::map_dbl(function(.x, .alpha) {
        norm_const <- base::lgamma(sum(.alpha)) - sum(base::lgamma(.alpha))
        return(norm_const + sum((.alpha - 1) * log(.x)))
      }, .alpha = alpha)

    if (log) {
      return(ldensity)
    } else {
      return(exp(ldensity))
    }
  }
}
