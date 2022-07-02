#-------------------------------------------------------------------------------
# Helper functions for distributions not in base R
#-------------------------------------------------------------------------------

# Dirichlet --------------------------------------------------------
rdirch <- function(n, alph) {
  if(any(alph <= 0)) {
    stop("Dirichlet concentration parameter alph must be > 0.")
  }

  if(n == 1) {
    y <- purrr::map_dbl(alph, ~ rgamma(n, shape = .x, scale = 1))
    return(y/sum(y))
  } else {
    return(
      purrr::map(alph, ~ rgamma(n, shape = .x, scale = 1)) %>%
      do.call(what = cbind, args = .) %>%
      sweep(MARGIN = 1, STATS = rowSums(.), FUN = `/`)
    )
  }
}

ddirch <- function(x, alph, log = FALSE) {
  # currently assumes alph must be a vector, but we may have multiple realizations
  # of the dirichlet distribution here at which to evaluate the density
  if(is.vector(x)) {
    if(length(x) != length(alph)) {
      stop("x must have the same dimension of alph to evaluate dirichlet density.")
    }
  } else if(is.matrix(x)) {
    if(ncol(x) != length(alph)) {
      stop("x must have the same dimension of alph to evaluate dirichlet density.")
    }
  }

  if(any(x <= 0)) {
    stop("Dirichlet distribution only valid on positive support.")
  }
  if(any(alph <= 0)) {
    stop("Dirichlet concentration parameter alph must be > 0.")
  }


  if(is.vector(x)) {

    # scale so that x sums to 1
    x <- x / sum(x)

    norm_const <- log(base::gamma(sum(alph))) - sum(log(base::gamma(alph)))

    ldensity <- norm_const + sum((alph-1) * log(x))
    if(log) {
      return(ldensity)
    } else {
      return(exp(ldensity))
    }

  } else {
    x <- sweep(x, MARGIN = 1, STATS = rowSums(x), FUN = `/`)
    ldensity <- purrr::array_branch(x, 1) %>%
      purrr::map_dbl(function(.x, .alph) {
        norm_const <- log(base::gamma(sum(.alph))) - sum(log(base::gamma(.alph)))
        return(norm_const + sum((.alph-1) * log(.x)))
      }, .alph = alph)

    if(log) {
      return(ldensity)
    } else {
      return(exp(ldensity))
    }
  }
}
