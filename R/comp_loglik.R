CompLogLik <- R6::R6Class(
  "CompLogLik",
  public = list(
    param_names = NULL,
    fn = NULL,
    # Two arguments:param (named list)
    #               data (named list)
    #
    ##########################################################################
    ## Public Methods
    ##########################################################################

    #' Create Component Log-likelihood object
    #'
    #' @param param_names
    #' @param fn
    #'
    #' @return
    initialize = function(param_names, fn) {
      self$fn <- fn
      self$param_names <- param_names
      invisible(self)
    },

    #' Mark Component Log-likelihood as Out-of-date (stale)
    #'
    #' @return
    mark_stale = function() {
      private$.stale = TRUE
    },

    #' Set Parameter and Update Component Log-Likelihood
    #'
    #' @param param
    #' @param data
    #'
    #' @return
    set_param = function(param, data) {
      private$.value = self$fn(param = param,
                               data = data)
      private$.stale = FALSE
      invisible(self)
    },

    #' Get Component Log-likelihood Value
    #'
    #' @return
    get_value = function() {
      return(self$value)
    },

    #' Revert Value to Previous Value
    #'
    #' @return
    revert_value = function() {
      private$.value = private$.prev_value
      invisible(self)
    },

    #' Cache Value of Component Log-likelihood for Future Retrieval
    #'
    #' @return
    cache_value = function() {
      private$.prev_value = private$.value
      invisible(self)
    }
  ),
  ##########################################################################
  ## Public Attributes
  ##########################################################################
  private = list(
    .value = NA,
    .prev_value = NA,
    .stale = TRUE
  ),
  ##########################################################################
  ## Active Fields
  ##########################################################################
  active = list(
    value = function(x) {
      if (missing(x)) {
        private$.value
      }
      else{
        stop("$value is read only", call. = F)
      }
    },
    prev_value = function(x) {
      if (missing(x)) {
        private$.prev_value
      }
      else{
        stop("$prev_value is read only", call. = F)
      }
    },
    stale = function(x) {
      if (missing(x)) {
        private$.stale
      }
      else{
        stop("$stale is read only", call. = F)
      }
    }
  )
)
