LogLik <- R6::R6Class(
  "LogLik",
  public = list(
    cll_list = NULL,
    cll_lookup = NULL,
    param_names = NULL,
    ##########################################################################
    ## Public Methods
    ##########################################################################
    #' @description
    #' Create a Log-Likelihood Object
    #'
    #' @param cll_list
    #'
    #' @return
    initialize = function(cll_list) {
      self$cll_list <- cll_list
      # Create a vector of all parameter names involved in log likelihood
      self$param_names <-
        map(cll_list, ~ {
          .x$param_names
        }) %>%
        unlist %>%
        unique %>%
        sort
      # Create a lookup table param_name -> index of cll_list components
      self$cll_lookup <- map(self$param_names, ~ {
        map_lgl(cll_list, function(cll) {
          .x %in% cll$param_names
        })
      } %>% which) %>% `names<-`(self$param_names)
      invisible(self)
    },

    #' @description
    #' Recompute Total Log-Likelihood from Cached Component Log-Likelihood Components
    #'
    #' @return
    recompute_total = function() {
      private$.value = map_dbl(self$cll_list, ~ {
        .x$value
      }) %>%
        sum
      invisible(self)
    },
    #' @description
    #' Set Parameters and Recalculate Component Log-Likelihood Terms with those
    #' Parameters
    #'
    #' @param param_names
    #' @param param
    #' @param data
    #'
    #' @return
    set_param = function(param_names, param, data) {
      cll_update_idx <- self$cll_lookup[param_names] %>%
        unlist %>%
        unique
      map(cll_update_idx, ~ {
        self$cll_list[[.x]]$set_param(param = param,
                                      data = data)
      })
      self$recompute_total()
      invisible(self)
    },
    #' @description
    #' Revert Component Log-Likelihood Terms Corresponding to Specified Parameters
    #'
    #' @param param_names
    #'
    #' @return
    revert_param_cll = function(param_names) {
      cll_update_idx <- self$cll_lookup[param_names] %>%
        unlist %>%
        unique
      map(cll_update_idx, ~ {
        self$cll_list[[.x]]$revert_value()
      })
      self$recompute_total()
      invisible(self)
    },

    #' @description
    #' Cache Component Log-Likelihoods for Specified Parameters
    #'
    #' @param param_names
    #'
    #' @return
    cache_param_cll = function(param_names) {
      cll_update_idx <- self$cll_lookup[param_names] %>%
        unlist %>%
        unique
      map(cll_update_idx, ~ {
        self$cll_list[[.x]]$cache_value()
      })
      invisible(self)
    },
    #' @description
    #' Recalculate all Component Log-Likelihood Terms
    #'
    #' @param param
    #' @param data
    #'
    #' @return
    set_all_param = function(param, data) {
      self$set_param(self$param_names,
                     param = param,
                     data = data)
      invisible(self)
    },
    #' @description
    #' Get Log-Likelihood Value
    #'
    #' @return
    get_value = function() {
      return(self$value)
    },
    #' @description
    #' Mark Component Log-Likelihoods Containing Specified Parameters as Stale
    #'
    #' @param param_names
    #'
    #' @return
    mark_stale_cll = function(param_names) {
      cll_update_idx <- self$cll_lookup[param_names] %>%
        unlist %>%
        unique
      map(cll_update_idx, ~ {
        self$cll_list[[.x]]$mark_stale()
      })
    },
    #' @description
    #' Recalculate Stale Component Log-Likelihood Terms Containing Specified
    #' Parameters
    #'
    #' @param param_names
    #' @param param
    #' @param data
    #'
    #' @return
    update_stale_cll = function(param_names, param, data) {
      cll_update_idx <- self$cll_lookup[param_names] %>%
        unlist %>%
        unique
      map(cll_update_idx, ~ {
        # If stale, recalculate with passed values
        if (self$cll_list[[.x]]$stale) {
          self$cll_list[[.x]]$set_param(param = param,
                                        data = data)
          self$cll_list[[.x]]$cache_value()
        }
      })
      self$recompute_total()
      invisible(self)
    }
  ),
  ##########################################################################
  ## Private Attributes
  ##########################################################################
  private = list(.value = NA),
  ##########################################################################
  ## Private Methods
  ##########################################################################

  ##########################################################################
  ## Active Fields
  ##########################################################################
  active = list(
    value = function(value) {
      if (missing(value)) {
        private$.value
      }
      else{
        stop("$value is read only", call. = F)
      }
    }
  )
)
