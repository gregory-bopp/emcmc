LogLik <- R6::R6Class(
  "LogLik",
  public = list(
    cll_list = NULL,
    cll_lookup = NULL,
    param_names = NULL
  ),
  private = list(.value = NA),
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


LogLik$set("public",
           "initialize",
           function(cll_list) {
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
           })


LogLik$set("public",
           "recompute_total",
           function() {
             private$.value = map_dbl(self$cll_list, ~ {
               .x$value
             }) %>%
               sum
             invisible(self)
           })

LogLik$set("public",
           "set_param",
           function(param_names, param, data) {
             cll_update_idx <- self$cll_lookup[param_names] %>%
               unlist %>%
               unique
             map(cll_update_idx, ~ {
               self$cll_list[[.x]]$set_param(param = param,
                                             data = data)
             })
             self$recompute_total()
             invisible(self)
           })

LogLik$set("public",
           "revert_param",
           function(param_names) {
             cll_update_idx <- self$cll_lookup[param_names] %>%
               unlist %>%
               unique
             map(cll_update_idx, ~ {
               self$cll_list[[.x]]$revert_value()
             })
             self$recompute_total()
             invisible(self)
           })

LogLik$set("public",
           "cache_param_cll",
           function(param_names) {
             cll_update_idx <- self$cll_lookup[param_names] %>%
               unlist %>%
               unique
             map(cll_update_idx, ~ {
               self$cll_list[[.x]]$cache_value()
             })
             invisible(self)
           })


LogLik$set("public",
           "set_all_param",
           function(param, data) {
             self$set_param(self$param_names,
                            param = param,
                            data = data)
             invisible(self)
           })


LogLik$set("public",
           "get_value",
           function() {
             return(self$value)
           })


LogLik$set("public",
           "mark_stale_cll",
           function(param_names){
             cll_update_idx <- self$cll_lookup[param_names] %>%
               unlist %>%
               unique
             map(cll_update_idx, ~ {
               self$cll_list[[.x]]$mark_stale()
             })
           }
           )


LogLik$set("public",
           "update_stale_cll",
           function(param_names, param, data){
             cll_update_idx <- self$cll_lookup[param_names] %>%
               unlist %>%
               unique
             map(cll_update_idx, ~ {
               # If stale, recalculate with passed values
               if(self$cll_list[[.x]]$stale){
                 self$cll_list[[.x]]$set_param(param = param,
                                               data = data)
                 self$cll_list[[.x]]$cache_value()
               }
             })
             self$recompute_total()
             invisible(self)
           }
           )
