CompLogLik <- R6::R6Class("CompLogLik",
            public = list(
             param_names = NULL,
             fn = NULL  # Two arguments:param (named list)
                        #               data (named list)
                        #
            ),
  private = list(.value = NA,
                 .prev_value = NA),
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
    }
  )
)

CompLogLik$set("public",
               "initialize",
               function(param_names, fn){
                self$fn <- fn
                self$param_names <- param_names
                invisible(self)
               }
)

CompLogLik$set("public",
               "set_param",
               function(param, data){
                private$.value = self$fn(param = param,
                                         data = data)
                invisible(self)
               }
)


CompLogLik$set("public",
               "get_value",
               function(){
                return(self$value)
               }
)


CompLogLik$set("public",
               "revert_value",
               function(){
                 private$.value = private$.prev_value
                 invisible(self)
               }
)

CompLogLik$set("public",
               "cache_value",
               function(){
                 private$.prev_value = private$.value
                 invisible(self)
               })



