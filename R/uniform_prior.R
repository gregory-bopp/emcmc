Uniform <- R6::R6Class(
  "Uniform",
  inherit = Prior,
  public =
    list(
      name = "uniform",
      min = 0,
      max = 1,
      #' @description
      #' Create New Uniform Prior Class Object
      #'
      #' @param min (numeric) lower-bound of uniform distribution
      #' @param max (numeric) upper-bound of uniform distribution
      #'
      #' @return
      initialize =  function(min,
                             max) {
        if (!missing(min))
          self$min = min
        if (!missing(max))
          self$max = max
        if (self$max <= self$min)
          stop("min must be < max")

        invisible(self)
      }
    )
)

Uniform$set('public',
            'dprior',
            function(x) {
              invisible(sum(dunif(
                x,
                min = self$min,
                max = self$max,
                log = TRUE
              )))
            })

Uniform$set('public',
            'rprior',
            function() {
              runif(1,
                    min = self$min,
                    max = self$max)
            })
