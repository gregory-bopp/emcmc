Uniform <- R6::R6Class(
  "Uniform",
  inherit = Prior,
  public =
    list(
      name = "uniform",
      length = 1,
      min = 0,
      max = 1
    )
)

Uniform$set('public',
            'initialize',
            function(min,
                     max) {
              if (!missing(min))
                self$min = min
              self$length = length(self$min)
              if (!missing(max))
                self$max = max

              invisible(self)
            })

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
