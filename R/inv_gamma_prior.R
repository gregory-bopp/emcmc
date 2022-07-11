InvGamma <- R6::R6Class(
  "InvGamma",
  inherit = Prior,
  public =
    list(
      name = "InvGamma",
      shape = 1,
      scale = 1,
      rate = 1,
      ##########################################################################
      ## Public Methods
      ##########################################################################
      #' @description
      #' Create new Inverse Gamma Prior Class Object
      #'
      #' @param shape (numeric) Gamma shape parameter
      #' @param rate (numeric) Gamma rate parameter (only one of rate and scale should
      #' be specified)
      #' @param scale (numeric) Gamma scale parameter (only one of rate and scale should
      #' be specified)
      #'
      #' @return
      initialize =  function(shape,
                             rate,
                             scale = 1 / rate) {
        if (!missing(rate) && !missing(scale)) {
          stop("specify 'rate' or 'scale' but not both")
        }
        if (!missing(shape))
          self$shape = shape
        if (!missing(scale)) {
          self$scale = scale
          self$rate = 1 / scale
        }
        if (!missing(rate)) {
          self$scale = 1 / rate
          self$rate = 1 / scale
        }
        invisible(self)
      }
    )
)


InvGamma$set('public',
             'dprior',
             function(x) {
               if (all(x > 0)) {
                 ll <- sum(dgamma(
                   1 / x,
                   shape = self$shape,
                   rate = self$scale,
                   log = T
                 ) -
                   2 * log(x)) # Jacobian
               }
               else{
                 ll <- -Inf
               }
             })
