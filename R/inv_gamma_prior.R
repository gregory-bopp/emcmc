InvGamma <- R6::R6Class(
  "InvGamma",
  inherit = Prior,
  public =
    list(
      name = "InvGamma",
      shape = 1,
      scale = 1,
      rate = 1
    )
)

InvGamma$set('public',
             'initialize',
             function(shape,
                      rate,
                      scale = 1/rate){
               if (!missing(rate) && !missing(scale)) {
                stop("specify 'rate' or 'scale' but not both")
               }
               if (!missing(shape))
                 self$shape = shape
               if (!missing(scale)){
                 self$scale = scale
                 self$rate = 1/scale
               }
               if(!missing(rate)){
                 self$scale = 1/rate
                 self$rate = 1/scale
               }
               invisible(self)
             })

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
