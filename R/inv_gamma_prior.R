InvGamma <- R6::R6Class("InvGamma",
                inherit = Prior,
                public =
                  list(name = "InvGamma",
                       ig_shape = 1,
                       ig_scale = 1
                  )
)

InvGamma$set('public',
          'initialize',
  function(ig_shape,
           ig_scale){
    if(!missing(ig_shape))
      self$ig_shape = ig_shape
    if(!missing(ig_scale))
    self$ig_scale = ig_scale
    invisible(self)
  }
)

InvGamma$set('public',
               'dprior',
 function(x){
   if (all(x > 0)) {
     ll <- dgamma(1 / x,
                  shape = self$ig_shape,
                  rate = self$ig_scale,
                  log = TRUE) -
       2 * sum(log(x)) # Jacobian
   }
   else{
     ll <- -Inf
   }
 }
)

