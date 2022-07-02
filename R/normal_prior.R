Normal <- R6::R6Class("Normal",
                inherit = Prior,
                public =
                  list(name = "normal",
                       length = 1,
                       mean = 0,
                       sd = 1
                  )
)

Normal$set('public',
          'initialize',
function(mean,
         sd){
  if(!missing(mean))
    self$mean = mean
  self$length = length(self$mean)
  if(!missing(sd))
    self$sd = sd

  invisible(self)
}
)

Normal$set('public',
         'dprior',
         function(x) {
  invisible(sum(dnorm(
    x,
    mean = self$mean,
    sd = self$sd,
    log = T
  )))
         }
)

Normal$set('public',
         'rprior',
         function(){
           rnorm(1,
                 mean = self$mean,
                 sd = slf$sd
           )
         }
)
