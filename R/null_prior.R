NullPrior <- R6::R6Class("NullPrior",
                      inherit = Prior,
                      public =
                        list(name = "null"))

NullPrior$set('public',
           'initialize',
           function() {
             invisible(self)
           })

NullPrior$set('public',
           'dprior',
           function(x) {
             invisible(0)
           })

NullPrior$set('public',
           'rprior',
           function() {
             invisible(NA)
           })
