Beta <- R6::R6Class(
  "Beta",
  inherit = Prior,
  public =
    list(
      name = "beta",
      shape1 = NA,
      shape2 = NA,
      ncp = 0,
      ##########################################################################
      ## Public Methods
      ##########################################################################
      #' @description
      #' Create New Beta Prior Class Object
      #'
      #' @param shape1 (numeric) first shape parameter
      #' @param shape2 (numeric) second shape parameter
      #' @param ncp (numeric, optional) non-centrality parameter
      #'
      #' @return
      initialize = function(shape1,
                            shape2,
                            ncp = 0) {
        if (missing(shape1) || missing(shape2)) {
          stop("Need both shape1 and shape2 parameters")
        }
        if (shape1 <= 0) {
          stop("shape1 must be > 0")
        }
        if (shape1 <= 0) {
          stop("shape2 must be > 0")
        }
        self$shape1 = shape1
        self$shape2 = shape2
        if (!missing(ncp)) {
          self$ncp = ncp
        }
        self$ncp = ncp
        invisible(self)
      }
    )
)

Beta$set('public',
         'dprior',
         function(x) {
           invisible(sum(
             dbeta(
               x,
               shape1 = self$shape1,
               shape2 = self$shape2,
               ncp = self$ncp,
               log = T
             )
           ))
         })

Beta$set('public',
         'rprior',
         function() {
           rbeta(
             1,
             shape1 = self$shape1,
             shape2 = self$shape2,
             ncp = self$ncp
           )
         })
