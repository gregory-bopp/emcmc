Dirichlet <- R6::R6Class(
  "Dirichlet",
  inherit = Prior,
  public =
    list(
      name = "dirichet",
      alpha = NULL
    )
)

Dirichlet$set('public',
              'initialize',
              function(alpha) {
                if (!missing(alpha)) {
                  if (any(alpha <= 0)) {
                    stop("All elements of Dirichlet concentration parameter alpha must be positive.")
                  }
                  self$alpha = alpha
                }
                else {
                  stop("Dirichlet prior must be initialized with concentration parameter 'alpha'")
                }

                invisible(self)
              })

Dirichlet$set('public',
              'dprior',
              function(x) {
                invisible(sum(ddirichlet(
                  x,
                  alpha = self$alpha,
                  log = TRUE
                )))
              })

Dirichlet$set('public',
              'rprior',
              function() {
                rdirichlet(1,
                       alpha = self$alpha)
              })
