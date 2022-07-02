Dirichlet <- R6::R6Class(
  "Dirichlet",
  inherit = Prior,
  public =
    list(
      name = "dirichet",
      length = NULL,
      alph = NULL
    )
)

Dirichlet$set('public',
              'initialize',
              function(alph) {
                if (!missing(alph)) {
                  if (any(alph <= 0)) {
                    stop("All elements of Dirichlet concentration parameter alph must be positive.")
                  }
                  self$alph = alph
                }
                else {
                  stop("Dirichlet prior must be initialized with concentration parameter 'alph'")
                }

                invisible(self)
              })

Dirichlet$set('public',
              'dprior',
              function(x) {
                invisible(sum(ddirch(
                  x,
                  alph = self$alph,
                  log = TRUE
                )))
              })

Dirichlet$set('public',
              'rprior',
              function() {
                rdirch(1,
                      alph = self$alph)
              })
