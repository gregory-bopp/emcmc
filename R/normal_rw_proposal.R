NormalRW <- R6::R6Class(
  "NormalRW",
  inherit = Proposal,
  public =
    list(
      name = "NormalRW",
      initialize = function(prop_var,
                            adapt_prop_var,
                            blocks) {
        # Proposal variance
        if (!missing(prop_var))
          private$.prop_var <-
            prop_var
        if (!missing(adapt_prop_var))
          private$.adapt_prop_var <-
            adapt_prop_var
        # Set blocks
        self$set_blocks(blocks)
        invisible(self)
      },
      #' @description
      #' Block-wise Normal Random Walk Proposal Sampling Function
      #'
      #' @param self
      #' @param cur
      #'
      #' @return
      r_fn = function(self, cur) {
        bi <- self$block_info
        prop <- cur
        prop[bi$index] <-
          rnorm(length(cur[bi$index]),
                mean = cur[bi$index],
                sd = sqrt(bi$prop_var))
        return(prop)
      }
    ),
  private =
    list(
      .adapt_prop_var = TRUE,
      .is_asymmetric = FALSE,
      .prop_var = NA
    )
)
