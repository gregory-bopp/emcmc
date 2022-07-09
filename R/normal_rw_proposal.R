NormalRW <- R6::R6Class("NormalRW",
                        inherit = Proposal,
                        public =
                          list(name = "NormalRW"
                          ),
                        private =
                          list(
                            .adapt_prop_var = TRUE,
                            .is_asymmetric = FALSE,
                            .prop_var = NA
                          )
)

NormalRW$set('public',
             'initialize',
             function(prop_var,
                      adapt_prop_var,
                      blocks) {
               # Proposal variance
               if (!missing(prop_var))
                 private$.prop_var <- prop_var
               if (!missing(adapt_prop_var))
                 private$.adapt_prop_var <- adapt_prop_var
               # Set blocks
               self$set_blocks(blocks)
               invisible(self)
             })

NormalRW$set('public',
             'r_fn',
             function(self, cur){
               bi <- self$block_info
               prop <- cur
               prop[bi$index] <- rnorm(length(cur[bi$index]),
                       mean = cur[bi$index],
                       sd = sqrt(bi$prop_var))
               return(prop)
             }
             )

# NormalRW$set('public',
#              'd_fn',
#              function(self, prop, cur, block_level){
#                block_index <- self$cur_block_index
#                return(dnorm(prop,
#                             mean = cur[block_index],
#                             sd = sqrt(private$.prop_var[[block_level]]),
#                             log = TRUE)
#                       )
#              }
# )
