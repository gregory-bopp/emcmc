GibbsProposal <- R6::R6Class("GibbsProposal",
                        inherit = Proposal,
                        public =
                          list(name = "GibbsProposal"
                          ),
                        private =
                          list(
                            .update_type = "gibbs"
                          )
)

GibbsProposal$set('public',
             'initialize',
             function(r_fn) {
               # Proposal variance
               if (!missing(prop_var))
                 private$.prop_var <- prop_var
               if (!missing(adapt_prop_var))
                 private$.adapt_prop_var <- adapt_prop_var
               # Set blocks
               self$set_blocks(blocks)
               invisible(self)
             })

GibbsProposal$set('public',
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
