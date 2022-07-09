Proposal <- R6::R6Class(
  "Proposal",
  public = list(block_info = NULL,
                mcmc = NULL),
  private = list(
    .acpt_rt = NA,
    .adapt_prop_var = FALSE,
    .ambiguous_blocks = TRUE,
    .blocks = factor(NA),
    .block_levels = NULL,
    .block_index_map = NULL,
    .d_fn = NULL,
    .is_asymmetric = FALSE,
    .opt_rt = 0.4,
    .prop_var = NA,
    .r_fn = NULL,
    .update_type = "mh"
  ),
  active = list(
    # Read only field
    acpt_rt = function(value) {
      if (missing(value)) {
        private$.acpt_rt
      }
      else{
        stop("$acpt_rt is read only", call. = F)
      }
    },
    adapt_prop_var = function(value) {
      if (missing(value)) {
        private$.adapt_prop_var
      }
      else{
        stop("$adapt_prop_var is read only", call. = F)
      }
    },
    ambiguous_blocks = function(value) {
      if (missing(value)) {
        private$.ambiguous_blocks
      }
      else{
        stop("$ambiguous_blocks is read only", call. = F)
      }
    },
    blocks = function(value) {
      if (missing(value)) {
        private$.blocks
      }
      else{
        stop("$blocks is read only", call. = F)
      }
    },
    block_levels = function(value) {
      if (missing(value)) {
        private$.block_levels
      }
      else{
        stop("$block_levels is read only", call. = F)
      }
    },
    block_index_map = function(value) {
      if (missing(value)) {
        private$.block_index_map
      }
      else{
        stop("$block_index_map is read only", call. = F)
      }
    },
    is_asymmetric = function(value) {
      if (missing(value)) {
        private$.is_asymmetric
      }
      else{
        stop("$is_asymmetric is read only", call. = F)
      }
    },
    opt_rt = function(value) {
      if (missing(value)) {
        private$.opt_rt
      }
      else{
        stop("$opt_rt is read only", call. = F)
      }
    },
    prop_var = function(value) {
      if (missing(value)) {
        private$.prop_var
      }
      else{
        stop("$prop_var is read only", call. = F)
      }
    },
    update_type = function(value) {
      if (missing(value)) {
        private$.update_type
      }
      else{
        stop("$update_type is read only", call. = F)
      }
    }
  )
)

Proposal$set("public",
             "initialize",
             function(r_fn,
                      d_fn,
                      prop_var,
                      adapt_prop_var,
                      is_asymmetric,
                      blocks,
                      mcmc,
                      update_type) {
               # Should proposal variance be adaptively tuned
               if (!missing(adapt_prop_var))
                 private$.adapt_prop_var <- adapt_prop_var
               # Is the proposal asymmetric
               if (!missing(is_asymmetric))
                 private$.is_asymmetric <- is_asymmetric
               # Define block update structure
               if (!missing(blocks)) {
                 self$set_blocks(blocks)
               }
               # Set proposal variances for each block
               if (!missing(prop_var)) {
                 self$set_prop_var(prop_var)
               }
               if (!missing(mcmc)) {
                 self$set_mcmc(mcmc)
               }
               if (!missing(update_type)) {
                 private$.update_type = stringr::str_to_lower(update_type)
               }

               # Random Proposal Generation Function
               private$.r_fn <- r_fn

               if (!missing(d_fn)) {
                 self$check_req_args(d_fn, c("cur", "prop"))
                 private$.d_fn <- d_fn

               }
               else{
                 if (private$.update_type == "mh") {
                   if (is_asymmetric) {
                     stop("Asymmetric proposals require d_fn")
                   }
                 }
               }
               invisible(self)
             })


Proposal$set("public",
             "set_blocks",
             function(blocks) {
               private$.blocks <- blocks
               private$.block_levels <- levels(blocks)
               private$.block_index_map <-
                 map(private$.block_levels,
                     ~ {
                       which(blocks == .x)
                     }) %>%
                 `names<-`(private$.block_levels)
               private$.ambiguous_blocks <- FALSE
               # Set prop_var
               self$set_prop_var()
               # Initialize the acceptance rate
               self$init_acpt_rt()
               invisible(self)
             })

Proposal$set("public",
             "set_prop_var",
             function(prop_var) {
               # If a prop_var is passed, try to set
               if (!missing(prop_var)) {
                 if (all(names(prop_var) %in% private$.block_levels)) {
                   private$.prop_var <- prop_var
                 }
                 else{
                   # Supply single prop_var to apply to all blocks
                   stopifnot(is.numeric(prop_var))
                   private$.prop_var <- map(private$.block_levels, ~ {
                     prop_var
                   }) %>% `names<-`(private$.block_levels)
                 }
               }
               # If no prop_var is passed, expand existing private$.prop_var to length
               # of private$.block_levels
               else{
                 # Check
                 if (length(private$.prop_var) == 1) {
                   single_prop_var <- private$.prop_var
                   private$.prop_var <- map(private$.block_levels, ~ {
                     single_prop_var
                   }) %>% `names<-`(private$.block_levels)
                 }
                 else{
                   if (!(all(names(private$.prop_var) %in% private$.block_levels))) {
                     stop('Length of prop_var does not match block_levels')
                   }
                 }
               }
               invisible(self)
             })

Proposal$set("public",
             "init_acpt_rt",
             function() {
               # Initialize acpt_rt list
               private$.acpt_rt <-
                 private$.block_levels %>%
                 map( ~ 0) %>%
                 `names<-`(private$.block_levels)
               invisible(self)
             })

Proposal$set("public",
             "check_req_args",
             function(fn, req_args) {
               if (!missing(fn)) {
                 f_args <- formalArgs(fn)
                 miss_arg_ind <- !(req_args %in% f_args)
                 if (any(miss_arg_ind)) {
                   stop(paste(
                     "Proposal r/d function missing required arguments:",
                     req_args[miss_arg_ind]
                   ))
                 }
               }
               invisible(self)
             })


Proposal$set("public",
             "r_fn",
             function(...) {
               return(private$.r_fn(...))
             })

Proposal$set("public",
             "d_fn",
             function(...) {
               return(private$.d_fn(...))
             })

Proposal$set("public",
             "tune_prop_var",
             function(block_level, gamma1) {
               private$.prop_var[[block_level]] <-
                 exp(log(private$.prop_var[[block_level]]) +
                       gamma1 * (private$.acpt_rt[[block_level]] -
                                   private$.opt_rt))
               invisible(self)
             })

Proposal$set("public", "calc_acpt_rt",
             function(cur_mcmc_iter, acpt_ind, block_level) {
               private$.acpt_rt[[block_level]] <-
                 (private$.acpt_rt[[block_level]] * (cur_mcmc_iter - 1) + acpt_ind) /
                 cur_mcmc_iter
               invisible(self)
             })

Proposal$set("public", "set_cur_block_info",
             function(block_level) {
               self$block_info$level <- block_level
               self$block_info$index <-
                 private$.block_index_map[[block_level]]
               self$block_info$prop_var <-
                 private$.prop_var[[block_level]]
               invisible(self)
             })

Proposal$set("public",
             "set_mcmc",
             function(mcmc) {
               self$mcmc <- mcmc
               invisible(self)
             })
