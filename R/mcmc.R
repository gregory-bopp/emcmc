MCMC <- R6::R6Class("MCMC",
  public = list(
    n_mcmc = 1000,
    n_adapt = 500,
    n_burnin = 500,
    n_cleared = 0,
    thin_int = 1,
    cache_freq = NA,
    cache_dir = NA,
    combine_cached = TRUE,
    init = NULL,
    cur = NULL,
    cur_lik = NA,
    prop_lik = NA,
    prop = NULL,
    proposals = NULL,
    data = NULL,
    const = NULL,
    samples = NULL,
    acpt_rt = NULL,
    param_names = NULL,
    log_lik = NULL,                    # LogLik Class Object
    priors = NULL,
    saved_file_paths = NULL,
    show_progress = TRUE,
    cur_mcmc_iter = 1,
    sample_row = 1,
    adapt_prop_control = list(
      c0 = 10,
      c1 = 0.8,
      tune_k = 3)
    ),
  private = list(.n_samples = NULL,
                 .cache_flag = FALSE,
                 .cache_ends = NA,
                 .cache_starts = NA,
                 .cache_part = NA,
                 .n_mcmc_full = NA,
                 .progress_bar = NULL,
                 .tick_interval = NA),
  active = list(
    # Read only field: n_samples
    n_samples = function(value){
      if(missing(value)){
        private$.n_samples
      }
      else{
        stop("$n_samples is read only", call. = F)
      }
    }
  )
)




##
## Private Methods
##
MCMC$set("private",
         "set_n_samples",
         function(n_samples){
           private$.n_samples = n_samples
         }
)




##
## Public Methods
##

# Initialize
MCMC$set("public", "initialize",
  function(data,
           init,
           priors,
           proposals,
           log_lik,
           n_mcmc,
           n_adapt,
           n_burnin,
           thin_int,
           cache_freq,
           cache_dir,
           combine_cached,
           const,
           show_progress){

    # If const is passed and members are in init, remove
    if (!missing(const) && any(names(const) %in% names(init))) {
      init <- list_modify(init, !!!map(names(const), ~ {
        NULL
      }) %>%
        `names<-`(names(const)))
    }

    # Check that init contains all params in param_names
    self$param_names <- names(init)
    self$init <- init
    self$cur <- init
    # Setup sample storage
    if(!missing(n_mcmc)){
      self$n_mcmc <- n_mcmc
    }
    if(!missing(n_adapt)){
      self$n_adapt <- n_adapt
    }
    if(!missing(n_burnin)){
      self$n_burnin <- n_burnin
    }
    if(!missing(thin_int)){
      self$thin_int <- thin_int
    }
    if(!missing(show_progress)){
      self$show_progress <- show_progress
    }

    # Store total requested n_mcmc as n_mcmc_full, and use n_mcmc as parts
    private$.n_mcmc_full <- self$n_mcmc
    private$.tick_interval <- round(private$.n_mcmc_full/50)

    # Set up caching (turn into method) ---------------------
    if(!missing(cache_freq)){
      cache_freq <- round(cache_freq)    # Ensure that cache_freq is an integer
      private$.cache_flag = TRUE
      if(missing(cache_dir)){
        cache_dir <- file.path(tempdir(), 'emcmc')
        print("Specified 'cache_freq', but missing 'cache_dir'")
        cat(paste("Using temporary cache dir: ", cache_dir, sep = "\n"))
      }
      dir.create(cache_dir, recursive = T)
      if(!missing(combine_cached)){
        self$combine_cached <- combine_cached
      }
      self$cache_dir <- cache_dir
      self$cache_freq <- min(cache_freq, private$.n_mcmc_full)
      cache_ends <- seq(cache_freq, private$.n_mcmc_full, by= cache_freq)
      if(!(n_mcmc %in% cache_ends)){
        cache_ends <- c(cache_ends, private$.n_mcmc_full)
      }
      private$.cache_ends <- cache_ends
      private$.cache_starts <- c(1, cache_ends[-length(cache_ends)] + 1)
      self$n_mcmc <- private$.cache_ends[1]
    }
    # End method ---------------------------------------------
    # Ignored samples not stored
    self$allocate_sample_storage()

    # Keep constants
    if(!missing(const)){
      self$const <- const
    }

    self$prop <- self$cur

    # Set likelihood function
    self$log_lik <- log_lik

    # Add Normal random walk proposals for any parameters with missing proposals
    if(missing(proposals))
      proposals <- list()

    for(param_name in self$param_names){
      if(!(param_name %in% names(proposals))){
        proposals[[param_name]] <- NormalRW$new(prop_var = 0.001,
                                                blocks = factor(1:length(self$cur[[param_name]])))
      }
    }
    self$proposals <- proposals

    # If proposals are generic (i.e. don't have defined blocks, assume single block)

    for(param_name in self$param_names){
      if(self$proposals[[param_name]]$ambiguous_blocks){
        self$proposals[[param_name]]$set_blocks(factor(rep(1, length(self$cur[[param_name]]))))
      }
    }

    # Store data
    self$data <- data

    # Set Parameter Class Objects (contains e.g. prior functions)
    self$priors <- priors

    # Adaptive tuning constant
    self$calc_gamma1()

    # Prepare likelihood object (evaluate at current value and cache)
    self$log_lik$update_all_param(param = list_modify(self$cur, !!!self$const),
                                  data = self$data)
    self$log_lik$cache_param_cll(param_names = self$log_lik$param_names)
    self$cur_lik = self$log_lik$value

    invisible(self)
  }
)

##
## Proposal Variance
##
MCMC$set("public",
         "calc_gamma1",
         function() {
           self$adapt_prop_control$gamma1 =
             self$adapt_prop_control$c0 / (self$cur_mcmc_iter +
                   self$adapt_prop_control$tune_k) ^ self$adapt_prop_control$c1
           invisible(self)
         })



##
## Proposals
##
MCMC$set("public", "set_prop",
         function(param_name, value) {
           self$prop[[param_name]] <- value
           invisible(self)
           }
)


MCMC$set("public", "eval_prop_log_lik",
         function(param_names) {
           self$log_lik$update_param(
             param_names = param_names,
             param = list_modify(self$prop,!!!self$const),
             data = self$data
           )
           self$prop_lik <- self$log_lik$value
           invisible(self)
         })

MCMC$set("public",
         "make_proposal",
         function(param_name, block_level) {
           value <-
             self$proposals[[param_name]]$r_fn(
               self = self$proposals[[param_name]],
               cur = self$cur[[param_name]]
             )
           self$set_prop(param_name, value)
           invisible(self)
         })

MCMC$set("public", "full_cond",
         function(param_name, type) {
           if (type == "cur") {
             lik <- self$cur_lik
             param_value <- self$cur[[param_name]]
           }
           else{
             lik <- self$prop_lik
             param_value <- self$prop[[param_name]]
           }
           # Evaluate prior value
           if (param_name %in% names(sampler$priors)) {
             prior <- self$priors[[param_name]]$dprior(param_value)
           }
           else{
             prior <- 0
           }
           invisible(lik + prior)
         })


##
## TODO: add block to parameters
##
MCMC$set("public", "mh_step",
         function(param_name, block_level){
          # Update proposal block_info (i.e. r_fn and d_fn needs index)
          self$proposals[[param_name]]$set_cur_block_info(block_level)
          # Record current full conditional
          cur_fc <- self$full_cond(param_name, "cur")
          # Cache current value of log-lik in prev_value spot
          self$log_lik$cache_param_cll(param_name)
          # Make a random walk proposal
          self$make_proposal(param_name, block_level = block_level)
          # Update log_lik object with proposal values of param
          self$eval_prop_log_lik(param_name)
          # Calculate full conditional at proposal
          prop_fc <- self$full_cond(param_name, "prop")

          # Calculate Hastings ratio if proposal is asymmetric
          if(self$proposals[[param_name]]$is_asymmetric){
            mh_num <- sum(
                    self$proposals[[param_name]]$d_fn(self$proposals[[param_name]],
                                                      prop = self$cur[[param_name]],
                                                      cur = self$prop[[param_name]])
            )
            mh_denom <- sum(
              self$proposals[[param_name]]$d_fn(self$proposals[[param_name]],
                                                prop = self$prop[[param_name]],
                                                cur = self$cur[[param_name]])
            )
          }
          else{
            mh_num <- mh_denom <- 0
          }

          ##
          ## Accept Case
          ##
          if(log(runif(1)) < prop_fc + mh_num - cur_fc - mh_denom){
            # Retain sample
            self$cur[[param_name]] <- self$prop[[param_name]]
            # Retain current log likelihood
            self$cur_lik <- self$prop_lik
            # Update acceptance indicator
            acpt_ind <- 1
          }
          ##
          ## Reject Case
          ##
          else{
            # Revert parameter
            self$prop[[param_name]] <- self$cur[[param_name]]
            # Revert log-likelihood to cached state
            self$log_lik$revert_param_cll(param_name)
            # Update acceptance indicator
            acpt_ind <- 0
          }

          # Calculate new acceptance rate
          self$proposals[[param_name]]$calc_acpt_rt(cur_mcmc_iter = self$cur_mcmc_iter,
                                                    acpt_ind = acpt_ind,
                                                    block_level = block_level)


          # Update proposal variance
          if(self$cur_mcmc_iter <= self$n_adapt){
            if(self$proposals[[param_name]]$adapt_prop_var){
              self$proposals[[param_name]]$tune_prop_var(
                block_level,
                self$adapt_prop_control$gamma1
              )
            }
          }
          invisible(self)
         }
)


MCMC$set("public",
         "store_sample",
         function(param_name){
          self$samples[[param_name]][self$sample_row,] <- c(self$cur[[param_name]])
          invisible(self)
         }
)


MCMC$set("public",
         "set_n_mcmc",
         function(n_mcmc){
           self$n_mcmc <- n_mcmc
           self$allocate_sample_storage()
           invisible(self)
         }
)


MCMC$set("public",
         "allocate_sample_storage",
         function() {
           self$sample_row <- 1
           n_ignore <- max(self$n_burnin, self$n_cleared)
           private$set_n_samples(floor((self$n_mcmc - n_ignore) / self$thin_int))
           # Allocate space to store samples
           # matrix: n_samples x length(param)
           self$samples <- self$param_names %>%
             map(~ matrix(NA,
                          nrow = self$n_samples,
                          ncol = prod(dim(
                            self$cur[[.x]] %>%
                              as.matrix
                          )))) %>%
             `names<-`(self$param_names)
           invisible(self)
         })

MCMC$set("public",
         "add_n_mcmc",
         function(n_add_mcmc){
          self$n_mcmc <- self$n_mcmc + n_add_mcmc
          self$add_sample_storage(n_add_mcmc)
           invisible(self)
         }
)

MCMC$set("public",
         "add_sample_storage",
         function(n_add_mcmc){
           n_ignore <- max(self$n_burnin,self$n_cleared)
           n_total_samples <- floor((self$n_mcmc - n_ignore) / self$thin_int)
           n_new_samples <- n_total_samples - self$n_samples
           private$set_n_samples(n_total_samples)
           map(self$param_names,
              ~{
                self$samples[[.x]] <- rbind(self$samples[[.x]],
                      matrix(NA,
                             nrow = n_new_samples,
                             ncol = prod(dim(
                            self$cur[[.x]] %>%
                              as.matrix
                          ))))
              }
          )
           invisible(self)
         }
)

MCMC$set("public", "run_mcmc",
         function() {
           if(self$show_progress){
             # Make progress bar
             private$.progress_bar <- progress::progress_bar$new(
              format = "[:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
              total = private$.n_mcmc_full,
              complete = "=",   # Completion bar character
              incomplete = "-", # Incomplete bar character
              current = ">",    # Current bar character
              clear = FALSE,    # If TRUE, clears the bar when finish
              width = 100)      # Width of the progress bar
           }
           # Run first part
           self$run_mcmc_part()
           ##
           ## Caching Loop: if there are multiple caching steps, loop over them
           ##
           if (private$.cache_flag &
               length(private$.cache_ends) > 1) {
             private$.cache_part <- 1
             # Write current mcmc samples to a file (cache them)
             self$write_to_file(file.path(self$cache_dir,
                                          paste0(paste0(
                                            c(zero_pad_strd(private$.cache_starts[private$.cache_part]),
                                              zero_pad_strd(private$.cache_ends[private$.cache_part])),
                                            collapse = '_'
                                          ), '.rds')))
             for (cp_ in 2:length(private$.cache_ends)) {
               private$.cache_part <- cp_
               # Free up storage space and run sampler
               self$clear_cache()
               # Calculate size of next mcmc section to run
               cache_part_size <- private$.cache_ends[private$.cache_part] -
                 private$.cache_starts[private$.cache_part] + 1
               # Allocate space for storing samples of next section
               self$add_n_mcmc(cache_part_size)
               # Run next part
               self$run_mcmc_part()
               # Write current mcmc samples to a file (cache them)
               self$write_to_file(file.path(self$cache_dir,
                                            paste0(paste0(
                                              c(zero_pad_strd(private$.cache_starts[private$.cache_part]),
                                                zero_pad_strd(private$.cache_ends[private$.cache_part])),
                                              collapse = '_'
                                            ), '.rds')))
             }
             if(private$.cache_flag&&self$combine_cached){
               self$clear_cache()
               self$prepend_stored_samples(self$saved_file_paths)
             }
           }
           ##
           ## End Caching loop
           ##
           invisible(self)
         })


MCMC$set("public", "run_mcmc_part",
         function(){
           if(self$sample_row <= self$n_samples){
             start_mcmc_iter <- self$cur_mcmc_iter
             ##
             ## Main MCMC Loop
             ##
             for(cur_mcmc_iter in start_mcmc_iter:self$n_mcmc){
              self$cur_mcmc_iter <- cur_mcmc_iter
              if(self$show_progress && (cur_mcmc_iter%%private$.tick_interval==0)){
                # Update progress bar
                private$.progress_bar$tick(private$.tick_interval)
             }
              if(self$cur_mcmc_iter <= self$n_adapt){
                # Update variance tuning parameters
                self$calc_gamma1()
              }

              ## Iterate through parameters and do Metropolis Step
              for(param_name in self$param_names){
                for(block_level in self$proposals[[param_name]]$block_levels){
                  self$mh_step(param_name = param_name, block_level = block_level)
                }
              }

              ## Store samples
              if((self$cur_mcmc_iter > self$n_burnin) &
                 (self$cur_mcmc_iter %% self$thin_int == 0)){
                for(param_name in self$param_names){
                  self$store_sample(param_name)
                }
                self$sample_row <- self$sample_row + 1
              }
             }
             ##
             ## End MCMC Loop
             ##

             self$cur_mcmc_iter <- self$cur_mcmc_iter + 1
           }
           invisible(self)
         }
)

MCMC$set("public",
         "clear_cache",
         function(){
           self$n_cleared <- self$n_mcmc
           self$sample_row <- 1
           self$allocate_sample_storage()
           invisible(self)
         }
)



MCMC$set("public",
         "write_to_file",
         function(file_path, ...){
           # Record archive saved file paths
           if(is.null(self$saved_file_paths)){
             self$saved_file_paths <- list(file_path)
           }
           else{
             self$saved_file_paths <- c(self$saved_file_paths, file_path)
           }
           saveRDS(self, file = file.path(file_path, ...))
           invisible(self)
         }
)

MCMC$set("public",
         "prepend_samples",
         function(samples) {
           self$samples <- map(names(self$samples), ~ {
             rbind(samples[[.x]], self$samples[[.x]])
           }) %>%
             `names<-`(names(self$samples))
           # Update sample row
           self$sample_row <- self$sample_row + nrow(samples[[1]])
           private$set_n_samples(nrow(self$samples[[1]]))
           invisible(self)
         })

MCMC$set("public",
         "prepend_stored_samples",
         function(file_paths) {
           # Order of file-paths should match order of storage
           # (i.e. older sample files are listed first)
           map(rev(file_paths), ~ {
             prev_sampler <- readRDS(.x)
             self$prepend_samples(prev_sampler$samples)
           })
          invisible(self)
         }
 )
