#' R6 Class for MCMC sampler
#'
#' An MCMC class is designed to abstract the sampling algorithm infrastructure
#' from the model definition, and enable faster iteration on model development
#' and selection, minimizing the need for changing MCMC code for different models.
MCMC <- R6::R6Class(
  "MCMC",
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
    cur_lik = NA,
    prop_lik = NA,
    prop = NULL,
    proposals = NULL,
    data = NULL,
    const = NULL,
    samples = NULL,
    acpt_rt = NULL,
    param_names = NULL,
    log_lik = NULL,
    # LogLik Class Object
    priors = NULL,
    saved_file_paths = NULL,
    show_progress = TRUE,
    cur_mcmc_iter = 1,
    sample_row = 1,
    adapt_prop_control = list(c0 = 10,
                              c1 = 0.8,
                              tune_k = 3),
    ##########################################################################
    ## Public Methods
    ##########################################################################
    #' Create an MCMC Sampler Object
    #'
    #' @param data (named list) of all data elements used in likelihood calculations
    #' @param init (named list) of initial values for all parameters for which to draw
    #' posterior samples. Parameter names listed here will be available in
    #' \code{param} argument passed to user-defined likelihood functions. See:
    #' \code{log_lik} argument and \code{LogLik} class.
    #' @param priors (named list)
    #' @param proposals (named list)
    #' @param log_lik (LogLik) object
    #' @param n_mcmc (integer)
    #' @param n_adapt (integer)
    #' @param n_burnin (integer)
    #' @param thin_int (integer)
    #' @param cache_freq (integer)
    #' @param cache_dir (string)
    #' @param combine_cached (logical)
    #' @param const (named list)
    #' @param show_progress (logical)
    #' @return A new `MCMC` sampler object
    initialize = function(data,
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
                          show_progress) {
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
      private$.cur <- init
      # Setup sample storage
      if (!missing(n_mcmc)) {
        self$n_mcmc <- n_mcmc
      }
      if (!missing(n_adapt)) {
        self$n_adapt <- n_adapt
      }
      if (!missing(n_burnin)) {
        self$n_burnin <- n_burnin
      }
      if (!missing(thin_int)) {
        self$thin_int <- thin_int
      }
      if (!missing(show_progress)) {
        self$show_progress <- show_progress
      }

      # Store total requested n_mcmc as n_mcmc_full, and use n_mcmc as parts
      private$.n_mcmc_full <- self$n_mcmc
      private$.tick_interval <- round(private$.n_mcmc_full / 50)

      # Set up caching (turn into method) ---------------------
      if (!missing(cache_freq)) {
        cache_freq <-
          round(cache_freq)    # Ensure that cache_freq is an integer
        private$.cache_flag = TRUE
        if (missing(cache_dir)) {
          cache_dir <- file.path(tempdir(), 'emcmc')
          print("Specified 'cache_freq', but missing 'cache_dir'")
          cat(paste("Using temporary cache dir: ", cache_dir, sep = "\n"))
        }
        dir.create(cache_dir, recursive = T)
        if (!missing(combine_cached)) {
          self$combine_cached <- combine_cached
        }
        self$cache_dir <- cache_dir
        self$cache_freq <- min(cache_freq, private$.n_mcmc_full)
        cache_ends <-
          seq(cache_freq, private$.n_mcmc_full, by = cache_freq)
        if (!(n_mcmc %in% cache_ends)) {
          cache_ends <- c(cache_ends, private$.n_mcmc_full)
        }
        private$.cache_ends <- cache_ends
        private$.cache_starts <-
          c(1, cache_ends[-length(cache_ends)] + 1)
        self$n_mcmc <- private$.cache_ends[1]
      }
      # End method ---------------------------------------------
      # Ignored samples not stored
      self$allocate_sample_storage()

      # Keep constants
      if (!missing(const)) {
        self$const <- const
      }

      self$prop <- private$.cur

      # Set likelihood function
      self$log_lik <- log_lik

      # Add Normal random walk proposals for any parameters with missing proposals
      if (missing(proposals))
        proposals <- list()

      for (param_name in self$param_names) {
        if (!(param_name %in% names(proposals))) {
          proposals[[param_name]] <- NormalRW$new(prop_var = 0.001,
                                                  blocks = factor(1:length(private$.cur[[param_name]])))
        }
      }
      self$proposals <- proposals
      # Add a reference to the containing MCMC class to each of the proposals
      walk(self$proposals, ~ {
        .x$set_mcmc(self)
      })

      # If proposals are generic (i.e. don't have defined blocks, assume single block)
      for (param_name in self$param_names) {
        if (self$proposals[[param_name]]$ambiguous_blocks) {
          self$proposals[[param_name]]$set_blocks(factor(rep(1, length(
            private$.cur[[param_name]]
          ))))
        }
      }

      # Store data
      self$data <- data

      # Set Parameter Class Objects (contains e.g. prior functions)
      self$set_priors(priors)

      # Adaptive tuning constant
      self$calc_gamma1()

      # Prepare likelihood object (evaluate at current value and cache)
      self$log_lik$set_all_param(param = list_modify(private$.cur, !!!self$const),
                                 data = self$data)
      self$log_lik$cache_param_cll(param_names = self$log_lik$param_names)
      self$cur_lik = self$log_lik$value

      invisible(self)
    },

    #' @description
    #' Calculate gamma1
    #'
    calc_gamma1 =  function() {
      self$adapt_prop_control$gamma1 =
        self$adapt_prop_control$c0 / (self$cur_mcmc_iter +
                                        self$adapt_prop_control$tune_k) ^ self$adapt_prop_control$c1
      invisible(self)
    },

    #' @description
    #' Set Priors
    set_priors = function(priors) {
      # Must be called after self$param_names are defined
      if (missing(priors))
        priors <- list()
     # If no prior is passed (e.g. in middle layers of hierarchical model),
     # then define a null prior for that parameter
      for (param_name in self$param_names) {
        if (!(param_name %in% names(priors))) {
          priors[[param_name]] <- NullPrior$new()
        }
      }

      self$priors <- priors

      invisible(self)
    },

    #' @description
    #' Set Proposal
    #'
    #' @param param_name
    #' @param value
    #'
    #' @return
    set_prop = function(param_name, value) {
      self$prop[[param_name]] <- value
      invisible(self)
    },

    #' @description
    #' Evaluate Proposal Log-likelihood
    #'
    #' @param param_names
    #'
    #' @return
    eval_prop_log_lik = function(param_names) {
      self$log_lik$set_param(
        param_names = param_names,
        param = list_modify(self$prop, !!!self$const),
        data = self$data
      )
      self$prop_lik <- self$log_lik$value
      invisible(self)
    },

    #' @description
    #' Make Proposal
    #'
    #' @param param_name
    #' @param block_level
    #'
    #' @return
    make_proposal = function(param_name, block_level) {
      update_type <- self$proposals[[param_name]]$update_type
      if (update_type == "mh") {
        value <-
          self$proposals[[param_name]]$r_fn(self = self$proposals[[param_name]],
                                            cur = private$.cur[[param_name]])

      }
      else if (update_type == "gibbs") {
        value <-
          self$proposals[[param_name]]$r_fn(self = self$proposals[[param_name]])
      }
      else{
        stop("Proposal 'update_type' must be one of 'mh' or 'gibbs'")
      }
      self$set_prop(param_name, value)

      invisible(self)
    },

    #' @description
    #' Calculate Full Conditional
    #'
    #' @param param_name
    #' @param type
    #'
    #' @return
    full_cond = function(param_name, type) {
      if (type == "cur") {
        lik <- self$cur_lik
        param_value <- private$.cur[[param_name]]
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
    },

    #' @description
    #' Update Parameter
    #'
    #' @param param_name
    #' @param block_level
    #'
    #' @return
    update_param = function(param_name, block_level) {
      # Update proposal block_info (i.e. r_fn and d_fn needs index)
      self$proposals[[param_name]]$set_cur_block_info(block_level)
      update_type <-
        stringr::str_to_lower(self$proposals[[param_name]]$update_type)
      if (update_type == "mh") {
        # Do Metropolis-Hastings Update
        self$mh_update(param_name, block_level)
      }
      if (update_type == "gibbs") {
        # Do Metropolis-Hastings Update
        self$gibbs_update(param_name, block_level)
      }

      invisible(self)
    },

    #' @description
    #' Gibbs Update
    #'
    #' @param param_name
    #' @param block_level
    #'
    #' @return
    gibbs_update = function(param_name, block_level) {
      # Make a Gibbs proposal
      self$make_proposal(param_name, block_level = block_level)
      # Retain sample
      private$.cur[[param_name]] <- self$prop[[param_name]]

      # Mark corresponding log-likelihood terms as having stale cache
      self$log_lik$mark_stale_cll(param_names = param_name)

      invisible(self)
    },

    #' @description
    #' Metropolis-Hastings Update
    #'
    #' @param param_name
    #' @param block_level
    #'
    #' @return
    mh_update = function(param_name, block_level) {
      # If component log-likelihood terms are stale, recalculate likelihood
      self$log_lik$update_stale_cll(
        param_names = param_name,
        param = list_modify(private$.cur,!!!self$const),
        data = self$data
      )

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
      if (self$proposals[[param_name]]$is_asymmetric) {
        mh_num <- sum(self$proposals[[param_name]]$d_fn(
          self$proposals[[param_name]],
          prop = private$.cur[[param_name]],
          cur = self$prop[[param_name]]
        ))
        mh_denom <- sum(self$proposals[[param_name]]$d_fn(
          self$proposals[[param_name]],
          prop = self$prop[[param_name]],
          cur = private$.cur[[param_name]]
        ))
      }
      else{
        mh_num <- mh_denom <- 0
      }

      ##
      ## Accept Case
      ##
      if (log(runif(1)) < prop_fc + mh_num - cur_fc - mh_denom) {
        # Retain sample
        private$.cur[[param_name]] <- self$prop[[param_name]]
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
        self$prop[[param_name]] <- private$.cur[[param_name]]
        # Revert log-likelihood to cached state
        self$log_lik$revert_param_cll(param_name)
        # Update acceptance indicator
        acpt_ind <- 0
      }

      # Calculate new acceptance rate
      self$proposals[[param_name]]$calc_acpt_rt(
        cur_mcmc_iter = self$cur_mcmc_iter,
        acpt_ind = acpt_ind,
        block_level = block_level
      )


      # Update proposal variance
      if (self$cur_mcmc_iter <= self$n_adapt) {
        if (self$proposals[[param_name]]$adapt_prop_var) {
          self$proposals[[param_name]]$tune_prop_var(block_level,
                                                     self$adapt_prop_control$gamma1)
        }
      }
      invisible(self)
    },

    #' @description
    #' Store Sample
    #'
    #' @param param_name
    #'
    #' @return
    store_sample = function(param_name) {
      self$samples[[param_name]][self$sample_row, ] <-
        c(private$.cur[[param_name]])
      invisible(self)
    },

    #' @description
    #' Set Number of MCMC Samples to Draw
    #'
    #' @param n_mcmc
    #'
    #' @return
    set_n_mcmc = function(n_mcmc) {
      self$n_mcmc <- n_mcmc
      self$allocate_sample_storage()
      invisible(self)
    },

    #' @description
    #' Allocate Sample Storage
    #'
    #' @return
    allocate_sample_storage = function() {
      self$sample_row <- 1
      n_ignore <- max(self$n_burnin, self$n_cleared)
      private$set_n_samples(floor((self$n_mcmc - n_ignore) / self$thin_int))
      # Allocate space to store samples
      # matrix: n_samples x length(param)
      self$samples <- self$param_names %>%
        map( ~ matrix(NA,
                      nrow = self$n_samples,
                      ncol = prod(dim(
                        private$.cur[[.x]] %>%
                          as.matrix
                      )))) %>%
        `names<-`(self$param_names)
      invisible(self)
    },

    #' @description
    #' Add n MCMC Samples
    #'
    #' @param n_add_mcmc
    #'
    #' @return
    add_n_mcmc = function(n_add_mcmc) {
      self$n_mcmc <- self$n_mcmc + n_add_mcmc
      self$add_sample_storage(n_add_mcmc)
      invisible(self)
    },

    #' @description
    #' Add Sample Storage
    #'
    #' @param n_add_mcmc
    #'
    #' @return
    add_sample_storage = function(n_add_mcmc) {
      n_ignore <- max(self$n_burnin, self$n_cleared)
      n_total_samples <-
        floor((self$n_mcmc - n_ignore) / self$thin_int)
      n_new_samples <- n_total_samples - self$n_samples
      private$set_n_samples(n_total_samples)
      map(self$param_names,
          ~ {
            self$samples[[.x]] <- rbind(self$samples[[.x]],
                                        matrix(NA,
                                               nrow = n_new_samples,
                                               ncol = prod(dim(
                                                 private$.cur[[.x]] %>%
                                                   as.matrix
                                               ))))
          })
      invisible(self)
    },

    #' @description
    #' Run MCMC
    #'
    #' @return
    run_mcmc = function() {
      if (self$show_progress) {
        # Make progress bar
        private$.progress_bar <- progress::progress_bar$new(
          format = "[:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
          total = private$.n_mcmc_full,
          complete = "=",
          # Completion bar character
          incomplete = "-",
          # Incomplete bar character
          current = ">",
          # Current bar character
          clear = FALSE,
          # If TRUE, clears the bar when finish
          width = 100
        )      # Width of the progress bar
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
                                       c(
                                         zero_pad_strd(private$.cache_starts[private$.cache_part]),
                                         zero_pad_strd(private$.cache_ends[private$.cache_part])
                                       ),
                                       collapse = '_'
                                     ), '.rds')))
        for (cp_ in 2:length(private$.cache_ends)) {
          private$.cache_part <- cp_
          # Free up storage space and run sampler
          self$clear_cache()
          # Calculate size of next mcmc section to run
          cache_part_size <-
            private$.cache_ends[private$.cache_part] -
            private$.cache_starts[private$.cache_part] + 1
          # Allocate space for storing samples of next section
          self$add_n_mcmc(cache_part_size)
          # Run next part
          self$run_mcmc_part()
          # Write current mcmc samples to a file (cache them)
          self$write_to_file(file.path(self$cache_dir,
                                       paste0(paste0(
                                         c(
                                           zero_pad_strd(private$.cache_starts[private$.cache_part]),
                                           zero_pad_strd(private$.cache_ends[private$.cache_part])
                                         ),
                                         collapse = '_'
                                       ), '.rds')))
        }
        if (private$.cache_flag && self$combine_cached) {
          self$clear_cache()
          self$prepend_stored_samples(self$saved_file_paths)
        }
      }
      ##
      ## End Caching loop
      ##
      invisible(self)
    },

    #' @description
    #' Run MCMC Part
    #'
    #' @return
    run_mcmc_part = function() {
      if (self$sample_row <= self$n_samples) {
        start_mcmc_iter <- self$cur_mcmc_iter
        ##
        ## Main MCMC Loop
        ##
        for (cur_mcmc_iter in start_mcmc_iter:self$n_mcmc) {
          self$cur_mcmc_iter <- cur_mcmc_iter
          if (self$show_progress &&
              (cur_mcmc_iter %% private$.tick_interval == 0)) {
            # Update progress bar
            private$.progress_bar$tick(private$.tick_interval)
          }
          if (self$cur_mcmc_iter <= self$n_adapt) {
            # Update variance tuning parameters
            self$calc_gamma1()
          }

          ## Iterate through parameters and update each parameter and block
          ## i.e. one mcmc iteration
          for (param_name in self$param_names) {
            for (block_level in self$proposals[[param_name]]$block_levels) {
              self$update_param(param_name = param_name,
                                block_level = block_level)
            }
          }

          ## Store samples
          if ((self$cur_mcmc_iter > self$n_burnin) &
              (self$cur_mcmc_iter %% self$thin_int == 0)) {
            for (param_name in self$param_names) {
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
    },

    #' @description
    #' Clear Cache
    #'
    #' @return
    clear_cache = function() {
      self$n_cleared <- self$n_mcmc
      self$sample_row <- 1
      self$allocate_sample_storage()
      invisible(self)
    },

    #' @description
    #' Write Samples to File
    #'
    #' @param file_path
    #' @param ...
    #'
    #' @return
    write_to_file = function(file_path, ...) {
      # Record archive saved file paths
      if (is.null(self$saved_file_paths)) {
        self$saved_file_paths <- list(file_path)
      }
      else{
        self$saved_file_paths <- c(self$saved_file_paths, file_path)
      }
      saveRDS(self, file = file.path(file_path, ...))
      invisible(self)
    },

    #' @description
    #' Prepennd Samples to Sampler Storage
    #'
    #' @param samples
    #'
    #' @return
    prepend_samples = function(samples) {
      self$samples <- map(names(self$samples), ~ {
        rbind(samples[[.x]], self$samples[[.x]])
      }) %>%
        `names<-`(names(self$samples))
      # Update sample row
      self$sample_row <- self$sample_row + nrow(samples[[1]])
      private$set_n_samples(nrow(self$samples[[1]]))
      invisible(self)
    },

    #' @description
    #' Prepend Stored Samples
    #'
    #' @param file_paths
    #'
    #' @return
    prepend_stored_samples = function(file_paths) {
      # Order of file-paths should match order of storage
      # (i.e. older sample files are listed first)
      map(rev(file_paths), ~ {
        prev_sampler <- readRDS(.x)
        self$prepend_samples(prev_sampler$samples)
      })
      invisible(self)
    },

    #' @description
    #'
    #' @return (list) of transposed samples. `\code{$samples}` contains a named
    #' list of matrices where the names correspond to sampled parameters, and
    #' each row of the matrices corresponds to a single mcmc iteration. This
    #' function 'transposes' this nesting structure, returning a nested list.
    #' Each element of the outer list corresponds now to a single mcmc iteration,
    #' and the inner list gives the samples for each parameter for that iteration.
    #' @export
    t_samples = function() {
      return(purrr::map(self$samples,
                        ~ .x %>%
                          array_branch(1)) %>%
               purrr::transpose())
    },


    #' @description
    #' Summarize Posterior Samples
    #'
    #' @param probs (vector of Real numbers between [0,1])
    #'
    #' @return (named list of tibbles) for each parameter summarizing posterior
    #' samples.
    post_summary = function(probs = c(0.025, 0.975)) {
      post_list <- map2(self$samples, names(self$samples),
                        ~ {
                          df <- as.data.frame(.x)
                          index <- 1:ncol(df)
                          suffix_flag <- F
                          if (ncol(.x) > 1) {
                            suffix_flag <- T
                          }

                          colnames(df) <- index
                          df %>%
                            pivot_longer(cols = everything()) %>%
                            mutate(index = as.numeric(name),
                                   name = if_else(
                                     rep(suffix_flag, length(name)),
                                     paste0(.y, '[', name, ']'),
                                     rep(.y, length(name))
                                   )) %>%
                            group_by(name) %>%
                            group_modify(~ {
                              .x %>%
                                summarize(
                                  mean = mean(value),
                                  sd = sd(value),
                                  q = quantile(value, probs = probs),
                                  percentile = probs * 100
                                ) %>%
                                mutate(index = .x$index[1]) %>%
                                pivot_wider(
                                  names_from = percentile,
                                  values_from = q,
                                  names_glue = "{.name}%"
                                )
                            }) %>%
                            relocate(index, .after = name) %>%
                            arrange(index)

                        })
      return(post_list)
    },

    ##
    ## Plotting
    ##
    #' @description
    #' Plot Posterior Histograms
    #'
    #' @return
    plot_post_histogram = function() {
      plotlist <- map2(self$samples, names(self$samples),
                       ~ {
                         df <- as.data.frame(.x)
                         if (ncol(.x) > 1) {
                           suffix <- paste0('[', 1:ncol(df), ']')
                         }
                         else{
                           suffix <- ''
                         }

                         colnames(df) <- paste0(.y, suffix)
                         df %>%
                           pivot_longer(cols = everything()) %>%
                           ggplot() +
                           geom_histogram(aes(x = value)) +
                           facet_wrap( ~ name, scales = "free") +
                           theme_bw() +
                           theme(strip.text.x = element_text(margin = margin(0.2, 0, 0.2, 0, "pt")))
                       })
      return(plotlist)
    },

    #' @description
    #' Plot Posterior Traceplots
    #'
    #' @return
    plot_post_trace = function() {
      plotlist <- map2(self$samples, names(self$samples),
                       ~ {
                         df <- as.data.frame(.x)
                         if (ncol(.x) > 1) {
                           suffix <- paste0('[', 1:ncol(df), ']')
                         }
                         else{
                           suffix <- ''
                         }

                         colnames(df) <- paste0(.y, suffix)
                         df %>%
                           mutate(sample_index = 1:nrow(df)) %>%
                           pivot_longer(cols = -sample_index) %>%
                           ggplot() +
                           geom_line(aes(x = sample_index, y = value)) +
                           facet_wrap( ~ name, scales = "free") +
                           labs(x = "Iteration", y = "Value") +
                           theme_bw() +
                           theme(strip.text.x = element_text(margin = margin(0.2, 0, 0.2, 0, "pt")))
                       })
      return(plotlist)
    }


  ),


  ##########################################################################
  ## Private Attributes
  ##########################################################################
  private = list(
    .cur = NULL,
    .n_samples = NULL,
    .cache_flag = FALSE,
    .cache_ends = NA,
    .cache_starts = NA,
    .cache_part = NA,
    .n_mcmc_full = NA,
    .progress_bar = NULL,
    .tick_interval = NA,

    ##########################################################################
    ## Private Methods
    ##########################################################################
    set_n_samples = function(n_samples) {
      private$.n_samples = n_samples
    }
  ),
  ##########################################################################
  ## Active Fields
  ##########################################################################
  active = list(
    # Read only fields
    n_samples = function(value) {
      if (missing(value)) {
        private$.n_samples
      }
      else{
        stop("$n_samples is read only", call. = F)
      }
    },
    cur = function(value) {
      if (missing(value)) {
        private$.cur
      }
      else{
        stop("$cur is read only", call. = F)
      }
    }
  )
)
