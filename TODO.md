# TODO:
# General
  - Document methods
  - Add examples
  - Adaptive block proposals
  - Add checking that object types passed to LogLik$new() are CompLogLik

# MCMC
  - Fix progress bar when more samples are added with MCMC$add_n_mcmc()
  - Think about which variables should be public and which should be private
  - Rewrite run_mcmc to better handle tracking of caching (i.e. what if user
    wants to add more runs, but still use caching; or what if user wants to turn 
    caching off)
    


