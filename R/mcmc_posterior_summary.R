MCMC$set("public",
         "post_summary",
         function(probs = c(0.025, 0.975)) {
           plotlist <- map2(self$samples, names(self$samples),
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
                                       name = if_else(rep(suffix_flag, length(name)),
                                                      paste0(.y, '[', name, ']'),
                                                      rep(.y, length(name)))) %>%
                                group_by(name) %>%
                                group_modify(~{
                                  .x %>%
                                    summarize(
                                      mean = mean(value),
                                      sd = sd(value),
                                      q = quantile(value, probs = probs),
                                      percentile = probs*100) %>%
                                    mutate(index = .x$index[1]) %>%
                                    pivot_wider(
                                      names_from = percentile,
                                      values_from = q,
                                      names_glue = "{.name}%")
                                }) %>%
                                relocate(index, .after = name) %>%
                                arrange(index)

                            })
           return(plotlist)
         }
)
