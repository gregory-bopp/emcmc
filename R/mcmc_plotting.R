
##
## Plotting
##
MCMC$set("public",
         "plot_post_histogram",
         function() {
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
                                theme(strip.text.x =element_text(margin = margin(0.2,0,0.2,0, "pt")))
                            })
           return(plotlist)
         }
)


MCMC$set("public",
         "plot_post_trace",
         function() {
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
                                theme(strip.text.x =element_text(margin = margin(0.2,0,0.2,0, "pt")))
                            })
           return(plotlist)
         }
)
