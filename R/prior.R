Prior <- R6::R6Class('Prior',
                     list(
                       name = NA,
                       #' @description
                       #' Create New Prior Object
                       #'
                       #' @param name
                       #'
                       #' @return
                       initialize = function(name) {
                         stopifnot(is.character(name))
                         self$name <- name
                         invisible(self)
                       }
                     ))
