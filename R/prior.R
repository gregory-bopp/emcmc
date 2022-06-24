Prior <- R6::R6Class('Prior',
            list(
              name = NA
            )
)

Prior$set('public',
          'initialize',
function(name){
  stopifnot(is.character(name))
  self$name <- name
  invisible(self)
}
)


