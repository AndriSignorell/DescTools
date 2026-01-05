

# internal function to restore settings after a plot has been created

.withGraphicsState <- function(expr) {
  
  op <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::layout(matrix(1))
    graphics::par(op)
  }, add = TRUE)
  force(expr)
  
}

