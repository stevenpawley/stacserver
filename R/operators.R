# Helper operator for NULL coalescing.
#
# base R gained `%||%` in 4.4.0, but the package supports R >= 4.1.0.
`%||%` <- function(a, b) {
  if (is.null(a))
    b
  else
    a
}
