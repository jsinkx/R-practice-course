f <- function(x) {
  return(x %^% 2 - 2 *x)
}

A <- matrix(c(4, -3, 9, 1), nrow = 2, byrow = TRUE)
f_A <- f(A)

cat("f(A) = A^2 - 2A =\n")
print(f_A)