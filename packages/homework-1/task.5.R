multiplication_matrix <- function(matrix_1, matrix_2) {
  if (ncol(matrix_1) == nrow(matrix_2)) {
    return (matrix_1 %*% matrix_2)
  }
  
  return (FALSE)
}

# ---- Часть а ----

A <- matrix(c(1,0,2,-3,2,1,0,0,1), nrow = 3, byrow = TRUE)
B <- matrix(c(-1,3,1,0,1,1,-2,1,3), nrow = 3, byrow = TRUE)

cat("Произведение AB =\n")
print(multiplication_matrix(A, B))

# ---- Часть б ----

A <- matrix(c(2, 1, -2, 3, -4, 2, 1, 0, 0), nrow = 3, byrow = TRUE)
B <- matrix(c(2, 0, 1, 1, 0, -2), ncol = 2, byrow = TRUE)

cat("Произведение AB =\n")
print(multiplication_matrix(A ,B))
# ---- Часть в ----

A <- matrix(c(1, -1, 2), nrow = 3, byrow = TRUE)
B <- matrix(c(3, 4, 1), ncol = 3, byrow = TRUE)

cat("Произведение AB =\n")
print(multiplication_matrix(A, B))

# ---- Часть г ----

A <- matrix(c(1, 2, 3, 4, 2, -1, -3, 0), nrow = 2, byrow = TRUE)
B <- matrix(c(1, 0, 1, 2), nrow = 4, byrow = TRUE)

cat("Произведение AB =\n")
print(multiplication_matrix(A, B))