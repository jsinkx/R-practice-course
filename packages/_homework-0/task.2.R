A <- matrix(c(2, -3, 4, 7, 6, -5, -1, 8, 9), nrow = 3, byrow = TRUE)
B <- matrix(c(-1, 3, -4, -7, -5, 5, 1, -8, -8), nrow = 3, byrow = TRUE)
result <- A + B

cat("A + B =\n")
print(result)
