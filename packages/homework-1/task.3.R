A <- matrix(c(1, 0, 3, -4, 2, 1), nrow = 3, byrow = TRUE)
B <- matrix(c(1, -1, 2, 3, 1, -5), nrow = 3, byrow = TRUE)
C <- matrix(c(3, 4, 1, -3, 8, 6), nrow = 3, byrow = TRUE)
result <- 3 * A + 4 * B - 2 * C

cat("3A + 4B - 2C =\n")
print(result)