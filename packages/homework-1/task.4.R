# ---- Часть а ----

A <- matrix(1:12, nrow = 3, ncol = 4)
B <- matrix(1:20, nrow = 4, ncol = 5)
C <- A %*% B

dim(C)

# ---- Часть б ----

A <- matrix(1:6, nrow = 2, ncol = 3)
B <- matrix(1:18, nrow = 3, ncol = 6)
C <- A %*% B

dim(C)