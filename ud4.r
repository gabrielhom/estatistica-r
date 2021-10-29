cat("3) Binomial n = 15, p = 0.4\n")
n <- 15
p <- 0.4

# lower.tail por padrão é TRUE

cat("a) P(Y >= 14): ")
round(pbinom(13, n, p, lower.tail = FALSE), 3)

cat("b) P(8 < Y <= 10): ")
round(sum(dbinom(9:10, n, p)), 3)

cat("c) P(Y < 2 ou Y >= 11): ")
round(pbinom(1, n, p) + pbinom(10, n, p, lower.tail = FALSE), 3)

cat("d) P(Y >= 11 ou Y > 13): ")
round(pbinom(10, n, p, lower.tail = FALSE) +
      pbinom(13, n, p, lower.tail = FALSE), 3)

cat("e) P(Y > 3 e Y < 6): ")
round(sum(dbinom(4:5, n, p)), 3)

cat("f) P(Y <= 13|Y >= 11): ")
# P(Y <= 13|Y >= 11) = P(11 <= Y <= 13) / P(Y >= 11)
round(sum(dbinom(11:13, n, p)) / pbinom(10, n, p, lower.tail = FALSE), 4)

cat("\n4) Binomial n = 15, p = 0.8\n")
n <- 15
p <- 0.8

cat("a) P(Y = 15): ")
round(dbinom(15, n, p), 3)

cat("b) P(Y <= 13): ")
round(pbinom(13, n, p), 3)

cat("c) P(Y >= 10): ")
round(pbinom(9, n, p, lower.tail = FALSE), 3)

cat("\n20) Exponencial lambda = 1/20, P(T > 15|T > 10): ")
lambda <- 1 / 20
pexp(15, lambda, lower.tail = FALSE) / pexp(10, lambda, lower.tail = FALSE)

cat("\n21) Normal media = 4, desvio padrao = 1\n")
m <- 4
sd <- 1

cat("a) P(X <= 4): ")
round(pnorm(4, m, sd), 3)

cat("b) P(4 < Y < 5): ")
round(pnorm(5, m, sd) - pnorm(4, m, sd), 3)

cat("c) P(2 <= Y < 5): ")
round(pnorm(5, m, sd) - pnorm(2, m, sd), 3)

cat("d) P(5 <= Y <= 7): ")
round(pnorm(7, m, sd) - pnorm(5, m, sd), 3)

cat("e) P(Y <= 1): ")
round(pnorm(1, m, sd), 3)

cat("f) P(0 <= Y <= 2): ")
round(pnorm(2, m, sd), 3)

cat("\n26) Binomial n = 10, p = 0.1\n")
n <- 10
p <- 0.1

cat("b) P(Y = 3):")
round(dbinom(3, n, p), 4)
cat("c) P(Y <= 1): ")
round(pbinom(1, n, p), 4)
cat("d) E(X) e DP(X):\n")
n * p
round(sqrt(n * p * (1 - p)), 4)


cat("\n28) Normal(0,1)\n")
m <- 0
sd <- 1

cat("a) P(Z > z) = 0.119: ")
round(qnorm(1 - 0.119, m, sd), 2)

cat("b) P(Z < z) = 0.8051: ")
round(qnorm(0.8051, m, sd), 2)


cat("\n30) Normal media = 95, desvio padrao = 7.5\n")
m <- 95
sd <- 7.5

cat("a) P(Y > b) = 0.2611: ")
round(qnorm(1 - 0.2611, m, sd), 2)

cat("b) P(Y < b) = 0.9750: ")
round(qnorm(0.9750, m, sd), 2)