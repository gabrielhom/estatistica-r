p <- 0.8
m <- p
n <- 25
sd <- (p * (1 - p)) / n
cat("4) b)\n")
pnorm(0.75, m, sd)
pnorm(0.85, m, sd, lower.tail = FALSE)


cat("\n5) Normal(100, 10):\n")

m <- 100
sd <- 10

cat("a) P(90 < X < 110): ")
round(pnorm(110, m, sd) - pnorm(90, m, sd), 2)

cat("b) P(90 < X < 110), n = 16: ")
sd <- sd / sqrt(16)
round(pnorm(110, m, sd, 16) - pnorm(90, m, sd, 16), 2)

cat("6) Normal(m, 10):\n")

sd <- 10
# z = (x - m) / sd
cat("a) Média para que apenas 10% esteja abaixo de 500g: ")
m <- 500 + abs(qnorm(0.1) * sd)
m

cat("\nb) Probabilidade de 4 amostras somadas serem menor que 2kg: ")
round(pnorm(500, m, sd / sqrt(4)), 4)

cat("7)\n")
cat("Y ∼ t20\n")
df <- 20
cat("P(−2.85 ≤ Y ≤ 2.85): ")
pt(2.85, df) - pt(-2.85, df)

cat("P(Y < −2.85): ")
pt(-2.85, df)

cat("P(Y > 2.85): ")
pt(2.85, df, lower.tail = FALSE)

cat("P(Y > 2.12): ")
pt(2.12, df, lower.tail = FALSE)

cat("P(Y < −3.01): ")
pt(-3.01, df)

cat("\nY ∼ x16\n")
df <- 16

cat("P(8.91 < Y < 32.85: ")
pchisq(32.85, df) - pchisq(8.91, df)
cat("P(Y > 8.91): ")
pchisq(8.91, df, lower.tail = FALSE)

cat("\nY ∼ F(10,7)\n")
df1 <- 10
df2 <- 7
cat("P(Y > 3.18): ")
pf(3.18, df1, df2, lower.tail = FALSE)

cat("P(Y > 0.15): ")
pf(0.15, df1, df2, lower.tail = FALSE)


cat("9)\n")

n <- 100
sd <- 15 / sqrt(n)
m <- 0
cat("a) ")
pnorm(2, sd = sd) - pnorm(-2, sd = sd)
cat("b) ")
sd <- 10 / sqrt(n)
pnorm(2, sd = sd) - pnorm(-2, sd = sd)

cat("\n10) Normal(1050, 200)\n")
m <- 1050
sd <- 200

cat("a) i) ")
pnorm(1200, m, sd)

cat("a) ii) ")
pnorm(1200, m, sd, lower.tail = FALSE)

cat("b) P(X > 1400|X > 1200): ")
pnorm(1400, m, sd, lower.tail = FALSE) / pnorm(1200, m, sd, lower.tail = FALSE)

n <- 25
cat("c) ")
dnorm(1200, m, sd / sqrt(n))
