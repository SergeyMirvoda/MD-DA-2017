# Задание 1
# 1
exp.1 <- rexp(n = 200)
exp.1.mean <- mean(exp.1)
exp.1.sd <- sd(exp.1)

# 2
exp.0.1 <- rexp(n = 200, rate = 0.1)
exp.0.5 <- rexp(n = 200, rate = 0.5)
exp.5 <- rexp(n = 200, rate = 5)
exp.10 <- rexp(n = 200, rate = 10)

# 3
hist(exp.1)

# 4
plot(exp.1)

# 5
plot(exp.10, exp.0.1)

# 6
exp.means <- c(mean(exp.1), mean(exp.0.1), mean(exp.0.5), mean(exp.5), mean(exp.10))

# 7
exp.parmtrs <- c(1, 0.1, 0.5, 5, 10)
exp.sds <- c(sd(exp.1), sd(exp.0.1), sd(exp.0.5), sd(exp.5), sd(exp.10))

plot(exp.means, exp.parmtrs)
plot(exp.sds, exp.parmtrs)
plot(exp.means, exp.sds)

#   Задание 2

# 1
huge.exp.1 <- rexp(1100000)
huge.exp.1.mean <- mean(huge.exp.1)
huge.exp.1.sd <- sd(huge.exp.1)

# 2
plot(0:15, type = "l", 1 - exp(-x))
hist(huge.exp.1)

# 3
mean(huge.exp.1[huge.exp.1 > 1])

# 4
huge.exp.1.mat <- matrix(huge.exp.1, 1100, 1000)
hist(huge.exp.1.mat)

# 5
colMeans(huge.exp.1.mat)[137]

# 6
huge.exp.1.mat.mean <- colMeans(huge.exp.1.mat)
barplot(huge.exp.1.mat.mean)
# 7
huge.exp.1.square <- huge.exp.1 ^ 2
huge.exp.1.square.mean <- mean(huge.exp.1.square)
huge.exp.1.square.sd <- sd(huge.exp.1.square)

huge.exp.1.square.mean - huge.exp.1.mean
huge.exp.1.square.sd - huge.exp.1.sd