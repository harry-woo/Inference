REP <- 1000
n <- 20
mu <- 5
sigma <- sqrt(2)

sample_mean <- c()
sample_var <- c()
bias_mean <- c()
bias_var <- c()

for(i in 1:REP) {
  x = rnorm(n, mean = mu, sd = sigma)
  sample_mean[i] <- mean(x)
  sample_var[i] <- var(x)
  bias_mean[i] <- mu - sample_mean[i]
  bias_var[i] <- sigma^2 - sample_var[i]
}

df <- data.frame(sample_mean , bias_mean, sample_var, bias_var)
df %>% summarise_each(mean)

library(ggplot2)
library(gridExtra)

plot1 <- ggplot(data = df, aes(x = sample_mean, y = ..density..)) +
  geom_histogram(fill = "gray", colour = "black", bins = 15) +
  geom_vline(xintercept = mu, col = "red", lwd = 1, lty = 2) +
  xlab(expression(bar(X))) + ylab("Density") +
  ggtitle(label = "Histogram of Sample Mean")
 
plot2 <- ggplot(data = df, aes(x = sample_var, y = ..density..)) +
  geom_histogram(fill = "gray", colour = "black", bins = 15) +
  geom_vline(xintercept = sigma^2, col = "red", lwd = 1, lty = 2) +
  xlab(expression(S^2)) + ylab("Density") +
  ggtitle(label = "Histogram of Sample Var")

grid.arrange(plot1, plot2, ncol = 2)


n2 <- seq(2, 100)

mse_varsam <- (2 * sigma^4)/(n2 - 1)
mse_varhat <- (((2 * n2) -1)/(n2)^2) * sigma^4

data.frame(n2, mse_varsam, mse_varhat) %>% 
  mutate(compare = mse_varsam > mse_varhat) %>% 
  select(compare) %>% table()

df2 <- rbind(data.frame(size = n2, MSE = mse_varsam, group = "Var_sample"),
             data.frame(size = n2, MSE = mse_varhat, group = "Var_hat"))

ggplot(data = df2, aes(x = size, y = MSE, colour = group)) +
  geom_line(lwd = 1) +
  xlab("Sample size") + ylab("MSE") +
  ggtitle(label = "MSE by sample size")


set.seed(123)

n_100 <- rbinom(100, size = 1, prob = 0.8)
n_1000 <- rbinom(1000, size = 1, prob = 0.8)
n_10000 <- rbinom(10000, size = 1, prob = 0.8)

mle_p <- function(n){sum(n)/length(n)}
data.frame(n_100 = mle_p(n_100), n_1000 = mle_p(n_1000), n_10000 = mle_p(n_10000))

data.frame(n_100, n_1000, n_10000) %>% 
  summarise_each(mean)

install.packages("bbmle")

mlogL_100 <- function(p){- sum(n_100) * log(p) - (100 - sum(n_100)) * log(1-p)}
mlogL_1000 <- function(p){- sum(n_1000) * log(p) - (1000 - sum(n_1000)) * log(1-p)}
mlogL_10000 <- function(p){- sum(n_10000) * log(p) - (10000 - sum(n_10000)) * log(1-p)}

mlogL <- function(p, n){
  ber <- rbinom(n, size = 1, prob = 0.8)
  ll <- - sum(ber) * log(p) - (n - sum(ber)) * log(1-p)
  return(ll)}

mle(mlogL, start = list(p = 0.1), fixed = list(n = 100))
mle(mlogL, start = list(p = 0.1), fixed = list(n = 1000))
mle(mlogL, start = list(p = 0.1), fixed = list(n = 10000))

warnings()

mle(mlogL_100, start = list(p = 0.2))
mle(mlogL_1000, start = list(p = 0.1))
mle(mlogL_10000, start = list(p = 0.1))

library(bbmle)
rbetabinom


optimi

set.seed(123)

n_100 <- rbinom(100, size = 1, prob = 0.8)
n_1000 <- rbinom(1000, size = 1, prob = 0.8)
n_10000 <- rbinom(10000, size = 1, prob = 0.8)

mlogL <- function(p,x){
  -sum(dbinom(x, size = 1, prob = p, log = TRUE))
}

r1 <- optimize(mlogL, interval = c(0,1), x = n_100)
r2 <- optimize(mlogL, interval = c(0,1), x = n_1000)
r3 <- optimize(mlogL, interval = c(0,1), x = n_10000)

data.frame(n_size = c(100, 1000, 10000),
           MLE_by_R = c(r1$minimum, r2$minimum, r3$minimum),
           mean = c(mean(n_100), mean(n_1000), mean(n_10000))) %>% 
  mutate(diff = MLE_by_R - mean)
