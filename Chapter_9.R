# Markov Chain Monte Carlo
library(rethinking)
# library(rstan)



# special case of General Metropolis Algorithm
# In real world MCMC algorithms are used to sample from complex distributions like posterior probability distributions
# possible islands

islands <- seq(1, 10, 1)
print(islands)


# randomly choose an island
i_0 <- sample(islands,1)

# for loop

island_visited <- c() # empty array to store the islands visited
N <- 100000  # number of steps

for (i in 1:N){

    # toss a coin
    p0 <- sample(c(-1,1),1)

    # if head move left, if tails move right
    if (p0 == -1) {
        i_p <- i_0 - 1
        if ( i_p < 1) {
            i_p <- 10
        }
    } else {
        i_p <- i_0 + 1
        if (i_p > 10) {
            i_p <- 1
        }
    }

    # move for sure if seashells > stones, move with prob of (#seashells)/(#stones) if seashells > stones
    if (i_p > i_0) {
        i_next <- i_p
    } else {
        if (runif(1) < i_p/i_0) {
            i_next <- i_p
        } else {
            i_next <- i_0
        }
    }

    i_0 <- i_next
    island_visited <- c(island_visited, i_0)
}

# print(island_visited)
# plot # times island visited in a bar chart
hist(island_visited, breaks = seq(0.5, 10.5, 1), col = "lightblue", xlab = "Island", ylab = "Frequency", main = "Island visited")

# count of islands visited
island_count <- table(island_visited)
print(island_count)

# high dimension problem
D <- 1000
T <- 1e3
Y <- rmvnorm(T, rep(0,D), diag(D))
rad_dist <- function(Y){sqrt(sum(Y^2))}
print(rad_dist(Y)) # far from 0 (mode)
Rd <- sapply(1:T, function(i) rad_dist(Y[i,]))
dens(Rd)


# fit a model using quap
data(rugged)
str(rugged)
d <- rugged
d$log_gdp <- log(d$rgdppc_2000)
dd <- d[complete.cases(d$rgdppc_2000),]
dd$log_gdp_std <- dd$log_gdp/mean(dd$log_gdp)
dd$rugged_std <- dd$rugged/max(dd$rugged)
dd$cid <- ifelse(dd$cont_africa==1,1,2)

m8.3 <- quap(alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a[cid] + b[cid]* (rugged_std - 0.215),
    a[cid] ~ dnorm(1,0.1),
    b[cid] ~ dnorm(0,0.3),
    sigma ~ dexp(1)
), data = dd)

precis(m8.3, depth = 2)

data_slim <- list(
    log_gdp_std <- dd$log_gdp_std,
    rugged_std <- dd$rugged_std,
    cid <- as.integer(dd$cid)
)
str(data_slim)


m9.1 <- ulam(alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a[cid] + b[cid]* (rugged_std - 0.215),
    a[cid] ~ dnorm(1,0.1),
    b[cid] ~ dnorm(0,0.3),
    sigma ~ dexp(1)
), data = data_slim, chains = 1)
