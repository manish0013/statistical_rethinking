## 2M1


# create a grid
p_grid <- seq(from = 0, to = 1, length.out = 20)
p_grid

# set uniform priors
priors <- rep(1,20)

# compute likelihood
# likelihood <- dbinom(x = 3,size = 3,prob = p_grid)
# likelihood <- dbinom(x = 3,size = 4,prob = p_grid)
likelihood <- dbinom(x = 5,size = 7,prob = p_grid)

# calculate unstandardized posterior
unstd.posterior <- likelihood * priors

# standardize posterior
posterior <- unstd.posterior/sum(unstd.posterior)

# plot
plot(p_grid,posterior,type = "b",xlab = "probability of water",ylab = "posterior probability")
mtext("20 points")


## 2M2
# create a grid
p_grid <- seq(from = 0, to = 1, length.out = 20)
p_grid

# set uniform priors
priors <- ifelse(p_grid<0.5,0,1)
priors
# compute likelihood
likelihood <- dbinom(x = 3,size = 3,prob = p_grid)
# likelihood <- dbinom(x = 3,size = 4,prob = p_grid)
# likelihood <- dbinom(x = 5,size = 7,prob = p_grid)

# calculate unstandardized posterior
unstd.posterior <- likelihood * priors

# standardize posterior
posterior <- unstd.posterior/sum(unstd.posterior)

# plot
plot(p_grid,posterior,type = "b",xlab = "probability of water",ylab = "posterior probability")
mtext("20 points")


## 2M3
p.land.earth = .3
p.land.mars = 1.0
p.earth = 0.5
p.mars = 0.5
# probability of earth|land
p.land = p.land.earth * p.earth + p.land.mars * p.mars

p.earth.given.land = (p.land.earth * p.earth)/p.land
p.earth.given.land


## 2M4
count.card1.total <- 2
count.card2.total <- 1
count.card3.total <- 0

count.card1.black <- 2
count.card2.black <- 0
count.card3.black <- 0

prob <- (count.card1.black + count.card2.black)/(count.card1.total + count.card2.total)
prob


## 2M5
count.card1.total <- 2
count.card2.total <- 1
count.card3.total <- 0
count.card4.total <- 2

count.card1.black <- 2
count.card2.black <- 0
count.card3.black <- 0
count.card4.black <- 2

prob <- (count.card1.black + count.card2.black + count.card3.black + count.card4.black)/(count.card1.total
                                                                                         + count.card2.total
                                                                                         + count.card3.total
                                                                                         + count.card4.total)
prob

## 2M6

ways.card1 = 1
ways.card2 = 2
ways.card3 = 3

count.card1.total <- 2
count.card2.total <- 1
count.card3.total <- 0

count.card1.black <- 2
count.card2.black <- 0
count.card3.black <- 0

prob <- (ways.card1 * count.card1.black + ways.card2 * count.card2.black)/(ways.card1*count.card1.total
                                                                           + ways.card2*count.card2.total)
prob

## 2M7

ways.black1 <- 3
ways.white2 <- 8

ways.black1.black <- 6
ways.black1.white <- 2

prob <- 6/8
prob


## 2H1

prob.A = 0.5
prob.B = 0.5

prob.twin.given.A = .1
prob.twin.given.B = .2

prob.twins.given.A = prob.twin.given.A * prob.twin.given.A
prob.twins.given.B = prob.twin.given.B * prob.twin.given.B

prob.twin.given.twin = (prob.twins.given.A * prob.A + prob.twins.given.B * prob.B) / ((prob.twin.given.A) * prob.A 
                                                                                    + (prob.twin.given.B) * prob.B)

prob.twin.given.twin


## 2H2
prob.A.given.twin = (prob.twin.given.A * prob.A)/ ((prob.twin.given.A * prob.A) + (prob.twin.given.B * prob.B))
prob.A.given.twin


## 2H3 

likelihood_twin <- c(prob.twin.given.A,prob.twin.given.B)
prior <- c(1,1)
posterior <- likelihood_twin* prior
posterior <- posterior/sum(posterior)

# another bayesian update
likelihood_single <- c(1-prob.twin.given.A, 1  - prob.twin.given.B)
prior <- posterior
posterior <- likelihood_single* prior
posterior <- posterior/sum(posterior)
posterior[1]



## 2H 4

likelihood <- c(0.8,0.35)
prior <- c(1,1)
posterior <- likelihood* prior
posterior <- posterior/sum(posterior)
posterior

likelihood_twin <- c(prob.twin.given.A,prob.twin.given.B)
prior <- posterior
posterior <- likelihood_twin*prior
posterior <- posterior/sum(posterior)
posterior

posterior[1]
