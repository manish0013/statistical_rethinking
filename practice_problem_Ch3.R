p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1,1000)
likelihood<- dbinom(6,size = 9, prob = p_grid)
posterior <- likelihood * prior
posterior<- posterior/sum(posterior)
set.seed(100)
samples <- sample(p_grid,size = 10000, prob = posterior,replace = TRUE)

# 3E1 posterior probability below 0.2
sum(samples<0.2)/10000


# 3E2 posterior probability above 0.8
sum(samples>0.8)/10000

# 3E3 posterior probability between 0.2 and 0.8
sum((samples>0.2)&(samples < 0.8))/10000

#3E4 20% of the posterior probability lies below which value of p
quantile(samples,0.2)

#3E5 20% of the posterior probability lies above which value of p
quantile(samples,0.8)


# 3E6 narrowest interval equal to 66% of the prbability
library(rethinking)
HPDI(samples, prob = 0.66)

# 3E7 values of p which contain 66% of the posterior probability, assuming equal posterior probability both below 
# and above the interval
PI(samples, prob = .66)



# 3M1 8 water in 15 tosses, posterior distribution
p_grid <- seq(from = 0, to = 1, length.out = 10000)
prior <- rep(1,10000)
likelihood <- dbinom(8,15,prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior/sum(posterior)

# 3M2 90% HPDI from 10000 samples
set.seed(100)
samples <- sample(p_grid, size = 10000,prob = posterior, replace = TRUE)
HPDI(samples,0.9)


# 3M3 posterior predictive check, prob of obtaining 8 water in 15 tosses
posterior_check <- rbinom(10000,size = 15,prob = samples)

sum((posterior_check == 8))/10000

# 3M4 Using same distribution, prob of obtaining 6 water in 9 tosses
posterior_check <- rbinom(10000,size = 9,prob = samples)

sum((posterior_check == 6))/10000

mean(samples)  # 0.53
# 3M5
p_grid <- seq(from = 0, to = 1, length.out = 10000)
prior <- rep(1,10000)
prior[p_grid < 0.5] <- 0
likelihood <- dbinom(8,15,prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior/sum(posterior)

# 90% HPDI
samples <- sample(p_grid, size = 10000,prob = posterior, replace = TRUE)
HPDI(samples,0.9)

# posterior predictive check, prob of obtaining 8 water in 15 tosses
posterior_check <- rbinom(10000,size = 15,prob = samples)

sum((posterior_check == 8))/10000

# Using same distribution, prob of obtaining 6 water in 9 tosses
posterior_check <- rbinom(10000,size = 9,prob = samples)

sum((posterior_check == 6))/10000
mean(samples)  # 0.60
# the above estimate of p is closer to the true value, this is because of we've better infromed priors

# 3M6 - # of times we need to toss to get 99% interval to be only 0.05 wide
for ( n in seq(100,3000,by = 100)){
        tosses <- rbinom(n,1,prob = 0.7)
        p_grid <- seq(from = 0, to = 1, length.out = 10000)
        prior <- rep(1,10000)
        likelihood <- dbinom(sum(tosses),length(tosses),prob = p_grid)
        posterior <- likelihood * prior
        posterior <- posterior/sum(posterior)
        
        # 3M2 90% HPDI from 10000 samples
        set.seed(100)
        samples <- sample(p_grid, size = 10000,prob = posterior, replace = TRUE)
        print(n)
        print(PI(samples,0.99)[2] - PI(samples,0.99)[1])
        
}

# 2200 samples

library(rethinking)
data(homeworkch3)

# total boys
sum(birth1) + sum(birth2)
# 3H1 # paramter which maximizes posterior probability
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1,1000)
likelihood <- dbinom(111,200,prob = p_grid)
posterior <- likelihood* prior
posterior <- posterior/sum(posterior)
p_grid[]
p_grid[which.max(posterior)]


# 3H2 HPDI 
samples <- sample(p_grid,size = 10000,replace = TRUE,prob = posterior)
HPDI(samples,prob = 0.50)
HPDI(samples,prob = 0.89)
HPDI(samples,prob = 0.97)


# 3H3  replication and posterior check
posterior_check <- rbinom(10000,200,prob = samples)
dens(posterior_check)

# it looks like the model fits the data well from this check, we've the peak of predicted distribution (~111) at the observed 
# # of boys in data

# 3H2 check the model agains the boys in first birth
posterior_check <- rbinom(10000,100,prob = samples)
dens(posterior_check)
mean(posterior_check)
sum(birth1)

# here the model doesn't seem to be doing too good, # total boys in birth 1 are 51, but our model estimate ~55

second.brth.post.girl <- birth2[birth1 == 0]

# 3H3 second birth where first birth were girls
sum(second.brth.post.girl)/length(second.brth.post.girl)

posterior_check <- rbinom(10000,length(second.brth.post.girl),prob = samples)
dens(posterior_check)
sum(second.brth.post.girl)  # 39

mean(posterior_check)
# model gives a peak at ~ 27, however the true # boys who are second birth is much higher - 39. Clearly the
# first and second birth are not independent. I wonder if the other way round holds true too.
# This could be happening because folks are giving preference to boys if their first born was a girl!


second.brth.post.boy <- birth2[birth1 == 1]

# 3H3 second birth where first birth were girls

posterior_check <- rbinom(10000,length(second.brth.post.boy),prob = samples)
dens(posterior_check)
sum(second.brth.post.boy)  # 21
length(second.brth.post.boy)

mean(posterior_check) # 51

# on this side the effect doesn't look that strong





