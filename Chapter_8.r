# Worked Out Examples

# we prefer interaction terms instead of splitting data because - 
# 1. some parameters (like sigma etc.) do not vary by continent (or any other data splitter)
# therefore we're hurting the accuracy of the estimates of these parameters

# 2. To acquire probability statement about the variable you're using to split the data
# you need to include it in the model for e.g., continent and it's impact on GDP

# 3. We may want to use information criteria or som other measure to compare models
# this comparison is only possible if these models are using the same underlying data

# 4. There are advantages in borrowing information across categories (like continent)
# learning about relationships in one continent may have effect on learning about
# the same relationship in another continent (specially helpful when sample sizes
# vary across categories and overfitting riks in higher in some categories)

# model impact of ruggedness on GDP
library (dagitty)
dag_8.1<- dagitty("dag{
                  C -> G
                  R -> G
                  }")
drawdag(dag_8.1)


# load data
data("rugged")
d <- rugged

d$log_gdp<- log(d$rgdppc_2000)
dd <- d[complete.cases(d$rgdppc_2000),]

dd$log_gdp_std <- dd$log_gdp/mean(dd$log_gdp)
dd$rugged<- dd$rugged/max(dd$rugged)


# relationship
# log(y) ~ Normal(mu, sigma)
# mu <- a + b* (r - r_bar)
# assigning priors
# adding r_bar help assign priors to alpha, alpha now becomes the value 
# when ruggedness is average ruggedness and should be same as avg gdp = 1 because
# we scaled gdp by mean i.e., alpha ~ Normal (1,1)
# for b, let's say both positive and negative relationships are equally likely
# b ~ Normal (0,1) std we chose 1 (guess)

# fit the model
m8.1 <- quap(
            alist(
              log_gdp_std ~ dnorm(mu, sigma),
              mu <- a + b*(rugged - 0.215),
              a ~ dnorm(1,1),
              b ~ dnorm(0,1),
              sigma ~ dexp(1)
            ), data =dd
)

# analyze prior predictions (plot the priors)
prior = extract.prior(m8.1)
max(dd$log_gdp_std)
plot(NULL, xlim = c(0,1), ylim = c(0.5, 1.5), xlab = "ruggedness",
     ylab = "log_gdp")
abline(h = min(dd$log_gdp_std), lty = 2)
abline(h = max(dd$log_gdp_std), lty = 2)

rugged_seq <- seq(from = -0.1, to= 1.1, length.out = 30)
mu <- link(m8.1, post = prior, data = data.frame(rugged = rugged_seq))
for (i in 1:50) {
  lines(rugged_seq, mu[i,], col = col.alpha("black",0.3))
}

# these priors are very vague let's make them a bit informative
# prior for alpha, the std currently is set to 1 which means that
# within 95% CI mean GDP can range from  1 - 2*1  to 1 + 2*1 which is very wide
# the GDP varies between 0.7 to 1.6 so we can choose a more restrictive std here
# a ~ Normal(1, 0.1) means 95% of the gdp values will vary between 0.8 to 1.2

# similarly the priors for b have extremely high positive or negative slopes these
# relationships are not feasible, even the strongest relationship between ruggedness
# and gdp wouldn't vary more than (max(gdp) - min(gdp)) > 1.3 - 0.7 = 0.6
# therefore for b we'll set a prior as b ~ Normal(0, 0.3)

m8.2 <- quap(
  alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a + b*(rugged - 0.215),
    a ~ dnorm(1,0.1),
    b ~ dnorm(0,0.3),
    sigma ~ dexp(1)
  ), data =dd
)
# analyze prior predictions (plot the priors)
prior = extract.prior(m8.2)
max(dd$log_gdp_std)
plot(NULL, xlim = c(0,1), ylim = c(0.5, 1.5), xlab = "ruggedness",
     ylab = "log_gdp")
abline(h = min(dd$log_gdp_std), lty = 2)
abline(h = max(dd$log_gdp_std), lty = 2)

rugged_seq <- seq(from = -0.1, to= 1.1, length.out = 30)
mu <- link(m8.1, post = prior, data = data.frame(rugged = rugged_seq))
for (i in 1:50) {
  lines(rugged_seq, mu[i,], col = col.alpha("black",0.3))
}

precis(m8.1)
precis(m8.2)



# let's try and see if adding an intercept for continent improves the relationship
# between ruggedness and gpd
# add an index variable for continent
dd$cid <- ifelse(dd$cont_africa ==1 , 1,2)

m8.3 <- quap(
  alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a[cid] + b*(rugged - 0.215),
    a[cid] ~ dnorm(1,0.1),
    b ~ dnorm(0,0.3),
    sigma ~ dexp(1)
  ), data =dd
)

precis(m8.3, depth = 2)

compare(m8.2, m8.3)

post <- extract.samples(m8.3)

diff <- post$a[,1] - post$a[,2]
PI(diff)

# let's plot posterior predictions
rugged_seq <- seq(from = -0.1, to = 1, length.out = 30)
mu.notAfrica <- link(m8.3, data = data.frame(cid = 2, rugged = rugged_seq))
mu.Africa <- link(m8.3, data = data.frame(cid = 1, rugged = rugged_seq))

mu.notAfrica_mu <- apply(mu.notAfrica,2,mean)
mu.Africa_mu <- apply(mu.Africa,2,mean)

mu.notAfrica_ci <- apply(mu.notAfrica,2,PI, prob = 0.97)
mu.Africa_ci <- apply(mu.Africa,2,PI,  prob = 0.97)


# add an interaction term for slope
m8.4 <- quap(
  alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a[cid] + b[cid]*(rugged - 0.215),
    a[cid] ~ dnorm(1,0.1),
    b[cid] ~ dnorm(0,0.3),
    sigma ~ dexp(1)
  ), data =dd
)

compare(m8.3, m8.4)
precis(m8.4, depth = 2)
compare(m8.2,m8.3, m8.4, func = PSIS)
# 0.215
print(mean(dd$rugged))
plot(PSIS(m8.3, pointwise = TRUE)$k)


# let's plot the data and model

d.A1 <- dd[dd$cid == 1, ]

plot(d.A1$rugged, d.A1$log_gdp_std, pch = 16, col = rangi2, xlab = "ruggedness(std)",
     ylab = "log GPD ", xlim = c(0, 1))

mu <- link(m8.4, data = data.frame(cid = 1, rugged = rugged_seq))
mu_mean <- apply(mu,2,mean)
mu_ci <- apply(mu,2,PI,  prob = 0.97)
lines(rugged_seq, mu_mean, lwd = 2)
shade(mu_ci, rugged_seq)
mtext("Non African nations")


d.A2 <- dd[dd$cid == 2, ]

plot(d.A2$rugged, d.A2$log_gdp_std, pch = 16, col = rangi2, xlab = "ruggedness(std)",
     ylab = "log GPD ", xlim = c(0, 1))

mu <- link(m8.4, data = data.frame(cid = 2, rugged = rugged_seq))
mu_mean <- apply(mu,2,mean)
mu_ci <- apply(mu,2,PI,  prob = 0.97)
lines(rugged_seq, mu_mean, lwd = 2)
shade(mu_ci, rugged_seq)
mtext("African nations")


# there are two types of interpretation to interaction models both are mathematically
# valid but may not seem intuitive, for e.g. the above model formulation
# log_gdp_std = a[cid]+ b[cid] * (r - r_bar) can be written as 
# log_gdp_std = (2 - a[cid]) * ( a1 + b1 * (r-r_bar)) + (a[cid] - 1) * ( a2 + b2 * (r-r_bar))

rugged_seq <- seq(from = -0.1, to = 1.1, length = 30)
mu.A <- link(m8.4, data = data.frame(cid = 1, rugged = rugged_seq))
mu.N <- link(m8.4, data = data.frame(cid = 2, rugged = rugged_seq))
delta <- mu.A - mu.N
delta_mean <- apply(delta, 2, mean)
delta_ci <- apply(delta, 2, PI)

length(delta_mean)
plot( rugged_seq,delta_mean)
shade(delta_ci,rugged_seq)



## another example having interaction between continuous variables, interactions
## are difficult to predict

data(tulips)
d <- tulips
str(d)


# model impact of ruggedness on GDP
library (dagitty)
dag_8.2<- dagitty("dag{
                  W -> B
                  S -> B
                  }")
drawdag(dag_8.2)


# first model without interactions

# data transformations
d$blooms_std = d$blooms/max(d$blooms)
d$water_center = (d$water - mean(d$water))
d$shade_center = (d$shade - mean(d$shade))

# B ~ Normal(mu, sigma)
# mu = a + bw*(W-W_bar) + bs* (S-S_bar) here W and S are water and shade respectively
# we have scalded bloom by max bloom and not converted to z score to preserve 0
# as a meaningful boundary and 1 as the max. Water and Shade are centered at 0 
# and will vary between -1 and 1
# we are substracting by W_bar and S_bar because it will make the interpreation of
# a easy and therefore relatively easy to set priors

# let's guess
# a ~ Normal(0.5,1)
# bw ~ Normal(0,1)
# bs ~ Normal(0,1)

mean(d$blooms_std)

a<- rnorm(1e4, 0.5, 1)
sum( a< 0 | a > 1)/length(a)
# that's 61% of data lying outside of possible bounds, priors should be more
# restrictinve
a<- rnorm(1e4, 0.5, 0.25)
sum( a< 0 | a > 1)/length(a)
# reasonable

# assigning slope, even if we assume that one variable alone could cause all the variation
## then see - bloom at most can vary from 0 to 1 i.e., 1 unit change, and corresponding variable can change
# from -1 to +1 in either side i.e., 2 unit change so max slope should be ~0.5

b<- rnorm(1e4, 0, 0.25)
sum(b < -0.5 | b> 0.5)/length(b)

# let's fit the model

m8.5 <- quap(
            alist(
              blooms_std ~ dnorm(mu, sigma),
              mu <- a + bw*(water_center) + bs*(shade_center),
              a ~ dnorm(0.5, 0.25),
              bw ~ dnorm(0, 0.25),
              bs ~ dnorm(0,0.25),
              sigma ~ dexp(1)
            ), data = d
)


# let's add interaction term
# we can define interaction term as
# yw,s = bw + bws*S and whole expression becomes
# mu = a + yws*W + bs*S, adding an interaction term for water
# is same as adding an interaction term for shade (they are symmetric)
# the whole expression if we simplify is
# mu = a + bw*W + bs*S + bws*WS 


m8.6 <- quap(
  alist(
    blooms_std ~ dnorm(mu, sigma),
    mu <- a + bw*(water_center) + bs*(shade_center) + bws*water_center*shade_center,
    a ~ dnorm(0.5, 0.25),
    bw ~ dnorm(0, 0.25),
    bs ~ dnorm(0,0.25),
    bws ~ dnorm(0,0.25),
    sigma ~ dexp(1)
  ), data = d
)


par(mfrow = c(1,3))

for (s in -1:1) {
  idx <- which(d$shade_center == s)
  plot(d$water_center[idx], d$blooms_std[idx], xlim = c(-1,1),
       ylim = c(0,1), xlab = "water", ylab = "bloom", pch = 16, col = rangi2)
  mu <- link(m8.5 , data = data.frame(shade_center = s, water_center = -1:1))
  for (i in 1:20) lines(-1:1, mu[i,], col = col.alpha("black",0.3))
}











