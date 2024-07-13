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


#plot priors

prior = extract.prior(m8.6)

for (s in -1) {
  idx <- which(d$shade_center == s)
  plot(d$water_center[idx], d$blooms_std[idx], xlim = c(-1,1),
       ylim = c(0,1), xlab = "water", ylab = "bloom", pch = 16, col = rangi2)
  mu <- link(m8.5 , data = data.frame(shade_center = s, water_center = -1:1))
  for (i in 1:20) lines(-1:1, mu[i,], col = col.alpha("black",0.3))
}



# Exercise

# 8E1. For each of the causal relationships below, name a hypothetical third variable that would lead
# to an interaction effect.
# (1) Bread dough rises because of yeast.  Room Temperature
# (2) Education leads to higher income.   Income of Parents
# (3) Gasoline makes a car go.            Weight of Car

# 8E2. Which of the following explanations invokes an interaction?
#   (1) Caramelizing onions requires cooking over low heat and making sure the onions do not
# dry out. Yes interaction between moisture and heat on caramelizing onion
# (2) A car will go faster when it has more cylinders or when it has a better fuel injector.
# yes interaction betwene # cylinders and fuel injector on caramelizing onion
# (3) Most people acquire their political beliefs from their parents, unless they get them instead
# from their friends.
# political beliefs of parents and friends
# (4) Intelligent animal species tend to be either highly social or have manipulative appendages
# (hands, tentacles, etc.).
# social behavior and appendages
# 

# 8E3. For each of the explanations in 8E2, write a linear model that expresses the stated relationship

# 1. CO ~ a + b*H + c*M + d*H*M
# 
# 2. S ~ a + b*C + c*F + d*C*F
# 
# 3. B ~ a + b*P + c*F + d*P*F
# 
# 4. I ~ a + b*S + c*A + d*S*A
# 
# 8M1. Recall the tulips example from the chapter. Suppose another set of treatments adjusted the
# temperature in the greenhouse over two levels: cold and hot. The data in the chapter were collected
# at the cold temperature. You find none of the plants grown under the hot temperature developed
# any blooms at all, regardless of the water and shade levels. Can you explain this result in terms of
# interactions between water, shade, and temperature?


# In terms of interaction between water and shade and temperature, since there was no
# bloom when the temperature was high, one plausible explaination could be 
# that Temperature has interaction with water and shade and when 
# temeprature is cold there it's impact on bloom is zero but as soon as the
# temperature becomes hot it has a negative impact on bloom while
# the interaction terms kick in to drive impact of water and shade insingificant 
# on bloom

# B ~ a + b*W + c*S + d*T + e*W*S + f*S*T + g*W*T + h*W*S*T

# 8M3. In parts of North America, ravens depend upon wolves for their food. This is because ravens
# are carnivorous but cannot usually kill or open carcasses of prey. Wolves however can and do kill
# and tear open animals, and they tolerate ravens co-feeding at their kills. This species relationship
# is generally described as a “species interaction.” Can you invent a hypothetical set of data on raven
# population size in which this relationship would manifest as a statistical interaction? Do you think
# the biological interaction could be linear? Why or why not?
#   
water <- rnorm(1000, 1, 0.2)
wolves <- rnorm(1000, 50, 10) + 50*water
raven <- rnorm(1000, 400,50) + wolves*5 + 10*water

d<- data.frame(wolves, raven, water)


m8.7 <- quap(alist(
  raven ~ dnorm(mu, sigma),
  mu <- a + bw* wolves + br*water + bwr*wolves*water,
  a ~ dnorm(300, 30),
  bw ~ dnorm(0, 10),
  br ~ dnorm(0, 10),
  bwr ~ dnorm(0, 10),  
  sigma ~ dexp(100)
), data = d)

precis(m8.7)

# No the biological interaction probably won't be linear

# 
# H1. Return to the data(tulips) example in the chapter. Now include the bed variable as a predictor in the interaction model. Don’t interact bed with the other predictors; just include it as a main
# effect. Note that bed is categorical. So to use it properly, you will need to either construct dummy
# variables or rather an index variable, as explained in Chapter 6.

data(tulips)
d <- tulips
str(d)


# model impact of Water, Shade and Bed on Bloom
library (dagitty)
dag_8.3<- dagitty("dag{
                  W -> B
                  S -> B
                  D -> B
                  }")
drawdag(dag_8.3)

# re-scale the data
d$blooms <- d$blooms/max(d$blooms)
d$water_center <- d$water- mean(d$water)
d$shade_center <- d$shade- mean(d$shade)
d$bid <- ifelse(d$bed == "a", 1, ifelse(d$bed == "b", 2, 3))

str(d)

m8.8 <- quap(alist(
  blooms ~ dnorm(mu, sigma),
  mu <- a[bed] + bw*water_center + bs*shade_center + bws*water_center*shade_center,
  a[bed] ~ dnorm(.5, .25),
  bw ~ dnorm(0, 0.25),
  bs ~ dnorm(0,0.25),
  bws ~ dnorm(0,0.25),
  sigma ~ dexp(1)
), data = d)


precis(m8.8, depth = 2)

m8.9 <- quap(alist(
  blooms ~ dnorm(mu, sigma),
  mu <- a + bw*water_center + bs*shade_center + bws*water_center*shade_center,
  a ~ dnorm(.5, .25),
  bw ~ dnorm(0, 0.25),
  bs ~ dnorm(0,0.25),
  bws ~ dnorm(0,0.25),
  sigma ~ dexp(1)
), data = d)

precis(m8.9)
compare(m8.8, m8.9)
 # the model with bed is slightly better than the one without bed, 
# from coefficients bed > a seems to impact bloom differently (less) than 
# the other two beds but based on WAIC, the difference is within 1 SE so not
# really that different
# 
# 8H3. Consider again the data(rugged) data on economic development and terrain ruggedness,
# examined in this chapter. One of the African countries in that example, Seychelles, is far outside
# the cloud of other nations, being a rare country with both relatively high GDP and high ruggedness.
# Seychelles is also unusual, in that it is a group of islands far from the coast of mainland Africa, and
# its main economic activity is tourism.
# (a) Focus on model m8.5 from the chapter. Use WAIC pointwise penalties and PSIS Pareto k
# values to measure relative influence of each country. By these criteria, is Seychelles influencing the
# results? Are there other nations that are relatively influential? If so, can you explain why?
#   (b) Now use robust regression, as described in the previous chapter. Modify


# load data
data("rugged")
d <- rugged

d$log_gdp<- log(d$rgdppc_2000)
dd <- d[complete.cases(d$rgdppc_2000),]

dd$log_gdp_std <- dd$log_gdp/mean(dd$log_gdp)
dd$rugged<- dd$rugged/max(dd$rugged)


dd$cid <- ifelse(dd$cont_africa ==1 , 1,2)

m8.3 <- quap(
  alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a[cid] + b[cid]*(rugged - 0.215),
    a[cid] ~ dnorm(1,0.1),
    b[cid] ~ dnorm(0,0.3),
    sigma ~ dexp(1)
  ), data =dd
)

precis(m8.3, depth = 2)

PSIS_m8.3 <- PSIS(m8.3, pointwise = TRUE)
WAIC_m8.3 <- WAIC(m8.3, pointwise = TRUE)

plot(PSIS_m8.3$penalty, WAIC_m8.3$penalty, xlab = "PSIS Pareto k",
     ylab = "WAIC penalty", col = rangi2, lwd = 2)

summary(PSIS_m8.3$penalty)

dd[WAIC_m8.3$penalty>0.2,]

# Influential countries - Switzerland, Guinea, Lesotho, Seychelles, Tajikistan
# Switzerland is rugged but high gdp because of tourism, Seychelles is in Africa
# it has high ruggedness


m8.4 <- quap(
  alist(
    log_gdp_std ~ dstudent(2,mu, sigma),
    mu <- a[cid] + b[cid]*(rugged - 0.215),
    a[cid] ~ dnorm(1,0.1),
    b[cid] ~ dnorm(0,0.3),
    sigma ~ dexp(1)
  ), data =dd
)

PSIS_m8.4 <- PSIS(m8.4, pointwise = TRUE)
WAIC_m8.4 <- WAIC(m8.4, pointwise = TRUE)

plot(PSIS_m8.4$penalty, WAIC_m8.4$penalty, xlab = "PSIS Pareto k",
     ylab = "WAIC penalty", col = rangi2, lwd = 2)
summary(PSIS_m8.4$penalty)

# the model doesn't seem influenced by outliers anymore
compare(m8.3, m8.4)


data("nettle")
d <- nettle
str(d)
d$lang.per.cap <- d$num.lang/d$k.pop
d$log.lang.per.cap <- log(d$lang.per.cap)
d$area_std <- d$area/max(d$area)
d$mean.growing.season_std <- d$mean.growing.season/max(d$mean.growing.season)
d$sd.growing.season_std <- d$sd.growing.season/max(d$sd.growing.season)

str(d)
summary(d$lang.per.cap)
summary(d$log.lang.per.cap)
summary(d$area_std)

plot(d$log.lang.per.cap, d$area_std)
plot(d$log.lang.per.cap, d$mean.growing.season_std)

# 
# (a) Evaluate the hypothesis that language diversity, as measured by log(lang.per.cap), is positively associated with the average length of the growing season, mean.growing.season. Consider
# log(area) in your regression(s) as a covariate (not an interaction). Interpret your results.

summary(d$mean.growing.season_std)
summary(d$sd.growing.season)
m8.10 <- quap(alist(
  log.lang.per.cap ~ dnorm(mu, sigma),
  mu <- a + bm*mean.growing.season_std + ba*area_std,
  a ~ dnorm(-5, 2.5),
  bm ~ dnorm(-5, 2.5),
  ba ~ dnorm(-5, 2.5),
  sigma ~ dexp(1)
), data = d)

precis(m8.10)

#language diversity is positively associated with length of growing season

m8.11 <- quap(alist(
  log.lang.per.cap ~ dnorm(mu, sigma),
  mu <- a + bs*sd.growing.season_std + ba*area_std,
  a ~ dnorm(-5, 2.5),
  bs ~ dnorm(-5, 2.5),
  ba ~ dnorm(-5, 2.5),
  sigma ~ dexp(1)
), data = d)

precis(m8.11)

# langauge diversity is negatively associated with variation in length of growing 
# season
# 
# (c) Finally, evaluate the hypothesis that mean.growing.season and sd.growing.season interact to synergistically reduce language diversity. The idea is that, in nations with longer average
# # growing seasons, high variance makes storage and redistribution even more important than it would
# be otherwise. That way, people can cooperate to preserve and protect windfalls to be used during the
# droughts. These forces in turn may lead to greater social integration and fewer languages.




m8.12 <- quap(alist(
  log.lang.per.cap ~ dnorm(mu, sigma),
  mu <- a + bm*mean.growing.season_std + bs*sd.growing.season_std + ba*area_std
  + bi*mean.growing.season_std*sd.growing.season_std,
  a ~ dnorm(-5, 2.5),
  bm ~ dnorm(-5, 2.5),  
  bs ~ dnorm(-5, 2.5),
  ba ~ dnorm(-5, 2.5),
  bi ~ dnorm(-5,2.5),
  sigma ~ dexp(1)
), data = d)

precis(m8.12)

# Hypothesis is true
