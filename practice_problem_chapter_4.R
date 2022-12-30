# 4E1. In the model definition below, which line is the likelihood?
#   yi ∼ Normal(µ, σ)
# µ ∼ Normal(0, 10)
# σ ∼ Exponential(1)

#  yi ∼ Normal(µ, σ) is likelihood

# 
# 4E2. In the model definition just above, how many parameters are in the posterior distribution?

# 2 , mu and sigma are the parameters in posterior distribution


# 4E3. Using the model definition above, write down the appropriate form of Bayes’ theorem that
# includes the proper likelihood and priors.

# Pr(mu,sigma|h) = Product(N(h_i|mu,sigma))*N(mu|0,10)*Exp(sigma|1)/
# Sum(Sum((Product(N(h_i|mu,sigma))*N(mu|0,10)*Exp(sigma|1)*dmu*dsigma)))

# 4E4. In the model definition below, which line is the linear model?
#   yi ∼ Normal(µ, σ)
# µi = α + βxi
# α ∼ Normal(0, 10)
# β ∼ Normal(0, 1)
# σ ∼ Exponential(2)
# µi = α + βxi is the linear model

# 4E5. In the model definition just above, how many parameters are in the posterior distribution?
# 3 - alpha, beta and sigma

# 4M1. For the model definition below, simulate observed y values from the prior (not the posterior).
# yi ∼ Normal(µ, σ)
# µ ∼ Normal(0, 10)
# σ ∼ Exponential(1)
N<- 1e4
mu <- rnorm(N,0,10)
sigma <- rexp(N,1)
y <- rnorm(N, mu, sigma)
dens(mu)
dens(sigma)
dens(y)

# 4M2. Translate the model just above into a quap formula.
alist(
    y~dnorm(mu, sigma),
    mu ~ dnorm(0,10),
    sigma ~ dexp(1))

# 4M3. Translate the quap model formula below into a mathematical model definition.
# flist <- alist(
#   y ~ dnorm( mu , sigma ),
#   mu <- a + b*x,
#   a ~ dnorm( 0 , 10 ),
#   b ~ dunif( 0 , 1 ),
#   sigma ~ dexp( 1 )
# )
# y ~ Normal (mu, sigma)
# mu = a + b*x
# a ~ Normal (0,10)
# b ~ Uniform(0,1)
# sigman ~ Exponential(1)


# 4M4. A sample of students is measured for height each year for 3 years. After the third year, you want
# to fit a linear regression predicting height using year as a predictor. Write down the mathematical
# model definition for this regression, using any variable names and priors you choose. Be prepared to
# defend your choice of priors.

# height ~ Normal (mu, sigma)
# mu <- a + b*(year - year_avg)
# a <- Normal(160, 20)
# b <- Normal(0,10)
# sigma <- Uniform(0,50)

# 4M5. Now suppose I remind you that every student got taller each year. Does this information lead
# you to change your choice of priors? How?
# have added lognormal priors for b, which implies that the prior for b is always positive.
# now from linear model, if the years increase and since the coefficient of change in year is +ve
# height of students will also increase
# height ~ Normal (mu, sigma)
# mu <- a + b*(year - year_avg)
# a <- Normal(160, 20)
# b <- LogNormal(0,1)
# sigma <- Uniform(0,50)


# 4M6. Now suppose I tell you that the variance among heights for students of the same age is never
# more than 64cm. How does this lead you to revise your priors?
# Have updated priors of standard deviation to uniform
# height ~ Normal (mu, sigma)
# mu <- a + b*(year - year_avg)
# a <- Normal(160, 20)
# b <- LogNormal(0,1)
# sigma <- Uniform(0,8)


# 4H1. The weights listed below were recorded in the !Kung census, but heights were not recorded for
# these individuals. Provide predicted heights and 89% intervals for each of these individuals. That is,
# fill in the table below, using model-based predictions.

data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ]
xbar <- mean(d2$weight)
m1 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu<- a + b*(weight - xbar),
    a <- dnorm(178,20),
    b<- dlnorm(0,1),
    sigma <- dunif(0,50)
  ),
  data = d2
)


precis(m1)
weight.seq <- c(46.95,43.72,64.78,32.59,54.63)
sim.height <- sim(m1, data = list(weight = weight.seq), 1e4)
sim.height.mean <- apply(sim.height,2, mean)
sim.height.PI <- apply(sim.height,2, PI)

sim.height.mean
sim.height.PI





# 4H2. Select out all the rows in the Howell1 data with ages below 18 years of age. If you do it right,
# you should end up with a new data frame with 192 rows in it.
data(Howell1)
d <- Howell1
d2 <- d[ d$age < 18 , ]
xbar <- mean(d2$weight)
# (a) Fit a linear regression to these data, using quap. Present and interpret the estimates. For
# every 10 units of increase in weight, how much taller does the model predict a child gets?

m2 <- quap(
  alist(
    height ~ dnorm(mu,sigma),
    mu <- a + b*(weight - xbar),
    a <- dnorm(140,30),
    b <- dlnorm(0,1),
    sigma <- dunif(0,50)
  ),
  data = d2
)

precis(m2)
#for every 10 unit increase in weight, height increases by 27.2 cm
#   (b) Plot the raw data, with height on the vertical axis and weight on the horizontal axis. Superimpose the MAP regression line and 89% interval for the mean. Also superimpose the 89% interval
# for predicted heights.

weight.seq<- seq(from = min(d2$weight),to = 50, by= 1)
plot(d2$height~d2$weight, col = col.alpha(rangi2,.5))
mu<- link(m2, data = data.frame(weight = weight.seq))
mu.mean <- apply(mu,2,mean)
mu.PI <- apply(mu,2,PI)

max(d2$weight)
lines(weight.seq,mu.mean)
shade(mu.PI,weight.seq)

sim.height <- sim(m2, data = list(weight = weight.seq))
str(sim.height)
height.PI <- apply(sim.height,2,PI)
shade(height.PI,weight.seq)

# (c) What aspects of the model fit concern you? Describe the kinds of assumptions you would
# change, if any, to improve the model. You don’t have to write any new code. Just explain what the
# model appears to be doing a bad job of, and what you hypothesize would be a better model.
# doesn't look like that the height is changing linearly with increase in weight, it increases in 
# rapidly with increase in weight and then starts to saturate. To improve the model fit, we can 
# use a ploynomial model which can better capture this relationshipt


# 4H3. Suppose a colleague of yours, who works on allometry, glances at the practice problems just
# above. Your colleague exclaims, “That’s silly. Everyone knows that it’s only the logarithm of body
# weight that scales with height!” Let’s take your colleague’s advice and see what happens.
# (a) Model the relationship between height (cm) and the natural logarithm of weight (log-kg). Use
# the entire Howell1 data frame, all 544 rows, adults and non-adults. Fit this model, using quadratic
# approximation:
#   hi ∼ Normal(µi
#               , σ)
# µi = α + β log(wi)
# α ∼ Normal(178, 20)
# β ∼ Log − Normal(0, 1)
# σ ∼ Uniform(0, 50)

m3 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*log(weight),
    a <- dnorm(178,20),
    b <- dlnorm(0,1),
    sigma <- dunif(0,50)
  ),
  data = d
)

precis(m3)

plot( height ~ weight , data=Howell1 ,
      col=col.alpha(rangi2,0.4) )

weight.seq<- seq(from = min(d2$weight),to = 80, by= 1)

mu<- link(m3, data = data.frame(weight = weight.seq))
mu.mean <- apply(mu,2,mean)
mu.PI <- apply(mu,2,PI, prob = .97)

lines(weight.seq,mu.mean)
shade(mu.PI,weight.seq)

sim.height <- sim(m3, data = list(weight = weight.seq))
sim.height.PI <- apply(sim.height,2,PI, prob = .97)
shade(sim.height.PI,weight.seq)


# 
# 4H4. Plot the prior predictive distribution for the polynomial regression model in the chapter. You
# can modify the code that plots the linear regression prior predictive distribution. Can you modify the
# prior distributions of α, β1, and β2 so that the prior predictions stay within the biologically reasonable
# outcome space? That is to say: Do not try to fit the data by hand. But do try to keep the curves
# consistent with what you know about height and weight, before seeing these exact data.

m4 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1* weight + b2 * weight^2,
    a <- dnorm(178,20),
    b1 <- dlnorm(0,1),
    b2 <- dnorm(0,1),
    sigma <- dunif(0,50)
  ),
  data = d
)

precis(m4)

N <- 1e4
a <- rnorm(N, 178,20)
b1 <- rlnorm(N, 0,1)
b2 <- -rlnorm(N,0.02,.05)
sigma <- runif(N,0,50)

plot(NULL, xlim = range(d$weight), ylim = c(-200,1200),
     xlab = "weight", ylab = "height")
abline(h = 0, lty = 2)
abline(h = 272,lty = 1, lwd = .5)
# mtext()

for (i in 1:100) curve(a[i] + b1[i]*x + b2[i]*x^2,
                       from = min(d$weight), to = max(d$weight), add = TRUE,
                                  col = col.alpha("black",0.2))



x = min(d$weight)
i <- 1
b2[1]
a[i] + b1[i]*x + b2[i]*x^2