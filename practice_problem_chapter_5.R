
lsd_dat <- data.frame(Location = c("Utah",
                                   "Idaho",
                                   "Wyoming",
                                   "Nevada",
                                   "Arizona",
                                   "Hawaii",
                                   "Montana",
                                   "Alaska",
                                   "Washington",
                                   "Oregon",
                                   "New Mexico",
                                   "Colorado",
                                   "California",
                                   "North Dakota",
                                   "Nebraska",
                                   "Kansas",
                                   "Texas",
                                   "Oklahoma",
                                   "South Dakota",
                                   "Missouri",
                                   "Virginia",
                                   "Arkansas",
                                   "West Virginia",
                                   "Iowa",
                                   "North Carolina",
                                   "Georgia",
                                   "Maine",
                                   "South Carolina",
                                   "Kentucky",
                                   "Alabama",
                                   "Florida",
                                   "Tennessee",
                                   "Mississippi",
                                   "Vermont",
                                   "Maryland",
                                   "Indiana",
                                   "New Hampshire",
                                   "Louisiana",
                                   "Minnesota",
                                   "Delaware",
                                   "Ohio",
                                   "Wisconsin",
                                   "Illinois",
                                   "Michigan",
                                   "Connecticut",
                                   "New York",
                                   "District of Columbia",
                                   "Massachusetts",
                                   "Pennsylvania",
                                   "Rhode Island",
                                   "New Jersey"),
                      pop_perc = c(0.677,
                                   0.2642,
                                   0.1153,
                                   0.0621,
                                   0.061,
                                   0.0517,
                                   0.0481,
                                   0.0456,
                                   0.0394,
                                   0.0376,
                                   0.0335,
                                   0.0274,
                                   0.0197,
                                   0.0149,
                                   0.013,
                                   0.0129,
                                   0.0125,
                                   0.0121,
                                   0.0121,
                                   0.0116,
                                   0.0113,
                                   0.0103,
                                   0.0093,
                                   0.009,
                                   0.0084,
                                   0.0082,
                                   0.0082,
                                   0.0081,
                                   0.0079,
                                   0.0077,
                                   0.0075,
                                   0.0075,
                                   0.0073,
                                   0.0073,
                                   0.0072,
                                   0.0067,
                                   0.0065,
                                   0.0064,
                                   0.0059,
                                   0.0057,
                                   0.0053,
                                   0.0046,
                                   0.0045,
                                   0.0045,
                                   0.0044,
                                   0.0041,
                                   0.004,
                                   0.004,
                                   0.004,
                                   0.0039,
                                   0.0037))
library(rethinking)
# 5E1. Which of the linear models below are multiple linear regressions?
#   (1) µi = α + βxi
#   (2) µi = βxxi + βzzi
#   (3) µi = α + β(xi − zi)
#   (4) µi = α + βxxi + βzzi

# 1 & 3 are multiple linear regressions, 2 & 4 do not have linear coefficients

# 
# 5E2. Write down a multiple regression to evaluate the claim: Animal diversity is linearly related to
# latitude, but only after controlling for plant diversity. You just need to write down the model definition.

# A ~ N(mu, sigma)
# mu <- a + bD * D + bL*L
# bD ~ N(0,0.5)
# bL ~ N(0,0.5) # hypothetical priors
# sigma ~ Exp(1)

# 5E3. Write down a multiple regression to evaluate the claim: Neither amount of funding nor size
# of laboratory is by itself a good predictor of time to PhD degree; but together these variables are both
# positively associated with time to degree. Write down the model definition and indicate which side of
# zero each slope parameter should be on.

# T ~ N(mu, sigma)
# mu <- a + bS * S + bF*F
# a ~ N(0,0.2)
# bS ~ N(0,0.5)
# bF ~ N(0,0.5)
# sigma ~ Exp(1)


# 
# 5E4. Suppose you have a single categorical predictor with 4 levels (unique values), labeled A, B, C
# and D. Let Ai be an indicator variable that is 1 where case i is in category A. Also suppose Bi
# , Ci
# ,
# and Di
# for the other categories. Now which of the following linear models are inferentially equivalent
# ways to include the categorical variable in a regression? Models are inferentially equivalent when it’s
# possible to compute one posterior distribution from the posterior distribution of another model.
# (1) µi = α + βAAi + βBBi + βDDi
# (2) µi = α + βAAi + βBBi + βCCi + βDDi
# (3) µi = α + βBBi + βCCi + βDDi
# (4) µi = αAAi + αBBi + αCCi + αDDi
# (5) µi = αA(1 − Bi − Ci − Di) + αBBi + αCCi + αDDi

# 1 2 3 4 & 5 all are inferentially equivalent


# 
# 5M1. Invent your own example of a spurious correlation. An outcome variable should be correlated
# with both predictor variables. But when both predictors are entered in the same model, the correlation
# between the outcome and one of the predictors should mostly vanish (or at least be greatly reduced).


A <- rnorm(100, 10, 5)
B <- rnorm(100, A)
D <- rnorm(100, A)

pairs(data.frame(A = A, B= B, D = D))
dat<- data.frame(A = A, B= B, D = D)

m5.4 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + ba*A +bb*B,
    a ~ dnorm(0,0.2),
    ba ~ dnorm(0,0.5),
    bb ~ dnorm(0,0.5),
    sigma ~ dexp(1)
  ),
  data = dat
)

precis(m5.4)
cor(B,D)

# 5M2. Invent your own example of a masked relationship. An outcome variable should be correlated
# with both predictor variables, but in opposite directions. And the two predictor variables should be
# correlated with one another.

A <- rnorm(100, 10, 5)
B <- rnorm(100, A)
D <- rnorm(100, A-B)
dat<- data.frame(A = A, B= B, D = D)
m5.2 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + ba*A +bb*B,
    a ~ dnorm(0,0.2),
    ba ~ dnorm(0,0.5),
    bb ~ dnorm(0,0.5),
    sigma ~ dexp(1)
  ),
  data = dat
)

precis(m5.2)
cor(B,A)



# 5M3. It is sometimes observed that the best predictor of fire risk is the presence of firefighters—
# States and localities with many firefighters also have more fires. Presumably firefighters do not cause
# fires. Nevertheless, this is not a spurious correlation. Instead fires cause firefighters. Consider the
# same reversal of causal inference in the context of the divorce and marriage data. How might a high
# divorce rate cause a higher marriage rate? Can you think of a way to evaluate this relationship, using
# multiple regression?

# High divorce rates can cause high marriage rates, the divorced individuals will re-marry and this will
# lade to increase in marriage rates. This relationship can be measured by introducing the 
#re-marriage rate as another variable in above relationship between divorce rates and high 
# marriage rates


# 5M4. In the divorce data, States with high numbers of Mormons (members of The Church of Jesus
#                                                                Christ of Latter-day Saints, LDS) have much lower divorce rates than the regression models expected.
# Find a list of LDS population by State and use those numbers as a predictor variable, predicting divorce rate using marriage rate, median age at marriage, and percent LDS population (possibly standardized). You may want to consider transformations of the raw percent LDS variable.
data("WaffleDivorce")
d <- WaffleDivorce
str(d)
str(lsd_dat)
d<- merge(d,lsd_dat,by = "Location")

d$M <- standardize(d$Marriage)
d$A <- standardize(d$MedianAgeMarriage)
d$P <- standardize(d$pop_perc)
d$D <- standardize(d$Divorce)

m5.4 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bM * M + bA * A + bP * P,
    a ~ dnorm(0,0.2),
    bM ~ dnorm(0,0.5),
    bA ~ dnorm(0,0.5),
    bP ~ dnorm(0,0.5),
    sigma ~ dexp(1)
  ),
  data = d
)


precis(m5.4)

# 5M5. One way to reason through multiple causation hypotheses is to imagine detailed mechanisms
# through which predictor variables may influence outcomes. For example, it is sometimes argued that
# the price of gasoline (predictor variable) is positively associated with lower obesity rates (outcome
#                                                                                               variable). However, there are at least two important mechanisms by which the price of gas could
# reduce obesity. First, it could lead to less driving and therefore more exercise. Second, it could lead to
# less driving, which leads to less eating out, which leads to less consumption of huge restaurant meals.
# Can you outline one or more multiple regressions that address these two mechanisms? Assume you
# can have any predictor data you need.
# Obesity ~ a + b1 * Price + b2 * Excercise + b3 * Outside Meal

# Hard. All three exercises below use the same data, data(foxes) (part of rethinking).84 The urban
# fox (Vulpes vulpes) is a successful exploiter of human habitat. Since urban foxes move in packs and
# defend territories, data on habitat quality and population density is also included. The data frame has
# five columns:
#   (1) group: Number of the social group the individual fox belongs to
# (2) avgfood: The average amount of food available in the territory
# (3) groupsize: The number of foxes in the social group
# (4) area: Size of the territory
# (5) weight: Body weight of the individual fox


data("foxes")
d <- foxes
str(d)
sum(is.na(d))

# 5H1. Fit two bivariate Gaussian regressions, using quap: (1) body weight as a linear function of
# territory size (area), and (2) body weight as a linear function of groupsize. Plot the results of these
# regressions, displaying the MAP regression line and the 95% interval of the mean. Is either variable
# important for predicting fox body weight?
#   

d$W <- standardize(d$weight)
d$A <- standardize(d$area)
d$G <- standardize(d$groupsize)
m5.1 <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a + bA*A,
    a ~ dnorm(0,0.2),
    bA ~ dnorm(0,0.5),
    sigma ~ dexp(1)
  ),
  data = d
)

precis(m5.1)

A_seq <- seq(from = -3, to= 3.2, length.out = 30)
mu <- link(m5.1, data = list(A = A_seq))
mu.mean <- apply(mu,2, mean)
mu.PI <- apply(mu,2,PI)

plot(W~A,data = d, col = rangi2)
lines(A_seq, mu.mean,lwd = 2)
shade(mu.PI, A_seq)

m5.1 <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a + bG*G,
    a ~ dnorm(0,0.2),
    bG ~ dnorm(0,0.5),
    sigma ~ dexp(1)
  ),
  data = d
)

precis(m5.1)

G_seq <- seq(from = -3, to= 3.2, length.out = 30)
mu <- link(m5.1, data = list(G = G_seq))
mu.mean <- apply(mu,2, mean)
mu.PI <- apply(mu,2,PI)

plot(W~G,data = d, col = rangi2)
lines(G_seq, mu.mean,lwd = 2)
shade(mu.PI, G_seq)

# 
# 5H2. Now fit a multiple linear regression with weight as the outcome and both area and groupsize
# as predictor variables. Plot the predictions of the model for each predictor, holding the other predictor
# constant at its mean. What does this model say about the importance of each variable? Why do you
# get different results than you got in the exercise just above?

m5.2 <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a + bA*A + bG*G,
    a ~ dnorm(0,0.2),
    bA ~ dnorm(0,0.5),
    bG ~ dnorm(0,0.5),
    sigma ~ dexp(1)
  ),
  data = d
)

precis(m5.2)

#Keeping Group size constant 
xseq <- seq( from=min(d$A)-0.15 , to=max(d$A)+0.15 , length.out=30 )
mu <- link( m5.2 , data=data.frame( A=xseq , G=0 ) )
mu_mean <- apply(mu,2,mean)
mu_PI <- apply(mu,2,PI)
plot( NULL , xlim=range(d$A) , ylim=range(d$W) )
lines( xseq , mu_mean , lwd=2 )
shade( mu_PI , xseq )

# Keeping Area constant
xseq <- seq( from=min(d$G)-0.15 , to=max(d$G)+0.15 , length.out=30 )
mu <- link( m5.2 , data=data.frame( G=xseq , A=0 ) )
mu_mean <- apply(mu,2,mean)
mu_PI <- apply(mu,2,PI)
plot( NULL , xlim=range(d$G) , ylim=range(d$W) )
lines( xseq , mu_mean , lwd=2 )
shade( mu_PI , xseq )


# Both the variables are predicotr of Fox weight, +ve relationship with Group Size and
# -ve relationship with Territory Area

#check correlation between group size and territory area
cor(d$G, d$A)

# there is a +ve correlation between the two predictor variables - group size and territory aread, 
# unless we include both the predictors in multiple regression, their individual relationship with
# weight isn't as clear. Including both predictor together helps model make inference for e.g., by controlling for
# group size feature and looking at impact of terriotry area on fox weight 

# 5H3. Finally, consider the avgfood variable. Fit two more multiple regressions: (1) body weight
# as an additive function of avgfood and groupsize, and (2) body weight as an additive function of
# all three variables, avgfood and groupsize and area. Compare the results of these models to the
# previous models you’ve fit, in the first two exercises. (a) Is avgfood or area a better predictor of body
# weight? If you had to choose one or the other to include in a model, which would it be? Support your
# assessment with any tables or plots you choose. (b) When both avgfood or area are in the same
# model, their effects are reduced (closer to zero) and their standard errors are larger than when they
# are included in separate models. Can you explain this result?

data("foxes")
d <- foxes
str(d)

d$W <- standardize(d$weight)
d$A <- standardize(d$area)
d$G <- standardize(d$groupsize)
d$F <- standardize(d$avgfood)

m5.3 <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a + bG*G + bF*F,
    a ~ dnorm(0,0.2),
    bG ~ dnorm(0,0.5),
    bF ~ dnorm(0,0.5),    
    sigma ~ dexp(1)
  ),
  data = d
)

precis(m5.3)


m5.3a <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a + bG*G + bF*F + bA*A,
    a ~ dnorm(0,0.2),
    bG ~ dnorm(0,0.5),
    bF ~ dnorm(0,0.5),
    bA ~ dnorm(0,0.5),
    sigma ~ dexp(1)
  ),
  data = d
)

precis(m5.3a)

#there is high +ve correlation between area and avg food
cor(d$A, d$F)

# area seems to be a slightly better predictor fo weight, as it has a smaller sd when included in the model
# along with group size compared to when food was included along with group size
# additionally, when both predictors are included area has a slightly tighter effect & +ve