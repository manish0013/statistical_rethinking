# 6M1. Modify the DAG on page 190 to include the variable V, an unobserved cause of C and Y:
#   C ← V → Y. Reanalyze the DAG. How many paths connect X to Y? Which must be closed? Which
# variables should you condition on now?

# Total 5 paths, both the new paths added contain a collider so we don't need to close those. To condition on A,
# because conditioning on C will cause the closed paths to open since it's a collider and create a confound

library(dagitty)
dag <- dagitty("dag {
               U [unobserved]
               V [unobserved]
               X -> Y
               X <- U <- A -> C -> Y
               U -> B <- C
               C <- V -> Y
               }")

adjustmentSets(dag, exposure = "X", outcome = "Y")

# 
# 6H1. Use the Waffle House data, data(WaffleDivorce), to find the total causal influence of number of
# Waffle Houses on divorce rate. Justify your model or models with a causal graph.

# build a causal graph
dag <- dagitty("dag {
               A -> D
               A <- S -> W -> D
               S -> M -> D
               A -> M
               }")

adjustmentSets(dag, exposure = "W", outcome = "D")
impliedConditionalIndependencies(dag)

data("WaffleDivorce")
d<- WaffleDivorce
str(d)

str(d)

d$D <- scale(d$Divorce)
d$W <- scale(d$WaffleHouses)
d$S <- d$South + 1
d$M <- scale(d$Marriage)
d$A <- scale(d$MedianAgeMarriage)

h6.1 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a[S] + bW*W,
    a[S] ~dnorm(0,0.5),
    bW ~ dnorm(0,0.5),
    sigma ~ dexp(1)
  ), data = d
)


precis(h6.1, depth = 2)
# Waffle houses do not impact divorce rates, once we condition onf Souther States
# 
# 6H2. Build a series of models to test the implied conditional independencies of the causal graph
# you used in the previous problem. If any of the tests fail, how do you think the graph needs to be
# amended? Does the graph need more or fewer arrows? Feel free to nominate variables that aren’t in
# the data
impliedConditionalIndependencies(dag)

h6.2a <- quap(
  alist(
    A ~ dnorm(mu, sigma),
    mu <- a[S] + bW*W,
    a[S] ~dnorm(0,0.5),
    bW ~ dnorm(0,0.5),
    sigma ~ dexp(1)
  ), data = d
)

precis(h6.2a)
# Age is conditionally independent of Waffle houses given South States


h6.2b <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a[S] + bW*W +bM*M + bA*A,
    a[S] ~dnorm(0,0.5),
    bW ~ dnorm(0,0.5),
    bM ~ dnorm(0,0.5),
    bA ~ dnorm(0,0.5),
    sigma ~ dexp(1)
  ), data = d
)

precis(h6.2b, depth = 2)
# Divorce is conditionally independent of South giveb Age at Marriage, Waffle houses & Marriage rate


h6.2c <- quap(
  alist(
    M ~ dnorm(mu, sigma),
    mu <- a[S] + bW*W ,
    a[S] ~dnorm(0,0.5),
    bW ~ dnorm(0,0.5),
    sigma ~ dexp(1)
  ), data = d
)

precis(h6.2c, depth = 2)
# Marriage rate is conditionally independent of Wafflehouses given if a State is Southern

