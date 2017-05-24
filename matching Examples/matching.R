library("Matching")
data("lalonde")
attach(lalonde)

Y <- lalonde$re78
Tr <- lalonde$treat

glm1 <- glm(Tr ~ age + educ + black + hisp + married + nodegr +
              + re74 + re75, family = binomial, data = lalonde)

rr1 <- Match(Y = Y, Tr = Tr, X = glm1$fitted)
MatchBalance(Tr ~ age + I(age^2) + educ + I(educ^2) + black + hisp +
               + married + nodegr + re74 + I(re74^2) + re75 + I(re75^2) + u74 + u75 +
               + I(re74 * re75) + I(age * nodegr) + I(educ * re74) + I(educ * re75),
              match.out = rr1, nboots = 1000, data = lalonde)

qqplot(lalonde$re74[rr1$index.control], lalonde$re74[rr1$index.treated])
abline(coef = c(0, 1), col = 2)
