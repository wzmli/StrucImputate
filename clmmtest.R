
library(ordinal)
library(lme4)

## Some data with a structural NA
## Religion not collected from country 3; we added a dummy level ("4")
## depends on functions in redundancy.R

set.seed(2830)
l <- 400

y <- sample(1:2, l, prob=1:2, replace=TRUE)
x <- rnorm(l)
religion <- sample(1:3, l, prob=1:3, replace=TRUE)
country <- sample(1:3, l, prob=1:3, replace=TRUE)
village <- sample(c("a","b","c","d"), l, prob=1:4, replace=TRUE)

religion[country==3] <- 4

dat <- data.frame(x=x, y=y
                  , religion=as.factor(religion)
                  , country=as.factor(country)
                  , village=village
)
## Naive lmer works, default behaviour is like our "base"
formula <- as.factor(y)~x+country+religion+(1|village)
summary(clmm(formula, data=dat))

summary(clmmFill(formula,data=dat,NArows = dat$country==3, fillvar="religion",Fillmethod="base"))

summary(clmmFill(formula,data=dat,NArows = dat$country==3, fillvar="religion",Fillmethod="mean"))

# debug(clmm)
# undebug(clmm)
# debug(clmmFill)
# undebug(clmmFill)
# 
# debug(ordinal:::getCtrlArgs)
# undebug(ordinal:::getCtrlArgs)
# 


summary(lme4:::glmer(formula,data=dat,family = binomial))
summary(glmerFill(formula, data=dat, NArows = dat$country==3, fillvar="religion", Fillmethod="base",check=FALSE,family=binomial))
summary(glmerFill(formula, data=dat, NArows = dat$country==3, fillvar="religion", Fillmethod="mean",check=FALSE,family=binomial))


# debug(glmerFill)
# undebug(glmerFill)