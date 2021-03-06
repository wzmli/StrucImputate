library(lme4)

## Some data with a structural NA
## Religion not collected from country 3; we added a dummy level ("4")
## depends on functions in redundancy.R

set.seed(1971)
l <- 40

y <- rnorm(l)
x <- rnorm(l)
religion <- sample(1:3, l, prob=1:3, replace=TRUE)
country <- sample(1:3, l, prob=1:3, replace=TRUE)
education <- sample(1:4, l, prob=1:4, replace=TRUE)
village <- sample(1:2, l, prob=1:2, replace=TRUE)

religion[country==3] <- 4

dat <- data.frame(x=x, y=y
                  , religion=as.factor(religion)
                  , country=as.factor(country)
                  , village=as.factor(village)
)

## Naive lmer works, default behaviour is like our "base"
formula <- y~x+country+religion+(1|village)
summary(lmer(formula, data=dat))

## Set NAs to base level; this matches the default behaviour
mod1 <- (lmerFill(y~x+country+religion+(1|village), data=dat, NArows = list(dat$country==3), fillvar=list("religion"), Fillmethod="base",check=FALSE))

## Set NAs to model center, or variable mean, or whatever we should call it
## Seems better
## Interestingly (but sensibly), this changes only the value estimated for the effect of the country with missing data
summary(lmerFill(y~x+country+religion+(1|village), data=dat, NArows = list(dat$country==3), fillvar=list("religion"), Fillmethod="mean",check=FALSE))

aa <- lmerMM(y~x+country+religion+(1|village), data=dat, NArows = list(dat$country==3), fillvar=list("religion"), Fillmethod="base",check=FALSE)

