
library(ordinal)
library(lme4)

## Some data with a structural NA
## Religion not collected from country 3; we added a dummy level ("4")
## depends on functions in redundancy.R

set.seed(2830)
l <- 400

y <- sample(1:3, l, prob=1:3, replace=TRUE)
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

fillbase <- clmmFill(formula,data=dat,NArows = list(dat$country==3), fillvar=list("religion"),Fillmethod="base")
mmbase <- CMM(formula,data=dat,NArows = list(dat$country==3), fillvar=list("religion"),Fillmethod="base")
CMbase <- mmbase[,c(1:6)]
fillmean <- clmmFill(formula,data=dat,NArows = list(dat$country==3), fillvar=list("religion"),Fillmethod="mean")
mmmean <- CMM(formula,data=dat,NArows = list(dat$country==3), fillvar=list("religion"),Fillmethod="mean")
CMmean <- mmmean[,c(1:6)]

fillbase$coefficients
fillmean$coefficients

checkS <- solve(t(CMmean)%*%CMmean)%*%t(CMmean)%*%CMbase
print(checkS)
SS <- matrix(data=0,nrow=7,ncol=7)
SS[1,1] <- 1
SS[2:7,2:7] <- checkS
bhat <- SS%*%fillbase$coefficients

# 
# 
# # debug(clmm)
# # undebug(clmm)
# # debug(clmmFill)
# # undebug(clmmFill)
# # 
# # debug(ordinal:::getCtrlArgs)
# # undebug(ordinal:::getCtrlArgs)
# # 
# 
# 
# summary(lme4:::glmer(formula,data=dat,family = binomial))
# summary(glmerFill(formula, data=dat, NArows = list(dat$country==3), fillvar=list("religion"), Fillmethod="base",check=FALSE,family=binomial))
# summary(glmerFill(formula, data=dat, NArows = list(dat$country==3), fillvar=list("religion"), Fillmethod="mean",check=FALSE,family=binomial))
# 
# 
# # debug(glmerFill)
# # undebug(glmerFill)