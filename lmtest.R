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

religion[country==3] <- 4
education[religion==2] <- 5

dat <- data.frame(x=x, y=y
	, religion=as.factor(religion)
	, country=as.factor(country)
	, education=as.factor(education)
)

## Naive lm works, but not so clear what it does
formula <- y~x+country+religion
summary(lm(formula, data=dat))

# Set NAs to base level; this matches the default behaviour (but without the dummy level, so better)
lmbase <- lmFill(y~x+country+religion, dat, NArows= list(dat$country==3), fillvar=list("religion"), Fillmethod="base",check=FALSE)
summary(lmbase)

# Set NAs to model center, or variable mean, or whatever we should call it
# Seems better
# Interestingly (but sensibly), this changes only the value estimated for the effect of the country with missing data
lmmean <- lmFill(y~x+country+religion, dat, NArows = list(dat$country==3), fillvar=list("religion"),Fillmethod="mean",check=FALSE)

## Compute model matrix under original and transformed (X and N)
CMbase <- CMM(formula,data=dat,NArows = list(dat$country==3), fillvar=list("religion"),Fillmethod="base",check=FALSE)
CMmean <- CMM(formula,data=dat,NArows = list(dat$country==3), fillvar=list("religion"),Fillmethod="mean",check=FALSE)

## 
checkS <- solve(t(CMmean)%*%CMmean)%*%t(CMmean)%*%CMbase
print(checkS)
bhat <- checkS%*%lmbase$coefficients[!is.na(lmbase$coefficients)]

## Are our two bhats the same? Yes
print(bhat)
print(lmmean$coefficients[!is.na(lmmean$coefficients)])


## Are the two vcovs the same? Yes!
print(vcov(lmmean))
print(checkS%*%vcov(lmbase)%*%t(checkS))



## check interaction term

lmint <- lm(y~x+country*religion,data=dat)
print(summary(lmint))
