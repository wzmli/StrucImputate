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

# Now set the NAs to really be NAs
dat <- droplevels(within(dat, {
	religion[country==3] <- NA
	education[religion==2] <- NA
	
}))

# Set NAs to base level; this matches the default behaviour (but without the dummy level, so better)
lmbase <- lmFill(y~x+country+religion, dat, NArows= list(dat$country==3), fillvar=list("religion"), Fillmethod="base",check=FALSE)
mmbase <- lmMM(y~x+country+religion, dat, NArows= list(dat$country==3), fillvar=list("religion"), Fillmethod="base",check=FALSE)
err <- y - mmbase%*%lmbase$coefficients
var_err <- (t(err) %*% err)/(nrow(mmbase)-ncol(mmbase))

varlmbase <- vcov(lmbase)
varlmbase_hand <- as.numeric(var_err) * solve(t(mmbase)%*%mmbase)

print(varlmbase)
print(varlmbase_hand)

all.equal(varlmbase_hand,varlmbase)

# Set NAs to model center, or variable mean, or whatever we should call it
# Seems better
# Interestingly (but sensibly), this changes only the value estimated for the effect of the country with missing data
lmmean <- lmFill(y~x+country+religion, dat, NArows = list(dat$country==3), fillvar=list("religion"),Fillmethod="mean",check=FALSE)
mmmean <- lmMM(y~x+country+religion, dat, NArows= list(dat$country==3), fillvar=list("religion"), Fillmethod="mean",check=FALSE)

errmean <- y - mmmean%*%lmmean$coefficients
var_errmean <- (t(errmean) %*% errmean)/(nrow(mmmean)-ncol(mmmean))

CM <- CMFill(y~x+country+religion, dat,rowfill=4, colfill=c(5,6))
bhat <- solve(CM) %*% lmbase$coefficients

print(CM)
print(lmbase)
print(bhat)
print(lmmean$coefficients)

all.equal(bhat,lmmean$coefficients)


# 
# varlmmean <- vcov(lmmean)
# varlmmean_hand <- as.numeric(var_errmean) * solve(t(mmmean)%*%mmmean)
# 
# print(lmmean)
# print(varlmmean)
# print(varlmmean_hand)
# 
# all.equal(varlmbase_hand,varlmbase)




# ### multiple structural NA
# 
# 
# formula2 <- y~x+country+religion+education
# summary(lm(formula2, data=dat))
# 
# # Set NAs to base level; this matches the default behaviour (but without the dummy level, so better)
# summary(lmFill(y~x+country+religion+education, dat, NArows  = list(dat$country==3,dat$religion==2), fillvar=list("religion","education"), Fillmethod="base",check=FALSE))
# 
# summary(lmFill(y~x+country+religion+education, dat, NArows  = list(dat$country==3,(dat$religion==2)&!is.na(dat$religion==2)), fillvar=list("religion","education"), Fillmethod="mean",check=FALSE))
# 
# 


print("Modelling Mike's way")
bhat_mike <- solve(t(mmmean)%*%mmmean)%*%t(mmmean)%*%mmbase%*%lmbase$coefficients
print(bhat_mike)
errmean_mike <- y - mmmean%*%bhat_mike
var_errmean_mike <- (t(errmean_mike) %*% errmean_mike)/(nrow(mmmean)-ncol(mmmean))
varlmmean_hand <- as.numeric(var_errmean) * solve(t(mmmean)%*%mmmean)
all.equal(varlmmean_hand,vcov(lmmean))



T_hand <- solve(solve(t(mmmean)%*%mmmean)%*%t(mmmean)%*%mmbase)
print(T_hand)