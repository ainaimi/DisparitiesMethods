packages <- c("data.table","tidyverse","skimr","here")

for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package, repos='http://lib.stat.cmu.edu/R/CRAN')
  }
}

for (package in packages) {
  library(package, character.only=T)
}

thm <- theme_classic() +
  theme(
    legend.position = "top",
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA)
  )
theme_set(thm)

expit <- function(x){1/(1+exp(-x))}

# data gen for SEM example

n <- 2000000
c <- rnorm(n)
x <- rbinom(n,1,expit(-2 + log(1.5)*c))
l <- rbinom(n,1,expit(-2 + log(2.5)*x))
m <- rbinom(n,1,expit(-2 + log(1.5)*c + log(2.5)*x + log(2.5)*l))
y <- 120 + 1.5*c + 1.5*x + 2.5*l + 1.5*m + rnorm(n)
a <- tibble(c,x,l,m,y)

summary(a)

pM <- glm(m ~ c + x + l, data=a, family = binomial(link = "logit"))$fitted.values
pX <- glm(x ~ c, data=a, family = binomial(link = "logit"))$fitted.values

wX <- (mean(a$x)/pX)*a$x + ((1-mean(a$x))/(1-pX))*(1-a$x)
wM <- (mean(a$m)/pM)*a$m + ((1-mean(a$m))/(1-pM))*(1-a$m)

wXM <- wX*wM

mu <- lm(y ~ x + m + l + c, data=a)
eta <- glm(m ~ c + x + l, data=a, family = binomial(link = "logit"))
zeta <- glm(l ~ x, data=a, family = binomial(link = "logit"))
gComp <- function(exposure){
  
  ndat <- transform(a, x = exposure, m = 0)
  
  l_pred <- as.numeric(predict(zeta, newdata=ndat, type="response")>runif(nrow(ndat)))
  ndat <- transform(ndat, l = l_pred)
  
  # m_pred <- as.numeric(predict(eta, newdata=ndat, type="response")>runif(nrow(ndat)))
  # ndat <- transform(ndat, m = m_pred)
  
  y_pred <- predict(mu, newdata=ndat)
  
  return(mean(y_pred))
}

# IP weighting
round(summary(lm(y ~ x + m, data=a, weights=wXM))$coefficients[2,1],2)

# G Comp
round(gComp(1) - gComp(0),2)

# Standard Regression
round(summary(lm(y ~ x + m + l + c, data=a))$coefficients[2,1],2)

library(lavaan)

model <- ' # direct effect
             y ~ a0*1 + a1*x + a2*m + a3*l + a4*c
           # mediator
             m ~ b0*1 + b1*x + b2*c + b3*l
           # indirect effect (a*b)
             ab := a1*b1
         '
fit <- sem(model, data = a)
obj <- summary(fit)
obj$pe[2,]