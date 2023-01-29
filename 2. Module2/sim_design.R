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

set.seed(123)
## simulate some data
# inverse logit function
expit <- function(x){1/(1+exp(-x))}

# data gen for ptb example

library(truncnorm)

n <- 7653

race <- rbinom(n, 1, .43)
bmi <- 26 + 2.7*race + rnorm(n, 0, 2.5)
prior_ptb <- rbinom(n, 1, expit(-2.5 + log(3)*race))
prepreg_smoking <- rbinom(n, 1, expit(-3.5 + log(1.75)*race))
high_school <- rbinom(n, 1, expit(-.5 + log(2)*race))
wic <- rbinom(n, 1, expit(-2.5 + log(4)*race))
maternal_age <- rtruncnorm(n, a = 18, b = Inf, mean = 27, sd = 4.5)
overall_diet <- rlnorm(n, meanlog = 2 - log(1.5)*race, sdlog = .3)

green_veg <- rbinom(n, 1, expit(-2 + 
                                log(2)*race + 
                                log(.3)*scale(overall_diet) +
                                log(1.1)*scale(maternal_age) +
                                log(1.75)*wic +
                                log(1.75)*high_school +
                                log(.8)*prepreg_smoking +
                                log(1.15)*prior_ptb +
                                log(.8)*scale(bmi)))

mean(green_veg)

ptb <- rbinom(n, 1, expit(-3 + 
                          log(3)*race + 
                          log(2)*green_veg +
                          log(1.3)*scale(overall_diet) +
                          log(1.1)*scale(maternal_age) +
                          log(.75)*wic +
                          log(.85)*high_school +
                          log(2)*prepreg_smoking +
                          log(6)*prior_ptb +
                          log(1.15)*scale(bmi)))

mean(ptb)

ID <- 1:n

a <- tibble(ID, ptb, green_veg, overall_diet, maternal_age, bmi, prepreg_smoking, prior_ptb, high_school, wic, race)

write_csv(a, here("data","vegetable_data.csv"))