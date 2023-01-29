packages <- c("data.table","tidyverse","skimr","here","survival")

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

# data gen for KM example

n <- 2000000
x <- rbinom(n,1,.5)
c <- 5 + 5*runif(n)
y <- rexp(n, exp(log(.2) + log(1.5)*x))
start_time <- 0
stop_time <- pmin(y,c)
event <- as.numeric(y>c)

a <- tibble(x, c, y, start_time, stop_time, event)
summary(a)

a

surv_km <- survfit(Surv(time = start_time, 
                        time2= stop_time, 
                        event = event) ~ x, 
                   data = a)

# create dataset for plotting
plot_dat <- tibble(Year=c(0,example_surv$time),Risk=c(0,1-example_surv$surv))

# examine dataset
plot_dat

# plot KM curve
km_plot <- ggplot() + 
  geom_step(data=plot_dat,aes(x=Year,y=Risk),
            direction="hv") +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0), limits=c(0,1))