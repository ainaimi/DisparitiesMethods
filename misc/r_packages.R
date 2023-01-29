packages <- c("data.table","tidyverse","skimr","here", "lubridate", "htmltab")

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

url <- "https://cran.r-project.org/web/packages/available_packages_by_date.html"
r_packages <- htmltab(doc = url)

a <- as_tibble(r_packages) %>% 
  mutate(date = ymd(Date), year = year(date))

b <- a %>% 
  group_by(year) %>% 
  summarize(count = n()) %>% 
  ungroup() 

b <- b %>% 
  arrange(year) %>% 
  mutate(count = cumsum(count))

ggplot(b) + 
  geom_line(aes(x = year, y = count), color = "blue") +
  scale_x_continuous(expand = c(0,0))  +
  scale_y_continuous(expand = c(0,0), limits = c(0,20000)) +
  xlab("Year") + ylab("Number of Packages") +
  ggtitle("Number of CRAN Packages Over Time")

ggsave(here("figures","CRAN_packages.pdf"))