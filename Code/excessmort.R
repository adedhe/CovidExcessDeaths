# Excessmort ----
library(excessmort) 
library(lubridate)
library(spatstat.utils)
library(spatstat.data)
library(tidyverse)

# do either of the following
# do this next line
total.deaths <- read.csv("/Users/.../totaldeaths_excessmort.csv")
# OR do this next line
total.deaths <- read.csv("/Users/.../totaldeaths_excessmort_prepandemic.csv")

# do either of the following
# do this next line - excluded dates of the pandemic
exclude_dates <- c(seq(make_date(2020, 03, 01), make_date(2021, 12, 01), by = "month"))
# OR do this next line - for pre-pandemic - exclude one year at a time
exclude_dates <- c(seq(make_date(2014, 01, 01), make_date(2014, 12, 01), by = "month"))

# default harmonics = 1
# make sure date is in the YYYY-MM-DD format
total.deaths$date <- as.Date(total.deaths$date)
counts <- total.deaths %>% 
  compute_expected(exclude = exclude_dates, harmonics = 1, keep.components = TRUE)

expected_plot(counts, title = "Monthly Mortality Counts in Pune City")

# -- Creating diagnostic plots
mean_diag <- expected_diagnostic(counts)

# -- Trend component
mean_diag$trend

# -- Seasonal component
mean_diag[["seasonal"]][["labels"]]$x <- "Months of the year"
mean_diag$seasonal

# -- Fitting excess model to data 
# default knots = 2
fit <- total.deaths %>% 
  excess_model(start = min(.$date),
               end = max(.$date),
               exclude = exclude_dates,
               knots.per.year = 2,
               verbose = FALSE)
excess_plot(fit, title = "Deviations from expected mortality in Pune City") 


# -- Intervals of inordinate mortality found by the excess model
fit$detected_intervals

# -- Computing excess deaths in Pune from start date - for pandemic
cumulative_deaths  <- excess_cumulative(fit, 
                                        start = make_date(2020, 03, 01),
                                        end   = make_date(2021, 12, 01))

# -- Computing excess deaths in Pune from start date - for pre-pandemic
cumulative_deaths  <- excess_cumulative(fit, 
                                        start = make_date(2014, 01, 01),
                                        end   = make_date(2014, 12, 01))

# -- Visualizing cumulative excess deaths in Pune
cumulative_deaths %>%
  ggplot(aes(date)) +
  geom_ribbon(aes(ymin = observed- 2*sd, ymax = observed + 2*sd), alpha = 0.5) +
  geom_line(aes(y = observed),
            color = "white",
            size  = 1) +
  geom_line(aes(y = observed)) +
  geom_point(aes(y = observed)) +
  scale_y_continuous(labels = scales::comma) +
  labs(x        = "Date",
       y        = "Cumulative excess deaths",
       title    = "Cumulative Excess Deaths in Pune",
       subtitle = "From March 2020 to Dec 2021")

# Most important section!!
# run this below loop twice 
# first with make_date(2020, i, 01), make_date(2020, i, 01)
# then with make_date(2021, i, 01), make_date(2021, i, 01)

# -- if doing for pre-pandemic leave-one-out, only use the line below appropriately
# then with make_date(2019, i, 01), make_date(2019, i, 01)


std_devs<-c()
expected<-c()
for (i in 1:12){
  interval<-  list(covid19 = seq(make_date(2014, i, 01), make_date(2014, i, 01), by = "month"))
  print(interval)
  final_answer<- total.deaths %>% 
    excess_model(exclude        = exclude_dates,
                 intervals      = interval,
                 frequency      = 12,
                 verbose        = TRUE)
  
  std_devs<- c(std_devs, final_answer[["sd"]])
  expected<- c(expected, final_answer[["expected"]])
} 

# this following line is the final answer
excess_mort_holder<-data.frame(std_devs,expected )
excess_mort_holder<-as.data.frame(excess_mort_holder)

total.deaths$outcome
fitdistr(total.deaths$outcome, densfun = "Poisson")

# Copying and pasting OP's input numbers into one long string
# https://www.reddit.com/r/rprogramming/comments/tr2a6i/how_to_check_if_the_data_is_poisson_distributed/
num_string <- total.deaths$outcome
mean(num_string)
sd(num_string)

x <- rnorm(72, mean = 2479.333, sd = 363.0985)
y <- rnorm(72, mean = 2479.333, sd = 2479.333)


# Splitting the string into individual strings of numbers
# The following steps aren't necessary if your input is already set as numbers
x <- as.integer(num_string) # Converts trimmed strings to integer

# Mean and number of observations for Poisson distributed random numbers
x_mean <- mean(x)
nobs <- length(x)
set.seed(46290)
poisson_data <- rpois(nobs, x_mean)

# Generating density, plotting histogram of original input data, and overlaying Poisson density curve
# Does our data look Poisson distributed based on the density curve?
poisson_density <- density(poisson_data)
hist(x, prob = TRUE)
lines(poisson_density, col = "red")
