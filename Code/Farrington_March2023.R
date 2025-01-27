# loading the required packages

library(surveillance) 
library(spatstat.utils)
library(spatstat.data)

# loading the raw data
# choose the appropriate file! - do individually for 2020 and 2021
# note how the file for 2021 contains values from 2014-2019 and 2021 - it does not contain data from 2020!
# this is intentional!

total.deaths <- read.csv("/Users/.../Downloads/totaldeaths_2020.csv")
total.deaths <- read.csv("/Users/.../Downloads/totaldeaths_2021.csv")

# for the leave-one-out
total.deaths <- read.csv("/Users/.../Downloads/SubmittedRevision_SecondRound/Supporting_Files_Revisions/FarringtonPrePandemic/totaldeaths_2016.csv")


# next few lines are overwriting the properly formatted salmonella raw object (type = list)
data("salmonella.agona")
new_data<-salmonella.agona
new_data$observed<-total.deaths$total.deaths
new_data$state<-integer(length(new_data$observed))

# we do not have weekly data - only have monthly data
new_data[["freq"]] <- 12

#ç<-as.double(12)
new_data$start<-as.double(c(2014,1))
# for leave on out
#new_data$start<-as.double(c(1,1))

n <- length(new_data$observed)
#Do surveillance for the last 12 months - do it for 2020 and 2021 independently
#Range will always remain n-11:n
#Set control parameters.
#b=5 for pandemic years; b=4 for leave-one-out
control <- list(b=1,w=1,range=(n-11):(n),reweight=TRUE, verbose=FALSE,alpha=1)
res <- algo.farrington(new_data,control=control)
#Plot the result.
plot(res,disease="COVID-19",method="Farrington", legend=FALSE, ylim = c(0,6000))

final_answer <- as.data.frame(res[["upperbound"]])

# confirm upper and lower bounds
# the upper bound was a one-sided 95% prediction interval i.e., alpha = 0.05
# the lower bound was computed using average expected deaths i.e., alpha = 1

# Farrington ends here 