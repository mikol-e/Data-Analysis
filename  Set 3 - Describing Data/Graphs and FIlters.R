rm(list=ls())
library("utils")
library("tidyverse")
library("stringr")

# Set your own working directory here
setwd("/Users/Micaela/Courses/Data Analysis/Problem Sets/ Set 3 - Describing Data ")

# Save your data using tidyverse
gender_data<-as_tibble(read.csv("Gender_StatsData.csv"))

#You can look how the data set looks by inspection using this commands.
head(gender_data)
tail(gender_data)
str(gender_data)

# Generate a tibble called teenager_fr that contains only the adolescent fertility rate indicator for each country-year
teenager_fr<-filter(gender_data,Indicator.Code=="SP.ADO.TFRT")

# Get rid of the quite large Data Set that is not of our interest.
rm(gender_data)

# It is possible to calculate statistics with simple R code mean(), sd(), min(), max(), sum(), etc.
round(mean(teenager_fr$X1960, na.rm = TRUE),2) # na.rm = TRUE specifies R to calculate the mean of the sample only for values that are not NaN.
round(sd(teenager_fr$X1960, na.rm = TRUE),2)
# Rounded sample mean of fertility rates in year 2000
round(mean(teenager_fr$X2000, na.rm = TRUE),2)
round(sd(teenager_fr$X2000, na.rm = TRUE),2)

head(teenager_fr)
# Use dyplr filter() command and the logical %in% to keep only relevant Country.Code observations.
byincomelevel<- filter(teenager_fr,Country.Code%in%c("LIC","MIC","HIC","WLD"))

# Plot one observation per income group and year.
plotdata_bygroupyear<-gather(byincomelevel,Year,FertilityRate,X1960:X2015) %>%
  select(Year, Country.Name,Country.Code, FertilityRate)
print(plotdata_bygroupyear)

# You prefer to look at the data at the year level and have the fertility rates for each income-group as separate variables.
plotdata_byyear<-select(plotdata_bygroupyear,Country.Code,Year,FertilityRate)%>%
  spread(Country.Code,FertilityRate)
print(plotdata_byyear)

# Question 12
#Let's enter the code as if this statement were TRUE
plotdata_bygroupyear<-gather(byincomelevel,Year,FertilityRate,X1960:X2015) %>%
  select(Year, Country.Name,Country.Code, FertilityRate) #from Question 10
plotdata_byyear<-spread(plotdata_bygroupyear,Country.Code,FertilityRate)
print(plotdata_byyear)
#Let's enter the code as if this statement were FALSE
plotdata_bygroupyear<-gather(byincomelevel,Year,FertilityRate,X1960:X2015) %>%
  select(Year, Country.Name,Country.Code, FertilityRate)
plotdata_byyear<-select(plotdata_bygroupyear,Country.Code,Year,FertilityRate)%>%
  spread(Country.Code,FertilityRate)
#We see from this that the answer to Question 12 is FALSE: using select() is not redundant

# Plotting the fertility rate over time, separetly for each income level.
ggplot (plotdata_bygroupyear, aes(x=Year,y=FertilityRate, group=Country.Code,colour=Country.Code)) +
  geom_line(  )+
  labs(title='Fertility Rate by Country-Income-Level over Time') # makes header title

# Transform the Year variable using dplyr mutate command with the stringr as.numeric() command.
plotdata_bygroupyear<-mutate(plotdata_bygroupyear,Year=as.numeric(str_sub(Year,-4)))
print(plotdata_bygroupyear)

#Question 18
histdata_twoyears <- select(teenager_fr, Country.Name, Country.Code, Indicator.Name, Indicator.Code, X1960,X2000)
print(histdata_twoyears)

histdata_twoyears <- gather(teenager_fr, Year, FertilityRate, X1960, X2000) %>%
  select(Year, Country.Name, Country.Code, FertilityRate) # generates this columns
print(histdata_twoyears)

histdata_twoyears <- filter(histdata_twoyears,!is.na(FertilityRate)) # filter FertilityRate with no NaNs
print(histdata_twoyears)

# alpha stands for transparency in the graphic
ggplot(histdata_twoyears, aes(x=FertilityRate)) + 
  geom_histogram(data=subset(histdata_twoyears, Year=="X1960"), 
                 color="darkred", fill="red", alpha=0.2) + 
  geom_histogram(data=subset(histdata_twoyears, Year=="X2000"), 
                 color="darkblue", fill="blue", alpha=0.2) 
ggsave("hist.png")

# binwidth, breaks and bins allows you to change the number of bins.

# ADD Kernels to the histogram
ggplot(histdata_twoyears, aes(x=FertilityRate, group=Year, color=Year, alpha=0.2)) +        geom_histogram(aes(y=..density..)) +
  geom_density(data=subset(histdata_twoyears, Year=="X1960"), color="darkred", fill="red", alpha=0.2, bw=5)+ 
  geom_density(data=subset(histdata_twoyears, Year=="X2000"), color="darkblue", fill="blue", alpha=0.2, bw=5)

# If the weighting function is not bell-shaped you can use other like rectangular, which doesn't underweight observations at boundaries of each bin.

#Part 2
#Question 2
rm(list=ls())
library("utils")
#install.packages('plot3D')
library(plot3D)
setwd("/Users/Micaela/Courses/Data Analysis/Problem Sets/ Set 3 - Describing Data")

#Creating the vector x and y
M <- mesh(seq(0,1,length=100), seq(0,1,length=100))
x<-M$x
y<-M$y
z<-6/5*(M$x+M$y^2) # joint density function

#Plotting this joint pdf in 3D
persp3D(x, y, z, xlab='X variable', ylab= 'Y variable', xlim = c(0,1), main= "Plotting joint pdf')")

#Calculating cdf
x <- seq(0, 1, length=1000)
y <- seq(0, 1, length=1000)
cdfy <- 6/5 * (1/2*y+y^3/3)
cdfx <- 6/5*(1/3*x+x^2/2)

#Plotting cdf and saving it as a pdf
pdf("cumulative.pdf")
plot(x, cdfx, type = "l", col="blue", xlab=" ", ylab = "Cumulative Probability", xlim=c(0,1), main="CDF plot")
lines(y, cdfy, lty=2, col="red", lwd=2)
legend("bottomright", ncol=1, legend = c("X", "Y"), lty=c(1,2), col=c("blue", "red"))
dev.off()
