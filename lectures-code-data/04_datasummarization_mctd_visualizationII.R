# 0 Set up####

# Packages we'll use (remember to install them if needed first...)
library("tidyverse") #well, ... everything. (today 4 some data wrangling functions)
library("e1071") #to use its skewness() function
library("skimr") #to use its skim() function 

# Working environment
setwd(file.path(Sys.getenv("USERPROFILE"),"Box/0 Teaching/Intro to R/2023_Rice_POLI102/materials/"))

# Data set

# code book and .csv data set files in "The Quality of Government (QoG) data sets" Canvas Module

data <- read.csv("qog_std_ts_jan23.csv") #this is a big one, it can take a few seconds

# Variables we'll use
data <- select(data, #select() selects only variables indicated below and disregards the rest 
               cname, # Country Name
               year, # Year
               wdi_gini, # Gini index
               wdi_gdppppcon2017, # GDP, PPP (constant 2017 international dollar)
               undp_hdi, # Human Development Index
               ht_colonial, # Colonial Origin
               gol_est, # Electoral System Type-3 classes
               gol_inst) # Country's regime type
               

# 1 Taking a look at our data ####

table(data$cname) # what countries are in the data?
table(data$year) # what years?

glimpse(data) # glipmse at # rows and columns, var names and type, first observations
head(data) #first 6 rows of your data

# filtering and asigning to a new data object ONLY US country and years after 1999 
data_s21_usa <- data %>%
  filter(year > 1999 & cname == "United States of America (the)")
glimpse(data_s21_usa)
head(data_s21_usa)

# 2 Measures of Central Tendency ####

## Mean ####

colnames(data)

data$iris <- "iris"

#mean() function
mean(data$wdi_gini) #if data contains NA it will output an NA too 
                    #bc can't summarize/average over non-numeric values
mean(data$wdi_gini, na.rm = T) #that's why you need to specificy na.rm = T
                               #now, you are calculating the mean considering complete obs.
rm() # to remove objects from environmnet 
#tidyverse
summarise(data,mean = mean(wdi_gini, na.rm = T))

## Median ####
#median()
median(data$wdi_gini, na.rm = T)

  # Mean is [1] 38.35625
  # Median [1] 36.2
  # What does this mean?

## Mode ####

# No in-built mode() function in R. But we can create it!
# Code thxs to Gregor Thomas answer here: https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode


# Create the function:
mode <- function(x, na.rm = FALSE) { # assign to object 'mode' a function that will do the below to object x (any) specified within (), and have na.rm = F as default
  if(na.rm){ #if removing NA
    x = x[!is.na(x)] #access the vector of non-NA values
  }
  
  ux <- unique(x) #get the unique values
  return(ux[which.max(tabulate(match(x, ux)))]) #and tell me which is more frequent
}

mode(data$wdi_gini) #again, if we don't remove NA output is NA
mode(data$wdi_gini, na.rm = T) 

table(data$wdi_gini) # yeahp, 35.7 is the most frequent value, with 17 observations

# 3 Measures of Dispersion ####

## Standard Deviation ####
sd()
sd(data$wdi_gini, na.rm = T)

## Variance ####
var(data$wdi_gini, na.rm = T) #var() for SAMPLE variance

### Population variance ####

# want to calculate the population variance out of this sample one?
# Adapting code here: https://sdsclub.com/how-to-find-variance-in-r-examples-included/#:~:text=var(data)*(n%2D1)%2Fn&text=So%2C%20we%20can%20use%20the,the%20following%20when%20calculated%20manually.


# Following the sample and population variance formulas We'll use this computation: var(data)*(n-1)/n
# but removing NAs since we may have them

# To drop na from entire data set: 
data_nna <- drop_na(data) # but we won't use this

# Gini vector object only with complete observations
wdi_gini_nna <- data$wdi_gini[!is.na(data$wdi_gini)] # we used this above... vector of NON-NA values

# Get the length (the n) of such vector
n <- length(wdi_gini_nna)

# Now we can get the population variance by following the var(data)*(n-1)/n formula
var(wdi_gini_nna)*(n-1)/n

### Side example of variance and manually calculating things ####

# Population variance
population <- c(1,2,4,5) # n = 4
mean_pop = sum(population)/4
variance_pop = sum((population-mean_pop)^2)/4

# Sample variance
# pick 2 elements from population above, e.g., 1 and 5
sample <- c(1,5) # n = 2
mean_sam = sum(sample)/2
Variance_sam = sum((sample-mean_sam)^2)/(2-1)

## Interquartile Range (IQR) ####
IQR(data$wdi_gini, na.rm = T) #inter-quantile range

quantile(data$wdi_gini, na.rm = T) #quantiles

## Skewness ####

skewness(data$wdi_gini, na.rm = T)

  # Mean is [1] 38.35625
  # Median [1] 36.2
  # What does this mean? 
    # Positive skewness => mean is larger than the median, thus distribution is positively or right-skewed
    # That is, more values are on the left side (tail) of the distribution while the right one is longer (bc of extrem values - pushing mean to be > median)

# 4 Data Visualization II ####

## Histogram ####
#With ggplot: http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization

# Histogram of Gini variable
hist(data$wdi_gini, # variable we want to plot
     main = "Distribution of Gini Variable", #title
     xlab = "Values", # Lab of x axis
     border = "gray") #changing bar border aesthetics 

# Let's add a line indicating the distribution mean
abline(v = mean(data$wdi_gini, na.rm = T), # abline() will add a line in the v= value you indicate, in this case the mean
       col = "red", #line color
       lwd = 3, lty = 1) #just some aesthetics on the width and type of line

# What about the actual center of the distribution?
range(data$wdi_gini, na.rm = T) #range of data
(max(data$wdi_gini, na.rm = T)-min(data$wdi_gini, na.rm = T))/2 + min(data$wdi_gini, na.rm = T)
# max value minus min divided by two
# added to min value, to get the theoretical center
# this will be our line
# Let's add a line there too 
abline(v = (max(data$wdi_gini, na.rm = T)-min(data$wdi_gini, na.rm = T))/2 + min(data$wdi_gini, na.rm = T), col = "red", lwd = 2, lty = 2)

# Median?
abline(v = median(data$wdi_gini, na.rm = T), # abline() will add a line in the v= value you indicate, in this case the mean
       col = "green", #line color
       lwd = 3, lty = 1) 


## Density plot####
# http://www.sthda.com/english/wiki/ggplot2-density-plot-quick-start-guide-r-software-and-data-visualization
density(data$wdi_gini, na.rm = TRUE)
plot(density(data$wdi_gini, na.rm = TRUE), # density we want to plot
             main = "Distribution of Gini Variable", #title
             xlab = "Values", # Lab of x axis
             border = "gray")

# Let's add a line indicating the distribution mean
abline(v = mean(data$wdi_gini, na.rm = T), # abline() will add a line in the v (stands for vertical) = value you indicate, in this case the mean
       col = "red", #line color
       lwd = 3, lty = 1) #just some aesthetics on the width and type of line

# 1 standard deviation ABOVE the mean (note: mean + sd)
abline(v = mean(data$wdi_gini, na.rm = T)+sd(data$wdi_gini, na.rm = T), # abline() will add a line in the v= value you indicate, in this case the mean
       col = "navy", #line color
       lwd = 2, lty = 1) #just some aesthetics on the width and type of line

# 1 standard deviation BELOW the mean (note: mean - sd)
abline(v = mean(data$wdi_gini, na.rm = T)-sd(data$wdi_gini, na.rm = T), # abline() will add a line in the v= value you indicate, in this case the mean
       col = "navy", #line color
       lwd = 2, lty = 1) #just some aesthetics on the width and type of line

## Box plot####
# More: https://r-charts.com/distribution/add-points-boxplot/#:~:text=Single%20box%20plot%20with%20points,-Adding%20points%20(strip&text=You%20need%20to%20pass%20the,added%20over%20the%20previous%20plot
# With ggplot: http://www.sthda.com/english/wiki/ggplot2-box-plot-quick-start-guide-r-software-and-data-visualization

boxplot(data$wdi_gini, na.rm = T, #var we want to plot
        ylim=c(min(data$wdi_gini, na.rm = T),max(data$wdi_gini, na.rm = T)), 
        main = "Range of Gini Variable")
# Adding median
abline(h = median(data$wdi_gini, na.rm = T), # h for horizontal line (v before for vertical) 
       col = "red", #line color
       lwd = 2, lty = 1) 
# Adding max and min
abline(h = min(data$wdi_gini, na.rm = T),
       col = "green",
       lwd = 2, lty = 1)
abline(h = max(data$wdi_gini, na.rm = T),
       col = "green",
       lwd = 2, lty = 1)
# Adding Q1 and Q2
quantile(data$wdi_gini, na.rm = T) #remember quantiles
# Q1 = 25%
abline(h = quantile(data$wdi_gini, na.rm = T)[2],
       col = "blue",
       lwd = 2, lty = 1)
# Q3 = 75%
abline(h = quantile(data$wdi_gini, na.rm = T)[4],
       col = "orange",
       lwd = 2, lty = 1)

# 5 Z-score ####
#Example: https://www.r-bloggers.com/2020/02/how-to-compute-the-z-score-with-r/

#Remember: z = (x - mean(x))/sd(x)
# Using vector with non-NA values we created above
z = (wdi_gini_nna - mean(wdi_gini_nna))/sd(wdi_gini_nna)

#We can also add a variable to our data with the z-scores
data$wdi_gini_zscore <- (data$wdi_gini - mean(data$wdi_gini, na.rm = T))/sd(data$wdi_gini, na.rm = T)

# 6 Data Summarization ####

## Built-in functions summarizing data set ####

summary(data) #gives you summary of data by providing some of these statistics for each var 
skim(data) #from skimr package. Same, even with mini histograms

#We can make summaries for our data sets with summarize() from dplyr (tidyverse package):
#summarize(approval, unemployment_mean = mean(unemployment))
#operations, we can do multiple at once:
summarize(data,
          gini_mean = mean(wdi_gini, na.rm = T),
          gdp_mean = mean(wdi_gdppppcon2017, na.rm = T),
          hdi_mean = mean(undp_hdi, na.rm = T))


## Summarizing data by groups ####

#This comes from the Book in syllabus, section 2.2 to 2.3.7

# Grouped summaries using tidyverse's group_by

#This is a very fun function! This task consists in collapsing rows until obtaining
#one row per observation that summarizes the information of different groups in the
#dataset.

#For doing this, first we need to have variables that group our observations (party,                                                                             country, region, etc.). We will let R know what is the variable we are grouping our
#oberrvations, and this new dataset will be the same as the original dataset, but R
#will know that the next operations we make need to be grouped.

data_by_country <- group_by(data, cname)

summarize(data_by_country,
          gini_mean = mean(wdi_gini, na.rm = T),
          gdp_mean = mean(wdi_gdppppcon2017, na.rm = T),
          hdi_mean = mean(undp_hdi, na.rm = T))

# Same but piping:
data %>% group_by(cname) %>% #since we're piping (/going) through the data set, we just say country
  summarize(gini_mean = mean(wdi_gini, na.rm = T), # we don't specify data name here either
            gdp_mean = mean(wdi_gdppppcon2017, na.rm = T),
            hdi_mean = mean(undp_hdi, na.rm = T))

### Visualization: Summary based on groups ####

# ht_colonial Colonial origin var
# 0. Never colonized by a Western overseas colonial power
# 1. Dutch
# 2. Spanish
# 3. Italian
# 4. US
# 5. British
# 6. French
# 7. Portuguese
# 8. Belgian
# 9. British-French
# 10. Australian

data$ht_colonial_labels <- recode(data$ht_colonial,
                                  "0" = "None",
                                  "1" = "Dutch",
                                  "2" = "Spanish",
                                  "3" = "Italian",
                                  "4" = "US",
                                  "5" = "British",
                                  "6" = "French",
                                  "7" = "Portuguese",
                                  "8" = "Belgian",
                                  "9" = "British-French",
                                  "10" = "Australian")

data %>% 
  group_by(ht_colonial_labels) %>%
  summarise(Mean = mean(wdi_gini, na.rm = T),) %>%
  ggplot(aes(x = ht_colonial_labels, y = Mean, fill = ht_colonial_labels)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(
    x = "Colonial origin",
    y = "Average Gini Index",
    title = "Average Gini by Colonial Origin")  
