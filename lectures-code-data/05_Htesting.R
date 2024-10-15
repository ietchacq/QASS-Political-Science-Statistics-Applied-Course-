# 0. Set-up ####

# Packages
library("tidyverse")
library("BSDA") # z.test() function

# Working directory to get/save files
setwd(file.path(Sys.getenv("USERPROFILE"),"Box/0 Teaching/Intro to R/2023_Rice_POLI102/materials/"))
#macOS users: setwd("/Users/yourusername/yourfoldername/anotherfolderifnested")

# Bringing the data in
data <- read.csv("qog_std_cs_jan23.csv") #QoG cross-sectional data

# Variables we'll use
data <- select(data, #select() selects only variables indicated below and disregards the rest 
               cname, # Country Name
               wdi_wip, # Proportion of seats held by women in national parliaments (%)
               br_dem) # Is the country a democracy or not? 

data<-drop_na(data) # dropping all NAs for simplicity, just in case

# The CS data is the last data available around 2022. So, it's current data. 
# Let's assume given is 2023 # we assume most countries should have at least
# 30% female legislators (considering 50% women in the population)

# So let's test the null hypothesis that, on average, countries have 30% female legs
# i.e. H0: mu/mean = 30; HA mu/mean != 30 

# Summary of the variable we'll use
summary(data$wdi_wip)

# Checking assumptions that data comes from normal distribution
# We have lots of data (193 obs) so it should be ok, but if you still want to check:
qqnorm(data$wdi_wip) # normal quantile-quantile plot
qqline(data$wdi_wip) # data lies close to y = x, no notable deviations, 
# so ok to treat as coming from normal


# 1.	Z-test ####
# On 'manual' z-test: https://cran.r-project.org/web/packages/distributions3/vignettes/one-sample-z-test.html

# Function and default arguments
?z.test
z.test(x, # for one-sample test only one object
       y, # you can add second sample for 2 sample test 
       alternative='two.sided', # default 2 sided, other options: "less", "greater"
       mu=0, # true value of the mean set to 0 by default
       sigma.x=NULL, # population sd of first sample
       sigma.y=NULL, # if you have 2, population sd of second sample
       conf.level=.95, # 0.95 default, set your desired conf. interval here
       ...)

## i. One sample ####
# Assuming population mean = 30% female leg, sd = 10% variation
# Testing the null that mean is indeed = 30%; can we reject it?
z.test(data$wdi_wip, mu=30, sigma.x = 10)

    # In output:
      # z -> the test statistic
      # p-value -> p-value (when seeing e- is standard scientific notation for powers of 10; it'll be close to 0)
                            # e.g., 2.2*10^-16

# At 95% confidence we can reject the null that the average percentage of female is 30%

# Actual in-sample values, just in case
mean(data$wdi_wip, na.rm=T) #22.67078
sd(data$wdi_wip, na.rm=T)#12.25035

## ii. Two sample ####

democracies <- data %>% filter(br_dem == 1) # new df only with democracies
autocracies <- data %>% filter(br_dem == 0) # new df only with autocracies

# Performing a 2 sample z-test to determine if the mean of % female legislators
# is different between democracies and autocracies

z.test(democracies$wdi_wip, autocracies$wdi_wip, 
       mu=0, # testing the null than mean differences btn samples is 0
       sigma.x=12,# and in general, these around 12
       sigma.y=12)

# We can't reject the null that difference in average % female legislators equals 0
# i.e.,  we conclude that the mean % female leg is not significantly different between 
# autocracies and democracies in this sample

# 2. T-test ####

# Notice in t-tests since we don't need to know, we are not obliged to set sd

# Function and default arguments
?t.test
t.test(x, # for one-sample test only one object
       y = NULL, # one sample is default, but you can add second sample for 2 sample test 
       alternative = "two.sided", # other options: "less", "greater"
       mu = 0, # true value of the mean set to 0
       paired = FALSE, # other option: TRUE
       var.equal = FALSE, # other option: TRUE
       conf.level = 0.95, # 0.95 default, set your desired conf. interval here
       ...)

## i. One sample ####

# Same H0 (null hypothesis) as before but now with t-tests
t.test(data$wdi_wip, mu=30)

# Again at 95% confidence we can reject the null that the average percentage of female is 30%

## ii. Two sample ####

# Two-sample t-test with unequal variance
t.test(democracies$wdi_wip, autocracies$wdi_wip) # given default is var.equal = FALSE
                                                 # this assumes variances differ

# similar output as before, but now it also tells you the degrees of freedom (df)

# Two-sample t-test with equal variance
t.test(democracies$wdi_wip, autocracies$wdi_wip,
       var.equal = T) #changing default to TRUE

## iii. Unpaired/Independent t-test ####

# All the above have been unpaired/independent bc default is paired = F
# Which makes sense in this setting because these are independent groups

## iv. Paired/Dependent t-test ####

# But we could have dependent groups, e.g. same countries measured in two time periods
# Imagine we want to check if something changed before and after the pandemic outbreak (2020)
# we'll have before as the year before the pandemic outbreak (2019), and after as year after (2021)

data2 <- read.csv("qog_std_ts_jan23.csv") 

data2 <- select(data2, #select() selects only variables indicated below and disregards the rest 
               cname, # Country Name
               year, # Year
               wdi_wip) # Proportion of seats held by women in national parliaments (%)

before <- data2 %>% filter(year == 2019)

after <- data2 %>% filter(year == 2021)

t.test(before$wdi_wip, after$wdi_wip,
       paired = T)

# Given p-value we reject the null hypothesis of no difference

mean(before$wdi_wip, na.rm = T) #22.76579
mean(after$wdi_wip, na.rm = T) #24.68232

# 3.	P-values and Confidence Intervals ####

# functions above tell you the p-value already!
# but here's one of many tutorials on how to things by yourself: 
  ## P-values: https://www.cyclismo.org/tutorial/R/pValues.html#
  ## Confidence intervals: https://www.r-bloggers.com/2021/11/calculate-confidence-intervals-in-r/


###################################
# NEXT: Data Visualization III ####
###################################
# Visualizing (potential) association btn vars
# Some of this statistics in plots

####################################
# THEN: Measures of Association ####
####################################

# Effect sizes and association: 
## Cohen’s d
## ANOVA
## F-statistic
## eta squared
## Odds ratio
## chi-squared
## Correlation, Pearson’s r 

# HAVE YOU SEEN WILCOX INDEX? 
# https://data-flair.training/blogs/hypothesis-testing-in-r/

# OTHER MEASURES YOU WANT/NEED TO SEE HOW TO DO IN R?
# Let me know!


