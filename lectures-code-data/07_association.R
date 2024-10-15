# 0. Set-up ####

# Packages
library("tidyverse")
library("effectsize") #cohen's d, eta squared, oddsratio
library("effsize") #cohen's d (just like output more)
library("epitools") #oddsratio (just like output more)


# Working directory to get/save files
setwd(file.path(Sys.getenv("USERPROFILE"),"Box/0 Teaching/Intro to R/2023_Rice_POLI102/materials/"))
#macOS users: setwd("/Users/yourusername/yourfoldername/anotherfolderifnested")

## Data ####

# We'll use the COVID case again (but now 2019-2020), 
# to test whether association described online exists
# Did the COVID pandemic decreased happiness and 
# increased violence worldwide?

# So, bringing the time series QoG data in
data_ts <- read.csv("qog_std_ts_jan23.csv") 

# Selecting happinnes and violence variables of interest
data <- select(data_ts, #select() selects only variables indicated below and disregards the rest 
               cname, # Country Name
               year, # Year
               br_dem, # Electoral democracy
               whr_hap, # World happiness index (subjective well-being)
               wvs_hap, # Feeling of happiness
               svs_ind, # Societal violence index
               who_homf, # Homicide Rate, Female
               wdi_homicidesf) # Intentional homicides, female (per 100,000 female)

data$br_dem_label <- case_when(data$br_dem == 1 ~ "Democracies", #new var = Yes when br_dem = 1 (democracy)
                               data$br_dem == 0 ~ "Autocracies") #and = No when br_dem = 0 (i.e., autocracies)

# Subseting our before and after covid treatment
before <- data %>% filter(year == 2019)
after <- data %>% filter(year == 2020)

# Same but before and after groups in same data set
data_treat <- data %>% # go through the time series data
  filter(year == 2019 | year == 2020) %>%
  mutate(#mutate() used to make changes to the data set, in this case, add a new var
    group = case_when(year == 2019 ~ 0, # Control (C) before covid
                      year == 2020 ~ 1) # Treatment (T) after covid outbreak
  )

# Cohen’s d ####

# Standardized effect size for measuring the difference between 
# two group means: difference between two means, expressed in
# standard deviation units. I.e., how many standard deviations 
# lie between the two means

data_treat$group <- as.factor(data_treat$group)
cohen.d(data_treat$whr_hap, data_treat$group,#'happiness' index
        na.rm=TRUE)

# negative d ~ second mean is larger, in this case T

cohen.d(data_treat$wvs_hap, data_treat$group,# feelings of happiness
        na.rm=TRUE)

# positive d ~ second mean is smaller, in this case T (so C greater)

#Cohen's d Interpretation
#0.00 < 0.20	Negligible
#0.20 < 0.50	Small
#0.50 < 0.80	Medium
#0.80 or more	Large

# Want to see a large effect? Effect size of democracy on happiness...
data_treat$br_dem <- as.factor(data_treat$br_dem)
cohen.d(data$whr_hap, data$br_dem,
        na.rm=TRUE)

# ANOVA ####
# Analysis of variance, 1- vs 2-way just way to say factors examined
# Nice explanation: https://bookdown.org/steve_midway/DAR/understanding-anova-in-r.html
# On aov() vs. anova(): https://www.statology.org/aov-vs-anova-in-r/

## One-way ####
anova_1w <- aov(whr_hap ~ group, 
                data = data_treat)
anova_1w #output
summary(anova_1w) #a bit more info
                  #check difference again w/democracy instead

# In output we get to see:
  # Degrees of freedom
  # Sum of squares 
  # Mean squares
  # F-statistic
  # P-values

# If you want the coefficients
anova_1w$coefficients

## Two-way ####
# (as one type of factorial ANOVA)
# http://www.sthda.com/english/wiki/two-way-anova-test-in-r
anova_2w <- aov(whr_hap ~ group + br_dem, 
                data = data_treat)
summary(anova_2w)

#If wanting to plot to explore differences visually, adapt:
#library(ggpubr)
#\ggboxplot(data_treat, x = "group", y = "whr_hap", color = "br_dem_label",
#palette = c("#00AFBB", "#E7B800"))

# F-statistic ####

# You have it in ANOVA output
# F-statistic is the ratio of two variances 

# F-test
# F-test of equality of variances is a test for the null hypothesis
# that two normal populations have the same variance
# Term based on the fact that it uses F-values to test the hypotheses
var.test(before$whr_hap,after$whr_hap)

# want F critical value?
#https://www.geeksforgeeks.org/how-to-find-the-f-critical-value-in-r/

# Eta squared ####
# measure of effect size for analysis of variance (ANOVA) models
eta_squared(anova_1w) #partial is default
eta_squared(anova_2w)

# Interpretation: out of the total variation in Y, 
# the proportion that can be attributed to a specific X 

#If no guidelines are provided, you can follow this:
#η2 = 0.01 indicates a small effect
#η2 = 0.06 indicates a medium effect
#η2 = 0.14 indicates a large effect

# Odds ratio ####
#https://www.statology.org/odds-ratios-in-r/
# strength of the association between two events
# Indicates: how much higher the odds of treatmemt/exposure are among case-patients than among controls

# Odds ratio magnitude (strength of association):
  #• 1.0 (or close to 1.0) indicates that the odds of treatment among case-patients are the same as, or
  #similar to, the odds of treatment among controls. The treatment is not associated with the outcome variable
  #• Greater than 1.0 indicates that the odds of treatment among case-patients are greater than the odds of
  #treatment among controls, indicating potential positive effect of treatment
  #• Less than 1.0 indicates that the odds of treatment among case-patients are lower than the odds of
  #treatment among controls, indicating potential negative effect of treatment

# We usually perform it on a 2 by 2 table
# Following the COVID effects example found online let's use it
# to asses if happiness relates to violence (homicides) 

# First construct 2 by 2 table of relatively happy vs. unhappy countries
# and relatively high and low homicide countries, after covid
after <- after %>%
  mutate(
    happy = case_when(whr_hap > mean(whr_hap, na.rm = T) ~ "Relatively happy",
                      whr_hap <= mean(whr_hap, na.rm = T) ~ "Relatively unhappy"),
    femhomicides = case_when(wdi_homicidesf > mean(wdi_homicidesf, na.rm = T) ~ "High homicides",
                             wdi_homicidesf <= mean(wdi_homicidesf, na.rm = T) ~ "Low homicides")
  )

table(after$happy, after$femhomicides)

oddsratio(table(after$happy, after$femhomicides))

# Odds ratio turns out to be 0.3409838
  # Interpretation: odds that happy countries have high female homicides
  # are just 0.3409838 times the odds that an unhappy country is amongst the 
  # high homicides subsample.
  # Meaning being a happy country reduces the odds of high number of female homicides
  # by 65.9%.
  # "lower" and "upper" columns indicate the bounds of the confidence interval,
  # so we know the 95% confidence interval for the odds ratio is : [0.10, 1.09].
  # Finally, in the table below, under the "midp.exact" title we can find the p-value
  # associated with the test, in this case the odds ratio are not significant at 0.05, but at 
  # 0.07 given p-value = 0.06978096.

# Chi-squared ####
# To test the two variables are independent
# H0: The two variables are independent.
# H1: The two variables relate to each other.
# We reject null if p-value is statistically significant (less than our predetermined significance level)

# Same thing example as above: happiness and female homicides 
chisq.test(table(after$happy, after$femhomicides))
chisq.test(after$happy, after$femhomicides) #unsignificant relationship

# Alternative example with happiness levels and being and electoral democracy
chisq.test(after$happy, after$br_dem) #how a significant one looks like; bc we can reject

# Correlation, Pearson’s r #### 

# Correlation coefficient goes from -1 to +1
# > 0 ~ positive/direct linear association
# < 0 ~ negative/inverse association
# Values indicate strength of association, a potential rule of thumb:
# +/- 1 ~ Perfect correlation
# +/- 0.91 to +/- 0.99 ~ Very strong
# +/- 0.71 to +/- 0.9 ~ High
# +/- 0.41 to +/- 0.7 ~ Moderate
# +/- 0.21 to +/- 0.4 ~ Small
# +/- 0.01 to +/- 0.2 ~ Slight-negligible
# 0 ~ No correlation

# Better in someone else's words (Mukaka, 2012, pp. 69):

# "[Correlation] It is a dimensionless quantity that takes a value in the range −1 to +1. 
# A correlation coefficient of zero indicates that no LINEAR relationship exists
# between two continuous variables, and a correlation coefficient of −1 or +1 indicates
# a perfect LINEAR relationship. The strength of relationship can be anywhere between 
# −1 and +1. The stronger the correlation, the closer the correlation coefficient comes to ±1.
# If the coefficient is a positive number, the variables are directly related 
# (i.e., as the value of one variable goes up, the value of the other also tends to do so).
# If, on the other hand, the coefficient is a negative number, the variables are inversely 
# related (i.e., as the value of one variable goes up, the value of the other tends to go down)."

#[cont. on linearity:]

# "Any other form of relationship between two continuous variables that is not linear 
# is not correlation in statistical terms. To emphasise this point, a mathematical 
# relationship does not necessarily mean that there is correlation. 
# For example, consider the equation y=2×2. In statistical terms, it is inappropriate
# to say that there is correlation between x and y. This is so because, although there 
# is a relationship, the relationship is not linear over this range of the specified values of x.
# It is possible to predict y exactly for each value of x in the given range, 
# but correlation is neither −1 nor +1. Hence, it would be inconsistent with the definition 
# of correlation and it cannot therefore be said that x is correlated with y."

#Mukaka MM. Statistics corner: A guide to appropriate use of correlation coefficient in medical research. Malawi Med J. 2012 Sep;24(3):69-71. PMID: 23638278; PMCID: PMC3576830.

# Are happiness index and fem homicides correlated?
cor(data$whr_hap,data$wdi_homicidesf, 
    use="complete.obs")  # Not really, low negative correlation
 
# Happiness index and violence?
cor(data$whr_hap,data$svs_ind, 
    use="complete.obs")  # Maybe not with fem homicides, but yes with violence!
                        #  Mid-strong negative correlation

# Happiness index and feelings of happiness var?
cor(data$whr_hap,data$wvs_hap, 
    use="complete.obs") # They should be right? Well, not that much, mid positive correlation
                        # Prob. cause if you check the questions out of which they are constructed,
                        # they actually measure != things.

# The two female homicides vars?
cor(data$wdi_homicidesf,data$who_homf, 
    use="complete.obs") # These are indeed highly positively correlated. Compared to case above, 
                        # this is statistical data referring to same events, so it makes sense different sources agrees in values.
                        # Would be problematic if not. 

# Violence and WDI female homicides var? 
cor(data$svs_ind,data$wdi_homicidesf, 
    use="complete.obs") # mild positive correlation

# Violence and WHO female homicides var? 
cor(data$svs_ind,data$who_homf, 
    use="complete.obs") # mild-low positive correlation
                        # both cases make sense, there should be some correlation, but violence against women,
                        # and gender violence in general, have their own causes, regardless of how violent a society is, 
                        # A greater propensity for violence, if anything, should 'just' make things worse.



