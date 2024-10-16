# DATA CLEANSING AND DATA VISUALIZATION I LAB #

# 0 Setup ####

# Working directory
setwd(file.path(Sys.getenv("USERPROFILE"),"Box/0 Teaching/Intro to R/2023_Rice_POLI102/materials/"))

# Packages
install.packages("poliscidata")
library("poliscidata") # This is a package of datasets and functions featured in Pollock's and Edwards' book: An R Companion to Essentials of Political Analysis, Second Edition
    # More on the package here: https://uk.sagepub.com/sites/default/files/upm-assets/81279_book_item_81279.pdf
    # We'll use it now to get some of their data. 
    # The poliscidata package has four datasets. Description below taken from pp. 16 of the pdf in the link above:
    #1. gss. This dataset has selected variables from the 2012 General Social Survey, a random sample of 1,974
    #adults aged 18 years or older, conducted by the National Opinion Research Center and made available
    #through the Inter-university Consortium for Political and Social Research (ICPSR) at the University of
    #Michigan. Some of the scales in gss were constructed by the authors. The variables in the gss dataset
    #are described in the Appendix (Table A.1).
    #2. nes. This dataset includes selected variables from the 2012 National Election Study, a random sample of
    #5,916 citizens of voting age, conducted by the University of Michigan’s Institute for Social Research and made
    #available through ICPSR. See the Appendix (Table A.2).
    #3. states. This dataset includes variables on each of the 50 states. Most of these variables were compiled by
    #the authors from various sources. A complete description of variables in the states dataset is found in the
    #Appendix (Table A.3).
    #4. world. This dataset includes variables on 167 countries of the world. These variables are based on data
    #compiled by Pippa Norris, John F. Kennedy School of Government, Harvard University, and made available to
    #the scholarly community through her Internet site. See the Appendix (Table A.4) for a complete description of
    #variables in the world dataset.
library(tidyverse) # we'll use it both for data wrangling/cleaning and plotting

# 0. Re-cap & > ####
#https://www.poliscidata.com/rdemos/showLiveDemo.php?code=createSimpleGraphic.txt&chapter=Create+a+Simple+Graphic

# This sample code creates objects, performs computations, and produces a graphic.
# Try changing the value of thisIntercept and thisSlope 

# Assigning Values to Objects 
thisIntercept = 0 # see you can use the = too, though generally <- will serve for most things
thisSlope = -1
?runif # check this function out, useful to create data from a uniform distribution, 
        # first argument is the number of observations you want, and the other two the lower and upper limits (must be finite numbers) of the distribution
simDataX = runif(20, -10, 10) #this function is telling R: give me 20 random observations/data points taken from a uniform distribution with limits -10,10
simDataX # and we indeed get some random data (20 data points), out of a uniform distribution limited from -10 to 10
         # since this is a random data generating process, each time you run the code generating this, the data will change
         # if you want the data produced to always be the same, you must first run set.seed(...) inserting whatever number you want within the brackets
set.seed(1) #a 1 'seed' is set, and we get the same data everytime, since the random generating process is set to that seed specifically
simDataX = runif(20, -10, 10) #now, if you run this (every time after running the set.seed line), you'll see the same data points
simDataX 

# Performing Computations with Objects
simDataY = thisIntercept + thisSlope*simDataX + runif(20, -10, 10) #this shows you that we can perform the same math calculations we performed before w/ objects too

# Creating Graphics (with base R)
plot(x="", y="", xlim=c(-10,10), ylim=range(simDataY), xlab="X Values", ylab="Y Values") #you can run the code in just one line, but for the purpose of explaining...
plot(x="", # creating a blank canvas, not really specifying x
     y="", # nor the y we'll plot 'against it'
     xlim=c(-10,10), #the limits of the x var, fixed from -10 to 10
     ylim=range(simDataY), #the limits of y var, fixed to be limited to whatever the range of the simDataY variable we created is
     xlab="X Values", #the name or label of the x axis 
     ylab="Y Values") #the name or label of the y axis

# the function abline() adds lines to a previous plot() when run right after it
abline(a=thisIntercept, b=thisSlope) #a defines the intercept, b the slope; we set those to be the values we defined above

#point() adds data points
points(x=simDataX , y=simDataY, col="red") # now using the two variables we created, we plot the data points with their corresponding x and y coordinates

abline(lm(simDataY ~ simDataX), col="red") # we can go a bit further and ad a regression line showing the correlation between our X and Y variables 
                                           # this is done with the lm() function, but we'll see this at the end of the course

# 1. Exploring Your Data ####
# https://www.poliscidata.com/rdemos/showLiveDemo.php?code=exploreNESdataset.txt&chapter=Explore+the+NES+Dataset


# Explore contents of dataset and variable in dataset
# Commands to explore an object, such as the NES dataset
cat("Number of rows in NES dataset:\n") #cat() to just concatenate and print
nrow(nes) # rows = number of observations

cat("\n\nNumber of columns in NES dataset:\n")
ncol(nes) #columns = how many variables it has

cat("\n\nDimensions of the NES dataset:\n")
dim(nes) #both observations and variables

# Variable names
colnames(nes)

# Look at info recorded in the first rows of the NES dataset.
# Only 1 row: These responses come from one of the people who participated in the NES survey.
cat("\n\nValues recorded in NES dataset, row 1:\n") # so, each single response from one participant who took the survey
t(nes[1,]) # To make it easier to read, we can transpose the row into a column with the t() function.

# 6 first rows:
head(nes) #head to cheack first observations of each var in the datset
head(nes[1:4]) #if you just want to see a given set of variables, e.g. from first to fourth

# Assign the NES data to an object so you can check best check it out
nes_df <- nes 
class(nes_df)
typeof(nes_df)

# If we want to call a specific variable of this data set will use the $ symbol 
# and follow the following form: data$variable, so e.g.,:
nes$deathpen #outputs the deathpen variable of the NES data set 

# Explore specific variable in the NES dataset: opinions about death penalty.
# The deathpen variable is the 370th variable/column in our NES dataset, so it is nes[,370]
# Rather than reference by column number, it is much easier to reference it by name.
# To keep the output simple, we are only viewing first 20 observations.
# The levels() command shows you all the unique values of a variable.
cat("First 20 responses to death penalty question in the dataset:\n")
nes$deathpen[1:20]

cat("\n\nLevels of the variable values:\n") 
levels(nes$deathpen) # levels tells you the values/categories of this var

# 2. Cleaning Your Data ####

## i. Renaming Variables ####

# Now we'll use tidyverse piping, explained but not shown in previous code
# the pipe %>% is used to perform subsequent tasks, for example, let's assignt to the object nes_df, the same nes data frame 
# but now with a variable w/changed name 
nes_df <- nes %>% rename(deathpenalty = deathpen) #in one line

# Broken up, to explain:
nes_df <- #assign the following to nes_df
  nes %>% # the ness data frame
  rename(deathpenalty = deathpen) #and then change the variable name (newname = oldname)

# you can do this without piping too (example renaming back to original)
nes_df <- rename(nes_df, deathpen = deathpenalty) #notice before when piping we don't add as argument the data object name, 
                                                  # but now, since we didn't pipe, inside rename() we first need to specify the data to which we'll change variable names 

#you can also do more than one at the same time
nes_df <- rename(nes_df, 
                 newname_obama_therm = obama_therm,
                 newname_own_dog = own_dog)

## ii. Recoding Variables ####
# https://www.sfu.ca/~mjbrydon/tutorials/BAinR/recode.html

#In these examples we're assigning recoded variables to same var name, so they'll be replaced

### Single Variable Condition ####

# Using if...else statements (base R)
# Recoding Support for Government assistance to African Americans scale, 7 point scale (where 1 govt helps, to 7 help themselves) to 1 they support, 0 they don't
table(nes$aidblack_self) # before
nes$aidblack_self <- ifelse(nes$aidblack_self <= 4, #if this var is less or equal to 4
                            1, #recode to 1
                            0) #else recode to 0
table(nes$aidblack_self) # after

# Tidyverse case_when() function
# similar with Support for defense spending, 7 point scale (1 less to 7 more support) / using case_when() 
table(nes$defsppr_self) #before
nes$defsppr_self <- case_when(nes$defsppr_self >= 5 ~ 1, #when this variable greater or equal to 5 recode to 1
                              nes$defsppr_self < 5 ~ 0) #when smaller recode to 0
table(nes$defsppr_self) #after

# Tidyverse recode
# Favor reducing the Federal budget deficit, 7 point scale using
table(nes$budget_deficit_x) # before
nes$budget_deficit_x <- recode(nes$budget_deficit_x, #with recode you do: old_value = new_value
                               "FavStrng" = "Favours Strongly", # we use "" because these are strings/character values
                               "FavWeak" = "Favours Weakly",
                               "FavLean" = "Favours Leaning",
                               "Neither" = "Neither Favours nor Opposes",
                               "OppLean" = "Opposes Leaning",
                               "OppWeak" = "Opposes Weakley",
                               "OppStrng" = "Opposes Strongly")
table(nes$budget_deficit_x) # after

### Multiple Variable Condition ####
# Examples with mutate here https://www.statology.org/conditional-mutating-r/

# Combine conditions as seen in code from previous lab (using logic operators), you can do this in many forms, this is one example:
# Considering our just recoded vars, people who both support government assistance to African Americans & support for defense spending
nes$supports_defense_nodeficit <- case_when(nes$defsppr_self == 1 & nes$aidblack_self == 1 ~ 1, 
                                            nes$defsppr_self == 0 | nes$aidblack_self == 0 ~ 0)
nes$supports_defense_nodeficit
table(nes$supports_defense_nodeficit)

## iii. New Variables (Data Transformation) ####

# Now we'll do similar as above but assigning to different varible names
# You can do the recoding above but still save your original data by just changing the object name to which you're assigning the recoding
# everytime you data$variable <- something, a new variable will be created in your data set if the name is one not yet used in any other column

# So we've been doing this already, just need to change variable name to which we are assigning things, just one more example:

nes_df$newname_obama_therm # is a var with the feeling thermometer towards Obama (measure how positively individuals feel about Obama)

table(nes_df$newname_obama_therm) # table() will show values and their frequency, we see it goes from 0 to 100
range(nes_df$newname_obama_therm, #range of the var
      na.rm = T)# removing missing values so that it shows actual range

# Let's create a new var out of this one that just says you're either likely to have positive feelings towards Obama vs. you're not
nes_df$obama_positive <- case_when(nes_df$newname_obama_therm > 50 ~ 'Likely yes', 
                                   nes_df$newname_obama_therm <= 50 ~ 'Likely not')

nes_df$obama_therm_01 <- case_when(nes_df$newname_obama_therm > 50 ~ 1, 
                                   nes_df$newname_obama_therm <= 50 ~ 0)

# let's cross tab, which we'll see below a bit more
table(nes_df$obama_positive, nes_df$obama_therm_01) # those that we coded as likely yes have a feeling thermometer value > 50, 
                                                    # those likely not, <= 50

## iii. New Variables (Data Creation) ####

### Random Distributions ####
# A bit more info if you want it: https://www.datascienceblog.net/post/basic-statistics/distributions/
# We can create variables out of probability distributions suing random sampling functions r... as seen below. 
# These are the most frequently used ones and their default values in R 
#rnorm(n, mean = 0, sd = 1) n for number of observations you want, mean/second argument for mean, sd/third arg. for standard deviation
#runif(n, min = 0, max = 1) n for number of observations you want, second and third arguments for lower and upper bounds respectively
#rbinom(n, size, prob) n for number of observations you want, # trails/observation, probability of success 

nes_df$var_norm <- rnorm(5916, 1, 23) # do same N as in data, otherwise error
nes_df$var_unif <- runif(5916, -5, 5) # do same N as in data, otherwise error
nes_df$var_bin <- rbinom(5916, 1, 0.5) # do same N as in data, otherwise error

#check these vars at the end of your data

### Vectors ####
nes_df$var_vector1s <- 1 # open you data go to the end, you'll see this creates a vector/new variable full of 1s

### NULL ####

nes_df$var_na <- NA # open you data go to the end, you'll see this creates a vector/new variable full of NA values
nes_df$var_null <- NULL # open you data go to the end, you'll see this DOESN'T create a vector/new variable full of NULL values

# Explanation here: https://www.r-bloggers.com/2018/07/r-null-values-null-na-nan-inf/
# Whenever you want to create an empty variable, best to just fill it with NAs (missing values)

# 3. Visualizing Your Data ####

## i. Frequencies, Tables, Cross Tabs ####

### Frequencies ####
#https://www.poliscidata.com/rdemos/showLiveDemo.php?code=analyzeDeathPenaltyOpinions.txt&chapter=Analyze+Opinions+about+the+Death+Penalty
# Describe national opinions about the death penalty using table and figure.
# Use sample weights so the statistics are nationally representative.
cat("\n\nFrequency Distribution Table:\n")
table(nes$pid_x) # just number of observations
freqC(nes$pid_x) # both observations, and relative frequencies. This function is from the poliscidata package, so call it to your library if you want to use it 
                 # if you get an 'Error in plot.new() : figure margins too large' error, just enlarge your plot panel manually

### Tables ####
# Frequency distribution of a single column

table(nes$envjob_3) #frequencies of categories of var from question asking about Environment or jobs more important

### Cross Tabs ####
# Cross tabs also tell you frequency distributions but across levels of a different variable, are useful to make comparisons

# Base R
# more here: https://libraryguides.mcgill.ca/c.php?g=699776&p=4968551#:~:text=Crosstabulations%20(2%2D%20way%20frequencies),of%20Total%20Income%20by%20Agegroup.
table(nes$envjob_3, nes$pid_3)

# Poliscidat xtp()
?xtp # cross-tabulation of dependent and indendent variables, also creates a mosiac plot. Makes use of the crosstab function in the descr (base R) package.
#https://www.poliscidata.com/rdemos/showLiveDemo.php?code=crossTabMosaicPlotBasic.txt&chapter=Basic+Cross-Tabulation+and+Mosaic+Plot
cat("Cross Tabulation:\n")
xtp(nes, #data set
    envjob_3,# variable we want to see frequencies of 
    pid_3, #across levels of this other variable
    wt) # since public opinion data, we're also applying weights here to make it nationally representative, but this not needed code-wise

## ii. Scatter Plot, Barplot, Histogram ####

### Basic Scatter Plots ####

# Simple Scatterplot base R
# https://www.statmethods.net/graphs/scatterplot.html (more there too)
attach(mtcars)
plot(wt, mpg, main="Scatterplot Example",
     xlab="Car Weight ", ylab="Miles Per Gallon ", pch=19)
# Add fit lines
abline(lm(mpg~wt), col="red") # regression line (y~x) (following bonus track seen at the beggining in 0. Re-cap & >)
lines(lowess(wt,mpg), col="blue") # lowess line (x,y)

# Simple Scatterplot Tidyverse (ggplot)
# Quick start guide: http://www.sthda.com/english/wiki/ggplot2-scatter-plots-quick-start-guide-r-software-and-data-visualization
ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point()

### Barplot ####
# Example taken from (and way more here too): https://www.geeksforgeeks.org/data-visualization-in-r/

# Horizontal Bar Plot for 
# Ozone concentration in air
barplot(airquality$Ozone, # Now using R built in data
        main = 'Ozone Concenteration in air',
        xlab = 'ozone levels', horiz = TRUE)

# Vertical Bar Plot for 
# Ozone concentration in air
barplot(airquality$Ozone, main = 'Ozone Concenteration in air', 
        xlab = 'ozone levels', col ='blue', horiz = FALSE)

### Histogram ####
# Now using the GSS dataset that's in the poliscidata package

# Histograms with base R
hist(gss$age) # hist() will just give you the histogram of the specified variables

# Histograms with poliscidata package
#the wtd.hist() function of the poliscidata package allows you to also apply weights, which are imporant to make public opinion data nationally representative
wtd.hist(gss$age, weight=gss$wtss, main="This level of detail (5 year groups) is better.", col="gray80") 

# Compare (indeed they are a bit different given weights)
par(mfrow=c(1,2)) # combine plots into one graphic, by defining a graphical panel of 1 row and 2 columns 
hist(gss$age)
wtd.hist(gss$age, weight=gss$wtss, main="Histogram with weights", col="gray80")

dev.off() # to rest par() values to original

# Histograms with ggplot
# have fun...: http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization
ggplot(gss, aes(x=age)) + geom_histogram()

library(poliscidata)
rm(nes)

nes$class2 <- 1
