# 1 Fundamentals ####

## (a) ####

install.packages("legislatoR")
library("legislatoR")

## (b) ####
help(legislatoR)
??legislatoR

# Comment describing something about the package here

## (c) ####

x <- seq(5,130,3)

x

typeof(x)
class(x)

x <- as.integer(x)

## (d) ####

l <- list("a" = x, 
          "b" = NULL, 
          "c" = 25 > 25,
          "d" = c("book", "movie", "eat"),
          "e" = abs(-x^2))

l

typeof(l)
class(l)
str(l)

## (e) ####

a <- c(12, 13, 15, 1328, 0.5)
b <- c(6, 13, 523, 13.8, 2/3)

c <- NULL

for (i in 1:5) {
  if (b[i] > a[i]) {
    c[i] = "b is greater than a"
  } else if (a[i] > b[i]) {
    c[i] = "b is smaller than a"
  } else {
    c[i] = "they are equal"
  }
}

c

# 2 Bringing In and Creating Data ####

## (a) ####

install.packages("poliscidata")
library("poliscidata")

data_nes <- nes

## (b) ####

setwd(file.path(Sys.getenv("USERPROFILE"),"Box/0 Teaching/Intro to R/2023_Rice_POLI102/hw/hw01/"))
data_hw01 <- read.csv("hw01-data.csv")

## (c) ####

set.seed(0311)

data_random <- data.frame (
  var1 = rbinom(100, 1,.5),
  var2 = rnorm(100,0,1),
  var3 = runif(100, 0, 1)
)

write.csv(data_random, "data_random.csv", row.names = F)

# 3 Data Cleansing and Visualization ####

library("tidyverse")

## (a) ####

?plot
plot(nes$married, 
     main = "Frequency of Married Variable", 
     xlab = "Married?", 
     ylab = "Number of Respondents", 
     col = 1:3)

# Comment on plot: This is a bar chart/plot. More married people than not. 

## (b) ####

### (i) ####
data_hw01$expenditure_log <- log(data_hw01$expenditure)

### (ii) ####
data_hw01$winning <- ifelse(data_hw01$voteshare > 50 , 1, 0)

### (iii) ####
data_hw01 <- rename(data_hw01, gender_text= gender)

### (iv) ####
data_hw01$incumbent_gender <- case_when(data_hw01$incumbency == 'yes' & data_hw01$gender_text == "male" ~ "Incumbent male",
                                        data_hw01$incumbency == 'yes' & data_hw01$gender_text == "female" ~ "Incumbent female",
                                        data_hw01$incumbency == 'no' & data_hw01$gender_text == "male" ~ "Nonincumbent male",
                                        data_hw01$incumbency == 'no' & data_hw01$gender_text == "female" ~ "Nonincumbent female")

# show this with piping and mutate in class
data_hw01 <- data_hw01 %>% mutate(
  incumbent_gender = case_when(data_hw01$incumbency == 'yes' & data_hw01$gender_text == "male" ~ "Incumbent male",
                               data_hw01$incumbency == 'yes' & data_hw01$gender_text == "female" ~ "Incumbent female",
                               data_hw01$incumbency == 'no' & data_hw01$gender_text == "male" ~ "Nonincumbent male",
                               data_hw01$incumbency == 'no' & data_hw01$gender_text == "female" ~ "Nonincumbent female")
) 

### (v) ####

# Crosstab with base R 
table(data_hw01$incumbent_gender, data_hw01$winning)
# You could a mosaic plot out of this table too
plot(table(data_hw01$winning,data_hw01$incumbent_gender))

# Adding frequencies too
# https://libraryguides.mcgill.ca/c.php?g=699776&p=4968551#:~:text=Crosstabulations%20(2%2D%20way%20frequencies),of%20Total%20Income%20by%20Agegroup.
mytable <- table(data_hw01$incumbent_gender, data_hw01$winning) # incumbent_gender will be rows, winning will be columns
margin.table(mytable, 1) # winning frequencies (summed over incumbent_gender)
margin.table(mytable, 2) # incumbent_gender frequencies (summed over winning)
prop.table(mytable) # cell percentages
prop.table(mytable, 1) # row percentages
prop.table(mytable, 2) # column percentages

# Mosaic plot with base R
# https://www.r-bloggers.com/2021/08/how-to-plot-categorical-data-in-r-quick-guide/
counts <- table(data_hw01$winning, data_hw01$incumbent_gender)
mosaicplot(counts, xlab='Match Result', ylab='Team',main='Wins by Team', col=2:1)

# Crosstab and mosaic plot with xtp() from poliscidata package
xtp(data_hw01, #data set
    incumbent_gender,# variable we want to see frequencies of 
    winning) #across levels of this other variable

# Bar chart with gg plot
ggplot(data_hw01, aes(x=incumbent_gender, y=winning)) + geom_col()
