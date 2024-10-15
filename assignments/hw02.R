# 0 Set up####

# Packages we'll use (remember to install them if needed first...)
library("tidyverse") #well, ... everything. (today 4 some data wrangling functions)
library("e1071") #to use its skewness() function
library("skimr") #to use its skim() function 

# Working environment
setwd(file.path(Sys.getenv("USERPROFILE"),"Box/0 Teaching/Intro to R/2023_Rice_POLI102/materials/"))

# 1 ####

## (a) ####

data <- read.csv("qog_std_cs_jan23.csv") #this is a big one, it can take a few seconds

# Variables we'll use
data <- select(data, #select() selects only variables indicated below and disregards the rest 
               cname, # Country Name
               undp_hdi, # Human Development Index
               gol_inst, # Country's regime type
) 

## (b) ####
glimpse(data)
skim(data)

# 2 ####

## (a) ####
mean(data$undp_hdi, na.rm = T)
median(data$undp_hdi, na.rm = T)

# Create the function:
mode <- function(x, na.rm = FALSE) { # assign to object 'mode' a function that will do the below to object x (any) specified within (), and have na.rm = F as default
  if(na.rm){ #if removing NA
    x = x[!is.na(x)] #access the vector of non-NA values
  }
  
  ux <- unique(x) #get the unique values
  return(ux[which.max(tabulate(match(x, ux)))]) #and tell me which is more frequent
}

mode(data$undp_hdi, na.rm = T) 

## (b) ####
sd(data$undp_hdi, na.rm = T)
IQR(data$undp_hdi, na.rm = T)
skewness(data$undp_hdi, na.rm = T)

# 3 ####

## (a) ####

hist(data$undp_hdi, # variable we want to plot
     main = "Distribution of HDI Variable", #title
     xlab = "Values", # Lab of x axis
     border = "gray") #changing bar border aesthetics 

# Let's add a line indicating the distribution mean
abline(v = mean(data$undp_hdi, na.rm = T), # abline() will add a line in the v= value you indicate, in this case the mean
       col = "red", #line color
       lwd = 3, lty = 1)
# Median
abline(v = median(data$undp_hdi, na.rm = T), # abline() will add a line in the v= value you indicate, in this case the mean
       col = "green", #line color
       lwd = 3, lty = 1) 

## (b) ####

boxplot(data$undp_hdi, na.rm = T, 
        main = "Range of HDI Variable")
# Adding median
abline(h = median(data$undp_hdi, na.rm = T), # h for horizontal line (v before for vertical) 
       col = "red", #line color
       lwd = 2, lty = 1) 
# Adding max and min
abline(h = min(data$undp_hdi, na.rm = T),
       col = "green",
       lwd = 2, lty = 1)
abline(h = max(data$undp_hdi, na.rm = T),
       col = "green",
       lwd = 2, lty = 1)
# Adding Q1 and Q2
# Q1 = 25%
abline(h = quantile(data$undp_hdi, na.rm = T)[2],
       col = "blue",
       lwd = 2, lty = 1)
# Q3 = 75%
abline(h = quantile(data$undp_hdi, na.rm = T)[4],
       col = "orange",
       lwd = 2, lty = 1)

## (c) ####
table(data$gol_inst)
data$gol_inst_labels <- recode(data$gol_inst,
                                  "0" = "Parliamentary democracy",
                                  "1" = "Semi-presidential democracy",
                                  "2" = "Presidential democracy")

data %>% 
  group_by(gol_inst_labels) %>%
  summarise(Mean = mean(undp_hdi, na.rm = T),) %>%
  ggplot(aes(x = gol_inst_labels, y = Mean, fill = gol_inst_labels)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(
    x = "Regime Type",
    y = "Average HDI",
    title = paste(
      "Average HDI by Regime Type"
    )
  )

