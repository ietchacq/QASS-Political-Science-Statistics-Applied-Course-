# 0. Set-up ####

# Packages
library("tidyverse")
#install.packages("devtools")
#devtools::install_github("cardiomoon/webr")
library("webr") # to plot() t-tests
#install.packages("ggpubr")
library("ggpubr") #for ggboxplot()

# Working directory to get/save files
setwd(file.path(Sys.getenv("USERPROFILE"),"Box/0 Teaching/Intro to R/2023_Rice_POLI102/materials/"))
#macOS users: setwd("/Users/yourusername/yourfoldername/anotherfolderifnested")

# Bringing the QoG data in
data_cs <- read.csv("qog_std_cs_jan23.csv") #CR0SS-SECTIONAL data
data_ts <- read.csv("qog_std_ts_jan23.csv") #TIME-SERIES data

# Variables we'll use
#CR0SS-SECTIONAL data
data_cs <- select(data_cs, #select() selects only variables indicated below and disregards the rest
                  cname, # Country Name
                  gggi_hss, # Global Gender Gap Health and Survival Subindex (0 to 1, where 1 indicates no gap)
                  gggi_pes, # Global Gender Gap Political Empowerment Subindex (0 to 1, where 1 indicates no gap)
                  wdi_wip, # Proportion of seats held by women in national parliaments (%, 0-100)
                  gggi_pos, # Global Gender Gap Economic Participation and Opportunity Subindex (0 to 1, where 1 indicates no gap)
                  br_dem, # Is the country a democracy or not? 
                  ht_region) # The Region of the Country

data_cs <- drop_na(data_cs)

#TIME-SERIES data
data_ts <- select(data_ts, #select() selects only variables indicated below and disregards the rest
                  cname, # Country Name
                  year, # Year
                  gggi_hss, # Global Gender Gap Health and Survival Subindex (0 to 1, where 1 indicates no gap)
                  gggi_pes, # Global Gender Gap Political Empowerment Subindex (0 to 1, where 1 indicates no gap)
                  wdi_wip, # Proportion of seats held by women in national parliaments (%, 0-100)
                  gggi_pos, # Global Gender Gap Economic Participation and Opportunity Subindex (0 to 1, where 1 indicates no gap)
                  br_dem, # Is the country a democracy or not? 
                  ht_region) # The Region of the Country

data_ts <- drop_na(data_ts)

# 1. Hypothesis testing ####
#https://cran.r-project.org/web/packages/webr/vignettes/plot-htest.html
# More: https://www.datanovia.com/en/lessons/how-to-do-a-t-test-in-r-calculation-and-reporting/

## i. One-sample t-test density plot
# Previous Lab: Testing the null hypothesis that, on average, countries today have 30% female legs
# i.e. H0: mu/mean = 30; HA mu/mean != 30 
t.test(data_cs$wdi_wip, mu=30)

# We can simply plot our t-test!
plot(t.test(data_cs$wdi_wip, mu=30)) # we reject null
mean(data_cs$wdi_wip) #what's the sample mean? 24.06871 
plot(t.test(data_cs$wdi_wip, mu=24)) # hey, we don't reject

## ii. Two-sample t-test density plot

# Subsetting our 2 samples: democracies vs. autocracies
democracies <- data_cs %>% filter(br_dem == 1) # new df only with democracies
autocracies <- data_cs %>% filter(br_dem == 0) # new df only with autocracies

# Plotting 2-sample t-test
sd(democracies$wdi_wip)
sd(autocracies$wdi_wip) #let's leave default of unequal variances as is

plot(t.test(democracies$wdi_wip, autocracies$wdi_wip))

## iii. Paired data before and after `treatment'

# Previous Lab: Pandemic outbreak as treatment that might have affected women's representation
before <- filter(data_ts, year == 2019) # pre-COVID outbreak
after <- data_ts %>% filter(year == 2021) # post-COVID outbreak

# There are less countries after, let's indeed pair observations that match 
# (same countries in both data sets, i.e., 1 observation per year)
after <- after %>% #go through current after df
  filter(cname %in% before$cname) #filter those countries which are in the before df,
                                  #and assign those to new after df
before <- before %>% # now same for before df
  filter(cname %in% after$cname)

# T-test
t.test(before$wdi_wip, after$wdi_wip,
       paired = T)

mean(before$wdi_wip)
mean(after$wdi_wip)

# Some data wrangling on original data set to plot this out
data_4boxplot <- data_ts %>% # go through the time series data
  filter(cname %in% after$cname) %>% #select only those countries in the after/before data set, bc they have both data for 2019 and 2021
  mutate(#mutate() used to make changes to the data set, in this case, add a new var
    group = case_when(year == 2019 ~ "Before, 2019", # create group variable that indicates treatment condition
                      year == 2021 ~ "After, 2021")
  ) %>%
  drop_na() #drop_na again (case_when is gonna generate NA in all other years not 2019 or 2021 bc not specified)

# Plot
ggboxplot(data_4boxplot, #boxplot
          x = "group", # our group var (b/a COVID), this tells R to divide what's gonna plot according to the levels of this var
          y = "wdi_wip", # a boxplot for each group in the Y axis
          color = "group", #let's colored them differently
          palette = c("violetred1", "violetred4"),#some colors for each boxplot
          order = c("Before, 2019", "After, 2021"),#the order in which we want to see them
          ylab = "% Women in Parliament", #label y-axis
          xlab = "Groups",#label x-axis
          main = "Difference in Women's Representation \nBefore and After the COVID-19 Pandemic Outbreak (2020)") + #title
  theme(legend.position="none") + #whether we want the legend explaining what the colors are, you can change to "bottom", "left", etc.
  annotate("text", #annotate() to add text to the plot
           x=1.5, y=50, #the coordinates of where we want the text to appear
           label= "Mean difference = - 1.6***\nPaired t-test p =9.598e-05") #the text

# 2. Multivariate graphs ####
#https://rkabacoff.github.io/datavis/Bivariate.html

## i. Scatter plots ####

# Bivariate: women's representation by econ gender gap
ggplot(data_cs, #the data we'll use, ggplot() to plot things using the ggplot package 
       aes(x = gggi_pos, #the var we want in the x axis
           y = wdi_wip)) + #the var for y axis
  geom_point() + #the type of plot we want, geom_point() is a scatter plot
  labs(title = "Women’s Representation by Economic Gender Gap (Qog Last Year)", #title
       x = "Global Gender Gap Economic Participation and Opportunity Subindex",#labs
       y = "% Women in Parliament") +
  theme_classic() #if we want to change the plot theme for != aesthetics

# Multivariate (3 variables): women's rep, gender econ gap, regime type
# Adding 3rd variable (reference group: democracies and autocracies)
data_cs$br_dem_label <- case_when(data_cs$br_dem == 1 ~ "Yes", #new var = Yes when br_dem = 1 (democracy)
                                  data_cs$br_dem == 0 ~ "No") #and = No when br_dem = 0 (i.e., autocracies)

ggplot(data_cs, #all same as before EXCEPT adding new var as color
       aes(x = gggi_pos, 
           y = wdi_wip,
           color = br_dem_label)) + # coloring observations according to this grouping var
  geom_point() + 
  labs(title = "Women’s Representation by Economic Gender Gap and Regime Type (Qog Last Year)",
       x = "Global Gender Gap Economic Participation and Opportunity Subindex",
       y = "% Women in Parliament",
       color = "Electoral Democracy?\n(Cheibub et al. 2010)") + # adding a title to the color legend
  theme_classic()

## ii. Line plot ####

# Bivariate: women's representation by year
data_ts %>% # going through our time series data
  group_by(year) %>% #grouping observations by year (all countries within a year)
  summarise(Mean = mean(wdi_wip, na.rm = T)) %>% #summarizing new Mean variable that's the mean of women's rep (wdi_wip) by year
  ggplot(aes(x = year, #now ggplo() to plot, don't need to specify data bc we're piping through it
             y = Mean)) + # just say variables for x and y axis 
  geom_line() + # plot we want, geom_line() for line plots
  labs(title = "Average Women’s Representation Worldwide by Year",
       x = "Year",
       y = "% Women in Parliament") + 
  theme_classic()

# Bivariate using facet_wrap() to make individual plots by group (country)
data_ts %>% #go through time series data 
  filter(ht_region == 4 & br_dem == 1) %>% #filter only Sub-Saharan African democracies
  ggplot(aes(x = year, #years in x-axis
             y = wdi_wip)) + #women's representation y-axis
  geom_line() + #produce a line plot
  facet_wrap(~cname) + #for each country
  labs(title = "Average Women’s Representation Worldwide per Year in Sub-Saharan African Electoral Democracies",
       x = "Year",
       y = "% Women in Parliament") +
  theme_classic()

# Multivariate (3 variables): women's rep, year, regime type
# Adding 3rd variable (reference group: democracies and autocracies)
data_ts$br_dem_label <- case_when(data_ts$br_dem == 1 ~ "Yes", 
                                  data_ts$br_dem == 0 ~ "No")

data_ts %>% # go to my data
  group_by(year, br_dem_label) %>% #group by year and regime type
  summarise(Mean = mean(wdi_wip, na.rm = T)) %>% #mean of women's rep at these 2 levels (by year + by regime type)
  ggplot(aes(x = year, # year x-axis
             y = Mean, # mean of women's rep per year
             color = br_dem_label)) + #in democracies and autocracies
  geom_line() + # line plot with line per group in grouping var (color = br_dem_label) 
  labs(title = "Average Women’s Representation Worldwide by Year\nDemocracies vs. Autocracies",
       x = "Year",
       y = "% Women in Parliament",
       color = "Electoral Democracy?\n(Cheibub et al. 2010)") + #title/lab for color legend 
  theme_classic()

# To see what you're doing above with group_by before plotting:
test <- data_ts %>% # go to my data
  group_by(year, br_dem_label) %>% #group by year and regime type
  summarise(Mean = mean(wdi_wip, na.rm = T)) ##mean of women's rep at these 2 levels (by year + by regime type)

## iii. Grouped kernel density plots ####
# Similar to histogram but smoothed version of it (kernel density used to estimate the probability density function of the var) 
# Useful to observe the distribution of your variable and its peaks
ggplot(data_cs, 
       aes(x = gggi_pos, 
           fill = br_dem_label)) + #now using fill instead of color, so that all density is colored (not just line)
  geom_density(alpha = 0.4) +# geom_density for density plots, the alpha defines the transparency of the color
  labs(title = "Economic Gender Gap by Regime Type",
       x = "Global Gender Gap Economic Participation",
       y = "Density",
       fill = "Electoral Democracy?\n(Cheibub et al. 2010)") + #fill also here bc we used fill not color
  theme_classic()

## iv. Box and violin plots by group ####

# More ways of doing several boxplots in same plot (before we used ggboxplot)

# Standard boxplots
ggplot(filter(data_cs, is.na(br_dem_label) == FALSE), #cases in our data w/complete democracy observations 
       aes(x = br_dem_label, #grouping vars whose categories we'll want a plot of
           y = gggi_pos)) + #the variable we want box plot the distribution of
  geom_boxplot() + #geom_boxplot() for boxplots using ggplot
  labs(title = "Economic Gender Gap by Regime Type",
       y = "Distribution of Global Gender Gap Economic Participation Variable",
       x = "Electoral Democracy?\n(Cheibub et al. 2010)") +
  theme_classic()

# Notched boxplots 
  # Approximate method for visualizing whether groups differ in terms of their medians
  # Not formal test,  but if the notches (inflection part around median which indicates confidence interval)
  # do not overlap, there is evidence at 95% confidence that the medians of the two groups are !=
  # Figure description: https://www.google.com/search?q=notched+boxplots&rlz=1C1GCEV_enUS862US862&source=lnms&tbm=isch&sa=X&ved=2ahUKEwivzJ7-nsv9AhVRm2oFHR67BqcQ_AUoAXoECAEQAw&biw=966&bih=994&dpr=1#imgrc=qbQuMyO_pdIA6M
ggplot(filter(data_cs, is.na(br_dem_label) == FALSE), 
       aes(x = br_dem_label, 
           y = gggi_pos)) +
  geom_boxplot(notch = TRUE, #all similar as before but you say you want notches
               fill = "purple", #if we want to specify a color to fill the boxplot
               alpha = .6) + #if we want to change transparency of the color
  labs(title = "Economic Gender Gap by Regime Type",
       y = "Global Gender Gap Economic Participation Variable",
       x = "Electoral Democracy?\n(Cheibub et al. 2010)") +
  theme_classic()

# Violin plots
  # Hybrid of box plot and kernel density plot
  # Box plot only show summary statistics
  # Violin plots both summary statistics and the density of each variable
  # Some more explanation here (tho python code): https://mode.com/blog/violin-plot-examples/#:~:text=A%20violin%20plot%20is%20a,the%20density%20of%20each%20variable.

ggplot(filter(data_cs, is.na(br_dem_label) == FALSE), 
       aes(x = br_dem_label, 
           y = gggi_pos,
           fill = br_dem_label)) +
  geom_violin(trim=F)+ #geom_violin() for violin plots using ggplot. trim -> if TRUE (default), trim the tails of the violins to the range of the data. If FALSE , don't trim the tails.
  geom_boxplot(width=0.1, fill="white")+ #add a box plot within it, width defines size, fill color
  labs(title = "Economic Gender Gap by Regime Type",
       y = "Global Gender Gap Economic Participation Variable",
       x = "Electoral Democracy?\n(Cheibub et al. 2010)") +
  theme_classic() +
  theme(legend.position="none")

## v. Mean/SEM plots ####
# Mean plot with error bars
# Error bars can represent standard deviations, standard error of the mean, or confidence intervals
# We'll do means and standard errors

# Multivariate plot (3 vars): region, econ gender gap, regime type

# Create labeled grouping variable to plot means by region of origin
data_cs$ht_region_labels <- recode(data_cs$ht_region,
                                   "1" = "Eastern Europe",
                                   "2" = "Latin America",
                                   "3" = "MENA",
                                   "4" = "Sub-Saharan Africa",
                                   "5" = "Western Europe + North America",
                                   "6" = "East Asia",
                                   "7" = "South-East Asia",
                                   "8" = "South Asia",
                                   "9" = "The Pacific",
                                   "10" = "The Caribbean")

# Calculate means and standard errors of econ gender gap by region and democracy
data_4semplot <- data_cs %>% # go through TS data
  group_by(br_dem_label, ht_region_labels) %>% #group by regime type and region of origin
  summarize(#summarize to create the following new vars according to those previous grouping vars
    n = n(), #count how many observations by region, n() gives the current group size
    mean = mean(gggi_pos, na.rm = T), # mean of econ gender gap by group
    sd = sd(gggi_pos, na.rm = T), #standard deviation of econ gender gap by group
    se = sd/sqrt(n))#standard error as the standard deviation divided the square root of number of observations (its formula...)

# Check what this produces:
View(data_4semplot)

# Plot the means and standard errors by countries' region
ggplot(filter(data_4semplot, is.na(br_dem_label) == F), #cases in our data w/complete democracy observations 
       aes(x = reorder(ht_region_labels, mean), #region in x axis; reorder this variable by mean (ascending, -mean 4 descending); this is to plot in this order
           y = mean, #mean of econ gender gap
           group=br_dem_label, #now you also need this for geom_line()
           color=br_dem_label)) +# if just geom point and error bar, just color would be OK
  geom_point(size = 3) + #geom_point() for points at mean value per region, size defines size of points
  geom_line(linewidth = 1) + #geom_line for line across means, linewidth defines that.
  geom_errorbar( #geom_errorbar to draw error bars as specified below (requires its own aesthetic)
    aes(ymin = mean-se, #confidence intervals as mean -/+ standard error
        ymax = mean+se), 
    width = .2) + #size of error bars
  labs(title = "Economic Gender Gap by Regime Type",
       y = "Average Global Gender Gap Economic Participation",
       x = "Region",
       color = "Electoral Democracy?\n(Cheibub et al. 2010)") +
  theme_classic() +
  theme(axis.text.x=element_text(angle=45,hjust=1)) #to rotate x-axis labels by 45`°
