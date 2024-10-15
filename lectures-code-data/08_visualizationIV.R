# 0. Set-up ####

## Packages ####
library("tidyverse")
library("effsize") #cohen's d
library("effectsize") #p_superiority, cohens_u1/2/3, p_overlap (not used here, but in case you want it.)
library("finalfit") #or_plot() function for odds ration plot
library("dvmisc") #quant_groups() function to divide continuous var into quantile groups
library("GGally") # ggpairs() for correlogram
library("ggstatsplot") #ggbetweenstats() plotting anova (and ttest) analysis
library("PerformanceAnalytics") # chart.Correlation() for first correlation plot
library("corrplot") # last correlogram

# Working directory to get/save files
setwd(file.path(Sys.getenv("USERPROFILE"),"Box/0 Teaching/Intro to R/2023_Rice_POLI102/materials/"))
#macOS users: setwd("/Users/yourusername/yourfoldername/anotherfolderifnested")

## Data ####

# This lab: Determinants of democracy (electoral vs. civil liberties)

## Dem variable
  # Electoral democracy (br_dem) 
  # Liberal democracy (vdem_libdem)

## Econ factors
  # per capita GDP (wdi_gdpcapcon2015)
  # econ development (bti_mes)
  # inequality (wdi_gini)
## Living conditions
  # education/human capital (egov_hci)
  # gap btn male and female educative attainment (gggi_eas)
  # urbanization, % of tot po (wdi_popurb)
  # human development (undp_hdi)
## Cultural factors
  # colonial heritage/origin (ht_colonial)
  # ethnic fractionalization (fe_etfra)

##(given data, we're missing religion and resource curse explanations)

# Cross-sectional QoG data
data <- read.csv("qog_std_cs_jan23.csv") 
data <- select(data,
               cname,
               br_dem,
               vdem_libdem,
               wdi_gdpcapcon2015,
               bti_mes,
               wdi_gini,
               egov_hci,
               gggi_eas,
               wdi_popurb,
               undp_hdi,
               ht_colonial,
               fe_etfra)

# Ordinal vars we'll use for some tests (ANOVA; Odds ratio)
data <- data %>% mutate(
  gdp = quant_groups(wdi_gdpcapcon2015, 3),
  econdev = quant_groups(bti_mes, 3),
  ineq = quant_groups(wdi_gini, 3),
  humcap = quant_groups(egov_hci, 3),
  gendergap = quant_groups(gggi_eas, 3),
  urban = quant_groups(wdi_popurb, 3),
  humdev = quant_groups(undp_hdi, 3),
  fraction = quant_groups(fe_etfra, 3)
)

# Cohen's D ####

## Analysis ####

# DEMOCRACY --> ECON DEVELOPMENT?

data$br_dem <- as.factor(data$br_dem)
cohen.d(data$bti_mes, data$br_dem,# electoral democracy effect on econ development
        na.rm=TRUE) # negative d ~ second mean is larger, in this case democ. T

## Visualization ####

# Checking standardized mean difference with density plots

democracies <- data %>% filter(br_dem == 1)
autocracies <- data %>% filter(br_dem == 0)

ggplot() +
  # Plotting normalized distribution of econ dev variable in democracies
  stat_function(data = democracies,
                aes(x = bti_mes, color = "Democracies"),
                linewidth = 1,
                fun = dnorm,
                args = with(democracies, c(mean = mean(bti_mes, na.rm=T), 
                                           sd = sd(bti_mes, na.rm=T)))) +
  # Plotting normalized distribution of econ dev variable in autocracies
  stat_function(data = autocracies,
                aes(x = bti_mes, color = "Autocracies"),
                linewidth = 1,
                fun = dnorm,
                args = with(autocracies, c(mean = mean(bti_mes, na.rm=T), 
                                           sd = sd(bti_mes, na.rm=T)))) +
  # Lines for group means
  geom_vline(xintercept = mean(democracies$bti_mes, na.rm = T), 
             linetype="dotted", linewidth = 1, color = "red") + 
  geom_vline(xintercept = mean(autocracies$bti_mes, na.rm = T), 
             linetype="dotted", linewidth = 1, color = "black") + 
  # Labs
  labs(title = "Democracy Effect Size on Economic Development (Cohen's d = -1.148713 -- 'large effect')",
       y = "Density",
       x = "Economic Development") +
  # Labeling each group line
  scale_color_manual("Group", 
                     values= c("Autocracies" = "black","Democracies" = "red")) +
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5))

# ANOVA ####
# More expl. here: https://www.youtube.com/watch?v=UpsPDRXTJWc
# His code: https://github.com/AriaAvi/Multivariate-Data-Analysis/blob/main/R%20code%20Video%208%20-%20Factorial%20ANOVA.R

## Analysis ####

# ECON DEVELOPMENT --> DEMOCRACY?
anova_1w <- aov(vdem_libdem ~ gdp, 
                data = data)
summary(anova_1w)

anova_2w <- aov(vdem_libdem ~ gdp + econdev, 
                data = data)
summary(anova_2w)

## Assumptions check ####

# ANOVA's assumptions
# Dependent/outcome variable must be metric
#(1) Normality (outcome distribution follows/approximates a normal distribution)
#(2) Homoscedasticity (equal variances - error term remains the same across sample/levels of explanatory factors)

par(mfrow=c(2,2)) # we do this to first set the 2x2 plot grid

plot(anova_1w) # bc this produces 4 plots; so that we can see them all at once

plot(anova_2w) 

par(mfrow=c(1,1)) #to go back to normal

# From these plots we can evaluate normality and equal variance
# Each diagnosis plot plots the residuals across levels of the data
# giving specific info about model fit, for our purposes it's sufficient to know
# that mean residuals (red line) should be horizontal to evaluate equal variance, 
# and qq plot (as the one we did before) fitting the line of what would be 
# perfect fit with theoretical quantiles if data coming from normal distribution
# you can also observe whether there are outliers affecting distribution

#(3) Independence (observations independent from each other)
  ## No formal way to do it, you mostly need a strong theoretical reason
  ## if not certain that results come frome a randomized design

#(4) No multicollineratiy (factorial anova, factors cannot be highly correlated)
# Correlations can be a suggestive way of assesing multicolinearity
# Let's do it on original variables, to avoid recoding and type changing of ordinal ones
cor(data$wdi_gdpcapcon2015, data$bti_mes, # no perfect correlation, but positively correlated
    use="complete.obs")

multi_check <- select(data, wdi_gdpcapcon2015, bti_mes) 
GGally::ggpairs(multi_check) # correlograms below too...
                            # log GDP prob best

dev.off() # if ggpairs code above doesn't work try running this first

## Visualization ####
# More on ggbetweenstats function: https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/ggbetweenstats.html

ggbetweenstats(
  data = data,
  x = gdp,
  y = vdem_libdem,
  type = "parametric", # ANOVA or Kruskal-Wallis
  var.equal = TRUE, # ANOVA or Welch ANOVA
  plot.type = "box",
  pairwise.comparisons = TRUE,#it'll likely ask you to install a package
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE) +
  labs(y = "Liberal democracy (V-Dem)",
       x = "GDP per capita quartile groups",
       title = "Democracy and Growth, Anova Post-hoc Analysis")


# Want more ANOVA visualizations? 
## http://biometry.github.io/APES/Stats/stats13-anova.html
## https://www.scribbr.com/statistics/anova-in-r/
## (1-w) http://www.sthda.com/english/wiki/one-way-anova-test-in-r
## (2-w) http://www.sthda.com/english/wiki/two-way-anova-test-in-r
## https://statsandr.com/blog/anova-in-r/

# Odds ratio visualization ####
#https://argoshare.is.ed.ac.uk/healthyr_book/odds-ratio-plot-1.html
# a note on visualization https://stephanieevergreen.com/odds-ratios/

explanatory <- c( "gdp", "econdev", "ineq", 
                  "humcap", "gendergap", "urban",
                  "humdev", "fraction")
dependent <- "br_dem"

data %>% 
  or_plot(dependent, explanatory,
          breaks = c(0.001, 1, 50, 5000),
          table_text_size = 3.5)

cor(data$br_dem, data$fe_etfra, 
    use="complete.obs")

# Correlograms ####
# More on corrplot package:
##https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
##http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram

# With chart.Correlation
my_data <- data[,4:12] #vars we want to analyze corrs (all the original ones)
                       #numbers indicate column positions
chart.Correlation(my_data, histogram=TRUE, pch=19)

# With corrplot
M <-cor(my_data, #your correlation matrix
        use = "complete.obs") 
corrplot(M, type="upper", order="hclust") #check links above to change aesthetics
                                          #plenty of options






