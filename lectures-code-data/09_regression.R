# 0. Set-up ####

## Packages ####
library("tidyverse") # if issues: https://stackoverflow.com/questions/55415631/error-package-or-namespace-load-failed-for-tidyverse-in-loadnamespace
                     # remove.packages("rlang")
                     # install.packages("rlang") 
                     # library("tidyverse")
library("stargazer") # stargazer() to visualize mode outputs
library("ggiraphExtra") # ggpredict() for interactive regression plots
library("car") # avPlots() for multiple linear regression plots
library("sjPlot") #plot_model() for interactions

# Working directory to get/save files
setwd(file.path(Sys.getenv("USERPROFILE"),"Box/0 Teaching/Intro to R/2023_Rice_POLI102/materials/"))
#macOS users: setwd("/Users/yourusername/yourfoldername/anotherfolderifnested")

## Data ####

# VERY NICE AND THOROUGH EXPLANATION OF LINEAR REGRESSION ANALYSIS (W/R CODE):
# https://www.machinelearningplus.com/machine-learning/complete-introduction-linear-regression-r/
# FOLLOW-UP ON LOGISTIC REGRESSION (W/R CODE):
# https://www.machinelearningplus.com/machine-learning/logistic-regression-tutorial-examples-r/
# Though more on model fit and diagnosis for logit here:
# https://www.statology.org/logistic-regression-in-r/
  
# This lab: Economic determinants of democracy and regime type
## Dem variable (DV)
# Electoral democracy -- br_dem 
# Liberal democracy -- vdem_libdem

## Econ factors (IV)
# GDP per capita (wdi_gdpcapcon2015)
# GDP per capita growth (annual %) -- wdi_gdpcapgr
# Inflation, consumer prices (annual %) -- wdi_inflation
# Economy Status/Development -- bti_mes
# Gini index of inequality -- wdi_gini

## Regime Type (IV)
# Regime Type -- gol_inst
# Presidential political system -- br_pres

# Cross-sectional QoG data
data <- read.csv("qog_std_cs_jan23.csv") 
data <- select(data,
               cname,
               br_dem,
               vdem_libdem,
               wdi_gdpcapcon2015,
               wdi_gdpcapgr,
               wdi_inflation,
               bti_mes,
               wdi_gini,
               gol_inst,
               br_pres)

## Intro: Last class correlation ####

cor(data$vdem_libdem, data$bti_mes, 
    use = "complete.obs")

# Scatter plot
plot(data$bti_mes, data$vdem_libdem, 
     pch = 19,
     col = "red",
     main = "coerr.coeff r = 0.68") # Correlation
# Regression line
abline(lm(data$vdem_libdem ~ data$bti_mes), 
       lty=2, lwd=3,
       col = "blue")

# Linear Regression ####
# DV: Continuous measure of level of democracy (civil liberties, V-Dem liberal democracy)

## (A) Bivariate ####

# lm() function for linear regression models
# Does the level of democracy increase as there is greater economic growth?
lm(vdem_libdem ~ wdi_gdpcapgr, # regression formula: y (DV) ~ x (IV) 
   data=data) # dataset

# Assign to object name to do things with it
linear_biv <- lm(vdem_libdem ~ wdi_gdpcapgr,
                 data = data)

# E.g., having a better sense of model outputs
summary(linear_biv)

stargazer(linear_biv, type='text') 

# You can do transformations of your IV inside the model as well
hist(data$wdi_gdpcapcon2015) # remember GDP is highly skewed?
hist(log(data$wdi_gdpcapcon2015)) # we thus usually use its logarithmic transformation

summary(lm(vdem_libdem ~ wdi_gdpcapcon2015, # democracy regressed on GDP per capita (not growth)
   data=data)) 

summary(lm(vdem_libdem ~ log(wdi_gdpcapcon2015), # democracy regressed on GDP per capita (not growth)
           data=data))

### Visualize ####

# W/ggplot

ggplot(data, # data set
       aes(y = vdem_libdem, x = wdi_gdpcapgr)) + # y and x variables
  geom_point() + # bivariate scatter plot
  geom_smooth(method="lm") + # geom_smooth to produce the regression line btn x & y, runs model in the background
theme_classic()

# W/ggpredict 
ggPredict(linear_biv, #you'll need to bring the model object in this case
          se=TRUE, #to add confidence interval
          interactive=TRUE) #interactive plot, you can check points and regression equation

### Polynomial ####

# We may think the relationship btn gdp growth and democracy is instead
# Growth leads to democracy up to a point, excessive growth less democracy
linear_poly <- lm(vdem_libdem ~ wdi_gdpcapgr + I(wdi_gdpcapgr^2), 
                  data = data)

summary(linear_poly)

# Alternative coding, same thing
summary(lm(vdem_libdem ~ poly(wdi_gdpcapgr, 2, raw = TRUE),
           data = data))

# Visualize 
ggplot(data, # data set
       aes(y = vdem_libdem, x = wdi_gdpcapgr)) + # y and x variables
  geom_point() + # bivariate scatter plot
  geom_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE)) + # geom_smooth to produce the regression line btn x & y, runs model in the background
  theme_classic()

## (B) Multivariate ####

# We can also regress Y on multiple independent variables (X, Z, W) that we think
# jointly affect our outcome variable of interest

# Just add them (+) as covariates in your model! (just as in the function)

# Economic determinants of democracy: growth, development, inflation, and inequality

linear_multiv <- lm(vdem_libdem ~ wdi_gdpcapgr + bti_mes + wdi_inflation + wdi_gini,
                    data=data) 

summary(linear_multiv)

### Visualize ####
avPlots(linear_multiv)

# X-axis displays each independent var and y-axis the outcome
# Blue line shows the association (regression) line between each predictor and the OV
  ## while holding other IV constant
  ## so partial regression coefficients and regression line represent the change in the OV
  ## when the predictor is increase by 1 while all other predictors remain constant
# Labelled points represent:
  ## 2 observations with larger residuals (diff btn observed and predicted value - so deviance)
  ## 2 obs. with largest partial leverage (extreme x values, measure of influence)
    ## different from outliers! https://online.stat.psu.edu/stat462/node/170/#:~:text=A%20data%20point%20has%20high,is%20particularly%20high%20or%20low.

### Interactions ####

table(data$gol_inst) 
#0. Parliamentary democracy
#1. Semi-presidential democracy
#2. Presidential democracy

# How the interaction model/function should look like 
linear_int_3cat <- lm(vdem_libdem ~ bti_mes + as.factor(gol_inst) +
                        bti_mes:as.factor(gol_inst),
                      data=data)
summary(linear_int_3cat)

# Same thing, R will do it for you
linear_int2_3cat <- lm(vdem_libdem ~ bti_mes*as.factor(gol_inst),
                       data=data)
summary(linear_int2_3cat)

table(data$br_pres) 
#1. Presidential Political System
#0. Otherwise

linear_int_2cat <- lm(vdem_libdem ~ bti_mes*as.factor(br_pres),
                       data=data)
summary(linear_int_2cat)

#### Visualize ####

plot_model(linear_int_3cat, type = "pred", terms = c("bti_mes", "gol_inst")) +
  theme_classic()

plot_model(linear_int_2cat, type = "pred", terms = c("bti_mes", "br_pres")) +
  theme_classic()

# So many more things you can do: https://cran.r-project.org/web/packages/interactions/vignettes/interactions.html

# Model testing and further regression models:
# https://andrewproctor.github.io/rcourse/module5.html

# Logistic Regression ####
# DV: Binary indicator of democracy vs autocracy (Cheibub's electoral democracy)

## (A) Binary Logit ####
# Bivariate logistic regression refers to something else (two related dependent variables regressed on same covariates)
# https://sites.google.com/site/econometricsacademy/econometrics-models/bivariate-probit-and-logit-models
# Nice intro: https://stats.oarc.ucla.edu/r/dae/logit-regression/

# glm() function for logistic regression models
# Are democratic regimes more likely when there is higher economic growth?
glm(br_dem ~ wdi_gdpcapgr,# regression formula: y (DV) ~ x (IV) 
    family = "binomial", #family = binomial for binary DV
   data=data) # data set

# Assign to object name to do things with it
logit_biv <- glm(br_dem ~ wdi_gdpcapgr,
                  family = "binomial",
                  data=data)

# E.g., having a better sense of model outputs
summary(logit_biv)

## (B) Multivariate ####

logit_multiv <- glm(br_dem ~ wdi_gdpcapgr + bti_mes + wdi_inflation + wdi_gini,
                    family = "binomial",
                    data=data) 
summary(logit_multiv) 

# (1) As before in output we first see the model we run
# (2) Then deviance residuals which can be used to assess model fit 
# (remember, residuals are the difference between the observed data and the model's predictions)
# Those are the ones corresponding to specific cases used in the model (min, first quantile, so on) 
# Residulas and AIC at the end will also be other measures used to assess model fit

# (3) Next, in main table, we see coefficients for each predictor, standard errors, z-stat and p-values (with significance indication)
# The way to interpret this is as follows (considering the multivariate logit and only significant association):
  ## For every unit change in econ development (bti_mes), the log odds of being an electoral democracy (versus an autocracy)
  ## increase by 0.72


# If you want to get confidence intervals for each predictor's coefficient
confint(logit_multiv) # using profiled log-likelihood
confint.default(logit_multiv) #using standard errors

# To get the odds ratio associated with each predictor and its confidence interval
exp(cbind(OR = coef(logit_multiv), confint(logit_multiv)))
## As with Odds ratios seen in previous labs, these are the odd of success (democracy in our case), 
## compared to failure (autocracy) as one unit increase in a given predictor
## The odds ratio (or odds of success) are calculated as the ratio btn probability of success over that of failure (e.g., p(dem)/p(aut))
## In the logit_multiv case, for a one unit increase in econ development (bti_mes) 
## the odds of democracy increase by a factor of 2 (odds of success (democracy) 2 to 1 (autocracy))

## The coefficient estimates you see in the model output and as a result of summary()
## are the log() of that, check it out:
log(exp(cbind(OR = coef(logit_multiv), confint(logit_multiv))))

logit_multiv #same coefficients

# Put together some slides
# Recorded session explaining cont. logit lab code
# WE'R DONE!

#https://bio304-class.github.io/bio304-fall2017/logistic-regression.html#:~:text=To%20visualize%20the%20logistic%20regression,as%20a%20function%20of%20age.&text=Having%20generated%20the%20predicted%20probabilities,our%20previous%20plot%20using%20geom_line%20.
#https://www.ericrscott.com/post/plot-logistic-regressions/
#http://www.cookbook-r.com/Statistical_analysis/Logistic_regression/
#https://stats.oarc.ucla.edu/r/dae/logit-regression/
