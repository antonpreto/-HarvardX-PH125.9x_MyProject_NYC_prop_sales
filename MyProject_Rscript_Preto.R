# Course: Data Science: HarvardX PH125.9x Capstone
# Name: Anton Preto
# Project: My own project = 
# Date: May 2023

# Project description: 
# For this project, you will be applying machine learning techniques that go beyond standard linear regression. 
# You will have the opportunity to use a publicly available dataset to solve the problem of your choice.

# Project Instructions:
# You will submit a report that documents your analysis and presents your findings, with supporting statistics and figures. 
# The report must be written in English and uploaded as both a PDF document and an Rmd file.
# The report should include the following at a minimum: 
  # an introduction/overview/executive summary section,
  # a methods/analysis section,
  # a results section,
  # a conclusion section,
  # a references section.

# Libraries and setup:
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(ggcorrplot)) install.packages("ggcorrplot", repos = "http://cran.us.r-project.org")
if(!require(fastDummies)) install.packages("fastDummies", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(DAAG)) install.packages("DAAG", repos = "http://cran.us.r-project.org")
if(!require(VIM)) install.packages("VIM", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(ROCR)) install.packages("ROCR", repos = "http://cran.us.r-project.org")
if(!require(pROC)) install.packages("pROC", repos = "http://cran.us.r-project.org")
if(!require(ranger)) install.packages("ranger", repos = "http://cran.us.r-project.org")
if(!require(broom)) install.packages("broom", repos = "http://cran.us.r-project.org")
if(!require(xgboost)) install.packages("xgboost", repos = "http://cran.us.r-project.org")


options(timeout = 120)
options(digits=7)

# Data: using kaggle dataset "NYC Property Sales"

# About Dataset: 
## This dataset is a record of every building or building unit (apartment, etc.) sold in the New York City property market over a 12-month period.
## This dataset contains the location, address, type, sale price, and sale date of building units sold. 

# A reference on the trickier fields:
  ## BOROUGH: A digit code for the borough the property is located in; in order these are 
    ### Manhattan (1), Bronx (2), Brooklyn (3), Queens (4), and Staten Island (5).
  ## BLOCK; LOT: The combination of borough, block, and lot forms a unique key for property in New York City. Commonly called a BBL.
  ## BUILDING CLASS AT PRESENT and BUILDING CLASS AT TIME OF SALE: The type of building at various points in time. See the glossary linked to below.
# Date of sales: September 2016 - August 2017

# kaggle dataset "NYC Property Sales"
# https://www.kaggle.com/datasets/new-york-city/nyc-property-sales/download?datasetVersionNumber=1
# https://github.com/antonpreto/HarvardX-PH125.9x_MovieLens_Project

# My Github repo used for loading data from csv file
NYC_prop_sales<-read.csv("https://raw.githubusercontent.com/antonpreto/-HarvardX-PH125.9x_MyProject_NYC_prop_sales/main/nyc-rolling-sales.csv")

# Basic dataset
as.data.frame(NYC_prop_sales)
head(NYC_prop_sales) # shows Preview for Dataset
names(NYC_prop_sales)
summary(NYC_prop_sales) # shows summary Statistics for Dataset
str(NYC_prop_sales) # shows Internal Structure for Dataset
length(NYC_prop_sales$SALE.PRICE)
nrow(NYC_prop_sales)
dim(NYC_prop_sales)

# Copy of default dataset that is going to be used for editing
sales <- NYC_prop_sales

str(sales) # shows Internal Structure for Dataset
nrow(sales)
sales$LAND.SQUARE.FEET
sales$GROSS.SQUARE.FEET 
sales$SALE.PRICE
sales$SALE.DATE

###############################################################################
# Data cleaning + Data editing
###############################################################################

# First, we need to inspect, clean, edit and prepare data for further analysis

# We need to edit type of some variables
suppressWarnings(sales <- sales %>%
  mutate(LAND.SQUARE.FEET = as.integer(LAND.SQUARE.FEET),
         GROSS.SQUARE.FEET = as.integer(GROSS.SQUARE.FEET),
         SALE.PRICE = as.integer(SALE.PRICE),
         SALE.DATE = as.Date(SALE.DATE)))
str(sales) # shows Internal Structure for Dataset (corrected, NAs introduced)

# Making duplicate of BOROUGH data (=BOROUGH2)
sales <- sales %>%
  mutate(BOROUGH2 = BOROUGH)

# Editing and adding borough name instead of numeric value
sales$BOROUGH2[sales$BOROUGH2 == '1'] <- 'Manhattan'
sales$BOROUGH2[sales$BOROUGH2 == '2'] <- 'Bronx'
sales$BOROUGH2[sales$BOROUGH2 == '3'] <- 'Brooklyn'
sales$BOROUGH2[sales$BOROUGH2 == '4'] <- 'Queens'
sales$BOROUGH2[sales$BOROUGH2 == '5'] <- 'Staten Island'

# Adding SALE.PRICE column in millions
sales <- sales %>%
  mutate(SALE.PRICE.M = SALE.PRICE/1000000)

# Calculating age of the property in year 2017
sales$YEAR.BUILT[sales$YEAR.BUILT == '0'] <- 'NA'           # if 0 = NA

# Adding current year value column
sales <- sales %>%
  mutate(Curr_Year = 2017)                

suppressWarnings(sales <- sales %>%
                   mutate(YEAR.BUILT = as.integer(YEAR.BUILT),
                          Curr_Year = as.integer(Curr_Year)))

# calculating age of the property
suppressWarnings(sales <- sales %>%         
  mutate(Prop_Age = Curr_Year - YEAR.BUILT))

sales$Prop_Age
str(sales)

# Subtracting year and month from sales date variable (SALE.DATE)
sales %>%
  mutate(SALE.DATE_year = lubridate::year(SALE.DATE), 
                SALE.DATE_month = lubridate::month(SALE.DATE))
sales <- sales %>%
  mutate(SALE.DATE_year = year(SALE.DATE), 
                SALE.DATE_month = month(SALE.DATE))

suppressWarnings(sales <- sales %>%
                   mutate(SALE.DATE_year = as.integer(SALE.DATE_year),
                          SALE.DATE_month = as.integer(SALE.DATE_month)))

str(sales)
names(sales)
summary(sales) # summary table for current edited dataset

###############################################################################
# Problem: NA values in SALE.PRICE
###############################################################################

# Number of NAs: SALE.PRICE
sum(is.na(sales$SALE.PRICE))
sum(is.na(sales))

# Total observations: SALE.PRICE
length(sales$SALE.PRICE)

# NAs share of total observations
sum(is.na(sales$SALE.PRICE)) / length(sales$SALE.PRICE)
sum(is.na(sales$LAND.SQUARE.FEET)) / length(sales$LAND.SQUARE.FEET)
sum(is.na(sales$GROSS.SQUARE.FEET)) / length(sales$GROSS.SQUARE.FEET)

# Solution: Removing rows containing NA in SALE.PRICE column
sales_removed <- sales %>% drop_na(SALE.PRICE)
# Check
sum(is.na(sales_removed$SALE.PRICE))
sum(is.na(sales_removed$SALE.PRICE)) / length(sales$SALE.PRICE)
sum(is.na(sales$SALE.PRICE)) / length(sales$SALE.PRICE)

###############################################################################
# Problem: outliers and 0 values
###############################################################################

# Cumulative Distribution: Properties according to Sale Price
plot1 <- sales_removed %>%
  ggplot() + 
  aes(SALE.PRICE.M) + 
  stat_ecdf(geom = "step", color="#0B625B") + 
  scale_x_continuous(breaks = c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100)) + 
  labs(x = "Sale Price in Millions", y = "Cumulative Sum of Sale Price in Millions")

plot2 <- sales_removed %>%
  ggplot() + 
  aes(SALE.PRICE.M) + 
  stat_ecdf(geom = "step", color="#0B625B") + 
  xlim(c(0, 10)) + 
  labs(x = "SALE.PRICE.M", y = "Cumulative Sum of Sale Price in Millions")

grid.arrange(plot1, plot2, ncol=2)

# Solution and Result: we can drop values of sale price below 100k and over 10M (removing outliers and non-logical values)

# only keep values of sale price that is equal to 100 000 and higher
sales_removed <- subset(sales_removed, SALE.PRICE>=100000)

# only keep values of sale price below 10 millions or equal to 10 million
sales_removed <- subset(sales_removed, SALE.PRICE<=10000000)

# check
max(sales_removed$SALE.PRICE)
min(sales_removed$SALE.PRICE)

# Solution and Result: we can drop values of total units that is 0 and over 45 (removing outliers and non-logical values)
mean(sales_removed$TOTAL.UNITS, na.rm=TRUE)
median(sales_removed$TOTAL.UNITS, na.rm=TRUE)
min(sales_removed$TOTAL.UNITS, na.rm=TRUE)
max(sales_removed$TOTAL.UNITS, na.rm=TRUE)

# Cumulative Distribution: TOTAL.UNITS
plot3 <- sales_removed %>%
  ggplot() + 
  aes(TOTAL.UNITS) + 
  stat_ecdf(geom = "step", color="#0B625B") + 
  xlim(c(0, 2261)) + 
  labs(x = "TOTAL.UNITS", y = "Cumulative Sum of TOTAL.UNITS")

plot4 <- sales_removed %>%
  ggplot() + 
  aes(TOTAL.UNITS) + 
  stat_ecdf(geom = "step", color="#0B625B") + 
  xlim(c(0, 50)) + 
  labs(x = "TOTAL.UNITS", y = "Cumulative Sum of TOTAL.UNITS")

grid.arrange(plot3, plot4, ncol=2)

# only keep values of total units that is 1 and higher
sales_removed <- subset(sales_removed, TOTAL.UNITS>=1)

# only keep values of total units below 45
sales_removed <- subset(sales_removed, TOTAL.UNITS<=45)

# check
max(sales_removed$TOTAL.UNITS)
min(sales_removed$TOTAL.UNITS)

# Number of NAs: YEAR.BUILT
sum(is.na(sales_removed$YEAR.BUILT))

sales_removed <- sales_removed %>% drop_na(YEAR.BUILT)
sum(is.na(sales_removed$YEAR.BUILT))
sum(is.na(sales_removed$YEAR.BUILT)) / length(sales$YEAR.BUILT)
# default dataset
sum(is.na(sales$YEAR.BUILT)) / length(sales$YEAR.BUILT)
sum(is.na(sales$YEAR.BUILT))

# another option is to define outliers as a numbers out of boundaries "mean +- 2SD"
# "mean +- 2SD" = should contain around 95% of observations if dataset is large enough

################################################################################
# Characteristics of edited Dataset
################################################################################

nrow(sales_removed)

# Most sales by borough: table
sales_removed %>% group_by(BOROUGH2) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  print(n = 10)

# Most sales by borough: plot
plot5 <- sales_removed %>%
  group_by(BOROUGH2) %>%
  summarize(count = n()) %>%
  arrange(-count) %>%
  top_n(10, count) %>%
  ggplot(aes(count, reorder(BOROUGH2, count))) +
  geom_bar(color = "black", fill = "#0B625B", stat = "identity") +
  xlab("Number of Sales") +
  ylab(NULL)
plot5

# Most sales by Neighborhood: table
sales_removed %>% group_by(NEIGHBORHOOD) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  print(n = 10)


# Most sales by number of Residential Units: table
sales_removed %>% group_by(RESIDENTIAL.UNITS) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  print(n = 10)

sum(sales_removed$RESIDENTIAL.UNITS)


# Most sales by BUILDING.CLASS.AT.PRESENT: table
sales_removed %>% group_by(BUILDING.CLASS.AT.PRESENT) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  print(n = 10) 

# Distribution of LAND.SQUARE.FEET
sales_removed %>% ggplot(aes(LAND.SQUARE.FEET)) +
  geom_histogram(color = "black", fill = "#0B625B", bins = 150) +
  xlab("LAND.SQUARE.FEET") +
  ylab("Count") + 
  ggtitle("Histogram: LAND.SQUARE.FEET")

plot6 <- sales_removed %>% ggplot(aes(LAND.SQUARE.FEET)) +                                 
  geom_histogram(color = "black", fill = "#0B625B", bins = 30) +
  xlab("LAND.SQUARE.FEET") +
  ylab("Count") + 
  xlim(c(10, 10000)) +
  geom_vline(aes(xintercept = median(LAND.SQUARE.FEET, na.rm=TRUE)), color = "red", linewidth = 0.5) +
  geom_text(aes(x = median(LAND.SQUARE.FEET, na.rm=TRUE)), y = 5100, label = "Median", color = "red", angle=90, vjust = 1, size=4) + 
  ggtitle("Histogram: LAND.SQ.FT.")

# Distribution of GROSS.SQUARE.FEET
plot7 <- sales_removed %>% ggplot(aes(GROSS.SQUARE.FEET)) +                                
  geom_histogram(color = "black", fill = "#0B625B", bins = 30) +
  xlab("GROSS.SQUARE.FEET") +
  ylab("Count") + 
  xlim(c(10, 10000)) +
  geom_vline(aes(xintercept = median(GROSS.SQUARE.FEET, na.rm=TRUE)), color = "red", linewidth = 0.5) +
  geom_text(aes(x = median(GROSS.SQUARE.FEET, na.rm=TRUE)), y = 5300, label = "Median", color = "red", angle=90, vjust = 1, size=4) + 
  ggtitle("Histogram: GROSS.SQ.FT.")

grid.arrange(plot6, plot7, ncol=2)

mean(sales_removed$GROSS.SQUARE.FEET, na.rm=TRUE)
median(sales_removed$GROSS.SQUARE.FEET, na.rm=TRUE)
min(sales_removed$GROSS.SQUARE.FEET, na.rm=TRUE)
max(sales_removed$GROSS.SQUARE.FEET, na.rm=TRUE)

# Distribution of Year and Age
plot8 <- sales_removed %>% ggplot(aes(YEAR.BUILT)) +                                   # toto
  geom_histogram(color = "black", fill = "#0B625B", bins = 20) +
  xlab("YEAR.BUILT") +
  ylab("Count") + 
  xlim(c(1850, 2017)) +
  geom_vline(aes(xintercept = mean(YEAR.BUILT, na.rm=TRUE)), color = "red", linewidth = 0.5) +
  geom_text(aes(x = mean(YEAR.BUILT, na.rm=TRUE)), y = 4500, label = "Mean", color = "red", angle=90, vjust = 1, size=4) +
  ggtitle("Histogram: Year Built")

mean(sales_removed$YEAR.BUILT, na.rm=TRUE)
median(sales_removed$YEAR.BUILT, na.rm=TRUE)
min(sales_removed$YEAR.BUILT, na.rm=TRUE)
max(sales_removed$YEAR.BUILT, na.rm=TRUE)

plot9 <- sales_removed %>% ggplot(aes(Prop_Age)) +                                   # toto
  geom_histogram(color = "black", fill = "#0B625B", bins = 20) +
  xlab("Prop_Age") +
  ylab("Count") + 
  xlim(c(0, 250)) +
  geom_vline(aes(xintercept = mean(Prop_Age, na.rm=TRUE)), color = "red", linewidth = 0.5) +
  geom_text(aes(x = mean(Prop_Age, na.rm=TRUE)), y = 9800, label = "Mean", color = "red", angle=90, vjust = 1, size=4) +
  ggtitle("Histogram: Property Age")

mean(sales_removed$Prop_Age, na.rm=TRUE)
median(sales_removed$Prop_Age, na.rm=TRUE)
min(sales_removed$Prop_Age, na.rm=TRUE)
max(sales_removed$Prop_Age, na.rm=TRUE)

grid.arrange(plot8, plot9, ncol=2)

# Distribution of SALE.PRICE
sales_removed %>% ggplot(aes(SALE.PRICE.M)) +                                   # toto
  geom_histogram(color = "black", fill = "#0B625B", bins = 50) +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10)) +
  xlab("SALE.PRICE.M") +
  ylab("Count") + 
  geom_vline(aes(xintercept = mean(SALE.PRICE.M, na.rm=TRUE)), color = "red", linewidth = 1) +
  geom_text(aes(x = mean(SALE.PRICE.M, na.rm=TRUE)), y = 8000, label = "Mean", color = "red", angle=90, vjust = 1) + 
  geom_vline(aes(xintercept = median(SALE.PRICE.M, na.rm=TRUE)), color = "blue", linewidth = 1) +
  geom_text(aes(x = median(SALE.PRICE.M, na.rm=TRUE)), y = 8000, label = "Median", color = "blue", angle=90, vjust = 1) +
  geom_vline(aes(xintercept = mean(SALE.PRICE.M, na.rm=TRUE) + sd(SALE.PRICE.M, na.rm=TRUE)), color = "#000000", size = 1, linetype = "dashed") +
  geom_vline(aes(xintercept = mean(SALE.PRICE.M, na.rm=TRUE) - sd(SALE.PRICE.M, na.rm=TRUE)), color = "#000000", size = 1, linetype = "dashed") +
  ggtitle("Histogram: SALE.PRICE")

# SALE.PRICE boxplot
sales_removed %>% ggplot(aes(SALE.PRICE.M)) +                               
  geom_boxplot() + 
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10)) + 
  xlab("SALE.PRICE.M") +
  ylab("Borough")

mean(sales_removed$SALE.PRICE, na.rm=TRUE)
median(sales_removed$SALE.PRICE, na.rm=TRUE)
sd(sales_removed$SALE.PRICE, na.rm=TRUE)
min(sales_removed$SALE.PRICE, na.rm=TRUE)
max(sales_removed$SALE.PRICE, na.rm=TRUE)

# SALE.PRICE boxplot according to Borough
plot10 <- sales_removed %>% 
  ggplot(aes(SALE.PRICE.M, BOROUGH2)) +                           
  geom_boxplot() + 
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10)) +
  xlab("SALE.PRICE in millions") +
  ylab("Borough")

# SALE.PRICE boxplot according to Tax class
plot11 <- sales_removed %>% 
  ggplot(aes(SALE.PRICE.M, TAX.CLASS.AT.PRESENT)) +                           
  geom_boxplot() + 
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10)) +
  xlab("SALE.PRICE in millions") +
  ylab("TAX.CLASS.AT.PRESENT")

grid.arrange(plot10, plot11, ncol=2)

# SALE.PRICE boxplot according to Building class category
sales_removed %>% 
  ggplot(aes(x=SALE.PRICE.M, y=reorder(BUILDING.CLASS.CATEGORY,SALE.PRICE.M,FUN = median))) +                           
  geom_boxplot() + 
  xlab("SALE.PRICE in millions") +
  ylab("BUILDING.CLASS.CATEGORY")

# SALE.PRICE boxplot according to month of the sale
sales_removed %>% 
  ggplot(aes(SALE.DATE_month)) +                           
  geom_bar(color = "gray", fill = "#0B625B") + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12)) + 
  xlab("Sale Month") +
  ylab("Count")

################################################################################
# Correlation matrix: matrix and visualization
################################################################################
str(sales_removed)
sales_corr <- sales_removed
sales_corr <- sales_corr[,-c(1,3,4,5,8,9,10,11,20,22,23,24,25,27,28)]
str(sales_corr)
## Compute a correlation matrix
corr <- round(cor(sales_corr, use = "complete.obs"), 1)
head(corr[, 1:10])

## Compute a matrix of correlation p-values
p.mat <- cor_pmat(sales_corr)
head(p.mat[, 1:13])

# Argument p.mat
# Barring the no significant coefficient, argument lab = TRUE
ggcorrplot(corr, hc.order = TRUE, type = "lower",
           lab = TRUE, digits = 2, tl.srt = 90, tl.cex = 7, lab_size = 2.5)
# There is no correlation higher than 0.7 except land and gross sq feet = not problem   

################################################################################
# Modelling
################################################################################



################################################################################
# Dependent Variable
################################################################################

# SALE.PRICE
# SALE.PRICE.M

# SALE.PRICE.M: Histogram
sales_removed %>% ggplot(aes(SALE.PRICE.M)) +                                  
  geom_histogram(color = "black", fill = "#0B625B", bins = 50) +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10)) +
  xlab("SALE.PRICE.M") +
  ylab("Count") + 
  geom_vline(aes(xintercept = mean(SALE.PRICE.M, na.rm=TRUE)), color = "red", linewidth = 1) +
  geom_text(aes(x = mean(SALE.PRICE.M, na.rm=TRUE)), y = 8300, label = "Mean", color = "red", angle=90, vjust = 1) + 
  geom_vline(aes(xintercept = median(SALE.PRICE.M, na.rm=TRUE)), color = "blue", linewidth = 1) +
  geom_text(aes(x = median(SALE.PRICE.M, na.rm=TRUE)), y = 8300, label = "Median", color = "blue", angle=90, vjust = 1) +
  ggtitle("Histogram: SALE.PRICE")

# SALE.PRICE.M: Boxplot
sales_removed %>% ggplot(aes(SALE.PRICE.M)) +                               
  geom_boxplot() + 
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10)) + 
  xlab("SALE.PRICE.M") +
  ylab("Borough")


################################################################################
# Independent Variables
################################################################################

# BOROUGH
# ZIP.CODE: no
# RESIDENTIAL.UNITS
# COMMERCIAL.UNITS
# TOTAL.UNITS
# LAND.SQUARE.FEET
# GROSS.SQUARE.FEET
# YEAR.BUILT
# BUILDING.CLASS.AT.TIME.OF.SALE
# Prop_Age

# TOTAL.UNITS
sales_removed %>%
  ggplot() + 
  aes(TOTAL.UNITS) + 
  stat_ecdf(geom = "step", color="#0B625B") + 
  xlim(c(0, 50)) + 
  labs(x = "TOTAL.UNITS", y = "Cumulative Sum of TOTAL.UNITS")

# RESIDENTIAL.UNITS: ordered
plot12 <- sales_removed %>% 
  ggplot(aes(x=SALE.PRICE.M, y=reorder(RESIDENTIAL.UNITS,SALE.PRICE.M,FUN = median))) +                           
  geom_boxplot() + 
  xlab("SALE.PRICE in millions") +
  ylab("RESIDENTIAL.UNITS")

# COMMERCIAL.UNITS: ordered
plot13 <- sales_removed %>% 
  ggplot(aes(x=SALE.PRICE.M, y=reorder(COMMERCIAL.UNITS,SALE.PRICE.M,FUN = median))) +                           
  geom_boxplot() + 
  xlab("SALE.PRICE in millions") +
  ylab("COMMERCIAL.UNITS")

grid.arrange(plot12, plot13, ncol=2)

# GROSS.SQUARE.FEET: 
sales_removed %>% 
  ggplot(aes(x=GROSS.SQUARE.FEET, y=SALE.PRICE.M, color = BOROUGH2)) +                           
  geom_point() + 
  xlim(c(0, 2e+4)) + 
  xlab("GROSS.SQUARE.FEET") +
  ylab("SALE.PRICE.M")

# Prop_Age
sales_removed %>% 
  ggplot(aes(x=Prop_Age, y=SALE.PRICE.M, color = BOROUGH2)) +                           
  geom_point() + 
  xlim(c(0, 200)) + 
  xlab("Prop_Age") +
  ylab("SALE.PRICE.M")

################################################################################
# Modelling
################################################################################

# Transforming/Normalizing/Standardizing data

# Removing variable Basement (ASE.MENT) for purpose of Machine Learning.
sales_removed_ML <- sales_removed[,-8]

# First, we need to check for missing and NA values in variables. 
# Most of the missing values are in variable Basement which is not that important for us for purposes of the analysis.
aggr(sales_removed_ML, sortVars = TRUE, prop = FALSE, cex.axis = .35, 
     numbers = TRUE, col = c('grey99','#0B625B'))

sum(is.na(sales_removed_ML$SALE.PRICE))


# For variable LAND.SQUARE.FEET we have 7054 NAs. We are using a linear model to predict these values.

# Creating linear model to predict LAND.SQUARE.FEET NAs values. 
lm_LAND.SQUARE.FEET <- lm(LAND.SQUARE.FEET ~ TOTAL.UNITS + BOROUGH + YEAR.BUILT + GROSS.SQUARE.FEET + COMMERCIAL.UNITS, 
                          data = sales_removed_ML, na.action = na.omit)

# Predicting all NAs in variable LAND.SQUARE.FEET with linear model.
sales_removed_ML$LAND.SQUARE.FEET[is.na(sales_removed_ML$LAND.SQUARE.FEET)] <- predict(lm_LAND.SQUARE.FEET)

aggr(sales_removed_ML, sortVars = TRUE, prop = FALSE, cex.axis = .35, 
     numbers = TRUE, col = c('grey99','#0B625B'))

# For variable GROSS.SQUARE.FEET we have 7191 NAs. We are using a linear model to predict these values.

# Creating linear model to predict GROSS.SQUARE.FEET NAs values. 
lm_GROSS.SQUARE.FEET <- lm(GROSS.SQUARE.FEET ~ TOTAL.UNITS + BOROUGH + YEAR.BUILT + LAND.SQUARE.FEET + COMMERCIAL.UNITS, 
                           data = sales_removed_ML, na.action = na.omit)

# Predicting all NAs in variable GROSS.SQUARE.FEET with linear model.
sales_removed_ML$GROSS.SQUARE.FEET[is.na(sales_removed_ML$GROSS.SQUARE.FEET)] <- predict(lm_GROSS.SQUARE.FEET)

aggr(sales_removed_ML, sortVars = TRUE, prop = FALSE, cex.axis = .35, 
     numbers = TRUE, col = c('grey99','#0B625B'))

# In Standard scaling, also known as Standardization of values, we scale the data values such that the overall statistical summary of every variable has a mean value of zero and an unit variance value.

sales_removed_ML <- sales_removed_ML %>%
  mutate(s_LAND.SQUARE.FEET = as.vector(scale(LAND.SQUARE.FEET, center = TRUE, scale = TRUE))) %>%
  mutate(s_GROSS.SQUARE.FEET = as.vector(scale(GROSS.SQUARE.FEET, center = TRUE, scale = TRUE))) %>%
  mutate(s_RESIDENTIAL.UNITS = as.vector(scale(RESIDENTIAL.UNITS, center = TRUE, scale = TRUE))) %>%
  mutate(s_COMMERCIAL.UNITS = as.vector(scale(COMMERCIAL.UNITS, center = TRUE, scale = TRUE))) %>%
  mutate(s_TOTAL.UNITS = as.vector(scale(TOTAL.UNITS, center = TRUE, scale = TRUE))) %>%
  mutate(s_YEAR.BUILT = as.vector(scale(YEAR.BUILT, center = TRUE, scale = TRUE))) %>%
  mutate(s_Prop_Age = as.vector(scale(Prop_Age, center = TRUE, scale = TRUE))) %>%
  mutate(s_BOROUGH = as.vector(scale(BOROUGH, center = TRUE, scale = TRUE))) %>%
  mutate(s_SALE.PRICE = as.vector(scale(SALE.PRICE, center = TRUE, scale = TRUE)))

as.data.frame(sales_removed_ML)
str(sales_removed_ML)

# SALE.PRICE.M: Histogram before Standardization
plot14 <- sales_removed_ML %>% ggplot(aes(SALE.PRICE)) +                                  
  geom_histogram(color = "black", fill = "#0B625B", bins = 40) +
  xlab("SALE.PRICE") +
  ylab("Count") + 
  geom_vline(aes(xintercept = mean(SALE.PRICE, na.rm=TRUE)), color = "red", linewidth = 1) +
  geom_text(aes(x = mean(SALE.PRICE, na.rm=TRUE)), y = 10000, label = "Mean", color = "red", angle=90, vjust = 1) + 
  geom_vline(aes(xintercept = median(SALE.PRICE, na.rm=TRUE)), color = "blue", linewidth = 1) +
  geom_text(aes(x = median(SALE.PRICE, na.rm=TRUE)), y = 10000, label = "Median", color = "blue", angle=90, vjust = 1) +
  ggtitle("Histogram: SALE.PRICE")

# s_SALE.PRICE: Histogram after Standardization
plot15 <- sales_removed_ML %>% ggplot(aes(s_SALE.PRICE)) +                                  
  geom_histogram(color = "black", fill = "#0B625B", bins = 40) +
  xlab("Standardized SALE.PRICE") +
  ylab("Count") + 
  geom_vline(aes(xintercept = mean(s_SALE.PRICE)), color = "red", linewidth = 1) +
  geom_text(aes(x = mean(s_SALE.PRICE)), y = 10000, label = "Mean", color = "red", angle=90, vjust = 1) + 
  geom_vline(aes(xintercept = median(s_SALE.PRICE)), color = "blue", linewidth = 1) +
  geom_text(aes(x = median(s_SALE.PRICE)), y = 10000, label = "Median", color = "blue", angle=90, vjust = 1) +
  ggtitle("Histogram: Standardized SALE.PRICE")

grid.arrange(plot14, plot15, ncol=2)


# Split data into training (90%) and testing (10%) set
set.seed(1, sample.kind="Rounding") # Using R v4+
test_index <- createDataPartition(y = sales_removed_ML$s_SALE.PRICE, times = 1, p = 0.1, list = FALSE)

test_set <- sales_removed_ML[test_index, ]
train_set <- sales_removed_ML[-test_index,]
temp <- sales_removed_ML[test_index,]

rm(temp)


test_set <- test_set[,!names(test_set) %in% c("X", "NEIGHBORHOOD", "BUILDING.CLASS.CATEGORY", "TAX.CLASS.AT.PRESENT", "BUILDING.CLASS.AT.PRESENT", "ADDRESS", "APARTMENT.NUMBER", "RESIDENTIAL.UNITS", "COMMERCIAL.UNITS", "TOTAL.UNITS", "LAND.SQUARE.FEET", "GROSS.SQUARE.FEET", "BUILDING.CLASS.AT.TIME.OF.SALE", "SALE.PRICE", "SALE.DATE", "BOROUGH2", "SALE.PRICE.M", "Curr_Year", "Prop_Age", "s_YEAR.BUILT", "s_Prop_Age", "s_BOROUGH")]
train_set <- train_set[,!names(train_set) %in% c("X", "NEIGHBORHOOD", "BUILDING.CLASS.CATEGORY", "TAX.CLASS.AT.PRESENT", "BUILDING.CLASS.AT.PRESENT", "ADDRESS", "APARTMENT.NUMBER", "RESIDENTIAL.UNITS", "COMMERCIAL.UNITS", "TOTAL.UNITS", "LAND.SQUARE.FEET", "GROSS.SQUARE.FEET", "BUILDING.CLASS.AT.TIME.OF.SALE", "SALE.PRICE", "SALE.DATE", "BOROUGH2", "SALE.PRICE.M", "Curr_Year", "Prop_Age", "s_YEAR.BUILT", "s_Prop_Age", "s_BOROUGH")]
names(train_set)

##########################################################
# Tibble for RMSE comparison and results
##########################################################
rmse_comp <- tibble()
rmse_comp
# Default S3 method:
# rmse(actual, predicted, ...)
# The root mean square error (RMSE) allows us to measure how far predicted values are from observed values in a regression analysis.
# The larger the difference indicates a larger gap between the predicted and observed values, which means poor regression model fit. 
# In the same way, the smaller RMSE that indicates the better the model.
# Based on RMSE we can compare the two different models with each other and be able to identify which model fits the data better.

##########################################################
# Model 1: using only average (mean) of values
##########################################################

# Simplest possible prediction 
# We predict the same sale price for all properties sold
mu1 <- mean(train_set$s_SALE.PRICE)
RMSE_model_1 <- RMSE(train_set$s_SALE.PRICE, mu1)

rmse_comp <- tibble(Method = "Model: Mean", RMSE = RMSE_model_1)
rmse_comp

##########################################################
# Model 2: Machine Learning
##########################################################

# Random Forest model
# Random Forest in R Programming is an ensemble of decision trees. It builds and combines multiple decision trees to get more accurate predictions. It’s a non-linear classification algorithm.
# Random Forest takes random samples from the observations, random initial variables(columns) and tries to build a model.
# Applying on training set

set.seed(77)
rf_model_1 <- randomForest(s_SALE.PRICE ~ .,
                           data = train_set,
                           ntree = 500,
                           replace = TRUE,
                           keep.forest = TRUE,
                           type = "regression")
print(rf_model_1)
rf_model_1_var_expl <- round(100 * rf_model_1$rsq[length(rf_model_1$rsq)], digits = 2)
rf_model_1_var_expl
rf_model_1_MSE <- rf_model_1$mse[length(rf_model_1$mse)]
rf_model_1_MSE

# Plotting the model will illustrate the error rate as we average across more trees and shows that our error rate stabalizes with around xxx trees but continues to decrease slowly until around xxx or so trees.
plot(rf_model_1)

# The mean of squared residuals and % variance explained indicate how well the model fits the data. 
# Residuals are a difference between prediction and the actual value.
# increase or decrease, the number of trees (ntree) or the number of variables tried at each split (mtry) and see whether the residuals or % variance change.
# If you also want to understand what the model has learnt, make sure that you do importance = TRUE as in the code above
# No. of variables tried at each split: 4
# In the base RF model, rf_model_2, the ntree is set to 500 and default mtry is set to 4.
# Random forest regression in R provides two outputs: decrease in mean square error (MSE) and node purity.

# To calculate RMSE of default RF model, we can also extract the last element of rf_model_1$mse, which corresponds to the final tree created, and take its square root.
# obtain MSE as of last element in rf_model_1$mse, which should match the output from printout
rf_model_1$mse[length(rf_model_1$mse)]
# take square root to calculate RMSE for the model
rf_model_1_RMSE_default <- sqrt(rf_model_1$mse[length(rf_model_1$mse)])
rf_model_1_RMSE_default

rf_model_1_trees_default <- rf_model_1[["call"]][["ntree"]]
rf_model_1_trees_default

rf_model_1_mtry_default <- rf_model_1[["mtry"]]
rf_model_1_mtry_default

rmse_comp <- bind_rows(rmse_comp,
                    tibble(Method = "Model 1: RF with default settings", 
                    RMSE = rf_model_1_RMSE_default, 
                    Trees = rf_model_1_trees_default, 
                    mTry = rf_model_1_mtry_default))
rmse_comp

### Error rate according to number of trees chosen
# number of trees with lowest MSE
rf_model_1_trees_chosen_trees <- which.min(rf_model_1$mse)
rf_model_1_trees_chosen_trees

# MSE of this optimal random forest
sqrt(rf_model_1$mse[which.min(rf_model_1$mse)])
rf_model_1_RMSE_chosen_trees <- sqrt(rf_model_1$mse[which.min(rf_model_1$mse)])
rf_model_1_RMSE_chosen_trees


rmse_comp <- bind_rows(rmse_comp,
                       tibble(Method = "Model 1: RF with chosen trees (lowest MSE)", 
                              RMSE = rf_model_1_RMSE_chosen_trees, 
                              Trees = rf_model_1_trees_chosen_trees, 
                              mTry = rf_model_1_mtry_default))
rmse_comp


# Creating an object for importance of variables
importance_var <- rf_model_1[["importance"]]

# Creating data frame using importance
varImportance <- data.frame(Variables = row.names(importance_var), 
                            Importance = round(importance_var[,'IncNodePurity'], 0))



# Creating plot. Here we can see our variables by importance. It looks like ZIP.CODE and BLOCK (connected) with GROSS.SQUARE.FEET are the most important variables in our model followed by YEAR.BUILT and LOT.
rf_model_1_varimpplot <- ggplot(varImportance, aes(x = reorder(Variables, Importance), 
                          y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  labs(title = 'Importance of predictors', x = 'Predictors', y = 'IncNodePurity') +
  coord_flip() + 
  theme_light()
rf_model_1_varimpplot

rf_model_1_varimpplot2 <- varImpPlot(rf_model_1)
rf_model_1_varimpplot2

# The first graph shows that if a variable is assigned values by random permutation by how much will the MSE increase.
# Higher the value, higher the variable importance.
# Node purity is measured by Gini Index which is the the difference between RSS before and after the split on that variable.
# There is no fixed criterion to select the “best” measure of variable importance it depends on the problem you have at hand.


### Model Tuning

## ntree: number of trees. We want enough trees to stabalize the error but using too many trees is unncessarily inefficient, especially when using large data sets.
## mtry: the number of variables selected at each split is denoted by mtry in randomforest function.
# Finding the optimal mtry value
# Select mtry value with minimum out of bag(OOB) error.

## Tuning with tuneRF() function
# starts with mtry = 5 and increases by a factor of 1.5 until the OOB error stops improving by 1%
features <- setdiff(names(train_set), "s_SALE.PRICE")
set.seed(77)
rf_model_2_tuneRF <- tuneRF(
  x          = train_set[features],
  y          = train_set$s_SALE.PRICE,
  ntreeTry   = 500,
  mtryStart  = 5,
  stepFactor = 1.5,
  improve    = 0.01)
print(rf_model_2_tuneRF)
plot(rf_model_2_tuneRF)
# mtry with lowest OOBError is 7

# 
rf_model_2 <- randomForest(s_SALE.PRICE ~ .,
                           data = train_set, 
                           ntree = 500,
                           replace = TRUE,
                           keep.forest = TRUE,
                           mtry = 7,
                           type = "regression")
print(rf_model_2)
######

rf_model_2_MSE <- rf_model_2$mse[length(rf_model_2$mse)]
rf_model_2_MSE
rf_model_2_var_expl <- round(100 * rf_model_2$rsq[length(rf_model_2$rsq)], digits = 2)
rf_model_2_var_expl

######
rf_model_2_RMSE_tuneRF <- sqrt(rf_model_2$mse[length(rf_model_2$mse)])
rf_model_2_RMSE_tuneRF

rf_model_2_trees_tuneRF <- rf_model_2[["call"]][["ntree"]]
rf_model_2_trees_tuneRF

rf_model_2_mtry_tuneRF <- rf_model_2[["mtry"]]
rf_model_2_mtry_tuneRF



rmse_comp <- bind_rows(rmse_comp,
                       tibble(Method = "Model 2: RF with tuneRF mTry", 
                              RMSE = rf_model_2_RMSE_tuneRF, 
                              Trees = rf_model_2_trees_tuneRF, 
                              mTry = rf_model_2_mtry_tuneRF))
rmse_comp

##########################

rf_model_2_chosen_trees <- randomForest(s_SALE.PRICE ~ .,
                           data = train_set, 
                           ntree = 485,
                           replace = TRUE,
                           keep.forest = TRUE,
                           mtry = 7,
                           type = "regression")
print(rf_model_2_chosen_trees)

rf_model_2_chosen_trees_MSE <- rf_model_2_chosen_trees$mse[length(rf_model_2_chosen_trees$mse)]
rf_model_2_chosen_trees_MSE
rf_model_2_chosen_trees_var_expl <- round(100 * rf_model_2_chosen_trees$rsq[length(rf_model_2_chosen_trees$rsq)], digits = 2)
rf_model_2_chosen_trees_var_expl

rf_model_2_chosen_trees_RMSE_tuneRF <- sqrt(rf_model_2_chosen_trees$mse[length(rf_model_2_chosen_trees$mse)])
rf_model_2_chosen_trees_RMSE_tuneRF

rf_model_2_chosen_trees_trees_tuneRF <- rf_model_2_chosen_trees[["call"]][["ntree"]]
rf_model_2_trees_tuneRF

rf_model_2_chosen_trees_mtry_tuneRF <- rf_model_2_chosen_trees[["mtry"]]
rf_model_2_chosen_trees_mtry_tuneRF



rmse_comp <- bind_rows(rmse_comp,
                       tibble(Method = "Model 2: RF with tuneRF mTry and chosen trees", 
                              RMSE = rf_model_2_chosen_trees_RMSE_tuneRF, 
                              Trees = rf_model_2_chosen_trees_trees_tuneRF, 
                              mTry = rf_model_2_chosen_trees_mtry_tuneRF))
rmse_comp

## Tuning with Grid Search
# Another search is to define a grid of algorithm parameters to try.
# Each axis of the grid is an algorithm parameter, and points in the grid are specific combinations of parameters. 
# Because we are only tuning one parameter, the grid search is a linear search through a vector of candidate values.


# specifying the CV technique which will be passed into the train() function later and number parameter is the "k" in K-fold cross validation
train_control <- trainControl(method = "cv", number = 5, search = "grid")

set.seed(77)
# Customsing the tuning grid
gbmGrid <-  expand.grid(max_depth = c(3, 5, 7), 
                        nrounds = (1:10)*50,    # number of trees
                        eta = 0.3,
                        gamma = 0,
                        subsample = 1,
                        min_child_weight = 1,
                        colsample_bytree = 0.6)

# training a XGboost Regression tree model while tuning parameters
rf_model_3_grid_train <- train(s_SALE.PRICE ~ ., 
                        data = train_set, 
                        method = "xgbTree", 
                        trControl = train_control, 
                        tuneGrid = gbmGrid)

# summarising the results
# RMSE was used to select the optimal model using the smallest value.
# The final values used for the model were nrounds = 100, max_depth = 7, eta = 0.3, gamma = 0, colsample_bytree = 0.6, min_child_weight = 1 and subsample = 1
print(rf_model_3_grid_train)

rf_model_3_grid <- randomForest(s_SALE.PRICE ~ .,
                                data = train_set, 
                                ntree = 100,
                                replace = TRUE,
                                keep.forest = TRUE,
                                mtry = 7,
                                max_depth = 7,
                                eta = 0.3,
                                gamma = 0, 
                                colsample_bytree = 0.6, 
                                min_child_weight = 1,
                                subsample = 1,
                                type = "regression")

print(rf_model_3_grid)

rf_model_3_grid_MSE <- rf_model_3_grid$mse[length(rf_model_3_grid$mse)]
rf_model_3_grid_MSE
rf_model_3_grid_var_expl <- round(100 * rf_model_3_grid$rsq[length(rf_model_3_grid$rsq)], digits = 2)
rf_model_3_grid_var_expl

rf_model_3_chosen_trees_RMSE_grid <- sqrt(rf_model_3_grid$mse[length(rf_model_3_grid$mse)])
rf_model_3_chosen_trees_RMSE_grid

rf_model_3_chosen_trees_trees_grid <- rf_model_3_grid[["call"]][["ntree"]]
rf_model_3_chosen_trees_trees_grid

rf_model_3_chosen_trees_mtry_grid <- rf_model_3_grid[["mtry"]]
rf_model_3_chosen_trees_mtry_grid



rmse_comp <- bind_rows(rmse_comp,
                       tibble(Method = "Model 3: RF with grid mTry and chosen trees", 
                              RMSE = rf_model_3_chosen_trees_RMSE_grid, 
                              Trees = rf_model_3_chosen_trees_trees_grid, 
                              mTry = rf_model_3_chosen_trees_mtry_grid))
rmse_comp

# Summary of RMSEs
rf_model_1_RMSE_default
rf_model_1_RMSE_chosen_trees
rf_model_2_RMSE_tuneRF
rf_model_2_chosen_trees_RMSE_tuneRF
rf_model_3_chosen_trees_RMSE_grid

# Using best performing model (with lowest RMSE) to predict using test data
# Now that we have created our final model, we will use it to predict sale price of the property on a test dataset.
prediction_final <- predict(rf_model_2_chosen_trees, test_set)
plot(prediction_final)

RMSE_model_final <- RMSE(test_set$s_SALE.PRICE, prediction_final)
# RMSE: 0.5032866
RMSE_model_final
