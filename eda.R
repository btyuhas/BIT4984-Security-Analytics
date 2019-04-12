# Install and load packages -----------------------------------------------------------------------
x <- c("tidyverse", "GGally", "corrr")
install.packages(x)

library(tidyverse)
library(GGally)
library(corrr)

# Load and format data ----------------------------------------------------------------------------
#setwd("BIT4984-Security-Analytics/")
breached <- read.csv("Breached_Augmented.csv")

numeric_cols <- breached %>% 
  select(-c(X, gvkey, ticker, compname, sic4, breachtype_this_year, industry_translate))

numeric_cols <- numeric_cols %>% 
  mutate_if(is.integer, as.numeric) %>% 
  mutate_if(is.factor, as.numeric)

# EDA ---------------------------------------------------------------------------------------------
ggcorr(numeric_cols)

ggpairs(numeric_cols[c(4, 25:31)], ggplot2::aes(colour=as.factor(breach)))

ggpairs(numeric_cols[c(4:13)], ggplot2::aes(colour=as.factor(breach)))

ggplot(numeric_cols, aes(x=industry, y=industry_breach_sum, fill=as.factor(breach))) + 
  geom_bar(stat='identity', position='fill') 