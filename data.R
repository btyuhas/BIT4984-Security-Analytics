## Install and load packages -----------------------------------------------------------------------
#install.packages(c('tidyverse', 'magrittr', 'BatchGetSymbols'))

library(tidyverse)
library(magrittr)
library(BatchGetSymbols)

## Load data and correct format --------------------------------------------------------------------
#setwd("~/drive/Security Analytics/")
df <- read.csv("BreachedCompanies.csv")
View(df)

# Fix dataset for forecasting 
df %<>% group_by(ticker) %>% 
  mutate(breach_this_year=breach) %>% 
  mutate(breach_last_year=lag(breach)) %>% 
  mutate(breachtype_this_year=breachtype) %>% 
  mutate(breachtype_last_year=lag(breachtype)) %>% 
  mutate(breach = lead(breach), breachtype = lead(breachtype)) %>%
  filter(year != 2010)

## Feature engineering -----------------------------------------------------------------------------
# Get stock details 
# Takes a lil bit, might run into API limits if rerun too much
l.out <- BatchGetSymbols(tickers = unique(df$ticker),
                         first.date = "2005-01-01",
                         last.date = "2009-12-31",
                         freq.data = "yearly")
l.out$df.tickers %<>% mutate(ref.date = as.numeric(substr(ref.date, 1, 4)))
# merge stock and given dataset
df %<>% left_join(l.out$df.tickers, by=c('ticker' = 'ticker', 'year' = 'ref.date'))

# Add one year percentage change
df %<>% group_by(ticker) %>% 
  mutate_at(vars(volume, price.open, price.high, price.low, price.close, price.adjusted,
                 ret.adjusted.prices, ret.closing.prices, salesmillions, employees),
            list(pct = ~ ./lag(.)))

# Save augmented dataset ----------------------------------------------------------------------------
write.csv(df, "Breached_Augmented.csv")
