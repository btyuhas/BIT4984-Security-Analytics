## Install and load packages -----------------------------------------------------------------------
#install.packages(c('tidyverse', 'magrittr', 'BatchGetSymbols', 'gtrendsR'))

library(tidyverse)
library(magrittr)
library(BatchGetSymbols)
library(gtrendsR)

## Load data and correct format --------------------------------------------------------------------
#setwd("~/git/BIT4984-Security-Analytics/")
df <- read.csv("BreachedCompanies.csv")
View(df)

# Fix dataset for forecasting 
df %<>% 
  group_by(ticker) %>% 
  mutate(breach_this_year=breach) %>% 
  mutate(breach_last_year=lag(breach)) %>% 
  mutate(breachtype_this_year=breachtype) %>% 
  mutate(breachtype_last_year=lag(breachtype)) %>% 
  mutate(breach = lead(breach), breachtype = lead(breachtype)) %>%
  filter(year != 2010)

## Feature engineering -----------------------------------------------------------------------------
# Google trends results
search_terms <- paste0(unique(df$compname), " data security")

search_trends <- gtrends(search_terms, time="2005-01-01 2009-12-31")$interest_over_time
trends %>% 
  group_by(date=substr(date, 1, 4)) %>% 
  mutate(max_hits=max(hits), hits=mean(hits)) %>% 
  mutate(keyword = substr(keyword, 1, nchar(keyword)-2)) %>% 
  select(-c(geo, gprop, category)) %>% 
  distinct()

df %<>% left_join(search_trends$hits, by=c("compname"="keyword", "year"="date"))

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
df %<>% 
  group_by(ticker) %>% 
  mutate_at(vars(volume, price.open, price.high, price.low, price.close, price.adjusted,
                 ret.adjusted.prices, ret.closing.prices, salesmillions, employees),
            list(pct = ~ ./lag(.)))

# Save augmented dataset ----------------------------------------------------------------------------
write.csv(df, "Breached_Augmented.csv")
