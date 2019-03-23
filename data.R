## Install and load packages -----------------------------------------------------------------------
#install.packages(c('tidyverse', 'magrittr', 'BatchGetSymbols', 'gtrendsR'))

library(tidyverse)
library(magrittr)
library(BatchGetSymbols)
library(gtrendsR)

## Load and clean dataset --------------------------------------------------------------------
setwd("~/git/BIT4984-Security-Analytics/")
df <- read.csv("BreachedCompanies.csv")

# Drop companies without all years
to_keep <- df %>%
  group_by(ticker) %>%
  summarise(n=n() == 6) %>%
  filter(n==T)
df %<>% filter(ticker %in% to_keep$ticker)

# Clean NA values
df %<>% 
  mutate(breach = ifelse(is.na(breach), 0, breach)) %>% 
  fill(salesmillions, employees, .direction='up')

# Fix dataset for forecasting
df %<>% 
  group_by(ticker) %>% 
  mutate(breach_this_year = breach, breachtype_this_year = breachtype) %>% 
  mutate(breach = lead(breach), breachtype = lead(breachtype)) %>%
  filter(year != 2010)

for (i in 1:ncol(df)){print(c(colnames(df)[i], sum(is.na(df[,i]))))}

## Feature engineering -----------------------------------------------------------------------------

# Breach types
bad <- c("Email", "Hack/Web", "Virus", "Web", "Hack")
internal <- c("Dishonest employee", "FraudSe", "FraudSe/StolenLaptop", "Unintended Disclosure")
df$bad_breach <- ifelse(df$breachtype_this_year %in% bad, 1, 0)
df$internal_breach <- ifelse(df$breach_this_year %in% internal, 1, 0)
df$rand_breach <- ifelse(df$breachtype_this_year %in% setdiff(setdiff(levels(df$breachtype), bad), internal), 1, 0)
df$breachtype <- NULL

# Cumulative Breaches
#   By company
df %<>%
  group_by(ticker) %>% 
  mutate(breach_sum = cumsum(breach_this_year), bad_breach_sum = cumsum(bad_breach))

#   By Industry
breaks <- c(100, 999, 1499, 1799, 1999, 3999, 4999, 5199, 5999, 6799, 8999, 9729, 9999)
df$industry <- findInterval(df$sic4, breaks)
df %<>%
  group_by(year, industry) %>% 
  mutate(industry_breach_sum = cumsum(breach_this_year), industry_bad_breach_sum = cumsum(bad_breach)) %>% 
  ungroup()

industries <- read.csv("Breached_Augmented_2nd.csv")
df %<>% left_join(industries, by = c("ticker"="ticker", "year"="year"))
# Google trends results
#   Captures consumer interest in company
#   Literally the slowest thing known to mankind
#   Stored in google_trends.csv
# df2 <- data.frame()
# for (i in 1:(400/5)){
#   search_terms <- as.character(unique(df$ticker))[(5*i-4):(5*i)]
#   search_trends <- gtrends(search_terms, time="2005-01-01 2009-12-31")$interest_over_time
#   
#   if(!is.null(search_trends)){
#     search_trends %<>%
#       mutate(hits=as.numeric(hits)) %>% 
#       drop_na(hits) %>% 
#       group_by(date=as.numeric(substr(date, 1, 4)), keyword) %>% 
#       summarise(mean_hits=mean(hits), max_hits=max(hits))
#     
#     df2 <- bind_rows(df2, search_trends) 
#   }
# }
df2 <- read.csv("google_trends.csv", row.names = 1)
df %<>% left_join(df2, by=c("ticker"="keyword", "year"="date"))

# Get stock details 
#   Takes a lil bit, might run into API limits if rerun too much
#   Stored in stock_prices.csv
# l.out <- BatchGetSymbols(tickers = unique(df$ticker),
#                          first.date = "2005-01-01",
#                          last.date = "2009-12-31",
#                          freq.data = "yearly")
# l.out$df.tickers %<>% mutate(ref.date = as.numeric(substr(ref.date, 1, 4)))
l.out <- read.csv("stock_prices.csv", row.names = 1)
# Merge stock and given dataset
df %<>% left_join(l.out, by=c('ticker' = 'ticker', 'year' = 'ref.date'))

# Add one year percentage change
df %<>% 
  group_by(ticker) %>% 
  mutate_at(vars(volume, price.open, price.high, price.low, price.close, price.adjusted,
                 ret.adjusted.prices, ret.closing.prices, salesmillions, employees, mean_hits, max_hits),
            list(pct = ~ ./lag(.)))

# Save augmented dataset ----------------------------------------------------------------------------
write.csv(df, "Breached_Augmented.csv")
