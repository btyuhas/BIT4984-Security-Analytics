# Install and load packages -----------------------------------------------------------------------
x <- c("tidyverse", "xgboost", "Ckmeans.1d.dp", "pROC", "ROCR")
install.packages(x)

library(tidyverse)
library(xgboost)
library(Ckmeans.1d.dp)
library(pROC)
library(ROCR)

# Load and transform data -------------------------------------------------------------------------
data <- read.csv("Breached_Augmented.csv")

numeric_cols <- data %>% 
  select(-c(X, gvkey, ticker, compname, sic4, breachtype_this_year, industry_translate, ag_frst_fsh))

numeric_cols <- numeric_cols %>% 
  mutate_if(is.integer, as.numeric) %>% 
  mutate_if(is.factor, as.numeric)

X_train <- numeric_cols %>% 
  filter(year %in% c(2005, 2006, 2007, 2008)) %>% 
  select(-breach)

X_test <- numeric_cols %>% 
  filter(year == 2009) %>% 
  select(-breach)

y_train <- numeric_cols %>% 
  filter(year %in% c(2005, 2006, 2007, 2008)) %>% 
  select(breach)

y_test <- numeric_cols %>% 
  filter(year == 2009) %>% 
  select(breach )

# Modeling ----------------------------------------------------------------------------------------
linear_model <- lm(y_train$breach ~ ., data = X_train)
summary(linear_model)

dtrain <- xgb.DMatrix(as.matrix(X_train), label=as.matrix(y_train))
dtest <- xgb.DMatrix(as.matrix(X_test), label=as.matrix(y_test))
param <- list(max_depth=2, eta=.1, objective="binary:logistic", eval_metric="auc")
watchlist <- list(train=dtrain, eval=dtest)
xgb_model <- xgb.train(param, dtrain, nrounds=100, watchlist)

# Evaluation --------------------------------------------------------------------------------------
importance <- xgb.importance(model=xgb_model)
xgb.ggplot.importance(importance)

xgb_pred <- predict(xgb_model, dtest)
pROC_obj <- roc(y_test$breach, xgb_pred,
                smoothed = TRUE, ci=TRUE, ci.alpha=0.9, plot=TRUE, auc.polygon=TRUE,
                max.auc.polygon=TRUE, grid=TRUE, print.auc=TRUE)

sens.ci <- ci.se(pROC_obj)
plot(sens.ci, type="shape", col="lightblue")
plot(sens.ci, type="bars")

hist(y_test$breach, col=rgb(1, 0, 0, .5), prob=T, ylim=c(0, 10), main="Distribution of Breaches")
hist(xgb_pred, col=rgb(0,0,1,.5), prob=T, add=T)
legend("topright", legend = c("Observed", "Predicted"), col=c("red", "blue"), fill=c("red", "blue"))
box()

pred <- prediction(xgb_pred, y_test$breach)
RP.perf <- performance(pred, "prec", "rec")

plot(RP.perf)
plot(performance(pred, 'f'))


