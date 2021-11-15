## Stats 504, F21
## Assignment 4: New Taipei Housing Litigation
##
## Author:  Hongfan Chen, chenhf@umich.edu
## Updated: November, 14, 2021
## 
# 79: -------------------------------------------------------------------------
# libraries: 
library(caret)
library(tidyverse)
library(gam)
library(rpart)
library(rattle)
library(randomForest)
library(gg3D)
library(plotly)
# directories: ----------------------------------------------------------------
path = './'
# data: -----------------------------------------------------------------------
housing_file = sprintf('%s/Real estate valuation data set.xlsx', path)
housing = readxl::read_xlsx(housing_file)[,-1]
names(housing) = c("Date", "Age", "MRTDist", "StoreNum",
                   "Latitude", "Longitude", "Price")
## ----------------------------------------------------------------------------
## ----------------------------------- EDA  -----------------------------------
## ----------------------------------------------------------------------------
plot_ly(data = housing,
        x = ~Longitude, y = ~Latitude, z = ~Price,
        type = "scatter3d", mode = "markers", color = ~MRTDist)
plot_ly(data = housing,
        x = ~Age, y = ~MRTDist, z = ~Price,
        type = "scatter3d", mode = "markers", color = ~StoreNum)
names(housing) = c("Transaction Date", "House Age", "MRT Distance", "Number of Stores",
                   "Latitude", "Longitude", "House Price")
par(mfrow=c(2,3))
with(data = housing, plot(`Transaction Date`, `House Price`))
with(data = housing, plot(`House Age`, `House Price`))
with(data = housing, plot(`MRT Distance`, `House Price`))
with(data = housing, plot(`Number of Stores`, `House Price`))
with(data = housing, plot(`Latitude`, `House Price`))
with(data = housing, plot(`Longitude`, `House Price`))
names(housing) = c("Date", "Age", "MRTDist", "StoreNum",
                   "Latitude", "Longitude", "Price")
housing %>%
  ggplot(aes(x = Date, y = Price)) +
  geom_point()
housing %>%
  ggplot(aes(x = Age, y = Price)) +
  geom_point()

## ----------------------------------------------------------------------------
## --------------------------- Model Selection --------------------------------
## ----------------------------------------------------------------------------
set.seed(2021504)
train = housing %>%
  mutate(StoreNum = as.integer(StoreNum)) %>%
  filter(Price <= 110)
## 10-fold Cross validation
K = 10
fold_size = floor(nrow(train)/K)
results_KNN = tibble(fold = numeric(), k = numeric(), RMSE = numeric())
results_tree = tibble(fold = numeric(), cp = numeric(), RMSE = numeric())
results_rf = tibble(fold = numeric(), mtry = numeric(), RMSE = numeric())
results_GAM = tibble(fold = numeric(), RMSE = numeric())
results_lm = tibble(fold = numeric(), RMSE = numeric())
for(i in 1:K){
    ## iteratively select K-1 folds as training data in CV procedure, remaining
    ## as test data.
    if(i!=K){
      CV_test_id = ((i-1)*fold_size+1):(i*fold_size)
    }else{
      CV_test_id = ((i-1)*fold_size+1):nrow(train)
    }
    CV_train = train[-CV_test_id,]
    CV_test = train[CV_test_id,]
    Test_feature = CV_test[,-7]
    Test_Y = CV_test[,7][[1]]
    ## KNN: -------------------------------------------------------------------
    for(j in 5:15){
    tuneGrid = expand.grid(
      k = j
    )
    MKNN = train(
      Price ~ .,
      data = CV_train,
      method = 'knn',
      preProcess = c("center", "scale"),
      tuneGrid = tuneGrid
    )
    KNN_Pred = predict(MKNN, Test_feature)
    ij_rmse = tibble(fold = i,
                     k = j,
                     RMSE = sqrt(mean((Test_Y - KNN_Pred)^2)))
    results_KNN = results_KNN %>%
      bind_rows(ij_rmse)
    }
    ## Regression Tree: -------------------------------------------------------
    cp_tune = c(1e-1, 1e-2, 5e-3, 1e-3)
    for(j in cp_tune){
      tree_mod = rpart(Price~., data = CV_train, cp = j)
      Tree_pred = predict(tree_mod, newdata = CV_test)
      ij_rmse = tibble(fold = i,
                       cp = j,
                       RMSE = sqrt(mean((Test_Y - Tree_pred)^2)))
      results_tree = results_tree %>%
        bind_rows(ij_rmse)
    }
    ## Random Forest: ---------------------------------------------------------
    mtry_tune = c(3,4,5,6)
    for(j in mtry_tune){
      rf_mod = randomForest(Price~., data = CV_train, mtry = j,
                            ntree = 100, importance = TRUE)
      rf_pred = predict(rf_mod, newdata = CV_test)
      ij_rmse = tibble(fold = i,
                       mtry = j,
                       RMSE = sqrt(mean((Test_Y - rf_pred)^2)))
      results_rf = results_rf %>%
        bind_rows(ij_rmse)
    }
    ## GAM: -------------------------------------------------------------------
    MGam = gam(Price ~ Date + s(Age, 6.5) + s(MRTDist, 9.4) + StoreNum + 
                 s(Latitude, 7.7) + s(Longitude, 8.9),
               data = CV_train)
    GAM_pred = predict(MGam, newdata = CV_test)
    gam_rmse = tibble(fold = i,
                      RMSE = sqrt(mean((Test_Y - GAM_pred)^2)))
    results_GAM = results_GAM %>%
      bind_rows(gam_rmse)
    ## Linear: ----------------------------------------------------------------
    LMod = lm(Price ~ ., data = CV_train)
    Lm_pred = predict(LMod, newdata = CV_test)
    lm_rmse = tibble(fold = i,
                     RMSE = sqrt(mean((Test_Y - Lm_pred)^2)))
    results_lm = results_lm %>%
      bind_rows(lm_rmse)
}

KNN_best = results_KNN %>%
  group_by(k) %>%
  summarize(CV_RMSE = mean(RMSE)) %>%
  arrange(CV_RMSE) %>%
  .[1,] %>%
  mutate(`Parameter` = "K(9)") %>%
  select(-k)
tree_best = results_tree %>%
  group_by(cp) %>%
  summarize(CV_RMSE = mean(RMSE)) %>%
  arrange(CV_RMSE) %>%
  .[1,] %>%
  mutate(`Parameter` = "cp(0.001)") %>%
  select(-cp)
rf_best = results_rf %>%
  group_by(mtry) %>%
  summarize(CV_RMSE = mean(RMSE)) %>%
  arrange(CV_RMSE) %>%
  .[1,] %>%
  mutate(`Parameter` = "mtry(3)") %>%
  select(-mtry)
GAM_best = results_GAM %>%
  summarize(CV_RMSE = mean(RMSE)) %>%
  arrange(CV_RMSE) %>%
  mutate(`Parameter` = "-")
lm_best = results_lm %>%
  summarize(CV_RMSE = mean(RMSE)) %>%
  arrange(CV_RMSE) %>%
  mutate(`Parameter` = "-")
KNN_best %>%
  add_row(tree_best) %>%
  add_row(rf_best) %>%
  add_row(GAM_best) %>%
  add_row(lm_best) %>%
  arrange(CV_RMSE) %>%
  mutate(base = 6.07) %>%
  mutate(pct = (CV_RMSE - base)/CV_RMSE * 100) %>%
  mutate(Method = c("Random Forest", "GAM(Smoothing Spline)",
                    "single regression tree", "KNN", "Linear model"),
         CV_RMSE = sprintf("%.3f", CV_RMSE),
         Improvement = sprintf("%.2f%%", pct)) %>%
  select(Method, Parameter, CV_RMSE, Improvement) %>%
  mutate(Improvement = ifelse(Method == "Random Forest", "-", Improvement)) %>%
  knitr::kable(caption = cap_tab1)
  
## ----------------------------------------------------------------------------
## --------------------- A look on the single tree ----------------------------
## ----------------------------------------------------------------------------
tree_mod = rpart(Price~., data = train, cp = 0.01)
fancyRpartPlot(tree_mod, sub = "")
## ----------------------------------------------------------------------------
## --------------------- Final Model: Random Forest ---------------------------
## ----------------------------------------------------------------------------
set.seed(2021504)
rf_mod = randomForest(Price ~ ., data = train, mtry = 3,
                      ntree = 100, importance = TRUE)
rownames(rf_mod$importance) = c("Transaction Date", "House Age",
                                "Distance to The Nearest MRT Station",
                                "Number of Convenience Stores", "Latitude",
                                "Longtitude")
importance(rf_mod) %>%
  as_tibble() %>%
  mutate(variable = dimnames(importance(rf_mod))[[1]]) %>%
  relocate(variable) %>%
  arrange(desc(`%IncMSE`)) %>%
  mutate(`%IncMSE` = sprintf('%.2f%%', `%IncMSE`),
         IncNodePurity = sprintf('%.2f', IncNodePurity)) %>%
  rename(`Variable Name` = variable,
         `Percent Increase in Mean Squared Error` = `%IncMSE`,
         `Increase in Node Purity` = IncNodePurity)
colnames(rf_mod$importance) = c("% Increase in MSE",
                                "Increase in Purity")
varImpPlot(rf_mod, main = "Random Forest: Importance of Variable")
HousePrice = function(x){
  results = predict(rf_mod, newdata = x)
  return(list(`Expected Price` = results, documentation = summary(rf_mod)))
}
## ----------------------------------------------------------------------------
## --------------------------------- Appendix ---------------------------------
## ----------------------------------------------------------------------------
knn_tab = results_KNN %>%
  group_by(k) %>%
  summarize(`KNN RMSE` = mean(RMSE)) %>%
  arrange(`KNN RMSE`) %>%
  mutate(k = as.character(k), `KNN RMSE` = sprintf("%.3f", `KNN RMSE`))

tree_tab = results_tree %>%
  group_by(cp) %>%
  summarize(`TREE RMSE` = mean(RMSE)) %>%
  arrange(`TREE RMSE`) %>%
  mutate(cp = as.character(cp), `TREE RMSE` = sprintf("%.3f", `TREE RMSE`)) %>%
  bind_rows(tibble(cp = rep("-", 7), `TREE RMSE` = rep("-", 7)))

rf_tab = results_rf %>%
  group_by(mtry) %>%
  summarize(`Random Forest RMSE` = mean(RMSE)) %>%
  arrange(`Random Forest RMSE`) %>%
  mutate(mtry = as.character(mtry),
         `Random Forest RMSE` = sprintf("%.3f", `Random Forest RMSE`)) %>%
  bind_rows(tibble(mtry = rep("-", 7), `Random Forest RMSE` = rep("-", 7)))

knn_tab %>%
  bind_cols(tree_tab) %>%
  bind_cols(rf_tab)
