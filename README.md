# CaretMisc
Miscellaneous Functions To Make Caret Easier in R

Installation instructions:
``` R
install.packages("devtools") 
devtools::install_github("gdmcdonald/CaretMisc")
```



So you can do things like this:



Make a list of your trained caret models, and then evaluate them against each other on the train and test sets.

``` R
allModels<- list(
  "Ridge" = ames_ridge,
  "Lasso" = ames_lasso,
  "EN" = ames_en,
  "PLSR" = ames_plsr, 
  "PCR" = ames_pcr,
  "MARS" = ames_mars,
  "LM all" = ames_lm_all,
  "RF" = ames_rf, 
  "KNN" = ames_knn, 
  "GBM" = ames_gbm, 
  "XGB" = ames_xgb_4caret
)
p <- compareModels(model_list = allModels,
                   test_data = test_set,
                   model_order = "RMSE")
p
```

![demo_img](./demo_img.png)



Or look at the values underlying the graph:



``` R
results<-compareModelsDf(model_list = allModels,
                         test_data = test_set,
                         model_order = "RMSE")

results %>% 
  filter(test_train == "2. test",
         metric == "RMSE") %>% 
  arrange(value)
```



``` output
# A tibble: 11 x 5
   metric model   value test_train Resample
   <chr>  <ord>   <dbl> <chr>      <chr>   
 1 RMSE   XGB    0.0472 2. test    test    
 2 RMSE   GBM    0.0517 2. test    test    
 3 RMSE   PLSR   0.0554 2. test    test    
 4 RMSE   LM all 0.0560 2. test    test    
 5 RMSE   RF     0.0563 2. test    test    
 6 RMSE   Ridge  0.0572 2. test    test    
 7 RMSE   EN     0.0572 2. test    test    
 8 RMSE   MARS   0.0586 2. test    test    
 9 RMSE   PCR    0.0603 2. test    test    
10 RMSE   KNN    0.0710 2. test    test    
11 RMSE   Lasso  0.142  2. test    test    
```

