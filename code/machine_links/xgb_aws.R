load("~/Dropbox/Work/UChicago/RA/RA Hornbeck/CMF Patents/Terence Working Folders/output/linkage_model.RData")

library(tidyverse)
library(caret)

train_control <- trainControl(method = "cv", 
                              number = 5,
                              allowParallel = TRUE)

set.seed(60615)
system.time({
  xgb_2 <- train(match ~ ., 
                 data = model_train_2, 
                 method = "xgbTree",
                 trControl = train_control,
                 metric = "Kappa")
})

save.image(file = "~/Dropbox/Work/UChicago/RA/RA Hornbeck/CMF Patents/Terence Working Folders/output/linkage_model_xgb.RData")

set.seed(60615)
system.time({
  xgb_3 <- train(match ~ ., 
                 data = model_train_3, 
                 method = "xgbTree",
                 trControl = train_control,
                 metric = "Kappa")
})

save.image(file = "~/Dropbox/Work/UChicago/RA/RA Hornbeck/CMF Patents/Terence Working Folders/output/linkage_model_xgb_2.RData")

preds_xgb_2 <- predict(xgb_2, 
                       newdata = model_validation_2, 
                       type = "class")

preds_xgb_3 <- predict(xgb_3, 
                       newdata = model_validation_3, 
                       type = "class")

cm_xgb_validation_2 <- table("Prediction" = preds_xgb_2, 
                             "Reference" = model_validation_2$match)

cm_xgb_validation_3 <- table("Prediction" = preds_xgb_3, 
                             "Reference" = model_validation_3$match)

if (kappa(cm_xgb_validation_2) >= kappa(cm_xgb_validation_3)){
  best_model <- 2
  best_tune <- xgb_2$bestTune
} else {
  best_model <- 3
  best_tune <- xgb_3$bestTune
}

train_control_final <- trainControl(method = "none", 
                                    allowParallel = TRUE)

tune_grid <- expand.grid(
  nrounds = best_tune$nrounds,
  eta = best_tune$eta,
  max_depth = best_tune$max_depth,
  gamma = best_tune$gamma,
  colsample_bytree = best_tune$colsample_bytree,
  min_child_weight = best_tune$min_child_weight,
  subsample = best_tune$subsample
)

set.seed(60615)
system.time({
  xgb_final <- train(match ~ ., 
                     data = big_train, 
                     method = "xgbTree",
                     trControl = train_control_final,
                     tuneGrid = tune_grid,
                     metric = "Kappa",
  )
})

save.image(file = "~/Dropbox/Work/UChicago/RA/RA Hornbeck/CMF Patents/Terence Working Folders/output/linkage_model_xgb_3.RData")

if (best_model == 2){
  preds_xgb_final <- predict(xgb_final, 
                             newdata = model_test_2, 
                             type = "class")
  cm_xgb_test_final <- table("Prediction" = preds_xgb_final, 
                             "Reference" = model_test_2$match)
} else {
  preds_xgb_final <- predict(xgb_final, 
                             newdata = model_test_3, 
                             type = "class")
  cm_xgb_test_final <- table("Prediction" = preds_xgb_final, 
                             "Reference" = model_test_3$match)
}

tpr_xgb <- tpr(cm_xgb_test_final)
tnr_xgb <- tnr(cm_xgb_test_final)
ppv_xgb <- ppv(cm_xgb_test_final)
f_xgb <- f_score(cm_xgb_test_final)
kappa_xgb <- kappa(cm_xgb_test_final)

save.image(file = "~/Dropbox/Work/UChicago/RA/RA Hornbeck/CMF Patents/Terence Working Folders/output/linkage_model_xgb_4.RData")