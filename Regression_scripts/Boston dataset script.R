
# Script for training and testing regression models on Boston dataset

# Each code line should be run with `CTRL + ENTER`.

# R Packages ----------------------------------------

source("Packages.R") # Any error? Check `Packages.R` script for instructions.

# Boston dataset --------------

dataset=read.xlsx('Regression_datasets/Boston_dataset.xlsx')

# Data wrangling and preprocessing

# Categorical

factor_variable_position <- 4

boston_dataset <- dataset %>% mutate_at(., vars(factor_variable_position), ~ as.factor(.))

# Scaling the continous variables

preProcess_scale_model <- preProcess(boston_dataset, method=c('center', 'scale'))

boston_dataset <- predict(preProcess_scale_model, newdata = boston_dataset)



#  Set the total number of replications 

R <- 100

#  Initialize the test error matrix

mse <- matrix(0, ncol = 15, nrow = R)

for (r in 1:R) {
  
  # Create the training and test datasets for boston_dataset
  
  # Step 1: Get row numbers for the training data
  trainRowNumbers <- createDataPartition(boston_dataset$y, p=0.8,   list=FALSE)
  
  # Step 2: Create the training  dataset
  train_bostonData<- boston_dataset[trainRowNumbers,]
  
  # Step 3: Create the test dataset
  test_bostonData <- boston_dataset[-trainRowNumbers,]
  
  # Store for later use.
  xtrain <-  train_bostonData[-ncol(train_bostonData)]
  ytrain <-  train_bostonData$y
  xtest <- test_bostonData[-ncol(test_bostonData)]
  ytest <- test_bostonData$y
  ntr <- nrow(train_bostonData)
  nte <- nrow(test_bostonData)
  
  # Models training
  
  #  1. Multivariate Adaptive Regression Splines (MARS)
  
  mars.model <- train(y~., data=train_bostonData, method = "earth", trControl = trainControl(method = "cv", number = 10, returnResamp = "all"), tuneGrid = data.frame(degree = 1, nprune = (2:4)*2))
  
  yhat.mars  <- predict(mars.model, xtest)
  
  mse[r,1] <- mean((ytest-yhat.mars)^2)
  
  # 2. Support Vector Machine (SVM)
  
  svm.model <- train(y ~ ., data=train_bostonData, method = "svmLinear",trControl = trainControl(method = "cv", number = 10, returnResamp = "all"), tuneGrid = data.frame(.C = c(.25, .5, 1)))
  
  yhat.svm  <- predict(svm.model, xtest)
  
  mse[r,2] <- mean((ytest-yhat.svm)^2)
  
  # 3. K-Nearest Neighbors (KNN)
  
  knn.model <- train(y~., data=train_bostonData, method = "knn",trControl = trainControl(method = "cv", number = 10, returnResamp = "all"))
  
  yhat.knn  <- predict(knn.model, xtest)
  
  mse[r,3] <- mean((ytest - yhat.knn)^2)
  
  # 4.  Ordinary Least Squares regression (OLS)
  
  lm.model <- train(y~., data=train_bostonData, method='lm', trControl=trainControl(method = "cv", number = 10, returnResamp = "all"))
  
  yhat.lm  <- predict(lm.model, xtest)
  
  mse[r,4] <- mean((ytest - yhat.lm)^2)
  
  # 5. Elastic net regularization
  
  enet.model <- train(y~., data=train_bostonData, method = "enet", trControl = trainControl(method = "cv", number = 10, returnResamp = "all"))
  
  yhat.enet  <- predict(enet.model, xtest)
  
  mse[r,5] <- mean((ytest-yhat.enet)^2)
  
  # 6. Ridge regression
  
  ridge.model <- train(y~., data=train_bostonData, method = "ridge", trControl = trainControl(method = "cv", number = 10, returnResamp = "all"))
  
  yhat.ridge  <- predict(ridge.model, xtest)
  
  mse[r,6] <- mean((ytest - yhat.ridge)^2)
  
  # 7. Least Absolute Shrinkage and Selection Operator(LASSO)
  
  lasso.model <- train(y~., data=train_bostonData, method = "lasso", trControl = trainControl(method = "cv", number = 10, returnResamp = "all"))
  
  yhat.lasso  <- predict(lasso.model, xtest)
  
  mse[r,7] <- mean((ytest-yhat.lasso)^2)
  
  # Random forest
  
  rf.model <- train(y~., data=train_bostonData, method = "rf", trControl = trainControl(method = "cv", number = 10, returnResamp = "all"), ntree = 20, importance = TRUE)
  
  yhat.rf  <- predict(rf.model, xtest)
  
  mse[r,8] <- mean((ytest - yhat.rf)^2)
  
  # 9. Gradient Boosting Machines (GBM)
  
  gbm.model <- train(y~., data=train_bostonData,method = "gbm", trControl = trainControl(method = "cv", number = 10, returnResamp = "all"), tuneGrid =expand.grid(interaction.depth = 1:2, shrinkage = .1, n.trees = c(10, 50, 100), n.minobsinnode = 10), verbose = FALSE)
  
  yhat.gbm  <- predict(gbm.model, xtest)
  
  mse[r,9] <- mean((ytest - yhat.gbm)^2) 
  
  # 10. Generalized Linear model via penalized maximum likelihood
  
  glmnet.model <- train(y ~ ., data=train_bostonData, 
                        method = "glmnet", trControl = trainControl(method = "cv", number = 10, returnResamp = "all"))
  yhat.glmnet  <- predict(glmnet.model, xtest)
  
  mse[r,10] <- mean((ytest - yhat.glmnet)^2)
  
  # 11. Artificial Neural Network 
  
  nnet.model <- train(y~., data = train_bostonData,  method = "nnet",  trControl = trainControl(method = "cv", number = 10, returnResamp = "all"), trace = FALSE, linout = TRUE)
  yhat.nnet  <- predict(nnet.model, xtest)
  mse.nnet <- mean((ytest-yhat.nnet)^2)
  
  mse[r,11] <- mse.nnet
  
  # 12. Relaxed Lasso
  
  relaxo.model <- train(y~.,train_bostonData, method = "relaxo", trControl = trainControl(method = "cv", number = 10, returnResamp = "all"))
  
  yhat.relaxo  <- predict(relaxo.model, xtest)
  
  mse[r,12] <- mean((ytest - yhat.relaxo)^2)
  
  # 13. Robust linear regression
  
  robustlm.model <- train(y~., train_bostonData, method = "rlm", trControl = trainControl(method = "cv", number = 10, returnResamp = "all"), tuneGrid = data.frame(intercept = TRUE, psi = c("psi.huber", "psi.hampel", "psi.bisquare")))
  
  yhat.robustlm  <- predict(robustlm.model, xtest)
  
  mse[r,13] <- mean((ytest - yhat.robustlm)^2)
  
  # 14. Partial least squares regression
  
  pls.model <- train(y~., train_bostonData, method = "pls", trControl = trainControl(method = "cv", number = 10, returnResamp = "all"))
  yhat.pls  <- predict(pls.model, xtest)
  
  mse[r,14] <- mean((ytest - yhat.pls)^2)
  
  # 15. Boosted linear regression
  
  boostedlm.model <- train(y~., train_bostonData, method = "BstLm", trControl = trainControl(method = "cv", number = 10, returnResamp = "all"))
  
  yhat.boostedlm  <- predict(boostedlm.model, xtest)
  
  mse[r,15] <- mean((ytest - yhat.boostedlm)^2)
  
  
  if (r %% 25 == 0) cat('\n', paste(round(100 * r / R, 0), '%', 'completed\n'))
}


# From matrix data type to tibble

colnames(mse) <- c("MARS", "SVM", "KNN", "MLR", "enet", "Ridge", "Lasso", "rForest", "GBM", "glmnet", "nnet", "Relaxo", "RLM", "PLS", "BstLm")

mse <- as_tibble(mse)

# Boxplot for the test errors of 15 regression models --------------

AVTE_boston_dataset <- mse %>% pivot_longer(1:15, names_to = "Model", values_to = "AVTE") %>%  ggplot(., aes(reorder(x = Model, AVTE, FUN = mean), y = AVTE, fill = Model)) + geom_boxplot(show.legend = F) + theme_bw() + labs(y = 'Test errors', x = 'Learning methods') + theme(axis.title.x = element_text(face = 'bold', size = 12), axis.title.y = element_text(face = 'bold', size = 12), axis.text.x = element_text(angle = 50, vjust = 0.5, face = 'bold'))                                            

# Save figure

ggsave("Regression_figures_test_errors/AVTE_boston_dataset.png", width = 6.74, height =4.54)


# Average prediction error ---------------------------

avg.mse <- apply(mse, 2, mean)

AVPE_boston_dataset <- enframe(avg.mse, name = "Model", value = "AVTE") %>% ggplot(., aes(x = reorder(1:15, AVTE, mean), y = AVTE)) +
  geom_point() + ggrepel::geom_label_repel(aes(label = Model)) + theme_bw() +
  labs(y = "Average test error", x = "Learning methods") +
  theme(axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) 

# Save figure

ggsave("Regression_figures_test_errors/AVPE_boston_dataset.png", width = 6.74, height =4.54)


# mean, median, and standard deviation test error of regression models-----------

boston_table <- mse %>% pivot_longer(1:15, names_to = "Model", values_to = "Value") %>%
  group_by(Model) %>%
  summarise(avg.mse = round(mean(Value), 4), med.mse = round(median(Value), 4), std.mse = round(sd(Value), 4))


# mean, median, and standard deviation test error of regression models ranking-----------


boston_table.rk <- boston_table %>% mutate(avg.mse.rk = rank(avg.mse, ties.method = "average"), med.mse.rk = rank(med.mse, ties.method = "average"), std.mse.rk = rank(std.mse, ties.method = "average"))

write_csv(boston_table.rk, "Boston rank.csv")

# End