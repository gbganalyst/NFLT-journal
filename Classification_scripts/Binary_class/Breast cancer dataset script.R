
# Script for training and testing models on binary class breast dataset

# Each code line should be run with `CTRL + ENTER`.

# R Packages ----------------------------------------

source("Packages.R") # Any error? Check `Packages.R` script for instructions.

# Breast cancer dataset ------

dataset <- read.xlsx("Classification_datasets/Binary_class/breast_cancer_dataset.xlsx", sheet = 1)


# Data wrangling and preprocessing

# Categorical variables

factor_variable_position <- 10

breastcancer_dataset <- dataset %>% mutate_at(.,vars (factor_variable_position), ~as.factor(.))


# Scaling the continous variables

# No scaling since the variables have the same scale

#  Set the total number of replications 

R <- 100

#  Initialize the test error matrix

err <- matrix(0, ncol = 15, nrow = R)

for (r in 1:R) {
  
  # Create the training and test datasets 
  
  split  <- sample(nrow(breastcancer_dataset), nrow(breastcancer_dataset) * 0.8)
  
  # Create the training  dataset
  train_breastcancerData <- breastcancer_dataset[split, ]
  
  # Create the test dataset
  test_breastcancerData <- breastcancer_dataset[-split, ]
  
  # For later use
  
  xtrain <- train_breastcancerData[-ncol(train_breastcancerData)]
  
  ytrain <- train_breastcancerData$y
  
  xtest <- test_breastcancerData[-ncol(test_breastcancerData)]
  
  ytest <- test_breastcancerData$y
  
  ntr <- nrow(train_breastcancerData) # Number of training set
  
  nte <- nrow(test_breastcancerData) # Number of test set
  
  # Models training
  
  #  1. LDA
  
  lda.model <- train(y ~ ., data = train_breastcancerData, method = "lda", trControl = trainControl(method = "cv", number = 10, returnResamp = "all", classProbs = TRUE, summaryFunction = twoClassSummary), metric = "ROC")
  
  yhat.lda <- predict(lda.model, xtest)
  
  err[r, 1] <- 1 - accuracy(ytest, yhat.lda)
  
  # 2. SVM
  
  svm.model <- train(y ~ ., data = train_breastcancerData, method = "svmLinear2", trControl = trainControl(method = "cv", number = 10, returnResamp = "all", classProbs = TRUE), tuneGrid = data.frame(cost = c(.25, .5, 1)))
  
  yhat.svm <- predict(svm.model, xtest)
  
  err[r, 2] <- 1 - accuracy(ytest, yhat.svm)
  
  # 3. Decision tree
  
  cart.model <- train(y ~ ., data = train_breastcancerData, method = "rpart", trControl = trainControl(method = "cv", number = 10, returnResamp = "all"))
  
  yhat.cart <- predict(cart.model, xtest)
  
  err[r, 3] <- 1 - accuracy(ytest, yhat.cart)
  
  # 4. Randome forest
  
  forest.model <- train(y ~ ., data = train_breastcancerData, method = "rf", trControl = trainControl(method = "cv", number = 10, returnResamp = "all", classProbs = TRUE, summaryFunction = twoClassSummary, seeds = vector(mode = "list", length = nrow(train_breastcancerData) + 1) %>% lapply(., function(x) 1:20)), metric = "ROC", ntree = 20, importance = TRUE)
  
  yhat.forest <- predict(forest.model, xtest)
  
  err[r, 4] <- 1 - accuracy(ytest, yhat.forest)
  
  # 5. Gaussian process
  
  gausspr.model <- gausspr(y ~ ., data = train_breastcancerData)
  
  
  yhat.gausspr <- predict(gausspr.model, xtest, type = "response")
  
  err[r, 5] <- 1 - accuracy(ytest, yhat.gausspr)
  
  # 6. KNN model
  
  knn.model <- train(y ~ ., data = train_breastcancerData, method = "knn", trControl = trainControl(method = "cv", number = 10), tuneLength = 10)
  
  yhat.knn <- predict(knn.model, newdata = xtest)
  
  err[r, 6] <- 1 - accuracy(ytest, yhat.knn)
  
  # 7. Adaboost model
  
  boost.model <- train(y ~ ., data = train_breastcancerData, method = "adaboost", trControl = trainControl(
    method = "cv", number = 10, returnResamp = "all", classProbs = TRUE, summaryFunction = twoClassSummary, seeds = vector(mode = "list", length = nrow(train_breastcancerData) + 1) %>%
      lapply(., function(x) 1:20)), metric = "ROC")
  
  yhat.boost <- predict(boost.model, xtest)
  
  err[r, 7] <- 1 - accuracy(ytest, yhat.boost)
  
  
  # 8. Artificial neural network
  
  nnet.model <- train(y ~ ., data = train_breastcancerData, method = "nnet", trControl = trainControl(method = "cv", number = 10, returnResamp = "all"), trace = FALSE)
  
  yhat.nnet <- predict(nnet.model, xtest)
  
  err[r, 8] <- 1 - accuracy(ytest, yhat.nnet)
  
  # 9. Logistic model
  
  logit.model <- train(y ~ ., data = train_breastcancerData, method = "LogitBoost", trControl = trainControl(method = "cv", number = 10, returnResamp = "all", classProbs = TRUE, summaryFunction = twoClassSummary), metric = "ROC")
  
  yhat.logit <- predict(logit.model, xtest)
  
  err[r, 9] <- 1 - accuracy(ytest, yhat.logit)
  
  # 10. Adabag model
  
  bagging.model <- train(y ~ ., data = train_breastcancerData, method = "AdaBag", tuneGrid = expand.grid(mfinal = (1:3) * 3, maxdepth = c(1, 3)), trControl = trainControl(method = "cv", number = 10, returnResamp = "all", classProbs = TRUE, summaryFunction = twoClassSummary, seeds = vector(mode = "list", length = nrow(train_breastcancerData) + 1) %>% lapply(., function(x) 1:20)), metric = "ROC")
  
  yhat.bagging <- predict(bagging.model, xtest)
  
  err[r, 10] <- 1 - accuracy(ytest, yhat.bagging)
  
  # 11. Naive bayes
  
  naiveBayes.model <- train(y ~ ., data = train_breastcancerData, method = "naive_bayes", trControl = trainControl(method = "cv", number = 10, returnResamp = "all", classProbs = TRUE, summaryFunction = twoClassSummary), metric = "ROC")
  
  yhat.naiveBayes <- predict(naiveBayes.model, xtest)
  
  err[r, 11] <- 1 - accuracy(ytest, yhat.naiveBayes)
  
  # 12. Multivariate adaptive spline (MARS)
  mars.model <- train(y ~ ., data = train_breastcancerData, method = "earth", trControl = trainControl(method = "cv", number = 10, returnResamp = "all", classProbs = TRUE), tuneGrid = data.frame(degree = 1, nprune = (2:4) * 2))
  
  yhat.mars <- predict(mars.model, xtest)
  
  err[r, 12] <- 1 - accuracy(ytest, yhat.mars)
  
  # 13. Quadratic discriminant analysis
  
  qda.model <- train(y ~ .,
                     data = train_breastcancerData,
                     method = "qda", trControl = trainControl(method = "cv", number = 3, returnResamp = "all", classProbs = TRUE, summaryFunction = twoClassSummary), metric = "ROC")
  
  yhat.qda <- predict(qda.model, xtest)
  
  err[r, 13] <- 1 - accuracy(ytest, yhat.qda)
  
  
  # 14. Regularized discriminant analysis (RDA)
  rda.model <- rda(y~.,data=train_breastcancerData)
  
  yhat.rda <- predict(rda.model, xtest)
  
  err[r, 14] <- 1 - accuracy(ytest, yhat.rda)
  
  # 15. glmnet model
  
  glmnet.model <- train(y ~ ., data = train_breastcancerData, method = "glmnet", trControl = trainControl(method = "cv", number = 3, returnResamp = "all", classProbs = TRUE, summaryFunction = twoClassSummary), metric = "ROC", tuneGrid = expand.grid(.alpha = seq(.05, 1, length = 15), .lambda = c((1:5) / 10)))
  
  yhat.glmnet <- predict(glmnet.model, xtest)
  
  err[r, 15] <- 1 - accuracy(ytest, yhat.glmnet)
  
  if (r %% 25 == 0) cat('\n', paste(round(100 * r / R, 0), '%', 'completed\n'))
}


# From matrix data type to tibble

colnames(err) <- c("LDA", "SVM", "CART", "rForest", "Gauss", "kNN", "Boost", "NNET", "Logit", "Bagging", "naiveBayes", "MARS", "QDA", "RDA", "glmnet")


err <- as_tibble(err)

# Boxplot for test errors of 15 classifiers --------------

AVTE_breastcancerdataset <-  err %>% pivot_longer(1:15, names_to = "Model", values_to = "AVTE") %>% ggplot(.,aes(x= reorder(Model, AVTE, FUN = mean), y=AVTE, fill=Model)) +geom_boxplot(show.legend = F)+theme_bw()+labs(y='Test errors', x='Learning methods (Classifiers)')+theme(axis.title.x = element_text(face = 'bold',size = 12),axis.title.y = element_text(face = 'bold',size = 12),axis.text.x = element_text(angle = 50, vjust = 0.5, face = "bold"))

# Save figure

ggsave("Classification_figures_test_errors/Binary_class/AVTE_breastcancerdataset.png",width = 6.74, height =4.54)

# Average prediction error (AVPE) ------------------------

avg.err <- apply(err, 2, mean)

AVPE_breastcancerdataset <- enframe(avg.err, name = "Model", value = "AVTE") %>% ggplot(., aes(x = reorder (1:15, AVTE, mean), y = AVTE)) + geom_point() +
  ggrepel::geom_label_repel(aes(label = Model)) + theme_bw() + labs(y = "Average test error", x = "Learning methods (Classifiers)") + theme(
    axis.title.x = element_text(face = "bold", size = 12), axis.title.y = element_text(face = "bold", size = 12),
    axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Save figure

ggsave("Classification_figures_test_errors/Binary_class/AVPE_breastcancerdataset.png")

# mean, median, and standard deviation test error of classifiers-----------

breastcancer_table <- err %>%
  pivot_longer(1:15, names_to = "Model", values_to = "Value") %>%
  group_by(Model) %>%
  summarise(avg.err = round(mean(Value), 4), med.err = round(median(Value), 4), std.err = round(sd(Value), 4))


# mean, median, and standard deviation test error of classifiers ranking-----------

breastcancer_table.rk <- breastcancer_table %>% mutate(avg.err.rk = rank(avg.err, ties.method = "average"), med.err.rk = rank(med.err, ties.method = "average"), std.err.rk = rank(std.err, ties.method = "average"))

write_csv(breastcancer_table.rk, "breastcancer_table.rk.csv")

# End