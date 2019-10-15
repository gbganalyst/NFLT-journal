
# Script for training and testing models on multi-class white wine quality

# Each code line should be run with `CTRL + ENTER`.

# R Packages ----------------------------------------

source("Packages.R") # Any error? Check `Packages.R` script for instructions.

# White wine quality -----------------------------

dataset <- read.xlsx("Classification_datasets/Multi_class/white_winequality_dataset.xlsx", sheet = 1)


# Data wrangling and preprocessing

# Due to the small group in the target variable (y), QDA could not work. Therefore, we regroup the target as follows:

white_winequality_dataset <- dataset %>% mutate(
    y = as_factor(case_when(
    y >= 8 ~ "Excellent",
    y >= 5 ~ "Good",
    TRUE ~ "Poor"
  )))

# Scaling the continous variables

preProcess_scale_model <- preProcess(white_winequality_dataset, method = c("center", "scale"))

white_winequality_dataset <- predict(preProcess_scale_model, newdata = white_winequality_dataset)


#  Set the total number of replications 

R <- 100

#  Initialize the test error matrix

err <- matrix(0, ncol = 15, nrow = R)

for (r in 1:R) {
  
  # Create the training and test datasets 
  
  split  <- sample(nrow(white_winequality_dataset), nrow(white_winequality_dataset) * 0.8)
  
  # Create the training  dataset
  train_white_winequalityData <- white_winequality_dataset[split, ]
  
  # Create the test dataset
  test_white_winequalityData <- white_winequality_dataset[-split, ]
  
  # For later use
  
  xtrain <- train_white_winequalityData[-ncol(train_white_winequalityData)]
  
  ytrain <- train_white_winequalityData$y
  
  xtest <- test_white_winequalityData[-ncol(test_white_winequalityData)]
  
  ytest <- test_white_winequalityData$y
  
  ntr <- nrow(train_white_winequalityData) # Number of training set
  
  nte <- nrow(test_white_winequalityData) # Number of test set
  
  # Models training
  
  #  1. LDA
  
  lda.model <- train(y ~ ., data = train_white_winequalityData, method = "lda", trControl = trainControl(method = "cv", number = 10, returnResamp = "all", classProbs = TRUE, summaryFunction = multiClassSummary), metric = "logLoss")
  
  yhat.lda <- predict(lda.model, xtest)
  
  err[r, 1] <- 1 - accuracy(ytest, yhat.lda)
  
  # 2. SVM
  
  svm.model <- train(y ~ ., data = train_white_winequalityData, method = "svmLinear2", trControl = trainControl(method = "cv", number = 10, returnResamp = "all", classProbs = TRUE), tuneGrid = data.frame(cost = c(.25, .5, 1)))
  
  yhat.svm <- predict(svm.model, xtest)
  
  err[r, 2] <- 1 - accuracy(ytest, yhat.svm)
  
  # 3. Decision tree
  
  cart.model <- train(y ~ ., data = train_white_winequalityData, method = "rpart", trControl = trainControl(method = "cv", number = 10, returnResamp = "all"))
  
  yhat.cart <- predict(cart.model, xtest)
  
  err[r, 3] <- 1 - accuracy(ytest, yhat.cart)
  
  # 4. Randome forest
  
  forest.model <- train(y ~ ., data = train_white_winequalityData, method = "rf", trControl = trainControl(method = "cv", number = 10, returnResamp = "all", classProbs = TRUE, summaryFunction =multiClassSummary, seeds = vector(mode = "list", length = nrow(train_white_winequalityData) + 1) %>% lapply(., function(x) 1:20)), metric = "logLoss", ntree = 20, importance = TRUE)
  
  yhat.forest <- predict(forest.model, xtest)
  
  err[r, 4] <- 1 - accuracy(ytest, yhat.forest)
  
  # 5. Gaussian plogLossess
  
  gausspr.model <- gausspr(y ~ ., data = train_white_winequalityData)
  
  
  yhat.gausspr <- predict(gausspr.model, xtest, type = "response")
  
  err[r, 5] <- 1 - accuracy(ytest, yhat.gausspr)
  
  # 6. KNN model
  
  knn.model <- train(y ~ ., data = train_white_winequalityData, method = "knn", trControl = trainControl(method = "cv", number = 10), tuneLength = 10)
  
  yhat.knn <- predict(knn.model, newdata = xtest)
  
  err[r, 6] <- 1 - accuracy(ytest, yhat.knn)
  
  # 7. Mixture Discriminant Analysis (MDA)
  
  mda.model <- train(y ~ ., data = train_white_winequalityData, method = "mda", trControl = trainControl(method = "cv", number = 10, returnResamp = "all", classProbs = TRUE, summaryFunction = multiClassSummary, seeds = vector(mode = "list", length = nrow(train_white_winequalityData) + 1) %>% lapply(., function(x) 1:20)), metric = "logLoss")
  
  yhat.mda  <- predict(mda.model, xtest)
  
  err[r,7] <-  1 - accuracy(ytest, yhat.mda)
  
  # 8. Artificial neural network
  
  nnet.model <- train(y ~ ., data = train_white_winequalityData, method = "nnet", trControl = trainControl(method = "cv", number = 10, returnResamp = "all"), trace = FALSE)
  
  yhat.nnet <- predict(nnet.model, xtest)
  
  err[r, 8] <- 1 - accuracy(ytest, yhat.nnet)
  
  # 9. Multinomial logistic regression
  
  multinom.model <- train(y ~ ., data = train_white_winequalityData, method = "multinom", trControl = trainControl(method = "cv", number = 10, returnResamp = "all", classProbs = TRUE, summaryFunction = multiClassSummary), metric = "logLoss", trace = FALSE)
  
  yhat.multinom<- predict(multinom.model, newdata = xtest) 
  
  err[r,9] <- 1 - accuracy(ytest, yhat.multinom)
  
  # 10. Adabag model
  
  bagging.model <- train(y ~ ., data = train_white_winequalityData, method = "AdaBag", tuneGrid = expand.grid(mfinal = (1:3) * 3, maxdepth = c(1, 3)), trControl = trainControl(method = "cv", number = 10, returnResamp = "all", classProbs = TRUE, summaryFunction = multiClassSummary, seeds = vector(mode = "list", length = nrow(train_white_winequalityData) + 1) %>% lapply(., function(x) 1:20)), metric = "logLoss")
  
  yhat.bagging <- predict(bagging.model, xtest)
  
  err[r, 10] <- 1 - accuracy(ytest, yhat.bagging)
  
  # 11. Naive bayes
  
  naiveBayes.model <- train(y ~ ., data = train_white_winequalityData, method = "naive_bayes", trControl = trainControl(method = "cv", number = 10, returnResamp = "all", classProbs = TRUE, summaryFunction = multiClassSummary), metric = "logLoss")
  
  yhat.naiveBayes <- predict(naiveBayes.model, xtest)
  
  err[r, 11] <- 1 - accuracy(ytest, yhat.naiveBayes)
  
  # 12. Multivariate adaptive regression splines (MARS)
  mars.model <- train(y ~ ., data = train_white_winequalityData, method = "earth", trControl = trainControl(method = "cv", number = 10, returnResamp = "all", classProbs = TRUE), tuneGrid = data.frame(degree = 1, nprune = (2:4) * 2))
  
  yhat.mars <- predict(mars.model, xtest)
  
  err[r, 12] <- 1 - accuracy(ytest, yhat.mars)
  
  # 13. Quadratic discriminant analysis
  
  qda.model <- train(y ~ .,
                     data = train_white_winequalityData,
                     method = "qda", trControl = trainControl(method = "cv", number = 10, returnResamp = "all", classProbs = TRUE, summaryFunction = multiClassSummary), metric = "logLoss")
  
  yhat.qda <- predict(qda.model, xtest)
  
  err[r, 13] <- 1 - accuracy(ytest, yhat.qda)
  
  
  # 14. Regularized discriminant analysis (RDA)
  rda.model <- train(y ~ ., data = train_white_winequalityData, method = "rda", trControl = cctrl1 <- trainControl(method = "cv", number = 10, returnResamp = "all", classProbs = TRUE, summaryFunction = multiClassSummary, seeds = vector(mode = "list", length = nrow(train_white_winequalityData) + 1) %>% lapply(., function(x) 1:20)), metric = "logLoss")
  
  yhat.rda <- predict(rda.model, xtest)
  
  err[r, 14] <- 1 - accuracy(ytest, yhat.rda)
  
  # 15. glmnet model
  
  glmnet.model <- train(y ~ ., data = train_white_winequalityData, method = "glmnet", trControl = trainControl(method = "cv", number = 10, returnResamp = "all", classProbs = TRUE, summaryFunction = multiClassSummary), metric = "logLoss", tuneGrid = expand.grid(.alpha = seq(.05, 1, length = 15), .lambda = c((1:5) / 10)))
  
  yhat.glmnet <- predict(glmnet.model, xtest)
  
  err[r, 15] <- 1 - accuracy(ytest, yhat.glmnet)
  
  if (r %% 25 == 0) cat('\n', paste(round(100 * r / R, 0), '%', 'completed\n'))
}


# From matrix data type to tibble

colnames(err) <- c("LDA", "SVM", "CART", "rForest", "Gauss", "kNN", "MDA", "NNET", "Multinom", "Bagging", "naiveBayes", "MARS", "QDA", "RDA", "glmnet")

err <- as_tibble(err)


# Boxplot for test errors of 15 classifiers --------------

AVTE_white_winequality_dataset <- err %>% pivot_longer(1:15, names_to = "Model", values_to = "AVTE") %>%
  ggplot(., aes(x = Model, y = AVTE, fill = Model)) + geom_boxplot(show.legend = F) + theme_bw() + labs(y = "Test error", x = "Method (Classifier)", title = "Misclassification error of each classifier", subtitle = "over 100 hold out sub sample", caption = "Source: White wine quality dataset") + theme(axis.title.x = element_text(face = "bold", size = 12), axis.title.y = element_text(face = "bold", size = 12), axis.text.x = element_text(angle = 50, vjust = 0.5, face = "bold"))                                      

# Save figure

ggsave("Classification_figures_test_errors/Multi_class/AVTE_white_winequality_dataset.png", width = 6.74, height = 4.54)


# Average prediction error (AVPE) ------------------------

avg.err <- apply(err, 2, mean)

AVPE_white_winequality_dataset <- enframe(avg.err, name = "Model", value = "AVTE") %>% ggplot(., aes(x = 1:15, y = AVTE)) + geom_point() +
  ggrepel::geom_label_repel(aes(label = Model)) + theme_bw() + labs(y = "Average test error", x = "Method (Classifier)", title = "Average misclassification error of each classifier", subtitle = "over 100 hold out sub sample", caption = "Source: White wine quality dataset") + theme(
    axis.title.x = element_text(face = "bold", size = 12), axis.title.y = element_text(face = "bold", size = 12),
    axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Save figure

ggsave("Classification_figures_test_errors/Multi_class/AVPE_white_winequality_dataset.png", width = 6.74, height = 4.54)

# mean, median, and standard deviation test error of classifiers-----------

white_winequality_table <- err %>% pivot_longer(1:15, names_to = "Model", values_to = "Value") %>%
  group_by(Model) %>%
  summarise(avg.err = round(mean(Value), 4), med.err = round(median(Value), 4), std.err = round(sd(Value), 4))


# mean, median, and standard deviation test error of classifiers ranking-----------

white_winequality_table.rk <- white_winequality_table %>% mutate(avg.err.rk=rank(avg.err,ties.method = "average"), med.err.rk=rank(med.err,ties.method = "average"), std.err.rk=rank(std.err,ties.method = "average"))

write_csv(white_winequality_table.rk, "white_winequality_table.rk.csv")

# End