
# Script for training and testing models on multi-class Seeds dataset

# Each code line should be run with `CTRL + ENTER`.

# R Packages ----------------------------------------

source("Packages.R") # Any error? Check `Packages.R` script for instructions.

# Seeds dataset -----------------------------

dataset <- read.xlsx("Classification_datasets/Multi_class/seeds_dataset.xlsx", sheet = 1)

# Recoding the label 

seed_dataset <- dataset %>% mutate(y = as_factor(case_when(
  y == 1 ~ "Kama",
  y == 2 ~ "Rosa",
  y == 3 ~ "Canadian")))


# Data wrangling and preprocessing

# Scaling the continous variables

preProcess_scale_model <- preProcess(seed_dataset, method=c('center', 'scale'))

seed_dataset <- predict(preProcess_scale_model, newdata = seed_dataset)


#  Set the total number of replications 

R <- 100

#  Initialize the test error matrix

err <- matrix(0, ncol = 15, nrow = R)

for (r in 1:R) {
  
  # Create the training and test datasets 
  
  split  <- sample(nrow(seed_dataset), nrow(seed_dataset) * 0.8)
  
  # Create the training  dataset
  train_seedData <- seed_dataset[split, ]
  
  # Create the test dataset
  test_seedData <- seed_dataset[-split, ]
  
  # For later use
  
  xtrain <- train_seedData[-ncol(train_seedData)]
  
  ytrain <- train_seedData$y
  
  xtest <- test_seedData[-ncol(test_seedData)]
  
  ytest <- test_seedData$y
  
  ntr <- nrow(train_seedData) # Number of training set
  
  nte <- nrow(test_seedData) # Number of test set
  
  # Models training
  
  #  1. LDA
  
  lda.model <- train(y ~ ., data = train_seedData, method = "lda", trControl = trainControl(method = "cv", number = 10, returnResamp = "all", classProbs = TRUE, summaryFunction = multiClassSummary), metric = "logLoss")
  
  yhat.lda <- predict(lda.model, xtest)
  
  err[r, 1] <- 1 - accuracy(ytest, yhat.lda)
  
  # 2. SVM
  
  svm.model <- train(y ~ ., data = train_seedData, method = "svmLinear2", trControl = trainControl(method = "cv", number = 10, returnResamp = "all", classProbs = TRUE), tuneGrid = data.frame(cost = c(.25, .5, 1)))
  
  yhat.svm <- predict(svm.model, xtest)
  
  err[r, 2] <- 1 - accuracy(ytest, yhat.svm)
  
  # 3. Decision tree
  
  cart.model <- train(y ~ ., data = train_seedData, method = "rpart", trControl = trainControl(method = "cv", number = 10, returnResamp = "all"))
  
  yhat.cart <- predict(cart.model, xtest)
  
  err[r, 3] <- 1 - accuracy(ytest, yhat.cart)
  
  # 4. Randome forest
  
  forest.model <- train(y ~ ., data = train_seedData, method = "rf", trControl = trainControl(method = "cv", number = 10, returnResamp = "all", classProbs = TRUE, summaryFunction =multiClassSummary, seeds = vector(mode = "list", length = nrow(train_seedData) + 1) %>% lapply(., function(x) 1:20)), metric = "logLoss", ntree = 20, importance = TRUE)
  
  yhat.forest <- predict(forest.model, xtest)
  
  err[r, 4] <- 1 - accuracy(ytest, yhat.forest)
  
  # 5. Gaussian plogLossess
  
  gausspr.model <- gausspr(y ~ ., data = train_seedData)
  
  
  yhat.gausspr <- predict(gausspr.model, xtest, type = "response")
  
  err[r, 5] <- 1 - accuracy(ytest, yhat.gausspr)
  
  # 6. KNN model
  
  knn.model <- train(y ~ ., data = train_seedData, method = "knn", trControl = trainControl(method = "cv", number = 10), tuneLength = 10)
  
  yhat.knn <- predict(knn.model, newdata = xtest)
  
  err[r, 6] <- 1 - accuracy(ytest, yhat.knn)
  
  # 7. Mixture Discriminant Analysis (MDA)
  
  mda.model <- train(y ~ ., data = train_seedData, method = "mda", trControl = trainControl(method = "cv", number = 10, returnResamp = "all", classProbs = TRUE, summaryFunction = multiClassSummary, seeds = vector(mode = "list", length = nrow(train_seedData) + 1) %>% lapply(., function(x) 1:20)), metric = "logLoss")
  
  yhat.mda  <- predict(mda.model, xtest)
  
  err[r,7] <-  1 - accuracy(ytest, yhat.mda)
  
  # 8. Artificial neural network
  
  nnet.model <- train(y ~ ., data = train_seedData, method = "nnet", trControl = trainControl(method = "cv", number = 10, returnResamp = "all"), trace = FALSE)
  
  yhat.nnet <- predict(nnet.model, xtest)
  
  err[r, 8] <- 1 - accuracy(ytest, yhat.nnet)
  
  # 9. Multinomial logistic regression
  
  multinom.model <- train(y ~ ., data = train_seedData, method = "multinom", trControl = trainControl(method = "cv", number = 10, returnResamp = "all", classProbs = TRUE, summaryFunction = multiClassSummary), metric = "logLoss", trace = FALSE)
  
  yhat.multinom<- predict(multinom.model, newdata = xtest) 
  
  err[r,9] <- 1 - accuracy(ytest, yhat.multinom)
  
  # 10. Adabag model
  
  bagging.model <- train(y ~ ., data = train_seedData, method = "AdaBag", tuneGrid = expand.grid(mfinal = (1:3) * 3, maxdepth = c(1, 3)), trControl = trainControl(method = "cv", number = 10, returnResamp = "all", classProbs = TRUE, summaryFunction = multiClassSummary, seeds = vector(mode = "list", length = nrow(train_seedData) + 1) %>% lapply(., function(x) 1:20)), metric = "logLoss")
  
  yhat.bagging <- predict(bagging.model, xtest)
  
  err[r, 10] <- 1 - accuracy(ytest, yhat.bagging)
  
  # 11. Naive bayes
  
  naiveBayes.model <- train(y ~ ., data = train_seedData, method = "naive_bayes", trControl = trainControl(method = "cv", number = 10, returnResamp = "all", classProbs = TRUE, summaryFunction = multiClassSummary), metric = "logLoss")
  
  yhat.naiveBayes <- predict(naiveBayes.model, xtest)
  
  err[r, 11] <- 1 - accuracy(ytest, yhat.naiveBayes)
  
  # 12. Multivariate adaptive regression splines (MARS)
  mars.model <- train(y ~ ., data = train_seedData, method = "earth", trControl = trainControl(method = "cv", number = 10, returnResamp = "all", classProbs = TRUE), tuneGrid = data.frame(degree = 1, nprune = (2:4) * 2))
  
  yhat.mars <- predict(mars.model, xtest)
  
  err[r, 12] <- 1 - accuracy(ytest, yhat.mars)
  
  # 13. Quadratic discriminant analysis
  
  qda.model <- train(y ~ .,
                     data = train_seedData,
                     method = "qda", trControl = trainControl(method = "cv", number = 10, returnResamp = "all", classProbs = TRUE, summaryFunction = multiClassSummary), metric = "logLoss")
  
  yhat.qda <- predict(qda.model, xtest)
  
  err[r, 13] <- 1 - accuracy(ytest, yhat.qda)
  
  
  # 14. Regularized discriminant analysis (RDA)
  rda.model <- train(y ~ ., data = train_seedData, method = "rda", trControl = cctrl1 <- trainControl(method = "cv", number = 10, returnResamp = "all", classProbs = TRUE, summaryFunction = multiClassSummary, seeds = vector(mode = "list", length = nrow(train_seedData) + 1) %>% lapply(., function(x) 1:20)), metric = "logLoss")
  
  yhat.rda <- predict(rda.model, xtest)
  
  err[r, 14] <- 1 - accuracy(ytest, yhat.rda)
  
  # 15. glmnet model
  
  glmnet.model <- train(y ~ ., data = train_seedData, method = "glmnet", trControl = trainControl(method = "cv", number = 10, returnResamp = "all", classProbs = TRUE, summaryFunction = multiClassSummary), metric = "logLoss", tuneGrid = expand.grid(.alpha = seq(.05, 1, length = 15), .lambda = c((1:5) / 10)))
  
  yhat.glmnet <- predict(glmnet.model, xtest)
  
  err[r, 15] <- 1 - accuracy(ytest, yhat.glmnet)
  
  if (r %% 25 == 0) cat('\n', paste(round(100 * r / R, 0), '%', 'completed\n'))
}


# From matrix data type to tibble

colnames(err) <- c("LDA", "SVM", "CART", "rForest", "Gauss", "kNN", "MDA", "NNET", "Multinom", "Bagging", "naiveBayes", "MARS", "QDA", "RDA", "glmnet")

err <- as_tibble(err)


# Boxplot for test errors of 15 classifiers --------------

AVTE_seed_dataset <- err %>% pivot_longer(1:15, names_to = "Model", values_to = "AVTE") %>% ggplot(.,aes(x=Model, y=AVTE, fill=Model))+geom_boxplot(show.legend = F)+theme_bw()+labs(y='Test error', x='Method (Classifier)', title = 'Misclassification error of each classifier', subtitle = 'over 100 hold out sub sample', caption = "Source: Seeds dataset")+theme(axis.title.x = element_text(face = 'bold',size = 12),axis.title.y = element_text(face = 'bold',size = 12),axis.text.x = element_text(angle = 50, vjust = 0.5, face = "bold"))                                    
# Save figure

ggsave("Classification_figures_test_errors/Multi_class/AVTE_seed_dataset.png", width = 6.74, height = 4.54)


# Average prediction error --------------------------------

avg.err <- apply(err, 2, mean)

AVPE_seed_dataset <- enframe(avg.err, name = "Model", value = "AVTE") %>% ggplot(., aes(x = 1:15, y = AVTE)) + geom_point() +
  ggrepel::geom_label_repel(aes(label = Model)) + theme_bw() + labs(y = "Average test error", x = "Method (Classifier)", title = "Average misclassification error of each classifier", subtitle = "over 100 hold out sub sample", caption = "Source: Seeds dataset") + theme(
    axis.title.x = element_text(face = "bold", size = 12), axis.title.y = element_text(face = "bold", size = 12),
    axis.text.x = element_blank(), axis.ticks.x = element_blank())


# Save figure

ggsave("Classification_figures_test_errors/Multi_class/AVPE_seed_dataset.png", width = 6.74, height = 4.54)


# Seeds dataset for mean, median, standard deviation -----------

seed_table <- err %>% pivot_longer(1:15, names_to = "Model", values_to = "Value") %>%
  group_by(Model) %>%
  summarise(avg.err = round(mean(Value), 4), med.err = round(median(Value), 4), std.err = round(sd(Value), 4))


# Seeds dataset for rank --------------------------------------

seed_table.rk <- seed_table %>% mutate(avg.err.rk = rank(avg.err, ties.method = "average"), med.err.rk = rank(med.err, ties.method = "average"), std.err.rk = rank(std.err, ties.method = "average"))

write_csv(seed_table.rk, "seed_table.rk.csv")

# End