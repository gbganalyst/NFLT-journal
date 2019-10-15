
# R Packages ----------------------------------------

data_wrangling_packages <- c("dplyr", "ggplot2","openxlsx", "tibble", "ggrepel", "tidyr")

machine_learning_packages <- c("caret","MASS", "car", "kernlab","rpart","randomForest","class","ada", "rda","e1071", "nnet","ipred", "dbarts", "klaR", "glmnet", "earth")

if(!require("xfun")){
  install.packages("xfun")
}

xfun::pkg_attach(c(data_wrangling_packages, machine_learning_packages),install = T)

# Multi Classification on brain dataset -----------------------------

dataset=read.xlsx('Large p small n dataset/Multi class/Brain Cancer Microarray Data.xlsx',sheet = 1)

# Data wrangling and preprocessing

factor_variable_position <- ncol(dataset)


brain_dataset <- dataset %>% mutate_at(.,vars (factor_variable_position),~as.factor(.))

# Scaling the continous variables

preProcess_scale_model <- preProcess(brain_dataset, method=c('center', 'scale'))

brain_dataset <- predict(preProcess_scale_model, newdata = brain_dataset)


#  Set the total number of replications 

R <- 20

#  Initialize the test error vector

err <- matrix(0, ncol=11, nrow=R)

for(r in 1:R){
  
  # Create the training and test datasets for brain_dataset
  
  # Step 1: Get row numbers for the training data
  trainRowNumbers <- createDataPartition(brain_dataset$y, p=0.8,   list=FALSE)
  
  # Step 2: Create the training  dataset
  train_brainData <- brain_dataset[trainRowNumbers,]
  
  # Step 3: Create the test dataset
  test_brainData <- brain_dataset[-trainRowNumbers,]
  
  # Store X and Y for later use.
  xtrain <-  train_brainData[-ncol(train_brainData)]
  ytrain <-  train_brainData$y
  xtest <- test_brainData[-ncol(test_brainData)]
  ytest <- test_brainData$y
  ntr <- nrow(train_brainData)
  nte <- nrow(test_brainData)
  
  
  lda.model <- lda(y~., data=train_brainData)  
  yhat.lda <- predict(lda.model, xtest)$class
  err.lda <- 1-sum(diag(table(ytest, yhat.lda)))/nte
  
  err[r,1] <- err.lda
  
  svm.model <- ksvm(y~., data=train_brainData)
  yhat.svm <- predict(svm.model, xtest)
  err.svm <- 1-sum(diag(table(ytest, yhat.svm)))/nte
  
  err[r,2] <- err.svm
  
  
  cart.model <- rpart(y~., data=train_brainData)
  yhat.cart <- predict(cart.model, xtest, type='class')
  err.cart <- 1-sum(diag(table(ytest, yhat.cart)))/nte
  
  err[r,3] <- err.cart
  
  
  forest.model <- randomForest(y~., data=train_brainData)
  yhat.forest <- predict(forest.model, xtest, type='class')
  err.forest <- 1-sum(diag(table(ytest, yhat.forest)))/nte
  
  err[r,4] <- err.forest
  
  gausspr.model <- gausspr(y~., data=train_brainData,kernel='rbfdot')
  yhat.gausspr <- predict(gausspr.model, xtest, type='response')
  err.gausspr <- 1-sum(diag(table(ytest, yhat.gausspr)))/nte
  
  err[r,5] <- err.gausspr
  
  knn.model <- train(y ~., data=train_brainData, method = "knn", trControl=trainControl(method = "repeatedcv", number = 10, repeats = 3), tuneLength = 10)
  yhat.knn <- predict(knn.model, newdata = xtest)
  err.knn <- 1-sum(diag(table(ytest, yhat.knn)))/nte
  
  err[r,6] <- err.knn
  
  
 #  mda.model <- train(y~., data=train_brainData, method='mda')
  # yhat.mda  <- predict(mda.model, xtest)
  # err.mda <- 1-sum(diag(table(ytest, yhat.mda)))/nte
  
  # err[r,7] <- err.mda
  
  
  #nnet.model <- nnet(y~., data=train_brainData, trace=F, size = 4,   rang = 0.1, decay = 5e-4, maxit = 200)
  # yhat.nnet <- predict(nnet.model, xtest, type="class")
  # err.nnet <- 1-sum(diag(table(ytest, yhat.nnet)))/nte
  
  # err[r,8] <- err.nnet 
  
  
  #multinom.model <- multinom(y ~., data=train_brainData)
  #yhat.multinom<- predict(multinom.model, newdata = xtest,type = "class") 
  #err.multinom <- 1-sum(diag(table(ytest, yhat.multinom)))/nte
  
  #err[r,9] <- err.multinom
  
  
  bagging.model <- bagging(y~., data=train_brainData)
  yhat.bagging <- predict(bagging.model, xtest)
  err.bagging <- 1-sum(diag(table(ytest, yhat.bagging)))/nte
  
  err[r,7] <- err.bagging
  
  naiveBayes.model <- naiveBayes(y~., data=train_brainData)
  yhat.naiveBayes <- predict(naiveBayes.model, xtest)
  err.naiveBayes <- 1-sum(diag(table(ytest, yhat.naiveBayes)))/nte
  
  err[r,8] <- err.naiveBayes
  
  
  mars.model <- train(y~., data=train_brainData, method='earth')
  yhat.mars  <- predict(mars.model, xtest)
  err.mars <- 1-sum(diag(table(ytest, yhat.mars)))/nte
  
  err[r,9] <- err.mars
  
  
  #qda.model <- qda(y ~., data=train_brainData)
  #yhat.qda <- predict(qda.model,xtest)$class
  #err.qda <- 1-sum(diag(table(ytest, yhat.qda)))/nte
  
  #err[r,13] <- err.qda
  
  
  rda.model <- train(y~., data=train_brainData, method='rda')
  yhat.rda  <- predict(rda.model, xtest)
  err.rda <- 1-sum(diag(table(ytest, yhat.rda)))/nte
  
  err[r,10] <- err.rda
  
  
  glmnet.model <- train(y~., data=train_brainData, method='glmnet')
  yhat.glmnet  <- predict(glmnet.model, xtest)
  err.glmnet <- 1-sum(diag(table(ytest, yhat.glmnet)))/nte
  
  err[r,11] <- err.glmnet
  
  if (r%%25==0)  cat('\n', round(100*r/R,0),'completed\n')
}



# Boxplot for AVTE ------------------------------------------

colnames(err)=c('LDA','SVM','CART','rForest','Gauss', 'kNN', 'Bagging', 'naiveBayes', 'MARS','RDA', 'glmnet')

err=as_tibble(err)

err %>% gather(1:11,key = 'Model',value = 'AVTE') %>% ggplot(.,aes(reorder(x=Model,AVTE, median), y=AVTE, fill=Model))+geom_boxplot(show.legend = F)+theme_bw()+labs(y=' Average test error', x='Method (Classifier)', title = 'Average test error of each classifier',caption = "Source: Brain Cancer Microarray Dataset")+theme(axis.title.x = element_text(face = 'bold',size = 12),axis.title.y = element_text(face = 'bold',size = 12),axis.text.x = element_text(angle = 50, vjust = 0.5))->AVTE_brain_dataset                                           

ggsave("Large p small n script/Images of average test errors/Multi class/AVTE_brain_dataset.png", width = 6.74, height = 4.54)


# Average prediction error --------------------------------

avg.err=apply(err, 2, mean)

data.frame(SN=1:11, AVTE=avg.err) %>%
  rownames_to_column(., var="Model") %>% ggplot(., aes(x=SN, y=AVTE))+geom_point()+
  ggrepel::geom_label_repel(aes(label=Model))+theme_bw()+
  labs(y="Average prediction error", x="Method (Classifier)",        title = "Average prediction error of each classifier", caption = "Source: Brain Cancer Microarray Dataset")+
  theme(axis.title.x = element_text(face = "bold",size = 12),axis.title.y = element_text(face = "bold",size = 12))->AVPE_brain_dataset


ggsave("Large p small n script/Images of average test errors/Multi class/AVPE_brain_dataset.png",width = 6.74, height = 4.54)

# brain dataset for mean, median, standard deviation -----------

brain_table<- err %>% gather(1:11, key = "Model",value = "Value") %>% group_by(Model) %>% summarise(avg.err=round(mean(Value),4),med.err=round(median(Value),4), std.err=round(sd(Value),4))


# brain dataset for rank ---------------------------------

brain_table.rk <- brain_table %>% mutate(avg.err.rk=rank(avg.err,ties.method = "average"), med.err.rk=rank(med.err,ties.method = "average"), std.err.rk=rank(std.err,ties.method = "average"))

write.table(brain_table.rk, "brain.cancer_table", sep="\t", row.names = F)

#End