# This project is based on the prediction of whether a website is phishing or not, based on its various attributes.
# You can get the data set from the following link- https://archive.ics.uci.edu/ml/machine-learning-databases/00327/ 
# To make the table of the above data set, simply use this command-
phishing <- read.csv("Phishing_Dataset.txt")
# Phishing_Dataset.txt was the name of the file,where i saved the data,put it in the same directory as your working directory
# Now to name the columns we use the following command-
colnames(phishing) <- c("having_IP_Address","URL_Length","Shortening_Service","having_At_Symbol","double_slash_redirecting","Prefix_Suffix","having_Sub_Domain","SSLfinal_State","Domain_registration_length","Favicon","port","HTTPS_token","Request_URL","URL_of_Anchor","Links_in_tags","SFH","Submitting_to_email","Abnormal_URL","Redirect","on_mouseover","RightClick","popUpWindow","Iframe","age_of_domain","DNSRecord","web_traffic","Page_Rank","Google_Index","Links_pointing_to_page","Statistical_report","Result")
# Now to view the dataset, simply use this command-
View(phishing)
# To see the number of rows and columns we have got, use the following command-
dim(phishing)
# Now to see what type of variables we have got, simply use this command-
str(phishing)
# Now you can see that the data set contains only values -1,0,1 still it is giving us numeric variables
# So let's change the variables into factor variables,but we will not use the same object we will copy it into other object,using the following command-
phishing1 <- phishing
for(i in 1:31){phishing1[,i] <- as.factor(phishing1[,i])}
str(phishing1)
# Now you will observe that variables type have been changed to factor.
# Now load some packages so that we can run next few functions,first of check whether you have installed these packages, then use these commands-
library("caret",lib.loc = "c:/Program Files/R/R-3.4.1/library")
# Here name of package is caret and its location is specified in the lib.loc
library(randomForest,lib.loc = "c:/Program Files/R/R-3.4.1/library")
library(rattle,lib.loc = "c:/Program Files/R/R-3.4.1/library")
library(rpart,lib.loc = "c:/Program Files/R/R-3.4.1/library")
# Now first of all partition the data set into 2 different data sets,so that one can be used for modelling the data and other can be used for prediction of data
# First of all set the seed,so that when you run the program many time results don't change drastically,use the following command-
set.seed(3230)
# Use following command to partition the data set-
intrain <- createDataPartition(y = phishing1$Result,p=0.7,list = FALSE)
# The above command shows that we are splitting the data based on variable Result,and the splitting will be on 70 to 30.
# Now make two data sets training and testing based on above partition wehere training will contain 70% of data and testing will contain 30% of data
training1 <- phishing1[intrain,]
testing1 <- phishing1[-intrain,]
# Now you can see the dimensions of the above two data sets and compare with the original data set, you will see that the partition worked correctly,use the following command- 
dim(training1)
dim(testing1)
# Now for model fitting we will use training data set and for prediction we will use testing dataset 


# Now we will apply various machine learning algorithms 

# First of all we will use Tree-Classification algorithm
# For this, use command-
fit1 <- train(Result~.,method = "rpart",data = training1,trControl = trainControl(method = "cv"))
# In above command method = "rpart" suggest that we are applying tree algorithm and we are applying it on training data set . For subsampling of data we are using cv i.e cross validation
# To make a prettier plot of how the splitting was done use the following command-
fancyRpartPlot(fit1$finalModel)
# To see the details of fit1, we use the following command-
fit1
# Now you can predict above alogorithm on testing data set,using the following command-
pre1 <- predict(fit1,newdata = testing1[,-31])
# Now to see how much we are right use the following command-
tr1 <-  table(pre1,testing1$Result)
tr1
correct1 <- (tr1[1,1]+tr1[2,2])/(tr1[1,1]+tr1[2,2]+tr1[1,2]+tr1[2,1])
correct1 <- correct1 * 100
correct1

# Now we will use random_forest algorithm
# For this use command-
fit2 <- train(Result~.,method = "rf",data = training1,trControl = trainControl(method = "cv"))
# In above command method = "rf" suggest that we are applying random_forest algorithm and we are applying it on training data set.
# To see the details of fit2,use the following command-
fit2
# Now you can predict above alogorithm on testing data set,using the following command-
pre2 <- predict(fit2,newdata = testing1[,-31])
# Now to see how much we are right use the following command-
tr2 <- table(pre2,testing1$Result)
tr2
correct2 <- (tr2[1,1]+tr2[2,2])/(tr2[1,1]+tr2[2,2]+tr2[1,2]+tr2[2,1])
correct2 <- correct2 * 100
correct2

# To show variable importance plot , i.e which variable is much important , we use the following command-
fit3 <- randomForest(Result~.,data = training1,ntree = 2000)
# In above command we are telling to produce 2000 trees.
# If you will see the details of this fit3 , you will observe that no subsampling is done
fit3
# To show the plot , use command-
varImpPlot(fit3)
pre3 <- predict(fit3,newdata = testing1)
tr3 <- table(pre3,testing1$Result)
tr3
correct3 <- (tr3[1,1]+tr3[2,2])/(tr3[1,1]+tr3[2,2]+tr3[1,2]+tr3[2,1])
correct3 <- correct3 * 100
correct3

# Next machine learning algorithm we are going to apply is Boosting
# For this use command-
fit4 <- train(Result~.,method = "gbm",data = training1,trControl = trainControl(method = "cv"),verbose = FALSE)
# In above command , method = "gbm" means we are using gradient boosting
# To see the details of fit4,use the following command-
fit4
# Now you can predict above alogorithm on testing data set,using the following command-
pre4 <- predict(fit4,newdata = testing1[,-31])
# Now to see how much we are right use the following command-
tr4 <- table(pre4,testing1$Result)
tr4
correct4 <- (tr4[1,1]+tr4[2,2])/(tr4[1,1]+tr4[2,2]+tr4[1,2]+tr4[2,1])
correct4 <- correct4 * 100
correct4



# Next algorithm we are going to apply is GLM(Generalised Linear Model)
# For this use command-
fit5 <- train(Result~.,method = "glm",data = training1,trControl = trainControl(method = "cv"))
# In above command , method = "glm" means we are using generalised linear model
# To see the details of fit5,use the following command-
fit5
# Now you can predict above alogorithm on testing data set,using the following command-
pre5 <- predict(fit5,newdata = testing1[,-31])
# Now to see how much we are right use the following command-
tr5 <- table(pre5,testing1$Result)
tr5
correct5 <- (tr5[1,1]+tr5[2,2])/(tr5[1,1]+tr5[2,2]+tr5[1,2]+tr5[2,1])
correct5 <- correct5 * 100
correct5
 

#Next algorithm we are going to apply is combined model prediction i.e we are going to combine some model fitting
# For this first of all again take the data set and again create data partition
inbuild <- createDataPartition(phishing1$Result,p=0.7,list = FALSE)
builddata <- phishing1[inbuild,]
validation <- phishing1[-inbuild,]
# Now in builddata we will again create partition into training and testing dataset
intrain2 <- createDataPartition(builddata$Result,p=0.7,list = FALSE)
training2 <- builddata[intrain2,]
testing2 <- builddata[-intrain2,]
mod1 <- train(Result~.,method = "glm",data = training2,trControl = trainControl(method = "cv"))
mod2 <- train(Result~.,method = "rf",data = training2,trControl = trainControl(method = "cv"))
pred1 <- predict(mod1,testing2[,-31])
pred2 <- predict(mod2,testing2[,-31])
# Now we will combine above predictors and make a new data frame
predf <- data.frame(pred1=pred1,pred2=pred2,Result = testing2$Result)
# Now we will apply a combined model fitting on this new data frame
combmodfit <- train(Result~.,method = "gam",data = predf,trControl = trainControl(method = "cv"))
# In the above method="gam" shows that we are using Generalised Linear Model
combpred <- predict(combmodfit,predf)
# Now to see how what is the advantage of combined model prediction use the following command-
table(pred1,testing2$Result)
table(pred2,testing2$Result)
table(combpred,testing2$Result)



# Now we will make some changes to our data set

# Let us first use Generalised Linear Model(GLM)
fit6 <- glm(Result~.,data = training1,family = "binomial")
# To see the summary of fit6 , use this command
summary(fit6)
# You will get the intercept and slope with respect to different variables.
# Now let's see which variables have got high variance inflation factor i.e which variable is affected most when used with other variables,for this first of all load the car package
library("car",lib.loc = "c:/Program Files/R/R-3.4.1/library")
vif(fit6)
# After observing the result from above command , let us remove those variables whose vif value is greater than 5,i.e whose value will be 5 times more than it would have been used individually
# Let us remove those variables , but also check whether those variables do not have got low p-values(we got these from summary of fit6)
# We will observe that varibles which have vif greater than 5 are double_slash_redirecting,Favicon,popUpWindow
newtraining1 <- training1[,-c(5,10,22)]
# Similar changes should be applied on testing data set
newtesting1 <- testing1[,-c(5,10,22)]
# in above command [,-c(5,10,22)] indicates that we are removing 5th,10th and 22nd column
#You can also view the above data set and check whether those variables are removed or not
View(newtraining1)
# Now if you will again apply above GLM
fit7 <- glm(Result~.,data = newtraining1,family = "binomial")
summary(fit7)
# Now if you observe vif , you will see no variables have vif values greater than 5 and other variables vif have also become smaller
vif(fit7)


# Now let us again apply machine learning algorithms on this new data set and predict it on testing data set

# First of all we will use Tree-Classification algorithm
# For this, use command-
fit8 <- train(Result~.,method = "rpart",data = newtraining1,trControl = trainControl(method = "cv"))
# In above command method = "rpart" suggest that we are applying tree algorithm and we are applying it on training data set . For subsampling of data we are using cv i.e cross validation
# To make a prettier plot of how the splitting was done use the following command-
fancyRpartPlot(fit8$finalModel)
# To see the details of fit1, we use the following command-
fit8
# Now you can predict above alogorithm on testing data set,using the following command-
pre8 <- predict(fit8,newdata =newtesting1[,-28])
# Now to see how much we are right use the following command-
tr8 <- table(pre8,newtesting1$Result)
tr8
correct8 <- (tr8[1,1]+tr8[2,2])/(tr8[1,1]+tr8[2,2]+tr8[1,2]+tr8[2,1])
correct8 <- correct8 * 100
correct8
  
  
# Now we will use random_forest algorithm
# For this use command-
fit9 <- train(Result~.,method = "rf",data = newtraining1,trControl = trainControl(method = "cv"))
# In above command method = "rf" suggest that we are applying random_forest algorithm and we are applying it on training data set.
# To see the details of fit2,use the following command-
fit9
# Now you can predict above alogorithm on testing data set,using the following command-
pre9 <- predict(fit9,newdata = newtesting1[,-28])
# Now to see how much we are right use the following command-
tr9 <- table(pre9,newtesting1$Result)
tr9
correct9 <- (tr9[1,1]+tr9[2,2])/(tr9[1,1]+tr9[2,2]+tr9[1,2]+tr9[2,1])
correct9 <- correct9 * 100
correct9
  
# To show variable importance plot , i.e which variable is much important , we use the following command-
fit10 <- randomForest(Result~.,data = newtraining1,ntree = 2000)
# In above command we are telling to produce 2000 trees.
# If you will see the details of this fit3 , you will observe that no subsampling is done
fit10
# To show the plot , use command-
varImpPlot(fit10)
pre10 <- predict(fit10,newdata = newtesting1)  
tr10 <- table(pre10,newtesting1$Result)
tr10
correct10 <- (tr10[1,1]+tr10[2,2])/(tr10[1,1]+tr10[2,2]+tr10[1,2]+tr10[2,1])
correct10 <- correct10 * 100
correct10
  
# Next machine learning algorithm we are going to apply is Boosting
# For this use command-
fit11 <- train(Result~.,method = "gbm",data = newtraining1,trControl = trainControl(method = "cv"),verbose = FALSE)
# In above command , method = "gb" means we are using gradient boosting
# To see the details of fit4,use the following command-
fit11
# Now you can predict above alogorithm on testing data set,using the following command-
pre11 <- predict(fit11,newdata = newtesting1[,-28])
# Now to see how much we are right use the following command-
tr11 <- table(pre11,newtesting1$Result)
tr11
correct11 <- (tr11[1,1]+tr11[2,2])/(tr11[1,1]+tr11[2,2]+tr11[1,2]+tr11[2,1])
correct11 <- correct11 * 100
correct11



# Now let us change our data set in different manner
# Now let us use Principle Component Analysis(PCA)
# For this first of all see the variables which have high correlation to other variables
# Let us use our initial phishing data set
intrain2 <- createDataPartition(y = phishing$Result,p=0.7,list = FALSE)
training2 <- phishing[intrain2,]
testing2 <- phishing[-intrain2,]


m <- abs(cor(training2[,-31]))
# cor() means finding corelation and abs() means absolute value of it
diag(m) <- 0
# it is showing that we have set corelation of variable with itself as 0, since it will always be 1
which(m > 0.8,arr.ind = TRUE)
# now we are selecting those variables which have m value greater than 0.8
# From this you will observe that double_slash_redirecting and Shortening_Service , port and Favicon , port and Submitting _to_email , popUpWindow and Favicon are highly corelated to each other
# Now we will try to combine these pairs and make them into their respective one variable
small1 <- training2[,c(5,3)]
small2 <- training2[,c(17,11)]
small3 <- training2[,c(22,10)]
prcmp1 <- prcomp(small1)
# we are applying principle component function in small1 to see how we can combine these 2 variables
prcmp2 <- prcomp(small2)
prcmp3 <- prcomp(small3)
prcmp1$rotation
# To see the best two combination we have used above command
prcmp2$rotation
prcmp3$rotation
# To see which component explains better use this plot command
plot(prcmp1$x[,1],prcmp1$x[,2])
# Always select that axis which has high variability in values , so that we get better explanation
plot(prcmp2$x[,1],prcmp2$x[,2])
plot(prcmp3$x[,1],prcmp3$x[,2])
# In all cases you will observe that pc1 will be better than pc2
# So let us combine these variables according to pc1
training2$new1 <- -(0.7044257*training2$double_slash_redirecting) - (0.7097778*training2$Shortening_Service)
training2$new2 <- -(0.7553941*training2$Submitting_to_email) - (0.6552708*training2$port)
training2$new3 <- -(0.7134187*training2$popUpWindow) - (0.7007380*training2$Favicon)
# Now put these same values in testing2 data set
testing2$new1 <- -(0.7044257*testing2$double_slash_redirecting) - (0.7097778*testing2$Shortening_Service)
testing2$new2 <- -(0.7553941*testing2$Submitting_to_email) - (0.6552708*testing2$port)
testing2$new3 <- -(0.7134187*testing2$popUpWindow) - (0.7007380*testing2$Favicon)

# After performng this combination , now let us remove those variables, which now we have combined them
training2 <- training2[,-c(3,5,10,11,17,22)]
for(i in 1:25){training2[,i] <- as.factor(training2[,i])}
# Applying same on test data set
testing2 <- testing2[,-c(3,5,10,11,17,22)]
for(i in 1:25){testing2[,i] <- as.factor(testing2[,i])}


# Now let us again apply machine learning algorithms on this new data set and predict it on testing data set

# First of all we will use Tree-Classification algorithm
# For this, use command-
fit12 <- train(Result~.,method = "rpart",data = training2,trControl = trainControl(method = "cv"))
# In above command method = "rpart" suggest that we are applying tree algorithm and we are applying it on training data set . For subsampling of data we are using cv i.e cross validation
# To make a prettier plot of how the splitting was done use the following command-
fancyRpartPlot(fit12$finalModel)
# To see the details of fit1, we use the following command-
fit12
# Now you can predict above alogorithm on testing data set,using the following command-
pre12 <- predict(fit12,newdata = testing2)
# Now to see how much we are right use the following command-
tr12 <-  table(pre12,testing2$Result)
tr12
correct12 <- (tr12[1,1]+tr12[2,2])/(tr12[1,1]+tr12[2,2]+tr12[1,2]+tr12[2,1])
correct12 <- correct12 * 100
correct12


# Now we will use random_forest algorithm
# For this use command-
fit13 <- train(Result~.,method = "rf",data = training2,trControl = trainControl(method = "cv"))
# In above command method = "rf" suggest that we are applying random_forest algorithm and we are applying it on training data set.
# To see the details of fit2,use the following command-
fit13
# Now you can predict above alogorithm on testing data set,using the following command-
pre13 <- predict(fit13,newdata = testing2)
# Now to see how much we are right use the following command-
tr13 <- table(pre13,testing2$Result)
tr13
correct13 <- (tr13[1,1]+tr13[2,2])/(tr13[1,1]+tr13[2,2]+tr13[1,2]+tr13[2,1])
correct13 <- correct13 * 100
correct13

# To show variable importance plot , i.e which variable is much important , we use the following command-
fit14 <- randomForest(Result~.,data = training2,ntree = 2000)
# In above command we are telling to produce 2000 trees.
# If you will see the details of this fit3 , you will observe that no subsampling is done
fit14
# To show the plot , use command-
varImpPlot(fit14)
pre14 <- predict(fit14,newdata = testing2)
tr14 <- table(fit14,testing2$Result)
tr14
correct14 <- (tr14[1,1]+tr14[2,2])/(tr14[1,1]+tr14[2,2]+tr14[1,2]+tr14[2,1])
correct14 <- correct14 * 100
correct14


# Next machine learning algorithm we are going to apply is Boosting
# For this use command-
fit15 <- train(Result~.,method = "gbm",data = training2,trControl = trainControl(method = "cv"),verbose = FALSE)
# In above command , method = "gb" means we are using gradient boosting
# To see the details of fit4,use the following command-
fit15
# Now you can predict above alogorithm on testing data set,using the following command-
pre15 <- predict(fit15,newdata = testing2)
# Now to see how much we are right use the following command-
tr15 <- table(pre15,testing2$Result)
tr15
correct15 <- (tr15[1,1]+tr15[2,2])/(tr15[1,1]+tr15[2,2]+tr15[1,2]+tr15[2,1])
correct15 <- correct15 * 100
correct15

