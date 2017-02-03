#read csv file 'student-mat.csv'
math <- read.csv("downloads/student-2/student-mat.csv", sep=";",header=TRUE)
math

########################################################
#EXPLORATORY DATA ANALYSIS

dim(math)
head(math)
View(head(math))
str(math)
sum(is.na(math))
sum(duplicated(math))
summary(math)
boxplot(math)

########################################################
#PREPROCESSING TASKS

#rearrange columns
math<-math[c(1,2,4,5,6,9,10,11,12,16,17,18,19,20,21,22,23,3,7,8,13,14,15,
             24,25,26,27,28,29,30,31,32,33)]

#create new column to show students' achievement in mathematics
achievement <- with(math, ifelse(G3 >= 10, "H", "L"))
achievement <- factor(achievement, levels=c("L", "H"))
math<- data.frame(math, achievement) #create new column in math


#convert columns with yes/no to 1/0
schoolsup = as.numeric(math$schoolsup)-1
famsup = as.numeric(math$famsup)-1
paid = as.numeric(math$paid)-1
activities = as.numeric(math$activities)-1
nursery = as.numeric(math$nursery)-1
higher = as.numeric(math$higher)-1
internet = as.numeric(math$internet)-1
romantic = as.numeric(math$romantic)-1
achievement.num = as.numeric(math$achievement)-1

#scaling math data using column 18 to 33
maxs<- apply(math[,18:33], 2, max)
mins <- apply(math[,18:33], 2, min)
scaled.math<- as.data.frame(scale(math[,18:33], center = mins, scale = maxs-mins))

#combining data from columns containing binary values of 1/0 and scaled.math
data = cbind(schoolsup, famsup, paid, activities, nursery, higher, internet, romantic, scaled.math, achievement.num)

########################################################
#ANN

library(caTools)
set.seed(101)

#create sample split with split ratio 0.7
split= sample.split(data$achievement.num, SplitRatio =0.70)

#Split data based on split Boolean vector
train=subset(data, split=TRUE)
test= subset(data, split=FALSE)

#creating formula to be used in neuralnet
feat<- names(scaled.math[1:15])
f<- paste(feat, collapse= '+')
f<- paste('achievement.num ~', f)
f<- as.formula(f)
f

#creating ANN
library(neuralnet)
nn<- neuralnet(f, data, hidden =c(10,10,10), linear.output =FALSE)
predicted.nn.values <- compute(nn, test[9:23])
predicted.nn.values$net.result <- sapply(predicted.nn.values$net.result, round, digits =0)
print(head(predicted.nn.values$net.result))

library(caret)
cmann<-confusionMatrix(test$achievement.num, predicted.nn.values$net.result)
cmann

#plotting ANN
plot(nn)

########################################################
#DECISION TREE

library(tree)

set.seed(2)

#creating training set to create training model
math.train<- sample(1:nrow(math), 200)

#creating test set to be tested with training model
math.test<- math[-math.train,]

#obtain values of achievement from test set
achievement.test<- achievement[-math.train]

#creating decision tree by classifying achievement
math.dtree<-tree::tree(achievement ~ . -G3, math, subset=math.train)
plot(math.dtree)
text(math.dtree, pretty=0)

#predicting achievement on math.test using the decision tree created
math.pred <-predict(math.dtree, math.test, type="class")

#confusion matrix
cmdt<-confusionMatrix(math.pred, achievement.test)
cmdt

########################################################
#NAIVE BAYES

library(e1071)

#create training set by splitting data
set.seed(2)
data.trainrows<- sample(1:nrow(data), 200)
data.train<- data[data.trainrows,]

#create test set from data
data.test<- data[-data.trainrows,]

#creating naive bayes model using the formula created
nb <- naiveBayes(achievement.num ~ ., data.train)
nb

#prediction on test set using naive bayes model created
prediction <- predict(nb, data.test ,type="raw")
prediction