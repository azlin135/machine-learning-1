##################################################
##################################################
#load libraries
library(class)
library(caret)
library(kernlab)
#load data set 'spam' from library 'kernlab'
data("spam")
#check properties in spam data set
str(spam)
summary(spam)
#check factor 'type' which is our binary respond:'spam' and 'nonspam'
table(spam$type) #shows total count of each class in table

#scale data - reasoning in document
spam_s=scale(spam[,-58])

#define sizes for simplicity: NN, n, s 
n = 100 #number of each class in training set
s = 50 #number of each class in test set
NN = n+s #total number of subset of data set in each class

#get indexes to create subset of 'spam' data set
set.seed(123) #for reproducibility
index_spam=spam_s[sample(which(spam$type=='spam'),NN),]
index_nonspam=spam_s[sample(which(spam$type=='nonspam'),NN),]

#training set of first 100 observations in each in class from subset
train_rand = rbind(index_nonspam[1:100,], index_spam[1:100,])
#test set of last 50 observations in each in class from subset
test_rand = rbind(index_nonspam[101:150,], index_spam[101:150,])

#Get class factor for training data
train_label=factor(c(rep('nonspam',n),rep('spam',n)))
#Get class factor for test data
test_label_true=factor(c(rep('nonspam',s),rep('spam',s)))

##################################################
## kNN3 - as it associates probability for both instances: 'nonspam' and 'spam'
##################################################

#define k as vector for simplification
kk = c(1,9,25)
#Classification using 1NN3
set.seed(123) #for reproducibility
knn3_pred1=knn3Train(train_rand, test_rand, train_label, k = kk[1], prob=TRUE) 
prob1 = attributes(knn3_pred1) #see the attributes of the 1NN3 model
prob1 
#accuracy for 1NN3 model
acc1=mean(knn3_pred1==test_label_true)
acc1

#Classification using 9NN3
set.seed(123) #for reproducibility
knn3_pred2=knn3Train(train_rand, test_rand, train_label, k = kk[2], prob=TRUE) 
prob2 = attributes(knn3_pred2) #see the attributes of the 9NN3 model
prob2 
#accuracy for 9NN3 model
acc2=mean(knn3_pred2==test_label_true)
acc2

#Classification using 25NN3
set.seed(123) #for reproducibility
knn3_pred3=knn3Train(train_rand, test_rand, train_label, k = kk[3], prob=TRUE) 
prob3 = attributes(knn3_pred3) #see the attributes of the 25NN3 model
prob3
#accuracy for 25NN3 model
acc3=mean(knn3_pred3==test_label_true)
acc3


##################################################
## Plotting accuracy of kk
##################################################

acc <- c(acc1, acc2, acc3)
plot(kk, acc, type='b',col='coral3',lty=4,pch=2,lwd=2,
     main='Accuracies with different K level',xlab='K', ylab='Accuracy')


