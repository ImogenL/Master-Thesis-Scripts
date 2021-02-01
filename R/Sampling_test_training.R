ordered.index.add <- index.add[order(index.add$label),] 

#Randomly sample for each group
ADHCsampleTest <- ordered.index.add[sample(1:20, 3, replace=FALSE),]
ADsampleTest <- ordered.index.add[sample(21:37, 5, replace=FALSE),]
HCsampleTest <- ordered.index.add[sample(38:189, 33, replace=FALSE),]
SZsampleTest <- ordered.index.add[sample(228:254, 5, replace=FALSE),]
SZHCsampleTest <- ordered.index.add[sample(255:283, 2, replace=FALSE),]

#Add all of the groups together
m1 <- rbind(ADHCsampleTest, ADsampleTest)
m2 <- rbind(m1, HCsampleTest)
m3 <- rbind(m2, SZsampleTest)
TotalSampleTest <- rbind(m3, SZHCsampleTest)

#Get id's for these participants
x <- TotalSampleTest$id

#List of all participant id's
y <- as.vector(index['id']) 

#TESTING SET
row_numbers_test <- which(y$id %in% x, ) #gives all the row numbers for randomly selected test id's

test_set <- array(NaN, dim=c(length(row_numbers_test),9,24)) #empty array

count=1
for (i in row_numbers_test) {
test_set[count,,] <- tens_ar[row_numbers_test[count],,]
count=count+1
}
save(test_set,file="test_set_addedbyImogen.Rdata")

#TRAINING SET
#Take subset of activity patterns for training set
library(tidyverse)
total <-unique(y) #gets 283, as it should (dataframe)
test <- as.vector(TotalSampleTest['id']) #convert test id's to dataframe
train <- setdiff(total,test) #find training list id's

row_numbers_training <- which(y$id %in% train$id, )

training_set <- array(NaN, dim=c(length(row_numbers_training),9,24))

count=1
for (i in row_numbers_training) {
training_set[count,,] <- tens_ar[row_numbers_training[count],,]
count=count+1
}
save(training_set,file="training_set_addedbyImogen.Rdata")

#to find rows for individual id: row_numbers_training2 <- which(y$id ==75, ). Compare these rows to tens_ar. Also compare these to rows_numbers_training to find row number in training_set. Compare by eye