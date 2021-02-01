#need to do for training and testing combined
LSTMConstructs_train_set = read.csv("C:\\Users\\Ideapad320S\\Documents\\University\\Groningen\\Year 2\\Major project\\Model\\LSTMConstructs_train-set.csv")
LSTMConstructs_test_set = read.csv("C:\\Users\\Ideapad320S\\Documents\\University\\Groningen\\Year 2\\Major project\\Model\\LSTMConstructs_test-set.csv")
LSTM_constructs <- rbind(LSTMConstructs_train_set, LSTMConstructs_test_set)

#add id column to LSTM constructs
#(row_numbers_training has the index (index$index). Need to take index$id that corresponds to index$index provided by row_numbers_training)
id_training = subset(index$id, index$index %in% row_numbers_training)
id_test = subset(index$id, index$index %in% row_numbers_test)
id_train_test <- data.frame(id = c(id_training, id_test))
LSTM_constructs_id = cbind(LSTM_constructs, id_train_test)

#Calculate averages for each construct per participant id (NB this method appears to sort the rows by id)
av1 <-aggregate(LSTM_constructs_id$X0, by=list(LSTM_constructs_id$id),
  FUN=mean, na.rm=TRUE)
av2 <-aggregate(LSTM_constructs_id$X1, by=list(LSTM_constructs_id$id),
  FUN=mean, na.rm=TRUE)
av3 <-aggregate(LSTM_constructs_id$X2, by=list(LSTM_constructs_id$id),
  FUN=mean, na.rm=TRUE)
av4 <-aggregate(LSTM_constructs_id$X3, by=list(LSTM_constructs_id$id),
  FUN=mean, na.rm=TRUE)


#Create dataframe containing the averages for each participant
LSTM_averages_per_id = cbind(av1$x, av2$x, av3$x, av4$x, av4$Group.1)
write.csv(LSTM_averages_per_id,'LSTM_averages_per_id.csv')

