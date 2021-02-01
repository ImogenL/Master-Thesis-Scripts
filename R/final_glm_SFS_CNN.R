#LSTM-VAE averages from: LSTM_averages_per_id
#SFS scores from: index.add (from indexv1), load("C:\\Users\\Ideapad320S\\
#Documents\\University\\Groningen\\Year 2\\Major project\\Data\\Nature Human 
#Behaviour\\data\\indexv1.Rdata"), Column 21-27: SFS data.
#Columns: SFS_indep, SFS_indep_c, SFS_INT, SFS_recr, SFS_WITH, SFS_pro, SFS_tot
#CNN-VAE scores from: index.add (from indexv1), V1-V4: Individual latent score 
#on the four dimensions as found by the VAE.

#LSTM-VAE scores: LSTM_averages_per_id= read.csv("C:\\Users\\Ideapad320S\\Documents\\University\\Groningen\\Year 2\\Major project\\Model\\Storing_final_results\\06_Average_latent_construct_scores\\LSTM_averages_per_id.csv")

# Take subset of patients that have SFS scores ----------------------------
#Should be 64

SFS_data <- data.frame(index.add[,1:4],index.add[,21:27], LSTM_averages_per_id[,1:5], index.add[,8:11])
SFS_data <- subset(SFS_data, SFS_data$label == "SZ control" | SFS_data$label == "Schizophrenia" | SFS_data$label == "AD control" | SFS_data$label == "Alzheimer's disease" | SFS_data$label == "Control")
SFS_data <- subset(SFS_data, !is.na(SFS_data$SFS_indep) | !is.na(SFS_data$SFS_indep_c) | !is.na(SFS_data$SFS_INT) | !is.na(SFS_data$SFS_recr) | !is.na(SFS_data$SFS_WITH) | !is.na(SFS_data$SFS_pro) | !is.na(SFS_data$SFS_tot))
#NB: some missing values in SFS_pro

# Correct SFS scores for age and gender -----------------------------------
#https://www.biostars.org/p/110251/

cor.test(SFS_data$SFS_indep, SFS_data$age)
plot(lm(SFS_data$SFS_indep ~ SFS_data$age))
## Fit model:
varlist <- names(SFS_data)[5:11]
models <- lapply(varlist, function(x) {
  lm(substitute(i ~ sex + age, list(i = as.name(x))), data = SFS_data)
})

cor.test(models[[1]]$residuals, SFS_data$age)

#Manually fix column 6 to include NA's
SFS_pro_NA <- is.na(SFS_data$SFS_pro)
res = 1
for (i in 1:64)  {
  if (SFS_pro_NA[i] == TRUE) {
    SFS_pro_NA[i] = NA
  } else  {
    SFS_pro_NA[i] = models[[6]]$residuals[res]
    res = res + 1 #iterate through the residuals
  }
}
models[[6]]$residuals = SFS_pro_NA

dfSFS_indep = data_frame(models[[1]]$residuals, models[[2]]$residuals, models[[3]]$residuals, models[[4]]$residuals, models[[5]]$residuals, models[[6]]$residuals, models[[7]]$residuals)

names(dfSFS_indep)[1] <- "SFS_indep_corr"     #perceived independence
names(dfSFS_indep)[2] <- "SFS_indep_c_corr"   #independence competence
names(dfSFS_indep)[3] <- "SFS_INT_corr"       #interpersonal
names(dfSFS_indep)[4] <- "SFS_recr_corr"      #recreation
names(dfSFS_indep)[5] <- "SFS_WITH_corr"      #withdrawal
names(dfSFS_indep)[6] <- "SFS_pro_corr"       #prosocial
names(dfSFS_indep)[7] <- "SFS_tot_corr"

# Create dataframe with corrected SFS scores, LSTM-VAE and CNN-VAE --------

SFScorr_LSTM_CNN <- data_frame(SFS_data, dfSFS_indep)
names(SFScorr_LSTM_CNN)[12] <- "LSTM1" 
names(SFScorr_LSTM_CNN)[13] <- "LSTM2" 
names(SFScorr_LSTM_CNN)[14] <- "LSTM3" 
names(SFScorr_LSTM_CNN)[15] <- "LSTM4"

names(SFScorr_LSTM_CNN)[17] <- "CNN1" 
names(SFScorr_LSTM_CNN)[18] <- "CNN2"
names(SFScorr_LSTM_CNN)[19] <- "CNN3"
names(SFScorr_LSTM_CNN)[20] <- "CNN4"

#write.csv(SFScorr_LSTM_CNN, 'SFScorr_LSTM_CNN.csv')

# Normality tests ---------------------------------------------------------
#http://www.sthda.com/english/wiki/normality-test-in-r
shapiro.test(SFScorr_LSTM_CNN$LSTM1)
shapiro.test(SFScorr_LSTM_CNN$LSTM2)
shapiro.test(SFScorr_LSTM_CNN$LSTM3)
shapiro.test(SFScorr_LSTM_CNN$LSTM4)

shapiro.test(SFScorr_LSTM_CNN$CNN1)
shapiro.test(SFScorr_LSTM_CNN$CNN2) #might not be normal, p-value = 0.0389
shapiro.test(SFScorr_LSTM_CNN$CNN3)
shapiro.test(SFScorr_LSTM_CNN$CNN4)

# Load data from csv ------------------------------------------------------
SFScorr_LSTM_CNN <- read.csv("C:\\Users\\Ideapad320S\\Documents\\University\\Groningen\\Year 2\\Major project\\Model\\Storing_final_results\\07_Comparison_to_SFS_and_CNN_VAE\\SFScorr_LSTM_CNN.csv")

# CNN: Use lapply to fit several GLM's (AIC for each CNN-VAE construct included in summary) -------
#https://stats.idre.ucla.edu/r/codefragments/looping_strings/

varlist <- names(SFScorr_LSTM_CNN)[13:16] #"LSTM1" "LSTM2" "LSTM3" "LSTM4"

models_CNN1 <- lapply(varlist, function(x) {
  glm(substitute(CNN1 ~ i, list(i = as.name(x))), data = SFScorr_LSTM_CNN, family = gaussian) #| SFScorr_LSTM_CNN[,2]
})

lapply(models_CNN1,summary)

models_CNN2 <- lapply(varlist, function(x) {
  glm(substitute(CNN2 ~ i, list(i = as.name(x))), data = SFScorr_LSTM_CNN, family = gaussian) #| SFScorr_LSTM_CNN[,2]
})

lapply(models_CNN2,summary)

models_CNN3 <- lapply(varlist, function(x) {
  glm(substitute(CNN3 ~ i, list(i = as.name(x))), data = SFScorr_LSTM_CNN, family = gaussian) #| SFScorr_LSTM_CNN[,2]
})

lapply(models_CNN3,summary)

models_CNN4 <- lapply(varlist, function(x) {
  glm(substitute(CNN4 ~ i, list(i = as.name(x))), data = SFScorr_LSTM_CNN, family = gaussian) #| SFScorr_LSTM_CNN[,2]
})

lapply(models_CNN4,summary)

# SFS: Use lapply to fit several GLM's (AIC for each SFS subdomain) ------------

varlist <- names(SFScorr_LSTM_CNN)[13:16] #"LSTM1" "LSTM2" "LSTM3" "LSTM4"
#1
models_SFS_indep_corr <- lapply(varlist, function(x) {
  glm(substitute(SFS_indep_corr ~ i, list(i = as.name(x))), data = SFScorr_LSTM_CNN, family = gaussian)
})
lapply(models_SFS_indep_corr,summary)
#2
models_SFS_indep_c_corr <- lapply(varlist, function(x) {
  glm(substitute(SFS_indep_c_corr ~ i, list(i = as.name(x))), data = SFScorr_LSTM_CNN, family = gaussian)
})
lapply(models_SFS_indep_c_corr,summary)
#3
models_SFS_INT_corr <- lapply(varlist, function(x) {
  glm(substitute(SFS_INT_corr ~ i, list(i = as.name(x))), data = SFScorr_LSTM_CNN, family = gaussian)
})
lapply(models_SFS_INT_corr,summary)
#4
models_SFS_recr_corr <- lapply(varlist, function(x) {
  glm(substitute(SFS_recr_corr ~ i, list(i = as.name(x))), data = SFScorr_LSTM_CNN, family = gaussian)
})
lapply(models_SFS_recr_corr,summary)
#5
models_SFS_WITH_corr <- lapply(varlist, function(x) {
  glm(substitute(SFS_WITH_corr ~ i, list(i = as.name(x))), data = SFScorr_LSTM_CNN, family = gaussian)
})
lapply(models_SFS_WITH_corr,summary)
#6
models_SFS_pro_corr <- lapply(varlist, function(x) {
  glm(substitute(SFS_pro_corr ~ i, list(i = as.name(x))), data = SFScorr_LSTM_CNN, family = gaussian)
})
lapply(models_SFS_pro_corr,summary)
#7
models_SFS_tot_corr <- lapply(varlist, function(x) {
  glm(substitute(SFS_tot_corr ~ i, list(i = as.name(x))), data = SFScorr_LSTM_CNN, family = gaussian)
})
lapply(models_SFS_tot_corr,summary)

