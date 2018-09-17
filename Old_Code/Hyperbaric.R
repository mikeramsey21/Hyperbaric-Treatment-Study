# Michael Ramsey
# Hyperbaric Medicine
# 4/16/18

#######################################################
# This is a script to explore and analyze the data on
# patients whom were treated via a hyperbaric chamber
#######################################################

# Import necessary packages
library(ggplot2)
library(reshape2)

# Set the working directory
setwd(~/Hyperbaric_Med)

# Load the data
data <- read.csv("Patient.csv", header = T, na.strings=c("", "NA"))

# Tests on baseline characteristics of people

# Plot the average increase in hearing for each frequency
decinc <- colMeans(data[,23:33],na.rm = T)
dec <- c('125','250','500','750','1000','1500','2000','3000','4000','6000','8000')
qplot(as.factor(dec),decinc) +
  scale_x_discrete(limits=dec) +
  ggtitle('Average Hearing Improvement vs. Frequency') +
  xlab('Frequency (Hz)') +
  ylab('Average Increase (Db)')
# Could also find overall average increase

# Plot the actual increase data
subdata <- data[,23:33]
subdata <- t(subdata)
colnames(subdata) = data$ï..ID
subdata <- subdata[,complete.cases(subdata[5,])]
data_long <- melt(subdata)  # convert to long format
ggplot(data=data_long, aes(x=Var1, y=value, color=as.factor(Var2))) + 
  geom_point() + 
  ggtitle('Average Hearing Improvement vs. Frequency') +
  xlab('Frequency (Hz)') +
  ylab('Average Increase (Db)') +
  scale_color_discrete(name="Patient")

# T-tests on the decibal increase
t.test(data$X250hz)
t.test(data$X500hz)
t.test(data$X750hz)
t.test(data$X1000hz)
t.test(data$X1500hz)
t.test(data$X2000hz) # Significant
t.test(data$X3000hz)
t.test(data$X4000hz)
t.test(data$X6000hz)
t.test(data$X8000hz)

# Could also do t-test on the overall average increase

# Two sample t-test on gender and average improvement
# Two sample t-test on ear and average improvement
# Kruskal Wallace on age categories and average improvement

# Paired t-tests on severety categories and average improvement
# Another test kind of like an anova
# Kruskal Wallace on severety categories and average improvement

# Paper divided data into severity categories and did tests on 
# gender, age, and ear

# Could also do independence tests on counts at a threshold 

# Create a contingency table for improvement
Imp_freq <- table(data$X20db_imp, dnn = 'Improvement')
H0_Imp <- c(.5,.5)
chisq.test(Imp_freq,p = H0_Imp)

# Contingency table for improvement and age categories
# Contingency table for improvement and ear
# Contingency table for improvement and gender

# Contingency table for improvement and treatment <10 days
Treat_freq <- table(data$onset_to_consult_10,data$X20db_imp,dnn = c('Treatment Time','Improvement'))
chisq.test(Treat_freq,H0_Imp) 

# Create linear model for treatment time 
model <- lm(X500hz ~ onset_to_consult, data = data, na.omit = T)
summary(model)
qplot(data$onset_to_consult,data$X500hz) +
  geom_point(shape = 19) + 
  geom_abline(intercept = model$coefficients[1], slope = model$coefficients[2], col = 'red')+ 
  ggtitle("Treatment lag vs. Treatment effect") +
  xlab("Treatment Lag") + 
  ylab("Treatment Effect")
# Note treatment lag of 32 had listed lag >30

# Implement a mixed effects regression model
# I do not think this is a good thing to do. Maybe collect data
# after every treatment. However I do not think the hearing improvement
# woud be immediate. 

                   