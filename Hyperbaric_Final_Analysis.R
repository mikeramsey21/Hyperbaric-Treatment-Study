#############################################
# Michael Ramsey
# Date Created: 4/20/18
# Last Updated: 9/2/18
#############################################

# This is an R-script to perform data analysis of "Ear_data_cleaned.csv"
# The file contains patient data with common descriptors and the audiogram
# results before and after Hyperbaric Therapy. The data was cleaned from
# the file "ISSNHL Case Review raw data 05-2018.csv". I decided to clean and 
# extract the necessary information in excel, since the dataset is small.

####### Workspace Items #######
# data: The main patient file that was initially loaded
# tidy_data: Patient data corrseponding to one column con
# pre_data: Pre-treatment patient data
# post_data: Post-treatment patient data
# diff_data: Treatment improvement data (post_data - pre_data)
# mean_data: Data frame containing means of improvement by frequency

# trauma_data: Main data frame. Isolated data from traumatized ear
# patient_average: Data frame containing important summary statistics by patient

# frequency_tTest: Table of p-values for hearing improvement by frequency
# overall_tTest: t-test results of average patient hearing increase

# model: A linear model relating average hearing increase to treatment lag
# overallMean: the average hearing increase over all patients and freqencies
# Treatment_Timetable: A contingency table for 20db_Imp and treatment_lab < 10
#############################################

# Import necessary libraries
library(ggplot2)
library(reshape2)
library(dplyr)
library(tinytex)

# Load the data
data <- read.csv("Data/Ear_data_cleaned.csv", header = T, na.strings=c("", "NA", "nd","n/a"))

#############################################################
# Cleaning the data frame

# Rename the columns
colnames(data) <- c('ptid','age','gender','treatment_lag','affected_ear',
                       'full_recovery_exp','20db_imp','date_of_audiology',
                       'laterality','pre_post','125','250','500','750','1000','1500',
                       '2000','3000','4000','6000','8000')

# Delete 125 and 1500 hz column - not enough data
data$'125' <- NULL
data$'1500' <- NULL

# Why am I deleting these
#usedata <- usedata[-nrow(usedata),]
#usedata <- usedata[-nrow(usedata),]

# Change the gender column
data$gender[data$gender == 'male'] <- 'Male'

# Data frame editing complete

############################################################
# Manipulate data for ease of use 

# Get data in tidy format
tidy_data <- melt(data, 
                  id.vars = c('ptid', 'age', 'gender', 'treatment_lag', 
                              'affected_ear', 'full_recovery_exp','20db_imp',
                              'date_of_audiology','laterality','pre_post'), 
                  measure.vars = c('250','500','750','1000','2000',
                                   '3000','4000','6000','8000'))

# Rename the columns for ease of calling variables
# frequency: the frequency of the audiogram measurement
# score: the audiogram measurement for the respective frequency
colnames(tidy_data)[colnames(tidy_data) == 'variable'] <- 'frequency'
colnames(tidy_data)[colnames(tidy_data) == 'value'] <- 'score'

# Create column "treat_ear", which indicates if the recorded measurement
# is for the traumatized ear (TRUE) or non-traumatized ear (FALSE)
tidy_data <- tidy_data %>% mutate(treat_ear = ifelse(affected_ear == 'Left' & 
                            laterality == 'left ear', T , ifelse(affected_ear == 'Right' & 
                            laterality == 'right ear', T, F)))

# Create two dataframes recording pre-treatment and post-treatment audiogram
# measurements
pre_data <- tidy_data %>% filter(pre_post == 'pre')
post_data <- tidy_data %>% filter(pre_post == 'post')

# Get the difference
diff_data <- pre_data %>% mutate(post = post_data$score, diff = score - post)
colnames(diff_data)[colnames(diff_data) == 'score'] <- 'pre'

# Data for scatter plot affected ear
trauma_data <- diff_data %>% filter(treat_ear == T)

# Calculate the mean hearing increase at each frequency level
# Calculate the overall mean
mean_data <- trauma_data %>% 
  group_by(frequency) %>% 
  summarise(diff = mean(diff, na.rm=T)) %>%
  mutate(ptid = "")
overallMean <- mean(trauma_data$diff, na.rm = T)

#################################################
# Data Visualization in black and white

# Scatter plot of patient improvement for the traumatized ear
# Note: Could not figure out how to delete the last legend column
ggplot(data=trauma_data, aes(y=diff, x = frequency, shape=as.factor(ptid))) + 
  geom_point(position=position_dodge(width=0.5)) + 
  ggtitle('Hearing Improvement by Frequency for traumatized ear') +
  xlab('Frequency (Hz)') +
  ylab('Hearing Improvement (Db)') +
  scale_shape_discrete(name="Patient",labels = c("1","2","3","4","5","6")) +
  geom_line(data = mean_data, color = 'black', group = 1,
            linetype = "dashed",show.legend=F)

# Boxplot of hearing improvment for the traumatized_ear vs.
# non-traumatized ear
ggplot(data=diff_data, aes(y=diff, x = frequency, fill=treat_ear)) + 
  geom_boxplot() + 
  ggtitle('Hearing Improvement for traumatized/non-traumatized ear') +
  xlab('Frequency (Hz)') +
  ylab('Hearing Increase (Db)') +
  scale_fill_manual(name = "Traumatized ear", values = c('white','grey'))

# Dotplot of hearing improvment for the traumatized_ear vs.
# non-traumatized ear
ggplot(data=diff_data, aes(y=diff, x = frequency, shape=treat_ear)) + 
  geom_jitter(width = .3,height = 0) + 
  ggtitle('Hearing Improvement for traumatized/non-traumatized ear') +
  xlab('Frequency (Hz)') +
  ylab('Hearing Improvement (Db)') +
  scale_shape_manual(name="Traumatized ear",values = c(1,2))

#################################################
# Data Visualization in color

# Scatter plot of patient improvement for the traumatized ear
# Note: Mean is presented via a dotted line
ggplot(data=trauma_data, aes(y=diff, x = frequency, color=as.factor(ptid))) + 
  geom_point(position=position_dodge(width=0.5)) + 
  ggtitle('Hearing Improvement by Frequency for traumatized ear') +
  xlab('Frequency (Hz)') +
  ylab('Hearing Improvement (Db)') +
  scale_color_discrete(name="Patient",labels = c("1","2","3","4","5","6")) +
  geom_line(data = mean_data, color = 'black', group = 1,
            linetype = "dashed")

# Boxplot of treated ear
ggplot(data=diff_data, aes(y=diff, x = frequency, color=treat_ear)) + 
  geom_boxplot() + 
  ggtitle('Hearing Improvement for traumatized/non-traumatized ear') +
  xlab('Frequency (Hz)') +
  ylab('Hearing Increase (Db)') +
  scale_color_discrete(name="Traumatized ear") 

# Dotplot for hearing improvment for the traumatized_ear vs.
# non-traumatized ear
ggplot(data=diff_data, aes(y=diff, x = frequency, color=treat_ear)) + 
  geom_jitter(width = .3,height = 0) + 
  ggtitle('Hearing Improvement for traumatized/non-traumatized ear') +
  xlab('Frequency (Hz)') +
  ylab('Hearing Improvement (Db)') +
  scale_color_manual(name="Traumatized ear",values = c('black','blue'))

# Another dotplot version
ggplot(data=diff_data, aes(y=diff, x = frequency, fill=treat_ear)) + 
  geom_dotplot(binaxis='y', stackdir='center') +
  ggtitle('Hearing Improvement for traumatized/non-traumatized ear') +
  xlab('Frequency (Hz)') +
  ylab('Hearing Improvement (Db)') +
  scale_fill_discrete(name="Traumatized ear")

#################################################
# Statistical analysis

# Construct the data frame "patient_average" that contains the average 
# hearing increase overall frequences by patient and other relevant data
# Create column for treatment less than 10 days or not
patient_average <- trauma_data %>% 
  group_by(ptid) %>%
  summarise(`20db_imp` = `20db_imp`[1], treatment_lag = treatment_lag[1],
            ave_inc = mean(diff, na.rm = T))
patient_average <- patient_average %>%
  mutate(treat10 = if_else(treatment_lag <= 10, 1, 0))

# Conduct a t.test on average increase over all frequencies
overall_tTest <- t.test(patient_average$ave_inc)
# No signigicance

# Conduct t-tests on the hearing improvement by frequency
frequency_tTest <- trauma_data %>%
  group_by(frequency) %>%
  summarise(pval = t.test(diff)[3])
# No significance

# Create contingency table for 20db improvement and treatment <10 days
Treatment_Timetable <- table(patient_average$treat10,patient_average$`20db_imp`,
                    dnn = c('Treatment Time','Improvement'))
Treatment_Timetable
fisher.test(Treatment_Timetable)
# Meaningless

# Create linear model for treatment lag and average hearing increase and plot
model <- lm(ave_inc ~ treatment_lag, data = patient_average, na.omit = T)
summary(model)
ggplot(data = patient_average, aes(x=treatment_lag, y = ave_inc, color = as.factor(ptid))) +
  geom_point(shape = 19) + 
  geom_abline(intercept = model$coefficients[1], slope = model$coefficients[2], col = 'red')+ 
  ggtitle("Treatment lag vs. Treatment effect") +
  xlab("Treatment Lag") + 
  ylab("Treatment Effect")
# Meaningless