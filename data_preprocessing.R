#Collect data from nback_task_20221001.csv
nback.data=read.csv('nback_task_20221001.csv')[,c('Participant.Private.ID','Correct','Timed.Out')]
nback.data$Timed.Out[is.na(nback.data$Timed.Out)]=0;


#Compute the memory score and the total time outs during the Nback task
#install.packages('dplyr')
library(dplyr)
nback.data = nback.data |>
  group_by(Participant.Private.ID) |>
  summarise(Nback.MemoryScore = 10*mean(Correct),Nback.TimedOut = sum(Timed.Out))
View(nback.data)


#Collect data from intonation_feedback_20221001.csv and use it to compute the learning ability
feedback.data=read.csv('intonation_feedback_20221001.csv')[,c('Participant.Private.ID','Correct','Trial.Number')]

feedback.data = feedback.data[feedback.data$Trial.Number<=17,] |>
  group_by(Participant.Private.ID) |>
  summarise(Learning.Ability = 10*mean(Correct))
View(feedback.data)

#Collect data from intonation_task_20221001.csv
task.data=read.csv('intonation_task_20221001.csv')[,c('Participant.Private.ID','Correct','Response','answer','Trial.Number','sound')]
FinalDataSet= merge(task.data,nback.data,by="Participant.Private.ID")
FinalDataSet= merge(FinalDataSet,feedback.data,by="Participant.Private.ID")

#Convert the variable "sound" into data about the tone variation:
#the first digit represents the prototype sound, the second represents the amount of semitones
#the last one represents the locatino of the variation

#Extract Intensity of variation
FinalDataSet$Intensity=substr(FinalDataSet$sound,9,9)
FinalDataSet$Intensity[FinalDataSet$Intensity=='e']=0
FinalDataSet$Intensity=sapply(FinalDataSet$Intensity,as.numeric)

#Extract Location of variation
library(readr)
FinalDataSet$Location=substr(FinalDataSet$sound,12,13)
FinalDataSet$Location[FinalDataSet$Location=='on']=NA
FinalDataSet$Location=sapply(FinalDataSet$Location,parse_number)
FinalDataSet$LocationDychotomous=FinalDataSet$Location>7

#Only select participants who are comfortable with English
data=read.csv('questionnaire_20221001.csv')[,c('Participant.Private.ID','Language.Background')]
ViableParticipants=data$Participant.Private.ID[data$Language.Background==1]
FinalDataSet=FinalDataSet[FinalDataSet$Participant.Private.ID %in% ViableParticipants,]


#Remove ".png" athe end of "Answer" and "Response" in the dataset
FinalDataSet[,'answer']=gsub(".png", "", FinalDataSet[,'answer'])
FinalDataSet[,'Response']=gsub(".png", "", FinalDataSet[,'Response'])

#Add the dycothomy of whether the pattern is known or not
FinalDataSet$KnownPattern=!(FinalDataSet$answer=='Others')

#Save dataset
write.table(FinalDataSet, file = "preprocessed_data.csv", row.names = F, sep=',')
View(FinalDataSet)
