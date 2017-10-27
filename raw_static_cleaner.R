raw_static_cleaner<-function(filename)
{
  rawstatic<-read.csv(filename)
  library(plyr)
  library(dplyr)
  library(caret)
  
  rawstatic$AdmitSpecialty[rawstatic$AdmitSpecialty %in% c('Neurology','Neurosurgery','Vascular Surgery','DoNotMap')] <- 'Neurology'
  rawstatic$AdmitSpecialty[rawstatic$AdmitSpecialty %in% c('Surgery','Pulmonology')] <- 'Pulmonology'
  rawstatic$AdmitSpecialty[rawstatic$AdmitSpecialty %in% c('Hospitalist','Internal Medicine','Family Practice','Infectious Diseases')] <- 'Internal Medicine'
  rawstatic$AdmitSpecialty[rawstatic$AdmitSpecialty %in% c('Hematology-Oncology','OB-Gyn')] <-'OB-Gyn'
  rawstatic$MaritalStatus[rawstatic$MaritalStatus %in% c('DIVORCED','WIDOWED','SEPARATED','UTD')] <-'SEPARATED'
  rawstatic$EthnicGroup[rawstatic$EthnicGroup %in% c('AMERICAN INDIAN/ALASKA NATIVE','AFRICAN AMERICAN')] <-'AFRICAN AMERICAN'
  rawstatic$EthnicGroup[rawstatic$EthnicGroup %in% c('HISPANIC','UTD')] <-'HISPANIC'
  rawstatic$AdmitSpecialty=as.factor(as.character(rawstatic$AdmitSpecialty))
  rawstatic$MaritalStatus=as.factor(as.character(rawstatic$MaritalStatus))
  rawstatic$EthnicGroup=as.factor(as.character(rawstatic$EthnicGroup))
  
  dmy <- dummyVars(" ~AdmitSpecialty", data = rawstatic,levelsOnly = TRUE)
  trsf <- data.frame(predict(dmy, newdata = rawstatic))
  rawstatic<-cbind(rawstatic,trsf)
  rawstatic$AdmitSpecialty<-NULL
  dmy <- dummyVars(" ~MaritalStatus", data = rawstatic,levelsOnly = TRUE)
  trsf <- data.frame(predict(dmy, newdata = rawstatic))
  rawstatic<-cbind(rawstatic,trsf)
  rawstatic$MaritalStatus<-NULL
  rm(dmy)
  rm(trsf)
  
  rawstatic<-rawstatic[order(rawstatic$id),]  
  names(rawstatic)[1]="Episode"
  return(rawstatic)
}