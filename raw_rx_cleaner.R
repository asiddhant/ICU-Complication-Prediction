raw_rx_cleaner<-function(filename)
{
  rawrx<-read.csv(filename)
  library(plyr)
  library(dplyr)
  library(stringr)
  library(caret)
  rawrx<-select(rawrx,c(Episode,CreatedDateRelSeconds,Description))
  rawrx$Description<-tolower(as.character(rawrx$Description))
  rawrx$Description<-sub('tpn ','',rawrx$Description)
  rawrx$Description<-sub('lvp   - ','',rawrx$Description)
  rawrx$Description<-sub('ivpb  - ','',rawrx$Description)
  rawrx$Description<-sub('/|,|-',' ',rawrx$Description)
  Medicines<-word(rawrx$Description,1)
  rawrx<-cbind(rawrx,Medicines)
  rm(Medicines)
  rawrx$Description<-NULL
  rawrx$Medicines[rawrx$Medicines %in% c('amox','amoxicillin')] <- 'amoxicillin'
  rawrx$Medicines[rawrx$Medicines %in% c('enalapril','enalaprilat')] <- 'enalapril'
  rawrx$Medicines[rawrx$Medicines %in% c('eplerenone','epleronone')] <- 'eplerenone'
  rawrx$Unique=paste(rawrx$Episode,rawrx$CreatedDateRelSeconds,rawrx$Medicines,sep="_")
  rawrx=rawrx[!duplicated(rawrx$Unique),]
  rawrx$Unique<-NULL
  meds_imp<-c("nalbuphine","norco","naproxen","sulfa","tapentadol","cefoxitin","lansoprazole",     
              "glimepiride","sitagliptin","moxifloxacin","gemfibrozil","cefuroxime","percocet",      
              "clindamycin","meperidine","butorphanol","meloxicam","rivaroxaban","esmolol",       
              "glyburide","penicillin","timolol","lortab","belladonna","carisoprodol","dabigatran",  
              "cephalexin","metolazone","erythromycin","gentamicin","olmesartan","torsemide",     
              "glipizide","triamterene","ezetimibe","benazepril","bivalirudin","fenofibrate",   
              "imipenem","niacin","ibuprofen","spironolactone","cefazolin","rosuvastatin",  
              "linezolid","cefepime","enalapril","propranolol","meropenem","celecoxib",     
              "ciprofloxacin","cholestyramine","amlodipine","azithromycin","levofloxacin","tmp",           
              "ampicillin","nadolol","doxycycline","piperacillin","nebivolol","doripenem",     
              "rifaximin","ceftazidime","sotalol","methadone","nitrofurantoin","bumetanide")

  rawrx<-subset(rawrx,as.character(rawrx$Medicines) %in% meds_imp)
  rawrx$Medicines=as.factor(as.character(rawrx$Medicines))
  
  dummy1=dummyVars(~Medicines,data=rawrx,levelsOnly = TRUE)
  dummy2=data.frame(predict(dummy1,rawrx))
  rawrx=cbind(rawrx,dummy2)
  rawrx$Medicines<-NULL
  rm(dummy1,dummy2)
  
  aggdata <-aggregate(rawrx[,3:70], by=list(rawrx$Episode,rawrx$CreatedDateRelSeconds), FUN=sum, na.rm=TRUE)
  names(aggdata)<-names(rawrx)
  rawrx<-aggdata
  rm(aggdata)
  rawrx<-rawrx[order(rawrx$Episode),]
  names(rawrx)[2]="ObservationDate"
  
  #write.csv(rawrx,"cleanrx.csv",row.names = FALSE)
  return(rawrx)
}