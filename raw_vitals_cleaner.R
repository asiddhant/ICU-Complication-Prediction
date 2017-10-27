raw_vitals_cleaner=function(filename)
{
  #Read the file from Working Directory
  rawvitals=read.csv(filename)
  
  print("File Loaded... Beginning PreProcessing")
  
  #Remove Unneccesary Variables
  rawvitals$SequenceNum=NULL
  
  #Remove Duplicated
  rawvitals$Unique=paste(rawvitals$Episode,rawvitals$ObservationDate,rawvitals$Measure,sep="_")
  rawvitals=rawvitals[ !duplicated(rawvitals$Unique), ]
  rawvitals$Unique=NULL
  
  print("Duplicates Removed... Creating Dummy Variables")
  
  library(caret)
  dummy1=dummyVars(~Measure,data=rawvitals,levelsOnly = TRUE)
  dummy2=predict(dummy1,rawvitals)
  dummy2=as.data.frame(dummy2)
  rawvitals=cbind(rawvitals,dummy2)
  
  rm(dummy1,dummy2)
  rawvitals$Measure=NULL
  
  print("Dummy Variables Created... Filling in Values")
  
  rawvitals=as.matrix(rawvitals)
  for (i in 5:10)
  {
    rawvitals[,i]=ifelse(rawvitals[,i]==1,rawvitals[,3],NA)
  }
  rm(i)
  rawvitals=as.data.frame(rawvitals)
  rawvitals$Result=NULL
  
  print("Aggregating... This may take upto 5 minutes")
  
  OVV=function(x)
  {
    result=ifelse(all(is.na(x)),NA,x[min(which(!is.na(x)))])
    return(result)
  }
  
  rawvitals_temp = aggregate(rawvitals[,3:ncol(rawvitals)], by=list(rawvitals$Episode,rawvitals$ObservationDate), FUN=OVV)
  rawvitals=rawvitals_temp
  rm(rawvitals_temp)
  
  names(rawvitals)=c("Episode","ObservationDate","ICU","BP.DIASTOLIC","BP.SYSTOLIC","HEART.RATE",
                     "O2.SATURATION","RESPIRATION.RATE","TEMPERATURE")
  
  rawvitals=rawvitals[order(rawvitals$Episode),]
  row.names(rawvitals)=1:nrow(rawvitals)
  
  
  print("Rolling Data into Time Series...")
  id=unique(rawvitals$Episode)
  len=length(id)
  vitals_seq=row.names(rawvitals[ !duplicated(rawvitals$Episode),])
  vitals_seq[len+1]=nrow(rawvitals)+1
  vitals_seq=as.numeric(vitals_seq)
  
  
  
  ObservationDate=list()
  for (i in 1:len)
  {
    temp=as.vector(rawvitals$ObservationDate[vitals_seq[i]:(vitals_seq[i+1]-1)])
    temp=ts(temp)
    ObservationDate[[toString(id[i])]]=temp
  }
  rm(temp,i)
  
  BP.SYSTOLIC=list()
  for (i in 1:len)
  {
    temp=as.vector(rawvitals$BP.SYSTOLIC[vitals_seq[i]:(vitals_seq[i+1]-1)])
    temp=ts(temp)
    BP.SYSTOLIC[[toString(id[i])]]=temp
  }
  rm(temp,i)
  
  BP.DIASTOLIC=list()
  for (i in 1:len)
  {
    temp=as.vector(rawvitals$BP.DIASTOLIC[vitals_seq[i]:(vitals_seq[i+1]-1)])
    temp=ts(temp)
    BP.DIASTOLIC[[toString(id[i])]]=temp
  }
  rm(temp,i)
  
  HEART.RATE=list()
  for (i in 1:len)
  {
    temp=as.vector(rawvitals$HEART.RATE[vitals_seq[i]:(vitals_seq[i+1]-1)])
    temp=ts(temp)
    HEART.RATE[[toString(id[i])]]=temp
  }
  rm(temp,i)
  
  TEMPERATURE=list()
  for (i in 1:len)
  {
    temp=as.vector(rawvitals$TEMPERATURE[vitals_seq[i]:(vitals_seq[i+1]-1)])
    temp=ts(temp)
    TEMPERATURE[[toString(id[i])]]=temp
  }
  rm(temp,i)
  
  O2.SATURATION=list()
  for (i in 1:len)
  {
    temp=as.vector(rawvitals$O2.SATURATION[vitals_seq[i]:(vitals_seq[i+1]-1)])
    temp=ts(temp)
    O2.SATURATION[[toString(id[i])]]=temp
  }
  rm(temp,i)
  
  RESPIRATION.RATE=list()
  for (i in 1:len)
  {
    temp=as.vector(rawvitals$RESPIRATION.RATE[vitals_seq[i]:(vitals_seq[i+1]-1)])
    temp=ts(temp)
    RESPIRATION.RATE[[toString(id[i])]]=temp
  }
  rm(temp,i)
  
  ICU=list()
  for (i in 1:len)
  {
    temp=as.vector(rawvitals$ICU[vitals_seq[i]:(vitals_seq[i+1]-1)])
    temp=ts(temp)
    ICU[[toString(id[i])]]=temp
  }
  rm(temp,i)
  
  Episode=id
  vitals_ts=data.frame(Episode,I(ObservationDate),I(ICU),I(BP.DIASTOLIC),I(BP.SYSTOLIC),I(HEART.RATE),I(O2.SATURATION),I(RESPIRATION.RATE),I(TEMPERATURE))
  rm(Episode,id,ObservationDate,BP.DIASTOLIC,BP.SYSTOLIC,HEART.RATE,O2.SATURATION,RESPIRATION.RATE,TEMPERATURE,ICU,len,vitals_seq,OVV)
  rm(rawvitals)
  
  return(vitals_ts)
}