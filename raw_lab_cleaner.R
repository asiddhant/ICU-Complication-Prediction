raw_lab_cleaner<-function(filename)
{
rawlab=read.csv(filename)

print("RawLabData Loaded... Cleaning RawLabData")

library(plyr)
library(dplyr)
rawlab<-select(rawlab,c(Episode,ObservationDate,ClientResult,Description))
rawlab$Description<-tolower(as.character(rawlab$Description))

rawlab$Description[rawlab$Description %in% c('urea nitrogen','blood urea nitrogen','bun')] <- 'bun'
rawlab$Description[rawlab$Description %in% c('poc semi-quant glucose result','glucose poc result')] <- 'glucose poc'
rawlab$Description[rawlab$Description %in% c('inorganic phosphorus','phophorus')] <- 'phosphorus'
rawlab$Description[rawlab$Description %in% c('hct','hematocrit')] <- 'hematocrit'
rawlab$Description[rawlab$Description %in% c('ast/sgot','ast (sgot)')] <- 'ast (sgot)'
rawlab$Description[rawlab$Description %in% c('hgb','hemoglobin')] <- 'hemoglobin'
rawlab$Description[rawlab$Description %in% c('poc troponin i result','troponin i')] <- 'troponin i'
rawlab$Description[rawlab$Description %in% c('alt/sgpt','alt (sgpt)')] <- 'alt (sgpt)'
rawlab$Description[rawlab$Description %in% c('white blood cell count','wbc')] <- 'wbc'
rawlab$Description[rawlab$Description %in% c('red blood cell count','rbc')] <- 'rbc'
rawlab$Description[rawlab$Description %in% c('carbon dioxide','co2 content')] <- 'carbon dioxide'
rawlab$Description[rawlab$Description %in% c('mean corpuscular hemoglobin','mch')] <- 'mch'
rawlab$Description[rawlab$Description %in% c('mean corpuscular hemoglobin concentratio','mchc')] <- 'mchc'
rawlab$Description[rawlab$Description %in% c('mean corpuscular volume','mcv')] <- 'mcv'
rawlab$Description[rawlab$Description %in% c('red cell distribution width','rdw')] <- 'rdw'
rawlab$Description[rawlab$Description %in% c('partial thromboplastin time','aptt')] <- 'aptt'
rawlab$Description[rawlab$Description %in% c('poc pco2(pt temp)','poc pco2','pco2m','pco2c','pco2')] <- 'pco2'
rawlab$Description[rawlab$Description %in% c('poc po2 (pt temp)','poc po2','po2m','po2c','po2')] <- 'po2'
rawlab$Description[rawlab$Description %in% c('poc so2','so2m','so2c')] <- 'pso2'
rawlab$Description[rawlab$Description %in% c('poc ph(pt temp)','poc ph','phm','phc')] <- 'ph'
rawlab$Description[rawlab$Description %in% c('poc hco3','hco3a','hco3')] <- 'phco3'
rawlab$Description[rawlab$Description %in% c('poc lactic acid','lactic acid','fluid lactic acid','lact')] <- 'lact'
rawlab$Description[rawlab$Description %in% c('total bilirubin','direct bilirubin')] <- 'bilirubin'
rawlab$Description[rawlab$Description %in% c('urine nitrite','nitrites')] <- 'urine nitrite'
rawlab$Description[rawlab$Description %in% c('urine ph','ph urine')] <- 'urine ph'
rawlab$Description[rawlab$Description %in% c('absolute neutrophil count automated','absolute neutrophil')] <- 'absolute neutrophil'
rawlab$Description[rawlab$Description %in% c('c reactive protein','c-reactive protein','c reactive protein inflamation')] <- 'c reactive protein'
rawlab$Description[rawlab$Description %in% c('ck mb','ckmb')] <- 'ckmb'

labs_imp<-c("glucose poc","potassium","glucose","hematocrit","sodium","calcium","bun","chloride",
            "carbon dioxide","anion gap","hemoglobin","rbc","wbc","platelet count","c reactive protein inflamation",                      
            "mcv","rdw","mch","creatinine","mchc","mpv","magnesium","glomerular filtration rate cal",
            "albumin","phosphorus","prothrombin time","inr","hemolysis index","lipemia index","fingerstick glucose",                        
            "icteric index","total protein","bilirubin","alkaline phosphatase","aptt","ast (sgot)",             
            "alt (sgpt)","globulin","pco2","po2","phco3","troponin i","lymphocytes","ph","monocytes","eosinophils",                             
            "neutrophils","basophils","absolute neutrophil count automated","ig %","basophils # (auto)",
            "basophils % (auto)","eosinophils # (auto)","eosinophils % (auto)","urine ph","lact", 
            "immature granulocyte","immature granulocyte #","lymphocytes # (auto)","lymphocytes % (auto)",
            "monocytes # (auto)","monocytes % (auto)","neutrophils # (auto)","neutrophils % (auto)",
            "poc patient temperature","creatinine (enz)","urine ph","lact","creatine kinase","fibrinogen",                              
            "uric acid","triglycerides","cholesterol","specific gravity","ph urine","chol/hdl ratio",                          
            "hdl cholesterol","ldl cholesterol","c.difficile toxin","absolute lymphocyte","absolute basophil",                       
            "absolute eosinophil","absolute monocyte","absolute neutrophil","c reactive protein",                      
            "c-reactive protein","urine protein","d-dimer","c. difficile dna pcr","bun/creat ratio","bnp",
            "osmolality calculation","bevt","lipase","ldh","ckmb","thyroid stimulating hormone",
            "albumin/globulin ratio","b-peptide","pso2")

rawlab <- rawlab[rawlab$Description %in% labs_imp,]
rawlab$Unique=paste(rawlab$Episode,rawlab$ObservationDate,rawlab$Description,sep="_")
rawlab=rawlab[!duplicated(rawlab$Unique),]
rawlab$Unique<-NULL

labs_imp_num<-c("glucose poc","potassium","glucose","hematocrit","sodium","calcium","bun","chloride",
            "carbon dioxide","anion gap","hemoglobin","rbc","wbc","platelet count","c reactive protein inflamation",                      
            "mcv","rdw","mch","creatinine","mchc","mpv","magnesium","glomerular filtration rate cal",
            "albumin","phosphorus","prothrombin time","inr","fingerstick glucose","b-peptide","pso2",                
            "total protein","bilirubin","alkaline phosphatase","aptt","ast (sgot)","albumin/globulin ratio",          
            "alt (sgpt)","globulin","pco2","po2","phco3","troponin i","lymphocytes","ph","monocytes","eosinophils",                             
            "neutrophils","basophils","absolute neutrophil count automated","ig %","basophils # (auto)",
            "basophils % (auto)","eosinophils # (auto)","eosinophils % (auto)","urine ph","lact", 
            "immature granulocyte","immature granulocyte #","lymphocytes # (auto)","lymphocytes % (auto)",
            "monocytes # (auto)","monocytes % (auto)","neutrophils # (auto)","neutrophils % (auto)",
            "poc patient temperature","creatinine (enz)","urine ph","lact","creatine kinase","fibrinogen",                              
            "uric acid","triglycerides","cholesterol","specific gravity","ph urine","chol/hdl ratio",                          
            "hdl cholesterol","ldl cholesterol","absolute lymphocyte","absolute basophil",                       
            "absolute eosinophil","absolute monocyte","absolute neutrophil","c reactive protein",                      
            "c-reactive protein","urine protein","d-dimer","bun/creat ratio","bnp",
            "osmolality calculation","bevt","lipase","ldh","ckmb","thyroid stimulating hormone")

labs_imp_fact<-c("hemolysis index","lipemia index","icteric index","c. difficile dna pcr","c.difficile toxin")

rawlab1 <- rawlab[rawlab$Description %in% labs_imp_num,]
rawlab1$ClientResult<-as.character(rawlab1$ClientResult)
rawlab1$ClientResult<-gsub('< |> |<= |>= |<|>|< |> | mg/dL| g/dL','',rawlab1$ClientResult)
rawlab1$ClientResult<-gsub('0-1','0.5',rawlab1$ClientResult)
rawlab1$ClientResult<-gsub('0-2','1',rawlab1$ClientResult)
rawlab1$ClientResult<-gsub('0-3','1.5',rawlab1$ClientResult)
rawlab1$ClientResult<-gsub('15-30','22.5',rawlab1$ClientResult)
rawlab1$ClientResult<-gsub('20-25','22.5',rawlab1$ClientResult)
rawlab1$ClientResult<-gsub('20-30','25',rawlab1$ClientResult)
rawlab1$ClientResult<-gsub('20-40','30',rawlab1$ClientResult)
rawlab1$ClientResult<-gsub('20-50','35',rawlab1$ClientResult)
rawlab1$ClientResult<-gsub('25-40','32.5',rawlab1$ClientResult)
rawlab1$ClientResult<-gsub('30-40','35',rawlab1$ClientResult)
rawlab1$ClientResult<-gsub('40-60','50',rawlab1$ClientResult)
rawlab1$ClientResult<-gsub('50-60','55',rawlab1$ClientResult)
rawlab1$ClientResult<-gsub('50-80','65',rawlab1$ClientResult)
rawlab1$ClientResult<-gsub('50-100','75',rawlab1$ClientResult)
rawlab1$ClientResult<-gsub('60-80','70',rawlab1$ClientResult)
rawlab1$ClientResult<-gsub('80-100','90',rawlab1$ClientResult)
rawlab1$ClientResult<-gsub('.0-2','0.1',rawlab1$ClientResult)
rawlab1$ClientResult<-gsub('.20-50','0.35',rawlab1$ClientResult)
rawlab1$ClientResult<-gsub('.5-10','0.75',rawlab1$ClientResult)
rawlab1$ClientResult<-as.numeric(rawlab1$ClientResult)
rawlab1<-rawlab1[!is.na(rawlab1$ClientResult),]

rawlab2<-rawlab[rawlab$Description %in% labs_imp_fact,]
rawlab2$ClientResult<-as.factor(as.character(rawlab2$ClientResult))
rawlab2<-rawlab2[!(as.factor(rawlab2$ClientResult)=="TNP"),]

rawlab<-rbind(rawlab1,rawlab2)
rm(labs_imp,labs_imp_num,labs_imp_fact,rawlab1,rawlab2)
rawlab$Unique=paste(rawlab$Episode,rawlab$ObservationDate,rawlab$Description,sep="_")
rawlab=rawlab[!duplicated(rawlab$Unique),]
rawlab$Unique<-NULL
count=as.data.frame(table(rawlab$Description))
count=count[order(-count$Freq),]

# write.csv(rawlab,"cleanlab.csv",row.names = FALSE)
# write.csv(count,"cleanlab_count.csv",row.names = FALSE)
rawlab$Description=as.factor(rawlab$Description)

print("Cleaning Complete... Creating Dummy Variables")
library(caret)
dummy1=dummyVars(~Description,data=rawlab,levelsOnly = TRUE)
dummy2=predict(dummy1,rawlab)
dummy2=as.data.frame(dummy2)
rawlab=cbind(rawlab,dummy2)
rm(dummy1,dummy2)

for (i in 5:ncol(rawlab))
{
  rawlab[,i]=ifelse(rawlab[,i]==1,rawlab[,3],NA)
  print(i)
}
rawlab$ClientResult=NULL
rawlab$Description=NULL

print("Dummy Variables Created... Aggregating... This may Take upto 10 minutes")

myfunction=function(x)
{
  result=ifelse(all(is.na(x)),NA,x[which(!is.na(x))])
  return(result)
}

rawlab_temp <-aggregate(rawlab[,3:ncol(rawlab)], by=list(rawlab$Episode,rawlab$ObservationDate), FUN=myfunction)
rawlab=rawlab_temp
rm(rawlab_temp)
names(rawlab)[1:2]=c("Episode","ObservationDate")
rawlab=rawlab[order(rawlab$Episode,rawlab$ObservationDate),]

return(rawlab)
}