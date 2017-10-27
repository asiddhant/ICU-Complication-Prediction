raw_label_maker<-function(vitals_ts,filename)
{
  labels<-read.csv(filename)
  
  temp=vitals_ts$ObservationDate
  for (i in 1:nrow(vitals_ts))
  {
    temp[[i]]=ts(rep(0,length(vitals_ts$ObservationDate[[i]])))
  }
  
  mat=as.data.frame(matrix(nrow = nrow(vitals_ts),ncol=ncol(labels)-1))
  names(mat)=names(labels)[2:ncol(labels)]
  for(j in 1:ncol(mat))
  {
    patients=labels[!is.na(labels[[j+1]]),]$id
    mat[[j]]=temp
    for( i in patients)
    {
      temp_timer=vitals_ts$ObservationDate[[toString(i)]]
      time=subset(labels,labels$id==i)[[j+1]]
      temp_akhil=ifelse(temp_timer+172800>=time,1,0)
      mat[[j]][[toString(i)]]=temp_akhil
    }
  }
  
  vitals_ts=cbind(vitals_ts,mat)
  return(vitals_ts)
}