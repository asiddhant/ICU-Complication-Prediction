all_merger=function(vital_file,labs_file,rx_file)  
{
  vitals_new<-vital_file[,1:2]
  labdata<-labs_file
  rxdata=rx_file
  
  print("Aggregating Lab... This May take upto 7 minutes")
  
  myfunction=function(x)
  {
    return(ifelse(all(is.na(x)),NA,x[max(which(!is.na(x)))]))
  }
  
  myfunction2<-function(x)
  {
    return(ifelse(x-max(a)<0,a[which(x<a)[1]],x))
  }
  
  for(i in 1:length(vitals_new$Episode))
  {
    print(i)
    a=as.numeric(unlist(vitals_new[i,]$ObservationDate))
    index1=which(labdata$Episode==vitals_new$Episode[i])
    index2=which(rxdata$Episode==vitals_new$Episode[i])
    temp1=apply(t(labdata[index1,2]),2,FUN=myfunction2)
    temp2=apply(t(rxdata[index2,2]),2,FUN=myfunction2)
    labdata[index1,2]=temp1
    rxdata[index2,2]=temp2
  }
  
  labs_file<-aggregate(labdata[,3:ncol(labdata)],by=list(labdata$Episode,labdata$ObservationDate),FUN=myfunction)
  names(labs_file)=names(labdata)
  labs_file<-labs_file[order(labs_file$Episode,labs_file$ObservationDate),]
  agg1=labs_file
  rx_file<-aggregate(rxdata[,3:ncol(rxdata)],by=list(rxdata$Episode,rxdata$ObservationDate),FUN=myfunction)
  names(rx_file)=names(rxdata)
  rx_file<-rx_file[order(rx_file$Episode,rx_file$ObservationDate),]
  agg2=rx_file
  rm(labdata,rxdata)
  n_labs=ncol(labs_file)-2
  n_rx=ncol(rx_file)-2
  print("Merging Labs and Vitals TimeStamp... This may take upto 1 hour...")
  temp=data.frame(matrix(nrow=nrow(vital_file),ncol=ncol(labs_file)+ncol(rx_file)-4))
  names(temp)=c(names(labs_file)[3:ncol(labs_file)],names(rx_file)[3:ncol(rx_file)])
  temp2=vital_file$ObservationDate
  temp3=temp2
  for(i in 1:nrow(temp))
  {
    temp2[[i]]=ts(rep(NA,tsp(temp2[[i]])[2]))
  }
  for(i in 1:nrow(temp))
  {
    temp3[[i]]=ts(rep(0,tsp(temp3[[i]])[2]))
  }
  
  for(i in 1:n_labs)
  {
    temp[[i]]=temp2
  }
  
  for(i in (n_labs+1):(n_labs+n_rx))
  {
    temp[[i]]=temp3
  }
  
  vital_file=cbind(vital_file,temp)
  
  vitals_no=3:9
  labs_no=tail(vitals_no,1)+(1:n_labs)
  rx_no=tail(labs_no,1)+(1:n_rx)
  rm(temp,temp2,temp3)
  
  count=0
  pat=unique(c(labs_file$Episode,rx_file$Episode))
  for(i in pat)
  {
    temp=vital_file[toString(i),2:9]
    len=length(unlist(temp$ObservationDate))
    temp2=matrix(nrow=len,ncol = ncol(temp))
    for (j in 1:8)
    {
      temp2[,j]=unlist(temp[,j])
    }
    #temp2[,1]=rep(temp[,1],len)
    temp2=as.data.frame(temp2)
    names(temp2)=names(temp)
    
    temp3=subset(labs_file[,2:ncol(labs_file)],labs_file$Episode==i)
    temp4=subset(rx_file[2:ncol(rx_file)],rx_file$Episode==i)
    temp5=merge(temp2,temp3,by="ObservationDate",all = TRUE)

    temp6=merge(temp5,temp4,by="ObservationDate",all = TRUE)
  
    for(k in rx_no-1)
    {
      temp6[,k]=ifelse(is.na(temp6[,k]),0,temp6[,k])
      temp6[,k]=ifelse(cumsum(temp6[,k])>0,1,0)
    }
    for(k in 1:ncol(temp6))
    {
      vital_file[[k+1]][[toString(i)]]=as.ts(temp6[,k])
    }
    count=count+1
    per=count/length(pat)
    print(per)
  }
  
  return(list(vital_file,vitals_no,labs_no,rx_no))
}