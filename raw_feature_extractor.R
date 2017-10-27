raw_feature_extractor=function(vitals_ts,vitals_no,labs_no,med_no,normvalues)
{

print("Removing Outliers... This may Take upto 15 minutes")

for( j in 1:nrow(vitals_ts))
{
  if (!all(is.na(vitals_ts$BP.DIASTOLIC[[j]])))
  {
    temp=vitals_ts$BP.DIASTOLIC[[j]]
    temp2=boxplot(vitals_ts$BP.DIASTOLIC[[j]])$out
    dev.off()
    
    if(length(temp2)!=0)
    {
      temp[which(temp %in% temp2)]=NA
      vitals_ts$BP.DIASTOLIC[[j]]=temp
    }
  }
}

for( j in 1:nrow(vitals_ts))
{
  if (!all(is.na(vitals_ts$BP.SYSTOLIC[[j]])))
  {
    temp=vitals_ts$BP.SYSTOLIC[[j]]
    temp2=boxplot(vitals_ts$BP.SYSTOLIC[[j]])$out
    dev.off()
    
    if(length(temp2)!=0)
    {
      temp[which(temp %in% temp2)]=NA
      vitals_ts$BP.SYSTOLIC[[j]]=temp
    }
  }
}

for( j in 1:nrow(vitals_ts))
{
  if (!all(is.na(vitals_ts$HEART.RATE[[j]])))
  {
    temp=vitals_ts$HEART.RATE[[j]]
    temp2=boxplot(vitals_ts$HEART.RATE[[j]])$out
    dev.off()
    
    if(length(temp2)!=0)
    {
      temp[which(temp %in% temp2)]=NA
      vitals_ts$HEART.RATE[[j]]=temp
    }
  }
}

for( j in 1:nrow(vitals_ts))
{
  if (!all(is.na(vitals_ts$RESPIRATION.RATE[[j]])))
  {
    temp=vitals_ts$RESPIRATION.RATE[[j]]
    temp2=boxplot(vitals_ts$RESPIRATION.RATE[[j]])$out
    dev.off()
    
    if(length(temp2)!=0)
    {
      temp[which(temp %in% temp2)]=NA
      vitals_ts$RESPIRATION.RATE[[j]]=temp
    }
  }
}

for( j in 1:nrow(vitals_ts))
{
  if (!all(is.na(vitals_ts$O2.SATURATION[[j]])))
  {
    temp=vitals_ts$O2.SATURATION[[j]]
    temp2=boxplot(vitals_ts$O2.SATURATION[[j]])$out
    dev.off()
    
    if(length(temp2)!=0)
    {
      temp[which(temp %in% temp2)]=NA
      vitals_ts$O2.SATURATION[[j]]=temp
    }
  }
}

for( j in 1:nrow(vitals_ts))
{
  if (!all(is.na(vitals_ts$TEMPERATURE[[j]])))
  {
    temp=vitals_ts$TEMPERATURE[[j]]
    temp2=boxplot(vitals_ts$TEMPERATURE[[j]])$out
    dev.off()
    
    if(length(temp2)!=0)
    {
      temp[which(temp %in% temp2)]=NA
      vitals_ts$TEMPERATURE[[j]]=temp
    }
  }
}

print("Imputing Data... This may take upto 5 minutes")

library(imputeTS)
vitals_unimputed=vitals_ts

for(i in 1:nrow(vitals_ts))
{
  
    for(j in c(vitals_no,labs_no))
    {
      if(is.na(vitals_ts[[j]][[i]][1]))
        vitals_ts[[j]][[i]][1]=normvalues[[j]]
      if(length(vitals_ts$ObservationDate[[i]])>1)
      {
      vitals_ts[[j]][[i]]=na.locf(vitals_ts[[j]][[i]])
      }
    }
  print(i)
}



print("Extracting Features... This may take upto 15 minutes")

# Get all IDs
all_id = vitals_ts$Episode

# Computing Moving average for each ID of all Vitals

mean_BPD = list()
mean_SYS = list()
mean_HRATE = list()
mean_RRATE = list()
mean_O2SAT = list()
mean_TEM = list()

min_BPD = list()
min_SYS = list()
min_HRATE = list()
min_RRATE = list()
min_O2SAT = list()
min_TEM = list()

max_BPD = list()
max_SYS = list()
max_HRATE = list()
max_RRATE = list()
max_O2SAT = list()
max_TEM = list()

Q1_BPD = list()
Q1_SYS = list()
Q1_HRATE = list()
Q1_RRATE = list()
Q1_O2SAT = list()
Q1_TEM = list()

Q3_BPD = list()
Q3_SYS = list()
Q3_HRATE = list()
Q3_RRATE = list()
Q3_O2SAT = list()
Q3_TEM = list()

first_BPD = list()
first_SYS = list()
first_HRATE = list()
first_RRATE = list()
first_O2SAT = list()
first_TEM = list()

var_BPD = list()
var_SYS = list()
var_HRATE = list()
var_RRATE = list()
var_O2SAT = list()
var_TEM = list()

diff_nor_BPD = list()
diff_nor_SYS = list()
diff_nor_HRATE = list()
diff_nor_RRATE = list()
diff_nor_O2SAT = list()
diff_nor_TEM = list()

#Prints all overall averages

for (id in all_id)
{
  id = as.character(id)
  temp = as.double(unlist(vitals_ts$BP.DIASTOLIC[id]))
  mean_BPD[[id]] = ts(cumsum(temp)/(1:length(temp)))
  min_BPD[[id]] = ts(cummin(temp))
  max_BPD[[id]] = ts(cummax(temp))
  Q1_BPD[[id]] = mean_BPD[[id]] * 0.3 + min_BPD[[id]] * 0.7
  Q3_BPD[[id]] = mean_BPD[[id]] * 0.3 + max_BPD[[id]] * 0.7
  first_BPD[[id]] = rep(temp[1], length(temp))
  var_BPD[[id]] = ts((cumsum((temp - mean_BPD[[id]])^2))/(1:length(temp)))
  diff_nor_BPD[[id]] = (temp - rep(80, length(temp)))^2
  
  
  temp = as.double(unlist(vitals_ts$BP.SYSTOLIC[id]))
  mean_SYS[[id]] = ts(cumsum(temp)/(1:length(temp)))
  min_SYS[[id]] = ts(cummin(temp))
  max_SYS[[id]] = ts(cummax(temp))
  Q1_SYS[[id]] = mean_SYS[[id]] * 0.3 + min_SYS[[id]] * 0.7
  Q3_SYS[[id]] = mean_SYS[[id]] * 0.3 + max_SYS[[id]] * 0.7
  first_SYS[[id]] = rep(temp[1], length(temp))
  var_SYS[[id]] = ts((cumsum((temp - mean_SYS[[id]])^2))/(1:length(temp)))
  diff_nor_SYS[[id]] = (temp - rep(120, length(temp)))^2
  
  
  temp = as.double(unlist(vitals_ts$HEART.RATE[id]))
  mean_HRATE[[id]] = ts(cumsum(temp)/(1:length(temp)))
  min_HRATE[[id]] = ts(cummin(temp))
  max_HRATE[[id]] = ts(cummax(temp))
  Q1_HRATE[[id]] = mean_HRATE[[id]] * 0.3 + min_HRATE[[id]] * 0.7
  Q3_HRATE[[id]] = mean_HRATE[[id]] * 0.3 + max_HRATE[[id]] * 0.7
  first_HRATE[[id]] = rep(temp[1], length(temp))
  var_HRATE[[id]] = ts((cumsum((temp - mean_HRATE[[id]])^2))/(1:length(temp)))  
  diff_nor_HRATE[[id]] = (temp - rep(72, length(temp)))^2
  
  
  temp = as.double(unlist(vitals_ts$RESPIRATION.RATE[id]))
  mean_RRATE[[id]] = ts(cumsum(temp)/(1:length(temp)))
  min_RRATE[[id]] = ts(cummin(temp))
  max_RRATE[[id]] = ts(cummax(temp))
  Q1_RRATE[[id]] = mean_RRATE[[id]] * 0.3 + min_RRATE[[id]] * 0.7
  Q3_RRATE[[id]] = mean_RRATE[[id]] * 0.3 + max_RRATE[[id]] * 0.7
  first_RRATE[[id]] = rep(temp[1], length(temp))
  var_RRATE[[id]] = ts((cumsum((temp - mean_RRATE[[id]])^2))/(1:length(temp)))
  diff_nor_RRATE[[id]] = (temp - rep(16, length(temp)))^2
  
  
  temp = as.double(unlist(vitals_ts$O2.SATURATION[id]))
  mean_O2SAT[[id]] = ts(cumsum(temp)/(1:length(temp)))
  min_O2SAT[[id]] = ts(cummin(temp))
  max_O2SAT[[id]] = ts(cummax(temp))
  Q1_O2SAT[[id]] = mean_O2SAT[[id]] * 0.3 + min_O2SAT[[id]] * 0.7
  Q3_O2SAT[[id]] = mean_O2SAT[[id]] * 0.3 + max_O2SAT[[id]] * 0.7
  first_O2SAT[[id]] = rep(temp[1], length(temp))
  var_O2SAT[[id]] = ts((cumsum((temp - mean_O2SAT[[id]])^2))/(1:length(temp)))
  diff_nor_O2SAT[[id]] = (temp - rep(95, length(temp)))^2
  
  
  temp = as.double(unlist(vitals_ts$TEMPERATURE[id]))
  mean_TEM[[id]] = ts(cumsum(temp)/(1:length(temp)))
  min_TEM[[id]] = ts(cummin(temp))
  max_TEM[[id]] = ts(cummax(temp))
  Q1_TEM[[id]] = mean_TEM[[id]] * 0.3 + min_TEM[[id]] * 0.7
  Q3_TEM[[id]] = mean_TEM[[id]] * 0.3 + max_TEM[[id]] * 0.7
  first_TEM[[id]] = rep(temp[1], length(temp))
  var_TEM[[id]] = ts((cumsum((temp - mean_TEM[[id]])^2))/(1:length(temp)))
  diff_nor_TEM[[id]] = (temp - rep(98.6, length(temp)))^2
}

print("Extracting Feature... Step 1 of 4 Complete")

diff_time = list()
obs_time = list()

for (id in all_id)
{
  id = as.character(id)
  temp2 = as.POSIXlt(as.numeric((unlist(vitals_ts$ObservationDate[id]))),origin = "1970-01-01")
  temp = as.numeric(diff(temp2), units = "hours")
  diff_time[[id]] = ts(c(0,temp))
  obs_time[[id]] = ts(as.numeric(temp2 - rep(temp2[1],length(temp2)), units="hours"))
}

icu_flag=vitals_ts$ICU

# Calculate ICU Time
 icu_time = list()
 icu_time2 = list()

for (id in all_id)
{
  
  id = as.character(id)
  temp2 = as.numeric(diff_time[[id]])
  temp3 = as.numeric(icu_flag[[id]])
  temp5 = cumsum(temp2 * temp3)
  icu_time[[id]] = ts(temp5)
  
  temp = c(0,(diff(icu_flag[[id]]) == -1)) * diff_time[[id]]
  temp = (temp > 24)        # If 24 hours have passed after the patient left the ICU
  temp6 = cumsum(temp * icu_time[[id]])
  icu_time2[[id]] = (icu_time[[id]] - temp6)
}



# Number of TimeStamps available till the time
num_stamps = list()

for (id in all_id)
{
  id = as.character(id)
  num_stamps[[id]] = ts(1:length(vitals_ts$ObservationDate[id]))
}


m_num_stamps = list()
ind_stamps = list()

for (id in all_id)
{
  id = as.character(id)
  temp = as.numeric(unlist(obs_time[[id]]))
  mat1 = matrix(rep(temp, length(temp)),nrow=length(temp))
  mat2 = t(mat1)
  temp_mat = mat1 - mat2  
  t = temp_mat - (temp_mat >= 48) * rep(100000000000, length(temp)^2 , nrow = length(temp))
  ind_stamps[[id]] = apply(t,1,which.max)
  m_num_stamps[[id]] = ts( (2:( 1+length(temp) ) ) - ind_stamps[[id]] )
}

print("Extracting Feature... Step 2 of 4 Complete")

ma_features = function(end, values, start, normal)
{
  ma_mean = mean(values[start[end]:end])
  ma_max = max(values[start[end]:end])
  ma_min = min(values[start[end]:end])
  ma_var = sum( (values[start[end]:end] - ma_mean)^2 )/(end - start[end]+1)
  ma_haar = mean(values[start[end]:ceiling(end/2)]) - mean(values[ceiling(end/2):end])
  ma_var_norm = ( ma_mean - normal)
  return(list(ma_mean, ma_max, ma_min, ma_var,ma_haar,ma_var_norm))
}

ma_mean_BPD = list()
ma_max_BPD = list()
ma_min_BPD = list()
ma_var_BPD = list()
ma_haar_BPD = list()
ma_mean_SYS = list()
ma_max_SYS = list()
ma_min_SYS = list()
ma_var_SYS = list()
ma_haar_SYS = list()
ma_mean_HRATE = list()
ma_max_HRATE = list()
ma_min_HRATE = list()
ma_var_HRATE = list()
ma_haar_HRATE = list()
ma_mean_RRATE = list()
ma_max_RRATE = list()
ma_min_RRATE = list()
ma_var_RRATE = list()
ma_haar_RRATE = list()
ma_mean_O2SAT = list()
ma_max_O2SAT = list()
ma_min_O2SAT = list()
ma_var_O2SAT = list()
ma_haar_O2SAT = list()
ma_mean_TEM = list()
ma_max_TEM = list()
ma_min_TEM = list()
ma_var_TEM = list()
ma_haar_TEM = list()
ma_mean_TEM = list()
ma_max_TEM = list()
ma_min_TEM = list()
ma_var_TEM = list()
ma_haar_TEM = list()
ma_var_norm_BPD = list()
ma_var_norm_SYS = list()
ma_var_norm_HRATE = list()
ma_var_norm_RRATE = list()
ma_var_norm_O2SAT = list()
ma_var_norm_TEM = list()



for (id in all_id)
{
  #BP DIASTOLIC
  id = as.character(id)
  temp = as.numeric(unlist(vitals_ts$BP.DIASTOLIC[id]))
  end = 1:length(temp)
  start = as.numeric(unlist(ind_stamps[[id]]))
  x = lapply(end, ma_features, values = temp, start = start, normal = 80)
  x = matrix(unlist(x), nrow=6, byrow=F)
  ma_mean_BPD[[id]] = ts(x[1,])
  ma_max_BPD[[id]] = ts(x[2,])
  ma_min_BPD[[id]] = ts(x[3,])
  ma_var_BPD[[id]] = ts(x[4,])
  ma_haar_BPD[[id]] = ts(x[5,])
  ma_var_norm_BPD[[id]] = ts(x[6,])
  
  #BP SYSTOLIC
  temp = as.numeric(unlist(vitals_ts$BP.SYSTOLIC[id]))
  end = 1:length(temp)
  start = as.numeric(unlist(ind_stamps[[id]]))
  x = lapply(end, ma_features, values = temp, start = start, normal = 120)
  x = matrix(unlist(x), nrow=6, byrow=F)
  ma_mean_SYS[[id]] = ts(x[1,])
  ma_max_SYS[[id]] = ts(x[2,])
  ma_min_SYS[[id]] = ts(x[3,])
  ma_var_SYS[[id]] = ts(x[4,])
  ma_haar_SYS[[id]] = ts(x[5,])
  ma_var_norm_SYS[[id]] = ts(x[6,])
  
  #HEART RATE
  temp = as.numeric(unlist(vitals_ts$HEART.RATE[id]))
  end = 1:length(temp)
  start = as.numeric(unlist(ind_stamps[[id]]))
  x = lapply(end, ma_features, values = temp, start = start, normal = 72)
  x = matrix(unlist(x), nrow=6, byrow=F)
  ma_mean_HRATE[[id]] = ts(x[1,])
  ma_max_HRATE[[id]] = ts(x[2,])
  ma_min_HRATE[[id]] = ts(x[3,])
  ma_var_HRATE[[id]] = ts(x[4,])
  ma_haar_HRATE[[id]] = ts(x[5,])
  ma_var_norm_HRATE[[id]] = ts(x[6,])
  
  #RESPIRATION RATE
  temp = as.numeric(unlist(vitals_ts$RESPIRATION.RATE[id]))
  end = 1:length(temp)
  start = as.numeric(unlist(ind_stamps[[id]]))
  x = lapply(end, ma_features, values = temp, start = start, normal = 16)
  x = matrix(unlist(x), nrow=6, byrow=F)
  ma_mean_RRATE[[id]] = ts(x[1,])
  ma_max_RRATE[[id]] = ts(x[2,])
  ma_min_RRATE[[id]] = ts(x[3,])
  ma_var_RRATE[[id]] = ts(x[4,])
  ma_haar_RRATE[[id]] = ts(x[5,])
  ma_var_norm_RRATE[[id]] = ts(x[6,])
  
  #O2 SATURATION
  temp = as.numeric(unlist(vitals_ts$O2.SATURATION[id]))
  end = 1:length(temp)
  start = as.numeric(unlist(ind_stamps[[id]]))
  x = lapply(end, ma_features, values = temp, start = start, normal = 95)
  x = matrix(unlist(x), nrow=6, byrow=F)
  ma_mean_O2SAT[[id]] = ts(x[1,])
  ma_max_O2SAT[[id]] = ts(x[2,])
  ma_min_O2SAT[[id]] = ts(x[3,])
  ma_var_O2SAT[[id]] = ts(x[4,])
  ma_haar_O2SAT[[id]] = ts(x[5,])
  ma_var_norm_O2SAT[[id]] = ts(x[6,])
  
  #TEMPERATURE
  temp = as.numeric(unlist(vitals_ts$TEMPERATURE[id]))
  end = 1:length(temp)
  start = as.numeric(unlist(ind_stamps[[id]]))
  x = lapply(end, ma_features, values = temp, start = start, normal = 98.6)
  x = matrix(unlist(x), nrow=6, byrow=F)
  ma_mean_TEM[[id]] = ts(x[1,])
  ma_max_TEM[[id]] = ts(x[2,])
  ma_min_TEM[[id]] = ts(x[3,])
  ma_var_TEM[[id]] = ts(x[4,])
  ma_haar_TEM[[id]] = ts(x[5,])
  ma_var_norm_TEM[[id]] = ts(x[6,])
}

# vitals_ts$mean_BPD = mean_BPD
# vitals_ts$mean_SYS = mean_SYS
# vitals_ts$mean_HRATE = mean_HRATE
# vitals_ts$mean_RRATE = mean_RRATE
# vitals_ts$mean_O2SAT = mean_O2SAT
# vitals_ts$mean_TEM = mean_TEM

# vitals_ts$min_BPD = min_BPD
# vitals_ts$min_SYS = min_SYS
# vitals_ts$min_HRATE = min_HRATE
# vitals_ts$min_RRATE = min_RRATE
# vitals_ts$min_O2SAT = min_O2SAT
# vitals_ts$min_TEM = min_TEM
# 
# vitals_ts$max_BPD = max_BPD
# vitals_ts$max_SYS = max_SYS
# vitals_ts$max_HRATE = max_HRATE
# vitals_ts$max_RRATE = max_RRATE
# vitals_ts$max_O2SAT = max_O2SAT
# vitals_ts$max_TEM = max_TEM

vitals_ts$Q1_BPD = Q1_BPD
vitals_ts$Q1_SYS = Q1_SYS
vitals_ts$Q1_HRATE = Q1_HRATE
vitals_ts$Q1_RRATE = Q1_RRATE
vitals_ts$Q1_O2SAT = Q1_O2SAT
vitals_ts$Q1_TEM = Q1_TEM

vitals_ts$Q3_BPD = Q3_BPD
vitals_ts$Q3_SYS = Q3_SYS
vitals_ts$Q3_HRATE = Q3_HRATE
vitals_ts$Q3_RRATE = Q3_RRATE
vitals_ts$Q3_O2SAT = Q3_O2SAT
vitals_ts$Q3_TEM = Q3_TEM

# vitals_ts$first_BPD = first_BPD
# vitals_ts$first_SYS = first_SYS
# vitals_ts$first_HRATE = first_HRATE
# vitals_ts$first_RRATE = first_RRATE
# vitals_ts$first_O2SAT = first_O2SAT
# vitals_ts$first_TEM = first_TEM

# vitals_ts$var_BPD = var_BPD
# vitals_ts$var_SYS = var_SYS
# vitals_ts$var_HRATE = var_HRATE
# vitals_ts$var_RRATE = var_RRATE
# vitals_ts$var_O2SAT = var_O2SAT
# vitals_ts$var_TEM = var_TEM

vitals_ts$diff_nor_BPD = diff_nor_BPD
vitals_ts$diff_nor_SYS = diff_nor_SYS
vitals_ts$diff_nor_HRATE = diff_nor_HRATE
vitals_ts$diff_nor_RRATE = diff_nor_RRATE
vitals_ts$diff_nor_O2SAT = diff_nor_O2SAT
vitals_ts$diff_nor_TEM = diff_nor_TEM

vitals_ts$ma_var_norm_BPD = ma_var_norm_BPD
vitals_ts$ma_var_norm_SYS = ma_var_norm_SYS
vitals_ts$ma_var_norm_HRATE = ma_var_norm_HRATE
vitals_ts$ma_var_norm_RRATE = ma_var_norm_RRATE
vitals_ts$ma_var_norm_O2SAT = ma_var_norm_O2SAT
vitals_ts$ma_var_norm_TEM = ma_var_norm_TEM


 vitals_ts$diff_time = diff_time
 vitals_ts$obs_time = obs_time

vitals_ts$icu_time = icu_time
# vitals_ts$icu_time2 = icu_time2

# vitals_ts$num_stamps = num_stamps
# 
# vitals_ts$m_num_stamps = m_num_stamps
# vitals_ts$ind_stamps = ind_stamps

vitals_ts$ma_mean_BPD = ma_mean_BPD
vitals_ts$ma_max_BPD = ma_max_BPD
vitals_ts$ma_min_BPD = ma_min_BPD
vitals_ts$ma_var_BPD = ma_var_BPD
vitals_ts$ma_haar_BPD = ma_haar_BPD

vitals_ts$ma_mean_SYS = ma_mean_SYS
vitals_ts$ma_max_SYS = ma_max_SYS
vitals_ts$ma_min_SYS = ma_min_SYS
vitals_ts$ma_var_SYS = ma_var_SYS
vitals_ts$ma_haar_SYS = ma_haar_SYS

vitals_ts$ma_mean_HRATE = ma_mean_HRATE
vitals_ts$ma_max_HRATE = ma_max_HRATE
vitals_ts$ma_min_HRATE = ma_min_HRATE
vitals_ts$ma_var_HRATE = ma_var_HRATE
vitals_ts$ma_haar_HRATE = ma_haar_HRATE

vitals_ts$ma_mean_RRATE = ma_mean_RRATE
vitals_ts$ma_max_RRATE = ma_max_RRATE
vitals_ts$ma_min_RRATE = ma_min_RRATE
vitals_ts$ma_var_RRATE = ma_var_RRATE
vitals_ts$ma_haar_RRATE = ma_haar_RRATE


vitals_ts$ma_mean_O2SAT = ma_mean_O2SAT
vitals_ts$ma_max_O2SAT = ma_max_O2SAT
vitals_ts$ma_min_O2SAT = ma_min_O2SAT
vitals_ts$ma_var_O2SAT = ma_var_O2SAT
vitals_ts$ma_haar_O2SAT = ma_haar_O2SAT


vitals_ts$ma_mean_TEM = ma_mean_TEM
vitals_ts$ma_max_TEM = ma_max_TEM
vitals_ts$ma_min_TEM = ma_min_TEM
vitals_ts$ma_var_TEM = ma_var_TEM
vitals_ts$ma_haar_TEM = ma_haar_TEM

print("All Features Extracted... Returning DataFrame containing 106 Features")
return(vitals_ts)
}