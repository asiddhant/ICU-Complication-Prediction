ptm=proc.time()
vitals_cleaned=raw_vitals_cleaner("test_RawVitalData.csv")
labs_cleaned=raw_lab_cleaner("test_RawLabData.csv")
rx_cleaned=raw_rx_cleaner("test_Rxdata.csv")
static_cleaned=raw_static_cleaner("test_Static_data.csv")

all_merged=all_merger(vitals_cleaned,labs_cleaned,rx_cleaned)
vitals_no=all_merged[[2]]
labs_no=all_merged[[3]]
med_no=all_merged[[4]]
all_merged=all_merged[[1]]

all_data=raw_feature_extractor(all_merged,vitals_no,labs_no,med_no,normvalues)

all_data2=cbind(all_data,static_cleaned[,2:ncol(static_cleaned)])

rm(list=ls()[!(ls() %in% c('all_data2'))])

#all_data=ts_unroller(all_data)
proc.time()-ptm

