sample_data = read.csv("asthma_dataset.csv", header = TRUE)

#create subset
sub_data3 = subset(sample_data,select=c("Gender","Smoking","FamilyHistoryAsthma","Diagnosis"))

#rename
names(sub_data3) = c("Sex","Smking","AsthmaHereditary","Infection")

#change datatype to round up
sub_data3$Sex = round(as.numeric(sub_data3$Sex))
sub_data3$Smking = round(as.numeric(sub_data3$Smking))
sub_data3$AsthmaHereditary = round(as.numeric(sub_data3$AsthmaHereditary))
sub_data3$Infection = round(as.numeric(sub_data3$Infection))

#change datatype
sub_data3$Sex = as.integer(sub_data3$Sex)
sub_data3$Smking = as.integer(sub_data3$Smking)
sub_data3$AsthmaHereditary = as.integer(sub_data3$AsthmaHereditary)
sub_data3$Infection = as.integer(sub_data3$Infection)

#mode imputation
get_mode <- function(v) 
{  
  uniqv <- unique(v)  
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
mode_value = get_mode(sub_data3$Sex)
sub_data3$Sex[is.na(sub_data3$Sex)] = mode_value
mode_value = get_mode(sub_data3$Smking)
sub_data3$Smking[is.na(sub_data3$Smking)] = mode_value
mode_value = get_mode(sub_data3$AsthmaHereditary)
sub_data3$AsthmaHereditary[is.na(sub_data3$AsthmaHereditary)] = mode_value
mode_value = get_mode(sub_data3$Infection)
sub_data3$Infection[is.na(sub_data3$Infection)] = mode_value


summary(sub_data3)
str(sub_data3)
View(sub_data3)