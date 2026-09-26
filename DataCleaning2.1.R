sample_data = read.csv("asthma_dataset.csv", header = TRUE)

#create subset
sub_data2 = subset(sample_data,select=c("Gender","FamilyHistoryAsthma","Diagnosis"))

#rename
names(sub_data2) = c("Sex","AsthmaHereditary","Infection")

#change datatype to round up
sub_data2$Sex = round(as.numeric(sub_data2$Sex))
sub_data2$AsthmaHereditary = round(as.numeric(sub_data2$AsthmaHereditary))
sub_data2$Infection = round(as.numeric(sub_data2$Infection))

#change datatype
sub_data2$Sex = as.integer(sub_data2$Sex)
sub_data2$AsthmaHereditary = as.integer(sub_data2$AsthmaHereditary)
sub_data2$Infection = as.integer(sub_data2$Infection)

#mode imputation
get_mode <- function(v) 
{  
  uniqv <- unique(v)  
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
mode_value = get_mode(sub_data2$Sex)
sub_data2$Sex[is.na(sub_data2$Sex)] = mode_value
mode_value = get_mode(sub_data2$AsthmaHereditary)
sub_data2$AsthmaHereditary[is.na(sub_data2$AsthmaHereditary)] = mode_value
mode_value = get_mode(sub_data2$Infection)
sub_data2$Infection[is.na(sub_data2$Infection)] = mode_value


summary(sub_data2)
str(sub_data2)
View(sub_data2)