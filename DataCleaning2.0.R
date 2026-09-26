sample_data = read.csv("asthma_dataset.csv", header = TRUE)

#create subset
sub_data1 = subset(sample_data,select=c("FamilyHistoryAsthma","Diagnosis"))

#rename
names(sub_data1) = c("AsthmaHereditary","Infection")

#change datatype to round up
sub_data1$AsthmaHereditary = round(as.numeric(sub_data1$AsthmaHereditary))
sub_data1$Infection = round(as.numeric(sub_data1$Infection))

#change datatype
sub_data1$AsthmaHereditary = as.integer(sub_data1$AsthmaHereditary)
sub_data1$Infection = as.integer(sub_data1$Infection)

#mode imputation
get_mode <- function(v) 
{  
  uniqv <- unique(v)  
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
mode_value = get_mode(sub_data1$AsthmaHereditary)
sub_data1$AsthmaHereditary[is.na(sub_data1$AsthmaHereditary)] = mode_value
mode_value = get_mode(sub_data1$Infection)
sub_data1$Infection[is.na(sub_data1$Infection)] = mode_value


summary(sub_data1)
str(sub_data1)
View(sub_data1)
