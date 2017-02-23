#############################################
#create a dataset where all unknowns, missing, and nonsense values are coded as "uknown" (for categorical data):
#[this is so that I can run correspondence analysis and see if unknowns are meaningful]
#############################################
dwu = data
head(dwu)

#drop variables we don't want to use:
drop = names(dwu) %in% c("wpt_name", "subvillage","lga", "ward", "scheme_name",
                         "date_recorded", "num_private", "region","recorded_by",
                         "scheme_management","payment_type","quantity_group") 
dwu = dwu[!drop]

#factors --> characters in order to recode values to "unknown"
library(dplyr)
dwu %>% mutate_if(is.factor, as.character) -> dwu

#Change all the suspicious values to "unkown"
library("MASS")
dwu[dwu=="unknown" | dwu=="Unknown" | dwu=="Unknown Installer" | dwu=="-" | dwu==""] <- "unknown"
dwu[dwu=="unknown"] <- "unknown"
dwu[dwu=="Unknown"] <- "unknown"
dwu[dwu=="Unknown Installer"] <- "unknown"
dwu[dwu=="-"] <- "unknown"
dwu[dwu=="other"] <- "unknown"
head(dwu)

#characters --> factors in order to do future analyses
dwu %>% mutate_if(is.character, as.factor) -> dwu

#check to make sure you have factors again:
sapply(dwu, class)

#logical --> characters to add "unknown", then change them to factors for further analyses:
dwu %>% mutate_if(is.logical, as.character) -> dwu
dwu[dwu=="NA"] <- "unknown"
dwu[is.na(dwu)] <- "unknown"
dwu %>% mutate_if(is.character, as.factor) -> dwu

#check to see if there are any values coded as NA:
sum(is.na(dwu))

#merge class labels into the data:
class = read.csv("Training set labels.csv")
head(class)
merged = merge(x = dwu, y = class, by = "id", all.x = TRUE)
head(merged)
# Add a column for a new response variable where "functional needs repair" --> "non funcitonal"
merged$status_group2 <-  ifelse(merged$status_group == "functional","functional", "non functional") #this says if status_group = functional, then functional, else it is 
#nonfunctional
#make a column where functional = 1 and nonfunctional = 0:
merged$status_group3 <-  ifelse(merged$status_group2 == "functional",1, 0)
head(merged)

#send data to a new file:
library("rio")
export(merged, "C:/Users/Renel/Documents/School/DePaul/3. Winter 2017/CSC 424/Project/unknowns.csv")


############################################################
#Exploratory Correspondence Analysis on data with unknowns:
############################################################
cleandata = read.csv("unknowns.csv")
head(cleandata)

#NOTE: CA requires that the categorical variables used have more than 2 levels, so for exploration,
#we must use status_group (3 levels) instead of status_group2 (2 levels)

#create a contingency table for the two nominal variables you want to compare:
mytable <- with(cleandata, table(extraction_type_class, status_group))
mytable

round(prop.table(mytable, 1), 2)  # Row Percentages
round(prop.table(mytable, 2), 2)  # Column Percentages

library(ca)
fit = ca(mytable)
summary(fit)#value is kind of like variance; % is percent of correspondence captured
#by that dimension
fit1 #instead of correspondence captured, they call it the principle inertias
#(eigenvalues); the % tells you the percentage of the interita 
#(variance/correspondance) that the dimensions (eigenvectors) account for. It also
#gives you the chi squared distances for the rows and the columns, which you want
#to plot
plot(fit)
mytable

plot(fit, mass=T, contrib="absolute", 
     map="rowgreen", arrows=c(F, T)) #this gives you the arrows
library(vcd)
mosaic(extr_pay, shade=TRUE, legend=TRUE)
