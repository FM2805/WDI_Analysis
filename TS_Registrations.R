# With this script, we intend to forecast monthly new registrations for certain car segments in Germany. The data
# is made publicly available by the responsible German government body located in Flensburg.
# This file downloads the relevant xlsx templates with the data into your "Temp" folder and then loads them into R.
# Afterwards, the 

# Note - Make sure that you have an adequate java Version installed (=64 bit Java for 64 bit R and 32 bit Java for 32 bit R), 
#otherwise the script wont work since the packages...EXCEL load rely on it)

#### SLIT IN 2 TEILE -- 1es - automatisch downloaden und verarbeiten
##### 2 ens Analyse


## NEGT -> logistic example regression + GAM models!!! + Poisson ggf?!


#tuneResult <- tune(svm, Y ~ X,  data = data,
#                   ranges = list(epsilon = seq(0,0.2,0.01), cost = 2^(2:9))
#) 
#print(tuneResult)
#plot(tuneResult)

library(ggplot2)
library(scales)
library(xlsx)
library(utils)
library(dplyr)
library(forecast)
#checks if packages installed
#list.of.packages <- c("ggplot2", "Rcpp")
#new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#if(length(new.packages)) install.packages(new.packages)

#

options(scipen=999)

iYearStart = 2011
iMonthStart= 1

iYearEnd = 2016
iMonthEnd = 6

# Set the baseline URL - use placeholders for months (MONTH) and years (YEAR)
Url_temp ="http://www.kba.de/SharedDocs/Publikationen/DE/Statistik/Fahrzeuge/FZ/YEAR_monatlich/FZ11/fz11_YEAR_MONTH_xls.xls?__blob=publicationFile&v=6"
Year_temp <- "YEAR"
Month_temp <- "MONTH"

# Years to evaluate - 2011 to 2015
Years_L <-rep(seq(2011,2015),12)
Years_L <- c(Years_L,rep(2016,6))
Years_L <-as.character(sort(Years_L))


# Add the first 6 months of 2016
Months_L <-rep((seq(1,12)),5)
Months_L <- c(Months_L,seq(1,6))
# Add a lead 0 (necessary for the URL)
Months_L <- ifelse(Months_L < 10, paste0("0",Months_L),Months_L)

# Define the structure for the dataframe used in the function below (for the time being define all columns as strings)
DF_Structure <- data.frame(Model=as.character(),Quantity=as.character(),Year=as.character(),Month = as.character())

####
### NOCH -> NEUES VZ LÖSCHEN
### NOCH -> NEUES VZ LÖSCHEN
### NOCH -> NEUES VZ LÖSCHEN

####

# Define the function for the download the excel files from the website
Download <- function(Year,Month,Url_temp,Year_temp,Month_temp) {
  
  # Define the string for the URL
  sUrl<- gsub(Year_temp,Year,Url_temp)
  sUrl <-gsub(Month_temp, Month,sUrl)
  
  # Create "temp" file in yout temp directory
  temp <-tempfile(fileext =".xls")
  #Download the file
  download.file(url = sUrl, destfile = temp, mode="wb")
  # Read the file 
  DF_temp <-read.xlsx(file=temp,sheetIndex = 1,header=F,startRow=6,colIndex=c(1,3),stringsAsFactors=FALSE)
  # Change names
  colnames(DF_temp) <- c("Model","Quantity")
  
  # Set year and Month
  DF_temp$Year  <- Year
  DF_temp$Month <- Month
  
  # Attach to the defined DF
  DF_Structure <- rbind(DF_Structure,DF_temp)
  
  return(list(DF_Structure))
}

# Call the function for the download
List_Data <-mapply(Download,Years_L,Months_L,Url_temp,Year_temp,Month_temp) 

# Create a dataframe
DF_Basic <- do.call(rbind, lapply(List_Data,data.frame,stringsAsFactors=FALSE))

# Reset rownames and other objects
rownames(DF_Basic) <- NULL
DF_Structure <- NULL
#List_Data <- NULL
Years_L <-NULL
Months_L <- NULL
Url_temp <- NULL
Year_temp <- NULL

########################
# Now we have created the Dataframe - but we need to clean it up and eliminate unnecessary / misleading entries
########################

# Remove the rows where Model is NA

# Get the rows indices where Model is NA
Drop_vec <-which(is.na(DF_Basic$Model))
#Drow the rows
DF_adjusted <- DF_Basic[-Drop_vec,]
#Reset the rownames
rownames(DF_adjusted) <- NULL


# Identify the final row (Entry: "Neuzulassungen insgesamt")
Drop_vec_1 <-which(trimws(DF_adjusted$Model)=="NEUZULASSUNGEN INSGESAMT")
# Add row above to the drop list - they contain aggregations of the category "Sonstige"
Drop_vec_1 <- c(Drop_vec_1,Drop_vec_1-1)
# Remove footers - Search the strings for the specific patterns
Drop_vec_2<-grep('Ausgewiesen werden',DF_adjusted$Model)
Drop_vec_3<-grep('Modellreihen',DF_adjusted$Model)
# Drop 'VANS' category - they are separately reported as 'MINI-VANS' and 'Großraum-VANS'
Drop_vec_4 <- which(trimws(DF_adjusted$Model)=="VANS" | trimws(DF_adjusted$Model)=="VANS INSGESAMT")
# Create final vector
Drop_vec <- c(Drop_vec_1,Drop_vec_2,Drop_vec_3,Drop_vec_4)
# Drop the identified rows
DF_adjusted  <- DF_adjusted[-Drop_vec,]

#Replace the '-' in Quantity with a '0'
DF_adjusted$Quantity <-gsub('-',0,DF_adjusted$Quantity)
rownames(DF_adjusted) <- NULL

##
# Now the first part of the clean-up is done. Actually, there are still NA's/missing values in our Data (Quantity)
# - but they now indicate that this is a "header" row with the vehicle class (e.g. MINIs, SUVs etc.)
# We now transform the DF in such a fashion, that these "class" entries form a seperate column. 
##


#Get index of the Models with an NA or "" entry in the respective 'cell'
Vec_Id <- which(is.na(DF_adjusted$Quantity) | trimws(DF_adjusted$Quantity)=="")
# Create the difference vector, that shows us the "length" of the class
Vec_Id_diff <- c(diff(Vec_Id),length(DF_adjusted$Quantity)-Vec_Id[length(Vec_Id)] + 1) 


#Extract the Classes that fit Vec_Id
Classes_Rep <-DF_adjusted[Vec_Id,1]

#Create the id Column 
rep(Classes_Rep,Vec_Id_diff)
# Add the class col to the df
DF_adjusted$Class <-rep(Classes_Rep,Vec_Id_diff)

# Remove the columns where Model = Class 
DF_adjusted<- DF_adjusted[-which(trimws(DF_adjusted$Class)==trimws(DF_adjusted$Model)),]

# Remove remnants of special characters in the Class-string
DF_adjusted$Class<-sub("Ã"","AE",DF_adjusted$Class)
# Remove the "-" because this may causes a plotting error later in the loop
DF_adjusted$Class<-sub("-","_",DF_adjusted$Class)
# Replace space with a "_" to avoid problems with naming
DF_adjusted$Class<-sub(" ","_",DF_adjusted$Class)
# Define Quantity as numeric
DF_adjusted$Quantity <-  as.numeric(DF_adjusted$Quantity)


##
# Now we are nearly done. We create 2 DF out of 'DF_adjusted'. One where we aggregate over the class (these
# values are also available in the rows with the Model "Zusammen" (Combined)) - and one where we keep the
# data at the Model level). These will be named DF_Class and DF_Model
##


# Aggregate to the the sum over classes
DF_Class <-aggregate(Quantity ~ Year + Month + Class, data=DF_adjusted, FUN=sum)
# Remove the class 'Sonstige', which is uninteresting
DF_Class <-subset(DF_Class,trimws(DF_Class$Class) != "SONSTIGE")

# Adjust the df and remove the rows with "Zusammen" (they give the sum over a class)
DF_Model<- subset(DF_adjusted,trimws(DF_adjusted$Class) != "ZUSAMMEN")

##
# Finally, we save the data to .csv file
##

write.csv(DF_Class, file="C:/Users/FloM/Desktop/Persönlich/Privat/AwetDesAmolWos/git_prospect/DF_Class.csv")
write.csv(DF_Class, file="C:/Users/FloM/Desktop/Persönlich/Privat/AwetDesAmolWos/git_prospect/DF_Model.csv")

##
# Final note: When taking a closer look at the data, it becomes apparent that SUVs exist as a seperate class only
# from 01/2013 onwards. Before, they were part of the class 'GELAENDEWAGEN'. This causes  a structural break in the TS of the latter )
##

test <- subset(DF_Class,Class=="GELAENDEWAGEN")
test <-test[with(test, order(Year, Month)), ]

##
# Firstly, we load our data which contains information about new vehicle registrations in Germany between 01/2011 and 06/2016
# We will forecasts for  01/2016-06/2016 period to determine whether be better use a ARIMA model or 3rd order exponential smoothing 
##


#Set the horizon for the initial forecast (01/2016-06/2016)
horizon = 6

  
# Set up empty data frame where the data will be stored 
DF_Structure <- data.frame(Arima_Forecast=numeric(),ExpSm_Forecast=numeric(),Realization=numeric(),Class = character())
  
# Split the DF
DF_Class_Compare <-split( DF_Class,DF_Class$Class)

# Get the lists of the TS
List_Compare <-sapply(DF_Class_Compare, function(x) x$Quantity)
# Get the names
List_Names<-names(List_Compare)   
# Remove names form list
List_Compare <- unname(List_Compare) 


#Set the Dates for the forecast (Übernimm von oben)
#Start year and month - forecast component
iYear_S = 2011
iMonth_S = 1

#End year and month - forecast component
iYear_E = 2015
iMonth_E = 12


#End year and month - compare component
iYear_Ce = 2016
iMonth_Ce = 6

#Start year and month - compare component
iYear_Cs = 2016
iMonth_Cs = 1


# Define the function for estimating ARIMA and exponential smoothing model (3rd order) for 01/2016-06/2016
# We note that data for SUVs is available only from 01/2013 onwards 

CompareEstimate <- function(Data, Names) {
  
  if (Names =="SUVs") {
    
    # DF definieren
    TS_Cars <- ts(Data,start=c(2013,1),end=c(iYear_Ce,iMonth_Ce),frequency=12) 
    # Für FC verwenden
    TS_Cars_fc <-window(TS_Cars,start=c(2013,1),end=c(iYear_E,iMonth_E),frequency=12)
    # Für Abgleich verwenden
    TS_Cars_comp <-window(TS_Cars,start=c(iYear_Cs,iMonth_Cs),end=c(iYear_Ce,iMonth_Ce),frequency=12)
    
  } else {
    
    
    # DF definieren
    TS_Cars <- ts(Data,start=c(iYear_S,iMonth_S),end=c(iYear_Ce,iMonth_Ce),frequency=12) 
    # Für FC verwenden
    TS_Cars_fc <-window(TS_Cars,start=c(iYear_S,iMonth_S),end=c(iYear_E,iMonth_E),frequency=12)
    # Für Abgleich verwenden
    TS_Cars_comp <-window(TS_Cars,start=c(iYear_Cs,iMonth_Cs),end=c(iYear_Ce,iMonth_Ce),frequency=12)
  }
  
  # Estimate the Model -# choos aic bic or aicc
  Arima_Model <- auto.arima(TS_Cars_fc)
  
  # Exp Smoothing (3rd order)
  ExpSm_Model  <-HoltWinters(TS_Cars_fc)
  
  
  #Forecast the models with 
  Arima_FC <- forecast(Arima_Model,h=horizon)
  ExpSm_FC <-forecast(ExpSm_Model,h=horizon)
  
  
  # Create a DF with the statistics
  DF_Combine <- data.frame(data.frame(Arima_FC)[,1],data.frame(ExpSm_FC)[,1],as.numeric(TS_Cars_comp),Names)
  colnames(DF_Combine) <- c("Arima_Forecast","ExpSm_Forecast","Realization","Class")
  
  
  DF_Structure <-rbind(DF_Structure,DF_Combine)
  
  return(list(DF_Structure))
  
}


# Execute the function and retur an list with the data
Compare_TS_List <- mapply(CompareEstimate,List_Compare,List_Names)

# Convert the List to a dataframe
DF_TS <-do.call(rbind, lapply(Compare_TS_List, data.frame, stringsAsFactors=FALSE))

###
# Now we have our "validation" df. We notice that algorithm for exponential smoothing apparently has some problems
# due to the error messages. On the other hand, for "Sportwagen" and "Utilities", the Arima model gives us a "flat" forecast
# (essentally the last observation) - because it could not detect a meningful pattern in the data. 
# But lets take a closer look and calculate the RMSE for this period
###

 # Create the colums with the deviations from the FC
 DF_TS <- mutate(DF_TS,Diff_Arima_Sq=(DF_TS$Arima_Forecast-DF_TS$Realization)^2,Diff_ExpSm_Sq = (DF_TS$ExpSm_Forecast-DF_TS$Realization)^2 )
 
 # Calculate RSME: sqrt(sum(y_hat - y)^2 /n)
 Error_ARIMA<- aggregate(DF_TS$Diff_Arima_Sq,by=list(DF_TS$Class), sum)
 Error_ARIMA$RSME <- sqrt(Error_ARIMA$x/horizon)
 Error_ExpSm <-aggregate(DF_TS$Diff_ExpSm_Sq,by=list(DF_TS$Class), sum)
 Error_ExpSm$RSME <-sqrt(Error_ExpSm$x/horizon)
 
 # Model choice
 ModelChoice <- ifelse(Error_ExpSm$RSME<=Error_ARIMA$RSME,"ExpSm","ARIMA")
 
 
 ###
 # Apparently, only in the case of "Utilities" does exponential smoothing outperform the ARIMA model.
 # Now we want to forecast the period 07/2016-12/2016 with the selected model. We will also plot the outcome.
 ### 
 
 # Split the DF into Segments 
 DF_Class_List <-split( DF_Class,DF_Class$Class)
 
 # Get the lists of the TS
 List_Fc <-sapply(DF_Class_List, function(x) x$Quantity)
 # Get the names
 List_Names<-names(List_Fc)   
 # Remove names form list
 List_Fc <- unname(List_Fc) 
 
 Forecast_2016 <- function(Data, Type,Name) {
   
   if (Name=="SUVs") {
     DF_FC <- ts(Data, start=c(2013,1),end=c(2016,6),frequency = 12)
     
     
   } else {
     
     DF_FC <- ts(Data, start=c(2011,1),end=c(2016,6),frequency = 12)
   }

   
   if (Type=="ARIMA") {
    
     Model <- auto.arima(DF_FC)
     FC <-forecast(Model,horizon)
     Dashed_End <-data.frame(FC)$Point.Forecast[1]
     Dashed_Start<- data.frame(DF_FC)[length(DF_FC),1]
     
   } else {
     
     Model <- HoltWinters(DF_FC)
     FC <-forecast(Model,horizon)
     Dashed_End <-data.frame(FC)$Point.Forecast[1]
     Dashed_Start<- data.frame(DF_FC)[length(DF_FC),1]
     
     
   }
   
   #Plot the data
   plot(FC,ylab="New Registrations",xlab="Years", main=paste("Forecast for the class",sep=" ",Name))
   
   #Connect the forecast with the realization on the plot with a dashed line - otherwise we have a strange blank space
   Dashed <-ts(c(Dashed_Start, Dashed_End),start=c(2016,6),end=c(2016,7),frequency=12)
   lines(Dashed,lty=2)

   return(list(FC))
   
 }

  # Call the function
  List_Results <-mapply(Forecast_2016, List_Fc,ModelChoice,List_Names)
 
  ###
  # Now we have created the forecast and can inspect them with the help of the plotted output
  ### 
 
  
 

