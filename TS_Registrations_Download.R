########################
# In this script, we download monthly new registrations for certain vehicle segments in Germany. The data
# is made publicly available by the responsible German government body located in Flensburg.
# This file downloads the relevant excel templates with the data into a specified folder and loads them back into R to
# create a dataframe. To be precise, we will download data from 01/2011 to 06/2016 (there is one excel for each month)
#
#
# Note - Make sure that you have an adequate java Version installed (=64 bit Java for 64 bit R and 32 bit Java for 32 bit R), 
# otherwise the script wont work.
########################

library(xlsx)

# Set the dates
YearStart = 2011
MonthStart= 1
YearEnd = 2016
MonthEnd = 6

# Set the baseline URL - use placeholders for months (MONTH) and years (YEAR): It is the website from the Federal Motor Transport Authority (KBA).
Url_temp <-"http://www.kba.de/SharedDocs/Publikationen/DE/Statistik/Fahrzeuge/FZ/YEAR_monatlich/FZ11/fz11_YEAR_MONTH_xls.xls?__blob=publicationFile&v=6"
Year_temp <- "YEAR"
Month_temp <- "MONTH"

# Set the folder for the xlsx we are going to download
Folder <- "C:/Users/FloM/Desktop/R_Base"

# Years to evaluate - 2011 to 2015
Years_L <-rep(seq(YearStart,YearEnd-1),12)
# Add first last six months of 2016
Years_L <- c(Years_L,rep(YearEnd,MonthEnd))
Years_L <-as.character(sort(Years_L))


# Create a corresponding list with months
Months_L <-rep((seq(1,12)),YearEnd-YearStart)
Months_L <- c(Months_L,seq(1,MonthEnd))
# Add a lead 0 (necessary for the URL)
Months_L <- ifelse(Months_L < 10, paste0("0",Months_L),Months_L)

# Define the structure for the dataframe used in the function below (for the time being define all columns as strings)
DF_Structure <- data.frame(Model=as.character(),Quantity=as.character(),Year=as.character(),Month = as.character())


# Define the function for the download the excel files from the website of the 
Download <- function(Year,Month,Url_temp,Year_temp,Month_temp,FileLocation) {
  
  # Define the string for the URL
  sUrl<- gsub(Year_temp,Year,Url_temp)
  sUrl <-gsub(Month_temp, Month,sUrl)
  
  # Create "temp" file in your temp directory
  FileName <- paste(Year,Month,".xls",sep="")
  Dest <- paste(FileLocation ,FileName, sep="/")
  
  #Download the file
  download.file(url = sUrl, destfile = Dest, mode="wb")
  # Read the file (only the first three columns are relevant for us)
  DF_temp <-read.xlsx(file=Dest,sheetIndex = 1,header=F,startRow=6,colIndex=c(1,3),stringsAsFactors=FALSE)
  
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
List_Data <-mapply(Download,Years_L,Months_L,Url_temp,Year_temp,Month_temp,Folder) 

# Create a dataframe - it will have the following columns: Model, Quantity(New Registrations), Year , Month 
DF_Unedited <- do.call(rbind, lapply(List_Data,data.frame,stringsAsFactors=FALSE))


# Generate translations of the classes (they are at this moment integrated in the "Model" column)
DF_Unedited$Model <- ifelse(trimws(DF_Unedited$Model) == "KLEINWAGEN", "SMALL_CARS",DF_Unedited$Model)
DF_Unedited$Model<- ifelse(trimws(DF_Unedited$Model) == "KOMPAKTKLASSE", "COMPACT_CARS",DF_Unedited$Model)
DF_Unedited$Model<- ifelse(trimws(DF_Unedited$Model) == "MITTELKLASSE", "MIDSIZE_CARS",DF_Unedited$Model)
DF_Unedited$Model<- ifelse(trimws(DF_Unedited$Model) == "OBERE MITTELKLASSE", "EXECUTIVE_CARS",DF_Unedited$Model)
DF_Unedited$Model<- ifelse(trimws(DF_Unedited$Model) == "OBERKLASSE", "LUXURY_VEHICLES",DF_Unedited$Model)
DF_Unedited$Model<- ifelse(trimws(DF_Unedited$Model) == "SPORTWAGEN", "SPORTS_CARS",DF_Unedited$Model)
DF_Unedited$Model<- ifelse(trimws(DF_Unedited$Model) == "MINI-VANS", "MINI_VANS",DF_Unedited$Model)
DF_Unedited$Model<- ifelse(trimws(DF_Unedited$Model) == "GROSSRAUM-VANS", "LARGE_VANS",DF_Unedited$Model)
DF_Unedited$Model<- ifelse(trimws(DF_Unedited$Model) == "WOHNMOBILE", "RVs",DF_Unedited$Model)
# ifelse has some issues when special characters are in play, therefore we remove them first in this case
DF_Unedited$Model<- ifelse(trimws(gsub('Ã"','AE',DF_Unedited$Model)) == "GELAENDEWAGEN", "OFFROAD_VEHICLE",DF_Unedited$Model)
# Translate "SONSTIGE": "Other Models" that are included in a class but not expicitly counted
DF_Unedited$Model<- ifelse(trimws(DF_Unedited$Model) == "SONSTIGE", "OTHER_MODEL",DF_Unedited$Model)



# Reset rownames and other objects
rownames(DF_Unedited) <- NULL

# Save the DF
write.csv(DF_Unedited, file="C:/Users/FloM/Documents/GitHub/Forecast_VehicleRegistration/DF_Unedited.csv")