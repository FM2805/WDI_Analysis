########################
# Firstly, we load our data which contains information about new vehicle registrations in Germany between 01/2011 and 06/2016
# We will forecasts for  01/2016-06/2016 period to determine whether be better use a ARIMA model or 3rd order exponential smoothing.
# Thereafter, we are going to forecast the time-period 07/2016 to 12/2016 with the selected model.
# For model selection and forecasting, we use the well known 'forecast' package.
########################

library(forecast)
library(dplyr)
library(RCurl)

# Load the data from github
DF_Class <- getURL("https://raw.githubusercontent.com/FM2805/Forecast_VehicleRegistration/Dev_SplitFiles/DF_Class.csv")
DF_Class <- read.csv(text = DF_Class)
DF_Class$X <- NULL



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


#Set the Dates for the forecast 

#Start year and month - Forecast input
Year_S = 2011
Month_S = 1

#End year and month - Forecast input
Year_E = 2015
Month_E = 12


#End year and month - Model comparison
Year_Ce = 2016
Month_Ce = 6

#Start year and month - Model comparison
Year_Cs = 2016
Month_Cs = 1


# Define the function for estimating ARIMA and exponential smoothing model (3rd order) for 01/2016-06/2016
# We note that data for SUVs is available only from 01/2013 onwards. The output will eventually be
# a dataframe with the forcasted values and the realizations (see below).

CompareEstimate <- function(Data, Names) {
  
  if (Names =="SUVs") {
    
    # DF definieren
    TS_Cars <- ts(Data,start=c(2013,1),end=c(Year_Ce,Month_Ce),frequency=12) 
    # Für FC verwenden
    TS_Cars_fc <-window(TS_Cars,start=c(2013,1),end=c(Year_E,Month_E),frequency=12)
    # Für Abgleich verwenden
    TS_Cars_comp <-window(TS_Cars,start=c(Year_Cs,Month_Cs),end=c(Year_Ce,Month_Ce),frequency=12)
    
  } else {
    
    
    # DF definieren
    TS_Cars <- ts(Data,start=c(Year_S,Month_S),end=c(Year_Ce,Month_Ce),frequency=12) 
    # Für FC verwenden
    TS_Cars_fc <-window(TS_Cars,start=c(Year_S,Month_S),end=c(Year_E,Month_E),frequency=12)
    # Für Abgleich verwenden
    TS_Cars_comp <-window(TS_Cars,start=c(Year_Cs,Month_Cs),end=c(Year_Ce,Month_Ce),frequency=12)
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
  
  # Combine the DF
  DF_Structure <-rbind(DF_Structure,DF_Combine)
  
  return(list(DF_Structure))
  
}


# Execute the function and retur an list with the data
Compare_TS_List <- mapply(CompareEstimate,List_Compare,List_Names)

# Convert the output list to a dataframe
DF_TS <-do.call(rbind, lapply(Compare_TS_List, data.frame, stringsAsFactors=FALSE))

########################
# Now we have our "validation" df. We notice that optimization algorithm for exponential smoothing apparently has some problems
# due to the error messages. On the other hand, for "SPORTS_CATS" and "UTILITES", the Arima model gives us a "flat" forecast
# (essentally the last observation) - because it could not detect a meaningful pattern (seasonality, trend ...) in the data. 
# But lets take a closer look and calculate the RMSE for this period
########################


# Create the colums with the deviations from the FC
DF_TS <-mutate(DF_TS,Diff_Arima_Sq = (DF_TS$Arima_Forecast - DF_TS$Realization) ^ 2,Diff_ExpSm_Sq = (DF_TS$ExpSm_Forecast - DF_TS$Realization) ^ 2
  )

# Calculate RSME: sqrt(sum(y_hat - y)^2 /n)
Error_ARIMA <-aggregate(DF_TS$Diff_Arima_Sq, by = list(DF_TS$Class), sum)
Error_ARIMA$RSME <- sqrt(Error_ARIMA$x / horizon)
Error_ExpSm <-aggregate(DF_TS$Diff_ExpSm_Sq, by = list(DF_TS$Class), sum)
Error_ExpSm$RSME <- sqrt(Error_ExpSm$x / horizon)

# Model choice
ModelChoice <- ifelse(Error_ExpSm$RSME <= Error_ARIMA$RSME, "ExpSm", "ARIMA")


########################
# Apparently, only in the case of "UTILITIES" does exponential smoothing outperform the ARIMA model.
# Now we want to forecast the period 07/2016-12/2016 with the selected model. We will also plot the result
########################

# Split the DF into Segments
DF_Class_List <- split(DF_Class, DF_Class$Class)

# Get the lists of the TS
List_Fc <- sapply(DF_Class_List, function(x)
  x$Quantity)
# Get the names
List_Names <- names(List_Fc)
# Remove names form list
List_Fc <- unname(List_Fc)

# Function to forecast the model
Forecast_2016 <- function(Data, Type, Name) {
  if (Name == "SUVs") {
    DF_FC <- ts(
      Data,
      start = c(2013, 1),
      end = c(2016, 6),
      frequency = 12
    )
    
    
  } else {
    DF_FC <- ts(
      Data,
      start = c(2011, 1),
      end = c(2016, 6),
      frequency = 12
    )
  }
  
  
  if (Type == "ARIMA") {
    Model <- auto.arima(DF_FC)
    FC <- forecast(Model, horizon)
    Dashed_End <- data.frame(FC)$Point.Forecast[1]
    Dashed_Start <- data.frame(DF_FC)[length(DF_FC), 1]
    
  } else {
    Model <- HoltWinters(DF_FC)
    FC <- forecast(Model, horizon)
    Dashed_End <- data.frame(FC)$Point.Forecast[1]
    Dashed_Start <- data.frame(DF_FC)[length(DF_FC), 1]
    
    
  }
  
  #Plot the data
  plot(
    FC,
    ylab = "New Registrations",
    xlab = "Years",
    main = paste("Forecast for the class", sep = " ", Name)
  )
  
  #Connect the forecast with the realization on the plot with a dashed line - otherwise we have a strange blank space
  Dashed <-
    ts(
      c(Dashed_Start, Dashed_End),
      start = c(2016, 6),
      end = c(2016, 7),
      frequency = 12
    )
  lines(Dashed, lty = 2)
  
  return(list(FC))
  
}

# Call the function
List_Results <-
  mapply(Forecast_2016, List_Fc, ModelChoice, List_Names)
# Create a dataframe with the results
DF_Results <-
  do.call(rbind,
          lapply(List_Results, data.frame, stringsAsFactors = FALSE))



########################
# Now we have created the forecast and can inspect them with the help of the plotted output. While we don't see a clear cut pattern
# in some instances (Luxury vehicles, for example), others - notably RVs and Sports Cars' have a clear cut seasonal pattern.
########################





