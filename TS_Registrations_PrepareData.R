########################
# In this script, we prepare and adjust our downloaded vehicle registration data. We
# access it direcly via my github page.
########################

library(RCurl)

# Load the DF from github into R.Remember that the DF has the following columns: Model, Quantity(New Registrations), Year , Month 
DF_Unedited <- getURL("https://raw.githubusercontent.com/FM2805/Forecast_VehicleRegistration/Dev_SplitFiles/DF_Unedited.csv")
DF_Unedited <- read.csv(text = DF_Unedited)
DF_Unedited$X <- NULL


# Now we have created the Dataframe - but we need to clean it up and eliminate unnecessary / misleading entries
# Get the rows indices where Model is NA
Drop_vec <-which(is.na(DF_Unedited$Model))
#Drow the rows
DF_adjusted <- DF_Unedited[-Drop_vec,]
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

########################
# Now the first part of the clean-up is done. Actually, there are still NA's/missing values in our Data (in the Quantity Column)
# - but they now indicate that this is a former "header" row from the excel file with the vehicle class (e.g. MINIs, SUVs etc.)
# We now transform the DF in such a fashion, that these "class" entries form a seperate column. 
########################


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


# Replace the "-"
DF_adjusted$Class<-sub("-","_",DF_adjusted$Class)
# Replace space with a "_" to avoid problems with naming
DF_adjusted$Class<-sub(" ","_",DF_adjusted$Class)
# Define Quantity as numeric
DF_adjusted$Quantity <-  as.numeric(DF_adjusted$Quantity)

########################
# Now we are nearly done. We create 2 DF out of 'DF_adjusted'. One where we aggregate over the class (these
# values are also available in the rows with the Model "Zusammen" (Combined)) - and one where we keep the
# data at the Model level). These will be named DF_Class and DF_Model.
########################


# Aggregate to the the sum over classes
DF_Class <-aggregate(Quantity ~ Year + Month + Class, data=DF_adjusted, FUN=sum)
# Remove the class 'Sonstige', which is uninteresting
DF_Class <-subset(DF_Class,trimws(DF_Class$Class) != "OTHER_MODEL")
# Order the DF
DF_Class <-DF_Class[with(DF_Class, order(DF_Class$Class, DF_Class$Year,DF_Class$Month)),]

# Adjust the df and remove the rows with "Zusammen" (they give the sum over a class)
DF_Model<- subset(DF_adjusted,trimws(DF_adjusted$Class) != "ZUSAMMEN")

########################
# Finally, we save the data to .csv file
########################

write.csv(DF_Class, file="C:/Users/FloM/Desktop/R_BASE/DF_Class.csv")
write.csv(DF_Model, file="C:/Users/FloM/Desktop/R_BASE/DF_Model.csv")


########################
# Final note: When taking a closer look at the data, it becomes apparent that SUVs exist as a seperate class only
# from 01/2013 onwards. Before, they were part of the class 'GELAENDEWAGEN'. 
# This causes  a structural break in the TS of the latter.
# In the following, we will use the DF_CLASS DF for our analysis.
########################
