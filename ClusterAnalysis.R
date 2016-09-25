########################
# The next step is to implement a cluster analysis with the WDI data. 
# Idea: Find subgroups in the data based on how "similar" (or dissimilar the observations are). 
# This is measured with a distance indicator. Usually the euclidian distance is used.
# For this, we select a subset ob the data (observations from 2010).
# We will start with k-means clustering.
########################

library(RCurl)

Data <- getURL("https://raw.githubusercontent.com/FM2805/WDI_Analysis/Development/WDIData_Prepared.csv")
Data <- read.csv(text=Data)
Data$X <- NULL

par(mfrow=c(1,1))


########################
# The following variables are at our disposal
########################
# Fertility rates: Adolescent fertility rate (births per 1,000 women ages 15-19)
# Life Expectancy in years: Life.Exp
# Population in total: Total.Pop
# Per Capita GDP (current USD): NY.GDP.MKTP.CD
# Education Spending (% of GDP): Educ.Exp
# Health Expenditures (% of GDP): Health.Exp
# Ratio of girls to boys in primary and secondary education (%): GirlsBoys.Ratio
# Income: Factor variable - High income (non OECD)/High income (OECD)/ Upper Middle Income / Lower Middle Income / Low Income
# Geographic Region : Region


#Set seed to get reproduceable results
set.seed(1)

# Include the "Status" variable
# Change factor variable to a string, other wise they can't be changed.
Data$income <- as.character(Data$income)

# Create a new "Status" variable: High and Upper middle income countries belong to the "rich" group (1), 
# other observations to the poor "category" (0)
Data$Status[Data$income == "High income: nonOECD" | Data$income == "High income: OECD" | Data$income =="Upper middle income"] <- 1 
Data$Status[Data$income =="Low income" | Data$income =="Lower middle income" ] <- 0

# Create the new factor variable 'Status'
Data$Status<-factor(Data$Status, levels=c(0,1),labels=c("Poor","Rich"))

# Select 2010 data
Data_2010<- subset(Data,year==2010)
rownames(Data_2010) <- NULL

#Scale the data #mean 0 - sd 1 
sd_data <- scale(Data_2010[,c("Adol.Fertility","Life.Exp","Health.Exp","Educ.Exp")])

# K-Means clustering -> Top Down - Choose the number of clusters yourself  (3 in our case)
# Randomly assign a number (1 or 3 in out case) to each observation. Then iteratively compute the centroid or the two
# clusters and assign each observation to the cluster whose centroid is closest (according to euclidian distance).
# We use set.seed(1) because of the random assignments in step 1
# nstart refers to the number of random assignments 
grp<-kmeans(sd_data, centers=3,nstart=20)

# Lets take a look at the cluster assignments (with respect to the income level of the countries)
table(grp$cluster, Data_2010$income)
# It is apparent that couster 1 identifies low income countries, cluster 2 lower/upper middle income,
# and cluster 3 high income countries.


########################
# Now we will use a "bottom up" approach (hierarchical clustering)
# Each observation is initially traded as its own cluster which are then iteratively fuse with the most "similar" one until 
# only a single cluster remains. Interpretation is done via a dendogram.
# The earlier the fusion (closer to the bottom of the dendogram) occurs the most similar the observations are.
# The vertical axis is the indication for how "similar" observations are.
########################


# We use euclidian distance as similarity measure
data.dist<-dist(sd_data) 

# We immediately plot the dendogramm an use - instead of the country labels and for the sake of simplicity, our Status
# indicator to 


par(cex=1,font=3)
plot(hclust(data.dist), labels =Data_2010$iso3c , main="Dendogram", xlab ="", sub ="", ylab ="")
# Enlarge the plot with the zoom function to identify the labels properly.
# We see that once again countries with a similar economic strenght (e.g. Denmark and New Zealand // Guinea Bissau and
# the Central African Republic) are merged early.

# Adjust the labels so that they reflect the "Status" variable.
par(cex=0.7, mar=c(5, 8, 4, 1))
plot(hclust(data.dist), labels =Data_2010$Status , main="Dendogram", xlab ="", sub ="", ylab ="")
# As mentioned above, it is apparent that economically similar nations are merged quickly.

# Reset plotting device
par(mfrow=c(1,1))
dev.off()

