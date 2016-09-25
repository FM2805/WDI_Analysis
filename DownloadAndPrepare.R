########################
# In this script, we intent to download and analyse data from the World Bank Development Indicators.
# Well will do this with the official API of the world bank and with the help of descriptive statistics.
# A rigorous analysis (regressions etc.) will be conducted seperately.
########################

library(WDI)
library(dplyr)


########################
# The following variables will be used
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

# Define the variables
variables <- c("SP.DYN.LE00.IN","SP.ADO.TFRT","SP.POP.TOTL","NY.GDP.MKTP.CD","SE.XPD.TOTL.GD.ZS","SH.XPD.TOTL.ZS","SE.ENR.PRIM.FM.ZS")

# Get the data
Data_temp <-WDI(country="all", indicator= variables, start=2000, end=2011,extra=TRUE) 

# Remove the aggregate variables"
Data_temp <- subset(Data_temp, region != "Aggregates")

#Rename the columns
Data<-rename(Data_temp,Life.Exp = SP.DYN.LE00.IN,Adol.Fertility = SP.ADO.TFRT,Total.Pop= SP.POP.TOTL, GDP.Capita = NY.GDP.MKTP.CD,Educ.Exp =SE.XPD.TOTL.GD.ZS, Health.Exp=SH.XPD.TOTL.ZS,GirlsBoys.Ratio=SE.ENR.PRIM.FM.ZS)

# Drop certain columns
DropCols <- c("iso2c", "capital","longitude","latitude","lending")
Data <-Data[ , !(names(Data) %in% DropCols)]

# Save the data
write.csv(Data,"C:/Users/FloM/Desktop/R_Base/WDIData_Unedited.csv")


########################
# Now we investigate and adjust the data
########################

#Summary of variables
summary(Data) 
# There are quite a lot of missing variables - drop those observations
Data_adjusted<-na.omit(Data)
# We see that the data is now fairly balanced over the years with roughly 100 observations per period
table(Data_adjusted$year)
summary(Data_adjusted)
# Reset the rows
rownames(Data_adjusted) <- NULL 

# Adjust the income variable (factor)
Data_adjusted$income <-  droplevels(Data_adjusted$income)
# Adjust the region variable (factor)
Data_adjusted$region <-droplevels(Data_adjusted$region)


# Attach the relevant DF
attach(Data_adjusted)

# Plot Histograms
par(mfrow=c(2,2))
hist(log(GDP.Capita), xlab = "Per Capita Income in USD", main="") # appears non-normal
hist(Educ.Exp, xlab="Spending on Education in % of GDP")          # right tail
hist(Adol.Fertility, xlab="Adolescent Fertility Rate")            # right tailed
hist(Health.Exp, xlab="Health Expenditures in % of GDP")          # right tailed
hist(GirlsBoys.Ratio, xlab="Health Expenditures in % of GDP")     # left tailed
# The data does not appear to be normally distributed, with the possible exception if the GDP per capita

# QQ-plot of GDP per capita
qqnorm(log(GDP.Capita))
shapiro.test(log(GDP.Capita))
# The Shapiro-Wilk normality test rejects the H0 and thus indicates that also this
# variable is not normally distributed

# Lets create some boxplots:
# Education Expenditures explained by income level: Outliers especially with respect to mid- and upper middle income countries. 
boxplot(Educ.Exp ~ income , data=Data_adjusted, xlab="Income Level", ylab ="Public Spending on Education (% of GDP)")

# Adolescent feritlity rate explained by income level: Not surprisingly, the fertility rate is highest with low income countries
boxplot(Adol.Fertility ~ income, data= Data_adjusted, xlab ="Per Capita GDP", ylab ="Adolescent Fertility Rate (per 1000 women)" )

# Save the data
write.csv(Data_adjusted,"C:/Users/FloM/Desktop/R_Base/WDIData_Prepared.csv")