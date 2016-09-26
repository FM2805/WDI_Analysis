
########################
# In this script, we intent to analyse data from the World Bank Development Indicators. We will use
# regression analysis (linear & non-linear) as well as logistic regression to take a closer look at the data.
# We use the prepared data from my github account.
########################

library(dplyr)
library(mgcv)
library(RCurl)
options(scipen=999)

# Load the data from my github
Data <- getURL("https://raw.githubusercontent.com/FM2805/WDI_Analysis/Development/WDIData_Prepared.csv")
Data <- read.csv(text = Data)
Data$X <- NULL


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


########################
# We will perfrom a basic regression analysis to investiage the impact of the covariates on life-expectancy
# We will start with (pooled) OLS, perform an outlier analysis and then use a non-linear modeling approach (GAM).
########################


# Estimate a simple OLS model - we controll of course for the region and the year 
OLS  <- lm(Life.Exp ~ Adol.Fertility + Educ.Exp + Health.Exp  + GirlsBoys.Ratio + log(Total.Pop)  + region + factor(year), data = Data)
summary(OLS)

# fertility rate      -> negative impact
# education spending  -> negative impact (somewhat surprsingly)
# health expenditures -> positive impact
# girls boys ratio    -> negative impact (the more girls in primary enrolement compared to boys, the higher is the life expectancy)
# We see from the factor variables that the average life expectancy increased over the years (baseline is 2000) and
# that the life expectancy in South Asia and Subsahran Africa is signifcantly lower than in the baseline region (East Asia & Pacific)
# R² of 80%


# Lets perform some diagnostics
hist(rstudent(OLS), xlab="Standardized Residuals", main="") # Standardized Residuals: e/std(e)
qqnorm(OLS$residuals) #If normality holds for then the residuals should be close to the diagonal line
qqline(OLS$residuals)
plot(rstudent(OLS)~OLS$fitted, ylab="Residuals", xlab="Fitted Values")
abline(0,0, col="red") 
plot(rstudent(OLS), ylab ="Standardized Residuals") 
abline(0,0, col="red")
# Normality assumption is probably violated, and there are apparently outlier observations.
# Lesson: The results suggest that we shold probably evaluate non-linear models.
# But first, eliminate outlier observations.


# High leverage observations -> Potentially large impact on fitted values
# Idea: Measure the (potential) impact of y on its own fitted value. We do this with the "hat" Matrix.
# Rule of thumb: If the hat value > 2*average(hat value) -> Suspicious, will be removed
par(mfrow=c(1,1))
lev <- hat(model.matrix(OLS))
plot(lev,ylab="Leverage ratio", xlab ="Index", main="Which observations are influential?")
abline(mean(lev)*2,0, col ="red")
Data_Robust <-Data[lev<mean(lev)*2,]
rownames(Data_Robust) <- NULL


########################
# We now resort to non-liner models, namly General Additive Models (GAM).
# Here, smoothing splines are exploitet to get a better fit.
# Simply put, smoothing splines allow us to divide the range of multiple covariates in k seperate regions and 
# fit distinct functions. We will do just that.
# Note: with these models, we focus on the structure of the relationship, the p-values etc. are of secondary interest,
########################

# Lets start with a very basic model
GAM_1 <- gam(Life.Exp ~ s(Adol.Fertility) + s(Educ.Exp) + s(Health.Exp) +
               s(GirlsBoys.Ratio) + s(log(Total.Pop)) +region + factor(year) , data= Data_Robust, method="GACV.Cp")

summary(GAM_1) 
# We obtain a rather high R² with 87%.

# Visualize the model
par(mfrow=c(2,2))
plot(GAM_1, pages=1, scheme=1, shade.col='gray90')
# We find clear cut non-linear relationships. 
# Adolescent fertility: Initially a strongly negative impact, but beginning at roughly 70 births per 1000 women (aged 15-19) the effect decreases
# Educational expenditures: A strong positive impact initially (up to 4% of GDP), then slow decline
# Health expenditurs: Starting from 6% of GDP strong positive effect
# Girs vs. boys primary enrolement ratio: Positive effect
# Population: remains mainly flat

#Diagnostics
gam.check(GAM_1) 
# The diagnostics indicate that - in comparison to the OLS model - normality of the residuals is much more likel to hold
# in this case. Additionally, the statistics indicate that the arbitrary setting of the knots (with 9 k) was a bit too low.
# We will correct this down below.


# Change specification #k controlls the "wigglyness". Also set the basis functions (cubic sline)
GAM_2 <- gam(Life.Exp~ s(Adol.Fertility, bs="cr", k=15) + s(GirlsBoys.Ratio, bs="cr", k=15) + s(Educ.Exp, bs="cr", k=15) +
               s(Health.Exp, bs="cr", k=15) + s(log(Total.Pop),bs="cr", k=15) + factor(year) + region, data= Data_Robust, method="GACV.Cp")

summary(GAM_2)
# R² of 88%. Somewhat better

#Diagnostics
gam.check(GAM_2)

# Visualize
plot(GAM_2, pages=1, scheme=1, shade.col='gray90')
# Curves now have somewhat more flexibility, but the structure of the relationship does not change

# Model evaluation: Use the AIC to decide
AIC(GAM_1) #6215
AIC(GAM_2) #6156
# Pick the second model

########################
# Obviously with non-linear modeling, there exits a problem with interpretation. 
# However, we saw that this approach worked much better than an OLS regression and we 
# can get a clear grasp of the relationship between the variables.
########################


########################
# Now, we will take a closer look at logistic regression models
# We will put the countries in two groups: "Rich" and "Poor" and then
# try to predict their "affiliation". To do this, we will of course split 
# our sample to into test and training data.
########################


# Manipulate the Data

# Change factor variable to a string, other wise they can't be changed.
Data_Robust$income <- as.character(Data_Robust$income)

# Create a new "Status" variable: High and Upper middle income countries belong to the "rich" group (1), 
# other observations to the poor "category" (0)
Data_Robust$Status[Data_Robust$income == "High income: nonOECD" | Data_Robust$income == "High income: OECD" | Data_Robust$income =="Upper middle income"] <- 1 
Data_Robust$Status[Data_Robust$income =="Low income" | Data_Robust$income =="Lower middle income" ] <- 0

# Create the new factor variable 'Status'
Data_Robust$Status<-factor(Data_Robust$Status, levels=c(0,1),labels=c("Poor","Rich"))

# We have a slight bias tw. rich countries:
table(Data_Robust$Status)

# "Rich" is the baseline
contrasts(Data_Robust$Status)

# Define test and training data
Train <- (Data_Robust$year <=2007)
Test<- Data_Robust[!Train ,]
Realization<- Data_Robust$Status[!Train]


#  Estimate the model
GLM <- glm(Status ~ Adol.Fertility +GirlsBoys.Ratio + Educ.Exp + Health.Exp + Life.Exp + region, family = binomial,subset=Train,data=Data_Robust)
summary(GLM)
# Spendings on education and health care are apparently not relevant in this sample.
# Interpretation? -> Use exponential transformation to interpret as "odds" (p/1-p)
exp(coef(GLM))
# Odds of being in the "Rich" category increase with more girls in primary education and life expectancy.
# They decrease with fertility rates (by a factor of 0.91)


# How "good" can our model predict the outcome? -> Create the confusion matrix:
# Get the probabilities for the training data
GLM_Prob <- predict(GLM, type="response",data=Train)

# Vector with Classification
GLM_Pred <-rep("Poor",length(GLM_Prob))  
GLM_Pred[GLM_Prob > .5] <- "Rich"

# How well did we do?
table(GLM_Pred,Data_Robust[Train,]$Status)
mean(GLM_Pred==Data_Robust[Train,]$Status)
# Correct classification in 89% of the cases. Thus, the training error is 11%.

# How about the test error:
# Get the probabilities for the training data
GLM_Prob <- predict(GLM,newdata=Test, type="response")

# Vector with Classification
GLM_Pred <-rep("Poor",dim(Test)[1])  
GLM_Pred[GLM_Prob > .5] <- "Rich"

# What is the test error?
table(GLM_Pred,Realization)
mean(GLM_Pred==Realization)
# The test error is 17%

########################
# We have now constructed a model that allows us to classify the countries in "rich" and "poor". It does a rather
# good job, with a test error of 17%.
########################
