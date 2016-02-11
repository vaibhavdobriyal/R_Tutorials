#just some clean up
rm(list=ls())
set.seed(42)

############################## Data Prepartaion Started ################################
#loading data
#this sample only had data for one site and includes temperature and usage
dat <- read.table("/Users/vaibhav/Essentials/Machine_Learning/trials_semented_regression/334_13_temperatureData.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
head(dat)

library(plyr)
#renaming columns 
dat <- rename(dat,c("dailytemperaturemean"="tmp_mean",
                    "dailyopenhourmean"="usage_mean_open"
              ))
head(dat)

#Only analysing for a meter and usage_mean_open
#dat <- subset(dat, siteid == '334_13')
relevantData <-  dat[,c("tmp_mean","usage_mean_open")]

#aading the temperature category atttributed
relevantData<-rename(relevantData,c("usage_mean_open"="usage"))
relevantData <- subset(relevantData, usage > 0)


processedData <- rename(relevantData,c("tmp_mean"="temperature_mean","usage"="usageKWhr"))
head(processedData)

# color coding based on bands not relevant at all
#processedData = within(processedData, {
#  category_temp = ifelse(temperature_mean >= 20, "Heating", ifelse(temperature_mean<=14,"Cooling","Flat"))
#})

#head(processedData)

# TODOoutlier correction

############################## Data Prepartaion Finished ################################
############################## Visualizing Raw Data #######################################
library(ggplot2)

# PLotting raw data
plot <- ggplot(aes(y = usageKWhr , x = temperature_mean), data = processedData) + 
  geom_point(aes(y = usageKWhr , x = temperature_mean)) +
  theme_bw(16) + 
  ggtitle("Usage vs. Temperature")+
  scale_size_area() + 
  xlab("Temperature Mean") +
  ylab("Usage WHr") 



################################## Data Analysis Begins####################################
library(segmented)

# usageKWhr is Y and temperature_mean is X
# we are doing a linear regression
lfit <- lm(usageKWhr ~ temperature_mean, data = processedData)
coef <- coef(lfit)

# The other alternate was to use MASS which has on option of rlm instead of lm above
# supposedly less impacted by outliers and noise 
#library(MASS)
#lfit <- rlm(usageKWhr ~ temperature_mean, data = processedData)
#coef <- coef(lfit)
# Approach rejected for now because there was not much differece - for now sticking no simple linear model 

######################outlier correction - let us take the basic approach################################## 

#remove X% od points with max residual and repeat regression
outlier_percent = 0
fitted <- fitted(lfit)
prelim_model <- data.frame(temperature_mean = processedData$temperature_mean, usageKWhr = fitted, orig_usage=processedData$usageKWhr)
#calculating absolute residual that is |observed - estimated|
prelim_model$residual_diff <- abs(prelim_model$orig_usage - prelim_model$usageKWhr)
#sorting by residual
prelim_model <- prelim_model[order(-prelim_model[,4]),]

#removing X% extreme residuals and following is the subset data which may be used for next round of regression
processedData2 <- tail(prelim_model,nrow(prelim_model)-as.integer(nrow(prelim_model)*(outlier_percent))) 
                       
#repeat regression with the data minus outliers
#Rejected performing repeat as R square was running too close to 1
#Depends on dataset - uncomment following two in case results are to be analyzed

#lfit <- lm(usageKWhr ~ temperature_mean, data = processedData2)
#coef <- coef(lfit)

summary(lfit)
print(lfit)

################################outlier correction finished###################################################

plot <- plot + 
 geom_hline(aes(yintercept = 0), color="grey", linetype="dashed")+
 geom_vline(aes(xintercept=0),color="grey",linetype="dashed")

#incase visualization of somple liner is required
# we could have plottted using the line as we have slope and intercept 
plot <- plot + geom_smooth(method="lm", se = FALSE) 

plot

################################Peicewise regression###########################################################
# performing segmentedregression using the linear model obtained
#without psi this may return anything for number of segments - algo is free to choose number of breakpoints 
#as well as position of breakpoint
sfit <- segmented(lfit, seg.Z = ~temperature_mean)

#trying with position of breakpoints
#sfit <- segmented(lfit, seg.Z = ~temperature_mean, psi = list(temperature_mean = c(14,20)))

#trying with number of breakpoints 
#sfit <- segmented(lfit, seg.Z = ~temperature_mean,psi=NA, control=seg.control(display=TRUE,K=1))


#plot(sfit, conf.level=0.95, shade=TRUE)

summary(sfit)
print(sfit)

# get the breakpoints
sfit$psi
slope(sfit)


# get the fitted data
fitted <- fitted(sfit)
model_residuals <- residuals(sfit)
sd_residuals <- sd(model_residuals,na.rm = T)

model <- data.frame(temperature_mean = processedData$temperature_mean, usageKWhr = fitted)

# plot the fitted model

plot <- plot+
 geom_point(data=model,aes(y = usageKWhr , x = temperature_mean),color="red",size=2) +
 #geom_line(data=model,aes(y = usageKWhr , x = temperature_mean),color="red") +
 geom_vline(aes(xintercept=14),color="grey",linetype="dashed",size=1.5) +
 geom_vline(aes(xintercept=20),color="grey",linetype="dashed",size=1.5) +
 geom_ribbon(data=model,aes(ymin = usageKWhr - sd_residuals, ymax = usageKWhr + sd_residuals), fill = "grey70",alpha=0.2) 
plot




