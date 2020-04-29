#### Example Description ####
## In this example, we developed a generalized additive model (GAM) to quantify the relationship between habitat variables and presence & density of juvenile lobsters using the data from ME-NH Inshore Bottom Trawl Survey. 
## The first stage of the model is to predict the presence/absence of juvenile lobster at each site, and the second stage of the model is to predict the density of lobsters at each site. 
## Input data:
## 1) two_stages_trawl_survey.csv
## 2) two_stages_environmental_data.csv

#### Install and load packages ####
list_of_packages <- c("rstudioapi", "maptools", "mgcv", "classInt", "RColorBrewer", "maps", "mapdata", "fields")
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(rstudioapi)
library(maptools)
library(maps)
library(mapdata)
library(mgcv)
library(RColorBrewer)
library(classInt)
library(fields)

#### Set working directory and library required packages ####
setwd("/Users/Bai/Desktop/Species_Distribution_Modeling_Tutorial")

mndata<-read.csv('./data/two_stages_trawl_survey.csv') # Read Maine-New Hampshier Bottom Trawl Survey data. Lobster (< 60 mm CL) abundance/ habitat variables (Bottom water temperature/ Salinity/ Sediment Type/ Latitude/ Longitude/ Depth/ Distance Offshore/ Distance to Sediment Boundary). There are 473 observations. 

predictiondata<-read.csv('./data/two_stages_environmental_data.csv') # Read potential sampling station data. Station Latitude/ Longitude/ Bottom water temperature/ Salinity/ Sediment Type/ Depth/ Distance Offshore/ Distance to Sediment Boundary. The temperature data are from September 2000. There are 700 observations. 

#### 1.1 Build GAM ####
# Presence/ Absence GAM
mndata$presence <- (mndata$abundance > 0) * 1 # Presence = 1 if the station has lobster, otherwise presence = 0

presence.full<-gam(presence ~ s(Latitude, k=5)+s(Longitude, k=5)+s(Temperature, k=5)+s(Salinity, k=5)+s(Distance_Offshore, k=5)+s(Depth, k=5)+s(Distance_Sediment_Boundary, k=5)+as.factor(Sediment_Type), family=binomial(logit), data=mndata) # Build first stage GAM with all possible habitat variables
summary(presence.full) # Find significant variables based on p-value

presence.sig<-gam(presence ~ s(Latitude, k=5)+s(Longitude, k=5)+s(Temperature, k=5)+s(Distance_Offshore, k=5),family=binomial(logit),data=mndata) # Build first stage GAM with significant habitat variables
summary(presence.sig) 
par(mfrow=c(2,2))
gam.check(presence.sig)

# Abundance GAM
abundance.full<-gam(log(abundance) ~ s(Latitude, k=5)+s(Longitude, k=5)+s(Temperature, k=5)+s(Salinity, k=5)+s(Distance_Offshore, k=5)+s(Depth, k=5)+s(Distance_Sediment_Boundary, k=5)+as.factor(Sediment_Type), family=gaussian,data=subset(mndata,abundance>0)) # Build second stage GAM with all possible habitat variables
summary(abundance.full) # Find significant variables based on p-value

abundance.sig<-gam(log(abundance) ~s(Latitude, k=5)+s(Longitude, k=5)+s(Temperature, k=5)+s(Depth, k=5), family=gaussian, data=subset(mndata,abundance>0)) # Build first stage GAM with significant habitat variables
summary(abundance.sig)
par(mfrow=c(2,2))
gam.check(abundance.sig)

# Plot GAM response curves
par(mar=c(4,4,1,1))
layout(matrix(1:8, ncol=2, byrow=FALSE))
plot(presence.sig, select =1, scale =0,ylab = expression(bold(Presence)), xlab = expression(bold(Latitude)))
plot(presence.sig, select =2, scale =0,ylab = expression(bold(Presence)), xlab = expression(bold(Longitude)))
plot(presence.sig, select =3, scale =0,ylab = expression(bold(Presence)), xlab = expression(bold(Temperature)))
plot(presence.sig, select =4, scale =0,ylab = expression(bold(Presence)), xlab = expression(bold(Distance~Offshore)))
plot(abundance.sig, select =1, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Latitude)))
plot(abundance.sig, select =2, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Longitude)))
plot(abundance.sig, select =3, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Temperature)))
plot(abundance.sig, select =4, scale =0,ylab = expression(bold(Abundance)), xlab = expression(bold(Depth)))

#### 1.2 Cross Validation ####
mndata_cv <- mndata

mndata_cv  = mndata_cv [!is.na(mndata_cv $Latitude) & !is.na(mndata_cv $Longitude) & !is.na(mndata_cv $Temperature) & !is.na(mndata_cv $Depth) & !is.na(mndata_cv $Sediment_Type) & !is.na(mndata_cv $Distance_Offshore) & !is.na(mndata_cv $Salinity) & !is.na(mndata_cv $Distance_Sediment_Boundary),]#Remove NAs

N = nrow(mndata_cv )
res = matrix(0, ncol = 3, nrow = 100)
colnames(res) = c("Intercept", "Slope", "R.Squared")
for (i in 1:100){          
    print(i)
    sub = sample(1:N, size = N/4)
    train = mndata_cv[-sub,]
    test = mndata_cv[sub,]
    for (j in 1: length(test$abundance)) {
        if (test$abundance[j] == 0 | test$abundance[j] < 0){
            test$abundance[j] = 0.0001
        } else {
            test$abundance[j] = test$abundance[j]
        }
    } 
    logtest = log(test$abundance)
    
    presence_cv <- gam(presence ~ s(Latitude, k=5)+s(Longitude,k=5)+s(Temperature,k=5)+s(Distance_Offshore),family=binomial(logit), data=train, select=TRUE, method="REML")
    
    abundance_cv <- gam(log(abundance) ~ s(Latitude, k=5)+s(Longitude,k=5)+s(Temperature,k=5)+s(Depth), family=gaussian, data=subset(train,abundance>0), select=TRUE, method="REML")
    
    prepresence = predict.gam(presence_cv, test, se.fit=TRUE, type = "response")
    prep = as.matrix(prepresence$fit)
    prev = as.matrix(prepresence$se.fit)
    predensity=predict.gam(abundance_cv, test, se.fit=TRUE, type = "response")
    preden=as.matrix(predensity$fit)
    prevar=as.matrix(predensity$se.fit)
    #newdensity=prep * exp(preden)
    newdensity = log(prep) + preden
    #lognewdensity = log(newdensity)
    result = cbind(logtest, newdensity) #lognewdensity)
    final = result[result[,1]>0 & result[,2]>0,]
    q = lm(final[,1] ~ final[,2], data = as.data.frame(final))
    res[i,] = c(summary(q)$coefficient[,1], summary(q)$r.squared)
}
summary(res)
par(mfrow=c(1,1))
plot(NULL,xlim=c(0,7),ylim=c(0,7), xlab = expression(bold("Predicted Lobsters (log #)")), 
     ylab = expression(bold("Observed Lobsters (log #)")), xaxt="n", yaxt="n")
axis(side=1, font=2)
axis(side=2, font=2)
addlines <- function(x) abline(x, col="gray70")
apply(res[,-3],1,addlines)

abline(a = mean(res[,1]), b = mean(res[,2]), col="black", lwd=3)
abline(0,1,lty =2, lwd=3)
text(2, 6.5, expression(bold(italic(y) == 0.65 + 0.86 %.% italic(x)*","~~italic(r)^2~"="~0.36)))

#### 1.3 GAM Prediction ####
prepresence = predict.gam(presence.sig, predictiondata, se.fit=TRUE, type = "response") # Fit first stage GAM with potential sampling station data
p.presence = as.matrix(prepresence$fit) # Presence values of lobsters at potential sampling stations 
p.presencevar = as.matrix(prepresence$se.fit^2) # Associated variance of presence after fitting model
preabundance=predict.gam(abundance.sig, predictiondata, se.fit=TRUE) # Fit second stage GAM with potential sampling station data
p.abundance=as.matrix(preabundance$fit) # Abundance values of lobsters at potential sampling stations
p.abundancevar=as.matrix(preabundance$se.fit^2) # Associated variance of abundance after fitting model

#### 1.4 Combine two-stage GAM ####
simabundance = p.presence * p.abundance # Predicted lobster abundance at potential sampling stations by combining results from two stages
simvar = p.presence * p.abundancevar + (p.abundance)^2 * p.presence *(1-p.presence) # Associated variance of simulated abundance at potential sampling stations

#### 1.5 Plot simulated abundance ####
colors <- brewer.pal(9, "YlOrRd")
colbrks<-classIntervals(simabundance, n=9, style="quantile")
brks<- colbrks$brks
par(mfrow=c(1,1)); par(mar=c(4,4,1,1))
plot(predictiondata$Longitude, predictiondata$Latitude, col=colors[findInterval(simabundance, brks ,all.inside=TRUE)], pch = 20, xlab="Longitude", ylab="Latitude")


