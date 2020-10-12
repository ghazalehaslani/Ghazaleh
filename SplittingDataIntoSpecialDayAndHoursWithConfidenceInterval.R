library(chron) 
library(lubridate)
library(scales)
library(dplyr) 
library(tidyr)
library(rlist)
library(stringr)
library(tidyverse)
library(anytime)
library(reshape2)
library(zoo)
library(magrittr)
library(reshape)
library(arsenal)
library("dplyr", character.only = TRUE)
library(data.table)
library(imputeTS)
library(ggplot2)
library(sfsmisc)
library(reshape)
library(e1071)
library(splines)
options(scipen=999)
library(nlme)
library(segmented)
library(tictoc)
library(lmtest)


All_Data_BUS<-read.csv(file="C:\\Users\\gaslani\\OneDrive - Massey University\\FirstProject\\NewTasks\\LocationTwo\\BT_BUS.csv")
All_Data_BUS<-All_Data_BUS %>% select(-c(X,Bus.prop))
colnames(All_Data_BUS)<-c("BT", "Bus.count","ATC","day","time")

# Adding another variable as time of day and Type of day ( weekdays vs weekend)
f =as.POSIXct(All_Data_BUS$time,tz="Europe/London", origin="1970-01-01")
Hour<-hour(f)
mon<-month(f)

All_Data_BUS=All_Data_BUS  %>% mutate(Hour = Hour)%>% mutate(Month= mon) %>% mutate(DayWeek = weekdays(as.Date(day)))

test<-All_Data_BUS %>%
  mutate(TypeOfDay = case_when(DayWeek=="Monday" |DayWeek=="Tuesday"| DayWeek=="Wednesday" | DayWeek=="Thursday"| DayWeek=="Friday" ~ "Weekday", 
                               DayWeek=="Saturday" |DayWeek=="Sunday"  ~ "Weekend"))%>%
  mutate(TypeOfHour = case_when(7<=Hour & Hour<10  |15<=Hour & Hour<19 ~ "Busy-time", 
                                TRUE  ~ "Non-Busy-time")) %>%
  mutate(season = case_when(Month==3 |Month==4| Month==5  ~ "Spring", 
                            Month==6 |Month==7| Month==8  ~ "Summer", 
                            Month==9 |Month==10| Month==11  ~ "Autumn",
                            Month==12 |Month==1| Month==2 ~ "Winter")) 


test$TypeOfDay<-as.factor(test$TypeOfDay)
test$Hour<-as.factor(test$Hour)
test$TypeOfHour<-as.factor(test$TypeOfHour)
test$Month<-as.factor(test$Month)
test$season<-as.factor(test$season)
#test$season<-	factor(c("Spring", "Summer","Autumn","Winter"), levels=c("Spring", "Summer","Autumn","Winter"))
test$DayWeek<-as.factor(test$DayWeek)
#test$DayWeek<-factor(c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"), levels=c("Monday", "Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday" ))

#*****************************************************
#%%%%%%%%%%% Type of Hour %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#=================== ===========================================
# Splitting Up data set into BUSY  and NON-BUSY time
# BUSY-TIME: 7:00-10:00 A.M. & 3:00-7:00 P.M.
#=================================================================================
#*# @@@@@@@@@@@@@@@@@@@@@@@@@@@ ALL Days seperately @@@@@@@@@@@@@@@@@@@@@@@@
dd<-rep(1:7, by=1) # different days
dh<-rep(1:24, by=1) # different hours
DAY<-c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")




#*****************************************************
#*****************************************************
#*#*****************************************************
#* Selecting Day
dd=2
d.dat<-test[which(test$DayWeek==DAY[dd]),]
#***********************# new empty list ************************************
myplots.scatt <- list()  # new empty list for scatterplots
myplots.pred <- list()  # new empty list for predict plots
out<-NULL # new empty list for regression summary
out.day <- NULL # new empty list for regression summary
myplots.res <- list()  # new empty list for residual plots
dat.coeff<-data.frame(n=numeric,Day=character(),Month=character(),ATC.coeff=numeric(),Bus.coeff=numeric(),
                      upper.beta1=numeric(),lower.beta1=numeric(),
                      upper.beta2=numeric(),lower.beta2=numeric() )
#dat.coeff<-data.frame(Time=numeric(),ATC.coeff=numeric(),Bus.coeff=numeric())
#*****************************************************v


#*****************************************************
for(i in dh){
  # Selecting hour
  h.d.dat<-d.dat[which(d.dat$Hour==(i-1)),]
  # Making scatterplot for that particular day & hour
  p<-ggplot() +
    geom_point(data = h.d.dat, aes(x = ATC, y = BT))+
    ggtitle(paste("Day & Hour\n",DAY[dd],"-",(i-1),":",(i)))+
    xlim(0,230)+ylim(0,90)
  p
  # Saving scatterplot for that particular day & hour
  myplots.scatt[[i]] <- p  # add each plot into plot list
  
  
  #@@@@@@@@@@@@@@@@@@@@@@@@@@ Regression part@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # simple regression
  Mod_lm <- lm(BT ~ ATC+Bus.count, data =h.d.dat) 
  summary(Mod_lm)
  out.day[[i]]<-summary(Mod_lm) # save summary of each model in list
  
  # Saving ATC coeff & Bus coeff in a data frame
  beta1<-Mod_lm[["coefficients"]][["ATC"]]
  up.beta1<-beta1+2*summary(Mod_lm )[["coefficients"]][2,2] # summary(Mod_lm )[["coefficients"]][2,2]: std error of beta 1
  low.beta1<-beta1-2*summary(Mod_lm )[["coefficients"]][2,2]
  # Bus coeff
  beta2<-Mod_lm[["coefficients"]][["Bus.count"]]
  up.beta2<-beta2+2*summary(Mod_lm )[["coefficients"]][3,2] # summary(Mod_lm )[["coefficients"]][2,2]: std error of beta 1
  low.beta2<-beta2-2*summary(Mod_lm )[["coefficients"]][3,2]
  
  tt<-data.frame(Time=paste((i-1)),ATC.coeff=beta1
                 ,Bus.coeff=beta2,upper.beta1=up.beta1,lower.beta1=low.beta1,
                 upper.beta2=up.beta2,lower.beta2=low.beta2 )
    
    dat.coeff<-rbind(dat.coeff,tt)
  
  # plot the predicted values against ATC count, for a fixed number of buses
  
  #png(paste0("linear model",DAY[dd],i,".png"))
  plot(fitted( Mod_lm),residuals( Mod_lm), main=paste("linear model-",DAY[dd],"-",i-1,":",i))
  abline(h=0, col="red")
  #dev.off()
  # Plotting regression line
  check<-as.data.frame(cbind(h.d.dat$ATC,h.d.dat$fixed.bus))
  # Considering bus count as a fixed number
  bus<-round(mean(h.d.dat$Bus.count))
  fixed.bus<-rep(bus,length = nrow(h.d.dat))
  check<-check %>% mutate(fixed.bus)
  colnames(check)<-c("ATC","Bus.count")
    prediction.lin <- predict(Mod_lm,newdata=check) # linear
    plot.pred<-as.data.frame(cbind(check$ATC,prediction.lin))
  colnames(plot.pred)<-c("ATC","lin.pred")
  
  # Plot regression line with fixed number of bus 
  pp<-ggplot() +
    geom_point(data = h.d.dat, aes(x = ATC, y = BT), color="gray")+
    geom_line(data = plot.pred, aes(x = ATC, y = lin.pred), size = 1)+
    ggtitle(paste("Day & Hour\n",DAY[dd],"-",(i-1),":",(i)))
  
  pp
  
  # Saving plot of regression line with fixed number of bus 
  myplots.pred[[i]] <- pp  # add each plot into plot list
  
  }


# Plot All coefficients in one plot 
# plot hourly changes in ATC & Bus coefficients
ggplot(data=dat.coeff, aes(x=as.numeric(Time),y=ATC.coeff)) +
  geom_line()+geom_point()+
  geom_line(data = dat.coeff, aes(x=as.numeric(Time),y=lower.beta1),linetype='dotted',color ="blue" ,size = 1)+
  geom_line(data = dat.coeff, aes(x=as.numeric(Time),y=upper.beta1),linetype='dotted',color ="green", size = 1)+
  ggtitle("Hourly changes in ATC Coefficients",DAY[dd]) +
  labs(x="Hours",y="ATC coeff")+scale_x_continuous(breaks = seq(0, 23, 1))+ylim(0,0.5)



ggplot(data=dat.coeff, aes(x=as.numeric(Time),y=Bus.coeff)) +
  geom_line()+geom_point()+
  geom_line(data = dat.coeff, aes(x=as.numeric(Time),y=lower.beta2),linetype='dotted',color ="blue" ,size = 1)+
  geom_line(data = dat.coeff, aes(x=as.numeric(Time),y=upper.beta2),linetype='dotted',color ="green", size = 1)+
  ggtitle("Hourly changes in Bus Coefficients",DAY[dd]) +
  labs(x="Hours",y="Bus coeff") +scale_x_continuous(breaks = seq(0, 23, 1))+ylim(-3,4)




