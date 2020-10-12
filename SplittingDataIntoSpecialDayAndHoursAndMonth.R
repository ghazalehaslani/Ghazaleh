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
                            Month==12 |Month==1| Month==2 ~ "Winter")) %>%
  mutate(mont = case_when(Month==1  ~ "January",
                          Month==2  ~ "February",
                          Month==3  ~ "March",
                          Month==4  ~ "April",
                          Month==5  ~ "May",
                          Month==6  ~ "June",
                          Month==7  ~ "July",
                          Month==8  ~ "August",
                          Month==9  ~ "September",
                          Month==10  ~ "October",
                          Month==11  ~ "November",
                          Month==12  ~ "December"))
                            


#January, February, March, April, May, June, July, August, September, October, November, December
test$TypeOfDay<-as.factor(test$TypeOfDay)
test$Hour<-as.factor(test$Hour)
test$TypeOfHour<-as.factor(test$TypeOfHour)
test$Month<-as.factor(test$Month)
test$season<-as.factor(test$season)
#test$season<-	factor(c("Spring", "Summer","Autumn","Winter"), levels=c("Spring", "Summer","Autumn","Winter"))
test$DayWeek<-as.factor(test$DayWeek)
#test$DayWeek<-factor(c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"), levels=c("Monday", "Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday" ))
test$mont <-as.factor(test$mont )
#*****************************************************
#%%%%%%%%%%% Type of Hour %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#=================================================================================
#*# @@@@@@@@@@@@@@@@@@@@@@@@@@@ ALL Days seperately @@@@@@@@@@@@@@@@@@@@@@@@
dd<-rep(1:7, by=1) # different days
dh<-rep(1:24, by=1) # different hours
dm<-rep(1:12, by=1) # different months
DAY<-c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")




#*****************************************************
#*****************************************************
#*#*****************************************************
#* Selecting Day
dd=1 #* Selecting Day
dm=6 #* Selecting month
#*****************************************************          
  d.dat<-test[which(test$DayWeek==DAY[dd]),]
  d.dat<-d.dat[which(d.dat$Month==dm),]
  #***********************# new empty list ************************************
  dat.summary<-data.frame(Time=numeric(),N.BT=numeric(),N.ATC=numeric(),N.Bus=numeric(),
                          min.BT=numeric(),min.ATC=numeric(),min.Bus=numeric(),
                          max.BT=numeric(),max.ATC=numeric(),max.Bus=numeric())
  myplots.scatt <- list()  # new empty list for scatterplots
  myplots.pred <- list()  # new empty list for predict plots
  out<-NULL # new empty list for regression summary
  out.day <- NULL # new empty list for regression summary
  myplots.res <- list()  # new empty list for residual plots
  dat.coeff<-data.frame(Time=numeric(),ATC.coeff=numeric(),Bus.coeff=numeric())
  #*****************************************************v
#*****************************************************
for(i in dh){
  # Selecting hour
  h.d.dat<-d.dat[which(d.dat$Hour==(i-1)),]
  # Making scatterplot for that particular day & hour
  p<-ggplot() +
    geom_point(data = h.d.dat, aes(x = ATC, y = BT))+
    #ggtitle(paste(" Hour\n",(i-1),":",(i),"\n Month:",h.d.dat$mont[1]))+
    ggtitle(paste("Day & Hour\n",DAY[dd],"-",(i-1),":",(i),"\n Month:",h.d.dat$mont[1]))+
    xlim(0,250)+ylim(0,80)
  p
  # Saving scatterplot for that particular day & hour
  myplots.scatt[[i]] <- p  # add each plot into plot list
  # Making summary for that particular day & hour
  s<-data.frame(Time=paste((i-1),":",(i)),N.BT=sum(h.d.dat$BT),N.ATC=sum(h.d.dat$ATC),N.Bus=sum(h.d.dat$Bus.count),
                min.BT=min(h.d.dat$BT),min.ATC=min(h.d.dat$ATC),min.Bus=min(h.d.dat$Bus.count),
                max.BT=max(h.d.dat$BT),max.ATC=max(h.d.dat$ATC),max.Bus=max(h.d.dat$Bus.count))
  # Saving summary for that particular day & hour
  dat.summary<-rbind(dat.summary,s) 
  
  
  #@@@@@@@@@@@@@@@@@@@@@@@@@@ Regression part@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # simple regression
  Mod_lm <- lm(BT ~ ATC+Bus.count, data =h.d.dat) 
  summary(Mod_lm)
  out.day[[i]]<-summary(Mod_lm) # save summary of each model in list
  
  # Saving ATC coeff & Bus coeff in a data frame
  beta1<-Mod_lm[["coefficients"]][["ATC"]]
  beta2<-Mod_lm[["coefficients"]][["Bus.count"]]
  
  tt<-data.frame(Time=paste((i-1)),ATC.coeff=beta1,Bus.coeff=beta2)
  dat.coeff<-rbind(dat.coeff,tt)
  
  #plot(fitted( Mod_lm),residuals( Mod_lm), main=paste("linear model-",DAY[dd],"-",i-1,":",i))
  #abline(h=0, col="red")
  
  # plot the predicted values against ATC count, for a fixed number of buses
  
  png(paste0("linear model",DAY[dd],i,".png"))
  plot(fitted( Mod_lm),residuals( Mod_lm), main=paste("linear model-",i-1,":",i))
  abline(h=0, col="red")
  dev.off()
  
  
  check<-as.data.frame(cbind(h.d.dat$ATC,h.d.dat$fixed.bus))
  
  bus<-round(mean(h.d.dat$Bus.count))
  fixed.bus<-rep(bus,length = nrow(h.d.dat))
  check<-check %>% mutate(fixed.bus)
  colnames(check)<-c("ATC","Bus.count")
  
  prediction.lin <- predict(Mod_lm,newdata=check) # linear
  
  
  plot.pred<-as.data.frame(cbind(check$ATC,prediction.lin))
  colnames(plot.pred)<-c("ATC","lin.pred")
  
  
  # Plot All models 
  pp<-ggplot() +
    geom_point(data = h.d.dat, aes(x = ATC, y = BT), color="gray")+
    geom_line(data = plot.pred, aes(x = ATC, y = lin.pred), size = 1)+
    ggtitle(paste("Day & Hour\n",DAY[dd],"-",(i-1),":",(i)))
  
  pp
  
  # Saving scatterplot for that particular day & hour
  myplots.pred[[i]] <- pp  # add each plot into plot list
  
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # For Regression part
  #lin<-data.frame(residual=numeric,fit=numeric())
  #stop<-5 # Number of iterations
  #t<-1
  #W0<-rep(1,length = nrow(h.d.dat)) # Weight vector
  #while(t<=stop){
  # linear with normalize weights
  #  Mod_lm <- lm(BT ~ ATC+Bus.count, data =h.d.dat,weights = W0) 
  #  summary(Mod_lm)
  # Starting Non-parametric variance function estimation 
  # lin<-as.data.frame(cbind(log(abs(residuals(Mod_lm))),fitted(Mod_lm)))
  # colnames(lin)<-c("residual","fit")
  # loess smoothing 
  # sp<-0.5
  # lowess_values <- loess(residual ~ fit, data=lin, span=sp)
  # phat would use to estimate Std & Var, as we used logarithm function,  
  # so exp(phat) is Std and exp(2*phat) would be Var
  # phat <- predict(lowess_values) 
  #  lin <- as.data.frame(cbind(lin,phat)) 
  #  std_est<-exp(lin$phat)
  #  var_est<-exp(2*lin$phat)
  #  Normal.W<-var_est/mean(var_est)
  #  W0<-1/Normal.W
  #  t<-t+1
  #}
  #out.day[[i]]<-summary(Mod_lm)
  
  #plot(fitted( Mod_lm),residuals( Mod_lm)*sqrt(W0),main=paste("linear model-",DAY[dd],"-",i-1,":",i))
  #abline(h=0, col="red")
  #plot(Mod_lm$fit, var_est, xlab="Fitted ", ylab="Variance",main=paste("linear model-",DAY[dd],"-",i-1,":",i))
  
  
}
  



# plot hourly changes in ATC & Bus coefficients
ggplot(data=dat.coeff, aes(x=as.numeric(Time),y=ATC.coeff)) +
  geom_line()+
  #ggtitle("Hourly changes in ATC Coefficients",DAY[dd]) +
  ggtitle(paste("Hourly changes in ATC Coefficients  ",h.d.dat$mont[1],DAY[dd])) +
  labs(x="Hours",y="ATC coeff")+scale_x_continuous(breaks = seq(0, 23, 1))+ylim(0,0.5)



ggplot(data=dat.coeff, aes(x=as.numeric(Time),y=Bus.coeff)) +
  geom_line()+
  ggtitle(paste("Hourly changes in Bus Coefficients  Month -",h.d.dat$mont[1],DAY[dd])) +
  #ggtitle("Hourly changes in Bus Coefficients",DAY[dd]) +
  labs(x="Hours",y="Bus coeff") +scale_x_continuous(breaks = seq(0, 23, 1))+ylim(-1,2)











