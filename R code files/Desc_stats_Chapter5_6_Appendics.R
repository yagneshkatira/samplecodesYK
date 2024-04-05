# Change to the directory where the data file is stored
knitr::opts_knit$set(root.dir = '/Data for analysis')

#install.packages("lifecycle")
#install.packages("rlang")
#install.packages("plyr")


library(tidyverse)
library(desctable)
library(stargazer)
library(lubridate)
library(dplyr)                                    
library(AER)
library(mclogit)
library(readr)
library(margins)
library(systemfit)
library(texreg)
library(devtools)
library(foreach)
library(lubridate)
library(data.table)
library(ggplot2)
library(scales)
library(readr)                                    
library(purrr)
library(stringr)
library(zoo) #provides extra tools for working with time series data and analysis
library(fpp3) #package that comes along with Forecasting: Principles and practice
library(dynlm)
library(tsibble)


## Elspot analysis 1

# Indexing time series


hourdata <- fread("data_fordescstats.csv")


hourdata <- hourdata %>%
  mutate(time = as.POSIXct(hourdata$tseries, format="%Y-%m-%d %H:%M:%OS", tz="UTC"))

# Plot of prices .....Figure 5.1

hourdata %>% dplyr::select(de_daprice, no2_daprice, time) %>% 
  pivot_longer(-time, names_to="variable", values_to="value") %>%
  ggplot(aes(x=time, y=value, color=variable)) +
  geom_line(size=0.001) + theme_bw()

hourdata <- hourdata %>%
  mutate(pricediff = hourdata$de_daprice - hourdata$no2_daprice)

postnldata = hourdata %>% filter(nl_indicator=="1")
prenlconnect = hourdata %>% filter(nl_indicator=="0")


## Plot of wind and PV forecast in Germany.....Figure A.3-4 and A.3-5

postnldata$month = as.character(postnldata$month)   
postnldata$hour = as.character(postnldata$hour) 

#Monthly
postnldata %>% dplyr::select(wind_fcast_de, pv_fcast_de, month) %>% 
  pivot_longer(-month, names_to="variable", values_to="value") %>%
  ggplot(aes(x=fct_relevel(month, "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"), y=value, fill=variable)) + stat_summary(fun = "mean", geom = "bar", position = "stack") + theme_bw() + xlab("Months") + ylab("Avg. hourly wind and PV forecast in Germany (in MW)") +   scale_fill_manual(name="",values=c("#eba834","#42c2f5"),labels=c("PV forecast","Wind forecast"))


#Hourly
postnldata %>% dplyr::select(wind_fcast_de, pv_fcast_de, hour) %>% 
  pivot_longer(-hour, names_to="variable", values_to="value") %>%
  ggplot(aes(x=fct_relevel(hour, "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24"), y=value, fill=variable)) + stat_summary(fun = "mean", geom = "bar", position = "stack") + theme_bw() + xlab("Hours") + ylab("Avg. hourly wind and PV forecast in Germany (in MW)") +   scale_fill_manual(name="",values=c("#eba834","#42c2f5"),labels=c("PV forecast","Wind forecast"))


##Import and Export flows......Figure A.3-3


postnldata$tmonth  <- as.Date(cut(postnldata$time, breaks = "month"))

postnldatagrouped <- postnldata %>%
  group_by(tmonth) %>%
  summarise_at(.vars = vars(imp,exp), .funs = c(sum="sum"))%>%
  select(tmonth, "Exports" = "exp_sum", "Imports" = "imp_sum")

postnldatagrouped %>% 
  ggplot(aes(x=tmonth))+
  geom_line(aes(y=Imports, color="Germany -> NO2"), size=0.5) +
  geom_line(aes(y=Exports, color="NO2 -> Germany"), size=0.5) +
  scale_y_continuous(labels = comma, breaks = seq(0, 1000000, 100000)) +
  scale_x_date(date_labels = "%b-%y", breaks = "1 month") + theme_bw() +
  theme(axis.text.x = element_text (size = 8, angle = 90)) +
  labs(x="Month", y="MW") + 
  geom_point(aes(y=Imports), color="blue")+ 
  geom_point(aes(y=Exports), color="red") +  
  scale_color_manual(values=c("blue","red")) + 
  theme(legend.position = "bottom") + 
  labs(colour="Flow Direction")


#####Comparative price analysis of NO2 and Germany.............Figure 5.2


hourdata$nl_indicator = as.character(hourdata$nl_indicator)   
hourdata$month = as.character(hourdata$month) 
hourdata$hour = as.character(hourdata$hour) 
hourdata$dayofwk = as.character(hourdata$dayofwk) 

##Monthly averages
ggplot(aes(x = fct_relevel(month, "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"), y = de_daprice, fill=nl_indicator), data = hourdata) + stat_summary(fun = "mean", geom = "bar", position="dodge") + theme_bw() +
  scale_fill_manual(name="",values=c("#eba834","#80550d"),labels=c("Pre-NordLink","Post-NordLink")) + xlab("Months") + ylab("German day-ahead prices (in EUR/MWh)")


ggplot(aes(x = fct_relevel(month, "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"), y = no2_daprice, fill=nl_indicator), data = hourdata) + stat_summary(fun = "mean", geom = "bar", position="dodge") + theme_bw() +
  scale_fill_manual(name="",values=c("#f5ccc1","#d60202"),labels=c("Pre-NordLink","Post-NordLink")) + xlab("Months") + ylab("NO2 day-ahead prices (in EUR/MWh)")


##Hourly averages over the day

ggplot(aes(x = fct_relevel(hour, "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24"), y = de_daprice, fill=nl_indicator), data = hourdata) + stat_summary(fun = "mean", geom = "bar", position="dodge") + theme_bw() +
  scale_fill_manual(name="",values=c("#eba834","#80550d"),labels=c("Pre-NordLink","Post-NordLink")) + xlab("Hours") + ylab("German day-ahead prices (in EUR/MWh)")


ggplot(aes(x = fct_relevel(hour, "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24"), y = no2_daprice, fill=nl_indicator), data = hourdata) + stat_summary(fun = "mean", geom = "bar", position="dodge") + theme_bw() +
  scale_fill_manual(name="",values=c("#f5ccc1","#d60202"),labels=c("Pre-NordLink","Post-NordLink")) + xlab("Hours") + ylab("NO2 day-ahead prices (in EUR/MWh)")

##Daily averages - Germany 

ggplot(aes(x = fct_relevel(dayofwk, "1", "2", "3", "4", "5", "6", "7"), y = de_daprice, fill=nl_indicator), data = hourdata) + stat_summary(fun = "mean", geom = "bar", position="dodge") + theme_bw() +
  scale_fill_manual(name="",values=c("#eba834","#80550d"),labels=c("Pre-NordLink","Post-NordLink")) + xlab("Days") + ylab("German day-ahead prices (in EUR/MWh)")

##Daily averages - NO2 

ggplot(aes(x = fct_relevel(dayofwk, "1", "2", "3", "4", "5", "6", "7"), y = no2_daprice, fill=nl_indicator), data = hourdata) + stat_summary(fun = "mean", geom = "bar", position="dodge") + theme_bw() +
  scale_fill_manual(name="",values=c("#f5ccc1","#d60202"),labels=c("Pre-NordLink","Post-NordLink")) + xlab("Days") + ylab("NO2 day-ahead prices (in EUR/MWh)")



# Correlation of DE and NO2 prices - Pre-NordLink period.....Figure 5.4

ggplot(prenlconnect, 
       aes(x=de_daprice, 
           y=no2_daprice))+
  geom_point()+
  geom_smooth(fill="blue", alpha=.01)+
  geom_smooth(method='lm', formula= y~x, se=FALSE, col="red",lty=2)+
  theme_bw() + xlab("German day-ahead prices (EUR/MWh)") + ylab("Day-ahead prices in NO2 (EUR/MWh)") 

# Correlation of DE and NO2 prices - Post-NordLink period.....Figure 5.4

ggplot(postnldata, 
       aes(x=de_daprice, 
           y=no2_daprice))+
  geom_point()+
  geom_smooth(fill="blue", alpha=.01)+
  geom_smooth(method='lm', formula= y~x, se=FALSE, col="red",lty=2)+
  theme_bw() + xlab("German day-ahead prices (EUR/MWh)") + ylab("Day-ahead prices in NO2 (EUR/MWh)") 


##### Graph of DE price, compared with average of 3 forward quarters.....Figure 5.9

postnldata %>% dplyr::select(wht_avg_deb, wht_avg_dep, de_daprice, time) %>% 
  pivot_longer(-time, names_to="variable", values_to="value") %>%
  ggplot(aes(x=time, y=value, color=variable)) +
  geom_line(size=0.00001) + theme_bw() + scale_color_manual(values=c('lightblue','red','darkgreen'),labels=c("Predicted German day-ahead spot prices","EEX DEB Quarter Futures daily closing price ( wght. avg.)","EEX DEP Quarter Futures daily closing price ( wght. avg.)")) + xlab("Time") + ylab("Prices (in EUR/MWh)") + theme(legend.position="bottom") + guides(color = guide_legend(nrow = 2)) + theme(legend.title=element_blank())


##### Distribution of transmission bottlenecks (Export and Import separately)...Figure 5.10

postnldata$tdate  <- as.Date(cut(postnldata$time, breaks = "day"))

detach(package:plyr)    
library(dplyr)

postnldata_cong <- postnldata %>%
  group_by(tdate) %>%
  summarise_at(.vars = vars(import_nl,export_nl,cong_imp,cong_exp,inuse_imp,inuse_exp), .funs = c(sum="sum"))%>%
  select(tdate, "Exports" = "export_nl_sum", "Imports" = "import_nl_sum", "CongImports" = "cong_imp_sum", "CongExports" = "cong_exp_sum", "InuseImports" = "inuse_imp_sum", "InuseExports" = "inuse_exp_sum")

postnldata_cong$month <- month(ymd(postnldata_cong$tdate))
postnldata_cong <- transform(postnldata_cong, monthname = month.abb[month])


postnldata_cong <- postnldata_cong %>%
  mutate(export_pc = CongExports/InuseExports,
         import_pc = CongImports/InuseImports)


library(ggplot2)

ggplot(postnldata_cong, aes(x=export_pc)) +
  stat_ecdf(size=1, color="darkgreen")  +
  xlab("Ratio of bottlenecks under export periods") +
  ylab("Density")+
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) + theme_bw()

ggplot(postnldata_cong, aes(x=import_pc)) +
  stat_ecdf(size=1, color="red") +
  xlab("Ratio of bottlenecks under import periods") +
  ylab("Density") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) + theme_bw()

##### Distribution of transmission bottlenecks (Export and Import separately - Month-wise break-up)...Figure A.3-1 and Figure A.3-2

postnldata_cong$monthname = as.character(postnldata_cong$monthname)

ggplot(postnldata_cong, aes(x=import_pc, color=fct_relevel(monthname, "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))) +
  stat_ecdf() +
  xlab("Ratio of bottlenecks under import periods") +
  ylab("Density")+
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) + theme_bw() + 
  labs(colour="Months")

ggplot(postnldata_cong, aes(x=export_pc, color=fct_relevel(monthname, "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))) +
  stat_ecdf() +
  xlab("Ratio of bottlenecks under export periods") +
  ylab("Density")+
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) + theme_bw() + 
  labs(colour="Months") 

## Descriptive statistics
##Density distribution of German prices....Figure 5.4

hourdata$nl_indicator = as.character(hourdata$nl_indicator)   


ggplot(hourdata, aes(x = de_daprice, fill = nl_indicator)) +
  geom_density(alpha=0.4)+
  xlab("Distribution of German day-ahead prices (EUR/MWh)") +
  ylab("Density") + 
  scale_fill_manual(name="",values=c("#f5c842","#42b6f5"),labels=c("Pre-NordLink","Post-NordLink")) + theme_bw() + theme(legend.position="bottom") + guides(color = guide_legend(nrow = 2)) + theme(legend.title=element_blank())


##Frequency distribution of German prices....Figure A.3-8

ggplot(hourdata, aes(x = de_daprice, fill = nl_indicator)) +
  geom_density(aes(y = ..count..), alpha=0.4)+
  xlab("Distribution of day-ahead prices in DE/LU region (EUR/MWh)") +
  ylab("Frequencies") + 
  scale_fill_manual(name="",values=c("#f5c842","#42b6f5"),labels=c("Pre-NordLink","Post-NordLink")) + theme_bw() + theme(legend.position="bottom") + guides(color = guide_legend(nrow = 2)) + theme(legend.title=element_blank())

##Density distribution of NO2 prices....Figure 5.4

ggplot(hourdata, aes(x = no2_daprice, fill = nl_indicator)) +
  geom_density(alpha=0.4)+
  xlab("Distribution of day-ahead prices in NO2 region (EUR/MWh)") +
  ylab("Density") + 
  scale_fill_manual(name="",values=c("#f54242","#42b6f5"),labels=c("Pre-NordLink","Post-NordLink")) + theme_bw() + theme(legend.position="bottom") + guides(color = guide_legend(nrow = 2)) + theme(legend.title=element_blank())

##Frequency distribution of NO2 prices....Figure A.3-8

ggplot(hourdata, aes(x = no2_daprice, fill = nl_indicator)) +
  geom_density(aes(y = ..count..), alpha=0.4)+
  xlab("Distribution of day-ahead prices in NO2 region (EUR/MWh)") +
  ylab("Frequencies") + 
  scale_fill_manual(name="",values=c("#f54242","#42b6f5"),labels=c("Pre-NordLink","Post-NordLink")) + theme_bw()


###Descriptive statistics of prices ........Table A.1-1

install.packages('tseries')
library(tseries)


hourdata1  <-  hourdata %>% dplyr::select(de_daprice, no2_daprice, nl_indicator)

describeBy(
  hourdata1,
  hourdata1$nl_indicator # grouping variable
)


jarque.bera.test(prenlconnect$de_daprice)
jarque.bera.test(prenlconnect$no2_daprice)

jarque.bera.test(postnldata$de_daprice)
jarque.bera.test(postnldata$no2_daprice)


##Difference plots - Check stationarity 

autoplot(postnldata, pricediff) + 
  ggtitle("Price difference - After Nordlink interconnector")

autoplot(prenlconnect, pricediff) + 
  ggtitle("Price difference - Before Nordlink interconnector")


#####Export and Import flows (hourly frequencies)......Figure 6.3

postnldata %>% dplyr::select(imp_ind_nl, exp_ind_nl, hour) %>% 
  pivot_longer(-hour, names_to="variable", values_to="value") %>%
  ggplot(aes(x = fct_relevel(hour, "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24"), y = value, fill=variable)) + stat_summary(fun = "sum", geom = "bar", position="dodge") + theme_bw() +
  scale_fill_manual(values=c("#eba834","#d60202"),labels=c("Exports","Imports")) + xlab("Hours") + ylab("Frequencies of power exchange in the hour") + theme(legend.position="bottom") + theme(legend.title=element_blank())


########## Residual Supply Index (RSI) Analysis

##Cumulative Distribution of RSI indices of Top six firms in NO2 in terms of capacity...Figure 5.6

rsidata <- fread("rsi_data.csv")

rsidata <- rsidata %>%
  mutate(time3 = as.POSIXct(rsidata$time, format="%Y-%m-%d %H:%M:%OS", tz="UTC"))

rsidata %>% dplyr::select(A151,A164,A130,A85,A03, A157, time3) %>% 
  pivot_longer(-time3, names_to="variable", values_to="value") %>%
  ggplot(aes(x=value, color=fct_relevel(variable, "A157", "A03", "A85", "A130", "A164","A151"))) + stat_ecdf(size=1) + theme_bw() + geom_vline(xintercept = 1,linetype=2) + 
  geom_vline(xintercept = 1.1,linetype=3) +
  xlab("RSI") + ylab("Share of hours") +
  scale_y_continuous(labels = scales::percent, breaks=seq(0, 1, 0.05)) +
  scale_color_manual(name="", values=c("#9b2226", "#ca6702", "#e9d8a6", "#94d2bd", "#0a9396","#005f73"),labels=c("Statkraft Energi AS","Agder Energi Vannkraft AS","Lyse Kraft DA","Norsk Hydro ASA","Sunnhordland Kraftlag AS","Skagerak Kraft AS")) +
  scale_x_continuous(breaks = seq(0, 8, 0.5)) + theme(legend.position="bottom")

postnlrsi = rsidata %>% filter(nl_indicator=="1")
prenlrsi = rsidata %>% filter(nl_indicator=="0")


##Cumulative Distribution of RSI indices of Top six firms in NO2 in terms of capacity...Pre-NordLink period....Figure 5.6

prenlrsi %>% dplyr::select(A151,A164,A130,A85,A03, A157, time3) %>% 
  pivot_longer(-time3, names_to="variable", values_to="value") %>%
  ggplot(aes(x=value, color=fct_relevel(variable, "A157", "A03", "A85", "A130", "A164","A151"))) + stat_ecdf(size=1) + theme_bw() + geom_vline(xintercept = 1,linetype=2) + 
  geom_vline(xintercept = 1.1,linetype=3) +
  xlab("RSI") + ylab("Share of hours") +
  scale_y_continuous(labels = scales::percent, breaks=seq(0, 1, 0.05)) +
  scale_color_manual(name="", values=c("#9b2226", "#ca6702", "#e9d8a6", "#94d2bd", "#0a9396","#005f73"),labels=c("Statkraft Energi AS","Agder Energi Vannkraft AS","Lyse Kraft DA","Norsk Hydro ASA","Sunnhordland Kraftlag AS","Skagerak Kraft AS")) +
  scale_x_continuous(breaks = seq(0, 8, 0.5)) + theme(legend.position="bottom")


##Cumulative Distribution of RSI indices of Top six firms in NO2 in terms of capacity...Post-NordLink period....Figure 5.6

postnlrsi %>% dplyr::select(A151,A164,A130,A85,A03, A157, time3) %>% 
  pivot_longer(-time3, names_to="variable", values_to="value") %>%
  ggplot(aes(x=value, color=fct_relevel(variable, "A157", "A03", "A85", "A130", "A164","A151"))) + stat_ecdf(size=1) + theme_bw() + geom_vline(xintercept = 1,linetype=2) + 
  geom_vline(xintercept = 1.1,linetype=3) +
  xlab("RSI") + ylab("Share of hours") +
  scale_y_continuous(labels = scales::percent, breaks=seq(0, 1, 0.05)) +
  scale_color_manual(name="", values=c("#9b2226", "#ca6702", "#e9d8a6", "#94d2bd", "#0a9396","#005f73"),labels=c("Statkraft Energi AS","Agder Energi Vannkraft AS","Lyse Kraft DA","Norsk Hydro ASA","Sunnhordland Kraftlag AS","Skagerak Kraft AS")) +
  scale_x_continuous(breaks = seq(0, 8, 0.5)) + theme(legend.position="bottom")


#####Descriptive analysis of RSI for Statkraft (also referred to as Company 1)

rsidata$nl_indicator = as.character(rsidata$nl_indicator)   
rsidata$month = as.character(rsidata$month)   
rsidata$hour = as.character(rsidata$hour)   

##Monthly frequency of RSI < 1....Figure 5.7

rsidata$tmonth  <- as.Date(cut(rsidata$time3, breaks = "month"))

rsidata_skg_m <- rsidata %>%
  group_by(tmonth, nl_indicator) %>%
  summarise(sum(c1_rsi_ind))%>%
  select(tmonth, nl_indicator, "RSIfreq" = "sum(c1_rsi_ind)")

rsidata_skg_m$month1 <- month(ymd(rsidata_skg_m$tmonth))
rsidata_skg_m <- transform(rsidata_skg_m, monthname = month.abb[month1])

ggplot(aes(x = fct_relevel(monthname, "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), y = RSIfreq, fill=nl_indicator), data = rsidata_skg_m) + stat_summary(fun = "mean", geom = "bar", position="dodge") + theme_bw() +
  scale_fill_manual(name="",values=c("#eba834","#80550d"),labels=c("Pre-NordLink","Post-NordLink")) + xlab("Months") + ylab("Frequencies of RSI<1")


##Hourly avg. frequencies of RSI<1....Figure A.3-7  


ggplot(aes(x = fct_relevel(hour, "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24"), y = c1_rsi_ind, fill=nl_indicator), data = rsidata) + stat_summary(fun = "sum", geom = "bar", position="dodge") + theme_bw() +
  scale_fill_manual(name="",values=c("#eba834","#80550d"),labels=c("Pre-NordLink","Post-NordLink")) + xlab("Hours") + ylab("Frequencies of RSI<1")


##Create data set for RSI price analysis conducted in Section 6.3 of Chapter 6

rsidata$tdate  <- as.Date(cut(rsidata$time3, breaks = "day"))

rsireg <- rsidata %>% 
  group_by(tdate, nl_indicator, year, quarter, week, month, dayofwk) %>%
  summarise_at(.vars = vars(c1_rsi_ind,avg_temp), .funs = c(sum="sum",mean="mean"))


rsireg2 <- setDT(rsidata)[,.(waverage=weighted.mean(no2_daprice,no2_daturnover)), tdate]

library(fuzzyjoin)

rsireg3 <- rsireg %>%
  fuzzy_left_join(rsireg2,
                  by = c("tdate" = "tdate"),
                  match_fun = list(`==`)
  ) 



rsireg3 <- rsireg3 %>%
  rename(tdate = tdate.x,
         rsisum = c1_rsi_ind_sum,
         avg_temp = avg_temp_mean,
         wavg= waverage)

rsireg3 <- rsireg3 %>%
  mutate(rsi_pc = rsisum/24)

rsireg3 <- rsireg3[, c("tdate", "nl_indicator" ,"rsisum" ,"avg_temp", "rsi_pc","year","quarter","week","month","dayofwk","wavg")] 

save(rsireg3, file = "rsi_analysisdata.dta")


##The following excel output was further modified to include Oil, gas, Coal and EUA prices and NSL indicator to create the file -  'rsi_analysis.csv' used for regression analysis

library("writexl")
write_xlsx(rsireg3,"rsi_analysis_may23.xlsx")


##Correlation between wht avg. No2 price and RSI percentage .....Figure 6.7

load(file = "rsi_analysisdata.Rdata")

postnlrsianalysis = rsireg3 %>% filter(nl_indicator=="1")
prenlrsianalysis= rsireg3 %>% filter(nl_indicator=="0")


##No relationship found, could be due to influence of other factors on prices too

ggplot(prenlrsianalysis, 
       aes(x=rsi_pc, 
           y=wavg))+
  geom_point()+
  geom_smooth(method='lm', formula= y~x, se=FALSE, col="red",lty=2)+
  theme_bw() + xlab("PSI Ratio") + ylab("Daily weighted avg. NO2 dayahead prices (EUR/MWh)") 


ggplot(postnlrsianalysis, 
       aes(x=rsi_pc, 
           y=wavg))+
  geom_point()+
  geom_smooth(method='lm', formula= y~x, se=FALSE, col="red",lty=2)+
  theme_bw() + xlab("PSI Ratio") + ylab("Daily weighted avg. NO2 dayahead prices (EUR/MWh)") 



