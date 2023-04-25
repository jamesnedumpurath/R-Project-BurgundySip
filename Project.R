burgundy<-read.csv("D:\\TSoM- Data Handling and Decision Making\\Project\\BurgundySip (1).csv"
                   ,na.strings =c("","NA"));
burgundy;
summary(burgundy);                   
str(burgundy);

burgundy<- burgundy[order(burgundy$SN),];
burgundy;
anyDuplicated(burgundy$SN);

duplicated(burgundy$SN);


# burgundycopy<- burgundy[duplicated(burgundy$SN),];
# burgundycopy;



dups <-
  duplicated(burgundy$SN) |
  duplicated(burgundy$SN, fromLast=TRUE);
dups;
burgundy[dups,];

## values with serial number NV and NA


burgundyclean<-burgundy[!duplicated(burgundy$SN,fromLast = TRUE),];##bottom to top becuase first value have NA,
#starting observations have NA values
burgundyclean;  #clean data set
summary(burgundyclean);
str(burgundyclean);

#Factorizing variables
burgundyclean[,c(2,3,5,6)]<-  lapply(burgundyclean[,c(2,3,5,6)],factor)
burgundyclean;
summary(burgundyclean);


burgundyclean$YR<-as.integer(burgundyclean$YR)

colnames(burgundyclean)<-c("SerialNo","WineryName","WineName","HarvestYear","Region","Variety",
                     "Rating","numberOfTesters","Price","BodyScore","AcidityScore",
                     "ResidualSugar","AlcoholPercent","Density");



#convering RSG, AL,DN as numeric

burgundyclean[,c(12,13,14)]<-  lapply(burgundyclean[,c(12,13,14)],as.numeric)
burgundyclean;
summary(burgundyclean);
str(burgundyclean);

##variable analysis

# $ SerialNo       : chr  "N.V." "W1001-62187" "W1003-83159" "W1007-31751" ...[Not relevent]
# $ WineryName     : Factor w/ 480 levels "A Coroa","Aalto",..: 160 319 385 287 386 233 270 435 383 319 ...[IND]
# $ WineName       : Factor w/ 844 levels "1194","17","1730 Amontillado",..: 618 261 496 53 772 487 810 615 822 261 ...[IND]
# $ HarvestYear    : int  2016 2019 2011 2018 2020 NA 2016 2017 1981 2019 ...[IND]
# $ Region         : Factor w/ 76 levels "Abona","Alella",..: 60 70 60 9 57 33 60 16 60 70 ...[IND]
# $ Variety        : Factor w/ 21 levels "Albarino","Cabernet Sauvignon",..: 13 20 13 6 NA 16 13 19 13 20 ...[IND]
# $ Rating         : num  4.02 4.1 4.1 4.01 3.96 4.31 3.99 4.16 4.45 4.1 ...[Dependent]
# $ numberOfTesters: int  392 400 402 415 407 1452 3825 54 66 400 ...[IND]
# $ Price          : num  20 27.9 28.5 17.9 11.9 ...[Dependent]
# $ BodyScore      : int  4 5 4 3 NA 4 4 4 4 5 ...[Dependent]
# $ AcidityScore   : int  3 3 3 3 NA 3 3 2 3 3 ...[Dependent]
# $ ResidualSugar  : num  10.21 9.68 9.39 10.64 11.77 ...[Dependent]
# $ AlcoholPercent : num  10.8 11.7 11.3 11.5 10.5 ...[Dependent]
# $ Density        : num  0.997 0.996 0.995 0.995 0.996 ...[Dependent]

install.packages("dplyr");
library(dplyr);



# PerformanceAnalytics
install.packages("PerformanceAnalytics");
?PerformanceAnalytics

library(PerformanceAnalytics);
?chart.Correlation;

burgundyclean %>%
  select_if(is.numeric) %>%
  chart.Correlation();


#Correlation Charts
install.packages("ggcorrplot");
library(ggplot2);
library(ggcorrplot);

burgundyclean %>% 
  na.omit() %>% 
  select_if(is.numeric) %>%
  cor() %>%
  round(3) %>% 
  ggcorrplot( hc.order =TRUE,type ="lower", lab =TRUE);


#Outlier treatment'



burgundyclean %>% 
  select_if(is.numeric) %>%
  boxplot();


boxplot(burgundyclean$numberOfTesters,);
boxplot(burgundyclean[,burgundyclean$Variety]$numberOfTesters);
boxplot.stats(burgundyclean$numberOfTesters);
# boxplot(irisOUT[irisOUT$Species == "setosa",]$Sepal.Width)
plot(burgundyclean$Variety,burgundyclean$numberOfTesters);
plot(burgundyclean$Variety,burgundyclean$Price);
burgundy_year_type_price<-aggregate(Price~HarvestYear+Variety,burgundyclean,mean);
boxplot(burgundyclean[burgundyclean$Variety,]$BodyScore);
boxplot(burgundyclean[burgundyclean$Variety,]$Price);
