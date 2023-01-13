#imports
install.packages('dplyr');
install.packages('ggplot2');
install.packages('tidyverse');
install.packages('xts');
install.packages('zoo');
library('dplyr');
library('ggplot2');
library('tidyverse');
install.packages("PerformanceAnalytics");
library(PerformanceAnalytics);


# STEP 1 : loading the data, use na.strings function set to 
# detect blanks and character strings NA and N.V. as missing values. Treating all N.V. as NAs
wine <- read.csv("E:\\TSOM\\MODULE 2\\final Project\\BurgundySip.csv",na.strings = c("","NA","N.V."));



head(wine);
dim(wine); # 7500 rows and 14 cols when loaded.
nrow(wine);
str(wine);
summary(wine);


# STEP 2: Analyze variable and treat them (sanity checks/ format adjustment) and transforming data
# replacing spaces with blanks. And converting into numeric  

wine$RSG <- as.numeric(gsub(" ","",wine$RSG));
wine$AL <- as.numeric(gsub(" ","",wine$AL));
wine$DN <- as.numeric(gsub(" ","",wine$DN));

# getting length of unique values
length(unique(wine$TP)); # 22
length(unique(wine$NAME)); # 481
length(unique(wine$WINE)); # 848
length(unique(wine$REG)); # 77

# function to extract top 10 frequency of variables and converting rest to "others"
replace <- function(dataFrame, var)
{
  wineCount <- dplyr::count(dataFrame, var = dataFrame[,var],sort = TRUE);
  top10 <- head(wineCount,10);
  countLess <- wineCount[!(wineCount[,1] %in% top10[,1]),1]
  dataFrame[dataFrame[,var] %in% countLess, var] <- "other";
  return(dataFrame);
}  

wine <- replace(wine, "REG");
wine <- replace(wine, "NAME");
wine <- replace(wine, "WINE");
wine <- replace(wine, "TP");

# Transforming variables (NAME,WINE,YR,TP,REG,BD,ACD) to factor
wine <- transform(wine,
                  NAME = factor(NAME),
                  WINE = factor(WINE),
                  
                  TP = factor(TP),
                  REG = factor(REG)
                  );


str(wine);





# STEP 3: Treating duplicates

#  SN primary key because each sample can have only one observation. 
#  which needs to be handled.

anyDuplicated(wine$SN); # returns the index of first duplicate entry

sum(duplicated(wine[,"SN"])); #It has 3834 duplicate values

nrow(wine[duplicated(wine[,"SN"]),]) ; 

# probability of missingness
(sum(duplicated(wine[1]))/nrow(wine)) * 100; # 51.12 % of data is will be taken out

# this will give all duplicate entries
dup2 <-duplicated(wine[,"SN"]) |
  duplicated(wine[,"SN"],fromLast=TRUE);
wineDup <- wine[dup2,];
wineDup;



# finally cleaning the dataset
wine <- wine[!duplicated(wine[,"SN"],fromLast=TRUE),]; # bottom to top 
nrow(wine) # after cleaning 3666

# conforming if dataset is cleaned
anyDuplicated(wine[,"SN"]);

# resetting the row numbers
rownames(wine) <- NULL;


####### From here, the data set has no duplicates in SN.

# STEP 4: Treating outliers

# analyzing correlation matrix
wine %>% select_if(is.numeric) %>% chart.Correlation();

summary(wine);

# 4-1 VARIABLE NUMR - number of testers that reviewed the wine / IND

summary(wine$NUMR);
# (+) max(NUMR) = 32624, this value is potential outlier

hist(wine$NUMR,
     xlab = "NUMR",
     main = "Number of testers",
     breaks = sqrt(nrow(wine)));

hist(wine[wine$NUMR > 3000,"NUMR"],
     xlab = "NUMR",
     main = "Number of testers",
     breaks = sqrt(nrow(wine)));
# (+) histogram has isolated bars, there are potential outliers

boxplot(wine$NUMR, main = "Number of testers");
boxplot.stats(wine$NUMR);
# (+) variable NUMR consists potential outliers

length(boxplot.stats(wine$NUMR)$out)/length(na.omit(wine$NUMR))*100;
min(boxplot.stats(wine$NUMR)$out);
max(boxplot.stats(wine$NUMR)$out);
# (-) 8,5% values are potential outliers, they lie in the interval [865,32624]

# NUMR has outliers 
# treating outliers (ceiling and floor)
# q3+1.5*IQR value is used for the upper extreme outliers

outlier <- boxplot.stats(wine$NUMR)$out;
outlier;
outind <- which(wine$NUMR %in% outlier);
outind;

wine$NUMR[outind] <- summary(wine$NUMR)[5] + 1.5*(summary(wine$NUMR)[5]-summary(wine$NUMR)[2]);
outind;
wine$NUMR[outind];

boxplot(wine$NUMR);
# we can proceed further without outliers of variable NUMR

# 4-2 VARIABLE PR - price in euros [€] / DEP
# COR between PR and RT = 0.71 - strong positive correlation
# COR between PR and RSG = -0.55 - moderate negative correlation
# COR between PR and AL = 0.4 - week positive correlation
# COR between PR and DN = -0.3 - week negative correlation

summary(wine$PR);
# (+) max(PR) = 3119.08, this value is potential outlier

hist(wine$PR,
     xlab = "PR",
     main = "Price of wine, €",
     breaks = sqrt(nrow(wine)));

hist(wine[wine$PR > 100,"PR"],
     xlab = "PR",
     main = "Price of wine, €",
     breaks = sqrt(nrow(wine)));
# (+) histogram has isolated bars, variable PR consists potential outliers

boxplot(wine$PR, main = "Price of wine, €");
boxplot.stats(wine$PR);
# (+) variable PR consists potential outliers

length(boxplot.stats(wine$PR)$out)/length(na.omit(wine$PR))*100;
min(boxplot.stats(wine$PR)$out);
max(boxplot.stats(wine$PR)$out);
# (-) 10,5% values are potential outliers, they lie in the interval [140.84,3119.08]

plot(wine$RT, wine$PR, , main = "Price vs average rating");
# (-) potential outliers don't violate relationship

plot(wine$RSG, wine$PR, main = "Price vs residual sugar level");
# (-) potential outliers don't violate relationship

plot(wine$TP, wine$PR, main = "Price by wine variety");
plot(wine$YR, wine$PR, main = "Price by year");
plot(wine$REG, wine$PR, main = "Price by region");
# (-) potential outliers are typical for many categories of TP, REG ans REG

# because potential price outliers aren’t due to human or machine errors, 
# but are true extreme values, we can proceed further without removing outliers of variable PR


# 4-3 VARIABLE RT - average rating given to the wine by the test users [from 1-5] / DEP
# COR between RT and RSG = -0.92 - strong negative correlation
# COR between RT and AL = 0.55 - moderate positive correlation
# COR between RT and DN = -0.45 - week negative correlation

summary(wine$RT);
# (-) all value lie in the interval [3.92,4.99]

hist(wine$RT,
     xlab = "RT",
     main = "Average rating given by the test users",
     breaks = sqrt(nrow(wine)));
# (-) histogram hasn't isolated bars

boxplot(wine$RT, main = "Average rating given by the test users");
boxplot.stats(wine$RT);
# (+) variable RT consists potential outliers

length(boxplot.stats(wine$RT)$out)/length(na.omit(wine$RT))*100;
min(boxplot.stats(wine$RT)$out);
max(boxplot.stats(wine$RT)$out);
# (-) 3% values are potential outliers, but all these points lie in the interval [4.7,4.99]

plot(wine$RSG, wine$RT, main = "Average rating vs residual sugar level");
# (-) potential outliers don't violate relationship

plot(wine$AL, wine$RT, main = "Average rating vs alcohol percentage");
# (-) potential outliers don't violate relationship

# because potential rating outliers lie in the interval [1-5]
# and don't violate relationship with other variables
# we can proceed further without removing outliers of variable RT


# 4-4 VARIABLE BD - Body score, defined as the richness and weight of 
# the wine in your mouth [from 1-5] / DEP
# COR between BD and RSG = -0.31 - week negative correlation

summary(wine$BD);
# all value lie in the interval [2,5]

hist(wine$BD,
     xlab = "BD",
     main = "Body score of wine",
     breaks = sqrt(nrow(wine)));
# (-) histogram hasn't isolated bars

boxplot(wine$BD, main = "Body score of wine");
boxplot.stats(wine$BD);
# (+) variable BD consists potential outliers

length(boxplot.stats(wine$BD)$out)/length(na.omit(wine$BD))*100;
min(boxplot.stats(wine$BD)$out);
max(boxplot.stats(wine$BD)$out);
# (-) 1% values are potential outliers, but all this values equals to 2

# because potential body score outliers lie in the interval [1-5]
# we can proceed further without removing outliers of variable BD


# 4-5 VARIABLE ACD - c, defined as wine's “pucker” or tartness; 
# it's what makes a wine refreshing [from 1-5] / DEP

summary(wine$ACD);
# (-) all value lie in the interval [1,3]

hist(wine$ACD,
     xlab = "ACD",
     main = "Acidity score",
     breaks = sqrt(nrow(wine)));
# (-) histogram hasn't isolated bars

boxplot(wine$ACD, main = "Acidity score");
boxplot.stats(wine$ACD);
# (+) variable ACD consists potential outliers

length(boxplot.stats(wine$ACD)$out)/length(na.omit(wine$ACD))*100;
min(boxplot.stats(wine$ACD)$out);
max(boxplot.stats(wine$ACD)$out);
# (-) 3,4% values are potential outliers, but all this values equals to 1 or 2

# because potential acidity score outliers lie in the interval [1-5]
# we can proceed further without removing outliers of variable ACD


# 4-6 VARIABLE RSG - residual sugar level of the wine [0-16] / DEP

summary(wine$RSG);
# (-) all value lie in the interval [3.35,15.9]

hist(wine$RSG,
     xlab = "RSG",
     main = "Residual sugar level of wine");
# histogram looks somewhat bell-shaped, indicating normality. 
# (-) histogram hasn't isolated bar

boxplot(wine$RSG, main = "Residual sugar level of wine");
boxplot.stats(wine$RSG);
# (+) variable RSG consists potential outliers

length(boxplot.stats(wine$RSG)$out)/length(na.omit(wine$RSG))*100;
min(boxplot.stats(wine$RSG)$out);
max(boxplot.stats(wine$RSG)$out);
# (-) 1,4% values are potential outliers, but all these points lie 
# in the intervals [3.35,5.06] or [12.45,15.9] 

# because potential sugar outliers lie in the interval [0-16]
# we can proceed further without removing outliers of variable RSG


# 4-7 VARIABLE AL - Alcohol percentage of the wine / DEP
# COR between AL and RSG  = -0.5 - moderate negative correlation

summary(wine$AL);
# (-) all value lie in the interval [9.52,14.02]

hist(wine$AL,
     xlab = "AL",
     main = "Alcohol percentage of wine");
# histogram looks somewhat bell-shaped, indicating normality.
# (-) histogram hasn't isolated bar

boxplot(wine$AL, main = "Alcohol percentage of wine");
boxplot.stats(wine$AL);
# (+) variable AL consists potential outliers

length(boxplot.stats(wine$AL)$out)/length(na.omit(wine$AL))*100;
min(boxplot.stats(wine$AL)$out);
max(boxplot.stats(wine$AL)$out);
# (-) 1,6% values are potential outliers, but all these points lie 
# in the intervals [9.52,9.85] or [12.91,14.02] 

plot(wine$RSG, wine$AL);
# potential outliers don't violate relationship

# because potential alcohol percentage outliers aren’t due to human or machine errors,
# but are true values, we can proceed further without removing outliers of variable AL


# 4-8 VARIABLE DN - The typical density or specific gravity of the wine is generally between 1.080 and 1.090 / DEP
# COR between DN and AL  = -0.8 - strong negative correlation
# COR between DN and RSG  = 0.4 - weak positive correlation

# the original gravity, the gravity just before adding the yeast, of a typical wine will be 1.075 to 1.090. 
# after a few days the gravity will have typically dropped to 1.040. 
# the final gravity of a wine will be in the region of 1.000 to 0.990.

summary(wine$DN);
# (-) all values lie in the interval [0.993,0.998]

hist(wine$DN,
     xlab = "DN",
     main = "Typical density or specific gravity of the wine");
# histogram looks somewhat bell-shaped, indicating normality.
# (-) histogram hasn't isolated bar

boxplot(wine$DN);
boxplot.stats(wine$DN);
# (+) variable AL consists potential outliers

length(boxplot.stats(wine$DN)$out)/length(na.omit(wine$DN))*100;
min(boxplot.stats(wine$DN)$out);
max(boxplot.stats(wine$DN)$out);
# (-) 0,24% values are potential outliers, but all these points lie 
# in the intervals [0.9929,0.9936]

plot(wine$AL, wine$DN);
# (-) potential outliers don't violate relationship

# because potential density (gravity) outliers lie in the interval [0.99-1]
# we can proceed further without removing outliers of variable DN

####### OUTLIERS HANDLED! From here, we build a df to show the qty of NAs per variable

#STEP 5: Handling missing values

# Function for count and percentage of NAs
count_na<-function(data){
  require(tidyverse);
  require(cowplot);
  
  df.na.count <- map(data,~sum(is.na(.))) %>% 
    simplify() %>% 
    tibble(col = names(.),
           NAs = .) %>% 
    mutate(percentage = round(NAs/nrow(data)*100));
  
  print(df.na.count %>% as.data.frame());
  
  p1<-ggplot(df.na.count,aes(x = col , y = NAs))+
    geom_col()+
    theme(axis.text.x = element_text(angle = 90))
  
  p2<- ggplot(df.na.count,aes(x = col, y = percentage))+
    geom_col()+
    theme(axis.text.x = element_text(angle = 90,))+
    scale_y_continuous(limits = c(0,100))
  
  #plot_grid(p1,p2,nrow = 2)
}
count_na(wine);
#   Variables    qty
# 1         SN   1
# 2       NAME   1
# 3       WINE   0
# 4         YR 144
# 5        REG   0
# 6         TP 154
# 7         RT   0
# 8       NUMR   0
# 9         PR  58
# 10        BD 443
# 11       ACD 443
# 12       RSG  18
# 13        AL  36
# 14        DN  29
# 15  TPFactor 154
# 16  YRFactor 144
# 17 REGFactor   0

#check the cor table of all numeric variables
wineCorrelation <- cbind(wine$YR, wine$RT, wine$NUMR, wine$PR, wine$BD, wine$ACD, wine$RSG, wine$AL, wine$DN);
colnames(wineCorrelation) <- c('year', 'rating', 'number', 'price', 'body', 'acidity', 'rsg', 'alcohol', 'dn');
head(wineCorrelation);
cor(wineCorrelation,  use = "complete.obs");

# STEP 5.1 handle NAs in SN
#identify the row with NA in SN and drop it.
#The observation with NA in NAME is the same with NA in SN, to both cases are solved here
wine[is.na(wine$SN),];
wine <- wine[-which(is.na(wine$SN)),];
wine[is.na(wine$SN),];

#STEP 5.2
#check NAs in year
wine[is.na(wine$YR),];

#function to fill the NA in years based on the wine name
fillYearNA <- function(df){ ##use the random only for the years that already exists in df
  
  SNfromYearNA <- wine[is.na(wine$YR),]$SN #get all SNs from the NAs in year
  allWinesNameYr <- aggregate(YR ~ NAME, wine, FUN=max) #sumamrize the maximum years by NAME
  #merge the summarizations between max/min of year by NAME
  ### allWinesNameYr <- merge(aggregate(YR ~NAME, wine, FUN =min), allWinesNameYr, by='NAME') 
  ### colnames(allWinesNameYr) <- c('NAME', 'MINYEAR', 'MAXYEAR')
  
  #as there is no correlation at all between year and any other numeric variable
  #so we will fill the year NAs by assigning a random number between the min and the max year for the NAME
  
  for(i in 1:length(SNfromYearNA)){
    curWineName <- allWinesNameYr[allWinesNameYr$NAME==wine[wine$SN==SNfromYearNA[i],]$NAME,]
    #print(wine[wine$SN==SNfromYearNA[i],])
    if(nrow(curWineName)==0){
      #if there is no year register for NAME, we will get a random value between the min and max from all wines
      wine[wine$SN==SNfromYearNA[i], ]$YR <<- round(runif(1, min(na.omit(wine$YR)), max(na.omit(wine$YR))))
      
    } else{
      #if there is at least one other observation of the same NAME with any year, we will generate a random
        #number between the min and the max year from this NAME
      allYearsFromName <- unique(wine[wine$NAME=="Contino",]$YR)
      wine[wine$SN==SNfromYearNA[i], ]$YR <<-  sample(allYearsFromName,1) ### round(runif(1, curWineName$MINYEAR, curWineName$MAXYEAR))
    }
  }
}

fillYearNA(wine)
sum(is.na(wine$YR)) #no NAs
#check NAs again:
count_na(wine)

# Variables qty
# 1         SN   0
# 2       NAME   0
# 3       WINE   0
# 4         YR   0
# 5        REG   0
# 6         TP 153
# 7         RT   0
# 8       NUMR   0
# 9         PR  58
# 10        BD 442
# 11       ACD 442
# 12       RSG  18
# 13        AL  36
# 14        DN  29
# 15  TPFactor 153
# 16  YRFactor 144
# 17 REGFactor   0

#function to find the mode
getmode <- function(variable) {
  uniqv <- unique(variable) #get uniques
  return (uniqv[which.max(tabulate(match(variable, uniqv)))]) #get modes
}

#STEP 5.3 Handle NAs in TP (153 NAs)
fillTypeNA <- function(df){
  SNfromTypeNA <- wine[is.na(wine$TP),]$SN #get all SNs from the NAs in year
  allWinesRegTp <- aggregate(TP ~ REG, wine, FUN=getmode) #get mode from type aggregated by REG
  #when aggregated by region, only 24 regions do not have a correspondent type mode. If we aggregate by NAME, it would be 101. 
  
  for(i in 1:length(SNfromTypeNA)){
    curWineReg <- allWinesRegTp[allWinesRegTp$REG==wine[wine$SN==SNfromTypeNA[i],]$REG,]
    
    #print(wine[wine$SN==SNfromTypeNA[i],])
    if(nrow(curWineReg)==0){
      #if there is no year register for REG, then we will get the mode from all wines
      wine[wine$SN==SNfromTypeNA[i], ]$TP <<- getmode(wine$TP)
      
    } else{
      #if there is at least one other observation of the same REG with any TP, we will take the mode from region 
      wine[wine$SN==SNfromTypeNA[i], ]$TP <<- curWineReg$TP
    }
  }
  
  
}

fillTypeNA(wine);
sum(is.na(wine$TP)); 

# STEP 5.4 handling NAs in DN :

sum(is.na(wine$DN));

# plot this variable :
hist(wine$DN);

# as we can see , this variable values are in normal distribution, so mean is good representation of the missing values :
wine$DN[is.na(wine$DN)]<- mean(wine$DN,na.rm = T);

# final check :
sum(is.na(wine$DN));

#STEP 5.5
# removing NAs from RSG : 

sum(is.na(wine$RSG));
hist(wine$RSG);
sd(wine$RSG,na.rm = T);

# RSG has normal distribution,with small standard deviation , mean is good representation for the missing values :

wine$RSG[is.na(wine$RSG)]<- mean(wine$RSG,na.rm = T);

sum(is.na(wine$RSG));


# STEP 5.6 handling  NAs in AL :

sum(is.na(wine$AL));
wine_no_na<- na.omit(wine);
cor(wine_no_na$AL,wine_no_na$RSG);

# AL and RSG has moderate corelation , we will use simple linear regression to treat NAs : 

# create train dataset :
al_train<-wine[!is.na(wine$AL),];

# create test set : 
test_set2<-wine[is.na(wine$AL),];

# generate the model :

al_model <- lm(AL~RSG,al_train);
summary(al_model);
al_model$fitted.values;

# make the prediction for NA values :

al_predict<- predict(al_model,test_set2);
al_predict;
# replace missing values :

wine$AL[is.na(wine$AL)]<-al_predict;

# final check :
sum(is.na(wine$AL));


# STEP 5.7 handling NAs in ACD : 
sum(is.na(wine$ACD));

sum(is.na(wine$ACD));
hist(wine$ACD);

count_values<-wine %>% 
  na.omit() %>% 
  group_by(ACD) %>% 
  summarise(count = n()) %>% 
  mutate(percent = count/sum(count));
#      ACD count percent
#   <int> <int>   <dbl>
# 1     1    26 0.00853
# 2     2    71 0.0233 
# 3     3  2951 0.968  

# we will treat missing values using sampling from original dataset by applying probabilities as shown above : 

# we will  randomly sample 438 value to replace the NAs :

na_fill_values<- sample(sample(c(3,2,1),size = 438,replace = T,prob = c(0.968,0.0233,0.00853)));

# replacing NAs : 

wine$ACD[is.na(wine$ACD)]<- na_fill_values;

# final check :
sum(is.na(wine$ACD));


# treating NAs in BD :

sum(is.na(wine$BD));

# same number of NAs with ACD , maybe there is corelation :

cor(wine4$BD,wine4$ACD);

# no corelation :
# check the distribution :
hist(wine$BD);

count_values_bd<-wine %>% 
  na.omit() %>% 
  group_by(BD) %>% 
  summarise(count = n()) %>% 
  mutate(percent = count/sum(count));

#       BD count percent
# <int> <int>   <dbl>
# 1     2    33  0.0108
# 2     3   379  0.124 
# 3     4  1763  0.578 
# 4     5   873  0.286 

# we will treat missing values using sampling from original dataset by applying probabilities as shown above : 

# we will  randomly sample 438 value to replace the NAs :

na_fill_values_bd<- sample(sample(c(4,5,3,2),size = 438,replace = T,prob = c(0.578,0.286,0.124,0.0108)));

# replacing NAs : 

wine$BD[is.na(wine$BD)]<- na_fill_values_bd;

# final check :
sum(is.na(wine$BD));

# STEP 5.8 NA Handling for price :
# we will use regression to solve it :

sum(is.na(wine$PR));
# create training set :
train_set1<- wine[!is.na(wine$PR),];
#create test set :
test_set1<- wine[is.na(wine$PR),];

# run the model : 
pr_model <- lm(PR~log(YR), data =train_set1);
# explore the model :
summary(pr_model);
pr_model$fitted;


# make the predictions for the missing prices :

pr_predict<- predict(pr_model,test_set1);
pr_predict;

# filling the NAs in price with predicted values :
wine$PR[is.na(wine$PR)]<- pr_predict;

# final check :
sum(is.na(wine$PR));

# All  NAs handled
count_na(wine);

# STEP 6 - TRANSFORMATION
# year is factored and stored in new variable.
wine$YRFactor <- cut(wine$YR, breaks = c(1900,1910,1920,1930,1940,1950,1960,1970,1980,1990,2000,2010,2020,2030),
               right = T,
               labels = c("1900s","1910s","1920s","1930s","1940s","1950s","1960s","1970s", "1980s",
                          "1990s", "2000s", "2010s", "2020s"));



#### STEP 7 - DESCRIPTIVE ANALYSIS

# Question 7.1- what is the top 5 wineries with the best rating average :
avg_rating<-wine %>% 
  #grouping by winery
  group_by(NAME) %>% 
  #calculating the mean for each group 
  summarise(avg_rating = mean(RT)) %>% 
  ungroup() %>%
  # sorting data by average rating DESC
  arrange(desc(avg_rating)) %>% 
  # factorizing NAME to sort it in the plot 
  mutate(NAME = fct_inorder(NAME)) %>% 
  #picking the top 5
  head(5) ;
p1<-avg_rating%>% 
  #plot
  ggplot()+
  geom_bar(aes(x = NAME, 
               y = avg_rating,
               fill = NAME),
           stat = "identity")+
  scale_fill_viridis_d(option = "magma",
                       direction = -1)+
  xlab("Name of the winery")+
  ylab("Average rating")+
  # add labels to the plot 
  geom_label(aes(x = NAME,
                 y = avg_rating,
                 label = round(avg_rating,2),
                 hjust = .5),
             size = 2.5)+
  scale_x_discrete(label = NULL)+
  ggtitle("Top 5 average rating Wineries")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45),
        panel.grid = element_line(linewidth = 0));
p1;

# question 7.2 - what is the top 5 most expensive wines  :

expensive_wine<-wine %>% 
  #group by NAME and WINE
  group_by(NAME,WINE) %>% 
  #Find the max price
  summarise(max_price = max(PR)) %>% 
  ungroup() %>%
  #order by max price desc
  arrange(desc(max_price)) %>% 
  #factorizing WINE to sort it in plot 
  mutate(WINE = fct_inorder(NAME)) %>% 
  #pick the top 5
  head(5);
 # plot the result :
 p2<-expensive_wine%>% 
  ggplot()+
  geom_bar(aes(x = NAME,
               y = max_price,
               fill = NAME),
           stat = "identity")+
  scale_fill_viridis_d(option = "magma",
                       direction = -1)+
  xlab("Wine")+
  ylab("Price")+
  #add labels to the plot :
  geom_label(aes(x = NAME,
                 y = max_price,
                 label = round(max_price,0),
                 hjust = .5),
             size = 2.5)+
  scale_x_discrete(label = NULL)+
  ggtitle("Top 5 most expensive wine")+
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid = element_line(linewidth = 0));
p2;

# Question 7.3- what is the top 5 most expensive wine variety   :

exp_wine_var<-wine %>% 
  #grouping by WINE and TP :
  group_by(WINE,TP) %>% 
  # calculating the max price per wine variety:
  summarise(max_price = max(PR)) %>% 
  ungroup() %>%
  #sorting DESC by max price :
  arrange(desc(max_price)) %>% 
  # factorizing TP to sort it in plot
  mutate(TP = fct_inorder(TP)) %>% 
  #picking the top 10
  head(10);
# plot the result :
p3<-exp_wine_var%>% 
  ggplot()+
  geom_bar(aes(x = TP,
               y = max_price,
               fill = TP),
           stat = "identity")+
  scale_fill_viridis_d(option = "magma",
                       direction = -1)+
  xlab("Wine Variety")+
  ylab("Price")+
  scale_x_discrete(label = NULL)+
  ggtitle("Top 6 most expensive wine",subtitle = "By wine Variety")+
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid = element_line(linewidth = 0),
        plot.subtitle = element_text(hjust = 0.5));
p3;

# Question 7.4 Determine correlation for Price with all numeric variables .

# create a function to run a series of functions on numeric variables :

analyse_cor<-function(data){
  require(tidyverse);
  require(PerformanceAnalytics);
  
  wine_numeric<-lapply(X = data,FUN = is.numeric) %>% simplify();
  
  wine_numeric<-data[,wine_numeric];
  
  wine_numeric %>% chart.Correlation();
  print(wine_numeric %>% na.omit() %>% cor());
  wine_numeric %>% na.omit %>% cor() %>% heatmap();
}

# run the function :
analyse_cor(wine);


# Question 7.5 : Analyse the effect of sugar and rating on price of wine variety over decades.
ggplot(data = wine,aes(x = RSG, y = RT,colour = TP,size = PR)) + geom_point() + facet_grid(.~YRFactor)+ 
      ggtitle("Affect of Sugar and Rating on Price");


# Question 7.6 : Analyse correlation of  alcohol Vs RSG on price for each wine variety 
ggplot(data = wine, aes( x = RSG, y = AL, size = PR,colour = TP)) + geom_point(alpha = 0.5) + 
  ggtitle("Correlation of Sugar and Alcohol")


# Question 7.7 : find  the confidence interval 95%  for residual sugar level of the wine 

# mean and standard deviation of RSG
meanRSG <- mean(wine$RSG,na.rm = T);
sdRSG <- sd(wine$RSG,na.rm = T);
n <- nrow(wine);
meanRSG;
sdRSG;

# |Z-score|
RSGZscore <- round(abs(wine$RSG - meanRSG)/sdRSG,2);

# |Z-score| that captures 95% of the values
RSGZscore95Per <- quantile(RSGZscore,0.95,na.rm = T);
RSGZscore95Per;

# confidence interval
c <- RSGZscore95Per * sdRSG/sqrt(n);
c;
a_confidence <- meanRSG-c;
b_confidence <- meanRSG+c;
print(paste("Residual sugar level with 95% confidence interval [",round(a_confidence,2),",",round(b_confidence,2), "]"));


hist(wine$RSG,
     xlab = "RSG",
     main = "Residual sugar level of wine");


abline(v=meanRSG, col="red");
abline(v=a_confidence, col="blue");
abline(v=b_confidence, col="blue");


# STEP 8 PREDICTIVE ANALYSIS

# 1- prediction of Alcohol level based on sugar level :

# plot our data :
wine %>% 
  ggplot()+
  geom_line(aes(x = RSG,
                y = AL))


# create training set :

df_train <- na.omit(wine)

# 1 - trying simple linear regression :

# create the model 
sl_model<- lm(AL~RSG,data = df_train)

# checking the model 

summary(sl_model)

# plot the fitted values with roiginal values :
ggplot(na.omit(wine))+
  geom_line(aes(x = RSG,
                y = AL,color = "purple"))+
  geom_line(aes(x = RSG,
                y = sl_model$fitted.values,color = "forestgreen"))
# 2 - trying quadratic regression :

q_model<-lm(AL~RSG+RSG^2,data =df_train)

# checking the model 
summary(q_model)
# plot the fitted values with roiginal values :
ggplot(na.omit(wine))+
  geom_line(aes(x = RSG,
                y = AL,color = "purple"))+
  geom_line(aes(x = RSG,
                y = q_model$fitted.values,color = "forestgreen"))

# as we can see both models gives the same accuracy we will predict AL based on low RSG values :

newdata<- data.frame(RSG = runif(n = 100,min = 1,max = 3),AL = rep(NA,100))

pred_sl<- predict(sl_model,newdata = newdata)

# replace the NAs in new data with predicted values :
newdata[,2]<- pred_sl;
newdata;
# create forecast dataset:
wine2<-df_train[,c(12,13)]
forecast<- rbind(wine2,newdata)
forecast<- forecast %>% 
  arrange(RSG) %>% 
  mutate(id = row_number())

#plot our predictions 

ggplot()+
  geom_line(data = wine,aes(x = RSG,
                            y = AL,color = "forestgreen"))+
  geom_line(data = df_train,aes(x = RSG,
                                y = sl_model$fitted.values))+
  geom_line(data = newdata,aes(x = RSG,
                               y = AL, color = "red"))+
  # plot a line to show the point where forcast started :
  geom_vline(xintercept = 3 ,color = "blue");

# 2- prediction of price using multivariate non-linear regression :
# first plot our data :


ggplot(wine)+
  geom_line(aes(x = YR,
                y = PR));

# create training set :

df2_train<-na.omit(wine);

ml_model <- lm(PR~log(YR)+ACD+ACD^2+AL+AL^2+log(BD), data =df2_train);
summary(ml_model);
ml_model$fitted;
# as we can see from the comparison between the two models ,second one has better Residual standard error, so we will run our prediction using the Second one :

# create new data :

newdata_pr<- data.frame(PR = rep(NA,100),
                        YR = seq(1811,1910,1),
                        ACD = runif(100,1,5),
                        AL = sample(wine$AL,size = 100,replace = F),
                        BD = runif(100,1,5));

# predict the price :
pred_ml<- predict(ml_model,newdata_pr);
pred_ml;
newdata_pr[,1]<-pred_ml;
newdata_pr;

# Plot the model , prediction  and original data:

# create forecast dataset:
wine4<-df2_train[,c(9,4,11,13,10)];
forecast2<- rbind(wine4,newdata_pr);


#plot final result 


ggplot()+
  geom_line(data = wine,aes(x = YR,
                            y = PR),color = "forestgreen")+
  geom_line(data = df2_train,aes(x = YR,
                                 y = ml_model$fitted.values))+
  geom_line(data = newdata_pr,aes(x = YR,
                                  y = PR), color = "red")+
  # plot a line to show the point where forecast started :
  geom_vline(xintercept = 1910 ,color = "blue");





#--------------------------------------------------------------------------------------------
#STEP 9 CLUSTERING 

# defining cluster within Price.

# K-Means Cluster Analysis - Univariate
fit <- kmeans(wine$PR, 3); # 3 cluster solution
fit;

fit$cluster;
fit$iter;

#get the clusters
cluster1 <- wine[fit$cluster==1,]; cluster1;
cluster2 <- wine[fit$cluster==2,]; cluster2;
cluster3 <- wine[fit$cluster==3,]; cluster3;


# creating 3 clusters 
wine$PRClass <- fit$cluster;
wine$PRClass[wine$PRClass==1] <- "LOW";
wine$PRClass[wine$PRClass==2] <- "MEDIUM";
wine$PRClass[wine$PRClass==3] <- "HIGH";

#Plot the clusters
plot(wine$PR, pch=21, bg=fit$cluster * 2 + 3 );



# STEP 9.2 CLUSTERING PART 
# Defining cluster based on Sugar level and alcohol level.
# K-Means Cluster Analysis - Multivariate
plot(wine[12:13], pch=21);

fit <- kmeans(wine[12:13], 4) # 4 cluster solution
fit;

# creating 4 clusters
wine$RsgAlClass <- fit$cluster;
wine$RsgAlClass[wine$RsgAlClass==1] <- "Cluster1";
wine$RsgAlClass[wine$RsgAlClass==2] <- "Cluster2";
wine$RsgAlClass[wine$RsgAlClass==3] <- "Cluster3";
wine$RsgAlClass[wine$RsgAlClass==4] <- "Cluster4";
wine$RsgAlClass <- factor(wine$RsgAlClass);



# plotting Clustering 
plot(wine[12:13], pch=21, bg=fit$cluster * 2 + 3 );
wine;












