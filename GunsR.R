# Econ Project - Do More Guns Reduce Crimes ?

#installing packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2,dplyr,grid,ggpubr,car,reshape,reshape2, moments,treemap,psych,plm,GGally , gplots,data.table, mlbench,  broom, lmtest, sandwich, reshape2)
search()
theme_set(theme_classic())

#reading data
guns<-read_stata(guns.dta)
summary(guns)

dim(guns)
## 1173 rows and 13 columns

##data treatment: converting state_id to character
guns$stateid <- as.character(guns$stateid)
guns$shall <- as.character(guns$shall)
#states are not identified by consecutive numbers
length(unique(guns$stateid))==max(guns$stateid)

##checking the missing values
any(is.na(guns))
#it's FALSE! so we dont have any missing value in our dataset.

#CHECK IF THE DATA IS BALANCED
##Hypothesis: every year should have all 51 states
guns %>% group_by(as.character(guns$year)) %>% summarise(state=n_distinct(guns$stateid)) 
#data is balanced pool with n=51 states, t= 23 and N=1173

#number of states in which shall law has been introduced in the observed time period (77 to 99)
states_with_shall <- guns %>% filter(shall==1) 
length(unique(states_with_shall$stateid))
#only in 29 states out of 51 states shall law has been introduced

#when shall law started in each state

shall_start<-guns %>% filter(shall==1) %>% group_by(stateid) %>% summarise(minyear=min(year)) %>% arrange(as.numeric(minyear))
h<-hist(shall_start$minyear, breaks = seq(77,99,1))
##pie(shall_start$minyear,  main="Pie Chart of Countries")
#17 out of 29 states implemented the shall law after 1990
#Therefore this data may not be sufficient for understanding the delay effects of shall law on crime rate in states which introduced shall law in the later part of the observation period

#simultaneous causality
plot(guns$incarc_rate, guns$vio)

##check normality of variables
par(mfrow=c(4,4))
plot(density(guns$density), main= paste(" guns$density skewness: ",round(skewness(guns$density),2)))
plot(density(guns$incarc_rate), main= paste(" guns$incarc_rate skewness: ",round(skewness(guns$incarc_rate),2)))
plot(density(guns$pop), main= paste(" guns$pop skewness: ",round(skewness(guns$pop),2)))
plot(density(guns$vio), main= paste(" guns$vio skewness: ",round(skewness(guns$vio),2)))
plot(density(guns$avginc), main= paste(" guns$avginc skewness: ",round(skewness(guns$avginc),2)))
plot(density(guns$pm1029), main= paste(" guns$pm1029 skewness: ",round(skewness(guns$pm1029),2)))
plot(density(guns$pw1064), main= paste(" guns$pw1064 skewness: ",round(skewness(guns$pw1064),2)))
plot(density(guns$pb1064), main= paste(" guns$pb1064 skewness: ",round(skewness(guns$pb1064),2)))

#check correlation between variables

#pb1064 and pw1064 are highly negatively correlated
guns.dt<-setDT(guns)
######correlation between variables
cor.mat <- round(cor(guns.dt[,-c("shall","stateid","year")]),2) # rounded correlation matrix 
melted.cor.mat <- reshape::melt(cor.mat) 
ggplot(melted.cor.mat, aes(x = X1, y = X2, fill = value)) + 
  scale_fill_gradient(low="wheat", high="orangered") +
  geom_tile() + 
  geom_text(aes(x = X1, y = X2, label = value)) +
  ggtitle("Which Variables Are Highly Correlated?")

#Graphs - Visual interpretations
#writing code for testing different hypothesis in further part of the analysis
guns_minyear<- guns %>% 
  left_join(shall_start) %>% 
  mutate(shall_yesorno = ifelse(is.na(minyear),"no","yes"), 
         year_bin=ifelse(minyear<=80,"77-80",ifelse(minyear<=85,"81-85",ifelse(minyear<=90,"86-90",ifelse(minyear<=95,"91-95","96-99")))))
str(guns_minyear)
guns_year_grp<- guns_minyear %>% 
  group_by(year) %>% 
  summarise(avg.mur = round(mean(mur),2), avg.rob = round(mean(rob),2), avg.vio = round(mean(vio),2))


guns_yes_no<- guns_minyear %>% 
  group_by(year,shall_yesorno) %>% 
  summarise(avg.mur.s = round(mean(mur),2), avg.rob.s = round(mean(rob),2), avg.vio.s = round(mean(vio),2)) 

guns_yes<- guns_minyear %>% filter(shall_yesorno=="yes") %>%
  group_by(year,shall) %>% 
  summarise(number_of_sates=n(),avg.mur.s_yes = round(mean(mur),2), avg.rob.s_yes = round(mean(rob),2), avg.vio.s_yes = round(mean(vio),2)) 


# we need to change this part

guns_year_shall_yesno<-dcast(setDT(guns_yes_no), year ~ shall_yesorno,  value.var=c("avg.mur.s","avg.rob.s","avg.vio.s")) 

guns_year_shall<-dcast(setDT(guns_yes), year ~ shall,  value.var=c("avg.mur.s_yes","avg.rob.s_yes","avg.vio.s_yes","number_of_sates"))

temp_7780 <- guns_minyear %>% filter(year_bin=="77-80") %>%
  group_by(year) %>% 
  summarise(n_7780=n(),avg.mur.s_7780 = round(mean(mur),2), avg.rob.s_7780 = round(mean(rob),2), avg.vio.s_7780 = round(mean(vio),2)) 

temp_8184 <- guns_minyear %>% filter(year_bin=="81-84") %>%
  group_by(year) %>% 
  summarise(n_8185=n(),avg.mur.s_8185 = round(mean(mur),2), avg.rob.s_8185 = round(mean(rob),2), avg.vio.s_8185 = round(mean(vio),2)) 
temp_8589 <- guns_minyear %>% filter(year_bin=="85-89") %>%
  group_by(year) %>% 
  summarise(n_8690=n(),avg.mur.s_8690 = round(mean(mur),2), avg.rob.s_8690 = round(mean(rob),2), avg.vio.s_8690 = round(mean(vio),2)) 
temp_9094 <- guns_minyear %>% filter(year_bin=="90-94") %>%
  group_by(year) %>% 
  summarise(n_9195=n(),avg.mur.s_9195 = round(mean(mur),2), avg.rob.s_9195 = round(mean(rob),2), avg.vio.s_9195 = round(mean(vio),2)) 
temp_9599 <- guns_minyear %>% filter(year_bin=="95-99") %>%
  group_by(year) %>% 
  summarise(n_9699=n(),avg.mur.s_9699 = round(mean(mur),2), avg.rob.s_9699 = round(mean(rob),2), avg.vio.s_9699 = round(mean(vio),2)) 


guns_vio_years <- guns_year_grp %>%
  left_join(guns_year_shall_yesno) %>%
  left_join(guns_year_shall) %>%
  left_join(temp_7780) %>%
  left_join(temp_8184) %>%
  left_join(temp_8589) %>%
  left_join(temp_9094) %>%
  left_join(temp_9599)

guns_vio_years[is.na(temp_9094)] <- 0

###avg vio rate across different years
ggplot(guns_vio_years , aes(x=year)) + geom_line(aes(y = avg.vio), size=2, col="blue") + labs(title = "Avg. vio rate across different years")
#interpret


##Avg. vio rate for overall and shall/no-shall states
ggplot(guns_vio_years , aes(x=year)) +
  geom_line(aes(y = avg.vio , colour ="overall avg. vio_rate"), size=2) +
  geom_line(aes(y = avg.vio.s_yes, colour ="shall states avg. vio_rate"), size=2) +
  geom_line(aes(y = avg.vio.s_no, colour ="no shall states avg. vio_rate"), size=2) +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(color = "gray75", size = 0.5),
        panel.grid.major.x = element_blank()) + scale_color_manual(values=c("dodgerblue4", "darkolivegreen4",
                                                                            "darkorchid3"))
+ scale_fill_continuous(guide = guide_legend()) +
  theme(legend.position="bottom")+ 
  labs(title = "Avg. vio rate for overall and shall/no-shall states", y="rate")


###Analyzing the before and after avg. vio rate for states where shall law has been introduced
ggplot(guns_vio_years , aes(x=year)) +
  geom_line(aes(y = avg.vio.s_yes, colour ="shall states ovareall avg. vio_rate"), size=2) +
  geom_line(aes(y = avg.vio.s_yes_0, colour ="shall states avg. vio_rate before shall intro"), size=2) +
  geom_line(aes(y = avg.vio.s_yes_1, colour ="shall states avg. vio_rate after shall intro"), size=2) +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(color = "gray50", size = 0.5),
        panel.grid.major.x = element_blank()) +scale_color_manual(values=c("dodgerblue4", "darkolivegreen4",
                                                                           "darkorchid3")) +
  scale_fill_continuous(guide = guide_legend()) +
  theme(legend.position="bottom")+
  labs(title = "Analyzing the before and after avg. vio rate for states where shall law has been introduced", y="rate")

ggplot(guns8 , aes(x=year)) +
  geom_line(aes(y = avg.vio.s_7780 , colour ="states that introduced shall law in 77-80"), size=2) +
  geom_line(aes(y = avg.vio.s_8185 , colour ="states that introduced shall law in 81-85"), size=2) +
  geom_line(aes(y = avg.vio.s_8690 , colour ="states that introduced shall law in 86-90"), size=2) +
  geom_line(aes(y = avg.vio.s_9195 , colour ="states that introduced shall law in 91-95"), size=2) +
  geom_line(aes(y = avg.vio.s_9699 , colour ="states that introduced shall law in 96-99"), size=2) +
  geom_vline(xintercept = 80, alpha=0.5, linetype="dashed") +
  geom_vline(xintercept = 85, alpha=0.5, linetype="dashed") +
  geom_vline(xintercept = 90, alpha=0.5, linetype="dashed") +
  geom_vline(xintercept = 95, alpha=0.5, linetype="dashed") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(color = "gray50", size = 0.5),
        panel.grid.major.x = element_blank()) + scale_fill_continuous(guide = guide_legend()) +
  theme(legend.position="bottom")+
  labs(title = "Avg. vio rate for states that started shall law at different points in time", y="rate") 


##############################################################################################################

Part2
#Assumptions of linear regression

#Transforming the variables
## Log transformation for vio, mur, rob, incar_rate and density

guns1 <- guns

guns1$vio<-log(guns1$vio)
guns1$incarc_rate<-log(guns1$incarc_rate)
guns1$density<-log(guns1$density) 

#Pairwise Correlation - post data transformation
## pw1064 and pb1064 are highly correlated (-0.98)
pairs.panels(guns1[,!colnames(guns1) %in% c("shall","stateid","year")])

#Pooled regression
guns2<-pdata.frame(guns, index = c("stateid","year")) # Converting to panel data

#violence
#pooled
pooled_vio<-plm(log(vio)~shall+log(incarc_rate)+pb1064+pm1029+pw1064+pop+avginc+log(density),data=guns2, model="pooling")
summary(pooled_vio) # pooled ols without white std error
coeftest(pooled_vio,vcov.=vcovHC) # pooled ols with white std error

#since we see differnces in std.error, we suspect heteroskedacity.

#plots for heteroscedasticity
res <- residuals(pooled_vio)
yhat <- fitted(pooled_vio)
plot(log(guns2$pm1029),res, xlab="incarceration rate", ylab="residuals") +abline(h =0)

#combined significance test ; Hyposthesis 1
Hnull <- c("pb1064=0","pw1064=0")
linearHypothesis(pooled_vio,Hnull)

#remove pw1064,pb1064
pooled_vio2<-plm(log(vio)~shall+log(incarc_rate)+pm1029+pop+avginc+log(density),data=guns2, model="pooling")
coeftest(pooled_vio2, vcov=vcovHC(pooled_vio2,type="HC0",cluster="group")) # pooled ols with robust std errors

#fixed effects
fixed_vio<-plm(log(vio)~shall+log(incarc_rate)+pb1064+pm1029+pw1064+pop+avginc+log(density),data=guns2, model="within")
summary(fixed_vio)
coeftest(fixed_vio, vcov.=vcovHC(fixed_vio)) # robust errors

# Hyposthesis 2
Hnull <- c("avginc=0") 
linearHypothesis(fixed_vio,Hnull)

fixed_vio2<-plm(log(vio)~shall+log(incarc_rate)+pb1064+pm1029+pw1064+pop+log(density),data=guns2, model="within")
summary(fixed_vio2)
coeftest(fixed_vio2, vcov.=vcovHC(fixed_vio2)) # robust errors

#fixed effects with time

fixed_vio3 <- plm(log(vio)~shall+log(incarc_rate)+pb1064+pm1029+pw1064+pop+avginc+log(density)+factor(year)-1, data=guns2, model="within")
summary(fixed_vio3)
coeftest(fixed_vio3, vcov.=vcovHC(fixed_vio3))

# Hyposthesis 3
Hnull <- c("pb1064=0","pop=0","avginc=0","pw1064=0")
linearHypothesis(fixed_vio3,Hnull)

fixed_vio4 <- plm(log(vio)~log(incarc_rate)+pm1029+log(density)+shall+factor(year)-1, data=guns2, model="within")
summary(fixed_vio4)
coeftest(fixed_vio4, vcov.=vcovHC(fixed_vio4))

# Hyposthesis 4
#Joint signifcance of Instrument year
Hnull <- c("factor(year)78=0","factor(year)79=0","factor(year)80=0","factor(year)81=0","factor(year)82=0","factor(year)83=0",
           "factor(year)84=0","factor(year)85=0","factor(year)86=0","factor(year)87=0","factor(year)88=0","factor(year)89=0",
           "factor(year)90=0","factor(year)91=0","factor(year)92=0","factor(year)93=0","factor(year)94=0","factor(year)95=0",
           "factor(year)96=0","factor(year)97=0","factor(year)98=0","factor(year)99=0")
linearHypothesis(fixed_vio4,Hnull)

#Highly Significant

# Random effects

random_vio <- plm(log(vio)~log(incarc_rate)+pb1064+pm1029+pop+avginc+log(density)+shall+pw1064, data=guns2, model="random")
summary(random_vio)

# Hypothesis 5
Hnull <- c("avginc=0","log(incarc_rate)=0")
linearHypothesis(random_vio,Hnull)

random_vio2<-plm(log(vio)~pb1064+pm1029+pop+log(density)+shall+pw1064,data=guns2, model="random")
summary(random_vio2)

#random effects with time

random_vio3 <- plm(log(vio)~shall+log(incarc_rate)+pb1064+pm1029+pw1064+pop+avginc+log(density)+factor(year)-1, data=guns2, model="random")
summary(random_vio3)
coeftest(random_vio3, vcov.=vcovHC(random_vio3)) # robust errors

# Hypothesis 6
phtest(fixed_vio3,random_vio3) # hausman test