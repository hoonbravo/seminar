## install.packages("https://cran.r-project.org/src/contrib/Archive/Zelig/Zelig_4.2-1.tar.gz", 
##                 repos=NULL, 
##                 type="source")
## install.packages("MatchIt")
## install.packages("cobalt")
## install.packages("sensitivitymw")
## install.packages("sensitivityfull")
## devtools::install_github("vdorie/treatSens")
##devtools::install_github('IQSS/Zelig')
install.packages("BayesTree")
##library(Zelig)
library(Hmisc) 
library(tidyverse)
library(Zelig)
library(readxl)
library(dplyr)
library(writexl)
##important? packages
library(nnet) 
library(MatchIt) 
library(cobalt) 
library(sensitivityfull) 
library(sensitivitymw)
library(causaldrf)

##cor, emp merge
work<-read.table("C:\\Users\\HOON\\Desktop\\seminar\\5. TXT Data\\HCCP_Head_7th.txt", header=T, fill=T, sep="\t") %>% 
  select(C7C02_01_08, ##direct fee
         C7C01_07_01,C7C01_07_02,C7C01_07_05,C7C01_07_06,C7C01_07_09,C7C01_07_10,
         C7C01_07_13,C7C01_07_14,C7C01_07_17,C7C01_07_18,C7C01_07_21,C7C01_07_22, ##CD
         C7C01_08_01:C7C01_08_18, 
         C7C01_09_01,C7C01_09_02,C7C01_09_03,C7C01_09_04,C7C01_09_05,C7C01_09_06,
         C7C01_09_07,C7C01_09_08,C7C01_09_13,C7C01_09_14, ##self, TD, OD
         C7C01_01,C7C01_01_01,C7C01_02,C7C01_03, ##INFRA
         C7C02_04_01,C7C02_04_02,C7C02_04_04,C7C02_04_06,C7C02_04_07, ##ENVIR
         C7D05_01,C7D05_02,C7D05_03,C7D05_04, ##HRM
         C7_ID1,C7_IND1,C7B02_01_04,C7B02_01_01,C7A01_01,C7D07_02,C7B02_03_04,C7B02_03_05,C7B02_03_06,C7B01_07,C7D01_05,C7D01_07,   ##CONTROL
         C7C02_03_01,C7C02_03_02,C7C02_03_03,C7C02_03_04,C7C02_03_05) ##succ

sell<-read.table("C:\\Users\\HOON\\Desktop\\seminar\\5. TXT Data\\HCCP_KIS.txt", header=T, fill=T, sep="\t") %>% 
  filter(YYYY==2017) %>% 
  select(ID1, K_121000) %>% 
  rename("C7_ID1"="ID1")
sell$K_121000<-log(sell$K_121000)
##Cleaning

## avg of succ
work$succ<-(work$C7C02_03_01+work$C7C02_03_02)/2

##control, sort
work$C7_IND1<-ifelse(work$C7_IND1==1,1,0)
work$C7D01_05<-2-work$C7D01_05
work$C7D01_07<-2-work$C7D01_07
##years of company
work$C7A01_01<-2017-work$C7A01_01

## # of employee
work<-work[(!work$C7B02_03_04==-9),]
work$percent<-work$C7B02_01_04/work$C7B02_01_01
work$emplnum<-log(work$C7B02_01_01)
## % of HE
work$HE<-(work$C7B02_03_04+work$C7B02_03_05+work$C7B02_03_06)/work$C7B02_01_01

## nozo
work$C7D07_02<-2-work$C7D07_02
##direct fee
work<-work[(!work$C7C02_01_08==-8),]  ##remove direct fee -8,-9
work<-work[(!work$C7C02_01_08==-9),] 
work<-work[(!work$C7C02_01_08==0),] 

##work %>% filter(C7C01_07_01 %in% c(1,2)) %>% ##remove CD -8,-9
##  filter(C7C01_07_05 %in% c(1,2)) %>% 
##  filter(C7C01_07_09 %in% c(1,2)) %>% 
##  filter(C7C01_07_13 %in% c(1,2)) %>%
##  filter(C7C01_07_17 %in% c(1,2)) %>% 
##  filter(C7C01_07_21 %in% c(1,2)) %>% 
##  filter(C7C01_07_02!=-9) %>% 
##  filter(C7C01_07_06!=-9) ->work2

work<-work[!(work$C7C01_07_01==1&work$C7C01_07_02==-9),] ##yes implement, no fee, CD
work<-work[!(work$C7C01_07_05==1&work$C7C01_07_06==-9),] ##yes implement, no fee
work<-work[!(work$C7C01_07_09==1&work$C7C01_07_10==-9),] ##yes implement, no fee
work<-work[!(work$C7C01_07_13==1&work$C7C01_07_14==-9),] ##yes implement, no fee
work<-work[!(work$C7C01_07_17==1&work$C7C01_07_18==-9),] ##yes implement, no fee
work<-work[!(work$C7C01_07_21==1&work$C7C01_07_22==-9),] ##yes implement, no fee

work %>% filter(C7C02_04_01 %in% c(1,2,3,4)) %>% ##remove ENVIR -8, -9
  filter(C7C02_04_02 %in% c(1,2,3,4)) %>%
  filter(C7C02_04_04 %in% c(1,2,3,4)) %>% 
  filter(C7C02_04_06 %in% c(1,2,3,4)) %>% 
  filter(C7C02_04_07 %in% c(1,2,3,4)) ->work

work %>% filter(C7D05_01 %in% c(1,2)) %>% ##remove HRM -8, -9
  filter(C7D05_02 %in% c(1,2)) %>% 
  filter(C7D05_03 %in% c(1,2)) %>% 
  filter(C7D05_04 %in% c(1,2)) -> work

##cost
work$X1_1<-1000000*work$C7C02_01_08/work$C7B02_01_01
work$X1_2<-ifelse(work$X1_1>1,work$X1_1,0)
work$X1<-log(work$X1_1)/log(10000000)

##TD
work$X2_1<-ifelse(work$C7C01_07_01==2,0,ifelse(work$C7C01_07_02/work$C7B02_01_01<0.25,1,ifelse(work$C7C01_07_02/work$C7B02_01_01<0.50,2,ifelse(work$C7C01_07_02/work$C7B02_01_01<0.75,3,4))))
work$X2_2<-ifelse(work$C7C01_07_05==2,0,ifelse(work$C7C01_07_06/work$C7B02_01_01<0.25,1,ifelse(work$C7C01_07_06/work$C7B02_01_01<0.50,2,ifelse(work$C7C01_07_06/work$C7B02_01_01<0.75,3,4))))
work$X2_3<-ifelse(work$C7C01_07_09==2,0,ifelse(work$C7C01_07_10/work$C7B02_01_01<0.25,1,ifelse(work$C7C01_07_10/work$C7B02_01_01<0.50,2,ifelse(work$C7C01_07_10/work$C7B02_01_01<0.75,3,4))))
work$X2_4<-ifelse(work$C7C01_07_13==2,0,ifelse(work$C7C01_07_14/work$C7B02_01_01<0.25,1,ifelse(work$C7C01_07_14/work$C7B02_01_01<0.50,2,ifelse(work$C7C01_07_14/work$C7B02_01_01<0.75,3,4))))
work$X2_5<-ifelse(work$C7C01_07_17==2,0,ifelse(work$C7C01_07_18/work$C7B02_01_01<0.25,1,ifelse(work$C7C01_07_18/work$C7B02_01_01<0.50,2,ifelse(work$C7C01_07_18/work$C7B02_01_01<0.75,3,4))))

work$X2_6<-ifelse(work$C7C01_07_21==2,0,ifelse(work$C7C01_07_22/work$C7B02_01_01<0.25,1,ifelse(work$C7C01_07_22/work$C7B02_01_01<0.50,2,ifelse(work$C7C01_07_22/work$C7B02_01_01<0.75,3,4))))
work$X2_7<-ifelse(work$C7C01_09_01==2,0,work$C7C01_09_02)
work$X2<-(work$X2_1+work$X2_2+work$X2_3+work$X2_4+work$X2_5+work$X2_6+work$X2_7)/28
##self
work$X3_1<-ifelse(work$C7C01_08_01==2,0,work$C7C01_08_02)
work$X3_2<-ifelse(work$C7C01_08_03==2,0,work$C7C01_08_04)
work$X3_3<-ifelse(work$C7C01_08_05==2,0,work$C7C01_08_06)
work$X3_4<-ifelse(work$C7C01_08_07==2,0,work$C7C01_08_08)
work$X3<-(work$X3_1+work$X3_2+work$X3_3+work$X3_4)/16
##CD
work$X4_1<-ifelse(work$C7C01_08_15==2,0,work$C7C01_08_16)
work$X4_2<-ifelse(work$C7C01_09_03==2,0,work$C7C01_09_04)
work$X4_3<-ifelse(work$C7C01_08_11==2,0,work$C7C01_08_12)
work$X4_4<-ifelse(work$C7C01_08_13==2,0,work$C7C01_08_14)
work$X4_5<-ifelse(work$C7C01_08_09==2,0,work$C7C01_08_10)
work$X4<-(work$X4_1+work$X4_2+work$X4_3+work$X4_4+work$X4_5)/20
##OD
work$X5_1<-ifelse(work$C7C01_08_17==2,0,work$C7C01_08_18)
work$X5_2<-ifelse(work$C7C01_09_13==2,0,work$C7C01_09_14)
work$X5_3<-ifelse(work$C7C01_09_05==2,0,work$C7C01_09_06)
work$X5_4<-ifelse(work$C7C01_09_07==2,0,work$C7C01_09_08)
work$X5<-(work$X5_1+work$X5_2+work$X5_3+work$X5_4)/16
##INFRA
work$X6_1<-ifelse(work$C7C01_02==1,2,ifelse(work$C7C01_01_01==1,1,0))
work$X6_2<-ifelse(work$C7C01_02==1,1,0)
work$X6_3<-ifelse(work$C7C01_03==1,1,0)
work$X6<-(work$X6_1+work$X6_2+work$X6_3)/4
##ENVIR
work$X7_1<-6-work$C7C02_04_01

work$X7_2<-6-work$C7C02_04_02
work$X7_3<-6-work$C7C02_04_04

work$X7_4<-6-work$C7C02_04_06
work$X7_5<-6-work$C7C02_04_07
work$X7<-(work$X7_1+work$X7_2+work$X7_3+work$X7_4+work$X7_5)/20
##HRM
work$X8_1<-ifelse(work$C7D05_01==1,1,0)
work$X8_2<-ifelse(work$C7D05_02==1,1,0)
work$X8_3<-ifelse(work$C7D05_03==1,1,0)
work$X8_4<-ifelse(work$C7D05_04==1,1,0)
work$X8<-(work$X8_1+work$X8_2+work$X8_3+work$X8_4)/4

##conc
## index<-work[,55:113]
## index<-subset(index, select=-c(C7B02_01_04, C7B02_03_04, C7B02_03_05, C7B02_03_06, C7C02_03_01,C7C02_03_02,C7C02_03_03,C7C02_03_04,C7C02_03_05))
## summary(index$X1)

## index2 = na.omit(index)
## index3 <- index[complete.cases(index),]
## write_xlsx(index3,path="C:\\Users\\HOON\\Desktop\\HCCP\\index.xlsx")
## index4<-read_xlsx(path="C:\\Users\\HOON\\Desktop\\HCCP\\index.xlsx")
## index<-na.omit(index)
## index<-index4

## index$treat<-index$X1+index$X2+index$X3+index$X4+index$X5+index$X6+index$X7+index$X8
## hist(index$treat)

##new data
index<-select(work,C7_ID1,succ,C7_IND1,emplnum,percent,C7A01_01,C7D07_02,HE,C7B01_07,C7D01_05,C7D01_07,X1,X2,X3,X4,X5,X6,X7,X8) ######################
index<-merge(index, sell, by="C7_ID1")
index$treat<-index$X1+index$X2+index$X3+index$X4+index$X5+index$X6+index$X7+index$X8
index<-na.omit(index)
## gps
lmGPS=lm(treat~C7_IND1+C7_IND1+HE+emplnum+C7B01_07+C7D01_05+C7D01_07+K_121000, index)  ##########################
summary(lmGPS)

index$gps=dnorm(index$treat,
                mean=lmGPS$fitted,
                sd=summary(lmGPS)$sigma)
index$numerator=dnorm(index$treat,
                      mean=mean(index$treat),
                      sd=sd(index$treat))

index$IPW=index$numerator/index$gps
hist(index$gps)
hist(index$numerator)
hist(index$IPW)
###### read
###### index

write_xlsx(index,path="C:\\Users\\HOON\\Desktop\\seminar\\index.xlsx")
index<-read_excel(path="C:\\Users\\HOON\\Desktop\\seminar\\index.xlsx")
index_hi<-select(index,succ,treat,C7_IND1,HE,emplnum,C7B01_07,C7D01_05,C7D01_07,K_121000)

##hist
hist(index$succ)
hist(index$C7_IND1)
hist(index$emplnum)
hist(log(index$percent))  ###log
hist(index$C7A01_01)
hist(index$C7D07_02)  ####no
hist(index$HE)
hist(index$C7B01_07)
hist(index$C7D01_05)
hist(index$C7D01_07)
hist(index$K_121000)
hist(index$gps)
plot(index$treat,index$succ)
##
stddata=index %>% 
  mutate_at(
    vars(C7_IND1,percent,C7A01_01,C7D07_02,HE,emplnum,C7B01_07,C7D01_05,C7D01_07,K_121000,treat), ##############
    function(x){(x-mean(x))/sd(x)}
  )


##standarized beta
lm(C7_IND1~treat,stddata)$coef %>% round(4) ##low
lm(emplnum~treat,stddata)$coef %>% round(4)
lm(percent~treat,stddata)$coef %>% round(4) ##low
lm(C7A01_01~treat,stddata)$coef %>% round(4) ##low
lm(C7D07_02~treat,stddata)$coef %>% round(4) ##low
lm(HE~treat,stddata)$coef %>% round(4) ##low
lm(C7B01_07~treat,stddata)$coef %>% round(4)
lm(C7D01_05~treat,stddata)$coef %>% round(4)
lm(C7D01_07~treat,stddata)$coef %>% round(4)
lm(K_121000~treat,stddata)$coef %>% round(4)

lm(C7_IND1~treat,stddata, weights=IPW)$coef %>% round(4)
lm(emplnum~treat,stddata, weights=IPW)$coef %>% round(4)
lm(percent~treat,stddata, weights=IPW)$coef %>% round(4)
lm(C7A01_01~treat,stddata, weights=IPW)$coef %>% round(4)
lm(C7D07_02~treat,stddata, weights=IPW)$coef %>% round(4)
lm(HE~treat,stddata, weights=IPW)$coef %>% round(4)
lm(C7B01_07~treat,stddata, weights=IPW)$coef %>% round(4)
lm(C7D01_05~treat,stddata, weights=IPW)$coef %>% round(4)
lm(C7D01_07~treat,stddata, weights=IPW)$coef %>% round(4)
lm(K_121000~treat,stddata, weights=IPW)$coef %>% round(4)

summary(lm(succ~treat+C7_IND1+C7_IND1+HE+emplnum+C7B01_07+C7D01_05+C7D01_07+K_121000,weights=IPW, index))  ##########################
#IPW
set.seed(12)
z_out_ipw=zelig(succ~treat+emplnum+C7B01_07+C7D01_05+C7D01_07+K_121000, ######## 
                data=index,
                model="ls",
                weights="IPW",
                cite=FALSE)

Table_Sim10000=data.frame()
range_treat=quantile(index$treat,prob=0.1*(1:9))
for(i in 1:length(range_treat)){
  X=setx(z_out_ipw,treat=range_treat[i],data=index) 
  S=sim(z_out_ipw,X,num=10000)
  EV=data.frame(t(get_qi(S,"ev")))
  Table_Sim10000=rbind(Table_Sim10000,EV)
}
names(Table_Sim10000)=str_c("sim",1:10000)
Table_Sim10000$treat=range_treat

IPW_estimate=Table_Sim10000 %>% 
  pivot_longer(cols=sim1:sim10000, names_to="sim") %>% 
  group_by(treat) %>% 
  summarise(
    LL95 = quantile(value, p=0.025),
    PEst = quantile(value, p=0.5),
    UL95 = quantile(value, p=0.975)
  )

IPW_estimate

##Simple OLS
z_out_ols=zelig(succ~treat+emplnum+C7B01_07+C7D01_05+C7D01_07+K_121000,  ############## 
                data=index,
                model="ls",
                cite=FALSE)

Table_Sim10000=data.frame()
for(i in 1:length(range_treat)){
  X=setx(z_out_ols,treat=range_treat[i],data=index) ##mydata?
  S=sim(z_out_ols,X,num=10000)
  EV=data.frame(t(get_qi(S,"ev")))
  Table_Sim10000=rbind(Table_Sim10000,EV)
}
names(Table_Sim10000)=str_c("sim",1:10000)
Table_Sim10000$treat=range_treat

OLS_estimate=Table_Sim10000 %>% 
  pivot_longer(cols=sim1:sim10000, names_to="sim") %>% 
  group_by(treat) %>% 
  summarise(
    LL95 = quantile(value, p=0.025),
    PEst = quantile(value, p=0.5),
    UL95 = quantile(value, p=0.975)
  )

OLS_estimate

##visual
bind_rows(OLS_estimate %>%  mutate(model="OLS"),
          IPW_estimate %>%  mutate(model="IPW")) %>% 
  ggplot(aes(x=treat, y=PEst, fill=model)) +
  geom_point(aes(col=model, shape=model),size=2)+
  geom_ribbon(aes(ymin=LL95, ymax=UL95), alpha=0.3) +
  labs(x="X, continuous variable\n(Dose)",
       y="Point estimates with their 95%CI\n(Response)",
       fill="Model", shape="Model",color="Model")+
  scale_x_continuous(breaks=round(IPW_estimate$treat,1))+
  coord_cartesian(ylim=c(1,3))+
  theme_bw()+
  theme(legend.position = "top")
## write_xlsx(treat,path="C:\\Users\\HOON\\Desktop\\HCCP\\merge.xlsx")

####Hirano-Imbens
index_no_gps<-subset(index,select=-gps)
hi_estimate <- hi_est(succ,
                      treat,
                      treat_formula = treat ~ C7_IND1+HE+emplnum+C7B01_07+C7D01_05+C7D01_07+K_121000,
                      outcome_formula = succ~treat+gps+treat*gps,
                      #succ~treat+I(treat^2)+gps+I(gps^2)+treat * gps,
                      #succ~treat+gps+treat*gps,
                      data = index_hi,
                      grid_val = seq(0,8, by = 1),
                      treat_mod = "Normal")
summary(hi_estimate)
hi_estimate[[1]]
plot(hi_estimate[[1]])
summary(lm(succ~treat+C7_IND1+HE+emplnum+C7B01_07+C7D01_05+C7D01_07+K_121000, data=index))

##### using_causaldrf
reg_estimate <- reg_est(Y = succ,
                        treat = treat,
                        covar_formula = ~ C7_IND1+HE+emplnum+C7B01_07+C7D01_05+C7D01_07+K_121000,
                        covar_lin_formula = ~ 1,
                        covar_sq_formula = ~ 1,
                        data = index,
                        degree = 2,
                        wt = index$IPW,
                        method = "different")

reg_estimate


library(BayesTree)

bart_estimate <- bart_est(Y = succ,
                          treat = treat,
                          outcome_formula = succ ~ treat+C7_IND1+HE+emplnum+C7B01_07+C7D01_05+C7D01_07+K_121000,
                          data = index,
                          grid_val = seq(0,8, by = 1))

iw_estimate <- iw_est(Y = succ,
                      treat = treat,
                      treat_formula = treat ~ C7_IND1+HE+emplnum+C7B01_07+C7D01_05+C7D01_07+K_121000,
                      data = index,
                      grid_val = seq(0,8, by = 1),
                      bandw = 2 * bw.SJ(index$treat),
                      treat_mod = "Normal")
summary(iw_estimate)
library(ggplot2)
plot(iw_estimate[[2]])
ggplot(data=index, aes(x=treat, y=iw_estimate))
summary(iw_estimate)
summary(bart_estimate)

