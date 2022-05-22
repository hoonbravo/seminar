summary(work)

##descriptive statistics
######
summary(index$C7C02_03_01)
sd(index$C7C02_03_01)
summary(index$C7C02_03_02)
sd(index$C7C02_03_01)

summary(index$treat)
sd(index$treat)

summary(index$C7_IND1)
sd(index$C7_IND1)
summary(index$C7B02_01_01)
sd(index$C7B02_01_01)
summary(index$percent)
summary(index$C7A01_01)
sd(index$C7A01_01)
summary(index$C7D07_02)
summary(index$C7B01_07)
summary(index$HE)
summary(index$C7B01_07)
summary(index$C7D01_05)
summary(index$C7D01_05_01)
summary(index$C7D01_07)
summary(0.000001*index$salesale)
sd(0.000001*index$salesale)

summary(index$gps)
summary(pnorm(index$gps))
summary(lmGPS$fitted)
summary(index$numerator)
summary(pnorm(index$numerator))
summary(index$IPW)

##hist
hist(index$succ)
hist(index$C7_IND1) ##binary
hist(index$emplnum)
hist(work$C7B02_01_01)
hist(log(index$percent))  
hist(index$C7A01_01)
hist(index$C7D07_02)  ####no
hist(index$HE)
hist(index$C7B01_07)
hist(index$C7D01_05)
hist(index$C7D01_07)
hist(index$K_121000)
hist(index$gps)
plot(index$treat,index$succ)

ggplot(data=index,aes(x=treat,y=succ)) + 
  stat_smooth(method='lm')
lm(succ~treat,data=index)
##GPS
#######
index %>% 
  ggplot(aes(x=C7_ID1)) +
  geom_point(aes(y=treat),color="blue")+
  geom_point(aes(y=lmGPS$fitted,color="red"))

index %>% 
  ggplot()+
  geom_point(aes(x=treat,y=lmGPS$fitted)) +
  geom_abline(slope=1, intercept=0)

##histo
#######
hist(index$succ)
hist(index$treat)

hist(index$gps)
hist(pnorm(index$gps))
hist(lmGPS$fitted)
hist(index$numerator)
hist(pnorm(index$numerator))
hist(index$IPW)
ggplot(data=index,aes(x=IPW))+geom_histogram()

##cronbach alpha
alpha(index[,3:4])
alpha(index[,17:23]) ##X2
alpha(index[,25:28]) ##X3
alpha(index[,20:34]) ##X4
alpha(index[,36:39]) ##X5
alpha(index[,41:43]) ##X6
alpha(index[,45:49]) ##X7
alpha(index[,51:54]) ##X8

##reliability
as.data.frame(cor(index[,c("succ","treat","C7_IND1","HE","percent",
                           "emplnum","C7D01_05","C7D01_05_01","C7D01_07",
                           "C7B01_07","K_121000")])) %>% round(3) ->cor
write_xlsx(cor,path="C:\\Users\\HOON\\Desktop\\seminar\\cor.xlsx")

as.data.frame(cor(index[,c("succ","treat","C7_IND1","emplnum","percent","C7A01_01",
                           "C7D07_02","HE","C7B01_07","C7D01_05","C7D01_05_01",
                           "C7D01_07","K_121000")])) %>% round(3) ->cor_treat
write_xlsx(cor_treat,path="C:\\Users\\HOON\\Desktop\\seminar\\cor_treat.xlsx")

cor
cor.test(index$succ,index$treat)
cor.test(index$succ,index$C7_IND1) ##low
cor.test(index$succ,index$emplnum)
cor.test(index$succ,index$percent) ##low
cor.test(index$succ,index$C7A01_01) ##low
cor.test(index$succ,index$C7D07_02) ##low
cor.test(index$succ,index$HE)
cor.test(index$succ,index$C7B01_07)
cor.test(index$succ,index$C7D01_05)
cor.test(index$succ,index$C7D01_05_01)
cor.test(index$succ,index$C7D01_07)
cor.test(index$succ,index$K_121000)

cor.test(index$treat,index$C7_IND1) ##low
cor.test(index$treat,index$emplnum)
cor.test(index$treat,index$percent) ##low
cor.test(index$treat,index$C7A01_01) ##low
cor.test(index$treat,index$C7D07_02) ##low
cor.test(index$treat,index$HE)
cor.test(index$treat,index$C7B01_07)
cor.test(index$treat,index$C7D01_05)
cor.test(index$treat,index$C7D01_05_01)
cor.test(index$treat,index$C7D01_07)
cor.test(index$treat,index$K_121000)

cor.test(index$C7_IND1,index$emplnum)
cor.test(index$C7_IND1,index$percent) ##low
cor.test(index$C7_IND1,index$C7A01_01) ##low
cor.test(index$C7_IND1,index$C7D07_02) ##low
cor.test(index$C7_IND1,index$HE)
cor.test(index$C7_IND1,index$C7B01_07)
cor.test(index$C7_IND1,index$C7D01_05)
cor.test(index$C7_IND1,index$C7D01_05_01)
cor.test(index$C7_IND1,index$C7D01_07)
cor.test(index$C7_IND1,index$K_121000)

cor.test(index$emplnum,index$percent) ##low
cor.test(index$emplnum,index$C7A01_01) ##low
cor.test(index$emplnum,index$C7D07_02) ##low
cor.test(index$emplnum,index$HE)
cor.test(index$emplnum,index$C7B01_07)
cor.test(index$emplnum,index$C7D01_05)
cor.test(index$emplnum,index$C7D01_05_01)
cor.test(index$emplnum,index$C7D01_07)
cor.test(index$emplnum,index$K_121000)

cor.test(index$percent,index$C7A01_01) ##low
cor.test(index$percent,index$C7D07_02) ##low
cor.test(index$percent,index$HE)
cor.test(index$percent,index$C7B01_07)
cor.test(index$percent,index$C7D01_05)
cor.test(index$percent,index$C7D01_05_01)
cor.test(index$percent,index$C7D01_07)
cor.test(index$percent,index$K_121000)

cor.test(index$C7A01_01,index$C7D07_02) ##low
cor.test(index$C7A01_01,index$HE)
cor.test(index$C7A01_01,index$C7B01_07)
cor.test(index$C7A01_01,index$C7D01_05)
cor.test(index$C7A01_01,index$C7D01_05_01)
cor.test(index$C7A01_01,index$C7D01_07)
cor.test(index$C7A01_01,index$K_121000)

cor.test(index$C7D07_02,index$HE)
cor.test(index$C7D07_02,index$C7B01_07)
cor.test(index$C7D07_02,index$C7D01_05)
cor.test(index$C7D07_02,index$C7D01_05_01)
cor.test(index$C7D07_02,index$C7D01_07)
cor.test(index$C7D07_02,index$K_121000)

cor.test(index$HE,index$C7B01_07)
cor.test(index$HE,index$C7D01_05)
cor.test(index$HE,index$C7D01_05_01)
cor.test(index$HE,index$C7D01_07)
cor.test(index$HE,index$K_121000)

cor.test(index$C7B01_07,index$C7D01_05)
cor.test(index$C7B01_07,index$C7D01_05_01)
cor.test(index$C7B01_07,index$C7D01_07)
cor.test(index$C7B01_07,index$K_121000)

cor.test(index$C7D01_05,index$C7D01_05_01)
cor.test(index$C7D01_05,index$C7D01_07)
cor.test(index$C7D01_05,index$K_121000)

cor.test(index$C7D01_05_01,index$C7D01_07)
cor.test(index$C7D01_05_01,index$K_121000)

cor.test(index$C7D01_07,index$K_121000)

##standarization
stddata=index %>% 
  mutate_at(
    vars(C7_IND1,emplnum, percent, C7A01_01,C7D07_02,HE,C7D01_05,C7D01_05_01,C7B01_07,C7D01_07,K_121000,treat), ##############
    function(x){(x-mean(x))/sd(x)}
  )

##standarized beta
summary(lm(C7_IND1~treat,stddata)) ##low
summary(lm(emplnum~treat,stddata)) 
summary(lm(percent~treat,stddata))  ##low
summary(lm(C7A01_01~treat,stddata))  ##low
summary(lm(C7D07_02~treat,stddata))  ##low
summary(lm(HE~treat,stddata))  
summary(lm(C7B01_07~treat,stddata)) 
summary(lm(C7D01_05~treat,stddata)) 
summary(lm(C7D01_05_01~treat,stddata))
summary(lm(C7D01_07~treat,stddata)) 
summary(lm(K_121000~treat,stddata)) 


summary(lm(C7_IND1~treat,stddata, weights=IPW)) ##low
summary(lm(percent~treat,stddata, weights=IPW)) ##low
summary(lm(C7A01_01~treat,stddata, weights=IPW)) ##low
summary(lm(C7D07_02~treat,stddata, weights=IPW)) ##low
summary(lm(emplnum~treat,stddata, weights=IPW)) 
summary(lm(HE~treat,stddata, weights=IPW))
summary(lm(C7B01_07~treat,stddata, weights=IPW))  ##high
summary(lm(C7D01_05~treat,stddata, weights=IPW))  ##high
summary(lm(C7D01_05_01~treat,stddata, weights=IPW)) ##high
summary(lm(C7D01_07~treat,stddata, weights=IPW)) 
summary(lm(K_121000~treat,stddata, weights=IPW)) ##high


model1<-lm(succ~emplnum+HE+C7B01_07+C7D01_05+C7D01_05_01+C7D01_07+K_121000, index) ##########################
summary(model1)
lm.beta(model1)$coef %>% round(3)
model2<-lm(succ~treat+emplnum+HE+C7B01_07+C7D01_05+C7D01_05_01+C7D01_07+K_121000, index) ##########################
summary(model2)
lm.beta(model2)

summary(lm(succ~treat+emplnum+HE+C7D01_05_01+C7D01_07,weights=IPW,  index)) ##########################
summary(lm(succ~treat+C7_IND1+HE+percent+emplnum+C7B01_07+C7D01_05+C7D01_07+K_121000+C7D01_05_01,weights=IPW, index)) ##########################


### ggplot
#######
ggplot(aes(x=treat,y=succ),data=index)+
  stat_smooth(method='lm')

ind1<-ggplot(aes(x=treat,y=succ,color=C7_IND1),data=index)+  
  geom_point() +
  scale_color_continuous_sequential(palette = 'Heat',
                                    begin = .2) +
  labs(x="X, Treat",
       y="Y, Job Performance",
       col="Industry") +
  theme_bw()+
  theme(legend.position = "top")

empl<-ggplot(aes(x=treat,y=succ,color=emplnum),data=index)+  
  geom_point()+
  scale_color_continuous_sequential(palette = 'Heat',
                                    begin = .2) +
  labs(x="X, Treat",
       y="Y, Job Performance",
       col="# of Employee") +
  theme_bw()+
  theme(legend.position = "top")

taln<-ggplot(aes(x=treat,y=succ,color=C7B01_07),data=index)+  
  geom_point()+
  scale_color_continuous_sequential(palette = 'Heat',
                                    begin = .2) +
  labs(x="X, Treat",
       y="Y, Job Performance",
       col="Concept of Key Talent") +
  theme_bw()+
  theme(legend.position = "top")

plan<-ggplot(aes(x=treat,y=succ,color=C7D01_05),data=index)+  
  geom_point()+
  scale_color_continuous_sequential(palette = 'Heat',
                                    begin = .2) +
  labs(x="X, Treat",
       y="Y, Job Performance",
       col="Presence of HR Plan") +
  theme_bw()+
  theme(legend.position = "top")

connect<-ggplot(aes(x=treat,y=succ,color=C7D01_05_01),data=index)+  
  geom_point() +
  scale_color_continuous_sequential(palette = 'Heat',
                                    begin = .2) +
  labs(x="X, Treat",
       y="Y, Job Performance",
       col="Linkage of HR Plan with Business Strategy") +
  theme_bw()+
  theme(legend.position = "top")

dacum<-ggplot(aes(x=treat,y=succ,color=C7D01_07),data=index)+  
  geom_point() +
  scale_color_continuous_sequential(palette = 'Heat',
                                    begin = .2) +
  labs(x="X, Treat",
       y="Y, Job Performance",
       col="Presence of Job Analysis") +
  theme_bw()+
  theme(legend.position = "top")

sale<-ggplot(aes(x=treat,y=succ,color=K_121000),data=index)+  
  geom_point() +
  scale_color_continuous_sequential(palette = 'Heat',
                                    begin = .2) +
  labs(x="X, Treat",
       y="Y, Job Performance",
       col="Sale Scale") +
  theme_bw()+
  theme(legend.position = "top")

high<-ggplot(aes(x=treat,y=succ,color=HE),data=index)+  
  geom_point() +
  scale_color_continuous_sequential(palette = 'Heat',
                                    begin = .2) +
  labs(x="X, Treat",
       y="Y, Job Performance",
       col="Proportion of High Education") +
  theme_bw()+
  theme(legend.position = "top")

age<-ggplot(aes(x=treat,y=succ,color=C7A01_01),data=index)+  
  geom_point() +
  scale_color_continuous_sequential(palette = 'Heat',
                                    begin = .2) +
  labs(x="X, Treat",
       y="Y, Job Performance",
       col="Age of Corporate") +
  theme_bw()+
  theme(legend.position = "top")

per<-ggplot(aes(x=treat,y=succ,color=percent),data=index)+  
  geom_point() +
  scale_color_continuous_sequential(palette = 'Heat',
                                    begin = .2) +
  labs(x="X, Treat",
       y="Y, Job Performance",
       col="Proportion of Temporary Employee") +
  theme_bw()+
  theme(legend.position = "top")

nozo<-ggplot(aes(x=treat,y=succ,color=C7D07_02),data=index)+  
  geom_point() +
  scale_color_continuous_sequential(palette = 'Heat',
                                    begin = .2) +
  labs(x="X, Treat",
       y="Y, Job Performance",
       col="Union in Corporate") +
  theme_bw()+
  theme(legend.position = "top")

gridExtra::grid.arrange(empl,high,sale, ncol=2)
gridExtra::grid.arrange(taln,plan,connect,dacum, ncol=2)

### ggplot_fitted
#######
ind12<-ggplot(aes(x=lmGPS$fitted,y=succ,color=C7_IND1),data=index)+  
  geom_point() +
  scale_color_continuous_sequential(palette = 'Heat',
                                    begin = .2) +
  labs(x="X, IPW Changed",
       y="Y, Job Performance",
       col="Industry") +
  theme_bw()+
  theme(legend.position = "top")

empl2<-ggplot(aes(x=lmGPS$fitted,y=succ,color=emplnum),data=index)+  
  geom_point()+
  scale_color_continuous_sequential(palette = 'Heat',
                                    begin = .2) +
  labs(x="X, IPW Changed",
       y="Y, Job Performance",
       col="# of Employee") +
  theme_bw()+
  theme(legend.position = "top")

taln2<-ggplot(aes(x=lmGPS$fitted,y=succ,color=C7B01_07),data=index)+  
  geom_point()+
  scale_color_continuous_sequential(palette = 'Heat',
                                    begin = .2) +
  labs(x="X, IPW Changed",
       y="Y, Job Performance",
       col="Concept of Key Talent") +
  theme_bw()+
  theme(legend.position = "top")

plan2<-ggplot(aes(x=lmGPS$fitted,y=succ,color=C7D01_05),data=index)+  
  geom_point()+
  scale_color_continuous_sequential(palette = 'Heat',
                                    begin = .2) +
  labs(x="X, IPW Changed",
       y="Y, Job Performance",
       col="Presence of HR Plan") +
  theme_bw()+
  theme(legend.position = "top")

connect2<-ggplot(aes(x=treat,y=succ,color=C7D01_05_01),data=index)+  
  geom_point() +
  scale_color_continuous_sequential(palette = 'Heat',
                                    begin = .2) +
  labs(x="X, IPW Changed",
       y="Y, Job Performance",
       col="Linkage of HR Pland with Business Strategy") +
  theme_bw()+
  theme(legend.position = "top")

dacum2<-ggplot(aes(x=lmGPS$fitted,y=succ,color=C7D01_07),data=index)+  
  geom_point() +
  scale_color_continuous_sequential(palette = 'Heat',
                                    begin = .2) +
  labs(x="X, IPW Changed",
       y="Y, Job Performance",
       col="Presence of Job Analysis") +
  theme_bw()+
  theme(legend.position = "top")

sale2<-ggplot(aes(x=lmGPS$fitted,y=succ,color=K_121000),data=index)+  
  geom_point() +
  scale_color_continuous_sequential(palette = 'Heat',
                                    begin = .2) +
  labs(x="X, IPW Changed",
       y="Y, Job Performance",
       col="Sale Scale") +
  theme_bw()+
  theme(legend.position = "top")

high2<-ggplot(aes(x=lmGPS$fitted,y=succ,color=HE),data=index)+  
  geom_point() +
  scale_color_continuous_sequential(palette = 'Heat',
                                    begin = .2) +
  labs(x="X, IPW Changed",
       y="Y, Job Performance",
       col="Proportion of High Education") +
  theme_bw()+
  theme(legend.position = "top")

age2<-ggplot(aes(x=lmGPS$fitted,y=succ,color=C7A01_01),data=index)+  
  geom_point() +
  scale_color_continuous_sequential(palette = 'Heat',
                                    begin = .2) +
  labs(x="X, IPW Changed",
       y="Y, Job Performance",
       col="Proportion of High Education") +
  theme_bw()+
  theme(legend.position = "top")

gridExtra::grid.arrange(dacum2,taln2,plan2,connect2, ncol=2)
gridExtra::grid.arrange(empl2,sale2,high2, ncol=2)
