##descriptive statistics
######
summary(index$succ)
summary(index$treat)
summary(index$C7C02_03_01)
sd(index$C7C02_03_01)
summary(index$C7C02_03_02)
sd(index$C7C02_03_01)

summary(index$treat)
sd(index$treat)

summary(index$C7_IND1)
summary(index$C7B02_03_04)
sd(index$C7B02_03_04)
summary(index$HE)
sd(index$HE)
hist(index$HE)
summary(index$C7B01_07)
summary(index$C7D01_05)
summary(index$C7D01_07)
summary(0.001*index$K_121000)
sd(index$K_121000)
summary(index$percent)
sd(index$percent)
summary(2017-index$C7A01_01)
sd(2017-index$C7A01_01)
summary(index$C7D07_02)
sd(index$C7D07_02)


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
hist(log(index$percent))  
hist(index$C7A01_01)
hist(index$C7D07_02)  ####no
hist(index$HE)
hist(index$C7B01_07)
hist(index$C7D01_05)
hist(index$C7D01_07)
hist(index$K_121)
hist(index$gps)
plot(index$treat,index$succ)

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
cor<-as.data.frame(cor(index[,c("succ","X1","X2","X3","X4","X5","X6","X7","X8")]))
write_xlsx(cor,path="C:\\Users\\HOON\\Desktop\\seminar\\cor.xlsx")

cor_treat<-as.data.frame(cor(index[,c("treat","C7_IND1","emplnum","C7B01_07","C7D01_05","C7D01_07","K_121000")]))
write_xlsx(cor_treat,path="C:\\Users\\HOON\\Desktop\\seminar\\cor_treat.xlsx")

### ggplot
#######
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
       col="Proportion of High Education") +
  theme_bw()+
  theme(legend.position = "top")

gridExtra::grid.arrange(ind1,empl,taln,plan, ncol=2)
gridExtra::grid.arrange(dacum,sale,high, ncol=2)

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

gridExtra::grid.arrange(ind12,empl2,taln2,plan2, ncol=2)
gridExtra::grid.arrange(dacum2,sale2,high2, ncol=2)

