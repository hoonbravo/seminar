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
alpha(index[,15:21])
alpha(index[,23:26])
alpha(index[,28:32])
alpha(index[,34:37])
alpha(index[,39:41])
alpha(index[,43:47])
alpha(index[,49:52])

##reliability
cor(index[,c("succ","X1","X2","X3","X4","X5","X6","X7","X8")])

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

