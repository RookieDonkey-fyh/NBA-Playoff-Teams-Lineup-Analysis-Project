# load data
library(readxl)
set.seed(1)
data <- data <- data <- read_excel("C:/users/richie/desktop/STA141A/Project/data.xlsx", 
                                   col_types = c("text", "text", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric","text"))

# Data Cleaning
for(i in 1:nrow(data)){
  if(data$MIN[i]<5){ #minutes played per game lower than 5 minutes
    data[i,] = NA
  }
}
data = na.omit(data) #Delete players
for(i in 1:nrow(data)){
  if(data$GP[i]<40){ #played less than 40 games
    data[i,] = NA
  }
}
data = na.omit(data) #Delete players

#Describe data
#Highly correlated variables
library(corrplot)
par(mfrow=c(1,1))
corrplot(cor(as.matrix(data[,4:41]), use = "complete.obs"), method="circle",type="lower")
#boxplot, histogram...


#进攻防守图
plot(data$OFFRTG,data$DEFRTG)
abline(v=median(data$OFFRTG))
abline(h=median(data$DEFRTG))

#Standardize data
#Normalization is important in PCA since it is a variance maximizing exercise. It projects your 
#original data onto directions which maximize the variance. The first plot below shows the amount 
#of total variance explained in the different principal components wher we have not normalized the 
#data. 
data[,4:44] = as.data.frame(scale(data[,4:44]))

#PCA
data.pc <- princomp(as.data.frame(data[,4:44])) #also standardize the data
summary(data.pc)
#We choose 12 components to be the result of PCA becasue this is the lowest number of components that 
#contain more than 90 percent of the information of the original data.
par(mfrow = c(1,1))
#Elbow Plot decide the value of PC's
plot(1:(length(data.pc$sdev)),  (data.pc$sdev)^2, type='b', 
     main="Screen Plot", xlab="Number of Components", ylab="Eigenvalue Size")

pcdata = as.data.frame(data.pc$scores[,1:10])
pcdata$PLAYER = data$PLAYER
pcdata$TEAM = data$TEAM
pcdata$YEAR = data$YEAR
loading = data.pc$loadings[,1:10] #see the loadings of each components that we choose

#Plot the histograms of each components and show the main factors in it
library(ggplot2)
names = matrix('a',nrow=10,ncol=41)
values = matrix(0,nrow=10,ncol=41)
for(i in 1:10){
  values[i,] = as.numeric(loading[,i][order(abs(loading[,i]))])
  names[i,] = rownames(as.data.frame(loading[,i][order(abs(loading[,i]))]))
}
library(ggpubr)
ggarrange(ggplot(data.frame(Ability = names[1,31:41], Values = values[1,31:41], Group=ifelse(values[1,31:41]>0,'Positive','Negative')),aes(x=Ability,y=Values,fill=Group))+geom_bar(stat="identity")+coord_flip(),
          ggplot(data.frame(Ability = names[2,31:41], Values = values[2,31:41], Group=ifelse(values[2,31:41]>0,'Positive','Negative')),aes(x=Ability,y=Values,fill=Group))+geom_bar(stat="identity")+coord_flip(),
          ggplot(data.frame(Ability = names[3,31:41], Values = values[3,31:41], Group=ifelse(values[3,31:41]>0,'Positive','Negative')),aes(x=Ability,y=Values,fill=Group))+geom_bar(stat="identity")+coord_flip(),
          ggplot(data.frame(Ability = names[4,31:41], Values = values[4,31:41], Group=ifelse(values[4,31:41]>0,'Positive','Negative')),aes(x=Ability,y=Values,fill=Group))+geom_bar(stat="identity")+coord_flip(),
          ggplot(data.frame(Ability = names[5,31:41], Values = values[5,31:41], Group=ifelse(values[5,31:41]>0,'Positive','Negative')),aes(x=Ability,y=Values,fill=Group))+geom_bar(stat="identity")+coord_flip(),
          ggplot(data.frame(Ability = names[6,31:41], Values = values[6,31:41], Group=ifelse(values[6,31:41]>0,'Positive','Negative')),aes(x=Ability,y=Values,fill=Group))+geom_bar(stat="identity")+coord_flip(),
          ggplot(data.frame(Ability = names[7,31:41], Values = values[7,31:41], Group=ifelse(values[7,31:41]>0,'Positive','Negative')),aes(x=Ability,y=Values,fill=Group))+geom_bar(stat="identity")+coord_flip(),
          ggplot(data.frame(Ability = names[8,31:41], Values = values[8,31:41], Group=ifelse(values[8,31:41]>0,'Positive','Negative')),aes(x=Ability,y=Values,fill=Group))+geom_bar(stat="identity")+coord_flip(),
          ggplot(data.frame(Ability = names[9,31:41], Values = values[9,31:41], Group=ifelse(values[9,31:41]>0,'Positive','Negative')),aes(x=Ability,y=Values,fill=Group))+geom_bar(stat="identity")+coord_flip(),
          ggplot(data.frame(Ability = names[10,31:41], Values = values[10,31:41], Group=ifelse(values[10,31:41]>0,'Positive','Negative')),aes(x=Ability,y=Values,fill=Group))+geom_bar(stat="identity")+coord_flip(),
          ncol = 5, nrow = 2)

#有的时候PCA就能够直接将data分成几个组，为了选择合适的k值，我一一画出了各个components的pairwise的图
#不过在画外之后并没有发现有明显的分组，所以需要尝试别的方法
par(mfrow=c(1,1))
plot(x=data.pc$scores[,2],y=data.pc$scores[,3])

#基于对篮球的理解，我们知道普遍将球员分为五个位置。我们预计我们的数据能至少将球员分成五组，并且
#我们希望能将球员分组进一步的细分。所以我们从k=6开始尝试。当k=11的时候，我们找到了我们认为比较合适的
#分组，每个组都有自己鲜明的特色，很好的将球员分成了各个类型。所以我们最终选择k=11。

kclustering = kmeans(pcdata[,1:10], centers = 11, nstart = 30)
pcdata$cluster = kclustering$cluster
data$cluster = kclustering$cluster
#Put players in to groups
group1=group2=group3=group4=group5=group6=group7=group8=group9=group10=group11=pcdata
for(i in 1:nrow(pcdata)){
  if(pcdata$cluster[i]!=1){
    group1[i,] = NA
  }
  if(pcdata$cluster[i]!=2){
    group2[i,] = NA
  }
  if(pcdata$cluster[i]!=3){
    group3[i,] = NA
  }
  if(pcdata$cluster[i]!=4){
    group4[i,] = NA
  }
  if(pcdata$cluster[i]!=5){
    group5[i,] = NA
  }
  if(pcdata$cluster[i]!=6){
    group6[i,] = NA
  }
  if(pcdata$cluster[i]!=7){
    group7[i,] = NA
  }
  if(pcdata$cluster[i]!=8){
    group8[i,] = NA
  }
  if(pcdata$cluster[i]!=9){
    group9[i,] = NA
  }
  if(pcdata$cluster[i]!=10){
    group10[i,] = NA
  }
  if(pcdata$cluster[i]!=11){
    group11[i,] = NA
  }
}
group1 = na.omit(group1)
group2 = na.omit(group2)
group3 = na.omit(group3)
group4 = na.omit(group4)
group5 = na.omit(group5)
group6 = na.omit(group6)
group7 = na.omit(group7)
group8 = na.omit(group8)
group9 = na.omit(group9)
group10 = na.omit(group10)
group11 = na.omit(group11)

#Characteristics for each group


performance = data.frame('group1'=apply(group1[,1:10],2,mean),
                         'group2'=apply(group2[,1:10],2,mean),
                         'group3'=apply(group3[,1:10],2,mean),
                         'group4'=apply(group4[,1:10],2,mean),
                         'group5'=apply(group5[,1:10],2,mean),
                         'group6'=apply(group6[,1:10],2,mean),
                         'group7'=apply(group7[,1:10],2,mean),
                         'group8'=apply(group8[,1:10],2,mean),
                         'group9'=apply(group9[,1:10],2,mean),
                         'group10'=apply(group10[,1:10],2,mean),
                         'group11'=apply(group11[,1:10],2,mean))
library(tidyverse)
library(viridis)
performance%>% 
  rownames_to_column(var = "make") %>% 
  gather(var, val, -make) %>% 
  ggplot(aes(var, make)) + 
  geom_tile(aes(fill = val)) + 
  geom_text(aes(label = round(val, 2)), size = 3) + 
  coord_fixed() + 
  scale_fill_viridis() + 
  guides(fill = FALSE)


#找每个playoff球员属于哪个cluster
playoff$cluster = rep(0,nrow(playoff)) #create a column to store the group number
data$cluster = k4$cluster #add clustering result to the data
for(i in 1:nrow(data)){
  if(data$name[i] == playoff$name[i]){ #Find the group number of playoff players
    for(j in 1:nrow(playoff)){
      playoff$cluster[j] = data$cluster[i]
    }
  }
}


