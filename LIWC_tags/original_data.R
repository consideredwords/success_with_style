# Using LIWC data for the same books

##Hypotheses

# H0:  There's no difference in the proportion of LIWC categories in successful and unsuccessful books, regardless of genre
# HA:  There is a difference in the proportion of LIWC categories in successful and unsuccessful books, and the pattern will depend on genre

# H0: There's no difference in the LIWC summary values of successful and unsuccessful books, regardless of the book's genre  
# HB: There is a difference in the LIWC summary values of successful and unsuccessful books, and the pattern will depend on  genre 


#libraries
library(tidyverse); library(ggpubr); library(readxl);library(caret);library(caretEnsemble);library(Hmisc);library(bestNormalize);library(data.table)

#setwd as needed
# setwd("/")


#load LIWC data
lf<- read_excel("readability_data.xlsx",sheet = 3) #load LIWC proportional data
names(lf)<- tolower(names(lf))

mf<- read_excel("readability_data.xlsx",sheet = 4) #load LIWC absolute data
names(mf)<- tolower(names(mf))


#setwd as needed
# setwd("/")

lf<- lf %>% rename(dicWo=`dictionarywords`,sixletter=`6letter`,mWoSen=`words-per-sentence`,functional=`function`)

mf<- mf %>% rename(dicWo=`dictionarywords`,sixletter=`6letter`,mWoSen=`words-per-sentence`,functional=`function`)



## Rename checks
# df -> lf
# FR -> dicWo
# 6letter -> mSyWo

##Draw by each 10

#near zero
zero.vars<- names(lf)[nearZeroVar(lf)] #variables that are zero
lf<- lf %>% select(-one_of(zero.vars)) %>% mutate(ranking=factor(ranking))


#split data
set.seed(424)
index<- createDataPartition(y=lf$ranking, p=0.8,list = F)
trainl<- lf[index,] #create training data
testl<- lf[-index,] #create test


x<- trainl %>% select(analytic:otherp) 
y<- testl %>% select(analytic:otherp)

p<- sapply(x,function(x){sum(x==0)}) #check number of zeros
vars<- names(which(p>50))
x %>% select(one_of(vars)) %>% gather(key,value) %>% gghistogram("value",facet.by = "key",scales="free",ggtheme = theme_minimal())

x %>% select(-one_of(vars)) %>% gather(key,value) %>% gghistogram("value",facet.by = "key",scales="free",ggtheme = theme_minimal())

#transform vars
contl<- x %>% select(-one_of(vars))
for(i in colnames(contl)){
  f1<- bestNormalize(contl[[i]])
  contl[[i]]<- predict(f1,contl[[i]])
}

#t.test for cont vars

px<- data.frame(matrix(NA,nrow=ncol(contl),ncol=3,dimnames = list(c(NULL),c("Var","P","P.A"))))
px$Var<- names(contl)
for(i in seq_along(names(contl))){
  px[i,2]<- round(t.test(contl[[i]]~trainl$ranking)$p.value,3)
}
px$P.A<- round(p.adjust(px$P,method = "fdr"),3)

t1<- px %>% filter(P.A<0.1)
write.csv(t1,"t1_LIWC.csv") #write to excel


#continous vars transformation
contl %>%  gather(key,value) %>% gghistogram("value",facet.by = "key",scales="free",ggtheme = theme_minimal())

##non normal vars
catl<- x %>% select(one_of(vars))

#Chi.sq
px2<- data.frame(matrix(NA,nrow=ncol(catl),ncol=3,dimnames = list(c(NULL),c("Var","P","P.A"))))
px2$Var<- names(catl)
for(i in seq_along(names(catl))){
  px2[i,2]<- round(t.test(catl[[i]]~trainl$ranking)$p.value,3)
}
px2$P.A<- round(p.adjust(px2$P,method = "fdr"),3)

t2<- px2 %>% filter(P.A<0.1)
write.csv(t2,"t2_LIWC.csv")

used<- c(t1$Var,t2$Var) #vars to use

nldf<- data.frame(contl,catl) #combine good vars

#scale and plot analytic, clout, authentic, tone, 6letter, dicWo, mWoSen

trainl %>% select(ranking, analytic, clout, authentic, tone, sixletter, dicWo, mWoSen) %>% mutate_at(vars(analytic:mWoSen),log) %>% mutate_at(vars(analytic:mWoSen),scale) %>%  gather(key,value,-ranking) %>% 
  ggboxplot("key","value",fill="ranking",ggtheme = theme_minimal(),palette = "jco",legend="top",xlab="") +stat_compare_means(aes(group=ranking),method = "t.test")


ggsave("LIWC categories - original data.png", 
       plot = last_plot(), 
       width = 260, height = 171, 
       units = "mm", # other options c("in", "cm", "mm"), 
       dpi = 240)



nldf2<- trainl %>% select(analytic, clout, authentic, tone, sixletter, dicWo) %>% mutate_all(log) %>%  mutate_all(function(x){
  f1<- bestNormalize(x)
  x<- predict(f1,x)
})

nldf3<- data.frame(nldf,nldf2)  #Final data


##########################################################################################################
#Machine learning
trl.ctrl<- trainControl(method = "cv",number = 10)

trainl.x<- trainl %>% select(one_of(names(nldf)))
testl.x<- testl %>% select(one_of(names(nldf)))


##Apply same transformations to train and test data
for(i in seq_along(names(trainl.x))){
  f1<-bestNormalize(trainl.x[[i]])
  trainl.x[[i]]<- predict(f1, trainl.x[[i]])
  testl.x[[i]]<- predict(f1, testl.x[[i]])
  
}

fitl<- caret::train(y=trainl$ranking,x=trainl.x,trControl=trl.ctrl,method="svmLinear3",tuneLength=10)
plot(fitl)
tx<- predict(fitl,newdata=testl.x)
confusionMatrix(tx,testl$ranking)

fit2<- caret::train(y=trainl$ranking,x=trainl.x,trControl=trl.ctrl,method="glmboost",tuneLength=10)
x<- varImp(fit2)#relactive variable importance
write.csv(x$importance,"x_LIWC.csv")


#scale and plot - original

s.df<- lf %>% select(functional:otherp) %>% mutate(rank=lf$ranking,genre=lf$genre) #imps

p.df<- s.df %>% group_by(genre,rank) %>% summarise_all(sum)  %>% summarise_at(vars(functional:otherp),function(x){x[2]-x[1]}) #sum

write.csv(p.df,"LIWC_tag_difference.csv")

p.df<- p.df %>% gather(POS,diff,-genre) 


#scale and plot - new

t.df<- mf %>% select(functional:otherp) %>% mutate(rank=mf$ranking,genre=mf$genre) #imps


tt <- t.df  %>% mutate(status=t.df$rank,genre=mf$genre) %>% select(genre, status, functional:otherp)

tt2 <- tt %>% group_by(genre,status) %>% summarise_all(sum)

tt3 <- tt2 %>%  as.data.frame()


tt4 <- tt3 %>% mutate(rowSum = rowSums(.[,names(tt3)[3:87]])) %>% 
  group_by(genre) %>% 
  summarise_at(vars(names(tt3)[3:87]),   
               funs(net = .[status == "SUCCESS"]/rowSum[status == "SUCCESS"] - 
                      .[status == "FAILURE"]/rowSum[status == "FAILURE"] )) %>%
  as.data.frame()


write.csv(tt4,"LIWC_tag_difference_new.csv")

tt5<- tt4 %>% gather(POS,diff,-genre) 

plotdata <- tt5

##plots###############################

ggbarplot("POS","diff",data=plotdata[1:80,],fill="genre",legend="right",palette = "jco", position=position_dodge(),ggtheme = theme_minimal(),ylab = "Difference in LIWC tag proportion")

ggsave("LIWC Difference in proportion function-article - original data.png", 
       plot = last_plot(), 
       width = 260, height = 171, 
       units = "mm", # other options c("in", "cm", "mm"), 
       dpi = 240)

ggbarplot("POS","diff",data=plotdata[81:160,],fill="genre",legend="right",palette = "jco", position=position_dodge(),ggtheme = theme_minimal(),ylab = "Difference in LIWC tag proportion") 

ggsave("LIWC Difference in proportion prep-number - original data.png", 
       plot = last_plot(), 
       width = 260, height = 171, 
       units = "mm", # other options c("in", "cm", "mm"), 
       dpi = 240)


ggbarplot("POS","diff",data=plotdata[161:240,],fill="genre",legend="right",palette = "jco", position=position_dodge(),ggtheme = theme_minimal(),ylab = "Difference in LIWC tag proportion")

ggsave("LIWC Difference in proportion quant-friend - original data.png", 
       plot = last_plot(), 
       width = 260, height = 171, 
       units = "mm", # other options c("in", "cm", "mm"), 
       dpi = 240)


ggbarplot("POS","diff",data=plotdata[241:320,],fill="genre",legend="right",palette = "jco", position=position_dodge(),ggtheme = theme_minimal(),ylab = "Difference in LIWC tag proportion")

ggsave("LIWC Difference in proportion female-percept - original data.png", 
       plot = last_plot(), 
       width = 260, height = 171, 
       units = "mm", # other options c("in", "cm", "mm"), 
       dpi = 240)


ggbarplot("POS","diff",data=plotdata[321:400,],fill="genre",legend="right",palette = "jco", position=position_dodge(),ggtheme = theme_minimal(),ylab = "Difference in LIWC tag proportion")

ggsave("LIWC Difference in proportion see-affiliation - original data.png", 
       plot = last_plot(), 
       width = 260, height = 171, 
       units = "mm", # other options c("in", "cm", "mm"), 
       dpi = 240)


ggbarplot("POS","diff",data=plotdata[401:480,],fill="genre",legend="right",palette = "jco", position=position_dodge(),ggtheme = theme_minimal(),ylab = "Difference in LIWC tag proportion")

ggsave("LIWC Difference in proportion achieve-space - original data.png", 
       plot = last_plot(), 
       width = 260, height = 171, 
       units = "mm", # other options c("in", "cm", "mm"), 
       dpi = 240)


ggbarplot("POS","diff",data=plotdata[481:560,],fill="genre",legend="right",palette = "jco", position=position_dodge(),ggtheme = theme_minimal(),ylab = "Difference in LIWC tag proportion")

ggsave("LIWC Difference in proportion time-netspeak - original data.png", 
       plot = last_plot(), 
       width = 260, height = 171, 
       units = "mm", # other options c("in", "cm", "mm"), 
       dpi = 240)


ggbarplot("POS","diff",data=plotdata[561:640,],fill="genre",legend="right",palette = "jco", position=position_dodge(),ggtheme = theme_minimal(),ylab = "Difference in LIWC tag proportion")

ggsave("LIWC Difference in proportion assent-exclam - original data.png", 
       plot = last_plot(), 
       width = 260, height = 171, 
       units = "mm", # other options c("in", "cm", "mm"), 
       dpi = 240)


ggbarplot("POS","diff",data=p.df[641:680,],fill="genre",legend="right",palette = "jco", position=position_dodge(),ggtheme = theme_minimal(),ylab = "Difference in LIWC tag proportion")


ggsave("LIWC Difference in proportion dash-otherp - original data.png", 
       plot = last_plot(), 
       width = 260, height = 171, 
       units = "mm", # other options c("in", "cm", "mm"), 
       dpi = 240)



##########################################

s.df %>% group_by(genre,rank) %>% summarize_all(sum)

#diff between ranks overall

#scale and plot

tt6 <- setDT(tt)[,lapply(.SD,sum),status,.SDcols=3:87][,
                                                       .SD/rowSums(.SD),.SDcols=-1][,rbind(.SD,.SD[1]-.SD[2])]

tt7 <- tt6[-c(1,2),]

tt8<- tt7 %>% gather(POS,diff)  %>% arrange(desc(diff))
write.csv(tt8,"overall_difference_LIWC.csv")

###LIWC summary values
trainl<- trainl %>% mutate(l.clout=log(clout))

ggdensity("mWoSen",fill="ranking",data=trainl)

lf %>% select(ranking, analytic, clout, authentic, tone, mWoSen, sixletter, dicWo) %>% mutate_at(vars(analytic:clout),log) %>% gather(index,value,-ranking) %>% 
  mutate(ranking=tolower(ranking),ranking=capitalize(ranking)) %>%
  ggboxplot("ranking","value",facet.by="index",ggtheme = theme_minimal(),scales="free") +stat_compare_means(method="t.test" ,label = "p.format")

ggboxplot("ranking", "dicWo",data=trainl)
ggdensity("dicWo",fill="ranking",data=lf)

ggboxplot("ranking", "sixletter",data=trainl)
ggdensity("sixletter",fill="ranking",data=lf)

ggboxplot("ranking", "authentic",data=trainl)
ggdensity("authentic",fill="ranking",data=lf)

ggboxplot("ranking", "tone",data=trainl)
ggdensity("tone",fill="ranking",data=lf)

ggboxplot("ranking", "mWoSen",data=trainl)
ggdensity("mWoSen",fill="ranking",data=lf)

ggboxplot("ranking", "analytic",data=trainl)
ggdensity("analytic",fill="ranking",data=lf)

ggboxplot("ranking", "clout",data=trainl)
ggdensity("clout",fill="ranking",data=lf)

t.test(dicWo~ranking,data=trainl) #dicWo
t.test(sixletter~ranking,data=trainl) #6letter
t.test(authentic~ranking,data=trainl) #Authentic
t.test(tone~ranking,data=trainl) #tone
t.test(mWoSen~ranking,data=trainl) #mWoSen
t.test(analytic~ranking,data=trainl) #analytic
t.test(clout~ranking,data=trainl) #clout


#####LIWC categories by genre

#Clout

dx<- data.frame(rank=lf$ranking, genre=lf$genre, clout=lf$clout)

library(reshape2)
fx<- dx %>% group_by(genre,rank) %>% summarise(m=mean(clout),s=sd(clout)) %>% gather(key,value,c(m,s)) %>% 
  dcast(genre~rank+key,value.var = "value")
write.csv(fx,"clout_fx_LIWC.csv")

fx<- dx %>% mutate_at(vars(clout), function(x){
  f1<- bestNormalize(x)
  x<- predict(f1,x)
})

ggboxplot("genre","clout",fill="rank",data=fx,ggtheme = theme_minimal(),palette = "jco",legend="top",xlab = "Genre") +
  stat_compare_means(aes(group=rank,label = ..p.format..),method = "t.test")

ggsave("LIWC - Clout - by genre - original data.png", 
       plot = last_plot(), 
       width = 260, height = 171, 
       units = "mm", # other options c("in", "cm", "mm"), 
       dpi = 240)


#analytic

dx<- data.frame(rank=lf$ranking, genre=lf$genre, analytic=lf$analytic)

library(reshape2)
fx<- dx %>% group_by(genre,rank) %>% summarise(m=mean(analytic),s=sd(analytic)) %>% gather(key,value,c(m,s)) %>% 
  dcast(genre~rank+key,value.var = "value")
write.csv(fx,"analytic_fx_LIWC.csv")

fx<- dx %>% mutate_at(vars(analytic), function(x){
  f1<- bestNormalize(x)
  x<- predict(f1,x)
})

ggboxplot("genre","analytic",fill="rank",data=fx,ggtheme = theme_minimal(),palette = "jco",legend="top",xlab = "Genre") +
  stat_compare_means(aes(group=rank,label = ..p.format..),method = "t.test")


ggsave("LIWC - Analytic - by genre - original data.png", 
       plot = last_plot(), 
       width = 260, height = 171, 
       units = "mm", # other options c("in", "cm", "mm"), 
       dpi = 240)


#authentic

dx<- data.frame(rank=lf$ranking, genre=lf$genre, authentic=lf$authentic)

library(reshape2)
fx<- dx %>% group_by(genre,rank) %>% summarise(m=mean(authentic),s=sd(authentic)) %>% gather(key,value,c(m,s)) %>% 
  dcast(genre~rank+key,value.var = "value")
write.csv(fx,"authentic_fx_LIWC.csv")

fx<- dx %>% mutate_at(vars(authentic), function(x){
  f1<- bestNormalize(x)
  x<- predict(f1,x)
})

ggboxplot("genre","authentic",fill="rank",data=fx,ggtheme = theme_minimal(),palette = "jco",legend="top",xlab = "Genre") +
  stat_compare_means(aes(group=rank,label = ..p.format..),method = "t.test")


ggsave("LIWC - Authentic - by genre - original data.png", 
       plot = last_plot(), 
       width = 260, height = 171, 
       units = "mm", # other options c("in", "cm", "mm"), 
       dpi = 240)


#tone

dx<- data.frame(rank=lf$ranking, genre=lf$genre, tone=lf$tone)

library(reshape2)
fx<- dx %>% group_by(genre,rank) %>% summarise(m=mean(tone),s=sd(tone)) %>% gather(key,value,c(m,s)) %>% 
  dcast(genre~rank+key,value.var = "value")
write.csv(fx,"tone_fx_LIWC.csv")

fx<- dx %>% mutate_at(vars(tone), function(x){
  f1<- bestNormalize(x)
  x<- predict(f1,x)
})

ggboxplot("genre","tone",fill="rank",data=fx,ggtheme = theme_minimal(),palette = "jco",legend="top",xlab = "Genre") +
  stat_compare_means(aes(group=rank,label = ..p.format..),method = "t.test")


ggsave("LIWC - Tone - by genre - original data.png", 
       plot = last_plot(), 
       width = 260, height = 171, 
       units = "mm", # other options c("in", "cm", "mm"), 
       dpi = 240)


#sixletter

dx<- data.frame(rank=lf$ranking, genre=lf$genre, sixletter=lf$sixletter)

library(reshape2)
fx<- dx %>% group_by(genre,rank) %>% summarise(m=mean(sixletter),s=sd(sixletter)) %>% gather(key,value,c(m,s)) %>% 
  dcast(genre~rank+key,value.var = "value")
write.csv(fx,"sixletter_fx_LIWC.csv")

fx<- dx %>% mutate_at(vars(sixletter), function(x){
  f1<- bestNormalize(x)
  x<- predict(f1,x)
})

ggboxplot("genre","sixletter",fill="rank",data=fx,ggtheme = theme_minimal(),palette = "jco",legend="top",xlab = "Genre") +
  stat_compare_means(aes(group=rank,label = ..p.format..),method = "t.test")


ggsave("LIWC - Six letter words - by genre - original data.png", 
       plot = last_plot(), 
       width = 260, height = 171, 
       units = "mm", # other options c("in", "cm", "mm"), 
       dpi = 240)


#dicWo

dx<- data.frame(rank=lf$ranking, genre=lf$genre, dicWo=lf$dicWo)

library(reshape2)
fx<- dx %>% group_by(genre,rank) %>% summarise(m=mean(dicWo),s=sd(dicWo)) %>% gather(key,value,c(m,s)) %>% 
  dcast(genre~rank+key,value.var = "value")
write.csv(fx,"dicWo_fx_LIWC.csv")

fx<- dx %>% mutate_at(vars(dicWo), function(x){
  f1<- bestNormalize(x)
  x<- predict(f1,x)
})

ggboxplot("genre","dicWo",fill="rank",data=fx,ggtheme = theme_minimal(),palette = "jco",legend="top",xlab = "Genre") +
  stat_compare_means(aes(group=rank,label = ..p.format..),method = "t.test")


ggsave("LIWC - Dictionary words - by genre - original data.png", 
       plot = last_plot(), 
       width = 260, height = 171, 
       units = "mm", # other options c("in", "cm", "mm"), 
       dpi = 240)


#mWoSen

dx<- data.frame(rank=lf$ranking, genre=lf$genre, mWoSen=lf$mWoSen)

library(reshape2)
fx<- dx %>% group_by(genre,rank) %>% summarise(m=mean(mWoSen),s=sd(mWoSen)) %>% gather(key,value,c(m,s)) %>% 
  dcast(genre~rank+key,value.var = "value")
write.csv(fx,"mWoSen_fx_LIWC.csv")

fx<- dx %>% mutate_at(vars(mWoSen), function(x){
  f1<- bestNormalize(x)
  x<- predict(f1,x)
})

ggboxplot("genre","mWoSen",fill="rank",data=fx,ggtheme = theme_minimal(),palette = "jco",legend="top",xlab = "Genre") +
  stat_compare_means(aes(group=rank,label = ..p.format..),method = "t.test")


ggsave("LIWC - Mean words per sentence - by genre - original data.png", 
       plot = last_plot(), 
       width = 260, height = 171, 
       units = "mm", # other options c("in", "cm", "mm"), 
       dpi = 240)


###End of code

