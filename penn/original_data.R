##Recreation of the original 'Success with style' experiment using original data with downloads and books from 2013-10-24

##Hypotheses

# H1
# H0: There's no difference in the distribution of the proportion of PoS tags in successful and unsuccessful books, regardless of the book's genre.
# HA: There is a difference in the distribution of the proportion of PoS tags in successful and unsuccessful books, and the pattern will depend on a book's genre.  

# H2
# H0: There's no difference in the Flesch-Kincaid readability of successful and unsuccessful books, regardless of the book's genre.   
# HA: There is a difference in the Flesch-Kincaid readability of successful and unsuccessful books, and the pattern will depend on a book's genre.  


#libraries
library(tidyverse); library(ggpubr); library(readxl);library(caret);library(caretEnsemble);library(Hmisc);library(bestNormalize);library(data.table)

#setwd if needed
#setwd("/")

#load proportional data
df<- read_excel("readability_data.xlsx",sheet = 1) #load proportional data
names(df)<- tolower(names(df))
df<- df %>% rename(FR=`fleschkincaid readability`,mSyWo=`mean-syllables-per-word`, mWoSen=`mean-words-per-sentence`)


#load absolute (original) data
ef<- read_excel("readability_data.xlsx",sheet = 2) #load absolute original data
names(ef)<- tolower(names(ef))


#setwd if needed
#setwd("/")

##Draw by each 10


#near zero
zero.vars<- names(df)[nearZeroVar(df)] #variables that are zero
df<- df %>% select(-one_of(zero.vars)) %>% mutate(ranking=factor(ranking))

zero.vars.ef<- names(ef)[nearZeroVar(ef)] #variables that are zero
ef<- ef %>% select(-one_of(zero.vars.ef)) %>% mutate(ranking=factor(ranking))


#split data
set.seed(424)
index<- createDataPartition(y=df$ranking, p=0.8,list = F)
train<- df[index,] #create train
test<- df[-index,] #create test


x<- train %>% select(cc:rrb) 
y<- test %>% select(cc:rrb)

p<- sapply(x,function(x){sum(x==0)}) #check number of zeros
vars<- names(which(p>50))
x %>% select(one_of(vars)) %>% gather(key,value) %>% gghistogram("value",facet.by = "key",scales="free",ggtheme = theme_minimal())

x %>% select(-one_of(vars)) %>% gather(key,value) %>% gghistogram("value",facet.by = "key",scales="free",ggtheme = theme_minimal())


#transform vars
cont<- x %>% select(-one_of(vars))
for(i in colnames(cont)){
  f1<- bestNormalize(cont[[i]])
  cont[[i]]<- predict(f1,cont[[i]])
}

#t.test for cont vars
px<- data.frame(matrix(NA,nrow=ncol(cont),ncol=3,dimnames = list(c(NULL),c("Var","P","P.A"))))
px$Var<- names(cont)
for(i in seq_along(names(cont))){
  px[i,2]<- round(t.test(cont[[i]]~train$ranking)$p.value,3)
}
px$P.A<- round(p.adjust(px$P,method = "fdr"),3)

t1<- px %>% filter(P.A<0.1)
write.csv(t1,"t1.csv") #write to excel



#continous vars transformation
cont %>%  gather(key,value) %>% gghistogram("value",facet.by = "key",scales="free",ggtheme = theme_minimal())

##non normal vars
cat<- x %>% select(one_of(vars))

#Chi.sq
px2<- data.frame(matrix(NA,nrow=ncol(cat),ncol=3,dimnames = list(c(NULL),c("Var","P","P.A"))))
px2$Var<- names(cat)
for(i in seq_along(names(cat))){
  px2[i,2]<- round(t.test(cat[[i]]~train$ranking)$p.value,3)
}
px2$P.A<- round(p.adjust(px2$P,method = "fdr"),3)

t2<- px2 %>% filter(P.A<0.1)
write.csv(t2,"t2.csv")

used<- c(t1$Var,t2$Var) #vars to use

ndf<- data.frame(cont,cat) #combine good vars

#scale and plot FR,mWoSen,mSyWo
train %>% select(ranking,mWoSen,mSyWo,FR) %>% mutate_at(vars(mWoSen:FR),log) %>% mutate_at(vars(mWoSen:FR),scale) %>%  gather(key,value,-ranking) %>% 
  ggboxplot("key","value",fill="ranking",ggtheme = theme_minimal(),palette = "jco",legend="top",xlab="") +stat_compare_means(aes(group=ranking),method = "t.test")


ggsave("Readability, mean words per sentence and mean syllables per word - original data.png", 
       plot = last_plot(), 
       width = 260, height = 171, 
       units = "mm", # other options c("in", "cm", "mm"), 
       dpi = 240)




ndf2<- train %>% select(FR,mWoSen,mSyWo) %>% mutate_all(log) %>%  mutate_all(function(x){
  f1<- bestNormalize(x)
  x<- predict(f1,x)
})

ndf<- data.frame(ndf,ndf2)  #Final data


##########################################################################################################
#Machine learning
tr.ctrl<- trainControl(method = "cv",number = 10)

train.x<- train %>% select(one_of(names(ndf)))
test.x<- test %>% select(one_of(names(ndf)))


##Apply same transformations to train and test data
for(i in seq_along(names(train.x))){
  f1<-bestNormalize(train.x[[i]])
  train.x[[i]]<- predict(f1, train.x[[i]])
  test.x[[i]]<- predict(f1, test.x[[i]])
  
}

fit<- caret::train(y=train$ranking,x=train.x,trControl=tr.ctrl,method="svmLinear3",tuneLength=10)
plot(fit)
tx<- predict(fit,newdata=test.x)
confusionMatrix(tx,test$ranking)

fit2<- caret::train(y=train$ranking,x=train.x,trControl=tr.ctrl,method="glmboost",tuneLength=10)
x<- varImp(fit2)#relactive variable importance
write.csv(x$importance,"x.csv")

#scale and plot
# adapted as first version didn't use absolute data

#original (proportional data) analysis
s.df<- df %>% select(cc:rrb) %>% mutate(rank=df$ranking,genre=df$genre) #imps

p.df<- s.df %>% group_by(genre,rank) %>% summarise_all(sum)  %>% summarise_at(vars(cc:rrb),function(x){x[2]-x[1]}) #sum

write.csv(p.df,"original_tag_output.csv")

p.df<- p.df %>% gather(POS,diff,-genre) 

#absolute data - only using tags used in original team's work

ff <- ef  %>% mutate(status=ef$ranking,genre=ef$genre) %>% select(genre, status, rb, wrb, inn, to, vb, vbd, vbg, vbn, vbp, vbz, wp, cc, cd, det, fw, jj, ls, md, nn, nns, prp, prps)

ff2 <- ff %>% group_by(genre,status) %>% summarise_all(sum)

ff3 <- ff2 %>%  as.data.frame()

ff4 <- ff3 %>% mutate(rowSum = rowSums(.[,names(ff3)[3:24]])) %>% 
  group_by(genre) %>% 
  summarise_at(vars(names(ff3)[3:24]),   
               funs(net = .[status == "SUCCESS"]/rowSum[status == "SUCCESS"] - 
                          .[status == "FAILURE"]/rowSum[status == "FAILURE"] )) %>%
  as.data.frame()

write.csv(ff4,"ff4.csv")

ff5<- ff4 %>% gather(POS,diff,-genre) 

#absolute data - using all tags

gg <- ef  %>% mutate(status=ef$ranking,genre=ef$genre) %>% select(genre, status, cc:rrb)

gg2 <- gg %>% group_by(genre,status) %>% summarise_all(sum)

gg3 <- gg2 %>%  as.data.frame()

gg4 <- gg3 %>% mutate(rowSum = rowSums(.[,names(gg3)[3:43]])) %>% 
  group_by(genre) %>% 
  summarise_at(vars(names(gg3)[3:43]),   
               funs(net = .[status == "SUCCESS"]/rowSum[status == "SUCCESS"] - 
                      .[status == "FAILURE"]/rowSum[status == "FAILURE"] )) %>%
  as.data.frame()

write.csv(gg4,"gg4.csv")

gg5<- gg4 %>% gather(POS,diff,-genre) 

##plots###############################

##ff dataframe

ggbarplot("POS","diff",data=ff5[1:80,],fill="genre",legend="right",palette = "jco", position=position_dodge(),ggtheme = theme_minimal(),ylab = "Difference in proportion (selected tags)")

ggsave("Difference in proportion (original tags) rb-vbz - original data.png", 
       plot = last_plot(), 
       width = 260, height = 171, 
       units = "mm", # other options c("in", "cm", "mm"), 
       dpi = 240)

ggbarplot("POS","diff",data=ff5[81:168,],fill="genre",legend="right",palette = "jco", position=position_dodge(),ggtheme = theme_minimal(),ylab = "Difference in proportion (selected tags)")


ggsave("Difference in proportion (original tags) wp-prp - original data.png", 
       plot = last_plot(), 
       width = 260, height = 171, 
       units = "mm", # other options c("in", "cm", "mm"), 
       dpi = 240)


##gg dataframe (ggbarplot sometimes crashes here but re-runnning it solves it)

ggbarplot("POS","diff",data=gg5[1:80,],fill="genre",legend="right",palette = "jco", position=position_dodge(),ggtheme = theme_minimal(),ylab = "Difference in proportion (all tags)") 


ggsave("Difference in proportion (all tags) cc-ls - original data.png", 
       plot = last_plot(), 
       width = 260, height = 171, 
       units = "mm", # other options c("in", "cm", "mm"), 
       dpi = 240)

ggbarplot("POS","diff",data=gg5[81:160,],fill="genre",legend="right",palette = "jco", position=position_dodge(),ggtheme = theme_minimal(),ylab = "Difference in proportion (all tags)")

ggsave("Difference in proportion (all tags) md-rbs - original data.png", 
       plot = last_plot(), 
       width = 260, height = 171, 
       units = "mm", # other options c("in", "cm", "mm"), 
       dpi = 240)

ggbarplot("POS","diff",data=gg5[161:240,],fill="genre",legend="right",palette = "jco", position=position_dodge(),ggtheme = theme_minimal(),ylab = "Difference in proportion (all tags)")

ggsave("Difference in proportion (all tags) sym-wdt - original data.png", 
       plot = last_plot(), 
       width = 260, height = 171, 
       units = "mm", # other options c("in", "cm", "mm"), 
       dpi = 240)


ggbarplot("POS","diff",data=gg5[241:264,],fill="genre",legend="right",palette = "jco", position=position_dodge(),ggtheme = theme_minimal(),ylab = "Difference in proportion (all tags)")


ggsave("Difference in proportion (all tags) wp-wrb - original data.png", 
       plot = last_plot(), 
       width = 260, height = 171, 
       units = "mm", # other options c("in", "cm", "mm"), 
       dpi = 240)



##########################################

s.df %>% group_by(genre,rank) %>% summarize_all(sum)

#diff between ranks overall
#scale and plot

#proportional data

s.df<- df %>% select(cc:rrb) %>% mutate(rank=df$ranking,genre=df$genre) #imps

p.df<- s.df %>% group_by(rank) %>% summarise_at(vars(cc:rrb),mean)  %>% summarise_at(vars(cc:rrb),function(x){x[2]-x[1]}) #sum

p.df<- p.df %>% gather(POS,diff) %>% arrange(desc(diff))
write.csv(p.df,"p.df.csv")

#absolute - original team's tags

ff6 <- setDT(ff)[,lapply(.SD,sum),status,.SDcols=3:24][,
                    .SD/rowSums(.SD),.SDcols=-1][,rbind(.SD,.SD[1]-.SD[2])]

ff7 <- ff6[-c(1,2),]


ff8<- ff7 %>% gather(POS,diff)  %>% arrange(desc(diff))
write.csv(ff8,"ff8.csv")

#absolute - all tags

gg6 <- setDT(gg)[,lapply(.SD,sum),status,.SDcols=3:43][,
                                                       .SD/rowSums(.SD),.SDcols=-1][,rbind(.SD,.SD[1]-.SD[2])]

gg7 <- gg6[-c(1,2),]

gg8<- gg7 %>% gather(POS,diff)  %>% arrange(desc(diff))
write.csv(gg8,"gg8.csv")


###Readability
train<- train %>% mutate(l.FR=log(FR))

ggdensity("mWoSen",fill="ranking",data=train)

df %>% select(ranking,mWoSen,mSyWo,FR) %>% mutate_at(vars(mWoSen:FR),log) %>% gather(index,value,-ranking) %>% 
  mutate(ranking=tolower(ranking),ranking=capitalize(ranking)) %>%
  ggboxplot("ranking","value",facet.by="index",ggtheme = theme_minimal(),scales="free") +stat_compare_means(method="t.test" ,label = "p.format")

ggboxplot("ranking", "mSyWo",data=train)
ggdensity("mSyWo",fill="ranking",data=df)

t.test(mSyWo~ranking,data=train) #ns
t.test(mWoSen~ranking,data=train) #ns
t.test(FR~ranking,data=train) #ns

#####FR by genre
dx<- data.frame(rank=df$ranking, genre=df$genre, FR=df$FR)

library(reshape2)
fx<- dx %>% group_by(genre,rank) %>% summarise(m=mean(FR),s=sd(FR)) %>% gather(key,value,c(m,s)) %>% 
  dcast(genre~rank+key,value.var = "value")
write.csv(fx,"fx.csv")

fx<- dx %>% mutate_at(vars(FR), function(x){
  f1<- bestNormalize(x)
  x<- predict(f1,x)
})

ggboxplot("genre","FR",fill="rank",data=fx,ggtheme = theme_minimal(),palette = "jco",legend="top",xlab = "Genre") +
  stat_compare_means(aes(group=rank,label = ..p.format..),method = "t.test")


ggsave("Readability by genre - original data.png", 
       plot = last_plot(), 
       width = 260, height = 171, 
       units = "mm", # other options c("in", "cm", "mm"), 
       dpi = 240)


#end of code

