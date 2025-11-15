library(readxl)
df<-read_excel('C:/Users/Oona/Documents/Tpc/usr/us_dois1.xlsx')
head(df)
dim(df)
sum(!is.na(df$title_exclusion))
sum(!is.na(df$abstract_exclusion))
sum(!is.na(df$other_exclusion))
sum(!is.na(df$target))
211+116+15+328
table(df$title_exclusion)
table(df$abstract_exclusion)
table(df$other_exclusion)

c(1:670)[!is.na(df$title_exclusion) & !is.na(df$abstract_exclusion)]
c(1:670)[is.na(df$title_exclusion) & is.na(df$abstract_exclusion) & is.na(df$other_exclusion) & is.na(df$target)]
df$target[312]

table(df$target)
dim(table(df$target))
for(i in 1:199){
  if(as.data.frame(table(df$target))$Freq[i]>3){
    print(as.data.frame(table(df$target))$Var1[i])
  }
}

dim(table(df$method))
for(i in 1:165){
  if(as.data.frame(table(df$method))$Freq[i]>3){
    print(as.data.frame(table(df$method))$Var1[i])
  }
}

1%in%c(1,2,3) 

df<-read_excel('C:/Users/Oona/Documents/Tpc/usr/us_final.xlsx')
dim(df)
table(df$category)
table(df$category1)
table(df[df$category1=='organ',]$target)
table(df[df$category1=='cardiac',]$category)
table(df[df$category1=='vascular',]$target)
table(df[df$category1=='obstetrics',]$category)
table(df[df$category1=='orthopedics',]$category)
table(df[df$category1=='oncology',]$target)
table(df[df$category1=='gynecological',]$target)
table(df[df$category1=='other',]$target)

table(df$method_type)
table(df$architecture)
table(df[df$method_type=='cnn',]$architecture)
#mod u-net(93), w.u(6), other(9)
#u-net(23), nnu-net(12), attention u-net(3), resunet(4), other u(2),
#deeplabv3(2), 3+(3), fcn(3), other(6)


table(df[df$method_type=='cnn',]$category1)

table(df$category1)
table(df[df$method_type=='cnn',]$category1)
y0<-c(65+9,29,6+9,28,15+9,32,26,31,17+9,29)
y1<-c(30+4,12,4+7,20,12+7,17,18,24,7+4,20)
chisq.test(rbind(y0-y1,y1))
y1/y0

#modified 112
#mod u-net 95, mod deeplabv3/+ 3, mod. w.e. 7, other 7 
#established 60
#u-net 24, nn-unet 12, deeplabv3/+ 5, u-net w.e. 10, fcn 3, other 6

table(df$method_type)
table(df[df$method_type=='transformer',]$architecture)

table(df[df$method_type=='diffusion model',]$target)
table(df[df$method_type=='gan',]$target)

table(df[df$method_type=='transformer',]$target)

table(df[df$method_type=='cnn/transformer hybrid',]$category1)
table(df$category1)

table(df[df$method_type=='segment anything',]$architecture)
table(df[df$method_type=='segment anything',]$category1)
table(df$method_type1)
table(df[df$method_type1=='generative model',]$method_type)
table(df[df$method_type1=='generative model',]$target)

table(df$imaging1)
table(df[df$imaging1=='video',]$method_type)

table(df[df$imaging1=='intraoral',]$target)

table(df$data)
table(df$data1)

table(df$data1)
table(df[df$data1=='public',]$data)
table(df[df$data1=='public',]$category1)
table(df[df$data1=='both',]$category1)
table(df[df$data1=='private',]$category1)
54/(54+11+2)

y0<-c(65+9,29,6+9,28,15+9,32,26,31,17+9,29)
y1<-c(13+1,7,5+6,19,11+6,20,23,23,7+1,23)
chisq.test(rbind(y0-y1,y1))
(y0-y1)/y1
(y0-y1)/y0

length(na.omit(df$number))
length(na.omit(df$dice))

wilcox.test(na.omit(as.numeric(df[df$method_type1=='cnn',]$number)),
            na.omit(as.numeric(df[df$method_type1=='sam',]$number)),paired=FALSE)
wilcox.test(na.omit(as.numeric(df[df$method_type1=='cnn',]$number)),
            na.omit(as.numeric(df[df$method_type1=='vit' | df$method_type1=='cnn/vit',]$number)),paired=FALSE)
wilcox.test(na.omit(as.numeric(df[df$method_type=='cnn',]$number)),
            na.omit(as.numeric(df[df$method_type=='transformer' | df$method_type=='cnn/transformer hybrid',]$number)),paired=FALSE)

y<-na.omit(as.numeric(df$number))
min(y)
max(y)
mean(y)
median(y)
sd(y)

y<-na.omit(as.numeric(df$dice))
length(y)
min(y)
max(y)
mean(y)
median(y)
sd(y)

mean(na.omit(df[df$data1=='public',]$number))
mean(na.omit(df[df$data1=='private',]$number))
wilcox.test(na.omit(df[df$data1=='public',]$number),
            na.omit(df[df$data1=='private',]$number),paired=FALSE)
wilcox.test(na.omit(as.numeric(df[df$method_type=='cnn',]$number)),
            na.omit(as.numeric(df[df$method_type=='segment anything',]$number)),paired=FALSE)
wilcox.test(na.omit(as.numeric(df[df$method_type=='cnn',]$number)),
            na.omit(as.numeric(df[df$method_type=='transformer' | df$method_type=='cnn/transformer hybrid',]$number)),paired=FALSE)

wilcox.test(na.omit(as.numeric(df[df$data1=='public',]$dice)),
            na.omit(as.numeric(df[df$data1=='private',]$dice)),paired=FALSE)
wilcox.test(na.omit(as.numeric(df[df$method_type=='cnn',]$dice)),
            na.omit(as.numeric(df[df$method_type=='transformer' | df$method_type=='cnn/transformer hybrid',]$dice)),paired=FALSE)
wilcox.test(na.omit(as.numeric(df[df$method_type=='cnn',]$dice)),
            na.omit(as.numeric(df[df$method_type=='segment anything',]$dice)),paired=FALSE)
cor.test(as.numeric(df$number),as.numeric(df$dice),method='spearman')

table(df$category1)
cus<-c('breast tumors','organ segmentation','cardiology',
       'angiology','obstetrics','orthopedics','thyroid nodules',
       'oncology','gynecology','other')
num<-c(65+9,32,29,29,28,26,17+9,15+9,6+9,31)
par(mar=c(5,10,5,3))
barplot(rev(num),horiz=TRUE,names.arg=rev(cus),
        col='blue',las=1,
        main='Number of studies in different
        application categories',
        xlab='Number of studies')
table(df$method_type1)
cty<-c('CNN','ViT','CNN/ViT hybrid','SAM','generative model',
       'other')
num1<-c(175,15,29,20,10,47)
col1<-c('skyblue','lavender','cyan','black','blue','gray')
barplot(rev(num1),horiz=TRUE,names.arg=rev(cty),
        col=rev(col1),las=1,
        main='Number of studies in different 
        method categories',
        xlab='Number of studies')
tabl<-table(df$method_type1,df$category1)

c1<-c(30+4,1,10+2,7+1,1,16+2)
c2<-c(17,0,5,4,3,3)
c3<-c(12,2,2,3,2,8)
c4<-c(20,1,2,0,3,3)
c5<-c(20,1,1,2,0,4)
c6<-c(18,2,2,1,1,2)
c7<-c(7+4,4,0+2,0+1,0,6+2)
c8<-c(12+7,2+1,1,0,0,0+1)
c9<-c(4+7,0+1,0,1,0,1+1)
c10<-c(24,1,4,1,0,1)
tabl<-100*cbind(c10/sum(c10),c9/sum(c9),c8/sum(c8),c7/sum(c7),c6/sum(c6),
            c5/sum(c5),c4/sum(c4),c3/sum(c3),c2/sum(c2),c1/sum(c1))

par(mar=c(5,10,5,3))
barplot(tabl,col=col1,names.arg=rev(cus),horiz=TRUE,las=1,
        main='Method distribution within application 
        categories',
        xlab='Percentage of studies (%)')
plot(1,type='n')
legend('center',legend=cty,pch=c(15,15,15,15,15,15),col=col1)

tabl<-table(df$data1,df$category1)
c1<-c(13+1,44+7,8+1)
c2<-c(20,9+1,2)
c3<-c(7,17,5)
c4<-c(23,5,1)
c5<-c(19,9,0)
c6<-c(23,3,0)
c7<-c(7+1,9+7,1+1)
c8<-c(11+6,2+1+3,1)
c9<-c(5+6,1+3,0)
c10<-c(23,6,2)
tabl<-100*cbind(c10/sum(c10),c9/sum(c9),c8/sum(c8),c7/sum(c7),c6/sum(c6),
                c5/sum(c5),c4/sum(c4),c3/sum(c3),c2/sum(c2),c1/sum(c1))

par(mar=c(5,10,5,3))
barplot(tabl,col=c('darkblue','cyan','lavender'),names.arg=rev(cus),horiz=TRUE,las=1,
        main='Distribution of private and public data
        within application categories',
        xlab='Percentage of studies (%)')
plot(1,type='n')
legend('center',legend=c('private','public','both'),
  pch=c(15,15,15),col=c('darkblue','skyblue','lavender'))

hist(na.omit(df$number)[na.omit(df$number)<10000],breaks=50,
     col='steelblue1', main='Number of data instances in the studies',
     xlab='Number of data instances',
     ylab='Number of studies')

table(df$category1)
c1<-c(na.omit(as.numeric(df[df$category1=='breast tumor' | df$category1=='breast tumor and thyroid nodules',]$dice)))
c2<-c(na.omit(as.numeric(df[df$category1=='organ',]$dice)))
c3<-c(na.omit(as.numeric(df[df$category1=='thyroid nodules' | df$category1=='breast tumor and thyroid nodules',]$dice)))
c4<-c(na.omit(as.numeric(df[df$category1=='cardiac',]$dice)))
c5<-c(na.omit(as.numeric(df[df$category1=='vascular',]$dice)))
c6<-c(na.omit(as.numeric(df[df$category1=='orthopedics',]$dice)))
c7<-c(na.omit(as.numeric(df[df$category1=='obstetrics',]$dice)))
c8<-c(na.omit(as.numeric(df[df$category1=='oncology' | df$category1=='gynecological oncology',]$dice)))
c9<-c(na.omit(as.numeric(df[df$category1=='gynecology' | df$category1=='gynecological oncology',]$dice)))
c10<-c(na.omit(as.numeric(df[df$category1=='other',]$dice)))
par(mar=c(10,10,3,5))
boxplot(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,
        names=cus,las=2,
        ylim=c(0,1),col='blue',ylab='Reported mean Dice')

table(df$method_type1)
c1<-c(na.omit(as.numeric(df[df$method_type1=='cnn',]$dice)))
c2<-c(na.omit(as.numeric(df[df$method_type1=='vit',]$dice)))
c3<-c(na.omit(as.numeric(df[df$method_type1=='cnn/vit',]$dice)))
c4<-c(na.omit(as.numeric(df[df$method_type1=='sam',]$dice)))
c5<-c(na.omit(as.numeric(df[df$method_type1=='generative model',]$dice)))
c6<-c(na.omit(as.numeric(df[df$method_type1=='other',]$dice)))
par(mar=c(10,10,3,5))
boxplot(c1,c2,c3,c4,c5,c6,
        names=cty,las=2,
        ylim=c(0,1),col=col1,ylab='Reported mean Dice')

plot(1,type='n',ylim=c(0,1),xlim=c(0,10000),
     ylab='Reported mean Dice',xlab='Number of data instances')
points(df[df$method_type1=='other',]$number[df[df$method_type1=='other',]$number<10000],
       as.numeric(df[df$method_type1=='other',]$dice)[df[df$method_type1=='other',]$number<10000],
       pch=3,col=col1[6],lwd=2)
points(df[df$method_type1=='cnn',]$number[df[df$method_type1=='cnn',]$number<10000],
       as.numeric(df[df$method_type1=='cnn',]$dice)[df[df$method_type1=='cnn',]$number<10000],
       pch=3,col=col1[1],lwd=2)
points(df[df$method_type1=='vit',]$number[df[df$method_type1=='vit',]$number<10000],
       as.numeric(df[df$method_type1=='vit',]$dice)[df[df$method_type1=='vit',]$number<10000],
       pch=3,col=col1[2],lwd=2)
points(df[df$method_type1=='cnn/vit',]$number[df[df$method_type1=='cnn/vit',]$number<10000],
       as.numeric(df[df$method_type1=='cnn/vit',]$dice)[df[df$method_type1=='cnn/vit',]$number<10000],
       pch=3,col=col1[3],lwd=2)
points(df[df$method_type1=='sam',]$number[df[df$method_type1=='sam',]$number<10000],
       as.numeric(df[df$method_type1=='sam',]$dice)[df[df$method_type1=='sam',]$number<10000],
       pch=3,col=col1[4],lwd=2)
points(df[df$method_type1=='generative model',]$number[df[df$method_type1=='generative model',]$number<10000],
       as.numeric(df[df$method_type1=='generative model',]$dice)[df[df$method_type1=='generative model',]$number<10000],
       pch=3,col=col1[5],lwd=2)
plot(1,type='n')
legend('center',legend=cty,pch=c(3,3,3,3,3,3),lty=c(0,0,0,0,0,0),
       col=col1,lwd=c(2,2,2,2,2,2))

