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
table(df[df$category1=='gynecological',]$target)
table(df[df$category1=='other',]$target)
5+3+15+3+10+1+4
table(df[df$category1=='orthopedics',]$category)
#cp4,ao4, b3,pap2, skin, ton

table(df[df$category1=='vascular',]$target)

table(df$method_type)
table(df$architecture)
table(df[df$method_type=='cnn/transformer hybrid',]$category1)

table(df$category1)
table(df[df$method_type=='cnn',]$category1)
y0<-c(68,10,30,6,10,26,15,34,28,36,21,30)
y1<-c(31,4,12,4,7,19,12,18,18,25,10,21)
chisq.test(rbind(y0-y1,y1))
y1/y0

#modified 112
#mod u-net 95, mod deeplabv3/+ 3, mod. w.e. 7, other 7 
#established 60
#u-net 24, nn-unet 12, deeplabv3/+ 5, u-net w.e. 10, fcn 3, other 6

table(df[df$method_type=='diffusion model',]$target)
table(df[df$method_type=='gan',]$target)

table(df[df$method_type=='transformer',]$target)

table(df[df$method_type=='cnn/transformer hybrid',]$category1)
table(df$category1)
y0<-c(68,10,30,6,10,26,16,34,29,38,21,30)
y1<-c(10,2,2,0,0,1,1,5,2,5,0,2)
y1/y0
chisq.test(rbind(y0,y1))

table(df[df$method_type=='segment anything',]$architecture)

table(df$imaging1)
table(df[df$imaging1=='video',]$method_type)

table(df[df$imaging1=='intraoral',]$target)

table(df$data)
table(df$data1)

y0<-c(271,30,17)
y1<-c(156,18,10)
chisq.test(rbind(y0-y1,y1))
y1/y0

table(df$data1)
table(df[df$data1=='public',]$data)
table(df[df$data1=='public',]$category1)
table(df[df$data1=='both',]$category1)
table(df[df$data1=='private',]$category1)
54/(54+11+2)

y0<-c(47,7,17,1,3,9,2,9,6,7,10,5)
yl<-c(8,1,5,0,0,0,1,2,0,3,2,1)
y1<-c(13,2,8,5,7,17,11,23,22,26,9,24)
chisq.test(rbind(y0+yl,y1))
(y0+yl)/(y0+yl+y1)

(47+7+9)/(47+9+7+15)

length(na.omit(df$number))
length(na.omit(df$dice))

wilcox.test(na.omit(as.numeric(df[df$method_type1=='cnn',]$number)),
            na.omit(as.numeric(df[df$method_type1=='sam',]$number)),paired=FALSE)
wilcox.test(na.omit(as.numeric(df[df$method_type1=='cnn',]$number)),
            na.omit(as.numeric(df[df$method_type1=='vit' | df$method_type1=='cnn/vit',]$number)),paired=FALSE)
wilcox.test(na.omit(as.numeric(df[df$method_type=='cnn',]$number)),
            na.omit(as.numeric(df[df$method_type=='transformer' | df$method_type=='cnn/transformer hybrid',]$number)),paired=FALSE)

y<-na.omit(as.numeric(df$dice))
min(y)
max(y)
mean(y)
median(y)
sd(y)
mean(na.omit(df[df$data1=='public',]$number))
mean(na.omit(df[df$data1=='private',]$number))
wilcox.test(na.omit(df[df$data1=='public',]$number),
            na.omit(df[df$data1=='private',]$number),paired=FALSE)
wilcox.test(na.omit(as.numeric(df[df$data1=='public',]$dice)),
            na.omit(as.numeric(df[df$data1=='private',]$dice)),paired=FALSE)
wilcox.test(na.omit(as.numeric(df[df$method_type=='cnn',]$dice)),
            na.omit(as.numeric(df[df$method_type=='transformer',]$dice)),paired=FALSE)
wilcox.test(na.omit(as.numeric(df[df$method_type=='cnn',]$dice)),
            na.omit(as.numeric(df[df$method_type=='cnn/transformer hybrid',]$dice)),paired=FALSE)
wilcox.test(na.omit(as.numeric(df[df$method_type=='cnn',]$dice)),
            na.omit(as.numeric(df[df$method_type=='segment anything',]$dice)),paired=FALSE)
wilcox.test(na.omit(as.numeric(df[df$method_type=='cnn',]$dice)),
            na.omit(as.numeric(df[df$method_type=='transformer' | df$method_type=='cnn/transformer hybrid',]$dice)),paired=FALSE)
cor.test(as.numeric(df$number),as.numeric(df$dice),method='pearson')

table(df$category1)
cus<-c('breast tumors','organ segmentation','thyroid nodules',
       'cardiology','angiology','orthopedics','obstetrics',
       'oncology','gynecology','other')
num<-c(68+10,34,21+10,30,30,28,26,15+10,10+6,36)
par(mar=c(5,10,5,3))
barplot(rev(num),horiz=TRUE,names.arg=rev(cus),
        col='blue',las=1,
        main='Number of studies in different
        application categories',
        xlab='Number of studies')
table(df$method_type1)
cty<-c('CNN','ViT','CNN/ViT hybrid','SAM','generative model',
       'other')
num1<-c(181,17,29,25,11,51)
col1<-c('skyblue','cyan','lavender','steelblue1','darkblue','gray')
barplot(rev(num1),horiz=TRUE,names.arg=rev(cty),
        col=rev(col1),las=1,
        main='Number of studies in different 
        method categories',
        xlab='Number of studies')
tabl<-table(df$method_type1,df$category1)

c1<-c(31+4,1+0,10+2,8+1,1,17+3)
c2<-c(18,0,5,4,4,3)
c3<-c(10+4,4,2,1,0,7+3)
c4<-c(12,3,2,3,2,8)
c5<-c(21,1,2,0,3,3)
c6<-c(19,0,1,2,0,4)
c7<-c(18,2,2,3,1,2)
c8<-c(7+7,1+1,0,0,0+2,2)
c9<-c(4+7,0+1,0,1,0+2,1)
c10<-c(25,3,4,3,0,1)
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
c1<-c(13+2,47+7,8+1)
c2<-c(22,9+1,2)
c3<-c(9+2,10+7,2+1)
c4<-c(8,17,5)
c5<-c(24,5,1)
c6<-c(22,6,0)
c7<-c(17,9,0)
c8<-c(11+7,2+1+3,1)
c9<-c(7+5,3+1,0)
c10<-c(26,7,3)
tabl<-100*cbind(c10/sum(c10),c9/sum(c9),c8/sum(c8),c7/sum(c7),c6/sum(c6),
                c5/sum(c5),c4/sum(c4),c3/sum(c3),c2/sum(c2),c1/sum(c1))

par(mar=c(5,10,5,3))
barplot(tabl,col=c('darkblue','skyblue','lavender'),names.arg=rev(cus),horiz=TRUE,las=1,
        main='Distribution of private and public data
        within application categories',
        xlab='Percentage of studies (%)')
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

col1<-c('cyan','steelblue','lavender','black','blue','gray')
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
legend('center',legend=cty,pch=c(3,3,3,3,3,3),lty=c(0,0,0,0,0,0),
       col=col1,lwd=c(2,2,2,2,2,2))

