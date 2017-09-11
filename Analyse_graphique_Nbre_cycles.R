library(plyr)

a <- read.table(file="RRBLUP_75_1000.txt", header=F)
b <- read.table(file="RRBLUP_75_900.txt", header=F)
c <- read.table(file="RRBLUP_75_800.txt", header=F)
d <- read.table(file="RRBLUP_75_700.txt", header=F)
e <- read.table(file="RRBLUP_75_600.txt", header=F)
f <- read.table(file="RRBLUP_75_500.txt", header=F)
g <- read.table(file="RRBLUP_75_400.txt", header=F)
h <- read.table(file="RRBLUP_75_300.txt", header=F)
i <- read.table(file="RRBLUP_75_200.txt", header=F)
j <- read.table(file="RRBLUP_75_100.txt", header=F)
k <- read.table(file="RRBLUP_75_50.txt", header=F)
l <- read.table(file="RRBLUP_75_20.txt", header=F)
m <- read.table(file="RRBLUP_75_10.txt", header=F)

a<-a[,c(2,12)]
b<-b[,c(2,12)]
c<-c[,c(2,12)]
d<-d[,c(2,12)]
e<-e[,c(2,12)]
f<-f[,c(2,12)]
g<-g[,c(2,12)]
h<-h[,c(2,12)]
i<-i[,c(2,12)]
j<-j[,c(2,12)]
k<-k[,c(2,12)]
l<-l[,c(2,12)]
m<-m[,c(2,12)]

colnames(a)<-c("Poids_1000","Malique_1000")
colnames(b)<-c("Poids_900","Malique_900")
colnames(c)<-c("Poids_800","Malique_800")
colnames(d)<-c("Poids_700","Malique_700")
colnames(e)<-c("Poids_600","Malique_600")
colnames(f)<-c("Poids_500","Malique_500")
colnames(g)<-c("Poids_400","Malique_400")
colnames(h)<-c("Poids_300","Malique_300")
colnames(i)<-c("Poids_200","Malique_200")
colnames(j)<-c("Poids_100","Malique_100")
colnames(k)<-c("Poids_50","Malique_50")
colnames(l)<-c("Poids_20","Malique_20")
colnames(m)<-c("Poids_10","Malique_10")

accuracy<-rbind.fill(m,l,k,j,i,h,g,f,e,d,c,b,a)

head(accuracy)

dim(accuracy)#26
accuracy_Poids<-accuracy[,c(1,3,5,7,9,11,13,15,17,19,21, 23, 25)]
colnames(accuracy_Poids)<- c(10,20,50,100,200,300,400,500, 600, 700, 800, 900, 1000)
accuracy_Malique<-accuracy[,c(2,4,6,8,10,12,14,16,18,20,22, 24, 26)]
colnames(accuracy_Malique)<- c(10,20,50,100,200,300,400,500, 600, 700, 800, 900, 1000)

#colors=c(rep("#FF9999",13))
colors=c(rep("salmon",13))
boxplot(accuracy_Poids,xlab="Nombre de cycles", ylab="Précision de la prédiction", main="Poids du fruit",
        col=colors,las=1,cex.axis=0.8,ylim=c(0,1))
boxplot(accuracy_Malique,xlab="Nombre de cycles", ylab="Précision de la prédiction", main="Acide Malique",
        col=colors,las=1,cex.axis=0.8,ylim=c(0,1))


############################################################
#################### TESTS STATS ###########################
############################################################
#Acide malique
#Rassemblement des Accuracies dans le même vecteur 
Accuracy<-c(m[,2], l[,2], k[,2],j[,2],i[,2],h[,2],g[,2],f[,2],e[,2],d[,2],c[,2],b[,2],a[,2])
length(Accuracy)

# Colonne aves le nom des paramètres
vector_a<-rep("10",10)
vector_b<-rep("20",20)
vector_c<-rep("50",50)
vector_d<-rep("100",100)
vector_e<-rep("200",200)
vector_f<-rep("300",300)
vector_g<-rep("400",400)
vector_h<-rep("500",500)
vector_i<-rep("600",600)
vector_j<-rep("700",700)
vector_k<-rep("800",800)
vector_l<-rep("900",900)
vector_m<-rep("1000",1000)

Parametres<-c(vector_a,vector_b,vector_c,vector_d,vector_e,vector_f,vector_g,vector_h,vector_i,vector_j,vector_k,vector_l,vector_m)
length(Parametres)

# Créer le dataframe

dataframe <- data.frame(Parametres = Parametres, Accuracy=Accuracy)
dataframe[1:5,1:2]

############ TUKEY ###########################
a1 <- aov(dataframe$Accuracy ~ dataframe$Parametres)
TukeyHSD_Parametres <- TukeyHSD(x=a1, 'dataframe$Parametres', conf.level=0.95)

test <- lm(dataframe$Accuracy ~ dataframe$Parametres)
library(agricolae)
HSD_para<-HSD.test(test, 'dataframe$Parametres')
HSD_para
########### TEST STATS ##########################
tests = matrix(nrow=4, ncol=13)

for(o in c(1:13)){

if (o==1) z=a else 
  if (o==2) z=b else
    if (o==3) z=c else
      if (o==4) z=d else
        if (o==5) z=e else
          if (o==6) z=f else
            if (o==7) z=g else
              if (o==8) z=h else
                if (o==9) z=i else
                  if (o==11) z=k else 
                    if (o==12) z=l else 
                     if (o==13) z=m
                  
t.test_Poids<-t.test(z[,1],j[,1])
ks.test_Poids<-ks.test(z[,1],j[,1])
t.test_Malique<-t.test(z[,2],j[,2])
ks.test_Malique<-ks.test(z[,2],j[,2])


tests[1,o]<-t.test_Poids$p.value
tests[2,o]<-ks.test_Poids$p.value 
tests[3,o]<-t.test_Malique$p.value
tests[4,o]<-ks.test_Malique$p.value 
}
colnames(tests)<-c("1000","900","800","700","600","500","400","300","200", "100", "50", "20","10")
rownames(tests)<-c("t.test_Poids","ks.test_Poids","t.test_Malique","ks.test_Malique")
write.table(tests, "Tests nb de cycles (t test and ks test).txt", sep=" ", quote=F, col.names=T, row.names=T)
write.csv2(tests, "Tests nb de cycles (t test and ks test).csv")
