library(plyr)
a <- read.table(file="RRBLUP_100_75_4922.txt", header=F)
b <- read.table(file="RRBLUP_100_75.txt", header=F)

a<-a[,-1]
b<-b[,-1]


colnames(a)<-c("Poids", "Pression","Hues","Huefond","Lchair","ethylene","IR","AT","Glucose","Fructose","Malique")
colnames(b)<-c("Poids", "Pression","Hues","Huefond","Lchair","ethylene","IR","AT","Glucose","Fructose","Malique")

accuracy<-cbind(a,b)
head(accuracy)
dim(accuracy)#100  22
#Poids
accuracy_Poids<-accuracy[,c(1,12)]
colnames(accuracy_Poids)<- c("Poids","Poids")
#Pression
accuracy_Pression<-accuracy[,c(2,13)]
colnames(accuracy_Pression)<- c("Pression","Pression")
#Hues
accuracy_Hues<-accuracy[,c(3,14)]
colnames(accuracy_Hues)<- c("Hues","Hues")
#Huefond
accuracy_Huefond<-accuracy[,c(4,15)]
colnames(accuracy_Huefond)<- c("Huefond","Huefond")
#Lchair
accuracy_Lchair<-accuracy[,c(5,16)]
colnames(accuracy_Lchair)<- c("Lchair","Lchair")
#ethylene
accuracy_ethylene<-accuracy[,c(6,17)]
colnames(accuracy_ethylene)<- c("Ethylène","Ethylène")
#IR
accuracy_IR<-accuracy[,c(7,18)]
colnames(accuracy_IR)<- c("IR","IR")
#AT
accuracy_AT<-accuracy[,c(8,19)]
colnames(accuracy_AT)<- c("AT","AT")
#Glucose
accuracy_Glucose<-accuracy[,c(9,20)]
colnames(accuracy_Glucose)<- c("Glucose","Glucose")
#Fructose
accuracy_Fructose<-accuracy[,c(10,21)]
colnames(accuracy_Fructose)<- c("Fructose","Fructose")
#Malique
accuracy_Malique<-accuracy[,c(11,22)]
colnames(accuracy_Malique)<- c("Malique","Malique")
acc<-cbind(accuracy_Poids,accuracy_Pression,accuracy_Hues,accuracy_Huefond,accuracy_Lchair,accuracy_ethylene,accuracy_IR,accuracy_AT,accuracy_Glucose,accuracy_Fructose,accuracy_Malique)
dim(acc)#100  22

pdf("Comparaison_nbre_M.pdf", height=10,width=10) 
names(acc)<-c("Poids du fruit", "Poids du fruit","Pression", "Pression", "Hue_s","Hue_s", "Hue_fond","Hue_fond","L_chair","L_chair","Ethylène","Ethylène","IR","IR","AT","AT","Glucose","Glucose","Fructose","Fructose","Acide malique","Acide malique")
colors=c("#99CC66","#FF6666")
boxplot(acc, ylab="Précision de la prédiction", col=colors,las=3,cex.axis=0.7)

legend("bottomright", legend = c("Nombre de marqueurs","4922 SNP","61030 SNP"), col = c("#FFFFFF","#99CC66","#FF6666"), 
       pch = 15, bty = "M", pt.cex = 1, cex = 0.8, text.col = "#000000", horiz = FALSE, inset = c(0.02, 0.02))

dev.off() 

