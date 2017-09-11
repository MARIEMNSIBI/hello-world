out_marker_density<- read.table(file="output_rrBLUP_M_density_Xu_traits.txt", header=F)
out_marker_density<-out_marker_density[,-1]
names(out_marker_density) <- c("Markers_number","Poids","Pression","Hues","Huefond","Lchair","ethylene","IR","AT","Glucose","Fructose","Malique")
out_marker_density$Markers_number<-as.factor(out_marker_density$Markers_number)

data<-t(out_marker_density)
data<-data[-1,]
colnames(data)<-c(rep("493",100),rep("983",100),rep("1477",100),rep("1969",100),rep("2461",100),rep("2954",100),rep("3344",100),rep("3938",100),rep("4430",100),rep("4922",100))

a <- out_marker_density[c(1:100),]
b <- out_marker_density[c(101:200),]
c <- out_marker_density[c(201:300),]
d <- out_marker_density[c(301:400),]
e <- out_marker_density[c(401:500),]
f <- out_marker_density[c(501:600),]
g <- out_marker_density[c(601:700),]
h <- out_marker_density[c(701:800),]
k <- out_marker_density[c(801:900),]
j <- out_marker_density[c(901:1000),]

  
accuracy_Poids<-cbind(a[,2],b[,2],c[,2],d[,2],e[,2],f[,2],g[,2],h[,2],k[,2],j[,2])
colnames(accuracy_Poids)<-c("493","983","1477","1969","2461","2954","3344","3938","4430","4922")
accuracy_Pression<-cbind(a[,3],b[,3],c[,3],d[,3],e[,3],f[,3],g[,3],h[,3],k[,3],j[,3])
colnames(accuracy_Pression)<-c("493","983","1477","1969","2461","2954","3344","3938","4430","4922")
accuracy_Hues<-cbind(a[,4],b[,4],c[,4],d[,4],e[,4],f[,4],g[,4],h[,4],k[,4],j[,4])
colnames(accuracy_Hues)<-c("493","983","1477","1969","2461","2954","3344","3938","4430","4922")
accuracy_Huefond<-cbind(a[,5],b[,5],c[,5],d[,5],e[,5],f[,5],g[,5],h[,5],k[,5],j[,5])
colnames(accuracy_Huefond)<-c("493","983","1477","1969","2461","2954","3344","3938","4430","4922")
accuracy_Lchair<-cbind(a[,6],b[,6],c[,6],d[,6],e[,6],f[,6],g[,6],h[,6],k[,6],j[,6])
colnames(accuracy_Lchair)<-c("493","983","1477","1969","2461","2954","3344","3938","4430","4922")
accuracy_ethylene<-cbind(a[,7],b[,7],c[,7],d[,7],e[,7],f[,7],g[,7],h[,7],k[,7],j[,7])
colnames(accuracy_ethylene)<-c("493","983","1477","1969","2461","2954","3344","3938","4430","4922")
accuracy_IR<-cbind(a[,8],b[,8],c[,8],d[,8],e[,8],f[,8],g[,8],h[,8],k[,8],j[,8])
colnames(accuracy_IR)<-c("493","983","1477","1969","2461","2954","3344","3938","4430","4922")
accuracy_AT<-cbind(a[,9],b[,9],c[,9],d[,9],e[,9],f[,9],g[,9],h[,9],k[,9],j[,9])
colnames(accuracy_AT)<-c("493","983","1477","1969","2461","2954","3344","3938","4430","4922")
accuracy_Glucose<-cbind(a[,10],b[,10],c[,10],d[,10],e[,10],f[,10],g[,10],h[,10],k[,10],j[,10])
colnames(accuracy_Glucose)<-c("493","983","1477","1969","2461","2954","3344","3938","4430","4922")
accuracy_Fructose<-cbind(a[,11],b[,11],c[,11],d[,11],e[,11],f[,11],g[,11],h[,11],k[,11],j[,11])
colnames(accuracy_Fructose)<-c("493","983","1477","1969","2461","2954","3344","3938","4430","4922")
accuracy_Malique<-cbind(a[,12],b[,12],c[,12],d[,12],e[,12],f[,12],g[,12],h[,12],k[,12],j[,12])
colnames(accuracy_Malique)<-c("493","983","1477","1969","2461","2954","3344","3938","4430","4922")

pdf("RRBLUP_100_75_NbM.pdf", height=10,width=10) 

colnames(accuracy_Poids)<-c("10%","20%","30%","40%","50%","60%","70%","80%","90%","100%")
A<-boxplot(accuracy_Poids,xlab="Poucentage du nombre de marqueurs utilisés", ylab="Précision de la prédiction", main="Poids",col="salmon",las=1,cex.axis=0.7,ylim=c(0,1))

colnames(accuracy_Pression)<-c("10%","20%","30%","40%","50%","60%","70%","80%","90%","100%")
B<-boxplot(accuracy_Pression,xlab="Poucentage du nombre de marqueurs utilisés", ylab="Précision de la prédiction", main="Pression", col="salmon",las=1,cex.axis=0.7,ylim=c(0,1))

colnames(accuracy_Hues)<-c("10%","20%","30%","40%","50%","60%","70%","80%","90%","100%")
C<-boxplot(accuracy_Hues,xlab="Poucentage du nombre de marqueurs utilisés", ylab="Précision de la prédiction", main="Hue_s", col="salmon",las=1,cex.axis=0.7,ylim=c(0,1))

colnames(accuracy_Huefond)<-c("10%","20%","30%","40%","50%","60%","70%","80%","90%","100%")
D<-boxplot(accuracy_Huefond,xlab="Poucentage du nombre de marqueurs utilisés", ylab="Précision de la prédiction", main="Hue_fond", col="salmon",las=1,cex.axis=0.7,ylim=c(0,1))

colnames(accuracy_Lchair)<-c("10%","20%","30%","40%","50%","60%","70%","80%","90%","100%")
E<-boxplot(accuracy_Lchair,xlab="Poucentage du nombre de marqueurs utilisés", ylab="Précision de la prédiction", main="L_chair", col="salmon",las=1,cex.axis=0.7,ylim=c(0,1))

colnames(accuracy_ethylene)<-c("10%","20%","30%","40%","50%","60%","70%","80%","90%","100%")
F<-boxplot(accuracy_ethylene,xlab="Poucentage du nombre de marqueurs utilisés", ylab="Précision de la prédiction",main="Ethylène",col="salmon",las=1,cex.axis=0.7,ylim=c(0,1))

colnames(accuracy_IR)<-c("10%","20%","30%","40%","50%","60%","70%","80%","90%","100%")
G<-boxplot(accuracy_IR,xlab="Poucentage du nombre de marqueurs utilisés", ylab="Précision de la prédiction",main="IR", col="salmon",las=1,cex.axis=0.7,ylim=c(0,1))

colnames(accuracy_AT)<-c("10%","20%","30%","40%","50%","60%","70%","80%","90%","100%")
H<-boxplot(accuracy_AT,xlab="Poucentage du nombre de marqueurs utilisés", ylab="Précision de la prédiction",main="AT", col="salmon",las=1,cex.axis=0.7,ylim=c(0,1))

colnames(accuracy_Glucose)<-c("10%","20%","30%","40%","50%","60%","70%","80%","90%","100%")
I<-boxplot(accuracy_Glucose,xlab="Poucentage du nombre de marqueurs utilisés", ylab="Précision de la prédiction",main="Glucose", col="salmon",las=1,cex.axis=0.7,ylim=c(0,1))

colnames(accuracy_Fructose)<-c("10%","20%","30%","40%","50%","60%","70%","80%","90%","100%")
J<-boxplot(accuracy_Fructose,xlab="Poucentage du nombre de marqueurs utilisés", ylab="Précision de la prédiction",main="Fructose", col="salmon",las=1,cex.axis=0.7,ylim=c(0,1))

colnames(accuracy_Malique)<-c("10%","20%","30%","40%","50%","60%","70%","80%","90%","100%")
K<-boxplot(accuracy_Malique,xlab="Poucentage du nombre de marqueurs utilisés", ylab="Précision de la prédiction",main="Malique", col="salmon",las=1,cex.axis=0.7,ylim=c(0,1))
dev.off()
par(mfrow=c(2,2))



D<-boxplot(accuracy_Huefond,xlab="Poucentage du nombre de marqueurs utilisés", ylab="Précision de la prédiction", main="Hue_fond", col="#FF6666",las=1,cex.axis=0.7,ylim=c(0,1))
F<-boxplot(accuracy_ethylene,xlab="Poucentage du nombre de marqueurs utilisés", ylab="Précision de la prédiction",main="Ethylène",col="#66CC99",las=1,cex.axis=0.7,ylim=c(0,1))
G<-boxplot(accuracy_IR,xlab="Poucentage du nombre de marqueurs utilisés", ylab="Précision de la prédiction",main="Indice de réfraction", col="#CC6666",las=1,cex.axis=0.7,ylim=c(0,1))
H<-boxplot(accuracy_AT,xlab="Poucentage du nombre de marqueurs utilisés", ylab="Précision de la prédiction",main="Acidité titrable", col="#CCCC33",las=1,cex.axis=0.7,ylim=c(0,1))

G<-boxplot(accuracy_IR,xlab="Poucentages du nombre de marqueurs utilisés", ylab="Précision de la prédiction",main="Indice de réfraction", col="#FF6666",las=1,cex.axis=0.8,ylim=c(0,1))
F<-boxplot(accuracy_ethylene,xlab="Poucentages du nombre de marqueurs utilisés", ylab="Précision de la prédiction",main="Ethylène",col="#66CC99",las=1,cex.axis=0.8,ylim=c(0,1))
H<-boxplot(accuracy_AT,xlab="Poucentages du nombre de marqueurs utilisés", ylab="Précision de la prédiction",main="Acidité titrable", col="#CC6666",las=1,cex.axis=0.8,ylim=c(0,1))
K<-boxplot(accuracy_Malique,xlab="Poucentages du nombre de marqueurs utilisés", ylab="Précision de la prédiction",main="Acide malique", col="#CCCC33",las=1,cex.axis=0.8,ylim=c(0,1))
library(gridExtra)
grid.arrange(A, B, C, D, E, F, G, H, I, J, K,  nrow=4, ncol=3)
M<-c("10%","20%","30%","40%","50%","60%","70%","80%","90%","100%")
accuracy_Pression<-as.data.frame(accuracy_Pression)
p <- ggplot(accuracy_Pression, aes(x=M, y=accuracy_Pression)) + 
  geom_boxplot(color="#99CCFF")
p <- ggplot(accuracy_Pression) + 
  geom_boxplot()
P
