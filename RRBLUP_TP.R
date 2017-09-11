##Markers
data1 <- as.matrix(read.table("FruitSelGen-apricot-PRPER-Lovell-v2-avignon_GoxMo_raw-snps_gatk-filter.txt"))
data1[data1==0] <- -1
data1[data1==1] <- 0
data1[data1==2] <- 1
Markers<-t(data1)
dim(Markers)#61030
##Phenotypes
Pheno<-read.table("Qualitéajustée.txt", header=TRUE, sep="\t")
dim(Pheno)#11
for(i in 1:11)
{Pheno[,i]<-as.numeric(as.character(Pheno[,i]))}
noms_caracteres<-names(Pheno)
Pheno <- as.matrix(Pheno)
## Calculates the realized additive relationship matrix.
#impute missing markers with A.mat
library(rrBLUP)
impute=A.mat(Markers,impute.method="mean",return.imputed=T)
#Imputed markers matrix
Markers_impute=impute$imputed
dim(Markers_impute)#32919
#Additive relationship matrix
impute$A
#define the training and test populations
#training-75% validation-25% (pop totale =186)
#
traits=11
cycles=100
accuracy = matrix(nrow=cycles, ncol=traits)
for(r in 1:cycles) {
  for(n in c(1:traits)) 
  {
    train= as.matrix(sample(1:186, 140))
    test<-setdiff(1:186,train)
    Pheno_train=Pheno[train,]
    m_train=Markers_impute[train,]
    Pheno_valid=Pheno[test,]
    m_valid=Markers_impute[test,]
    
    y= Pheno_train[,n]
    y_answer<-mixed.solve(y, Z=m_train, K=NULL, SE = FALSE, return.Hinv=FALSE)
    
    p = y_answer$u
    e = as.matrix(p)
    pred_y_valid =  m_valid %*% e
    pred_y=(pred_y_valid[,1]) + y_answer$beta
    y_valid = Pheno_valid[,n]
    accuracy[r,n] <-cor(pred_y_valid, y_valid, use="complete" )
    colnames(accuracy)<-colnames(Pheno)
  }
}
write.table(accuracy, "RRBLUP_100_75.txt", sep=" ", quote=F, col.names=F, row.names=T)
#Boxplot en pdf
colnames(accuracy)<-colnames(Pheno)
boxplot(accuracy, xlab="Phénotypes",  ylab="Précision de la prédiction", col="salmon", las=2, cex.axis=.7)
boxplot(accuracy, xlab="Phénotypes",  ylab="Accuracies", col="darksalmon", 
        main =" Précision de la prédiction_RR BLUP", las=2, cex.axis=.7)
boxplot(accuracy, xlab="Phénotypes",  ylab="Accuracies", col="cyan4", 
        main =" Précision de la prédiction_RR BLUP", las=2, cex.axis=.7)
boxplot(accuracy, xlab="Phénotypes",  ylab="Accuracies", col="sandybrown", 
        main =" Précision de la prédiction_RR BLUP", las=2, cex.axis=.7)
boxplot(accuracy, xlab="Phénotypes",  ylab="Accuracies", col="lightcoral", 
        main =" Précision de la prédiction_RR BLUP", las=2, cex.axis=.7)
pdf("RRBLUP_75_06.pdf", 
    height=10,width=10) 
boxplot(accuracy_75_25_06, xlab="Phénotypes",  ylab="Accuracies", col="salmon", 
        main =" Précision de la prédiction_RR BLUP", las=2, cex.axis=.7)
dev.off()
#
#training-60% validation-40% (pop totale =186)
#
traits=11
cycles=100
accuracy = matrix(nrow=cycles, ncol=traits)
for(r in 1:cycles) {
  for(n in c(1:traits)) 
  {
    train= as.matrix(sample(1:186, 112))
    test<-setdiff(1:186,train)
    Pheno_train=Pheno[train,]
    m_train=Markers_impute[train,]
    Pheno_valid=Pheno[test,]
    m_valid=Markers_impute[test,]
    
    y= Pheno_train[,n]
    y_answer<-mixed.solve(y, Z=m_train, K=NULL, SE = FALSE, return.Hinv=FALSE)
    
    p = y_answer$u
    e = as.matrix(p)
    pred_y_valid =  m_valid %*% e
    pred_y=(pred_y_valid[,1]) + y_answer$beta
    y_valid = Pheno_valid[,n]
    accuracy[r,n] <-cor(pred_y_valid, y_valid, use="complete" )
    colnames(accuracy)<-colnames(Pheno)
  }
}
write.table(accuracy, "RRBLUP_100_60.txt", sep=" ", quote=F, col.names=F, row.names=T)
#Boxplot en pdf
colnames(accuracy)<-colnames(Pheno)
boxplot(accuracy, xlab="Phénotypes",  ylab="Accuracies", col="salmon", 
        main =" Précision de la prédiction_RR BLUP", las=2, cex.axis=.7)
#
#training-50% validation-50% (pop totale =186)
traits=11
cycles=100
accuracy = matrix(nrow=cycles, ncol=traits)
for(r in 1:cycles) {
  for(n in c(1:traits)) 
  {
    train= as.matrix(sample(1:186, 93))
    test<-setdiff(1:186,train)
    Pheno_train=Pheno[train,]
    m_train=Markers_impute[train,]
    Pheno_valid=Pheno[test,]
    m_valid=Markers_impute[test,]
    
    y= Pheno_train[,n]
    y_answer<-mixed.solve(y, Z=m_train, K=NULL, SE = FALSE, return.Hinv=FALSE)
    
    p = y_answer$u
    e = as.matrix(p)
    pred_y_valid =  m_valid %*% e
    pred_y=(pred_y_valid[,1]) + y_answer$beta
    y_valid = Pheno_valid[,n]
    accuracy[r,n] <-cor(pred_y_valid, y_valid, use="complete" )
    colnames(accuracy)<-colnames(Pheno)
  }
}
write.table(accuracy, "RRBLUP_100_50.txt", sep=" ", quote=F, col.names=F, row.names=T)
#Boxplot en pdf
colnames(accuracy)<-colnames(Pheno)
boxplot(accuracy, xlab="Phénotypes",  ylab="Accuracies", col="salmon", 
        main =" Précision de la prédiction_RR BLUP", las=2, cex.axis=.7)
#training-25% validation-75% (pop totale =186)
traits=11
cycles=100
accuracy = matrix(nrow=cycles, ncol=traits)
for(r in 1:cycles) {
  for(n in c(1:traits)) 
  {
    train= as.matrix(sample(1:186, 46))
    test<-setdiff(1:186,train)
    Pheno_train=Pheno[train,]
    m_train=Markers_impute[train,]
    Pheno_valid=Pheno[test,]
    m_valid=Markers_impute[test,]
    
    y= Pheno_train[,n]
    y_answer<-mixed.solve(y, Z=m_train, K=NULL, SE = FALSE, return.Hinv=FALSE)
    
    p = y_answer$u
    e = as.matrix(p)
    pred_y_valid =  m_valid %*% e
    pred_y=(pred_y_valid[,1]) + y_answer$beta
    y_valid = Pheno_valid[,n]
    accuracy[r,n] <-cor(pred_y_valid, y_valid, use="complete" )
    colnames(accuracy)<-colnames(Pheno)
  }
}
write.table(accuracy, "RRBLUP_100_25.txt", sep=" ", quote=F, col.names=F, row.names=T)
#Boxplot en pdf
colnames(accuracy)<-colnames(Pheno)
boxplot(accuracy, xlab="Phénotypes",  ylab="Accuracies", col="salmon", 
        main =" Précision de la prédiction_RR BLUP", las=2, cex.axis=.7)