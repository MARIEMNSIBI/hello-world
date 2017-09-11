#############################################################################
# Script to optimize the calibration set in genomic selection (maximize the expected reliability).
# Method based on the generalized CD.
# (Rincent et al. 2012)
#############################################################################
library(QTLRel)
library(plyr)
Pheno<-read.table("Qualité_ajustée.txt", header=TRUE, sep="\t")
Pheno<-Pheno[,-1]
noms_caracteres<-names(Pheno)
Pheno <- Pheno[,-1]
for(i in 1:11)
{Pheno[,i]<-as.numeric(as.character(Pheno[,i]))}
Pheno <- as.matrix(Pheno)
data1 <- as.matrix(read.table("FruitSelGen-apricot-PRPER-Lovell-v2-avignon_GoxMo_raw-snps_gatk-filter.txt"),header=F)
dim(data1)#61030   186
data1[data1==0] <- -1
data1[data1==1] <- 0
data1[data1==2] <- 1
Markers<-t(data1)
#Importation de la matrice d'apparentement
matA1=read.table("Matrice_apparentement.csv",header=T,sep=";")
dim(matA1)#186 187
matA1<-matA1[,-c(1)]
matA1=as.matrix(matA1)
#
library(rrBLUP)
impute=A.mat(Markers,impute.method="mean",return.imputed=T)
#Imputed markers matrix
Markers_impute=impute$imputed
dim(Markers_impute)#32919
#
heritabilites<-read.table("Héritabilités.csv",h=F, sep=";", dec = ",")
heritabilites[,2]<-as.numeric(as.character(heritabilites[,2]))
#heritabilites<- as.matrix(heritabilites)
nindrep=140 # Choose a size for your calibration set
traits=11
cycles=100
accuracy = matrix(nrow=cycles, ncol=traits)
for(r in 1:cycles) {
  for(n in c(1:traits)) 
  {
    ###############
    #Functions used
    ###############
    
    # This function creates the matrix of contrast between each of the individual not in the calibration set and the mean of the population
    contrasteNonPheno=function(NotSampled)
    {
      mat=matrix(-1/Nind,Nind,Nind-Nind_in_Sample)
      for (i in 1:ncol(mat)) {
        mat[NotSampled[i],i]=1-1/Nind
      }
      return(mat)
    }
    
    ##############################
    # Data required
    ##########################
    
    #matA1=read.table("Matrice_apparentement.csv",header=T,sep=";")
    #dim(matA1)#186 186
    #matA1<-matA1[,-c(1)]
    #matA1=as.matrix(matA1)
    Nind=nrow(matA1) # total number of individuals
    #Pheno<-read.table("Qualitéajustée.txt", header=TRUE, sep="\t")
    #rownames(Pheno) <- Pheno[,1]
    #Pheno <- Pheno[,-1]
    #Pheno <- as.matrix(Pheno)
    #Sélectionner les phénos intéressants :
    #Pheno<-Pheno[,c("Fmax",     "Pnoyau",   "Lchair",   "Huefond",  "ethylene", "IR",       "AT",   "Fructose", "Citrique", "Malique" )]
    pheno<-Pheno[,n]
    varP <-var(pheno, y = NULL, na.rm = TRUE) # Pheno is a vector of phenotypes
    h2=heritabilites[n,2]  # Trait heritability
    varG=h2*varP
    varE=(1-h2)/h2*varG
    lambda=varE/varG # lambda is needed to estimate the CDmean
    invA1=solve(matA1) # Inverse of the covariance matrix
    ##############################
    # Optimization algo
    ##############################
    Nind_in_Sample=nindrep
    #Design matrices
    Ident<-diag(Nind_in_Sample)
    X<-rep(1,Nind_in_Sample)
    M<-Ident- (X%*%solve(t(X)%*%X) %*% t(X) )
    
    Sample1<-sample(Nind,Nind_in_Sample) #Calibration set initialization
    SaveSample=Sample1
    NotSampled1<-seq(1:Nind)
    NotSampled<-NotSampled1[-Sample1] # Initial validation set
    
    Z=matrix(0,Nind_in_Sample,Nind)
    for (i in 1:length(Sample1)) { Z[i,Sample1[i]]=1 } 
    
    W<-contrasteNonPheno(NotSampled)   # W matrice des contrastes
    
    # Calculate of CDmean of the initial set
    matCD<-(t(W)%*%(matA1-lambda*solve(t(Z)%*%M%*%Z + lambda*invA1))%*%W)/(t(W)%*%matA1%*%W)
    CD=diag(matCD)
    CDmeanSave=mean(CD)
    
    CDmeanMax1=rep(NA,800)
    
    # Exchange algorithm (maximize CDmean)
    cpt2=1
    cpt=0
    while (cpt2<800) {  # Make sure that 800 is enough in your case (that you reached a plateau), for this look at CDmeanMax1.
      NotSampled=NotSampled1[-Sample1] 
      cpt2=cpt2+1
      # Remove one individual (randomly choosen) from the sample :
      Sample2=sample(Sample1,1)
      # Select one individual (randomly choosen) from the individuals that are not in the Calibration set :
      Sample3=sample(NotSampled,1)
      # New calibration set :
      Sample4=c(Sample3,Sample1[Sample1!=Sample2])
      # Calculate the mean CD of the new calibration set :
      Z=matrix(0,Nind_in_Sample,Nind)
      for (i in 1:length(Sample4)) { Z[i,Sample4[i]]=1 } 
      NotSampled=NotSampled1[-Sample4] 
      W<-contrasteNonPheno(NotSampled)
      
      matCD<-(t(W)%*%(matA1-lambda*solve(t(Z)%*%M%*%Z + lambda*invA1))%*%W)/(t(W)%*%matA1%*%W)
      CD=diag(matCD)
      
      if (mean(CD)>CDmeanSave ) { Sample1=Sample4 # Accept the new Calibration set if CDmean is increased, reject otherwise.
      CDmeanSave=mean(CD)  
      cpt=0 } else { cpt=cpt+1 
      }
      CDmeanMax1[cpt2-1]=CDmeanSave
    }  #Fin du while
    
    SampleOptimiz=Sample1 # SampleOptimiz is the optimized calibration set
    # End
    lengthsample<-length(Sample1) 
    vecteurTP<-c()
    ###### Prediction #######
    #train= as.matrix(sample(1:186, 140))
    train <- as.matrix(Sample1)
    #test <- setdiff(1:186,train)
    test <- setdiff(1:186,Sample1)
    #train= as.matrix(TP)
    #test<-setdiff(1:186,train)
    #test<-setdiff(1:186,SampleOptimiz)
    
     Pheno_train=Pheno[train,]
    
    #Pheno_train=Pheno[Sample1,]
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
colnames(accuracy)<-colnames(Pheno)
write.table(accuracy, "RRBLUP_CDmean_75_100.txt", sep=" ", quote=F, col.names=F, row.names=T)
#Boxplot en pdf
colnames(accuracy)<-colnames(Pheno)
boxplot(accuracy, xlab="Phénotypes",  ylab="Précision de la prédiction", col="salmon", las=2, cex.axis=.7)
dev.off()
