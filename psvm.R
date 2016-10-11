library(e1071)    #SVM
library(cluster)  #PAM
library(fpc)      #PAMK

psvm<-function(coord,clases,ngrupos,kernel)
{

	d1<-round(length(coord[,1][clases==1])/ngrupos)
	d2<-round(length(coord[,1][clases==-1])/ngrupos)
	
	coord1<-coord[which(clases==1),]
	coord2<-coord[which(clases==-1),]

#	aux1<-pamk(coord1,2:ngrupos,criterion="asw")
 	aux1<-kmeans(coord1,ngrupos)
	
# 	Para k-means
 	cluster1<-aux1$cluster
	
# 	Para pamk
#	cluster1<-aux1$pamobject$clustering
		
#	aux2<-pamk(coord2,2:ngrupos,criterion="asw")
 	aux2<-kmeans(coord2,ngrupos)
	
	
# 	Para k-means
 	cluster2<-aux2$cluster
	
# 	Para pamk
#	cluster2<-aux2$pamobject$clustering

	l<-array(dim=(ngrupos^2))
	class<-array(dim=(ngrupos^2))
	k<-1

# 	Generamos subgrupos para despu?s parearlos y crear datos para las SVMs.
# 	Tambi?n creamos un vector de clases para saber a que clase pertenece cada subgrupo.
# 	k-means
 	for(j in 1:ngrupos)
#	for(j in 1:aux2$nc)
	{
	  # k-means
	  for(i in 1:ngrupos)
#		for(i in 1:aux1$nc)#ngrupos)
		{
			x<-rbind(coord1[which(cluster1==i),],coord2[which(cluster2==j),])
			class[k]<-list(clases=(c(rep(1,length(which(cluster1==i))),rep(-1,length(which(cluster2==j))))))
			l[k]<-list(grupo=x)
			k<-k+1
		}
	}

	
# 	K-means
 	models<-array(dim=ngrupos^2)
# 	pamk
#	ng<-aux1$nc*aux2$nc
#	models<-array(dim=ng)


# 	Agrupamos los grupos creados en el paso anterior y sus clases.
# 	Con los datos y las clases ya creados, podemos crear las SVMs

# 	kmeans
 	for(i in 1:(ngrupos^2))
# 	pamk
#	for(i in 1:ng)
	{
		models[i]<-list(model=svm(l[[i]],class[[i]],type="C",kernel=kernel,scale=FALSE))
	}
	
	models
}

ppredict<-function(models,coord)
{
	n<-length(models)
	predi<-array(dim=n)

# 	Generamos n modelos de predicciones con las SVMs	

	for(i in 1:n)
	{
		predi[i]<-list(pred=predict(models[[i]],coord))
	}

	vprediction<-c()
	for(i in 1:n)
	{
# 	  OLD
     vprediction<-cbind(vprediction,predi[[i]])
# 	  NEW
#	  f<-attr(predi[[i]],"decision.values")
#		vprediction<-cbind(vprediction,f)
	}
	predictionrounded<-array(dim=length(vprediction[,1]))

# 	El sistema de votaci?n se realiza haciendo la media de los datos y redonde?ndolo al m?s cercano.

	vprediction[which(vprediction==1)]=-1
	vprediction[which(vprediction==2)]=1
	
	for(i in 1:length(vprediction[,1]))
	{
	  predictionrounded[i]<-sign(mean(vprediction[i,]))
	}
	
	predictionrounded
}