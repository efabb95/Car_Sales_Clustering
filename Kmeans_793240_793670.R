#Fabbris Elia 793240
#Fumagalli Matteo 793670

library(cluster)
library(fpc) #contiene la funzione cluster.stats
library(seriation)

datasetCar = read.csv('car_integration_sample.csv',header = TRUE)

#Prova 1, attributi lenght, width, height e wheelbase
dbCar_prova1 = scale(datasetCar[4:7])

#Stima del numero ottimale di cluster
nCluster = 2:8
set.seed(1293)
SW = sapply(nCluster, function(k) {cluster.stats(dist(dbCar_prova1), kmeans(dbCar_prova1,
                                  centers=k,nstart = 20)$cluster)$avg.silwidth}) 
SW
plot(nCluster,SW, type="l", xlab= "number of clusters",ylab = "average")

#Stima della silhouette per ciascun cluster ottenuto rispetto alla soluzione ottimale
set.seed(1293)
fit = kmeans(dbCar_prova1,8,nstart = 200)
kms = silhouette(fit$cluster, dist(dbCar_prova1))
plot(kms)
clusplot(dbCar_prova1, fit$cluster, color=TRUE, shade=TRUE,labels=1, lines=0)

#Stima della matrice di dissimilarità rispetto alla soluzione di cluster ottimale
dissplot(dist(dbCar_prova1), labels=fit$cluster, options=list(main="Kmeans Clustering"))
#Risultati clustering
table(fit$cluster,datasetCar$body)

#Prova 2, attributi price, lenght, width, height e wheelbase
dbCar_prova2 = scale(datasetCar[3:7])

#Stima del numero ottimale di cluster
nCluster = 2:8
set.seed(1293)
SW = sapply(nCluster, function(k) {cluster.stats(dist(dbCar_prova2), kmeans(dbCar_prova2,
                                                                     centers=k,nstart = 20)$cluster)$avg.silwidth}) 
SW
plot(nCluster,SW, type="l", xlab= "number of clusters",ylab = "average")

#Stima della silhouette per ciascun cluster ottenuto rispetto alla soluzione ottimale
set.seed(1293)
fit = kmeans(dbCar_prova2,4,nstart = 200)
kms = silhouette(fit$cluster, dist(dbCar_prova2))
plot(kms)
clusplot(dbCar_prova2, fit$cluster, color=TRUE, shade=TRUE,labels=1, lines=0)
#Stima della matrice di dissimilarità rispetto alla soluzione di cluster ottimale
dissplot(dist(dbCar_prova2), labels=fit$cluster, options=list(main="Kmeans Clustering"))
#Risultati clustering
table(fit$cluster,datasetCar$body)

#Prova_extra_prova1_4cluster
#Stima della silhouette per ciascun cluster ottenuto rispetto alla soluzione con 4 cluster
set.seed(1293)
fit = kmeans(dbCar_prova1,4,nstart = 200)
kms = silhouette(fit$cluster, dist(dbCar_prova1))
plot(kms)
clusplot(dbCar_prova1, fit$cluster, color=TRUE, shade=TRUE,labels=1, lines=0)
#Stima della matrice di dissimilarità rispetto alla soluzione con 4 cluster
dissplot(dist(dbCar_prova1), labels=fit$cluster, options=list(main="Kmeans Clustering"))
#Risultati clustering
table(fit$cluster,datasetCar$body)
  