library(dtw)
library(cluster)
library(TSdist)
library(fpc)

#DATA PREPARATION

data <- read.table('D:\\UoA\\Data Mining\\Time Series1\\synthetic_control.data',header=F, sep="")
s <-sample(1:100,10)
idx <-c(s,100+s,200+s,300+s,400+s,500+s)
random_data <- data[idx,]



##CLUSTERING ANALYSIS
#HIERARCHICAL 
observedlabels <- rep(1:6,each=10)

dev.new()
hc_eu <- hclust(dist(random_data),method="average")
plot(hc_eu, labels=observedLabels,main="Hierarchical clustering- Euclidean Dist")
rect.hclust(hc_eu,k=6)
 hc_eu.cluster <- cutree(hc_eu,k=6)
table(observedLabels, hc_eu.cluster)

dev.new()
dist_DTW <- dist(random_data,method="DTW")
hc_dtw <- hclust(dist_DTW,method="average")
plot(hc_eu, labels=observedLabels,main="Hierarchical clustering- DTW")
rect.hclust(hc_dtw,k=6)
hc_dtw.cluster <- cutree(hc_dtw,k=6)
table(observedLabels, hc_dtw.cluster)

dist_sax <- TSDatabaseDistances(random_data,y=NULL,w=5,alpha=5,distance="mindist.sax")
hc_sax <- hclust(dist_sax,method="average")
plot(hc_sax, labels=observedLabels,main="Hierarchical clustering- SAX")
rect.hclust(hc_sax,k=6)
hc_sax.cluster <- cutree(hc_sax,k=6)
table(observedLabels,  hc_sax.cluster)

#KMEDOIDS
pam_eu <- pam(dist(random_data),6,diss=TRUE)
plot(pam_eu)
pam_eu$clustering

pam_dtw <- pam(dist_DTW,6,diss=TRUE)
plot(pam_dtw)
pam_dtw$clustering

pam_sax <- pam(dist_sax,6,diss=TRUE)
plot(pam_sax)
pam_sax$clustering

# creation of ground truth vector ref
ref <- seq(1,1,length.out=10)
sub <- seq(2,2,length.out=10)
ref <- append(ref, sub, after = length(ref))
sub <- seq(3,3,length.out=10)
ref <- append(ref, sub, after = length(ref))
sub <- seq(4,4,length.out=10)
ref <- append(ref, sub, after = length(ref))
sub <- seq(5,5,length.out=10)
ref <- append(ref, sub, after = length(ref))
sub <- seq(6,6,length.out=10)
ref <- append(ref, sub, after = length(ref))

##EXTERNAL VALIDATION
#CLUSTER STATISTIC - using package fpc

stat_hc_EU <- cluster.stats(dist(random_data),ref,hc_eu.cluster)
stat_hc_DTW <- cluster.stats(dist_DTW,ref,hc_dtw.cluster)
stat_hc_SAX <- cluster.stats(dist_sax,ref,hc_sax.cluster)

stat_pam_EU <- cluster.stats(dist(random_data),ref,pam_eu$cluster);
stat_pam_DTW <- cluster.stats(dist_DTW,ref,pam_dtw$cluster);
stat_pam_SAX <- cluster.stats(dist_sax,ref,pam_sax$cluster)

#RAND INDEX CALCULATION
stat_hc_EU$corrected.rand
stat_hc_DTW$corrected.rand
stat_hc_SAX$corrected.rand

stat_pam_EU$corrected.rand
stat_pam_DTW$corrected.rand
stat_pam_SAX$corrected.rand

#ENTROPY CALCULATION

stat_hc_EU$entropy;
stat_hc_DTW$entropy;
stat_hc_SAX$entropy;

stat_pam_EU$entropy;
stat_pam_DTW$entropy;
stat_pam_SAX$entropy


