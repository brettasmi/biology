#Soil PCA and ggbiplot
#Edited 141001

#load ggbiplot and dependencies
library(ggbiplot)
#set working directory
setwd('/Users/brettsmith/Thesis/soils')
#read soil csv
rawSoil<-read.table('soil_raw.csv', header=TRUE, sep=',')
#subset actual data using columns 5-19 of rawSoil
soil_data<-rawSoil[,5:19]
#create species vector for use in plot
species<-rawSoil[,2]
sample_names<-rawSoil[,1]
#do pca using prcomp function
soil.pca<-prcomp(soil_data, center=TRUE, scale=TRUE, retx=TRUE)
#create vectors for sdev, loadings, scores, variance
sd <- soil.pca$sdev
loadings<-soil.pca$rotation
rownames(loadings)<-colnames(soil_data)
scores<-soil.pca$x
var<-sd^2
var.percent<-var/sum(var)*100
#create barplot for variance (in percent) explained by each PC, and add line for var explained by each variable
dev.new()
barplot(var.percent,xlab="PC", ylab="Percent Variance", names.arg=1:length(var.percent), las=1, ylim=c(0,max(var.percent)), col="gray")
abline(h=1/ncol(soil_data)*100, col="red")
#view a subset of variances as percents and sum them
var.percent[1:x] #replace 'x' with the last individual variance you want to see
sum(var.percent[1:x]) #again, replace x
#use ggbiplot to create plot
pca_plot<-ggbiplot(soil.pca,obs.scale=1,var.scale=1,groups=species,ellipse=TRUE,ellipse.prob=.95)
#####testing######
pca_plot<-ggbiplot(soil.pca,obs.scale=1,var.scale=1,shapes=species, alpha=0,ellipse=TRUE,ellipse.prob=.95)
pca_plot_titled<-pca_plot+ggtitle("Soil PCA") + geom_point(aes(color=species, shape=species), size = 3)
#######end test######
#following line will print pca_plot to quartz if you want
print(pca_plot_titled)
#following lines will activate postscript printer, print the plot, then turn off postscript printer
postscript("soil_pca_title_test.ps", width=8, height=8)
print(pca_plot_titled)
dev.off()
#following lines will activate tiff printer, print the plot, then turn off tiff printer
tiff("soil_pca_test.tiff", width=1000, height=1000, units="px", compression="none")
print(pca_plot_titled)
dev.off()
#write PC summary to disk
summary.table<-summary(soil.pca)[6]
write.table(summary.table, file="soil_PC_prop_variance.txt", sep='\t')
#following lines will create a line plot for the first 10 PCs with PC on x axis and variance on y axis
#extracting scores by pop and taking means
#mofo pops
mofo3009<-scores[1:5,]
mofo3009_means<-colMeans(mofo3009)
mofo3009<-rbind(mofo3009,mofo3009_means)
pop_names<-"mofo3009"
mofo3001<-scores[6:10,]
mofo3001_means<-colMeans(mofo3001)
mofo3001<-rbind(mofo3001,mofo3001_means)
pop_names<-c(pop_names,"mofo3001")
mofo3003<-scores[11:15,]
mofo3003_means<-colMeans(mofo3003)
mofo3003<-rbind(mofo3003,mofo3003_means)
pop_names<-c(pop_names,"mofo3003")
mofo3002<-scores[16:20,]
mofo3002_means<-colMeans(mofo3002)
mofo3002<-rbind(mofo3002,mofo3002_means)
pop_names<-c(pop_names,"mofo3002")
mofo3005<-scores[21:25,]
mofo3005_means<-colMeans(mofo3005)
mofo3005<-rbind(mofo3005,mofo3005_means)
pop_names<-c(pop_names,"mofo3005")
#most pops
most001<-scores[31:35,]
most001_means<-colMeans(most001)
most001<-rbind(most001,most001_means)
pop_names<-c(pop_names,"most001")
most004<-scores[36:40,]
most004_means<-colMeans(most004)
most004<-rbind(most004,most004_means)
pop_names<-c(pop_names,"most004")
most003<-scores[41:45,]
most003_means<-colMeans(most003)
most003<-rbind(most003,most003_means)
pop_names<-c(pop_names,"most003")
most005<-scores[31:35,]
most005<-rbind(most005,scores[53:57])
most005_means<-colMeans(most005)
most005<-rbind(most005,most005_means)
pop_names<-c(pop_names,"most005")
#hz pops
bhhz<-scores[58:61,]
bhhz_means<-colMeans(bhhz)
bhhz<-rbind(bhhz,bhhz_means)
pop_names<-c(pop_names,"bhhz")
rhhz<-scores[64:70,]
rhhz_means<-colMeans(rhhz)
rhhz<-rbind(rhhz,rhhz_means)
pop_names<-c(pop_names,"rhhz")
hz70<-scores[51:52,]
hz70_means<-colMeans(hz70)
hz70<-rbind(hz70,hz70_means)
pop_names<-c(pop_names,"hz70")
#sh pops
sh<-scores[62:63,]
sh_means<-colMeans(sh)
sh<-rbind(sh,sh_means)
pop_names<-c(pop_names,"sh")
pop_PC_score_means<-rbind(mofo3009_means,mofo3001_means,mofo3003_means,mofo3002_means,mofo3005_means,most001_means,most004_means,most003_means,most005_means,bhhz_means,rhhz_means,hz70_means,sh_means)