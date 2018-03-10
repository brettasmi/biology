#import necessary library
library(introgress)
#set working directory
setwd('/Users/brettsmith/Thesis/introgress/RHHY_141009')
#import hybrid genotype data, transpose it, and subset it
HybData<- read.table("rhhy_hy.txt",header=FALSE, sep="\t")
HybData<-t(HybData)
newHybData<-HybData[,c(2:18)]
#import Follettii data
FOData<- read.table("rhhy_fo.txt", header =FALSE, sep="\t")
FOData<-t(FOData)
newFOData<-FOData[,c(2:94)]
#import Stebbinsii data
STData<- read.table("rhhy_st.txt", header=FALSE, sep="\t")
STData<-t(STData)
newSTData<-STData[,c(2:72)]
#import loci data table
locData<-read.table("RHHY_lociData.txt", header=TRUE, sep="\t")
#prepare data for analysis using introgress's prepare.data function
Data<-prepare.data(admix.gen=newHybData, loci.data=locData, pop.id=TRUE, ind.id=TRUE, sep.rows=TRUE, parental1 = newSTData, parental2 = newFOData, fixed=FALSE)
#estimate hybrid index
h.index<-est.h(introgress.data=Data,loci.data=locData, ind.touse=NULL, fixed=FALSE)
#save the hybrid index to file to 
write.table(h.index,"RHHY141009hindex.txt", quote=FALSE, sep="\t")
#set up postscript printer and print introgress plot to file
postscript(file="rhhy_introgress_plot.ps", height=11, width=8)
mk.image(introgress.data=Data, loci.data=locData,marker.order=NULL, hi.index=h.index,ind.touse=NULL, loci.touse=NULL, ylab.image="Individuals",main.image="Red Hill Hybrid Zone",xlab.h="Follettii Ancestry", col.image=NULL, pdf=FALSE)
dev.off()
#set up post script printer and make a histogram from h-index estimations
postscript(file='rhhy_hybrid_index.ps', height=10, width=6)
hist(h.index[,c(2)], main="Red Hill Hybrid Index", xlab="Hybrid Index", ylab="Frequency", col="grey")
dev.off()