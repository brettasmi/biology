# figures and data for APS poster
# data from cfx96

# load libraries
library(ggplot2)
library(reshape2)

# set proper working directory
setwd("/home/brett/Documents/Work/")

data <- read.table('160624_dilution_and_soil.csv', header=TRUE, sep=',', 
                   stringsAsFactors=FALSE)
data$Cq <- as.numeric(data$Cq)

# data for 160714
curves.160714 <- read.table("160714_data_sheets/160714_curve_data.csv", 
                            header=TRUE, sep=",")
cqs.160714 <- read.table("160714_data_sheets/160714_results.csv", 
                         header=TRUE, sep=",")
cqs.160714$Starting.Quantity..SQ.<-as.numeric(cqs.160714$Starting.Quantity..SQ.)
cqs.160714$Cq <- as.numeric(cqs.160714$Cq)

# further subset into separate dyes
sub_fam <- data[data$Fluor=="FAM", cbind("Sample", "Fluor","Cq", "Cq.Mean", 
                                         "Cq.Std..Dev")]
sub_hex <- data[data$Fluor=="HEX", cbind("Sample", "Fluor","Cq", "Cq.Mean", 
                                         "Cq.Std..Dev")]
sub_cy5 <- data[data$Fluor=="Cy5", cbind("Sample", "Fluor","Cq", "Cq.Mean", 
                                         "Cq.Std..Dev")]

cqs.160714.fam <- cqs.160714[cqs.160714$Fluor=="FAM", 
                             cbind("Sample", "Fluor", 
                                   "Cq","Starting.Quantity..SQ.")]
cqs.160714.hex <- cqs.160714[cqs.160714$Fluor=="HEX", 
                             cbind("Sample", "Fluor", 
                                   "Cq","Starting.Quantity..SQ.")]

# sort and get mean and sd of the fam Cq values
fam_all <- setNames(aggregate(sub_fam$Cq ~ sub_fam$Sample, data = sub_fam, 
                              FUN = function(x) c(mean=mean(x), sd = sd(x))), 
                    c("Sample","Cq"))

fam_all <- setNames(cbind(data.frame(unlist(fam_all$Sample)),
                          data.frame(unlist(fam_all$Cq))),
                    c("Sample","Cq.mean","Cq.sd"))

fam_all <- fam_all[2:14,]
# sort and get mean and sd of the hex Cq values
hex_all <- setNames(aggregate(sub_hex$Cq ~ sub_hex$Sample, data = sub_hex, 
                              FUN = function(x) c(mean=mean(x), sd = sd(x))), 
                    c("Sample","Cq"))

hex_all <- setNames(cbind(data.frame(unlist(hex_all$Sample)),
                          data.frame(unlist(hex_all$Cq))),
                    c("Sample","Cq.mean","Cq.sd"))

# sort and get mean and sd of the Cy5 Cq values
cy5_all <- setNames(aggregate(sub_cy5$Cq ~ sub_cy5$Sample, data = sub_cy5, 
                              FUN = function(x) c(mean=mean(x), sd = sd(x))), 
                    c("Sample","Cq"))

cy5_all <- setNames(cbind(data.frame(unlist(cy5_all$Sample)),
                          data.frame(unlist(cy5_all$Cq))),
                    c("Sample","Cq.mean","Cq.sd"))

# 160714 values
cqs.160714.fam.res <- setNames(aggregate(cbind(cqs.160714.fam$Cq, 
                                               cqs.160714.fam$Starting.Quantity..SQ.) ~ 
                               cqs.160714.fam$Sample, 
                               data = cqs.160714.fam, 
                               FUN = function(x) c(mean=mean(x), sd = sd(x))), 
                               c("Sample","Cq","sq"))

cqs.160714.fam.res <- setNames(cbind(data.frame(unlist(cqs.160714.fam.res$Sample)),
                                 data.frame(unlist(cqs.160714.fam.res$Cq)),
                                 data.frame(unlist(cqs.160714.fam.res$sq))),
                           c("Sample","Cq.mean","Cq.sd","sq.mean","sq.sd"))

cqs.160714.hex.res <- setNames(aggregate(cbind(cqs.160714.hex$Cq, 
                                               cqs.160714.hex$Starting.Quantity..SQ.) ~ 
                                         cqs.160714.hex$Sample, 
                                         data = cqs.160714.hex, 
                                         FUN = function(x) 
                                           c(mean=mean(x), sd = sd(x))), 
                               c("Sample","Cq","sq"))

cqs.160714.hex.res <- setNames(cbind(data.frame(unlist(cqs.160714.hex.res$Sample)),
                                 data.frame(unlist(cqs.160714.hex.res$Cq)),
                                 data.frame(unlist(cqs.160714.hex.res$sq))),
                           c("Sample","Cq.mean","Cq.sd","sq.mean","sq.sd"))




# Plots
#  Note if you want to add colors by category, need a separate column
#  for sample, std, neg, etc, then define as color="sep column name" 

# set up colors
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
               "#D55E00", "#CC79A7")
col.scale <-scale_color_manual(name = "Target", values = c("FAM"= cbPalette[3], 
                                                           "HEX"= cbPalette[4]), 
                               labels = c("P. betae", "B. vulgaris"))

# Pb plot
p <- ggplot(fam_all,aes(Sample, Cq.mean))
p + geom_errorbar(aes(ymin = Cq.mean - Cq.sd, ymax = Cq.mean + Cq.sd, width=0.1)) +
  geom_point(size=3, color = cbPalette[3]) +
  scale_y_continuous(breaks = c(20,21,22,23,24,25,26,27,28)) + 
  ggtitle("Polymyxa betae concentrations") + 
  ylab("Starting Quantity (ng)\n +/-sd")

# Bv Plot
bvp <- ggplot(hex_all,aes(Sample, Cq.mean))
bvp + geom_errorbar(aes(ymin = Cq.mean - Cq.sd, ymax = Cq.mean + Cq.sd, width=0.1)) +
  geom_point(size=3, color = cbPalette[3]) +
  ggtitle("Polymyxa betae concentrations") + 
  ylab("Starting Quantity (ng)\n +/-sd")



#160714 plots
# first fit the data
fam.sc<-lm(cqs.160714.fam.res$Cq.mean~log10(cqs.160714.fam.res$sq.mean))
hex.sc<-lm(cqs.160714.hex.res$Cq.mean~log10(cqs.160714.hex.res$sq.mean))

fam.intercept <-fam.sc[[1]][1]
fam.slope<-fam.sc[[1]][2]
fam.efficiency <- (10^(-1/fam.slope[[1]][1])-1)*100

hex.intercept<-hex.sc[[1]][1]
hex.slope<-hex.sc[[1]][2]
hex.efficiency <- (10^(-1/hex.slope[[1]][1])-1)*100

scp <- ggplot(cqs.160714, aes(Starting.Quantity..SQ., Cq, group = Fluor, color = Fluor)) +
  scale_color_manual(name = "Target", values = c("FAM"= cbPalette[3], "HEX"= cbPalette[4]),
                     labels = c("P. betae", "B. vulgaris")) +
  geom_point(size=2) +
  ggtitle("qPCR Standard Curves") +
  scale_x_log10(breaks=c(0.00001,0.0001,0.001,0.01,0.1,1,10), 
                labels=c("10 fg","100 fg","1 pg","10 pg", "100 pg","1 ng","10 ng")) +
  geom_abline(intercept=fam.intercept, slope=fam.slope, color = cbPalette[3],
              size = 0.3) +
  geom_abline(intercept = hex.intercept, slope = hex.slope, color = cbPalette[4],
              size = 0.3) +
  geom_text(aes(0.4, 19, label=paste("P.betae = ", formatC(fam.slope, digits = 4),
                                     "x +", formatC(fam.intercept, digits = 5),"\n",
                                       "efficiency = ", 
                                     formatC(fam.efficiency, digits = 6), "%"), 
                color = "FAM")) +
  geom_text(show_guide = F, aes(1, 33, label=paste("B. vulgaris = ", 
                                                    formatC(hex.slope, digits = 4), 
                                "x +", formatC(hex.intercept, digits = 5),"\n",
                                "efficiency = ", formatC(hex.efficiency, digits = 6), "%"), 
                color = "HEX")) +
  xlab("Starting Quantity") +
  ylab("Cq")
#ggsave(scp,filename = "160714_scp.png")

# testing making curve plots
#read data and change column names
quant.data <- read.table('160624_quant.csv',header = TRUE, sep ="," )
names <- c("Cycle", "1ng_1", "1ng_2", "1ng_3","soil_1_1","soil_1_2",
           "0.8ng_1","0.8ng_2","0.8ng_3", "soil_2_1","soil_2_2","0.6ng_1",
           "0.6ng_2","0.6ng_3","soil_3_1","soil_3_2","0.5ng_1", "0.5ng_2",
           "0.5ng_3", "soil_4_1","soil_4_2","0.4ng_1", "0.4ng_2", "0.4ng_3",
           "pb_1","pb_2","0.2ng_1", "0.2ng_2", "0.2ng_3", "water_1","water_2",
           "0.1ng_1","0.1ng_2","0.1ng_3","0.01ng_1","0.01ng_2","0.01ng_3")
colnames(quant.data) <- names

# melt data to a format that ggplot likes better
quant.long <- melt(quant.data, id="Cycle")
curves.160714.long <- melt(curves.160714, id=c("Cycle", "Dye"))
# basic plot
curve.plot <- ggplot(quant.long, aes(Cycle, value, color = variable))
curve.plot + geom_line()

# 160714
curves.160714.p <- ggplot(aes(Cycle, value, group = interaction(Dye,variable),
                              color=Dye), data = curves.160714.long)
curves.160714.p + scale_color_manual(name = "Target", values = c("FAM"= cbPalette[3],
                                                                 "HEX"= cbPalette[4]),
                                     labels = c("P. betae", "B. vulgaris")) +
  geom_line() +
  geom_hline(yintercept = 50, color = cbPalette[3]) +
  geom_hline(yintercept = 25, color = cbPalette[4]) +
  ggtitle("qPCR Curves") + 
  ylab("Relative Fluorescence Units (RFU)") + 
  geom_text(aes(37,-35, label=paste("water x 4 & P. graminis x 2 ^")), color = "black")

# subset data to only triplicate samples
sub.quant <- quant.data[c(1,2,3,4,7,8,9,12,13,14,17,18,19,22,23,24,27,28,29,32,33,34,35,36,37)]
sub.melt <- melt(sub.quant,id="Cycle")

# basic subset plot
ggplot(sub.melt, aes(Cycle, value, color = variable)) + geom_line()

# no legend plot
ggplot(sub_melt, aes(Cycle, value, color = variable)) + geom_line() + 
  theme(legend.position="none")

# same color lines subset dataset and plot
sub.sub.quant <- quant.data[c(1,2,3)]
sub.sub.melt <- melt(sub.sub.quant, id="Cycle")
all.black.palette <- c("#000000","#000000")
sub.sub.plot <- ggplot(sub_sub_quant, aes(Cycle, value, color = variable))
sub.sub.plot + geom_line() +
scale_color_manual(values = test.pal)