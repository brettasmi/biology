# import libraries 
library(survival)
library(ggplot2)


# get data (all, 11-12, 11-16)
mp.mortality <- read.table("/Users/brettsmith/Desktop/mp_mortality1.csv", 
                           header = TRUE, sep = ",")

mp.11.12.mortality <- read.table("/Users/brettsmith/Desktop/1112_survival.csv", 
                       header = TRUE, sep = ",")

mp.11.16.mortality <- read.table("/Users/brettsmith/Desktop/1116_survival.csv", 
                                 header = TRUE, sep = ",")

root.scores <- read.table("/Users/brettsmith/Desktop/soil_scores.csv", header=TRUE, sep=",")

weights <- read.table("/Users/brettsmith/Desktop/160714_weights.csv", sep = ",", header = TRUE)

## SURVIVAL OBJECTS AND MODELS, PROBABLY DO NOT USE

# # get survival objects (all, 11-12, 11-16)
# mp.surv <- survfit(Surv(time,status == 5.0) ~ strata(treatment), data = mp.mortality)
# 
# mp.11.12.surv <- survfit(Surv(time,status == 5.0) ~ strata(treatment), data = mp.11.12.mortality, conf.type="none")
# 
# mp.11.16.surv <- survfit(Surv(time,status == 5.0) ~ strata(treatment), data = mp.11.16.mortality, conf.type="none")
# 
# # make survival plots
# # all
# plot(mp.surv, lty=c(1,2,3,4,1,2,3,4,1), 
#      lwd=c(1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5),
#      col=c("black","black","black","black","red","red","red","red","blue"), 
#      xlab="Week",ylab="Proportion Surviving")
# 
# legend(x=.2,y=.4,
#        c("11-12_100", "11-12_2.5","11-12_20","11-12_500","11-16_100",
#          "11-16_2.5","11-16_20","11-16_500","control"), 
#        lty=c(1,2,3,4,1,2,3,4,1), col=c("black","black","black","black",
#                                        "red","red","red","red","blue"),
#        lwd=c(1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5),bty="n")
# 
# # 11-12
# plot(mp.11.12.surv, lty=c(2,2,2,2,3), 
#      lwd=c(1.5,1.5,1.5,1.5,1.5),
#      col=c("black","red","darkorange3","darkgreen","blueviolet"), 
#      xlab="Week",ylab="Proportion Surviving",
#      main="11-12 Survival")
# 
# legend(x=0.2,y=0.2,
#        c("11-12 100,000", "11-12 2,500,000","11-12 20,000","11-12 500,000","control"), 
#        lty=c(2,2,2,2,3), col=c("black","red","darkorange3","darkgreen","blueviolet"),
#        lwd=c(1.5,1.5,1.5,1.5,1.5),bty="n")
# 
# # 11-16
# plot(mp.11.16.surv, lty=c(2,2,2,2,3), 
#      lwd=c(1.5,1.5,1.5,1.5,1.5),
#      col=c("black","red","darkorange3","darkgreen","blueviolet"), 
#      xlab="Week",ylab="Proportion Surviving",
#      main="11-16 Survival")
# 
# legend(x=0.2,y=0.2,
#        c("11-16 100,000", "11-16 2,500,000","11-16 20,000","11-16 500,000","control"), 
#        lty=c(2,2,2,2,3), col=c("black","red","darkorange3","darkgreen","blueviolet"),
#        lwd=c(1.5,1.5,1.5,1.5,1.5),bty="n")



# ratings

mp.11.12.status <- setNames(aggregate(mp.11.12.mortality$status ~ (mp.11.12.mortality$time + mp.11.12.mortality$treatment),
                    data = mp.11.12.mortality, FUN = function(x) c(mean=mean(x), se=(sd(x)/sqrt(length(x))))),
                    c("time","treatment","status"))

mp.11.12.status <- setNames(cbind(data.frame(unlist(mp.11.12.status$time)),
                          data.frame(unlist(mp.11.12.status$treatment)),
                          data.frame(unlist(mp.11.12.status$status))), c("time","treatment","mean","se"))
                         
mp.11.16.status <- setNames(aggregate(mp.11.16.mortality$status ~ (mp.11.16.mortality$time + mp.11.16.mortality$treatment),
                    data = mp.11.16.mortality, FUN = function(x) c(mean=mean(x), se=(sd(x)/sqrt(length(x))))),
                            c("time","treatment","status"))

mp.11.16.status <- setNames(cbind(data.frame(unlist(mp.11.16.status$time)),
                                  data.frame(unlist(mp.11.16.status$treatment)),
                                  data.frame(unlist(mp.11.16.status$status))), c("time","treatment","mean","se"))

# status plots
dodge <- position_dodge(0.2)
status.plot.11.12 <- ggplot(mp.11.12.status,aes(time, mean, color = treatment))
status.plot.11.12 + geom_errorbar(aes(ymin = mean - se, ymax = mean + se, width=0.1), position = dodge) +
  geom_point(size=2, position = dodge) + 
  geom_line(position = dodge) +
  scale_y_continuous(breaks = c(0,1,2,3,4,5,6)) + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7)) +
  ggtitle("11-12 mean disease rating by treatment") + 
  ylab("disease rating +/- se") + xlab("week")

status.plot.11.16 <- ggplot(mp.11.16.status,aes(time, mean, color = treatment))
status.plot.11.16 + geom_errorbar(aes(ymin = mean - se, ymax = mean + se, width=0.1), position = dodge) +
  geom_point(size=2, position = dodge) + 
  scale_y_continuous(breaks = c(0,1,2,3,4,5,6)) + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7)) +
  ggtitle("11-16 mean disease rating by treatment") + 
  ylab("disease rating +/- se") + xlab("week")

# roots
mp.root.scores <- setNames(aggregate(root.scores$score ~ root.scores$treatment,
                                     data = root.scores, FUN = function(x) c(mean=mean(x), se=(sd(x)/sqrt(length(x))))),
                           c("treatment","score"))

mp.root.scores <- setNames(cbind(data.frame(unlist(mp.root.scores$treatment)),
                                 data.frame(unlist(mp.root.scores$score))),
                           c("treatment","score","se"))

root.plot <- ggplot(mp.root.scores,aes(treatment, score))
root.plot + geom_errorbar(aes(ymin = score - se, ymax = score + se, width=0.1)) +
  geom_point(size=2) + 
  scale_y_continuous(breaks = c(0,1,2,3,4,5,6)) + 
  ggtitle("roots mean disease rating by treatment") + 
  ylab("disease rating +/- se") + xlab("treatment") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

count_dead <- function(arg1) {
  return(sum(arg1 == 5.0))
}


# weights

full.weights <- setNames(aggregate(weights$full.plant ~ weights$treatment,
                                   data = weights, FUN = function(x) c(mean=mean(x), sd=sd(x))),
                         c("treatment","weights"))

full.weights <- setNames(cbind(data.frame(unlist(full.weights$treatment)),
                               data.frame(unlist(full.weights$weights))), 
                         c("treatment","mean","sd"))
full.weights$treatment <- factor(full.weights$treatment, levels = c("control", "11-12 20,000", "11-12 100,000",
                                                                    "11-12 500,000","11-12 2,500,000","11-16 20,000",
                                                                    "11-16 100,000","11-16 500,000","11-16 2,500,000"))
full.weights <- full.weights[order(full.weights$treatment),]

root.weights <- setNames(aggregate(weights$roots ~ weights$treatment,
                                   data = weights, FUN = function(x) c(mean=mean(x), sd=sd(x))),
                         c("treatment","weights"))

root.weights <- setNames(cbind(data.frame(unlist(root.weights$treatment)),
                               data.frame(unlist(root.weights$weights))), 
                         c("treatment","mean","sd"))

root.weights$treatment <- factor(root.weights$treatment, levels = c("control", "11-12 20,000", "11-12 100,000",
                                                                    "11-12 500,000","11-12 2,500,000","11-16 20,000",
                                                                    "11-16 100,000","11-16 500,000","11-16 2,500,000"))
root.weights <- root.weights[order(root.weights$treatment),]

top.weights <- setNames(aggregate(weights$top ~ weights$treatment,
                                   data = weights, FUN = function(x) c(mean=mean(x), sd=sd(x))),
                         c("treatment","weights"))

top.weights <- setNames(cbind(data.frame(unlist(top.weights$treatment)),
                               data.frame(unlist(top.weights$weights))), 
                         c("treatment","mean","sd"))

top.weights$treatment <- factor(top.weights$treatment, levels = c("control", "11-12 20,000", "11-12 100,000",
                                                                  "11-12 500,000","11-12 2,500,000","11-16 20,000",
                                                                  "11-16 100,000","11-16 500,000","11-16 2,500,000"))
top.weights <- top.weights[order(top.weights$treatment),]

# weight plots

fw.plot <- ggplot(full.weights,aes(treatment,mean))
fw.plot + geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd, width=0.1)) +
  geom_point(size=2) + 
  #scale_y_continuous(breaks = c(0,1,2,3,4,5,6)) + 
  #scale_x_continuous(breaks = c(1,2,3,4,5,6,7)) +
  ggtitle("Full Plant Weights by Treatment") + 
  ylab("weight +/- sd (g)") + xlab("Treatment") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

rw.plot <- ggplot(root.weights,aes(treatment,mean))
rw.plot + geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd, width=0.1)) +
  geom_point(size=2) + 
  #scale_y_continuous(breaks = c(0,1,2,3,4,5,6)) + 
  #scale_x_continuous(breaks = c(1,2,3,4,5,6,7)) +
  ggtitle("Root Weights by Treatment") + 
  ylab("weight (g)") + xlab("Treatment")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

tw.plot <- ggplot(top.weights,aes(treatment,mean))
tw.plot + geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd, width=0.1)) +
  geom_point(size=2) + 
  #scale_y_continuous(breaks = c(0,1,2,3,4,5,6)) + 
  #scale_x_continuous(breaks = c(1,2,3,4,5,6,7)) +
  ggtitle("Plant Top Weights by Treatment") + 
  ylab("weight (g)") + xlab("Treatment")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# working section
mp.toy <- aggregate(mp.11.12.mortality$status ~ (mp.11.12.mortality$time + mp.11.12.mortality$treatment), 
                    data = mp.11.12.mortality, FUN = function(x) count_dead(x))