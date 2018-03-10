# import library
library(PopGenome)

# import data
gen <- (readVCF("vcf/sub3.vcf.gz", include.unknown = TRUE, frompos = 1, 
                topos = 10049, tid = "1", numcols = 10000))

# set populations
bhhz <- as.character(read.table("populations/bhhz.txt", sep = ",")[[1]])
hznearmofo3003 <- as.character(read.table("populations/hznearmofo3003.txt", sep = ",")[[1]])
hznearmost005 <- as.character(read.table("populations/hznearmost005.txt", sep = ",")[[1]])
mofo3001nn <- as.character(read.table("populations/mofo3001nn.txt", sep = ",")[[1]])
mofo3002 <- as.character(read.table("populations/mofo3002.txt", sep = ",")[[1]])
mofo3003 <- as.character(read.table("populations/mofo3005.txt", sep = ",")[[1]])
mofo3009 <- as.character(read.table("populations/mofo3009.txt", sep = ",")[[1]])
lfo <- as.character(read.table("populations/lfo.txt", sep = ",")[[1]])
mosh <- as.character(read.table("populations/mosh.txt", sep = ",")[[1]])
most001 <- as.character(read.table("populations/most001.txt", sep = ",")[[1]])
most003 <- as.character(read.table("populations/most003.txt", sep = ",")[[1]])
most004 <- as.character(read.table("populations/most004.txt", sep = ",")[[1]])
most005 <- as.character(read.table("populations/most005.txt", sep = ",")[[1]])
rhhz <- as.character(read.table("populations/rhhz.txt", sep = ",")[[1]])

poplist <- list(bhhz, hznearmofo3003, hznearmost005, mofo3001nn, mofo3002, 
                mofo3003, mofo3009, lfo, mosh, most001, most003, most004, most005,
                rhhz)

gen <- set.populations(gen, poplist, diploid=TRUE)

# get stats
gen <- diversity.stats(gen, pi = TRUE)
gen <- diversity.stats.between(gen)

# get per site stats
Pi.per.site <- gen@Pi / gen@n.sites
Pi.per.site <- data.frame(Pi.per.site)
dxy.per.site <- gen@nuc.diversity.between / gen@n.sites
dxy.per.site <- data.frame(dxy.per.site)

# print stats
gen@Pi
gen@nuc.diversity.between
Pi.per.site
dxy.per.site

# write to file
write.table(dxy.per.site, file = "dxy.csv", sep = ",")
write.table(Pi.per.site, file = "pi.csv", sep = ",")