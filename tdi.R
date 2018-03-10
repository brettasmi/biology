options(digits = 10)

# import data
mdd <- read.csv('Medicare_Provider_Utilization_and_Payment_Data__Part_D_Prescriber_Summary_Table_CY2014.csv')

# overall mean benefits count
mean(mdd$bene_count, na.rm = TRUE)

# median length prescriptions
new.col <- mdd$total_day_supply / mdd$total_claim_count
median(new.col)

# standard deviation of ratio brand name/generic
mddq3 <- mdd[mdd$brand_suppress_flag == "", ]

all.spec.brand <- setNames(aggregate(mddq3$brand_claim_count ~ mddq3$specialty_description, 
                               data = mddq3, FUN = function(x) sum = sum(x)),
                     c("Sample", "Brand Claim Sum"))

all.spec.total <- setNames(aggregate(mddq3$total_claim_count ~ mddq3$specialty_description, 
                                     data = mddq3, FUN = function(x) sum = sum(x)),
                           c("Sample", "Total Claim Sum"))

all.spec.data <- setNames(cbind(data.frame(unlist(all.spec.brand$Sample)),
                                data.frame(unlist(all.spec.brand$"Brand Claim Sum")),
                                data.frame(unlist(all.spec.total$"Total Claim Sum"))),
                          c("Sample", "Brand", "Total"))
all.spec.data <- all.spec.data[all.spec.data$Total >= 1000, ]
all.spec.data <- setNames(cbind(all.spec.data[, ], 
                                all.spec.data$Brand / all.spec.data$Total),
                          c("Sample", "Brand", "Total", "Ratio"))
sd(all.spec.data$Ratio)

# diff b/w max and min state opioid prescriptions

all.state.opioid <- setNames(aggregate(mdd$opioid_bene_count ~ mdd$nppes_provider_state, 
                                       data = mdd, FUN = function(x) sum = sum(x)),
                             c("State", "Opioid Beneficiaries"))

all.state.antibi <- setNames(aggregate(mdd$antibiotic_bene_count ~ mdd$nppes_provider_state,
                                       data = mdd, FUN = function(x) sum = sum(x)),
                             c("State", "Antibiotic Beneficiaries"))

all.state.data <- setNames(cbind(data.frame(unlist(all.state.antibi$State)),
                                 data.frame(unlist(all.state.opioid$`Opioid Beneficiaries`)),
                                 data.frame(unlist(all.state.antibi$`Antibiotic Beneficiaries`))),
                           c("State", "Opioid", "Antibiotic"))
all.state.data <- setNames(cbind(all.state.data[, ], 
                                 all.state.data$Opioid / all.state.data$Antibiotic),
                           c("State", "Opioid", "Antibiotic", "Ratio"))

max(all.state.data$Ratio) - min(all.state.data$Ratio)

# correlation between 

mdd5 <- mdd[mdd$ge65_suppress_flag == "", ]
mdd5 <- mdd5[mdd5$lis_suppress_flag == "", ]

mdd5 <- setNames(cbind(mdd5[, ], 
                         mdd5$total_claim_count_ge65 / mdd5$total_claim_count),
                   c(colnames(mdd5), "ge65_ratio"))

mdd5 <- setNames(cbind(mdd5[, ], 
                         mdd5$lis_claim_count / mdd5$total_claim_count),
                   c(colnames(mdd5), "lis_ratio"))

cor(mdd5$ge65_ratio, mdd5$lis_ratio, method = "pearson")

# specialty-state:national avg ratio of opiod prescriptions

mdd6 <- setNames(cbind(mdd[, ], mdd$opioid_day_supply / mdd$opioid_claim_count),
                 c(colnames(mdd), "mean_opioid_len"))

mdd6a <- setNames(aggregate(mdd6$mean_opioid_len ~ 
                            mdd6$nppes_provider_state + mdd6$specialty_description,
                            data = mdd6, FUN = function(x) mean = mean(x)),
                  c("State", "Specialty", "mean_opiod_len"))

mdd6b <- setNames(aggregate(cbind(count = mdd6$mean_opioid_len) ~ 
                              mdd6$nppes_provider_state + mdd6$specialty_description,
                            data = mdd6, FUN = function(x) nrow = NROW(x)),
                  c("State", "Specialty", "count"))
mdd6a <- setNames(cbind(mdd6a[, ], mdd6b$count),c(colnames(mdd6a), "count"))
mdd6a <- mdd6a[mdd6a$count>=100, ]
mdd6c <- setNames(aggregate(mdd6$mean_opioid_len ~ mdd6$specialty_description,
                            data = mdd6, function(x) mean = mean(x)), c("Specialty", "mean"))
mdd6a$nat_avg <- mdd6c[match(mdd6a$Specialty, mdd6c$Specialty), 2]
mdd6a$ratio <- mdd6a$mean_opiod_len / mdd6a$nat_avg
max(mdd6a$ratio)

# avg rate of drug cost inflation 2013->2014
mdd7 <- mdd[mdd$total_drug_cost != "", ]
mdd7$average_daily_cost <- (mdd7$total_drug_cost / mdd7$total_claim_count) / 365
mdd2013 <- read.csv("Medicare_Provider_Utilization_and_Payment_Data__Part_D_Prescriber_Summary_Table_CY2013.csv")
mdd2013$Total.Drug.Cost <- as.numeric(gsub("$", "", mdd2013$Total.Drug.Cost, fixed = TRUE))
mdd2013.7 <- mdd2013[mdd2013$Total.Drug.Cost != "", ]
mdd2013.7$average_daily_cost <- (mdd2013.7$Total.Drug.Cost / mdd2013.7$Total.Claim.Count) / 365
mdd7$average_daily_cost_2013 <- mdd2013.7[match(mdd7$npi, 
                                                mdd2013.7$National.Provider.Identifier),
                                          "average_daily_cost"]
mdd7$inflation_rate <- ((mdd7$average_daily_cost - mdd7$average_daily_cost_2013)
                        / mdd7$average_daily_cost_2013)
mean(mdd7$inflation_rate, na.rm = TRUE)

# max percent change specialty type
mdd8.14 <- mdd[mdd$specialty_description != "", ]
mdd8.13 <- mdd2013[mdd2013$Provider.Specialty.Type != "", ]
mdd8.14a <- setNames(aggregate(cbind(count = mdd8.14$npi) ~ mdd8.14$specialty_description,
                               data = mdd8.14, function(x) nrow = NROW(x)),
                     c("Specialty", "Count"))
mdd8.14a <- mdd8.14a[mdd8.14a$Count >= 1000, ]
mdd8.13a <- setNames(aggregate(cbind(count = mdd8.13$National.Provider.Identifier) ~ 
                                mdd8.13$Provider.Specialty.Type, data = mdd8.13,
                               function(x) nrow = NROW(x)), c("Specialty", "Count"))
mdd8.13a <- mdd8.13a[mdd8.13a$Count >= 1000, ]
mdd8.14a$Count_2013 <- mdd8.13a[match(mdd8.14a$Specialty, mdd8.13a$Specialty),
                                "Count"]
mdd8.14a$fraction <- (mdd8.14a$Count_2013 - mdd8.14a$Count) / mdd8.14a$Count_2013
max(mdd8.14a$fraction)
