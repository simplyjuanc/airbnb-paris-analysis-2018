# ---- Loading and cleaning initial dataset ----

library(tidyverse)
library(readr)

df <- read_csv("C:/Users/juan.vasquez/OneDrive - Tremor International/01 PROJECTS/JCV - Legwork/Liveramp/Paris Airbnb Listings Final.csv")
df_clean <- df[,-c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,21,22,24,25,28,30,31,32,33,38,41,42,43,45,46,47,48,51,76,91)]
rm(df)


df_clean$host_id <- as.factor(df_clean$host_id)
df_clean$host_is_superhost <- ifelse(df_clean$host_is_superhost == TRUE, 1, 0)
df_clean$has_license <- ifelse(!is.na(df_clean$license), 1, 0)
df_clean$host_is_multihost <- ifelse(df_clean$host_total_listings_count > 1, 1, 0)
df_clean$revenueABNB <- df_clean$price * 0.03
df_clean$future_occupancy <- 365 - df_clean$availability_365
df_clean$revenueABNB_Booked <- df_clean$revenueABNB * df_clean$future_occupancy
df_clean <- df_clean[!is.na(df_clean$host_is_multihost),]

df_clean$d_requires_license <- ifelse(df_clean$host_is_multihost == 1 || (df_clean$host_is_multihost == 0 && df_clean$room_type != "Private room"), 1, 0)



# ---- Summary counts for license mismatch in total marketplace, shosts, and multi-hosts ----

shosts <- df_clean[df_clean$host_is_multihost == 0, ]
mhosts <- df_clean[df_clean$host_is_multihost == 1, ]


#Total marketplace



sum(df_clean$requires_license)
sum(df_clean$has_license)

sum(df_clean$has_license) / sum(df_clean$requires_license)

  

#Shosts


sum(shosts$requires_license, na.rm = TRUE)
sum(shosts$has_license, na.rm = TRUE)

sum(shosts$has_license, na.rm = TRUE) / sum(shosts$requires_license, na.rm = TRUE)


#Mhosts



sum(mhosts$requires_license, na.rm = TRUE)
sum(mhosts$has_license, na.rm = TRUE)

sum(mhosts$has_license, na.rm = TRUE) / sum(mhosts$requires_license, na.rm = TRUE)





# ---- Revenue risk calculation for different host types ----

# Focussing on different hosts - multi and single

library(ggplot2)
library(scales)


ul <- df_clean[df_clean$has_license == 0,]
ul$host_is_multihost <- as.factor(ul$host_is_multihost)
ul$host_is_multihost <- factor(ul$host_is_multihost, levels = c("Single-host", "Multi-host"))

host_labels <- c("Single-listing Host", "Multi-listing Host")

ggplot(ul) +
  aes(x = host_is_multihost, weight = revenueABNB_Booked) +
  geom_bar(fill = "#0c4c8a") +
  labs(x = "Type of host ", y = "Revenue at risk on booked listings") +
  theme_classic() +
  scale_x_discrete(labels = host_labels) +
  scale_y_continuous(labels = unit_format(scale = 0.000001, prefix = "€", unit = "m", trim = FALSE), expand = c(0,0), limits = c(0,30000000))




ul_mhosts <- ul[ul$host_is_multihost == 1,]
ul_shosts <- ul[ul$host_is_multihost == 0,]

# % of rev at risk from shosts
sum(ul_shosts$revenueABNB_Booked) / (sum(ul_shosts$revenueABNB_Booked) + sum(ul_mhosts$revenueABNB_Booked))


# ---- Incorporate data from 2020 and 2019 ----



# ---- Revenue risk comparison between cohorts ---- 

# Set dummy variable and merge according to set of common columns

ul$dataSetIndicator <- 2018
ul_2019$dataSetIndicator <- 2019
ul_2020$dataSetIndicator <- 2020


colnames2018 <- colnames(ul)
colnames2019 <- colnames(ul_2019)
colnames2020 <- colnames(ul_2020)
common_cols <- Reduce(intersect, list(colnames2018,colnames2019,colnames2020))


ul_all <- rbind(
  subset(ul, select = common_cols),
  subset(ul_2019, select = common_cols),
  subset(ul_2020, select = common_cols)
)


# Obtain 'YoY' comparison for revenue numbers per type of host

revRisk2018 <- sum(ul_all[ul_all$dataSetIndicator == 2018,]$revenueABNB_Booked, na.rm = TRUE) #Total rev at risk in 2018
revRisk2019 <-sum(ul_all[ul_all$dataSetIndicator == 2019,]$revenueABNB_Booked, na.rm = TRUE) #Total rev at risk in 2019
revRisk2020 <-sum(ul_all[ul_all$dataSetIndicator == 2020,]$revenueABNB_Booked, na.rm = TRUE) #Total rev at risk in 2020

revRisk2020 - revRisk2018



# Create YoY comparison plot

library(reshape)


ul_all$dataSetIndicator <- as.factor(ul_all$dataSetIndicator)

ul_all_melt <- ul_all[,c("host_is_multihost","revenueABNB_Booked","dataSetIndicator")]
ul_all_melt <- melt(ul_all_melt, id=c("host_is_multihost","dataSetIndicator"))

# bar chart with same info as ggplot below
# ggplot(ul_all_melt) +
#   aes(x = host_is_multihost, fill = dataSetIndicator, group = dataSetIndicator, weight = revenueABNB_Booked) +
#   geom_bar(position = "dodge") +
#   scale_fill_manual(values = c("#0c4c8a", "#4eb8e1", "#74c068")) +
#   labs(x = "Type of host", y = "Revenue at risk on booked listings", fill = "Year") +
#   scale_x_discrete(labels = c("Single-listing host", "Multi-listing host")) +
#   theme_classic() +
#   scale_y_continuous(labels = unit_format(scale = 0.000001, prefix = "$", unit = "m", trim = FALSE), expand = c(0,0), limits = c(0, 30000000))

compYearlyRev <- aggregate(ul_all_melt$revenueABNB_Booked, by = list(ul_all_melt$host_is_multihost, ul_all_melt$dataSetIndicator), FUN = "sum", na.rm = TRUE) #table with all revs at risk by year and host type


colnames(compYearlyRev) <- c("Type of host", "Year", "Revenue at risk")
compYearlyRev$`Type of host` <- ifelse(compYearlyRev$`Type of host` == 0, "Single-host", "Multi-host")
compYearlyRev$`Type of host` <- as.factor(compYearlyRev$`Type of host`)


compYearlyRev %>% ggplot(aes(x = Year, y = `Revenue at risk`, fill = `Type of host`)) +
  geom_area(aes(group = `Type of host`, fill = `Type of host`)) +
  theme_classic() +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(labels = unit_format(scale = 0.000001, prefix = "€", unit = "m", trim = FALSE), expand = c(0,0), limits = c(0, 40000000)) +
  scale_fill_manual(values=c("#0c4c8a", "#4eb8e1"))



# ---- Calculations on number of changes in listings licensed v unlicensed  ----


l_2018 <- df_clean[df_clean$has_license == 1,]
l_2019 <- df_2019[df_2019$has_license == 1,]
l_2020 <- df_2020[df_2020$has_license == 1,]



ulHosts2019 <- ul[ul$id %in% ul_2019$id,] # ul that have not acquired licenses (2019)
licHosts2019 <- ul[ul$id %in% l_2019$id,] # lic_props that have acquired licenses (2019)

licHosts2020 <- ul[ul$id %in% l_2020$id,] # ul that have acquired licenses (2020)
ulHosts2020 <- ul[ul$id %in% ul_2020$id,] # ul that have not acquired licenses (2020)
ul_lostListings <- ul[!(ul$id %in% df_2020$id),] # ul props gone offline 


unlicHosts2020 <- l_2018[l_2018$id %in% l_2020$id,] #properties that have kept license
unlicNewHosts2020 <- l_2018[l_2018$id %in% ul_2020$id,] #properties that have lost license
l_lostListings <- l_2018[!(l_2018$id %in% df_2020$id),] #licensed properties gone offline


nrow(l_2020[!(l_2020$id %in% df_clean$id),]) # number of licensed props that didn't exist before
nrow(ul_2020[!(ul_2020$id %in% df_clean$id),]) # number of unlicensed props that didn't exist before



 
# licHosts2020$situation <- "UL-UL"
# ulHosts2020$situation <- "UL-L"
# ul_lostListings$situation <- "UL-OF"
# 
# unlicHosts2020$situation <- "L-L"
# unlicNewHosts2020$situation <- "L-UL"
# l_lostListings$situation <- "L-OD"
# 
# licHosts2020_cols <- colnames(licHosts2020)
# ulHosts2020_cols <- colnames(ulHosts2020)
# ul_lostListings_cols <- colnames(ul_lostListings)
# 
# unlicHosts2020_cols <- colnames(unlicHosts2020)
# unlicNewHosts2020_cols <- colnames(unlicNewHosts2020)
# l_lostListings_cols <- colnames(l_lostListings)
# 
# 
# common_cols <- Reduce(intersect, list(licHosts2020_cols,ulHosts2020_cols,ul_lostListings_cols,unlicHosts2020_cols,unlicNewHosts2020_cols,l_lostListings_cols))
# 
# 
# 
# df_chisquare <- rbind(
#   subset(licHosts2020, select = common_cols),
#   subset(ulHosts2020, select = common_cols),
#   subset(ul_lostListings, select = common_cols),
#   subset(unlicHosts2020, select = common_cols),
#   subset(unlicNewHosts2020, select = common_cols),
#   subset(l_lostListings, select = common_cols)
# )



# ---- Zooming in on UL-Ul and UL-L groups ----
library(rstatix)
library(emmeans)
library(ggpubr)



licHosts2020$anova_group <- 2 # ul that have acquired licenses (2020)
ulHosts2020$anova_group <- 1 # ul that have not acquired licenses (2020)
ul_lostListings$anova_group <- 0 # ul props gone offline 

anova_df <- rbind(licHosts2020,ulHosts2020,ul_lostListings)

anova_df$anova_group <- as.factor(anova_df$anova_group)
anova_df$host_is_multihost <- as.factor(anova_df$host_is_multihost)

anova_df <- anova_df[!is.na(anova_df$price),]


#eliminating outliers for price

price_Q <- quantile(anova_df$price, probs=c(.25, .75), na.rm = TRUE)
price_iqr <- IQR(anova_df$price, na.rm = TRUE)
price_up <-  price_Q[2]+1.5*price_iqr # Upper Range  
price_low<- price_Q[1]-1.5*price_iqr # Lower Range

price_df <- subset(anova_df, anova_df$price > (price_Q[1] - 1.5*price_iqr) & anova_df$price < (price_Q[2]+1.5*price_iqr))

#checking significance of relationships

aov_price <- aov(data = price_df, price ~ anova_group + host_is_multihost)
summary(aov_price)
TukeyHSD(aov_price)

# plotting price

ggplot(price_df) +
  aes(x = anova_group, y = price, fill = host_is_multihost) +
  geom_boxplot() +
  scale_fill_hue() +
  labs(x = "Current status", y = "Price per night", fill = "Host type") +
  scale_fill_manual(labels = c("Single-host", "Multi-host"),values = c("#0c4c8a", "#4eb8e1")) +
  scale_x_discrete(labels = c("Offline", "Unlicensed", "Licensed")) +
  theme_classic()




#eliminating outliers for month_rev

month_rev_Q <- quantile(anova_df$reviews_per_month, probs=c(.25, .75), na.rm = TRUE)
month_rev_iqr <- IQR(anova_df$reviews_per_month, na.rm = TRUE)
month_rev_up <-  month_rev_Q[2]+1.5*month_rev_iqr # Upper Range  
month_rev_low<- month_rev_Q[1]-1.5*month_rev_iqr # Lower Range

month_rev_df <- subset(anova_df, anova_df$reviews_per_month > (month_rev_Q[1] - 1.5*month_rev_iqr) & anova_df$reviews_per_month < (month_rev_Q[2]+1.5*month_rev_iqr))

#checking significance of relationships

aov_month_rev_aov <- aov(data = anova_df, reviews_per_month ~ anova_group + host_is_multihost)
summary(aov_month_rev_aov)
TukeyHSD(aov_month_rev_aov)


# plotting month_rev

ggplot(month_rev_df) +
  aes(x = anova_group, y = reviews_per_month, fill = host_is_multihost) +
  geom_boxplot() +
  scale_fill_hue() +
  labs(x = "Current status", y = "Monthly reviews", fill = "Host type") +
  scale_fill_manual(labels = c("Single-host", "Multi-host"),values = c("#0c4c8a", "#4eb8e1")) +
  scale_x_discrete(labels = c("Offline", "Unlicensed", "Licensed")) +
  theme_classic()




#eliminating outliers for month_rev

rev_score_Q <- quantile(anova_df$review_scores_rating, probs=c(.25, .75), na.rm = TRUE)
rev_score_iqr <- IQR(anova_df$review_scores_rating, na.rm = TRUE)
rev_score_up <-  rev_score_Q[2]+1.5*rev_score_iqr # Upper Range  
rev_score_low<- rev_score_Q[1]-1.5*rev_score_iqr # Lower Range

rev_score_df <- subset(anova_df, anova_df$review_scores_rating > (rev_score_Q[1] - 1.5*rev_score_iqr) & anova_df$review_scores_rating < (rev_score_Q[2]+1.5*rev_score_iqr))

#checking significance of relationships

aov_rev_score <- aov(data = rev_score_df, review_scores_rating ~ anova_group + host_is_multihost)
summary(aov_rev_score)
TukeyHSD(aov_rev_score)


# plotting rev_score

ggplot(rev_score_df) +
  aes(x = anova_group, y = review_scores_rating, fill = host_is_multihost) +
  geom_boxplot() +
  scale_fill_hue() +
  labs(x = "Current status", y = "Review Scores", fill = "Host type") +
  scale_fill_manual(labels = c("Single-host", "Multi-host"),values = c("#0c4c8a", "#4eb8e1")) +
  scale_x_discrete(labels = c("Offline", "Unlicensed", "Licensed")) +
  theme_classic()



# ---- Enforcement mechanisms ----



ul_2020 <- df_2020[df_2020$has_license == 0,]

listings_old <- df_2019[df_2019$id %in% ul_2020$id,]
listings_old <- listings_old[is.na(listings_old$last_review),]


sum(listings_old$host_is_multihost)

listings_old_mh <- listings_old[listings_old$host_is_multihost ==1,]
listings_old_sh <- listings_old[listings_old$host_is_multihost ==0,]




length(unique(listings_old_mh$host_id))
length(unique(listings_old_sh$host_id))
