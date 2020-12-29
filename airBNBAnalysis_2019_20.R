library(tidyverse)
library(esquisse)
library(readr)

##################### 2020

# ---- Loading and cleaning initial dataset - 2020 ----


df_2020 <- read_csv("C:/Users/juan.vasquez/OneDrive - Tremor International/01 PROJECTS/JCV - Legwork/Liveramp/listings-2020.csv")


df_2020$host_id <- as.factor(df_2020$host_id)
df_2020$host_is_superhost <- ifelse(df_2020$host_is_superhost == TRUE, 1, 0)
df_2020$has_license <- ifelse(!is.na(df_2020$license), 1, 0)
df_2020$host_is_multihost <- ifelse(df_2020$host_total_listings_count > 1, 1, 0)
df_2020$price <- as.numeric(gsub('[$.]', '', df_2020$price)) / 100
df_2020$revenueABNB <- df_2020$price * 0.03
df_2020$future_occupancy <- 365 - df_2020$availability_365
df_2020$revenueABNB_Booked <- df_2020$revenueABNB * df_2020$future_occupancy
df_2020 <- df_2020[!is.na(df_2020$host_is_multihost),]
df_2020$d_requires_license <- ifelse(df_2020$host_is_multihost == 1 || (df_2020$host_is_multihost == 0 && df_2020$room_type != "Private room"), 1, 0)


sum(df_2020$has_license)

colnames2020 <- colnames(df_2020)

ul_2020 <- df_2020[df_2020$has_license == 0,]


# ---- Summary counts for license mismatch in total marketplace, superhosts, and multi-hosts ----

#Total marketplace

sum(df_2020$d_requires_license)
sum(df_2020$has_license)

sum(df_2020$has_license) / sum(df_2020$d_requires_license) # % of total listings with license


#Superhosts

shosts_2020 <- df_2020[df_2020$host_is_superhost == 0, ]

sum(shosts_2020$has_license, na.rm = TRUE)
nrow(shosts_2020)
sum(shosts_2020$has_license, na.rm = TRUE) / nrow(shosts_2020)



#Multihosts

mhosts_2020 <- df_2020[df_2020$host_is_superhost == 1, ]

sum(mhosts_2020$has_license, na.rm = TRUE)
nrow(mhosts_2020)
sum(mhosts_2020$has_license, na.rm = TRUE) / nrow(mhosts_2020)





##################### 2019  ##########################
# ---- Loading and cleaning initial dataset - 2019 ----


df_2019 <- read_csv("C:/Users/juan.vasquez/OneDrive - Tremor International/01 PROJECTS/JCV - Legwork/Liveramp/listings-2019.csv")


df_2019$host_id <- as.factor(df_2019$host_id)
df_2019$host_is_superhost <- ifelse(df_2019$host_is_superhost == TRUE, 1, 0)
df_2019$has_license <- ifelse(!is.na(df_2019$license), 1, 0)
df_2019$host_is_multihost <- ifelse(df_2019$host_total_listings_count > 1, 1, 0)

df_2019$price <- as.numeric(gsub('[$.]', '', df_2019$price)) / 100
df_2019$revenueABNB <- df_2019$price * 0.03
df_2019$future_occupancy <- 365 - df_2019$availability_365
df_2019$revenueABNB_Booked <- df_2019$revenueABNB * df_2019$future_occupancy
df_2019 <- df_2019[!is.na(df_2019$host_is_multihost),]


df_2019$d_requires_license <- ifelse(df_2019$host_is_multihost == 1 || (df_2019$host_is_multihost == 0 && df_2019$room_type != "Private room"), 1, 0)

colnames2019 <- colnames(df_2019)
sum(df_2019$has_license)

ul_2019 <- df_2019[df_2019$has_license == 0,]


# ---- Summary counts for license mismatch in total marketplace, superhosts, and multi-hosts ----

#Total marketplace

sum(df_2019$d_requires_license)
sum(df_2019$has_license)

sum(df_2019$has_license) / sum(df_2019$d_requires_license) # % of total listings with license


#Shosts

shosts_2019 <- df_2019[df_2019$host_is_superhost == 0, ]

sum(shosts_2019$has_license, na.rm = TRUE)
nrow(shosts_2019)
sum(shosts_2019$has_license, na.rm = TRUE) / nrow(shosts_2019)



#Multihosts

mhosts_2019 <- df_2019[df_2019$host_is_superhost == 1, ]

sum(mhosts_2019$has_license, na.rm = TRUE)
nrow(mhosts_2019)
sum(mhosts_2019$has_license, na.rm = TRUE) / nrow(mhosts_2019)




