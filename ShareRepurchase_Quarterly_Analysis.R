library(data.table)
library(dplyr)
library(haven)
library(stringr)
library(mvtnorm)
library(tidyr)
library(readxl)
library(ggplot2)

options(scipen = 999)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

rm(list = ls())


# Generate Dividend and Repurchase for each gvkey fyearq fqtr

data <- fread("../Data/compustat_quarterly.csv")
# Put the variable gvkey fyearq fqtr dvpy dividend dvp repurchase repurchase_a repurchase_q in front 
data <- data %>%
  select(gvkey, tic, fyearq, fqtr, atq, dvpy, dvp, tstkq, tstk, prstkcy, sstky, prstkc, sstk, naicsh, conm)
# note that quarterly dividend is the difference between the current quarter and the previous quarter amount
# for each given gvkey fyearq
# generate the quarterly dividend amount
data <- data %>%
  mutate(dividend = dvpy - lag(dvpy, 1))
# replace dividend as dvpy if fqtr == 1
data <- data %>%
  mutate(dividend = ifelse(fqtr == 1, dvpy, dividend))
# generate quarterly repurchase amount
data <- data %>%
  mutate(repurchase = tstkq - lag(tstkq, 1))
# replace repurchase as prstkcy - sstky if repurchase is 0 or NA
data <- data %>%
  mutate(repurchase = ifelse(is.na(repurchase), prstkcy - sstky, repurchase))
# set repurchase as 0 if repurchase < 0
data <- data %>%
  mutate(repurchase = ifelse(repurchase < 0, 0, repurchase))

# MERGE in TR 13F data on institutional holdings 
# Read the csv dataset in the current folder
data_tr <- fread("tr_13f.csv")
# check the first few rows
head(data_tr)
# generate fyearq fqtr using rdate
data_tr <- data_tr %>%
  mutate(fyearq = year(rdate),
         fqtr = ifelse(month(rdate) %in% 1:3, 1, ifelse(month(rdate) %in% 4:6, 2, ifelse(month(rdate) %in% 7:9, 3, 4)))
  )
# check if ticker fyearq fqtr is unique in the dataset
data_tr %>%
  group_by(ticker, fyearq, fqtr) %>%
  summarise(total = n()) %>%
  filter(total > 1)
# select the first row by ticker fyearq fqtr in case of duplicates
data_tr <- data_tr %>%
  group_by(ticker, fyearq, fqtr) %>%
  slice(1)

# merge the two datasets using ticker and fyearq-fqtr
setnames(data_tr, "ticker", "tic")
data <- merge(data, data_tr, by = c("tic", "fyearq", "fqtr"), all.x = TRUE)
# check if tic fyearq fqtr is key
data %>%
  select(tic, fyearq, fqtr) %>%
  unique()

# Generate banks dummy == 1 if naics == 522110
data <- data %>%
  mutate(banks = ifelse(naicsh == 522110, 1, 0))
# check the number of banks and nonbanks firms
data %>%
  group_by(banks) %>%
  summarise(total_firms = n())
# replace NA to 0 for share repurchase and dividend
data <- data %>%
  mutate(repurchase = ifelse(is.na(repurchase), 0, repurchase),
         dividend = ifelse(is.na(dividend), 0, dividend))

# divide dividends and share-repurchase by 1,000 to make units $Billion
data <- data %>%
  mutate(dividend = dividend / 1000,
         repurchase = repurchase / 1000)

# Generate the share repurchase dummy == 1 if repurchase> 0
data <- data %>%
  mutate(repurchase_dummy = ifelse(repurchase > 0, 1, 0))
# Generate the dividend dummy == 1 if dividend > 0
data <- data %>%
  mutate(dividend_dummy = ifelse(dividend > 0, 1, 0))

# Generate the total sum of dividends and repurchases for each fyearq-fqtr by banks and nonbanks firms
# as well as the total number of firms that issues dividends or repurchase in that year-quarter by banks dummy
data1 <- data %>%
  group_by(fyearq, banks) %>%
  summarise(total_dividend = sum(dividend, na.rm = TRUE),
            total_repurchase = sum(repurchase, na.rm = TRUE),
            total_firms = n(), 
            total_dividend_firms = sum(dividend_dummy, na.rm = TRUE),
            total_repurchase_firms = sum(repurchase_dummy, na.rm = TRUE))

# plot the times series trend of share repurchase and dividend sums by banks dummy
# in the same ggplot (overlay plot))
data1 %>%
  ggplot(aes(x = fyearq, y = total_repurchase, color = factor(banks))) +
  geom_line() +
  geom_point() +
  labs(title = "Share Repurchase by Banks and Nonbanks Firms",
       x = "Year",
       y = "Total Share Repurchase ($billion)",
       color = "Banks Dummy") +
  theme_minimal() +
  theme(legend.position = "top")
# save the plot as pdf
ggsave("ShareRepurchase_Sum.pdf")

data1 %>%
  ggplot(aes(x = fyearq, y = total_dividend, color = factor(banks))) +
  geom_line() +
  geom_point() +
  labs(title = "Dividends by Banks and Nonbanks Firms",
       x = "Year",
       y = "Total Dividends ($billion)",
       color = "Banks Dummy") +
  theme_minimal() +
  theme(legend.position = "top")
# save the plot as pdf
ggsave("Dividend_Sum.pdf")

# Plot the share of firms that issue dividends or repurchase by banks dummy
data1 %>%
  ggplot(aes(x = fyearq, y = total_repurchase_firms / total_firms, color = factor(banks))) +
  geom_line() +
  geom_point() +
  labs(title = "Share of Firms that Issue Dividends by Banks and Nonbanks Firms",
       x = "Year",
       y = "Share of Firms",
       color = "Banks Dummy") +
  theme_minimal() +
  theme(legend.position = "top")
ggsave("ShareRepurchase_Share.pdf")

data1 %>%
  ggplot(aes(x = fyearq, y = total_dividend_firms / total_firms, color = factor(banks))) +
  geom_line() +
  geom_point() +
  labs(title = "Share of Firms that Issue Dividends by Banks and Nonbanks Firms",
       x = "Year",
       y = "Share of Firms",
       color = "Banks Dummy") +
  theme_minimal() +
  theme(legend.position = "top")
ggsave("Dividend_Share.pdf")

# Check Trends for Specific Banks 
# keep only the top 8 biggest banks using tic
data_bb <- data %>%
  filter(tic %in% c("JPM", "BAC", "C", "WFC", "GS", "MS", "BK", "USB"))
# list of conm
data_bb %>%
  select(conm) %>%
  unique()
# plot the trends again for the sum of repurchase and dividends of top 8 banks
data2 <- data_bb %>%
  group_by(fyearq) %>%
  summarise(total_dividend = sum(dividend, na.rm = TRUE),
            total_repurchase = sum(repurchase, na.rm = TRUE))

data3 <- data_bb %>%
  group_by(tic, fyearq) %>%
  summarise(total_dividend = sum(dividend, na.rm = TRUE),
            total_repurchase = sum(repurchase, na.rm = TRUE))
# plot the times series trend of share repurchase and dividend sums in one figure for the top 8 banks
data3 %>%
  ggplot(aes(x = fyearq, y = total_repurchase, color = factor(tic))) +
  geom_line() +
  geom_point() +
  labs(title = "Share Repurchase by Top 8 Banks",
       x = "Year",
       y = "Total Share Repurchase ($billion)",
       color = "Bank Ticker") +
  theme_minimal() +
  theme(legend.position = "top")
ggsave("ShareRepurchase_Top8.pdf")

# plot the time series trend of share repurchase and dividend sums in one figure for the top 8 banks
# Use legend to show that one series is total_dividend, the other is total repurchase
# reshape to long format first
data2 <- data2 %>%
  gather(key = "method", value = "value", -fyearq)
data2 %>%
  ggplot(aes(x = fyearq, y = value, color = method)) +
  geom_line() +
  geom_point() +
  labs(title = "Dividends and Share Repurchase by Top 8 Banks",
       x = "Year",
       y = "Total Dividends and Share Repurchase ($billion)",
       color = "Method") +
  theme_minimal() +
  theme(legend.position = "top")
ggsave("Dividend_ShareRepurchase_Top8.pdf")

# Generate dummy variable for the top 8 banks "JPM", "BAC", "C", "WFC", "GS", "MS", "BK", "USB"
data <- data %>%
  mutate(top8 = ifelse(tic %in% c("JPM", "BAC", "C", "WFC", "GS", "MS", "BK", "USB"), 1, 0))
# keep only the banks
data_bank <- data %>%
  filter(banks == 1)
# Generate dummy variable for the top 25 banks in each fyearq fqtr by atq 
data_bank <- data_bank %>%
  group_by(fyearq, fqtr) %>%
  mutate(rank = rank(-atq)) %>%
  ungroup() %>%
  mutate(top25 = ifelse(rank <= 25, 1, 0))
# generate total dividend and repurchase for the top 8 banks by fyearq
data_bank_top8 <- data_bank %>%
  group_by(fyearq, top8) %>%
  summarise(total_dividend = sum(dividend, na.rm = TRUE),
            total_repurchase = sum(repurchase, na.rm = TRUE),
            total_firms = n(), 
            total_dividend_firms = sum(dividend_dummy, na.rm = TRUE),
            total_repurchase_firms = sum(repurchase_dummy, na.rm = TRUE))
data_bank_top25 <- data_bank %>%
  group_by(fyearq, top25) %>%
  summarise(total_dividend = sum(dividend, na.rm = TRUE),
            total_repurchase = sum(repurchase, na.rm = TRUE),
            total_firms = n(), 
            total_dividend_firms = sum(dividend_dummy, na.rm = TRUE),
            total_repurchase_firms = sum(repurchase_dummy, na.rm = TRUE))

# plot the total dividend and repurchase for the top 8 banks by fyearq
data_bank_top8 %>%
  ggplot(aes(x = fyearq, y = total_repurchase, color = factor(top8))) +
  geom_line() +
  geom_point() +
  labs(title = "Share Repurchase by Top 8 Banks",
       x = "Year",
       y = "Total Share Repurchase ($billion)",
       color = "Top 8 Banks Dummy") +
  theme_minimal() +
  theme(legend.position = "top")
ggsave("ShareRepurchase_byTop8.pdf")

# plot the total dividend and repurchase for the top 25 banks by fyearq
data_bank_top25 %>%
  ggplot(aes(x = fyearq, y = total_repurchase, color = factor(top25))) +
  geom_line() +
  geom_point() +
  labs(title = "Share Repurchase by Top 25 Banks",
       x = "Year",
       y = "Total Share Repurchase ($billion)",
       color = "Top 25 Banks Dummy") +
  theme_minimal() +
  theme(legend.position = "top")
ggsave("ShareRepurchase_byTop25.pdf")
