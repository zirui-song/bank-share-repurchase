library(data.table)
library(dplyr)
library(haven)
library(stringr)
library(mvtnorm)
library(tidyr)
library(readxl)
library(ggplot2)
library(patchwork) # To display 2 charts together
library(hrbrthemes) # For ggplot2 themes

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
data_tr <- fread("../Data/tr_13f.csv")
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

# replace NA to 0 for share repurchase and dividend
data <- data %>%
  mutate(repurchase = ifelse(is.na(repurchase), 0, repurchase),
         dividend = ifelse(is.na(dividend), 0, dividend))

# keep only the firms with naics == 522110
data <- data %>%
  filter(naicsh == 522110)

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

# Generate the total sum of dividends and repurchases for each fyearq-fqtr
# as well as the total number of firms that issues dividends or repurchase in that year-quarter
data1 <- data %>%
  group_by(fyearq) %>%
  summarise(total_dividend = sum(dividend, na.rm = TRUE),
            total_repurchase = sum(repurchase, na.rm = TRUE),
            total_firms = n(), 
            total_dividend_firms = sum(dividend_dummy, na.rm = TRUE),
            total_repurchase_firms = sum(repurchase_dummy, na.rm = TRUE),
            )

# plot the times series trend of share repurchase and dividend sums in the same ggplot (overlay plot))
# keep only total dividend and total repurchase
total_div_repur <- select(data1, c('fyearq','total_dividend','total_repurchase')) %>%
  gather(key = "method", value = "value", -fyearq)
total_div_repur %>%
  ggplot(aes(x = fyearq, y = value, color = method)) +
  geom_line() +
  geom_point() +
  labs(title = "Total Dividends and Share Repurchase by Commercial Banks",
       x = "Year",
       y = "Total Dividends and Share Repurchase ($billion)",
       color = "Method") +
  theme_minimal() +
  theme(legend.position = "top")
# save to Results folder as pdf
ggsave("../Results/Total_Div_Repur_Banks.pdf")

# Plot the share of firms that issue dividends or repurchase using the same method as above
# generate share of firms that issue dividends or repurchase
data1 <- data1 %>%
  mutate(share_dividend_firms = total_dividend_firms / total_firms,
         share_repurchase_firms = total_repurchase_firms / total_firms)

share_div_repur <- select(data1, c('fyearq','share_dividend_firms','share_repurchase_firms')) %>%
  gather(key = "method", value = "value", -fyearq)
share_div_repur %>%
  ggplot(aes(x = fyearq, y = value, color = method)) +
  geom_line() +
  geom_point() +
  labs(title = "Share of Banks that Issue Dividends and Share Repurchase",
       x = "Year",
       y = "Total Share of Banks that Issue Dividends and Share Repurchase",
       color = "Method") +
  theme_minimal() +
  theme(legend.position = "top")
# save to Results folder as pdf
ggsave("../Results/Share_Div_Repur_Banks.pdf")

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
ggsave("../Results/ShareRepurchase_Top8.pdf")

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
ggsave("../Results/Dividend_ShareRepurchase_Top8.pdf")

# Generate dummy variable for the top 8 banks "JPM", "BAC", "C", "WFC", "GS", "MS", "BK", "USB"
data <- data %>%
  mutate(top8 = ifelse(tic %in% c("JPM", "BAC", "C", "WFC", "GS", "MS", "BK", "USB"), 1, 0))

# Generate dummy variable for the top 25 banks in each fyearq fqtr by atq of the quarter before
data <- data %>%
  group_by(fyearq, fqtr) %>%
  mutate(rank = rank(-atq)) %>%
  ungroup() %>%
  mutate(top25 = ifelse(rank <= 25, 1, 0))
# generate new variable top25_prev_quarter that equals 1 if the bank is in the top 25 in the previous quarter
data <- data %>%
  group_by(tic) %>%
  mutate(top25_prev_quarter = lag(top25, 1)) %>%
  ungroup() %>%
  mutate(top25_prev_quarter = ifelse(is.na(top25_prev_quarter), 0, top25_prev_quarter))
# generate total dividend and repurchase for the top 8 banks by fyearq
data_top8 <- data %>%
  group_by(fyearq, top8) %>%
  summarise(total_dividend = sum(dividend, na.rm = TRUE),
            total_repurchase = sum(repurchase, na.rm = TRUE),
            total_firms = n(), 
            total_dividend_firms = sum(dividend_dummy, na.rm = TRUE),
            total_repurchase_firms = sum(repurchase_dummy, na.rm = TRUE))
data_top25 <- data %>%
  group_by(fyearq, top25_prev_quarter) %>%
  summarise(total_dividend = sum(dividend, na.rm = TRUE),
            total_repurchase = sum(repurchase, na.rm = TRUE),
            total_firms = n(), 
            total_dividend_firms = sum(dividend_dummy, na.rm = TRUE),
            total_repurchase_firms = sum(repurchase_dummy, na.rm = TRUE))

# plot the total dividend and repurchase for the top 8 banks by fyearq
data_top8 %>%
  ggplot(aes(x = fyearq, y = total_repurchase, color = factor(top8))) +
  geom_line() +
  geom_point() +
  labs(title = "Share Repurchase by Top 8 Banks",
       x = "Year",
       y = "Total Share Repurchase ($billion)",
       color = "Top 8 Banks Dummy") +
  theme_minimal() +
  theme(legend.position = "top")
ggsave("../Results/ShareRepurchase_byTop8.pdf")

# plot the total dividend and repurchase for the top 25 banks by fyearq
data_top25 %>%
  ggplot(aes(x = fyearq, y = total_repurchase, color = factor(top25_prev_quarter))) +
  geom_line() +
  geom_point() +
  labs(title = "Share Repurchase by Top 25 Banks",
       x = "Year",
       y = "Total Share Repurchase ($billion)",
       color = "Top 25 Banks Dummy") +
  theme_minimal() +
  theme(legend.position = "top")
ggsave("../Results/ShareRepurchase_byTop25.pdf")

### New Plots with Institutional Holdings Overlayed with Total Dividends and Share Repurchase

# keep only those observations with valid institutional holdings (non-NA InstOwn_Perc)
data_inst <- data %>%
  filter(!is.na(InstOwn_Perc))

# generate total dividends/repurchase, share repurchase/dividend, and share of firms that issue dividends/repurchase, and InstOwn_Perc
data_inst1 <- data_inst %>%
  group_by(fyearq) %>%
  summarise(total_dividend = sum(dividend, na.rm = TRUE),
            total_repurchase = sum(repurchase, na.rm = TRUE),
            total_firms = n(), 
            total_dividend_firms = sum(dividend_dummy, na.rm = TRUE),
            total_repurchase_firms = sum(repurchase_dummy, na.rm = TRUE),
            avg_inst = mean(InstOwn_Perc, na.rm = TRUE))

# plot the times series trend of share repurchase and dividend sums in the same ggplot (overlay plot))
# keep only total dividend and total repurchase

ggplot(data_inst1, aes(x = fyearq)) +
  geom_line(aes(y = total_repurchase, color = "Total Share Repurchase")) +
  geom_line(aes(y = total_dividend, color = "Total Dividend")) +
  geom_line(aes(y = avg_inst * 100, color = "Institutional Ownership")) +
  scale_y_continuous(
    name = "Total Amount ($billion)",
    sec.axis = sec_axis(~./100, name = "Institutional Ownership")
  ) +
  labs(color = "Legend") +
  theme_minimal() +
  theme(legend.position = "top")
# Save to Results folder as pdf
ggsave("../Results/Total_Div_Repur_Inst.pdf")

# Do the same as above but keeping only the top 25 banks as before 
data_inst_top25 <- data_inst %>%
  filter(top25_prev_quarter == 1) %>% group_by(fyearq) %>%
  summarise(total_dividend = sum(dividend, na.rm = TRUE),
            total_repurchase = sum(repurchase, na.rm = TRUE),
            total_firms = n(), 
            total_dividend_firms = sum(dividend_dummy, na.rm = TRUE),
            total_repurchase_firms = sum(repurchase_dummy, na.rm = TRUE),
            avg_inst = mean(InstOwn_Perc, na.rm = TRUE))

# plot the times series trend of share repurchase and dividend sums in the same ggplot (overlay plot))
# keep only total dividend and total repurchase

ggplot(data_inst_top25, aes(x = fyearq)) +
  geom_line(aes(y = total_repurchase, color = "Total Share Repurchase")) +
  geom_line(aes(y = total_dividend, color = "Total Dividend")) +
  geom_line(aes(y = avg_inst * 100, color = "Institutional Ownership")) +
  scale_y_continuous(
    name = "Total Amount ($billion)",
    sec.axis = sec_axis(~./100, name = "Institutional Ownership")
  ) +
  labs(color = "Legend") +
  theme_minimal() +
  theme(legend.position = "top")
# Save to Results folder as pdf
ggsave("../Results/Total_Div_Repur_Inst_Top25.pdf")

