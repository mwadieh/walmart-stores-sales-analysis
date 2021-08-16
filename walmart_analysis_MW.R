# loading dependencies
library(plyr)
library(dplyr)
library(ggplot2)

# reading in data
walmart_sd <- read.csv("Walmart_Store_sales.csv")

# store with maximum sales
store_with_max_sales <- 
  aggregate(Weekly_Sales~Store,walmart_sd,sum) %>% 
  filter(Weekly_Sales == max(Weekly_Sales)) %>%
  select(Store,Weekly_Sales); store_with_max_sales

# determining store with maximum standard deviation
# store standard deviations table
store_standard_deviation_table <- 
  aggregate(Weekly_Sales~Store,walmart_sd,sd) %>% 
  select(Store,Weekly_Sales)

# store with maximum standard deviation
store_with_max_standard_deviation <- 
  store_standard_deviation_table %>% 
  filter(Weekly_Sales == max(Weekly_Sales)); store_with_max_standard_deviation

# store means table
store_mean_table <- 
  aggregate(Weekly_Sales~Store,walmart_sd,mean) %>% 
  select(Store,Weekly_Sales)

# store mean to standard deviation table
store_mean_to_standard_deviation_table <-
  cbind(select(store_mean_table,Store,Weekly_Sales), 
        store_standard_deviation_table$Weekly_Sales,
        store_mean_table$Weekly_Sales/store_standard_deviation_table$Weekly_Sales) %>%
  rename("Mean_to_sd_coef" = 
           "store_mean_table$Weekly_Sales/store_standard_deviation_table$Weekly_Sales") %>%
  rename("Standard_Deviation" = "store_standard_deviation_table$Weekly_Sales")

# coefficient of mean to standard deviation
store14_coefficient_of_mean_to_standard_deviation <- 
  filter(store_mean_to_standard_deviation_table,
         Standard_Deviation == max(Standard_Deviation)) %>%
  select(Store,Mean_to_sd_coef); store14_coefficient_of_mean_to_standard_deviation

# store with good quarterly growth rate in Q3 2012
# changing Date var from character to Date
walmart_sd$Date <- as.Date(walmart_sd$Date, format = "%d-%m-%Y") 

# 2012 Quarter 2 Store Sales 
store_q2_sales <- walmart_sd %>% 
  filter(Date >= as.Date("2012-04-01") & Date < as.Date("2012-07-01"))

# 2012 Quarter 3 Store Sales
store_q3_sales <- walmart_sd %>% 
  filter(Date >= as.Date("2012-07-01") & Date < as.Date("2012-10-01"))

# aggregated store weekly sales for Quarter 2
store_q2_sales_aggregated <- aggregate(Weekly_Sales~Store,store_q2_sales,sum)

# aggregated store weekly sales for Quarter 3
store_q3_sales_aggregated <- aggregate(Weekly_Sales~Store,store_q3_sales,sum)

# growth rate (q3 sales - q2 sales)
store_q3_growth_rate <- 
  data.frame(cbind(store_q3_sales_aggregated$Store,
        store_q3_sales_aggregated$Weekly_Sales-store_q2_sales_aggregated$Weekly_Sales)) %>%
  rename("Store"= "X1") %>% rename("Q3 Sales Growth" = "X2")

# store 3 good (positive) growth rate
store_q3_good_growth_rate <- store_q3_growth_rate %>% 
  filter(`Q3 Sales Growth`>1); store_q3_good_growth_rate

# non-holiday season sales 
non_holiday_sales <- walmart_sd %>% 
  filter(Holiday_Flag == 0)

# non-holiday mean sales
non_holiday_mean_sales <- mean(non_holiday_sales$Weekly_Sales)

# holiday sales greater than mean non-holiday sales
positive_holiday_sales <- walmart_sd %>%
  filter(Holiday_Flag == 1 & Weekly_Sales > non_holiday_mean_sales)

# holiday events
`Super Bowl` <- as.Date(c("12-02-10","11-02-11","10-02-12","08-02-13"), format = "%d-%m-%y")
`Labour Day` <- as.Date(c("10-09-10","09-09-11","07-09-12","06-09-13"), format = "%d-%m-%y")
Thanksgiving <- as.Date(c("26-11-10","25-11-11","23-11-12","29-11-13"), format = "%d-%m-%y")
Christmas <- as.Date(c("31-12-10","30-12-11","28-12-12","27-12-13"), format = "%d-%m-%y")

# creating holiday variable
positive_holiday_sales_labelled <- positive_holiday_sales %>%
  mutate(holidays = ifelse(Date %in% `Super Bowl`,"Super Bowl",
                           ifelse(Date %in% `Labour Day`,"Labour Day",
                                  ifelse(Date %in% Thanksgiving, "Thanksgiving",
                                  ifelse(Date %in% Christmas, "Christmas","Not Holiday")))))

# aggregating positive holiday sales
positive_holiday_sales_aggregated <- 
  aggregate(Weekly_Sales~holidays,positive_holiday_sales_labelled,sum) %>%
  rename("Total Sales"="Weekly_Sales"); positive_holiday_sales_aggregated

# column plot of positive impact holiday sales
positive_impact_holidays_plot <- ggplot(data = positive_holiday_sales_aggregated) +
  geom_col(mapping = aes(holidays,`Total Sales`/1000000, fill = holidays)) +
  ggtitle("Positive Impact Holidays Plot") +
  ylab("Total Sales in Millions Dollars ($)"); positive_impact_holidays_plot

# classify data into years
walmart_sd_with_years <- walmart_sd %>% 
  mutate(years = format(as.POSIXct(Date), format = "%Y"))

# classify data into months
walmart_sd_with_months <- walmart_sd_with_years %>%
  mutate(months = format(as.POSIXct(Date), format = "%m"))

# aggregating data by years and months
walmart_sd_months_aggregated <- aggregate(Weekly_Sales~years+months,walmart_sd_with_months,sum)

# creating years-months variable
walmart_sd_year_months <- walmart_sd_months_aggregated %>% 
  mutate(year_month = paste(as.character(years),as.character(months),sep = "-"))

# Monthly View of Sales Plot
monthly_view_of_sales <- ggplot(walmart_sd_year_months) + 
  geom_col(mapping = aes(year_month,Weekly_Sales/1000000,fill = Weekly_Sales/1000000)) +
  xlab("year-month") + ylab("monthly sales in million dollars ($)") +
  ggtitle("Monthly View of Sales") +
  theme(axis.text.x=element_text(angle = 80, vjust = 0.5)) +
  scale_fill_continuous("Monthly Sales in Millions"); monthly_view_of_sales

# classify data into semesters
walmart_sd_with_semesters <- walmart_sd_with_months %>%
  mutate(semesters = ifelse(quarters(Date) %in% c("Q1","Q2"),"semester 1","semester 2"))

# aggregating by years and semesters
walmart_sd_semesters_aggregated <- 
  aggregate(Weekly_Sales~years+semesters,walmart_sd_with_semesters,sum)

# creting year-semester variable
walmart_sd_year_semester <- walmart_sd_semesters_aggregated %>%
  mutate(year_semester = paste(as.character(years),as.character(semesters),sep = " "))

# Semester View of Sales Plot
semester_view_of_sales <- ggplot(walmart_sd_year_semester) + 
  geom_col(mapping = aes(year_semester,Weekly_Sales/1000000,fill = Weekly_Sales/1000000)) +
  xlab("year-semester") + ylab("semester sales in million dollars ($)") +
  theme(axis.text.x=element_text(angle = 30, vjust = 0.7)) +
  ggtitle("Semester View of Sales") +
  scale_fill_continuous("Semester Sales in Millions"); semester_view_of_sales

# walmart store 1 data
walmart_sd_store_1 <- walmart_sd %>% filter(Store == 1)

# changing holiday flag to factor
walmart_sd_store_1$Holiday_Flag <- as.factor(walmart_sd_store_1$Holiday_Flag)

# setting reeference date
reference_date <- as.Date("2010-02-04")

# changing date format to numeric
walmart_sd_store_1$Date <- as.numeric(difftime(walmart_sd_store_1$Date, reference_date))

# changing holiday flag variable to int
walmart_sd_store_1$Holiday_Flag <- as.integer(walmart_sd_store_1$Holiday_Flag)

# Evaluating prediction model to forecast demand for store 1
walmart_final_linear_model_store_1 <- 
  lm(Weekly_Sales~CPI+Temperature+Holiday_Flag,
     walmart_sd_store_1); summary(walmart_final_linear_model_store_1)

# plotting linear regression model for store 1
walmart_store1_linear_plot <-
  ggplot(walmart_sd_store_1, aes(CPI+Temperature+Holiday_Flag, Weekly_Sales)) +
  geom_point() +
  geom_smooth(method = lm, se = T) +
  xlab("x = CPI + Temperature + Holiday Flag") +
  ylab("Weekly Sales in Dollars ($)") +
  ggtitle("Prediction Model for Store 1"); walmart_store1_linear_plot

# Assuming 95% confidence interval

