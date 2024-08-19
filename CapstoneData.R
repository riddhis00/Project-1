library(readxl)
T10 <- read_excel("~/Excel Files/T10YIE.xlsx")
#View(T10)
library(readxl)
Tarisio <- read_excel("~/Excel Files/tarisio_data_20230825.xlsx")
#View(Tarisio)
nrow(Tarisio)

##sort by bows
bow<-"violin bow"
filtered_data <- subset(Tarisio, Type == bow)
sorted_data <- filtered_data[order(filtered_data$Type), ]
nrow(sorted_data) ##Number of entries with bows only

##Now change sales to numeric form
clean<- function(x){
  numeric<-as.numeric(gsub("[^0-9.]","",x))
  round(numeric,2)
}
currencies<- c("USD","EUR","GBP")
cleaned<- apply(sorted_data[currencies],2,clean)
sorted_data[currencies] <- cleaned
#View(cleaned)

##Verify length is still the same
nrow(cleaned)

#Find Unique Non-Numerical Values Such as Maker, Location, Etc.
ucity<-unique(sorted_data$City)
umaker<-unique(sorted_data$maker)
uauctionhouse<-unique(sorted_data$`Auction House`)
print(ucity)
print(umaker)
print(uauctionhouse)

##Finds all non numeric values that occur once. Change '$maker' for what column is needed.
col_counts <- table(sorted_data$maker)
unique_values <- names(col_counts[col_counts == 1])
indices_list <- list()
for (value in unique_values) {
  indices <- which(sorted_data$maker == value)
  indices_list[[value]] <- indices
}
print(indices_list)
length(indices_list)
length(umaker) ##compare unique # to total #

##To find where a specific value is in the table
input_value<-"Bisch, Paul"  #This can be the maker, city, auction house, etc.
input_colname<-"maker"  #Enter the full, correct name of the column
indicies<-which(sorted_data$maker==input_value) ##change '$maker' to whichever column is needed
print(indicies) ##Shows value in data table

#Find Nulls and Zeros
null_counts <- list()
zero_counts <- list()

# Loop over each column
for (col in names(sorted_data)) {
  # Count nulls in the column
  null_counts[[col]] <- sum(is.na(sorted_data[[col]]))
}

zero_counts<-colSums(cleaned == 0)

# Display the counts
print(null_counts)
print(zero_counts)

#Find Date Range
range(sorted_data$`Sale Date`)
range(cleaned$USD)
DateRange<-2023-1969

## Plot Scatter of Sale Prices over Time
x<- sorted_data$`Sale Date`
y<- cleaned[1:16134]
plot(x,y,main = "Sale Price Over Time",xlab="Date",ylab = "$ of Sales",ylim =c(0,200000),pch=20)

# Histogram of Sales over time
hist(x,breaks = DateRange)

##Find Mean, Median, Quantiles of Sale Prices and Dates
summary(cleaned[1:16134])
summary(sorted_data$`Sale Date`)

##Remove the zeroes
data_no_zeroes <- cleaned[apply(cleaned != 0, 1, all), ]

# Print the resulting data frame
print(data_no_zeroes)
length(data_no_zeroes)
summary(data_no_zeroes[1:16133])

###Average time between sales per maker
library(dplyr)
bow_sales <- sorted_data %>%
  arrange(maker, `Sale Date`)

bow_sales <- sorted_data %>%
  group_by(maker) %>%
  arrange('Sale Date') %>%
  mutate(date_diff = `Sale Date` - lag(`Sale Date`)) %>%
  filter(!is.na(date_diff)) %>%
  ungroup()

average_distance <- bow_sales %>%
  group_by(maker) %>%
  summarize(avg_distance = mean(date_diff, na.rm = TRUE))
print(average_distance)
ranked_average_distance <- average_distance %>%
  arrange(desc(avg_distance))
# Print the result
print(ranked_average_distance)
#in ascending order
ranked_average_distance <- average_distance %>%
  arrange((avg_distance))
# Print the result
print(ranked_average_distance)
##take out zeroes
average_distance_filtered <- average_distance %>%
  filter(avg_distance != 0)
# Rank the average distance from least time to most
ranked_average_distance <- average_distance_filtered %>%
  arrange(avg_distance)
# Print the result with no zeroes now#
print(ranked_average_distance)


#Filter after 1982
bow_sales <- bow_sales %>%
  filter(as.Date(`Sale Date`, format = "%m/%d/%Y") > as.Date("1982-01-01"))
#Find log returns per maker, find highest and lowest average returns, summary (mean, median, etc)
###Find log returns and volatility
bow_sales<-bow_sales %>% 
  mutate(log_return = log(bow_sales$USD/lag(bow_sales$USD)))
average_log_returns <- bow_sales %>%
  group_by(maker) %>%
  summarize(avg_log_return = mean(log_return, na.rm = TRUE))
print(average_log_returns)
volatility <- bow_sales %>%
  group_by(maker) %>%
  summarize(volatility = sd(log_return, na.rm = TRUE))
#Top 10 highest and lowest average return
top_10_highest_avg_return <- average_log_returns %>%
  top_n(10, avg_log_return) %>%
  arrange(desc(avg_log_return))
top_10_lowest_avg_return <- average_log_returns %>%
  top_n(-10, avg_log_return) %>%
  arrange(avg_log_return)
# Rank the makers by top 10 highest and lowest volatility
top_10_highest_volatility <- volatility %>%
  top_n(10, volatility) %>%
  arrange(desc(volatility))
top_10_lowest_volatility <- volatility %>%
  top_n(-10, volatility) %>%
  arrange(volatility)
# Print the results
print("Top 10 makers with highest average return:")
print(top_10_highest_avg_return)
print("Top 10 makers with lowest average return:")
print(top_10_lowest_avg_return)
print("Top 10 makers with highest volatility:")
print(top_10_highest_volatility)
print("Top 10 makers with lowest volatility:")
print(top_10_lowest_volatility)

##plot results
library(ggplot2)
# Create the scatter plot
ggplot(data = volatility, aes(x = maker, y = volatility)) +
  geom_point() +
  labs(title = "Volatilities per Maker",
       x = "Maker",
       y = "Volatility") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

##Scatterplot and histogram of all log returns updated
hist(bow_sales$log_return, breaks=30)
ggplot(average_log_returns, aes(x = maker, y = avg_log_return)) +
  geom_point() +
  labs(title = "Average Log Returns per Maker",
       x = "Maker",
       y = "Average Log Return") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot(bow_sales$log_return)

##Make sure data is 1982 and after, Create Benchmark Here:
sorted_data_filtered <- sorted_data %>%
  filter(as.Date(`Sale Date`) >= as.Date("1982-01-01"))
##Group by month and find average price of all bow sales
monthly_average_price <- sorted_data_filtered %>%
  mutate(month_year = format(`Sale Date`, "%Y-%m")) %>%
  group_by(month_year) %>%
  summarise(avg_price = mean(USD))
# Calculate the log returns for each month
monthly_log_returns <- diff(log(monthly_average_price$avg_price))
# Print the resulting monthly log returns
plot(monthly_average_price,type='l')
print(monthly_log_returns)
length(monthly_log_returns)
plot(monthly_log_returns, type='l')
hist(monthly_log_returns, breaks = 30)
avereturn<-mean(monthly_log_returns)
sdbenchmark<-sd(monthly_log_returns)
##Turn into columns
monthly_returns_data <- data.frame(
  month_year = monthly_average_price$month_year[-1],  # Exclude the first month_year as it corresponds to the first return
  monthly_return = monthly_log_returns
)
# Print the resulting dataframe
print(monthly_returns_data)

##Turn Inflation Data into Monthly Rate
library(dplyr)
# Replace periods with NA in the T10YIE column
T10$T10YIE[T10$T10YIE == "."] <- NA
# Convert T10YIE column to numeric
T10$T10YIE <- as.numeric(T10$T10YIE)
T10 <- T10[order(T10$DATE), ]
monthly_average <- T10 %>%
  mutate(Month = format(DATE, "%Y-%m")) %>%
  group_by(Month) %>%
  summarise(Average_T10YIE = mean(T10YIE, na.rm = TRUE))
print(monthly_average)
nrow(monthly_average)

# Load required libraries to get S&P Data
library(quantmod)
library(dplyr)
# Define start and end dates for the data
start_date <- "2003-01-01"
end_date <- "2023-12-31"
# Download S&P 500 index data from Yahoo Finance
getSymbols("^GSPC", src = "yahoo", from = start_date, to = end_date, adjust = TRUE)
# Extract adjusted closing prices
sp_prices <- GSPC$GSPC.Adjusted
# Calculate daily returns
sp_returns <- diff(log(sp_prices))
# Convert daily returns to a data frame
sp_returns_df <- data.frame(Date = index(sp_returns), Return = coredata(sp_returns))
# Add a column for YearMonth
sp_returns_df$YearMonth <- format(sp_returns_df$Date, "%Y-%m")
# Group by YearMonth and calculate the average daily return for each month
monthly_avg_returns <- sp_returns_df %>%
  group_by(YearMonth) %>%
  summarise(Avg_Return = mean(GSPC.Adjusted, na.rm = TRUE))
# Print or visualize the monthly average returns
print(monthly_avg_returns)

correlation <- cor(monthly_average$Average_T10YIE, monthly_avg_returns$Avg_Return, use = "complete.obs")
correlation

##Rolling correlations between SandP returns and violin bow returns index
library(zoo)
library(ggplot2)
# Convert the data to zoo objects
average_t10yie_zoo <- zoo::zoo(monthly_average$Average_T10YIE, as.yearmon(monthly_average$Month))
avg_return_zoo <- zoo::zoo(monthly_avg_returns$Avg_Return, as.yearmon(monthly_avg_returns$YearMonth))
# Calculate rolling correlations for window sizes of 3, 6, and 12 months
roll_corr_3 <- rollapplyr(cbind(average_t10yie_zoo, avg_return_zoo), width = 3, FUN = function(x) cor(x[,1], x[,2], use = "complete.obs"), by.column = FALSE, align = "right", fill = NA)
roll_corr_6 <- rollapplyr(cbind(average_t10yie_zoo, avg_return_zoo), width = 6, FUN = function(x) cor(x[,1], x[,2], use = "complete.obs"), by.column = FALSE, align = "right", fill = NA)
roll_corr_12 <- rollapplyr(cbind(average_t10yie_zoo, avg_return_zoo), width = 12, FUN = function(x) cor(x[,1], x[,2], use = "complete.obs"), by.column = FALSE, align = "right", fill = NA)
# Convert to data frames
roll_corr_3_df <- as.data.frame(roll_corr_3)
roll_corr_6_df <- as.data.frame(roll_corr_6)
roll_corr_12_df <- as.data.frame(roll_corr_12)
# Rename columns
colnames(roll_corr_3_df) <- c("Rolling_Correlation_3")
colnames(roll_corr_6_df) <- c("Rolling_Correlation_6")
colnames(roll_corr_12_df) <- c("Rolling_Correlation_12")
# Add date column
roll_corr_3_df$Date <- as.Date(time(roll_corr_3))
roll_corr_6_df$Date <- as.Date(time(roll_corr_6))
roll_corr_12_df$Date <- as.Date(time(roll_corr_12))
# Plot rolling correlations
ggplot() +
  geom_line(data = roll_corr_3_df, aes(x = Date, y = Rolling_Correlation_3), color = "blue", size = 1, na.rm = TRUE) +
  geom_line(data = roll_corr_6_df, aes(x = Date, y = Rolling_Correlation_6), color = "red", size = 1, na.rm = TRUE) +
  geom_line(data = roll_corr_12_df, aes(x = Date, y = Rolling_Correlation_12), color = "green", size = 1, na.rm = TRUE) +
  labs(title = "Rolling Correlations between Average_T10YIE and Avg_Return",
       x = "Date",
       y = "Correlation") +
  theme_minimal()
# Plot rolling correlations for each window size separately
# Window size 3
ggplot(roll_corr_3_df, aes(x = Date, y = Rolling_Correlation_3)) +
  geom_line(color = "blue", size = 1, na.rm = TRUE) +
  labs(title = "Rolling Correlation (Window Size: 3 months)",
       x = "Date",
       y = "Correlation") +
  theme_minimal()
# Window size 6
ggplot(roll_corr_6_df, aes(x = Date, y = Rolling_Correlation_6)) +
  geom_line(color = "red", size = 1, na.rm = TRUE) +
  labs(title = "Rolling Correlation (Window Size: 6 months)",
       x = "Date",
       y = "Correlation") +
  theme_minimal()
# Window size 12
ggplot(roll_corr_12_df, aes(x = Date, y = Rolling_Correlation_12)) +
  geom_line(color = "green", size = 1, na.rm = TRUE) +
  labs(title = "Rolling Correlation (Window Size: 12 months)",
       x = "Date",
       y = "Correlation") +
  theme_minimal()

# Calculate rolling correlations for window sizes of 1 Year, 2 Years, 5 Years
roll_corr_1yr <- rollapplyr(cbind(average_t10yie_zoo, avg_return_zoo), width = 12, FUN = function(x) cor(x[,1], x[,2], use = "complete.obs"), by.column = FALSE, align = "right", fill = NA)
roll_corr_2yr <- rollapplyr(cbind(average_t10yie_zoo, avg_return_zoo), width = 24, FUN = function(x) cor(x[,1], x[,2], use = "complete.obs"), by.column = FALSE, align = "right", fill = NA)
roll_corr_5yr <- rollapplyr(cbind(average_t10yie_zoo, avg_return_zoo), width = 60, FUN = function(x) cor(x[,1], x[,2], use = "complete.obs"), by.column = FALSE, align = "right", fill = NA)
# Convert to data frames
roll_corr_1yr_df <- as.data.frame(roll_corr_1yr)
roll_corr_2yr_df <- as.data.frame(roll_corr_2yr)
roll_corr_5yr_df <- as.data.frame(roll_corr_5yr)
# Rename columns
colnames(roll_corr_1yr_df) <- c("Rolling_Correlation_1_Year")
colnames(roll_corr_2yr_df) <- c("Rolling_Correlation_2_Years")
colnames(roll_corr_5yr_df) <- c("Rolling_Correlation_5_Years")
# Add date column
roll_corr_1yr_df$Date <- as.Date(time(roll_corr_1yr))
roll_corr_2yr_df$Date <- as.Date(time(roll_corr_2yr))
roll_corr_5yr_df$Date <- as.Date(time(roll_corr_5yr))
# Plot rolling correlations
ggplot() +
  geom_line(data = roll_corr_1yr_df, aes(x = Date, y = Rolling_Correlation_1_Year), color = "blue", size = 1, na.rm = TRUE) +
  geom_line(data = roll_corr_2yr_df, aes(x = Date, y = Rolling_Correlation_2_Years), color = "red", size = 1, na.rm = TRUE) +
  geom_line(data = roll_corr_5yr_df, aes(x = Date, y = Rolling_Correlation_5_Years), color = "green", size = 1, na.rm = TRUE) +
  labs(title = "Rolling Correlations between Average_T10YIE and Avg_Return",
       x = "Date",
       y = "Correlation") +
  theme_minimal()
# Plot rolling correlations for each window size separately
# Window size 1 year
ggplot(roll_corr_1yr_df, aes(x = Date, y = Rolling_Correlation_1_Year)) +
  geom_line(color = "blue", size = 1, na.rm = TRUE) +
  labs(title = "Rolling Correlation (Window Size: 1 Year)",
       x = "Date",
       y = "Correlation") +
  theme_minimal()
# Window size 2 years
ggplot(roll_corr_2yr_df, aes(x = Date, y = Rolling_Correlation_2_Years)) +
  geom_line(color = "red", size = 1, na.rm = TRUE) +
  labs(title = "Rolling Correlation (Window Size: 2 Years)",
       x = "Date",
       y = "Correlation") +
  theme_minimal()
# Window size 5 years
ggplot(roll_corr_5yr_df, aes(x = Date, y = Rolling_Correlation_5_Years)) +
  geom_line(color = "green", size = 1, na.rm = TRUE) +
  labs(title = "Rolling Correlation (Window Size: 5 Years)",
       x = "Date",
       y = "Correlation") +
  theme_minimal()

# Make sure data is 1982 and after, plot average benchmark price
sorted_data_filtered <- sorted_data %>%
  filter(as.Date(`Sale Date`) >= as.Date("1982-01-01"))
# Group by month and find average price of all bow sales
monthly_average_price <- sorted_data_filtered %>%
  mutate(month_year = as.Date(paste0(`Sale Date`, "-01"), format = "%Y-%m-%d")) %>%
  group_by(month_year) %>%
  summarise(avg_price = mean(USD))
# Check for missing or non-numeric values in avg_price column
if (any(is.na(monthly_average_price$avg_price)) || any(!is.numeric(monthly_average_price$avg_price))) {
  # If there are missing or non-numeric values, remove them
  monthly_average_price <- monthly_average_price[complete.cases(monthly_average_price$avg_price), ]
}
# Plot with dates on the x-axis
ggplot(monthly_average_price, aes(x = month_year, y = avg_price)) +
  geom_line() +
  labs(x = "Date", y = "Average Price", title = "Monthly Average Price")
