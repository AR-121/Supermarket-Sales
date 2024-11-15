# Install required packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Read the data---
sales_data <- read.csv("https://raw.githubusercontent.com/AR-121/Supermarket-Sales/refs/heads/main/Data/supermarket_sales%20-%20Sheet1.csv")

# Basic data exploration
str(sales_data)
summary(sales_data)

# 1. Total Revenue and Profit Trends
# Convert Date column to proper date format
sales_data$Date <- as.Date(sales_data$Date, format = "%m/%d/%Y")

# Calculate daily revenue and profit
daily_metrics <- sales_data %>%
  group_by(Date) %>%
  summarise(
    Revenue = sum(Total),
    Profit = sum(Total - (Unit.price * Quantity))
  ) %>%
  arrange(Date)

# Reshape the data for plotting
daily_metrics_long <- daily_metrics %>%
  pivot_longer(
    cols = c(Revenue, Profit),
    names_to = "Metric",
    values_to = "Value"
  )

# Create the line chart
ggplot(daily_metrics_long, aes(x = Date, y = Value, color = Metric, group = Metric)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2, alpha = 0.6) +
  theme_minimal() +
  scale_color_manual(values = c("Revenue" = "#2E86C1", "Profit" = "#28B463")) +
  labs(
    title = "Daily Revenue and Profit Over Time",
    x = "Date",
    y = "Amount ($)",
    color = "Metric"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  ) +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_x_date(date_breaks = "1 week", date_labels = "%Y-%m-%d")

# 2. Sales by Product Line 
product_sales <- sales_data %>%
  group_by(Product.line) %>%
  summarise(total_sales = sum(Total)) %>%
  mutate(percentage = total_sales / sum(total_sales) * 100)

ggplot(product_sales, aes(x = "", y = total_sales, fill = Product.line)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_minimal() +
  labs(title = "Sales Distribution by Product Line",
       fill = "Product Line") +
  theme(axis.text = element_blank(),
        axis.title = element_blank()) +
  scale_fill_manual(values = c("#FF9999", "#66B2FF", "#99FF99", 
                               "#FFCC99", "#FF99CC", "#99CCFF")) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5))

# 3. Average Rating by Product Line
ggplot(sales_data, aes(x = Product.line, y = Rating, fill = Product.line)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Customer Ratings Distribution by Product Line",
       x = "Product Line",
       y = "Rating",
       fill = "Product Line") +
  scale_fill_brewer(palette = "Set3")

# 4. Sales by Payment Method 
payment_sales <- sales_data %>%
  group_by(Payment) %>%
  summarise(total_sales = sum(Total)) %>%
  mutate(percentage = total_sales / sum(total_sales) * 100)

ggplot(payment_sales, aes(x = "", y = total_sales, fill = Payment)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_minimal() +
  labs(title = "Sales Distribution by Payment Method",
       fill = "Payment Method") +
  theme(axis.text = element_blank(),
        axis.title = element_blank()) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5))

# 5. Member Card Usage 
member_dist <- sales_data %>%
  group_by(Customer.type) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

ggplot(member_dist, aes(x = "", y = count, fill = Customer.type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_minimal() +
  labs(title = "Distribution of Customer Types",
       fill = "Customer Type") +
  theme(axis.text = element_blank(),
        axis.title = element_blank()) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5))

# 6. Sales by Time of Day
sales_data$hour <- as.numeric(substr(sales_data$Time, 1, 2))
sales_data$time_of_day <- cut(sales_data$hour, 
                              breaks = c(0, 12, 17, 24),
                              labels = c("Morning", "Afternoon", "Evening"))

ggplot(sales_data, aes(x = time_of_day, y = Total, fill = time_of_day)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Sales Distribution by Time of Day",
       x = "Time of Day",
       y = "Total Sales",
       fill = "Time of Day") +
  scale_fill_brewer(palette = "Set2")

# 7. Gender Distribution across Product Lines
ggplot(sales_data, aes(x = Product.line, fill = Gender)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Gender Distribution across Product Lines",
       x = "Product Line",
       y = "Proportion",
       fill = "Gender") +
  scale_fill_brewer(palette = "Set1")

# 8. Correlation between Unit Price vs Total Sales
ggplot(sales_data, aes(x = Unit.price, y = Total)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = "Correlation between Unit Price and Total Sales",
       x = "Unit Price",
       y = "Total Sales")
