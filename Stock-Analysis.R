library(tidyverse)
library(tidyquant)

# Get stock data
stock <- tq_get("AAPL", from = "2020-01-01")

# Remove duplicate dates (IMPORTANT FIX)
stock <- stock %>%
  distinct(date, .keep_all = TRUE)

# Convert to monthly prices
stock_m <- stock %>%
  tq_transmute(select = adjusted,
               mutate_fun = to.monthly,
               indexAt = "lastof",
               col_rename = "price")

# Calculate log returns
stock_m <- stock_m %>%
  mutate(log_return = log(price / lag(price)))

# Remove NA values
stock_ret <- stock_m %>%
  drop_na()

# Plot
ggplot(stock_ret, aes(x = date, y = log_return)) +
  geom_line(color = "blue", linewidth = 1) +
  theme_minimal() +
  labs(title = "Apple Monthly Log Returns",
       x = "Date",
       y = "Log Return")
