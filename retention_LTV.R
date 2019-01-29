library(tidyverse)
library(lubridate)
library(Hmisc)
library(plotly)
library(spatstat.utils)
df_sales_invoices <- read_csv('woocommerce orders - Last 12 Months - Without Products - Orders.csv')
df_sales_products <- read_csv("woocommerce orders - Last 12 Months - With Products - Orders.csv")

# products file has data from NOV '17. removing the extra month
df_sales_products <- subset(df_sales_products, `Order Date`>=as.Date("2017-12-01"))

df_fromGA <- read_csv('Analytics Master view source by order 20171201-20181130.csv', skip = 6)
df_fromGA$Date <- ymd(df_fromGA$Date)
names(df_sales_invoices) <- make.names(names(df_sales_invoices))
names(df_sales_products) <- make.names(names(df_sales_products))

save(df_sales_invoices, file = "df_sales_invoices")
save(df_sales_products, file = "df_sales_products")
save(df_fromGA, file = "df_fromGA")
# most of the orders missing from GA data, so not using that

# make df for customer metrics, without products ####
df_sales_invoices %>% 
  group_by(Email..Billing.) %>% 
  summarise(LTV = sum(Order.Subtotal.Amount), firstOrderID = min(Order.Number),
            month_acquired = format(min(Order.Date), format = "%b-%y"),
            avgOrderValue = mean(Order.Subtotal.Amount), 
            span_days = days(round(max(Order.Date) - min(Order.Date)))$day,
            span_months = ceiling(days(round(max(Order.Date) - min(Order.Date)))$day / 30),
            n = n()) %>% 
  mutate(daysBwRpt = span_days %/% n,
         customerType = ifelse(n > 1, ifelse(n > 2, "above2orders", "2orders"),
                               "notRepeatCustomer"),
         repeatRate = round(n/span_months, 1)) -> df_customers

# make df for customer metrics, with products of first purchase ####


# visualization: distribution of customers by type ####
df_customers %>% group_by(customerType) %>% count() %>% 
  ggplot(aes(x = customerType, y = nn)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = nn), vjust = -0.5) +
  theme(axis.text.x = element_text(size = 15), axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 15), axis.title.y = element_text(size = 20)) +
  labs(x = "customer type", y = "count")

# visualization: distribution of customers by number of orders ####
df_customers %>% group_by(n) %>% count() %>% 
  ggplot(aes(x = n, y = nn)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = nn), vjust = -0.5) +
  theme(axis.text.x = element_text(size = 15), axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 15), axis.title.y = element_text(size = 20)) +
  labs(x = "number of orders", y = "count")

df_customers$repeatRate[which(is.infinite(df_customers$repeatRate))] <- NA

# make monthAcquired metrics ####
df_customers %>% 
  group_by(month_acquired, customerType) %>% 
  summarise(avgLTV = round(mean(LTV)), avgRetention = round(mean(span_months)), 
            avgDaysBwRpt = round(mean(daysBwRpt)), n = n()) -> df_monthAcquired_metrics

# visualization: distribution of customers by month acquired, customerType stacked ####
ggplot(df_monthAcquired_metrics, aes(x = month_acquired, y = n)) +
  geom_bar(stat = 'identity', aes(fill = customerType)) +
  theme(axis.text.x = element_text(size = 15, angle = 90), axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 15), axis.title.y = element_text(size = 20)) +
  labs(y = 'count')

# visualization: same as above, percentage of customers acquired that month ####
ggplot(df_monthAcquired_metrics, aes(x = month_acquired, y = n)) +
  geom_bar(stat = 'identity', position = 'fill', aes(fill = customerType)) +
  scale_y_continuous(labels = c(0, 25, 50, 75, 100), breaks = c(0, 0.25, 0.50, 0.75, 1)) +
  theme(axis.text.x = element_text(size = 15, angle = 90), axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 15), axis.title.y = element_text(size = 20)) +
  labs(y = "percent")

# make df_customerType, only two columns from df_customers
df_customerType <- df_customers %>% 
  select(Email..Billing., customerType, month_acquired)

# add customerType, month_acquired, and month of invoice to df_sales_invoices
df_sales_invoices <- merge(df_sales_invoices, df_customerType)
df_sales_invoices$month <- format(df_sales_invoices$Order.Date, "%b-%y")
df_sales_invoices$month <- factor(df_sales_invoices$month,
                                  levels = c("Dec-17", "Jan-18", "Feb-18", "Mar-18", 
                                             "Apr-18", "May-18", "Jun-18", "Jul-18", 
                                             "Aug-18", "Sep-18", "Oct-18", "Nov-18"))

# df for summed LTV of each cohort by months in ####
df_sales_invoices %>%
  group_by(month_acquired, month) %>%
  summarise(orderValue = sum(Order.Subtotal.Amount)) -> df_LTV
lst_LTV <- lapply(unique(df_sales_invoices$month_acquired), function(month1st){
  df_1month <- subset(df_LTV, month_acquired == month1st)
  df_1month$cumulativeLTV <- cumsum(df_1month$orderValue)
  df_1month
})
df_LTV <- do.call(rbind, lst_LTV)
df_LTV <- ungroup(df_LTV)

# For proper LTV calculations ####
# add year
df_sales_invoices$year <- year(df_sales_invoices$order_date)
# number of years
# average purchase value, 
df_sales_invoices %>%
  group_by(year) %>% 
  summarise(total_gross = sum(order_subtotal_amount),
            n_purchases = n(),
            n_unique_customers = n_distinct(email_billing)) %>% 
  mutate(avg_purchase_val = total_gross/n_purchases,
         avg_purchase_freq = n_purchases/n_unique_customers) %>% 
  mutate(avg_customer_val = avg_purchase_val * avg_purchase_freq)

# reference:
# 1. Calculate average purchase value: Calculate this number by dividing your 
# company's total revenue in a time period (usually one year) by the number 
# of purchases over the course of that same time period.
# 2. Calculate average purchase frequency rate: Calculate this number by dividing 
# the number of purchases over the course of the time period by the number of unique 
# customers who made purchases during that time period.
# 3. Calculate customer value: Calculate this number by multiplying the average purchase 
# value by the average purchase frequency rate.
# 4. Calculate average customer lifespan: Calculate this number by averaging 
# out the number of years a customer continues purchasing from your company.
# 5. Then, calculate LTV by multiplying customer value by the average customer 
# lifespan. This will give you an estimate of how much revenue you can reasonably 
# expect an average customer to generate for your company over the course of their relationship with you.

# visualization: MoM Growth of LTV ####
ggplot(df_LTV, aes(x = month, y = cumulativeLTV, color = month_acquired, group = month_acquired)) +
  geom_line() +
  geom_point() +
  theme(axis.text.x = element_text(size = 15, angle = 90), axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 15), axis.title.y = element_text(size = 20)) +
  geom_text(aes(label = round(cumulativeLTV)), vjust = -1)

# write.csv(df_retention, "df_retention.csv")
# 
# df_retention <- read_csv("df_retention.csv")

# df for rolling retention ####
df_customers %>% 
  group_by(month_acquired, span_months) %>% 
  count() -> df_retention
df_retention %>% 
  spread(span_months, nn) -> df_retention_spread
df_retention_back <- df_retention_spread %>% 
  gather(key = month, value = n, 2:14)
df_retention_back$month <- as.integer(df_retention_back$month)
df_retention_back <- dplyr::ungroup(df_retention_back)
lst_rollRetention <- lapply(unique(df_retention_back$month_acquired), function(mY){
  df_1month <- subset(df_retention_back, month_acquired == mY)
  vec_retention <- revcumsum(na.omit(df_1month$n))
  vec_retention <- c(vec_retention, rep(NA, 13 - length(vec_retention)))
  df_1month$retention <- vec_retention
  totalN <- df_1month$retention[1]
  df_1month$retentionRate <- sapply(df_1month$retention, function(retentionFigure){
    round((retentionFigure/totalN)*100)
  })
  df_1month$month_acquired <- ordered(df_1month$month_acquired, levels = rev(levels(df_1month$month_acquired)))
  df_1month
} )
df_rollRetention_extnd <- do.call(rbind, lst_rollRetention)

# visualization: heatmap for rolling retention
ggplot(df_rollRetention_extnd, aes(month, month_acquired)) +
  geom_tile(aes(fill = retentionRate), color = "white") + 
  scale_fill_gradient(low = "red", high = "green") + 
  geom_text(aes(label = ifelse(!is.na(retentionRate), 
                               paste(retentionRate, "%\n(", retention, ")", sep = ""),
                               NA))) +
  theme(axis.text.x = element_text(size = 15), axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 15), axis.title.y = element_text(size = 20)) +
  labs(x = "months out")

# df for classic retention ####
lst_clscRetention <- lapply(unique(df_sales_invoices$Email..Billing.), function(user){
  df_1user <- arrange(subset(df_sales_invoices, Email..Billing. == user), Order.Date)
  df_1user$retention <- sapply(df_1user$Order.Date, function(date){
    ceiling(days(round(max(df_1user$Order.Date) - date))$day / 30)
  })
  df_1user %>% select(Email..Billing., Order.Date, retention) -> df_1user
  df_1user$month_acquired <- df_customers$month_acquired[match(user, df_customers$Email..Billing.)]
  df_1user
})
df_clscRetention_customers <- do.call(rbind, lst_clscRetention)
df_clscRetention_customers %>% 
  group_by(month_acquired, retention) %>% 
  count() -> df_clscRetention
df_clscRetention %>% 
  spread(retention, n) -> df_clscRetention_spread
df_clscRetention_spread %>% 
  gather(key = month, value = n, 2:14) -> df_clscRetention
df_clscRetention$month <- as.integer(df_clscRetention$month)
df_clscRetention <- dplyr::ungroup(df_clscRetention)
lst_clscRetention <- lapply(unique(df_clscRetention$month_acquired), function(mY){
  df_1month <- subset(df_clscRetention, month_acquired == mY)
  df_1month$retentionRate <- sapply(df_1month$n, function(retentionFigure){
    round((retentionFigure/max(df_1month$n, na.rm = TRUE))*100)
  })
  df_1month$month_acquired <- ordered(df_1month$month_acquired, levels = rev(levels(df_1month$month_acquired)))
  df_1month
})
df_clscRetention_extnd <- do.call(rbind, lst_clscRetention)

# visualization: heatmap for classic retention ####
ggplot(df_clscRetention_extnd, aes(month, month_acquired)) +
  geom_tile(aes(fill = retentionRate), color = "white") + 
  scale_fill_gradient(low = "red", high = "green") + 
  geom_text(aes(label = ifelse(!is.na(retentionRate), 
                               paste(retentionRate, "%\n(", n, ")", sep = ""),
                               NA))) +
  theme(axis.text.x = element_text(size = 15), axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 15), axis.title.y = element_text(size = 20)) + 
  labs(x = "months out")

# mat_retention <- as.matrix(df_retention_spread[-1])
# rownames(mat_retention) <- df_retention_spread$month_acquired
# heatmap(mat_retention, Rowv = NA, Colv = NA, scale = 'none')
