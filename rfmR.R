
## TODO: give choice to calculate monetary value by summing or by averaging  


library(Hmisc)

# deal with negative values in quantity and unitPrice
df_retailData$Quantity[which(is.na(df_retailData$Quantity))] <- NA
df_retailData$UnitPrice[which(is.na(df_retailData$UnitPrice))] <- NA

# drop rows with NA's
df_retailData %>% drop_na() -> df_retailData_noNA

## start of action. Function should start here
# get metrics, monetary by summing
df_retailData_noNA %>% 
  group_by(CustomerID) %>%
  summarise(recency = as.integer(Sys.Date() - max(InvoiceDate)), 
            frequency = n_distinct(InvoiceNo), monetary = sum(total)) -> df_RFM_metrics
df_RFM_metrics$logMonetary <- log(df_RFM_metrics$monetary)

# make segments by quintiles
df_RFM_segments <- data.frame(customerId = df_RFM_metrics$CustomerID, 
                             R = cut2(df_RFM_metrics$recency, g = 5), 
                             F = cut2(df_RFM_metrics$frequency, g = 5), 
                             M = cut2(df_RFM_metrics$monetary, g = 5))

# reorder factor for R to be in descending order
df_RFM_segments$R <- ordered(df_RFM_segments$R, levels = sort(levels(df_RFM_segments$R), 
                                                            decreasing = TRUE))
# present scores 5, 5, 5
df_RFM_scores <- data.frame(customerId = df_RFM_segments$customerId,
                            R_score = as.numeric(df_RFM_grading$R), 
                            F_score = as.numeric(df_RFM_grading$F), 
                            M_score = as.numeric(df_RFM_grading$M))

# TODO: create 11 segments as in https://www.putler.com/rfm-analysis/
df_RFM_scores$segment <- sapply(seq_len(nrow(df_RFM_scores)), function(i){
  ifelse((df_RFM_scores$R_score[i] >= 4 & (df_RFM_scores$F_score[i] + df_RFM_scores$M_score[i])/2 >= 4),
         "Champion", 
         ifelse((df_RFM_scores$R_score[i] >= 2 & (df_RFM_scores$F_score[i] + df_RFM_scores$M_score[i])/2 >= 3),
                "Loyal Customer", 
                ifelse((df_RFM_scores$R_score[i] >= 3 & (df_RFM_scores$F_score[i] + df_RFM_scores$M_score[i])/2 <= 3),
                       "Potential Loyalist", 
                       ifelse((df_RFM_scores$R_score[i] >= 4 & (df_RFM_scores$F_score[i] + df_RFM_scores$M_score[i])/2 <= 1),
                              "Recent Customer",
                              ifelse(((df_RFM_scores$R_score[i] >= 3 & df_RFM_scores$R_score[i] <= 4) & 
                                        (df_RFM_scores$F_score[i] + df_RFM_scores$M_score[i])/2 <= 1),
                                     "Promising", 
                                     ifelse(((df_RFM_scores$R_score[i] >= 2 & df_RFM_scores$R_score[i] <= 3) & 
                                               ((df_RFM_scores$F_score[i] + df_RFM_scores$M_score[i])/2 >= 2 & 
                                                  (df_RFM_scores$F_score[i] + df_RFM_scores$M_score[i])/2 <= 3)),
                                            "Needs Attention",
                                            ifelse(((df_RFM_scores$R_score[i] >= 2 & df_RFM_scores$R_score[i] <= 3) & 
                                                      (df_RFM_scores$F_score[i] + df_RFM_scores$M_score[i])/2 <= 2),
                                                   "About to Sleep",
                                                   ifelse((df_RFM_scores$R_score[i] <= 2 & 
                                                             (df_RFM_scores$F_score[i] + df_RFM_scores$M_score[i])/2 >= 2), 
                                                          "At Risk",
                                                          ifelse((df_RFM_scores$R_score[i] <= 1 & 
                                                                    (df_RFM_scores$F_score[i] + df_RFM_scores$M_score[i])/2 >= 4),
                                                                 "Can't Lose", 
                                                                 ifelse((df_RFM_scores$R_score[i] <= 2 & 
                                                                           (df_RFM_scores$F_score[i] + df_RFM_scores$M_score[i])/2 <= 2),
                                                                        "Hibernating", "Lost"))))))))))
})
