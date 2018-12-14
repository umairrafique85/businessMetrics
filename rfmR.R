
## TODO: give choice to calculate monetary value by summing or by averaging  
library(Hmisc)

# function to get metrics, monetary by sum
getRFM_metrics <- function(customerId, invoiceDate, dateBaseline = Sys.Date(), 
                           invoiceNumber, invoiceAmount, dataframe){
  customerId <- enquo(customerId)
  invoiceDate <- enquo(invoiceDate)
  dateBaseline <- enquo(dateBaseline)
  invoiceNumber <- enquo(invoiceNumber)
  invoiceAmount <- enquo(invoiceAmount)
  as.data.frame(dataframe) %>% group_by(customerId = !!customerId) %>% 
    summarise(recency = as.integer(!!dateBaseline - max(!!invoiceDate)),
              frequency = n_distinct(!!invoiceNumber), monetary=sum(!!invoiceAmount))
}

# function to make bins of metrics. Input will be dataframe returned from getRFM_metrics()
getRFM_quintiles <- function(dataframe_metrics){
  df_binnedMetrics <- data.frame(customerId = dataframe_metrics$customerId,
             R = cut2(dataframe_metrics$recency, g = 5), 
             F = cut2(dataframe_metrics$frequency, g = 5), 
             M = cut2(dataframe_metrics$monetary, g = 5))
  # reorder factor for R to be in descending order
  df_binnedMetrics$R <- ordered(df_binnedMetrics$R, levels = sort(levels(df_binnedMetrics$R), 
                                                                  decreasing = TRUE))
  return(df_binnedMetrics)
}

# function to present scores as 5, 5, 5. 1 being lowest and 5 being highest.
# input will be dataframe returned from getRFM_quintiles()
getRFM_scores <- function(dataframe_binnedMetrics){
  data.frame(customerId = dataframe_binnedMetrics$customerId,
             R_score = as.numeric(dataframe_binnedMetrics$R),
             F_score = as.numeric(dataframe_binnedMetrics$F),
             M_score = as.numeric(dataframe_binnedMetrics$M))
}


# create 11 segments as in https://www.putler.com/rfm-analysis/
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

# Also possible to segment according to this: https://www.blastam.com/blog/rfm-analysis-boosts-sales
# 
# Some valuable insights on what to do further available here as well: https://www.r-bloggers.com/rfm-customer-analysis-with-r-language/
# 