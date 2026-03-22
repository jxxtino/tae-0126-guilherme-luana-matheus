## install.packages("dplyr")
## install.packages("lubridate")

library(dplyr)
library(lubridate)

## ======= DATA LOADING ========================================================

df_raw <- read.csv("data/01-raw/santos_environmental_indicators.csv", sep = ",")
df_clean <- df_raw 

## ======= DATA INSPECTION ===================================================== 

df_clean %>% 
  {
    ## variable types 
    print(sapply(., class))
    cat("\n")
    
    ## missing values per column
    print(colSums(is.na(.)))
    cat("\n")
    
    ## data sample
    print(head(., 3))
    cat("\n")
  }

## ======= DATA CLEANING =======================================================

df_clean <- df_clean %>%
  
  ## columns to lower case
  rename_with(tolower) %>%

  ## char trim 
  mutate(nome = trimws(nome)) %>%
  

  ## drop unnecessary columns
  select(-mean, -stddev) %>%
  
  ## feature engineering
  mutate(ndvi_class = case_when(
      ndvi_mean <= 0 ~ "non-vegetation",
      ndvi_mean > 0 & ndvi_mean <= 0.33 ~ "bare-soil",
      ndvi_mean > 0.33 & ndvi_mean <= 0.60 ~ "light-vegetation",
      ndvi_mean > 0.60 ~ "dense-vegetation"),
      lst_mean = (lst_mean-273.15)
  ) %>%
  
  ## type conversion
  mutate(
    district = as.factor(nome),
    date_obs = ym(date),
    ndvi_class = factor(ndvi_class,
                        levels=c("non-vegetation",
                                "bare-soil",
                                "light-vegetation",
                                "dense-vegetation"), 
                       ordered=TRUE),
  ) %>%

  ## feature selection
  select(date_obs, 
         district,
         ndvi_class,
         ndvi_mean,
         ndvi_stddev,
         ndbi_mean,
         ndbi_stddev,
         lst_mean,
         lst_stddev
  )

## ======= DATA AGGREGATION ====================================================

df_stats <- df_clean %>%
  group_by(district) %>%
    summarise(
      d_ndvi_mean = mean(ndvi_mean, na.rm = TRUE),
      d_ndbi_mean = mean(ndbi_mean, na.rm = TRUE),
      d_lst_mean = mean(lst_mean, na.rm = TRUE),
      
      d_ndvi_median = median(ndvi_mean, na.rm = TRUE),
      d_ndbi_median = median(ndbi_mean, na.rm = TRUE),
      d_lst_median = median(lst_mean, na.rm = TRUE),
      
      d_ndvi_std = sd(ndvi_mean, na.rm = TRUE),
      d_ndbi_std = sd(ndbi_mean, na.rm = TRUE),
      d_lst_std = sd(lst_mean, na.rm = TRUE)
    )

df_stats

## ======= DATA VISUALIZATION ==================================================

## box plot
boxplot(
  df_stats$d_ndvi_mean,
  main = "SANTOS 2021-2025 MEAN NDVI",
  ylab = "NDVI VALUE",
  ylim = c(0,1),
  col = "lightgreen"
)

## histogram
hist(
  df_stats$d_lst_mean, 
  main = "SANTOS 2021-2025 MEAN LST",
  ylab = "COUNT",
  xlab = "LST CELSIUS",
  xlim = c(19,28),
  col = "lightcoral"
  )

## bar plot
par(mar = c(10, 4, 4, 2))

barplot(
  df_stats$d_ndvi_mean[
    order(df_stats$d_ndvi_mean, decreasing=TRUE)[1:10]],
  names.arg = df_stats$district[
    order(df_stats$d_ndvi_mean, decreasing=TRUE)[1:10]],
  main = "SANTOS 2021-2025 TOP 10 NDVI DISTRICTS",
  ylim = c(0,1),
  las = 2,
  col = "lightgreen"
)

## ======= DATA SAVE ===========================================================

dir.create("data/02-processed")

write.table(df_clean, 
            "data/02-processed/df_clean.txt", 
            sep = ";",
            row.names = FALSE, 
            fileEncoding = "UTF-8")

write.table(df_stats, 
            "data/02-processed/df_stats.txt", 
            sep = ";",
            row.names = FALSE, 
            fileEncoding = "UTF-8")




