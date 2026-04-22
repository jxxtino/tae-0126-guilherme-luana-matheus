## install.packages("stringi")
## install.packages("dplyr")
## install.packages("ggcorrplot")
## install.packages("lubridate")
## install.packages("fitdistrplus")


library(MASS)
library(dplyr)
library(stringi)
library(lubridate)
library(ggcorrplot)
library(fitdistrplus)

## ======= DATA LOADING ========================================================

df <- read.csv(
  "data/01-raw/santos_environmental_indicators.csv", 
  sep = ",", 
  fileEncoding = "UTF-8"
)

## ======= DATA INSPECTION ===================================================== 

df %>% 
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

df_clean <- df %>%
  
  # columns to lower case
  rename_with(tolower) %>%
  
  # text normalization
  mutate(nome = stri_trans_general(nome, "Latin-ASCII")) %>%
  
  # char trim
  mutate(nome = trimws(nome)) %>%
  
  # type conversion
  mutate(date = ym(date), year = year(date), district = as.factor(nome)) %>%
  
  # lst join & conversion
  mutate(
    lst_mean = coalesce(lst_mean, mean),
    lst_stddev = coalesce(lst_stddev, stddev),
    lst_mean = abs(lst_mean - 273.15)
  ) %>%
  
  # feature engineering
  mutate(ndvi_class = factor(
    case_when(
      ndvi_mean <= 0 ~ "non-vegetation",
      ndvi_mean > 0 & ndvi_mean <= 0.33 ~ "bare-soil",
      ndvi_mean > 0.33 & ndvi_mean <= 0.60 ~ "light-vegetation",
      ndvi_mean > 0.60 ~ "dense-vegetation"), 
    ordered = TRUE,
    levels=c("non-vegetation","bare-soil","light-vegetation","dense-vegetation")
  ),
  ) %>%
  
  # feature selection
  dplyr::select(
    year,
    date,
    district,
    ndvi_class,
    ndvi_mean,
    ndbi_mean,
    lst_mean,
    ndvi_stddev,
    ndbi_stddev,
    lst_stddev
  )

## ======= DATA SUMMARY ========================================================

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

## ======= DATA SAVE ===========================================================

dir.create("data/02-processed", recursive = TRUE)
dir.create("output/01-charts", recursive = TRUE)

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

## ======= DATA PLOTS ==========================================================

# box plot
png("output/01-charts/santos-2021-2025-mean-ndvi.png", width = 800, height = 600)

boxplot(
  df_stats$d_ndvi_mean,
  main = "SANTOS 2021-2025 MEAN NDVI",
  ylab = "NDVI VALUE",
  ylim = c(0,1),
  col = "lightgreen"
)

dev.off()

# histogram
png("output/01-charts/santos-2021-2025-mean-lst.png", width = 800, height = 600)

hist(
  df_stats$d_lst_mean, 
  main = "SANTOS DISTRICTS 2021-2025 MEAN LST COUNT",
  ylab = "COUNT",
  xlab = "LST CELSIUS",
  col = "lightcoral"
)

dev.off()

# bar plot
par(mar = c(10, 4, 4, 2))

png("output/01-charts/santos-2021-2025-top-10-ndvi-districts.png", 
    width = 800, height = 600)

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

dev.off()

## ======= DIST PLOT ==========================================================

par(mar = c(5, 4, 4, 2))

ndvi_fit_norm <- fitdist(
  df_clean$ndvi_mean[!is.na(df_clean$ndbi_mean) & df_clean$ndvi_mean > 0],
  "lnorm")

plot(ndvi_fit_norm)

## ======= CORR PLOTS ==========================================================

# ndvi x lst corr test
cor.test(df_stats$d_ndvi_mean, 
         df_stats$d_lst_mean)

# ndvi x lst plot
plot(
  df_stats$d_ndvi_mean, 
  df_stats$d_lst_mean,
  main = "NDVI x LST",
  xlab = "NDVI MEAN",
  ylab = "LST MEAN")

# ndvi x ndbi corr test
cor.test(df_stats$d_ndvi_mean, 
         df_stats$d_ndbi_mean)

# ndvi x ndbi plot
plot(
  df_stats$d_ndvi_mean, 
  df_stats$d_ndbi_mean,
  main = "NDVI x NDBI",
  xlab = "NDVI MEAN",
  ylab = "NDBI MEAN")
legend("topright", legend = (cor(df_stats$d_ndvi_mean, df_stats$d_ndbi_mean)))

# lst x ndbi corr test
cor.test(df_stats$d_lst_mean, 
         df_stats$d_ndbi_mean)

# lst x ndbi plot
plot(
  df_stats$d_lst_mean, 
  df_stats$d_ndbi_mean,
  main = "LST x NDBI",
  xlab = "LST MEAN",
  ylab = "NDBI MEAN"
)
legend("topleft", legend = (cor(df_stats$d_lst_mean, df_stats$d_ndbi_mean)))

# corr matrix
corr_matrix <- round(cor(df_stats[, c(2,3,4)]), 2)

# corr matrix plot
ggcorrplot(
  corr_matrix,
  hc.order = TRUE,
  type = "lower",
  outline.color = "white",
  lab = TRUE
)

modelo_alfa <- lm(d_lst_mean ~ d_ndvi_mean, data = df_stats)
modelo_beta <- lm(d_lst_mean ~ log(d_ndvi_mean), data = df_stats)
summary(modelo_alfa)
summary(modelo_beta)

plot(df_stats$d_ndvi_mean, df_stats$d_lst_mean,
     main = "Comparação LM Alfa vs Beta",
     xlab = "NDVI",
     ylab = "LST")

abline(modelo_alfa, col = "blue")

curve(coef(modelo_beta)[1] + coef(modelo_beta)[2]*log(x),
      add = TRUE, col = "red")

AIC(modelo_alfa, modelo_beta)
