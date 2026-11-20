# Load library
library(dplyr)    # Untuk manipulasi data
library(car)      # Untuk uji homogenitas Levene
library(ggplot2)  # Untuk visualisasi data

# Input Data
data_gizi <- read.csv("Data CSV/Prevalensi Balita Bermasalah Gizi Berdasarkan Status Gizi menurut Provinsi.csv", sep = ";")
head(data_gizi)
data_gizi$wasting.fix <- data_gizi$PSevereWasting + data_gizi$Pwasting
head(data_gizi)

# Mengelompokkan provinsi ke dalam faktor 'Wilayah' ####
data_gizi <- data_gizi %>%
  mutate(
    Wilayah = case_when(
      Provinsi %in% c(
        "Aceh", "Sumatera Utara", "Sumatera Barat", "Riau", "Jambi",
        "Sumatera Selatan", "Bengkulu", "Lampung", "Bangka Belitung", 
        "Kepulauan Riau"
      ) ~ "Sumatra",
      
      Provinsi %in% c(
        "DKI Jakarta", "Jawa Barat", "Jawa Tengah", "DI Yogyakarta", 
        "Jawa Timur", "Banten", "Bali"
      ) ~ "Jawa-Bali",
      
      Provinsi %in% c(
        "Nusa Tenggara Barat", "Nusa Tenggara Timur"
      ) ~ "Nusa Tenggara",
      
      Provinsi %in% c(
        "Kalimantan Barat", "Kalimantan Tengah", "Kalimantan Selatan", 
        "Kalimantan Timur", "Kalimantan Utara"
      ) ~ "Kalimantan",
      
      Provinsi %in% c(
        "Sulawesi Utara", "Sulawesi Tengah", "Sulawesi Selatan", 
        "Sulawesi Tenggara", "Gorontalo", "Sulawesi Barat"
      ) ~ "Sulawesi",
      
      TRUE ~ "Maluku-Papua"
    ),
    Wilayah = as.factor(Wilayah)
  )
table(data_gizi$Wilayah)

# MODEL STUNTING ####
# model ANOVA
model_anova <- aov(PStunting ~ Wilayah, data = data_gizi)

# Uji Asumsi
# Uji Normalitas Sisaan (Shapiro-Wilk)
shapiro.test(residuals(model_anova))

# Uji Homogenitas Varians (Levene's Test)
leveneTest(PStunting ~ Wilayah, data = data_gizi)

# hasil ANOVA
summary(model_anova)

# Uji Lanjut Tukey HSD (Uji Tukey-Kramer)
tukey_result <- TukeyHSD(model_anova)
tukey_result

# Visualisasi Interval Kepercayaan Tukey HSD
plot(
  tukey_result, 
  las = 1, 
  col = "darkblue"
)


# MODEL WASTING ####
# model ANOVA
model_anova <- aov(wasting.fix ~ Wilayah, data = data_gizi)

# Uji Asumsi
# Uji Normalitas Sisaan (Shapiro-Wilk)
shapiro.test(residuals(model_anova))

# Uji Homogenitas Varians (Levene's Test)
leveneTest(wasting.fix ~ Wilayah, data = data_gizi)

# hasil ANOVA
summary(model_anova)

# Uji Lanjut Tukey HSD (Uji Tukey-Kramer)
tukey_result <- TukeyHSD(model_anova)
tukey_result

# Visualisasi Interval Kepercayaan Tukey HSD
plot(
  tukey_result, 
  las = 1, 
  col = "darkblue"
)


# MODEL REGRESI DUMMY ####
data_gizi$Wilayah <- as.factor(data_gizi$Wilayah)

# base line
data_gizi$Wilayah <- relevel(data_gizi$Wilayah, ref = "Jawa-Bali")

# model regresi dummy stunting
model_dummy.stunting <- lm(PStunting ~ Wilayah, data = data_gizi)
summary(model_dummy.stunting)

# model regresi dummy wasting
model_dummy.stunting <- lm(wasting.fix ~ Wilayah, data = data_gizi)
summary(model_dummy.stunting)
