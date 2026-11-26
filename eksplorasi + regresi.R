library(dplyr)
library(car)
library(leaps)
library(ggplot2)
library(reshape2)
library(plotly)

# Data Preparation --------------------------------------------------------------

stunting = read.csv("Data CSV/Prevalensi Balita Bermasalah Gizi Berdasarkan Status Gizi menurut Provinsi.csv",sep=";")
stunting$PWasting = ifelse(is.na(stunting$PSevereWasting),stunting$Pwasting, stunting$Pwasting+ stunting$PSevereWasting)

kemiskinan = read.csv("Data CSV/Jumlah dan Persentase Penduduk Miskin Menurut Provinsi, 2024.csv")
kemiskinan[kemiskinan$Provinsi == "Kepulauan Bangka Belitung",]$Provinsi = "Bangka Belitung"
kemiskinan$PM = (kemiskinan$Persentase.Penduduk.Miskin...Maret+kemiskinan$Persentase.Penduduk.Miskin...September)/2

anc = read.csv("Data CSV/Proporsi Ibu Balita yang Melakukan ANC pada Masa Kehamilan Balita berdasarkan Tenaga Kesehatan Pemberi Layanan ANC.csv",sep=";")
names(anc)[names(anc)=="DokterKandungan"] = "ANC"

usiaASI = read.csv("Data CSV/Proporsi Usia Saat Mulai Rutin Setiap Hari Mendapatkan Minuman Makanan Selain ASI.csv",sep=";")
usiaASI = usiaASI %>%
  mutate(NoASI = rowSums(across(2:8)))

layanan = read.csv("Data CSV/Proporsi Balita yang Mendapatkan Layanan Kesehatan Dalam 12 Bulan Terakhir.csv",sep=";")
names(layanan)[names(layanan)=="Perkembangan"] = "PP"

# Data Merging ------------------------------------------------------------------

data = stunting[,c("Provinsi","Area","PWasting", "PStunting")]
data = merge(data,kemiskinan[,c(1,8)], by="Provinsi", all.x=T)
data = merge(data,anc[,1:2], by="Provinsi", all.x=T)
data = merge(data,usiaASI[,c(1,14)], by="Provinsi", all.x=T)
data = merge(data,layanan[,c(1,5)], by="Provinsi", all.x=T)

data$Bali = ifelse(data$Provinsi == "Bali",1,0)
data$PMN = ifelse(data$Area == "Maluku-Papua" |
                          data$Area == "Nusa Tenggara" ,1,0)

head(data)
# Data Visualisation ------------------------------------------------------------

cor(data$PWasting,data$NoASI)

cormat <- round(x = cor(data[,c(3:length(data))]), digits = 2)
melted_cormat <- melt(cormat)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson Correlation")

ggplot(aes(x=PWasting,y=PM, colour = PMN), data=data) + geom_point()

plot_ly(x=data$PWasting, y=data$PM, z=data$PP,
        type="scatter3d", size=0.5,mode="markers", color=data$PMN) %>% 
  layout(scene = list(xaxis = list(title = 'Wasting'), 
                      yaxis = list(title = 'Miskin'),
                      zaxis = list(title = 'PP')))

# Uji Proporsi 2 Populasi -------------------------------------------------------

prop.test(x = c(32101,27121), n = c(154330,145813))

prop.test(x = c(12810,9624), n = c(154330,145813))

# Regression Model STUNTING -----------------------------------------------------

model_stunting = lm(PStunting ~ PM + ANC, data=data, na.rm=T)
summary(model_stunting)

# Regression Model WASTING ------------------------------------------------------

model_wasting = lm(PWasting ~ NoASI + PP + PMN , 
                   data=data[data$Provinsi!="Bali",], na.rm=T)
summary(model_wasting)

# Regression Model WASTING + Bali -----------------------------------------------

model_wasting_bali = lm(PWasting ~ NoASI + PP + PMN + Bali , 
                        data=data, na.rm=T)
summary(model_wasting_bali)

# Cek Asumsi -----------------------------kemiskinan# Cek Asumsi --------------------------------------------------------------------

# Error Menyebar Normal
shapiro.test(residuals(model_stunting))
shapiro.test(residuals(model_wasting))
shapiro.test(residuals(model_wasting_bali))

# Homoskedastisitas
ncvTest(model_stunting)
ncvTest(model_wasting)
ncvTest(model_wasting_bali)

# Autocorrelation
durbinWatsonTest(model_stunting)
durbinWatsonTest(model_wasting)
durbinWatsonTest(model_wasting_bali)

# Multicolinearity
vif(model_stunting)
vif(model_wasting)
vif(model_wasting_bali)

