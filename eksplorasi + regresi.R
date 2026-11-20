library(dplyr)
library(car)
library(leaps)
library(ggplot2)
library(reshape2)
library(plotly)

# Data Preparation --------------------------------------------------------------

stunting = read.csv("Data CSV/Prevalensi Balita Bermasalah Gizi Berdasarkan Status Gizi menurut Provinsi.csv",sep=";")
stunting$PWasting = ifelse(is.na(stunting$PSevereWasting),stunting$Pwasting, stunting$Pwasting+ stunting$PSevereWasting)

# keragamanPangan = read.csv("Data CSV/Proporsi balita berusia 6-23 Bulan Berdasarkan Pemenuhan Keragaman Pangan Minimal.csv",sep=";")
# names(keragamanPangan)[names(keragamanPangan) == 'Memenuhi'] <- 'KPMemenuhi'

# protein = read.csv("Data CSV/Proporsi Balita Usia 6-23 Bulan yang Mengonsumsi Sumber Protein Hewani Selama 24 Jam Terakhir.csv",sep=";")

kemiskinan = read.csv("Data CSV/Jumlah dan Persentase Penduduk Miskin Menurut Provinsi, 2024.csv")
kemiskinan[kemiskinan$Provinsi == "Kepulauan Bangka Belitung",]$Provinsi = "Bangka Belitung"
kemiskinan$PMiskin = (kemiskinan$Persentase.Penduduk.Miskin...Maret+kemiskinan$Persentase.Penduduk.Miskin...September)/2

anc = read.csv("Data CSV/Proporsi Ibu Balita yang Melakukan ANC pada Masa Kehamilan Balita berdasarkan Tenaga Kesehatan Pemberi Layanan ANC.csv",sep=";")

usiaASI = read.csv("Data CSV/Proporsi Usia Saat Mulai Rutin Setiap Hari Mendapatkan Minuman Makanan Selain ASI.csv",sep=";")
usiaASI = usiaASI %>%
  mutate(ASINonEksklusif = rowSums(across(2:8)))

layanan = read.csv("Data CSV/Proporsi Balita yang Mendapatkan Layanan Kesehatan Dalam 12 Bulan Terakhir.csv",sep=";")


# Data Merging ------------------------------------------------------------------

data = stunting[,c("Provinsi","Area","PWasting", "PStunting")]
data = merge(data,protein[,1:2], by="Provinsi")
data = merge(data,keragamanPangan[,1:2], by="Provinsi")
data = merge(data,kemiskinan[,c(1,8)], by="Provinsi")
data = merge(data,anc[,1:2], by="Provinsi")
data = merge(data,usiaASI[,c(1,14)], by="Provinsi")
data = merge(data,layanan[,c(1,5)], by="Provinsi")

data$PapuaNusa = ifelse(data$Area == "Maluku-Papua" |
                          data$Area == "Nusa Tenggara" ,1,0)


# Data Visualisation ------------------------------------------------------------

cor(data$PWasting,data$ASINonEksklusif)

cormat <- round(x = cor(data[,c(3:length(data))]), digits = 2)
melted_cormat <- melt(cormat)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson Correlation")

ggplot(aes(x=PWasting,y=PMiskin, colour = PapuaNusa), data=data) + geom_point()

plot_ly(x=data$PWasting, y=data$PMiskin, z=data$Perkembangan,
        type="scatter3d", size=0.5,mode="markers", color=data$PapuaNusa) %>% 
  layout(scene = list(xaxis = list(title = 'Wasting'), 
                      yaxis = list(title = 'Miskin'),
                      zaxis = list(title = 'Perkembangan')))

# Regression Model STUNTING -----------------------------------------------------

data_stunting = stunting[,c("Provinsi","Area","PWasting", "PStunting")]
data_stunting = merge(data_stunting,kemiskinan[,c(1,8)], by="Provinsi")
data_stunting = merge(data_stunting,anc[,1:2], by="Provinsi")

model_stunting = lm(PStunting ~ PMiskin + DokterKandungan, data=data_stunting)
summary(model_stunting)

# Regression Model WASTING ------------------------------------------------------

data_wasting = stunting[,c("Provinsi","Area","PWasting", "PStunting")]
data_wasting = merge(data_wasting,usiaASI[,c(1,14)], by="Provinsi")
data_wasting = merge(data_wasting,layanan[,c(1,5)], by="Provinsi")

data_wasting$PapuaNusa = ifelse(data_wasting$Area == "Maluku-Papua" |
                          data_wasting$Area == "Nusa Tenggara" ,1,0)
data_wasting = data_wasting[-2,]

model_wasting = lm(PWasting ~ ASINonEksklusif + Perkembangan + PapuaNusa, 
                   data=data_wasting)
summary(model_wasting)

# Cek Asumsi --------------------------------------------------------------------

# Error Menyebar Normal
shapiro.test(residuals(model_stunting))
shapiro.test(residuals(model_wasting))

# Homoskedastisitas
ncvTest(model_stunting)
ncvTest(model_wasting)

# Autocorrelation
durbinWatsonTest(model_stunting)
durbinWatsonTest(model_wasting)

# Multicolinearity
vif(model_stunting)
vif(model_wasting)

