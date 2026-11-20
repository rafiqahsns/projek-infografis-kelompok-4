library(sf)
library(ggplot2)

my_sf <- read_sf("Provinsi Indo/LapakGIS_Batas_Provinsi_2024.shp")
stunting = read.csv("Data CSV/Prevalensi Balita Bermasalah Gizi Berdasarkan Status Gizi menurut Provinsi.csv",sep=";")

my_sf[my_sf$WADMPR == "Riau",]$WADMPR = "Kepulauan Riau"
my_sf[my_sf$WADMPR == "Daerah Istimewa Yogyakarta",]$WADMPR = "DI Yogyakarta"
my_sf[my_sf$WADMPR == "Kepulauan Bangka Belitung",]$WADMPR = "Bangka Belitung"
stunting$PWasting = ifelse(is.na(stunting$PSevereWasting),stunting$Pwasting, stunting$Pwasting+ stunting$PSevereWasting)

my_sf = merge(x=my_sf,y=stunting[,c("Provinsi","PStunting","PWasting")], by.x="WADMPR", by.y="Provinsi")


ggplot(my_sf) +
  geom_sf(aes(fill = PStunting),lwd = 0) + 
  scale_fill_gradient(low = "#9DE0D7", high = "#C25107") +
  theme_void()

ggsave(filename="stunting.png", scale=3)

ggplot(my_sf) +
  geom_sf(aes(fill = PWasting),lwd = 0) + 
  scale_fill_gradient(low = "#9DE0D7", high = "#C25107") +
  theme_void()

ggsave(filename="wasting.png", scale=3)


ggplot(data, aes(x=PStunting/100, y=PWasting/100)) +
  labs(y="Prevalensi Balita Wasting", x="Prevalensi Balita Stunting") +
  geom_point(col="darkblue") + theme_light() +
  geom_smooth(method=lm , color="#87AAFF", size = 0.5, alpha=0.1)+
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent)
ggsave(filename="stunting vs wasting.png", scale=3)
