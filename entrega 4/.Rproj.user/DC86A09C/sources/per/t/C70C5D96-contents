library(geobr)
library(spDataLarge)
library(tidyverse)
library(sf)
library(terra)
library(wesanderson)

rast = rast('brasil_coverage_2020.tif')

# arquivos de municipalidades
mun = read_municipality(year =2020)
rn = mun %>% filter(abbrev_state == "RN") %>% vect()

## crop and maks
cr = crop(rast, rn)
ms = mask(cr, rn)
plot(ms)

extrct =terra::extract(ms, rn)


pixels_mun = extrct %>%
  group_by(ID) %>% 
  summarise(n_pixels = n())

forest_mun = extrct %>%
  group_by(ID) %>% 
  filter(brasil_coverage_2020 %in% c(3,4,5,49)) %>% 
  summarise(n_forest = n())

pixel_total = left_join(pixels_mun, forest_mun)

pixel_total = pixel_total %>%
  mutate(ratio = n_forest/n_pixels)


rn_shapefile = st_as_sf(rn)
rn_shapefile$ID = 1:nrow(rn_shapefile)

rn_completo = merge(rn_shapefile, pixel_total)
colnames(rn_completo)[11] = 'Porcentagem Cobertura Vegetal'

pal <- wes_palette("Moonrise1", 100, type = "continuous")

ggplot(rn_completo)+
  geom_sf(aes(fill = `Porcentagem Cobertura Vegetal`), alpha =0.8 , col ="white")+ 
  scale_fill_gradientn(colours = pal) +
  labs(title = "Cobertura Vegetal de Rio Grande do Norte, por município")+
  theme_minimal()

linhas = 
  order(rn_completo$`Porcentagem Cobertura Vegetal`, decreasing=TRUE)[1:10]

top_10 = rn_completo[linhas, c(3,11)]
top_10 = cbind.data.frame(Municipio = top_10$name_muni,
                          `Porcentagem Cobertura Vegetal` = top_10$`Porcentagem Cobertura Vegetal`)
write.csv(top_10, 'top_10.csv')




