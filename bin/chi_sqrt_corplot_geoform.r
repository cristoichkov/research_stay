library(raster)
library(readr)
library(dplyr)
library(ggplot2)
library(gplots)
library(tidyr)


## Cragar la base de datos de la distribucion de M. haageana y M. albilanata, con grupos geneticos
BD_M_haag_alb <- read_csv("../out/files/BD_haageana_albilanata_geoformas.csv") 

colnames(BD_M_haag_alb)

anch_format <- BD_M_haag_alb %>%
  dplyr::rename(Geoform = "_Geoform MX") %>%
  dplyr::group_by(env_group_3, Geoform) %>%
  count() %>%
  spread(Geoform , n) %>%
  replace_na(list("env_group_3" = 0, "Llanuras lacustres y eolicas" = 0, "Montanas de plegamiento" = 0, "Planicies" = 0,
                  "Relieve volcanico" = 0, "Sierras" = 0, "Sistema carstico" = 0, "Sistema de piedemonte" = 0, "Sistema fluvial" = 0))

anch_format <- data.frame(anch_format)

rownames(anch_format) <- anch_format$env_group_3

anch_format <- anch_format[, -1]

anch_format <- as.table(as.matrix(anch_format))
# 2. Graph
balloonplot(t(anch_format), main ="housetasks", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)



chisq <- chisq.test(anch_format)
chisq
chisq$observed
round(chisq$expected,2)
round(chisq$residuals, 3)

library(corrplot)
corrplot(t(chisq$residuals), is.cor = FALSE)

contrib <- 100*chisq$residuals^2/chisq$statistic
round(contrib, 3)
# Visualize the contribution
corrplot(t(contrib), is.cor = FALSE)

