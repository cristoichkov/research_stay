library(ggplot2)
library(ggrepel)
library(formattable)
library(tibble)
library(tidyr)
library(dplyr)

source("export_formattable.R")

## Cragar la base de datos de la distribucion con los valores de bios para M. haageana y M. albilanata
bio_haag_alb <- read.csv("../out/files/M_haag_alb_bios_pca_only_genetic.csv")

bio_haag_alb <- bio_haag_alb %>%
  dplyr::select(Especie:bio19, env_group_3)
  
######### Calcular componentes principales ###########
## Seleccionar las variables bioclimaticas 
bios_val <- bio_haag_alb[, 6:24]

## Generamos la matriz de datos centrados y estandarizados de nuestras variables usando el comando scale: 
CentrEst <- scale(bios_val)

## Calculamos la matríz de correlación entre todas las variables mediante el método de spearman:
MatCorr <- cor(CentrEst, method="spearman") 


## calculamos los eigen vectorers y eigen values 
Eigen <- eigen(MatCorr) 
eigvec <- Eigen$vectors 
eigval <- Eigen$values 

## calculamos el porcentaje de participación de cada componente: 
Porcentaje <- (Eigen$values)/(sum(Eigen$values))  ## muestra el % individual de cada componente

pca.porcen <- data.frame(Porcentaje)

pca.porcen$index <- as.factor(1:nrow(pca.porcen))

pca.porcen$index.cont <- 1:nrow(pca.porcen)

PCA_percentage_var <- pca.porcen %>%
  ggplot(aes(x = index, y = Porcentaje*100, label = round(Porcentaje*100, 2))) +
  geom_bar(stat = "identity") +
  geom_path(aes(x = index.cont), size = 1, colour = "Gray50") +
  geom_point(size = 3) +
  geom_label_repel(size = 7, nudge_y = 0.5, direction = "y") +
  labs(x = "Principal components", y = "Percentage of explained variance") +
  theme(axis.text = element_text(size = 24),
        axis.title = element_text(size = 28))

ggsave(PCA_percentage_var, filename = "../out/PCA_bios/PCA_percentage_var.png",
       device = "png", dpi = 300, width = 24, height = 12)

PorcentajeAcum <- cumsum(Porcentaje)  ## muestra el % acumulado del número de componente

## calculamos los componentes principales: 
Componentes <- CentrEst %*% Eigen$vector


## calculamos la correlacion de variables
CorrelaVariables <- cor(cbind(CentrEst,Componentes))[1 : ncol(CentrEst), 
                                                     (ncol(CentrEst)+1):(ncol(CentrEst)+ncol(Componentes))] 

## calculamos la comunalidad (pareticipacion de cada cada vartiable dentro del compnete)
Comunali <- CorrelaVariables^2

cp_1 <- data.frame(Comunali) %>% 
  rownames_to_column() %>%
  as.tibble() %>%
  dplyr::select(rowname, V1) %>%
  rename(Variable_C1 = rowname, Component_1 = V1) %>%
  arrange(desc(Component_1)) %>%
  mutate(Component_1 = round(Component_1, 3))

cp_2 <- data.frame(Comunali) %>% 
  rownames_to_column() %>%
  as.tibble() %>%
  dplyr::select(rowname, V2) %>%
  rename(Variable_C2 = rowname, Component_2 = V2) %>%
  arrange(desc(Component_2)) %>%
  mutate(Component_2 = round(Component_2, 3))

cp_3 <- data.frame(Comunali) %>% 
  rownames_to_column() %>%
  as.tibble() %>%
  dplyr::select(rowname, V3) %>%
  rename(Variable_C3 = rowname, Component_3 = V3) %>%
  arrange(desc(Component_3)) %>%
  mutate(Component_3 = round(Component_3, 3))

df_stat_comun <- cbind(cp_1, cp_2, cp_3)

tab_stat_comun <- formattable(df_stat_comun, align = c("l", "c", "l", "c", "l", "c"))

## Guardar tabla en formato .png
export_formattable(tab_stat_comun, file = "../out/PCA_bios/Contribucion_var_comp_espinas.png")

## imprimimos la gráfica de componente uno contra dos:]
df_componentes <- data.frame(Componentes)

bio_haag_alb_PCA <- cbind(bio_haag_alb, df_componentes)

write.csv(bio_haag_alb_PCA, file = "../out/files/bio_haag_alb_genetic_PCA.csv", row.names = FALSE)

pallete_cris <- c(Acultingensis = "#7a60cf", Albilanata = "#c45db9", Haageana = "#62b650", Huauclilla = "#bab241", 
                  M_conspicua = "#c68343", M_meissneri = "#7f7ec4", Oaxacana = "#4facd8", Reppenhagenii = "#cc5143",
                  San_angelensis = "#4baf8c", Sto_domingo = "#c75980", Tehuantepec = "#6c7f38")

PCA_haag_alb_1 <- bio_haag_alb_PCA %>%
  ggplot(aes(x = X1, y = X2, color = env_group_3, shape = env_group_3)) +
  geom_point(size = 8) +
  labs(x = "Principal component 1 (52.94%)", y = "Principal component 2 (23.84%)") +
  scale_shape_manual(name = "", values = 1:19,
                     labels = c(Acultingensis = expression(italic("M. acultzingensis")),
                                Albilanata = expression(italic("M. albilanata")),
                                M_conspicua = expression(italic("M. conspicua")),
                                M_meissneri = expression(italic("M. meissneri")),
                                Haageana = expression(italic("M. haageana")),
                                Huauclilla = "Huauclilla",
                                Oaxacana= expression(italic("M. oaxacana")),
                                Reppenhagenii = expression(italic("M. reppenhagenii")),
                                San_angelensis = expression(italic("M. san-angelensis")),
                                Sto_domingo = "Sto. Dom. Ton.",
                                Tehuantepec = "Tehuantepec")) +
  scale_color_manual(name = "", values = pallete_cris, 
                     labels = c(Acultingensis = expression(italic("M. acultzingensis")),
                                Albilanata = expression(italic("M. albilanata")),
                                M_conspicua = expression(italic("M. conspicua")),
                                M_meissneri = expression(italic("M. meissneri")),
                                Haageana = expression(italic("M. haageana")),
                                Huauclilla = "Huauclilla",
                                Oaxacana= expression(italic("M. oaxacana")),
                                Reppenhagenii = expression(italic("M. reppenhagenii")),
                                San_angelensis = expression(italic("M. san-angelensis")),
                                Sto_domingo = "Sto. Dom. Ton.",
                                Tehuantepec = "Tehuantepec")) +
  theme(legend.title = element_text(size=24),
        legend.text = element_text(size=28), 
        legend.text.align = 0, 
        axis.text = element_text(size = 24),
        axis.title = element_text(size = 28))

ggsave(PCA_haag_alb_1, file="../out/PCA_bios/PCA_bios_genetico_C1_C2.png", device="png", dpi = 300, width = 24, height = 14)

PCA_haag_alb_2 <- bio_haag_alb_PCA %>%
  ggplot(aes(x = X1, y = X3, color = env_group_3, shape = env_group_3)) +
  geom_point(size = 8) +
  labs(x = "Principal component 1 (52.94%)", y = "Principal component 3 (10.36%)") +
  scale_shape_manual(name = "", values = 1:19,
                     labels = c(Acultingensis = expression(italic("M. acultzingensis")),
                                Albilanata = expression(italic("M. albilanata")),
                                M_conspicua = expression(italic("M. conspicua")),
                                M_meissneri = expression(italic("M. meissneri")),
                                Haageana = expression(italic("M. haageana")),
                                Huauclilla = "Huauclilla",
                                Oaxacana= expression(italic("M. oaxacana")),
                                Reppenhagenii = expression(italic("M. reppenhagenii")),
                                San_angelensis = expression(italic("M. san-angelensis")),
                                Sto_domingo = "Sto. Dom. Ton.",
                                Tehuantepec = "Tehuantepec")) +
  scale_color_manual(name = "", values = pallete_cris, 
                     labels = c(Acultingensis = expression(italic("M. acultzingensis")),
                                Albilanata = expression(italic("M. albilanata")),
                                M_conspicua = expression(italic("M. conspicua")),
                                M_meissneri = expression(italic("M. meissneri")),
                                Haageana = expression(italic("M. haageana")),
                                Huauclilla = "Huauclilla",
                                Oaxacana= expression(italic("M. oaxacana")),
                                Reppenhagenii = expression(italic("M. reppenhagenii")),
                                San_angelensis = expression(italic("M. san-angelensis")),
                                Sto_domingo = "Sto. Dom. Ton.",
                                Tehuantepec = "Tehuantepec")) +
  theme(legend.title = element_text(size=24),
        legend.text = element_text(size=28), 
        legend.text.align = 0, 
        axis.text = element_text(size = 24),
        axis.title = element_text(size = 28))

ggsave(PCA_haag_alb_2, file="../out/PCA_bios/PCA_bios_genetico_C1_C3.png", device="png", dpi = 300, width = 24, height = 14)
