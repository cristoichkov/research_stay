library(corrplot)
library(dplyr)
library(tibble)


M_alb_haag <- read.csv("../out/files/bio_haag_alb_genetic_PCA.csv")

cor_bios <- cor(M_alb_haag[,6:24])

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat) 
  p.mat
}

p.mat <- cor.mtest(cor_bios)

png(filename = "../out/PCA_bios/Cor_mat_plot_bios.png", width = 1000, height = 1000, pointsize = 14)
colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor_bios, method="color", col=col(200),  
         tl.srt = 45,
         diag=FALSE, # tl.pos="d", 
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         # hide correlation coefficient on the principal diagonal
         mar=c(0,0,1,0))
dev.off()

variables_mne <- data.frame(cor_bios) %>%
  rownames_to_column() %>%
  rename(Variable = rowname) %>%
  filter(bio08 < 0.85 & bio08 > -0.85) %>%
  filter(bio13 < 0.85 & bio13 > -0.85) %>%
  filter(bio07 < 0.85 & bio07 > -0.85)

new_no_cor <- M_alb_haag %>%
  dplyr::select(bio08, bio13, bio07, bio03, bio04, bio14, bio18, bio19)

cor_new_var <- cor(new_no_cor)

png(filename = "../out/PCA_bios/No_Cor_mat_plot_bios.png", width = 600, height = 600, pointsize = 14)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor_new_var, method="color", col=col(200),  
         tl.srt = 45,
         diag=FALSE, # tl.pos="d", 
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         # hide correlation coefficient on the principal diagonal
         mar=c(0,0,1,0))
dev.off()