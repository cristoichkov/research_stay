set.seed(1)
dat <- matrix(stats::rnorm(2000), ncol = 2)
ch <- chull(dat)
coords <- dat[c(ch, ch[1]), ]  # closed polygon

plot(dat, pch=19)
lines(coords, col="red")

poly_fil <- M_haag_alb_bios_pca_genetic %>%
  dplyr::filter(Long > -102)
  

dat <- matrix(cbind(poly_fil$Long, poly_fil$Lat), ncol = 2)
ch <- chull(dat)
coords <- dat[c(ch, ch[1]), ]

plot(dat, pch=19)
lines(coords, col="red")

poly_coords <- data.frame(coords)

poly_coords <- poly_coords %>%
  rename(longitude = X1, latitude = X2)

write.csv(poly_coords, file = "../out/files/polygon.csv", row.names = FALSE)
