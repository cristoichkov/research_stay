library(dplyr)
library(tibble)
library(formattable)

source("export_formattable.R")

dir_wallace <- list.files("../out/maxent/wallace_out/")

result_wallace <- data.frame()

for (i in dir_wallace) {
  
  csv_file <- list.files(paste0("../out/maxent/wallace_out/", i,"/"), pattern = "Tbl.csv")
  
  csv_wallace <- read.csv(paste0("../out/maxent/wallace_out/", i, "/", csv_file))
  
  df <- csv_wallace %>%
    arrange(delta.AICc) %>% 
    slice_max(1) %>%
    mutate(gen_group = rep(i, nrow(.))) %>%
    dplyr::select(gen_group, settings:parameters) 
  
  result_wallace <- rbind(result_wallace, df)
}

rownames(result_wallace) <- result_wallace$gen_group

result_wallace <- result_wallace[,-1]

make_italic <- formatter("span", style =  "font-style:italic")

rownames(result_wallace)[2] <- make_italic("M. albilanata")
rownames(result_wallace)[3] <- make_italic("M. conspicua")
rownames(result_wallace)[4] <- make_italic("M. haageana")
rownames(result_wallace)[5] <- make_italic("M. meissneri")
rownames(result_wallace)[6] <- make_italic("M. oaxacana")
rownames(result_wallace)[7] <- make_italic("M. san-angelensis")

tab_stat_comun <- formattable(result_wallace, align = c("l", rep("c", 16)))

export_formattable(tab_stat_comun, file = "../out/maxent/out_files/wallace_parameters.png", width = 1900)



n_occ_wallace <- data.frame()

for (i in dir_wallace) {
  
  csv_file <- list.files(paste0("../out/maxent/wallace_out/", i,"/"), pattern = "processed_occs.csv")
  
  occ_number <- read.csv(paste0("../out/maxent/wallace_out/", i, "/", csv_file))
  
  occ_number_df <- occ_number %>%
    group_by(name) %>%
    count()
  
  n_occ_wallace <- rbind(n_occ_wallace, data.frame(occ_number_df))
}

rownames(n_occ_wallace) <- n_occ_wallace$name

result_wallace <- n_occ_wallace %>%
  dplyr::select(n)

make_italic <- formatter("span", style =  "font-style:italic")

rownames(result_wallace)[2] <- make_italic("M. albilanata")
rownames(result_wallace)[3] <- make_italic("M. conspicua")
rownames(result_wallace)[4] <- make_italic("M. haageana")
rownames(result_wallace)[5] <- make_italic("M. meissneri")
rownames(result_wallace)[6] <- make_italic("M. oaxacana")
rownames(result_wallace)[7] <- make_italic("M. san-angelensis")

tab_stat_comun <- formattable(result_wallace, align = "c")

export_formattable(tab_stat_comun, file = "../out/maxent/out_files/n_occ_wallace.png", width = 300)
