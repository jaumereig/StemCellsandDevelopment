library("readxl")
library("dplyr")
####################
# selection miRNAs #
####################
smri_network <- read_xlsx("~/Escritorio/Jaume_ Compartit_Esrrb/Table S4. miR Array and ReadsI (copia).xlsx", 
                          sheet = "seleccion_miRs")
head(smri_network)
attach(smri_network)
# DAY 5
max_fold_inc_5 <- c()
#max_fold_dec_5 <- c()
direction_5 <- c()
for (value in 1:nrow(smri_network)) {
  max_fold_inc_5[value] <- log(max(`Median Ratio Day 1:Day 0`[value], `Median Ratio Day 5:Day 0`[value]), 2)
  #max_fold_dec_5[value] <- log(min(`Median Ratio Day 1:Day 0`[value], `Median Ratio Day 5:Day 0`[value]), 2)
  if (max_fold_inc_5[value] <= 0) {
    direction_5[value] <- "Activator"
  }
  else{
    direction_5[value] <- "Repressor"
  }
}
# DAY 3
max_fold_inc_3 <- c()
#max_fold_dec_3 <- c()
direction_3 <- c()
for (value in 1:nrow(smri_network)) {
  max_fold_inc_3[value] <- log(max(`Median Ratio Day 1:Day 0`[value], `Median Ratio Day 3:Day 0`[value]), 2)
  #max_fold_dec_3[value] <- log(min(`Median Ratio Day 1:Day 0`[value], `Median Ratio Day 3:Day 0`[value]), 2)
  if (max_fold_inc_3[value] < 0) {
    direction_3[value] <- "Activator"
  }
  else{
    direction_3[value] <- "Repressor"
  }
}
# DAY 1
max_fold_inc_1 <- c()
#max_fold_dec_1 <- c()
direction_1 <- c()
for (value in 1:nrow(smri_network)) {
  max_fold_inc_1[value] <- log(`Median Ratio Day 1:Day 0`[value], 2)
  #max_fold_dec_1[value] <- log(`Median Ratio Day 1:Day 0`[value], 2)
  if (max_fold_inc_1[value] < 0) {
    direction_1[value] <- "Activator"
  }
  else{
    direction_1[value] <- "Repressor"
  }
}
# DATA SUMMARY
selection_mirna <- data.frame(`Entrze Gene Symbol`, `Median Ratio Day 1:Day 0`, `Median Ratio Day 3:Day 0`, `Median Ratio Day 5:Day 0`, max_fold_inc_5, max_fold_inc_3, max_fold_inc_1, direction_5, direction_3, direction_1)
names(selection_mirna)[1] <- "Mir.Name"
head(selection_mirna)
write.csv(selection_mirna,file="~/Escritorio/Jaume_ Compartit_Esrrb/Jaume_Files/selection_mirna_table.xlsx",row.names = FALSE)
####################
# larger set below #
####################
mirna.reads <- read_xlsx("~/Escritorio/Jaume_ Compartit_Esrrb/Table S4. miR Array and ReadsI (copia).xlsx", 
                         sheet = "MicroRNA_Reads")
head(mirna.reads)
# drop unnecessary columns and lines
cols_wanted <- c("Mir Name","fc_d1_d0","fc_d3_d0", "fc_d5_d0")
mirna.reads <- mirna.reads[, names(mirna.reads) %in% cols_wanted]
mirna.reads <- mirna.reads[-c(462, 463), ]
attach(mirna.reads)
#DAY 5
max_fold_inc_5 <- c() # maximum fold increase
#max_fold_dec_5 <- c() # maximum fold decrease
direction_5 <- c()
for (value in 1:nrow(mirna.reads)) {
  max_fold_inc_5[value] <- log(max(fc_d1_d0[value], fc_d5_d0[value]), 2)
  #max_fold_dec_5[value] <- log(min(fc_d1_d0[value], fc_d5_d0[value]), 2)
  if (max_fold_inc[value] <= 0) {
    direction_5[value] <- "Activator"
  }
  else{
    direction_5[value] <- "Repressor"
  }
}
# DAY 3
max_fold_inc_3 <- c() # maximum fold increase
#max_fold_dec_3 <- c() # maximum fold decrease
direction_3 <- c()
for (value in 1:nrow(mirna.reads)) {
  max_fold_inc_3[value] <- log(max(fc_d1_d0[value], fc_d3_d0[value]), 2)
  #max_fold_dec_3[value] <- log(min(fc_d1_d0[value], fc_d3_d0[value]), 2)
  if (max_fold_inc[value] <= 0) {
    direction_3[value] <- "Activator"
  }
  else{
    direction_3[value] <- "Repressor"
  }
}
# DAY 1
max_fold_inc_1 <- c()
#max_fold_dec_1 <- c()
direction_1 <- c()
for (value in 1:nrow(mirna.reads)) {
  max_fold_inc_1[value] <- log(fc_d1_d0[value], 2)
  #max_fold_dec_1[value] <- log(fc_d1_d0[value], 2)
  if (max_fold_inc_1[value] < 0) {
    direction_1[value] <- "Activator"
  }
  else{
    direction_1[value] <- "Repressor"
  }
}
# DATA SUMMARY
mirna.reads.df <- data.frame(`Mir Name`, fc_d1_d0, fc_d3_d0, fc_d5_d0, max_fold_inc_5, max_fold_inc_3, max_fold_inc_1, direction_5, direction_3, direction_1)
head(mirna.reads.df)
write.csv(mirna.reads.df,file="~/Escritorio/Jaume_ Compartit_Esrrb/Jaume_Files/all_mri_table.xlsx",row.names = FALSE)
