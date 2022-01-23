library("readxl")
##########################
# LIST OF GENES IN FIGURE: 
# CBX7
# CDKN1A
# PDGFB
# KDM6B
# BTG2
# SPRY2
# ITGB1
# FOXA2
# ATP1A1
# LAMC1
# CTDSP1
# PSIP1
# JAG1
# ADCY9
# DLX2
# EFNA3
# PTPN12
# VEGFA
# CXCR4
# 4632428N05RIK
##########################
genes.df <- read_xlsx("~/Escritorio/Jaume_ Compartit_Esrrb/genes_list.xlsx", col_names = TRUE)
head(genes.df)
# drop unnecessary column
cols_wanted <- c("Entrze Gene Symbol","Median Ratio Day 1:Day 0","Median Ratio Day 3:Day 0", "Median Ratio Day 5:Day 0")
genes.df <- genes.df[, names(genes.df) %in% cols_wanted]
names(genes.df)[1] <- "Gene.Name"
attach(genes.df)
# DAY 5
max_fold_inc_5 <- c()
#max_fold_dec_5 <- c()
direction_5 <- c()
for (value in 1:nrow(genes.df)) {
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
for (value in 1:nrow(genes.df)) {
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
for (value in 1:nrow(genes.df)) {
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
genes.df.final <- data.frame(Gene.Name, `Median Ratio Day 1:Day 0`, `Median Ratio Day 3:Day 0`, `Median Ratio Day 5:Day 0`, max_fold_inc_5, max_fold_inc_3, max_fold_inc_1, direction_5, direction_3, direction_1)
# SELECTION OF GENES
selected.genes <- c("'CBX7'", "'CDKN1A'", "'PDGFB'", "'KDM6B'", "'BTG2'", "'SPRY2'", "'ITGB1'", "'FOXA2'", "'ATP1A1'", "'LAMC1'",
                    "'CTDSP1'", "'PSIP1'", "'JAG1'", "'ADCY9'", "'DLX2'", "'EFNA3'", "'PTPN12'", "'VEGFA'", "'CXCR4'", "'4632428N05RIK'",
                    "'ACVR1B'", "'ARNTL'", "'MYB'")
genes.selected.df <- genes.df.final[genes.df.final$Gene.Name %in% selected.genes, ]
write.csv(genes.selected.df, file="~/Escritorio/Jaume_ Compartit_Esrrb/Jaume_Files/genes_selected.xlsx", row.names = FALSE)
