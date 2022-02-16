# StemCellsandDevelopment

1. read_genes_network.R

From a given list of genes I extract the max fold ratio and if the value is <0 I assign an 'activator' relationship betweeen kd ESRRB and the gene, when >0 the relationship is 'repressor'. This is done for 3 different emrbionary stages: day1, day3 and day5. The output is a table with the list of genes, their value for each day and the relationship to ESRRB.

2. read_mri_direction.R
Same as the program above but in this case with a list of miRNAs.
