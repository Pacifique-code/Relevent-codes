setwd("~/Bureau")

data = read.csv("Monthly_TerraClimate_Kir_Muy.csv")
#data2 = as.numeric(paste(data))
mat = as.matrix((data[,2:8]))
View(mat)

corr_mat=cor(mat,method="s")
library(corrplot)
corrplot(corr_mat)
library(RColorBrewer)
corrplot(corr_mat, method = "color", outline = T, addgrid.col = "darkgray", order="hclust", addrect = 4, rect.col = "black", 
         rect.lwd = 5,cl.pos = "b", tl.col = "indianred4", tl.cex = 1.5, cl.cex = 1.5, addCoef.col = "white", number.digits = 2, 
         number.cex = 0.75, col = colorRampPalette(c("darkred","white","midnightblue"))(100))
