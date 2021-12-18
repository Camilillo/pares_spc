library("data.table")
library("stringr")

setwd("C:\\Users\\lillcam\\Documents\\F2spectral\\F0")

data_pt = fread("pozos_pgeo_pozos.csv")
data_pt = as.data.frame(data_pt)
#data_pt = data_pt[(data_pt$mensual > 0)&(!is.na(data_pt$mensual)),]

aux = str_split(data_pt$fbpid, "-", simplify = TRUE)

data_pt$FASE  = aux[,1]
data_pt$BANCO = aux[,2]
data_pt$PATIO = aux[,3]

# length(na.omit(data_pt_2$Rec_BOT))
# length(na.omit(data_pt_2$Rec))
# length(na.omit(data_pt_2$Rec_cortado))

data_pt_2 = as.data.frame(data_pt)

data_CON = data_pt_2[data_pt_2$tipo == "LIX",]

datason = fread("C:\\Users\\lillcam\\Documents\\F2spectral\\F0\\cy19comp_to_pancho_b15_entry.csv")

#datason = datason[(!is.na(datason$rec_leyes))&(datason$rec_leyes > 0),]
#datason$malla100_modelo = datason$malla100_modelo -1.5

#meses = unique(data_CON$mensual)[-1]

MATFIN = numeric()
#for(jm in 1:length(meses)){

data_CON_mensual = data_CON

bancos = unique(data_CON_mensual$BANCO)

MAT = numeric()
for(du in 1:length(bancos)){
  
  b_aux = as.numeric(bancos[du])
  
  limits_ymax = c(max(data_CON_mensual$norte) + 20)
  limits_xmax = c(max(data_CON_mensual$este) + 20)
  limits_ymin = c(min(data_CON_mensual$norte) - 20)
  limits_xmin = c(min(data_CON_mensual$este) - 20)
  
  datason_b = datason[(datason$midx > limits_xmin),]
  datason_b = datason_b[(datason_b$midy > limits_ymin),]
  datason_b = datason_b[(datason_b$midx < limits_xmax),]
  datason_b = datason_b[(datason_b$midy < limits_ymax),]
  datason_b = datason_b[(datason_b$midz < ((b_aux + 15))),]
  datason_b = datason_b[(datason_b$midz > ((b_aux + 0.0))),]
  
  if(dim(datason_b) > 0){
    
    distancias = matrix(NA,
                        ncol = NROW(data_CON_mensual),
                        nrow = NROW(datason_b))
    
    for(u in 1:NROW(datason_b)){
      distancias[u,] =  
        sqrt(
          ((datason_b$midx[u] - data_CON_mensual$este)^2) +
            ((datason_b$midy[u] - data_CON_mensual$norte)^2) +
            ((datason_b$midz[u] - data_CON_mensual$cota)^2)
        )  
    }
    
    
    for(k in 1:min(dim(distancias))){
      auz = which(distancias == min(distancias), arr.ind = TRUE)[1,]
      MAT = rbind(MAT, as.data.frame(cbind( datason_b[auz[1],],
                                            data_CON_mensual[auz[2],])))
      
      distancias[auz[1],] = rep(99999, length(distancias[auz[1],]))
      distancias[,auz[2]] = rep(99999, length(distancias[,auz[2]]))
      
    }
    
  }
  
  nombres1 = paste(colnames(datason_b),        "_son", sep = "")
  nombres2 = paste(colnames(data_CON_mensual), "_pt", sep = "")
  
  MAT_FIN2 = MAT
  
  colnames(MAT_FIN2) = c(nombres1, nombres2)
  
  
  MAT_FIN2$dist =
    sqrt(
      ((MAT_FIN2$este_pt - MAT_FIN2$midx_son)^2) +
        ((MAT_FIN2$norte_pt - MAT_FIN2$midy_son)^2) +
        ((MAT_FIN2$cota_pt - MAT_FIN2$midz_son)^2))
  
  MATFIN = rbind(MATFIN, MAT_FIN2)
  
  plot(data_CON_mensual$este, data_CON_mensual$norte)
  
  points(MAT_FIN2$midx_son[MAT_FIN2$dist < 10],
         MAT_FIN2$midy_son[MAT_FIN2$dist < 10], col = 2)
  
}
}


MATFIN_DIST = MATFIN[MATFIN$dist < 10,]

MATFIN_DIST = unique(MATFIN_DIST)

source("BHPplots.R")

dev.off()

dev.off()
par(mfrow = c(1,2))

source("BHPplots.R")

mean(MATFIN_DIST$cut_pt[(MATFIN_DIST$cut_pt > 0.3)&(MATFIN_DIST$cut_son > 0.3)])
mean(MATFIN_DIST$cut_son[(MATFIN_DIST$cut_pt > 0.3)&(MATFIN_DIST$cut_son > 0.3)])

BHP.QQplot(MATFIN_DIST$co3_pt, y = MATFIN_DIST$c, palette = TRUE, scale = 1, 
           xlabel = "CuT PT", ylabel = "CuT Son", slabel = 1.3,
           diag = TRUE, title = "Pares Lixiviación \n Dist < 10 mts", stitle = 1.6)

pdf("pares_lix.pdf", width = 7, height = 7)

BHP.scatter(data_CON_mensual$este, 
            data_CON_mensual$norte,
            xmin = 74800, ymin = 481500,
            xlabel = "Este", ylabel = "Norte",
            color = "gray87", spoint = 0.5)
points(data_CON_mensual$este, 
       data_CON_mensual$norte, cex = 0.5)

points(MATFIN_DIST$este_pt, 
       y = MATFIN_DIST$norte_pt, pch = 16, col = 2, cex = 0.5)

points(MATFIN_DIST$midx_son, 
       y = MATFIN_DIST$midy_son, pch = 16, col = 3, cex = 0.5)

dev.off()



