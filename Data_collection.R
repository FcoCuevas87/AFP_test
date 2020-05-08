rm(list = ls())

library(httr)
library(rvest)

form <- NULL

date.grid <- as.matrix(expand.grid(1:12,2011:2020))
tp.m <- matrix(0,nrow = nrow(date.grid),ncol = 7) 
tp.m[,2:1] <- date.grid
Fondos <- list("CAPITAL"= list("Rentabilidad" = tp.m, "Acumulado" = tp.m, "Ult12Meses" = tp.m, "Prom_anual" = tp.m),
               "CUPRUM" = list("Rentabilidad" = tp.m, "Acumulado" = tp.m, "Ult12Meses" = tp.m, "Prom_anual" = tp.m),
               "HABITAT"= list("Rentabilidad" = tp.m, "Acumulado" = tp.m, "Ult12Meses" = tp.m, "Prom_anual" = tp.m),
               "MODELO"= list("Rentabilidad" = tp.m, "Acumulado" = tp.m, "Ult12Meses" = tp.m, "Prom_anual" = tp.m),
               "PLANVITAL"= list("Rentabilidad" = tp.m, "Acumulado" = tp.m, "Ult12Meses" = tp.m, "Prom_anual" = tp.m),
               "PROVIDA"= list("Rentabilidad" = tp.m, "Acumulado" = tp.m, "Ult12Meses" = tp.m, "Prom_anual" = tp.m),
               "UNO"= list("Rentabilidad" = tp.m, "Acumulado" = tp.m, "Ult12Meses" = tp.m, "Prom_anual" = tp.m),
               "SISTEMA"= list("Rentabilidad" = tp.m, "Acumulado" = tp.m, "Ult12Meses" = tp.m, "Prom_anual" = tp.m))

df2format <- function(x){
  nnn <- nrow(x)
  tbl.tmp <- t(x[4:nnn,])
  rownames(tbl.tmp) <- NULL
  nombres <- tbl.tmp[1,]
  vvv <- apply(tbl.tmp[-1,],1, function(x) as.numeric(gsub(",",".",gsub("%", "", x))) )
  vvv <- list(nombres = nombres, values = vvv)
  return(vvv)
}

URL <- "https://www.spensiones.cl/apps/rentabilidad/getRentabilidad.php?tiprent=FP&template=1"
jj <- "02"; i <- 2018; ll <- 0
for(i in 2011:2020){
  for(jj in c("01","02","03","04","05","06","07","08","09","10","11","12")){
    ll <- ll + 1
    
    j <- as.numeric(jj)
    
    #Query, codigo html
    form <- POST(url = URL,
         body = list("aaaa" = i, "mm" = jj, btn = "Buscar")) 
  
    tbls <- html_nodes(content(form), "table")
    
    #Cargando tabla de los distintos fondos
    tbl.list <- list()
    for(kk in 3:7) tbl.list[[kk-2]] <- df2format(html_table(tbls[[kk]],fill=TRUE)) #A 
  
    n0 <- length(tbl.list[[1]]$nombres) 
    for(kk in 1:n0){
      Fondos[[ tbl.list[[1]]$nombres[kk] ]]$Rentabilidad[ll,3:7] <- unlist(lapply(tbl.list,function(x) x$values[kk,1]))
      Fondos[[ tbl.list[[1]]$nombres[kk] ]]$Acumulado[ll,3:7] <- unlist(lapply(tbl.list,function(x) x$values[kk,2]))
      Fondos[[ tbl.list[[1]]$nombres[kk] ]]$Ult12Meses[ll,3:7] <- unlist(lapply(tbl.list,function(x) x$values[kk,3])) 
      Fondos[[ tbl.list[[1]]$nombres[kk] ]]$Prom_anual[ll,3:7] <- unlist(lapply(tbl.list,function(x) x$values[kk,4]))  
    }
    
   Sys.sleep(2) 
  }
}

unlist(lapply(tbl.list,function(x) x$values[1,1]))
unlist(lapply(tbl.list,function(x) x$values[1,1]))
unlist(lapply(tbl.list,function(x) x$values[1,1]))
unlist(lapply(tbl.list,function(x) x$values[1,1]))
unlist(lapply(tbl.list,function(x) x$values[1,1]))




