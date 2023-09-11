# PREPROCESAMIENTO

library(data.table)

home <- "C:/Users/feder/Documents/DMEyF/dmeyf2023"

dt <- fread(paste0(home, "./datasets/competencia_01.csv"))

### CENTILES

# Variables con drift
cols_centiles <- c(
  "mcuentas_saldo",
  "mcuenta_corriente",
  "mcomisiones",
  #"cpayroll_trx",
  "Visa_msaldopesos",
  "Visa_msaldototal",
  "Visa_mpagominimo",
  "mcomisiones_otras",
  #"ccomisiones_mantenimiento",
  "mcaja_ahorro_dolares",
  "mtransferencias_recibidas",
  "Master_msaldopesos",
  "Master_msaldototal",
  "mtransferencias_emitidas",
  "mcheques_emitidos_rechazados",
  "minversion1_pesos",
  "minversion2",
  "mcheques_emitidos",
  "mprestamos_prendarios",
  "Master_mconsumosdolares",
  "Visa_mpagosdolares",
  "mplazo_fijo_dolares",
  "mpayroll2",
  #"ccuenta_debitos_automaticos",
  "mcuenta_debitos_automaticos",
  "mforex_sell",
  "mcheques_depositados_rechazados"
)

foto_meses <- c("202103", "202104", "202105")



for (col in cols_centiles){
  colname <- paste0(col, "_centile")
  dt1 <- within(dt[foto_mes == "202103"], assign(colname,as.integer(cut(dt[foto_mes == "202103",get(col)], unique(quantile(dt[foto_mes == "202103",get(col)], probs=seq(0,1,0.01), na.rm = T, include.lowest = T))))))
  dt2 <- within(dt[foto_mes == "202104"], assign(colname,as.integer(cut(dt[foto_mes == "202104",get(col)], unique(quantile(dt[foto_mes == "202104",get(col)], probs=seq(0,1,0.01), na.rm = T, include.lowest = T))))))
  dt3 <- within(dt[foto_mes == "202105"], assign(colname,as.integer(cut(dt[foto_mes == "202105",get(col)], unique(quantile(dt[foto_mes == "202105",get(col)], probs=seq(0,1,0.01), na.rm = T, include.lowest = T))))))
  
  dt <- rbind(dt1, dt2, dt3)  
  dt <- within(dt, assign(col, NULL))
}

### WRITE

data.table::fwrite(dt, paste0(home, "./datasets/competencia_centiles.csv"))




