#------------------------------------------- MODELOS ------------------------------------------- 
library(party)
# Planteamiento de modelos (Numero de accidentes por mes por barrio)
mmm <- cforest(N_ACC_MES_BARRIO~COMUNA+PERIODO+CLASE+DISENO+GRAVEDAD+MES,
               data = aranjuez,
               controls = cforest_unbiased(ntree = 150, mtry = 4))
View(aranjuez)
mmm
p <- predict(mmm, aranjuez, OOB = TRUE,
             type = "response")
p
mean((p-aranjuez$N_ACC_MES_BARRIO)^2)

# Planteamiento de modelos (Numero de accidentes por semana por barrio)
mmmm <- cforest(N_ACC_SEMANA_BARRIO~COMUNA+PERIODO+CLASE+DISENO+GRAVEDAD+MES,
                data = aranjuez,
                controls = cforest_unbiased(ntree = 100, mtry = 4))
View(aranjuez)
mmmm
p <- predict(mmmm, aranjuez, OOB = TRUE,
             type = "response")
p
mean((p-aranjuez$N_ACC_SEMANA_BARRIO)^2)

# Planteamiento de modelos (Numero de accidentes por dia por barrio)
mmmmmmm <- cforest(N_ACC_DIA_BARRIO~DIA+COMUNA+PERIODO+CLASE+DISENO+GRAVEDAD+MES,
                   data = aranjuez,
                   controls = cforest_unbiased(ntree = 100, mtry = 4))
View(aranjuez)
mmmmmmm
p <- predict(mmmmmmm, aranjuez, OOB = TRUE,
             type = "response")
p
mean((p-aranjuez$N_ACC_DIA_BARRIO)^2)
