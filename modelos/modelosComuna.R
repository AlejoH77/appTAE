#------------------------------------------- MODELO ------------------------------------------- 
aranjuez <- subset(af1, COMUNA == "Aranjuez")
m19 <- cforest(N_ACC_MES_COMUNA~PERIODO+CLASE+DISENO+GRAVEDAD+MES,
               data = aranjuez,
               controls = cforest_unbiased(ntree = 100, mtry = 5))
m19
p <- predict(m19, aranjuez, OOB = TRUE,
             type = "response")
p
mean((p-aranjuez$N_ACC_MES_COMUNA)^2)

m19 <- cforest(N_ACC_MES_COMUNA~PERIODO+CLASE+DISENO+GRAVEDAD+MES,
               data = subset(af1, COMUNA == "Aranjuez"),
               controls = cforest_unbiased(ntree = 100, mtry = 5))
m19
p <- predict(m19, aranjuez, OOB = TRUE,
             type = "response")
p
mean((p-subset(af1, COMUNA == "Aranjuez")$N_ACC_MES_COMUNA)^2)
