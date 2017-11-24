rm(list=ls())
library(class)
library(party)
library(caret)
library(e1071)
library(randomForest)
require (outliers)
require( mice )
require ( mvoutlier )
library(discretization)
library(FSelector)
library(adabag)
library(frbs)
# setwd("C:/Users/manue/Desktop/R Projects/KAGGLE/")

writeCsv = function(predi, f = "Prueba.csv"){
  write.csv(predi, file = f, quote = FALSE)
  ln = readLines(paste(f,sep = ""),-1)
  ln[1]="Id,Prediction"
  writeLines(ln,paste(f,sep = ""))
}

changeHour = function(db){
  h = as.character(db[["HORA"]])
  h = gsub(",", ".", h)
  h = as.numeric(h)
  db[["HORA"]] = ordered(cut(h, c(-1,0,1,2,3,4,5,6,7,8,9,10,11,12,
                                  13,14,15,16,17,18,19,20,21,22,23,24)), 
                         labels = as.character(c(0,1,2,3,4,5,6,7,8,9,10,11,12,
                                    13,14,15,16,17,18,19,20,21,22,23,24)))
  return(db)
}

factor2num = function(fact){
  levels(fact) = 1:length(levels(fact))
  x = as.numeric(fact)
  return(x);
}

database = read.csv("C:/Users/manue/Desktop/R Projects/KAGGLE/accidentes-kaggle.csv")
database[,"ACERAS"] = read.csv("C:/Users/manue/Desktop/R Projects/KAGGLE/MissingValues/train/ACERAS.csv")
database[,"ACOND_CALZADA"] = read.csv("C:/Users/manue/Desktop/R Projects/KAGGLE/MissingValues/train/ACOND_CALZADA.csv")
database[,"DENSIDAD_CIRCULACION"] = read.csv("C:/Users/manue/Desktop/R Projects/KAGGLE/MissingValues/train/DENSIDAD_CIRCULACION.csv")
database[,"PRIORIDAD"] = read.csv("C:/Users/manue/Desktop/R Projects/KAGGLE/MissingValues/train/PRIORIDAD.csv")
database[,"VISIBILIDAD_RESTRINGIDA"] = read.csv("C:/Users/manue/Desktop/R Projects/KAGGLE/MissingValues/train/VISIBILIDAD_RESTRINGIDA.csv")
database = changeHour(database)
database = database[sample(nrow(database)),]
database[,"X"] = NULL
database[,"MEDIDAS_ESPECIALES"] = NULL
database[,"ISLA"] = NULL
database[,"OTRA_CIRCUNSTANCIA"] = NULL
database[,"RED_CARRETERA"] = NULL
database[,"ANIO"] = NULL
database[,"MES"] = NULL
database[,"PROVINCIA"] = NULL
database[,"CARRETERA"] = NULL
# database[,"HORA"] = factor2num(database[,"HORA"])
# database[,"DIASEMANA"] = factor2num(database[,"DIASEMANA"])
# database[,"PROVINCIA"] = factor2num(database[,"PROVINCIA"])
# database[,"COMUNIDAD_AUTONOMA"] = factor2num(database[,"COMUNIDAD_AUTONOMA"])
# database[,"ZONA_AGRUPADA"] = factor2num(database[,"ZONA_AGRUPADA"])
# database[,"TIPO_VIA"] = factor2num(database[,"TIPO_VIA"])
# database[,"TRAZADO_NO_INTERSEC"] = factor2num(database[,"TRAZADO_NO_INTERSEC"])
# database[,"TIPO_INTERSEC"] = factor2num(database[,"TIPO_INTERSEC"])
# database[,"ACOND_CALZADA"] = factor2num(database[,"ACOND_CALZADA"])
# database[,"PRIORIDAD"] = factor2num(database[,"PRIORIDAD"])
# database[,"SUPERFICIE_CALZADA"] = factor2num(database[,"SUPERFICIE_CALZADA"])
# database[,"LUMINOSIDAD"] = factor2num(database[,"LUMINOSIDAD"])
# database[,"FACTORES_ATMOSFERICOS"] = factor2num(database[,"FACTORES_ATMOSFERICOS"])
# database[,"VISIBILIDAD_RESTRINGIDA"] = factor2num(database[,"VISIBILIDAD_RESTRINGIDA"])
# database[,"ACERAS"] = factor2num(database[,"ACERAS"])
# database[,"DENSIDAD_CIRCULACION"] = factor2num(database[,"DENSIDAD_CIRCULACION"])
ndatabase = length(names(database))

databaseClass1 = database
databaseClass1[,ndatabase] = as.character(databaseClass1[,ndatabase])
index = which(database[,ndatabase]=="Colision_Vehiculos",TRUE)
databaseClass1[-index,ndatabase] = "OTROS"
databaseClass1[,ndatabase] = as.factor(databaseClass1[,ndatabase])

databaseClass2 = database
databaseClass2[,ndatabase] = as.character(databaseClass2[,ndatabase])
index = which(databaseClass2[,ndatabase]=="Atropello",TRUE)
databaseClass2[-index,ndatabase] = "OTROS"
databaseClass2[,ndatabase] = as.factor(databaseClass2[,ndatabase])

databaseClass3 = database
databaseClass3[,ndatabase] = as.character(databaseClass3[,ndatabase])
index = which(databaseClass3[,ndatabase]=="Salida_Via",TRUE)
databaseClass3[-index,ndatabase] = "OTROS"
databaseClass3[,ndatabase] = as.factor(databaseClass3[,ndatabase])

databaseClass4 = database
databaseClass4[,ndatabase] = as.character(databaseClass4[,ndatabase])
index = which(databaseClass4[,ndatabase]=="Otro",TRUE)
databaseClass4[-index,ndatabase] = "OTROS"
databaseClass4[,ndatabase] = as.factor(databaseClass4[,ndatabase])

databaseClass5 = database
databaseClass5[,ndatabase] = as.character(databaseClass5[,ndatabase])
index = which(databaseClass5[,ndatabase]=="Vuelco",TRUE)
databaseClass5[-index,ndatabase] = "OTROS"
databaseClass5[,ndatabase] = as.factor(databaseClass5[,ndatabase])

databaseClass6 = database
databaseClass6[,ndatabase] = as.character(databaseClass6[,ndatabase])
index = which(databaseClass6[,ndatabase]=="Colision_Obstaculo",TRUE)
databaseClass6[-index,ndatabase] = "OTROS"
databaseClass6[,ndatabase] = as.factor(databaseClass6[,ndatabase])

# outliers
# for (i in 4:8){
#   outlierData = outlier(database[,i])
#   a = which(database[,i] == outlierData,TRUE)
#   database = database[-c(2, 4, 6), ]
# }

databaseTest = read.csv("C:/Users/manue/Desktop/R Projects/KAGGLE/accidentes-kaggle-test.csv")
databaseTest[,"ACERAS"] = read.csv("C:/Users/manue/Desktop/R Projects/KAGGLE/MissingValues/test/ACERAS.csv")
databaseTest[,"ACOND_CALZADA"] = read.csv("C:/Users/manue/Desktop/R Projects/KAGGLE/MissingValues/test/ACOND_CALZADA.csv")
databaseTest[,"DENSIDAD_CIRCULACION"] = read.csv("C:/Users/manue/Desktop/R Projects/KAGGLE/MissingValues/test/DENSIDAD_CIRCULACION.csv")
databaseTest[,"PRIORIDAD"] = read.csv("C:/Users/manue/Desktop/R Projects/KAGGLE/MissingValues/test/PRIORIDAD.csv")
databaseTest[,"VISIBILIDAD_RESTRINGIDA"] = read.csv("C:/Users/manue/Desktop/R Projects/KAGGLE/MissingValues/test/VISIBILIDAD_RESTRINGIDA.csv")
databaseTest = changeHour(databaseTest)
# databaseTest[,"HORA"] = factor2num(databaseTest[,"HORA"])
# databaseTest[,"DIASEMANA"] = factor2num(databaseTest[,"DIASEMANA"])
# databaseTest[,"PROVINCIA"] = factor2num(databaseTest[,"PROVINCIA"])
# databaseTest[,"COMUNIDAD_AUTONOMA"] = factor2num(databaseTest[,"COMUNIDAD_AUTONOMA"])
# databaseTest[,"ZONA_AGRUPADA"] = factor2num(databaseTest[,"ZONA_AGRUPADA"])
# databaseTest[,"TIPO_VIA"] = factor2num(databaseTest[,"TIPO_VIA"])
# databaseTest[,"TRAZADO_NO_INTERSEC"] = factor2num(databaseTest[,"TRAZADO_NO_INTERSEC"])
# databaseTest[,"TIPO_INTERSEC"] = factor2num(databaseTest[,"TIPO_INTERSEC"])
# databaseTest[,"ACOND_CALZADA"] = factor2num(databaseTest[,"ACOND_CALZADA"])
# databaseTest[,"PRIORIDAD"] = factor2num(databaseTest[,"PRIORIDAD"])
# databaseTest[,"SUPERFICIE_CALZADA"] = factor2num(databaseTest[,"SUPERFICIE_CALZADA"])
# databaseTest[,"LUMINOSIDAD"] = factor2num(databaseTest[,"LUMINOSIDAD"])
# databaseTest[,"FACTORES_ATMOSFERICOS"] = factor2num(databaseTest[,"FACTORES_ATMOSFERICOS"])
# databaseTest[,"VISIBILIDAD_RESTRINGIDA"] = factor2num(databaseTest[,"VISIBILIDAD_RESTRINGIDA"])
# databaseTest[,"ACERAS"] = factor2num(databaseTest[,"ACERAS"])
# databaseTest[,"DENSIDAD_CIRCULACION"] = factor2num(databaseTest[,"DENSIDAD_CIRCULACION"])
databaseTest[,"X"] = NULL
databaseTest[,"MEDIDAS_ESPECIALES"] = NULL
databaseTest[,"ISLA"] = NULL
databaseTest[,"OTRA_CIRCUNSTANCIA"] = NULL
databaseTest[,"RED_CARRETERA"] = NULL
databaseTest[,"ANIO"] = NULL
databaseTest[,"MES"] = NULL
databaseTest[,"PROVINCIA"] = NULL
databaseTest[,"CARRETERA"] = NULL
ntst = length(names(databaseTest))

# AMEVA
# dis = rbind(database[,-ndatabase],databaseTest)
# cuts= disc.Topdown (dis, method=3)
# dis = cuts$Disc.data
# database = cbind(dis[1:dim(database)[1],], database[,ndatabase])
# d = dim(database)[1]+1
# r = rownames(databaseTest)
# databaseTest = dis[ d : dim(dis)[1],]
# rownames(databaseTest) = r
# colnames(database)[ndatabase] = "TIPO_ACCIDENTE"

###################################### Random forest  with CV ####################################

ind = seq(1,nrow(database),by=1)
trainPart = createFolds(ind, k = 3, returnTrain = TRUE)
testPart = list()
for(i in 1:3){testPart[[i]] = ind[-trainPart[[i]]]}


bestM = 0
for (i in seq(40, 450, by = 45)){
  print(i)
  
  cvMean = 0
  for(j in 1:3){
    model = randomForest::randomForest(TIPO_ACCIDENTE ~., 
                                       data=database[trainPart[[j]],], 
                                       ntree=i, 
                                       mtry = 4, 
                                       maxnodes = 1000)
    predictions = predict(model, database[testPart[[j]],-ndatabase])
    cvMean = cvMean + mean(predictions==database[testPart[[j]],ndatabase])
  }
  
  cvMean = cvMean / 3
  if (bestM<cvMean){
    bestM = cvMean
    bestModel = model
    cat("New best model! ",cvMean," ",i,"\n")
  }
}

predictions = predict(bestModel, databaseTest)
writeCsv(predictions, f = "Prueba.csv")

######################## bucle que ejecuta repetidas veces el algoritmo ##############################
# 
# bestM = 0
# for (i in 1:50){
#   print(i)
# 
#   db = createDataPartition(y=database[,ndatabase], p = 0.5, list = FALSE)
#   dbTrain = database[ db,]
#   dbTest  = database[-db,]
#   model = randomForest::randomForest(TIPO_ACCIDENTE ~., 
#                                      data=dbTrain, 
#                                      ntree=2000, 
#                                      mtry = 6, nodesize=2, 
#                                      maxnodes = 500)
#   predictions = predict(model, newdata = dbTest[,-ndatabase])
#   m = mean(predictions==dbTest[,ndatabase])
#   m
#   
#   if (bestM<m){
#     bestM = m
#     bestModel = model
#     cat("New best model! ",m," ",i,"\n")
#   }
# }
# 
# predictions = predict(model, databaseTest)
# writeCsv(predictions, f = "Prueba.csv")

###################################### --- ####################################
# 
# databaseDOP = with(database,data.frame(
#   HORA,
#   model.matrix(~DIASEMANA-1,database),
#   model.matrix(~COMUNIDAD_AUTONOMA-1,database),
#   TOT_VICTIMAS,
#   TOT_MUERTOS,
#   TOT_HERIDOS_GRAVES,
#   TOT_HERIDOS_LEVES,
#   TOT_VEHICULOS_IMPLICADOS,
#   model.matrix(~ZONA-1,database),
#   model.matrix(~ZONA_AGRUPADA-1,database),
#   model.matrix(~TIPO_VIA-1,database),
#   model.matrix(~TRAZADO_NO_INTERSEC-1,database),
#   model.matrix(~ACOND_CALZADA-1,database),
#   model.matrix(~PRIORIDAD-1,database),
#   model.matrix(~SUPERFICIE_CALZADA-1,database),
#   model.matrix(~LUMINOSIDAD-1,database),
#   model.matrix(~FACTORES_ATMOSFERICOS-1,database),
#   model.matrix(~ACERAS-1,database),
#   model.matrix(~DENSIDAD_CIRCULACION-1,database),
#   TIPO_ACCIDENTE))
# ndatabaseDOP = length(names(databaseDOP))
# 
# db = createDataPartition(y=databaseDOP[,ndatabaseDOP], p = 0.80, list = FALSE)
# dbTrain = databaseDOP[ db,]
# dbTest  = databaseDOP[-db,]
# dbTrain[,ndatabaseDOP] = as.factor(as.character(dbTrain[,ndatabaseDOP]))
# 
# model = randomForest::randomForest(TIPO_ACCIDENTE ~.,
#                               data=dbTrain,
#                               ntree=50,
#                               mtry = 10, maxnodes=1000)
# 
# predictions = predict(model, newdata = dbTest[,-ndatabaseDOP])
# confusionMatrix(table(predictions,dbTest[,ndatabaseDOP]))
# mean(predictions==dbTest[,ndatabaseDOP])


###################################### RF ####################################

# 
# i1 = which(database[,ndatabase]=="Colision_Obstaculo",TRUE)
# i2 = which(database[,ndatabase]=="Atropello",TRUE)
# i3 = which(database[,ndatabase]=="Otro",TRUE)
# i4 = which(database[,ndatabase]=="Salida_Via",TRUE)
# i5 = which(database[,ndatabase]=="Vuelco",TRUE)
# i6 = which(database[,ndatabase]=="Colision_Vehiculos",TRUE)
# 
# db2 = rbind(database[i1[1:2],],
#            database[i2,],
#            database[i3,],
#            database[i4,],
#            database[i5[1:2],],
#            database[i6,])
# 
# db = createDataPartition(y=db2[,ndatabase], p = 0.50, list = FALSE)
# dbTrain = db2[ db,]
# dbTest  = db2[-db,]
# dbTrain[,ndatabase] = as.factor(as.character(dbTrain[,ndatabase]))
# 
# model = randomForest::randomForest(TIPO_ACCIDENTE ~., 
#                                            data=dbTrain, 
#                                            ntree=100)
# predictions = predict(model, newdata = dbTest[,-ndatabase])
# confusionMatrix(table(predictions,dbTest[,ndatabase]))
# mean(predictions==dbTest[,ndatabase])
# 
# predictions = predict(model, databaseTest)
# writeCsv(predictions, f = "Prueba.csv")

###################################### boosting ####################################

# weights = FSelector::chi.squared(TIPO_ACCIDENTE~.,database)
# sub = FSelector::cutoff.k(weights, 12)
# f = as.simple.formula(sub, "TIPO_ACCIDENTE")
# db = createDataPartition(y=database[,ndatabase], p = 0.8, list = FALSE)
# dbTrain = database[ db,]
# dbTest  = database[-db,]
# dbTrain[,ndatabase] = as.factor(as.character(dbTrain[,ndatabase]))
# model = adabag::boosting(f, data = dbTrain, 
#                            mfinal = 10, 
#                            control = rpart::rpart.control(maxdepth = 3))
# predictions = predict.boosting(model, newdata = dbTest[,-ndatabase])
# mean(predictions$class==dbTest[ndatabase])

###################################### bagging ####################################

# weights = FSelector::chi.squared(TIPO_ACCIDENTE~.,database)
# sub = FSelector::cutoff.k(weights, 12)
# f = as.simple.formula(sub, "TIPO_ACCIDENTE")
# db = createDataPartition(y=database[,ndatabase], p = 0.8, list = FALSE)
# dbTrain = database[ db,]
# dbTest  = database[-db,]
# dbTrain[,ndatabase] = as.factor(as.character(dbTrain[,ndatabase]))
# model = adabag::bagging(f, 
#                 data = dbTrain, 
#                 control=rpart::rpart.control(maxdepth=5, minsplit=15))
# predictions = predict.bagging(model, newdata = dbTest[,-ndatabase])
# mean(predictions$class==dbTest[,ndatabase])


###################################### separated By class ####################################

# weights = FSelector::chi.squared(TIPO_ACCIDENTE~.,databaseClass1)
# sub = FSelector::cutoff.k(weights, 1)
# f = as.simple.formula(sub, "TIPO_ACCIDENTE")
# db1 = createDataPartition(y=databaseClass1[,ndatabase], p = 0.8, list = FALSE)
# db1Train = databaseClass1[ db1,]
# db1Test  = databaseClass1[-db1,]
# db1Train[,ndatabase] = as.factor(as.character(db1Train[,ndatabase]))
# model1 = knn(train = db1Train[,-ndatabase], test = db1Train[,-ndatabase] ,cl = db1Train[,ndatabase], k=1)
# predictions1 = predict(model1, db1Test[,-ndatabase])
# confusionMatrix(predictions1, db1Test[,ndatabase])
# 
# weights = FSelector::chi.squared(TIPO_ACCIDENTE~.,databaseClass2)
# sub = FSelector::cutoff.k(weights, 2)
# f = as.simple.formula(sub, "TIPO_ACCIDENTE")
# db2 = createDataPartition(y=databaseClass2[,ndatabase], p = 0.8, list = FALSE)
# db2Train = databaseClass2[ db2,]
# db2Test  = databaseClass2[-db2,]
# db2Train[,ndatabase] = as.factor(as.character(db2Train[,ndatabase]))
# model2 = randomForest::randomForest(f, data=db2Train, ntree=50, mtry = 2, maxnodes=100)
# predictions2 = predict(model2, db2Test[,-ndatabase])
# confusionMatrix(predictions2, db2Test[,ndatabase])
# 
# weights = FSelector::chi.squared(TIPO_ACCIDENTE~.,databaseClass3)
# sub = FSelector::cutoff.k(weights, 4)
# f = as.simple.formula(sub, "TIPO_ACCIDENTE")
# db3 = createDataPartition(y=databaseClass3[,ndatabase], p = 0.8, list = FALSE)
# db3Train = databaseClass3[ db3,]
# db3Test  = databaseClass3[-db3,]
# db3Train[,ndatabase] = as.factor(as.character(db3Train[,ndatabase]))
# model3 = randomForest::randomForest(f, data=db3Train, ntree=200, mtry = 3, maxnodes=100)
# predictions3 = predict(model3, db3Test[,-ndatabase])
# confusionMatrix(predictions3, db3Test[,ndatabase])
# 
# weights = FSelector::chi.squared(TIPO_ACCIDENTE~.,databaseClass4)
# sub = FSelector::cutoff.k(weights, 19)
# f = as.simple.formula(sub, "TIPO_ACCIDENTE")
# db4 = createDataPartition(y=databaseClass4[,ndatabase], p = 0.8, list = FALSE)
# db4Train = databaseClass4[ db4,]
# db4Test  = databaseClass4[-db4,]
# db4Train[,ndatabase] = as.factor(as.character(db4Train[,ndatabase]))
# model4 = randomForest::randomForest(f, data=db4Train, ntree=100, mtry = 19, maxnodes=100)
# predictions4 = predict(model4, db4Test[,-ndatabase])
# confusionMatrix(predictions4, db4Test[,ndatabase])
# 
# weights = FSelector::chi.squared(TIPO_ACCIDENTE~.,databaseClass5)
# sub = FSelector::cutoff.k(weights, 5)
# f = as.simple.formula(sub, "TIPO_ACCIDENTE")
# db5 = createDataPartition(y=databaseClass5[,ndatabase], p = 0.8, list = FALSE)
# db5Train = databaseClass5[ db5,]
# db5Test  = databaseClass5[-db5,]
# db5Train[,ndatabase] = as.factor(as.character(db5Train[,ndatabase]))
# model5 = randomForest::randomForest(f, data=db5Train, ntree=100, mtry = 2, maxnodes=100)
# predictions5 = predict(model5, db5Test[,-ndatabase])
# confusionMatrix(predictions5, db5Test[,ndatabase])
# 
# weights = FSelector::chi.squared(TIPO_ACCIDENTE~.,databaseClass6)
# sub = FSelector::cutoff.k(weights, 5)
# f = as.simple.formula(sub, "TIPO_ACCIDENTE")
# db6 = createDataPartition(y=databaseClass6[,ndatabase], p = 0.8, list = FALSE)
# db6Train = databaseClass6[ db6,]
# db6Test  = databaseClass6[-db6,]
# db6Train[,ndatabase] = as.factor(as.character(db6Train[,ndatabase]))
# model6 = randomForest::randomForest(f, data=db6Train, ntree=100, mtry = 2, maxnodes=100)
# predictions6 = predict(model6, db6Test[,-ndatabase])
# confusionMatrix(predictions6, db6Test[,ndatabase])
# 
# ##### export ####
# 
# predictions1 = as.character(predict(model1, databaseTest))
# predictions2 = as.character(predict(model2, databaseTest))
# predictions3 = as.character(predict(model3, databaseTest))
# predictions4 = as.character(predict(model4, databaseTest))
# predictions5 = as.character(predict(model5, databaseTest))
# predictions6 = as.character(predict(model5, databaseTest))
# 
# i = which(predictions1=="OTROS",TRUE)
# predictions1[i] = predictions3[i]
# i2 = which(predictions1=="OTROS",TRUE)
# predictions1[i2] = predictions2[i2]
# i3 = which(predictions1=="OTROS",TRUE)
# predictions1[i3] = predictions4[i3]
# i4 = which(predictions1=="OTROS",TRUE)
# predictions1[i4] = predictions5[i4]
# i5 = which(predictions1=="OTROS",TRUE)
# predictions1[i5] = predictions6[i5]
# 
# writeCsv(predictions1, f = "Prueba.csv")


