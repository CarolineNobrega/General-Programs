#######################################################################
####################### CC - SDM - IUNC - IPAM ########################
#######################################################################
library("maptools")
library("raster")
library("foreign")
memory.limit()
memory.limit(size=16024)              
#######################################################################
########Riqueza dos poligonos da IUCN##########
################################################

amazonia <- readShapePoly("C:\\Laboratorio\\Imagens\\Base_ambiental\\Biomes\\amaz_legal_Dis.shp",
      proj4string=CRS("+proj=longlat +datum=WGS84"))
brazil <- readShapePoly("C:\\Laboratorio\\Imagens\\Base_cartografica\\Brazil.shp",
      proj4string=CRS("+proj=longlat +datum=WGS84"))
neo <- readShapePoly("C:\\Laboratorio\\Imagens\\Base_cartografica\\neotropic_Dissolve.shp",
                    proj4string=CRS("+proj=longlat +datum=WGS84"))
mammals <- readShapePoly("C:\\IPAM\\IUCN_Shapes\\Mammals_Terrestrial.shp",
                        proj4string=CRS("+proj=longlat +datum=WGS84"))
mam_neo <- crop(mammals,neo)

# Shape já cortado para a América do Sul #
mamneo <- readShapePoly("C:\\IPAM\\IUCN_Shapes\\Mammals_Ter_neotrop.shp",
                          proj4string=CRS("+proj=longlat +datum=WGS84"))
exemplo<- raster("C:\\IPAM\\IUCN_Shapes\\Raster\\exemplo.asc")

################################################
# Selecionar shape de acordo com o nome das spp da lista de spp ameacadas do icmbio #
# Salvar arquivo raster#
################################################
tab<-read.table("C:\\IPAM\\IUCN_Shapes\\lista_spp_ameacadas_icmbio.csv", header = TRUE, dec = ".", sep=";")
tab_mam<-subset(tab,tab$grupo_tax=="Mamíferos",select=c("spp"))
for (i in 10:nrow(tab_mam))
{
  submap <- subset(mamneo, mamneo$BINOMIAL== as.character(tab_mam[i,1]))
  mask<- rasterize(submap, exemplo, field=1)
  writeRaster(mask, paste("C:\\IPAM\\IUCN_Shapes\\Raster\\",as.character(tab_mam[i,1]),".asc",sep='') , format="ascii", overwrite=TRUE, NAflag=-9999) 
}

#######################################################################
# Criar tabela com pontos a partir dos arquivos .asc #
################################################
files <- list.files(path=choose.dir(), pattern='.asc', full.names=T )
pts<-data.frame(spp=NA,POINT_X=NA,POINT_Y=NA)
x1<-data.frame(spp=NA,POINT_X=NA,POINT_Y=NA)
mask <- raster("C:\\Laboratorio\\Datasets\\bioclim\\Neotrop5\\bio1_n5.asc")
ext<-extent(mask)
for (i in 1:14) {
  print(i)
  distr<-raster(files[[i]])
  a<-unlist(strsplit(unlist(strsplit(files[[i]], "/"))[2],".asc"))[1]
  distr <- crop(distr, ext)
  x<-rasterToPoints(distr)
  # Selecionar pontos espacialmente únicos #
  cell <- cellFromXY(mask, x[,1:2]) # get the cell number for each point
  dup <- duplicated(cell)#x[,3],
  x <- x[!dup, ]# select the records that are not duplicated
  x1<-data.frame(spp=NA,POINT_X=NA,POINT_Y=NA)
  x1[1:nrow(x),2:3]<- x[1:nrow(x),1:2]
  x1[,1]<-a
  pts<-rbind(pts,x1)
  }
write.table(pts, file = "C:\\IPAM\\IUCN_Shapes\\Raster\\pts_oco1.csv",
            quote = F, col.names = TRUE, row.names = FALSE, dec = ".", sep = ",")

pts<-data.frame(spp=NA,POINT_X=NA,POINT_Y=NA)
for (i in 15:length(files)) {
  print(i)
  distr<-raster(files[[i]])
  a<-unlist(strsplit(unlist(strsplit(files[[i]], "/"))[2],".asc"))[1]
  distr <- crop(distr, ext)
  x<-rasterToPoints(distr)
  # Selecionar pontos espacialmente únicos #
  cell <- cellFromXY(mask, x[,1:2]) # get the cell number for each point
  dup <- duplicated(cell)#x[,3],
  x <- x[!dup, ]# select the records that are not duplicated
  x1<-data.frame(spp=NA,POINT_X=NA,POINT_Y=NA)
  x1[1:nrow(x),2:3]<- x[1:nrow(x),1:2]
  x1[,1]<-a
  pts<-rbind(pts,x1)
}
write.table(pts, file = "C:\\IPAM\\IUCN_Shapes\\Raster\\pts_oco2.csv",
            quote = F, col.names = TRUE, row.names = FALSE, dec = ".", sep = ",")

#######################################################################
# Calcular a area de distribuição das spp selecionando spp com pelo menos 20% da distr. na amazonia #
################################################
 res<-data.frame(spp=th$Species, n=th$n, AUC=th$AUC, th_min=th$th_min, th_roc=th$th_roc,th_shp=1, distr=NA, distr_amaz=NA, prop_distr=NA)
for (i in 1:nrow(th))
{
distr<-raster(paste("E:\\IPAM\\Nat_ICMBIO\\Distribuicao_Poligonos_Shapes\\shapes_spp\\",th$Species[i],".asc",sep=""))
s <- extract(distr,brazil)
res$distr[i]<-sum(s[[1]],na.rm = TRUE)
t <- extract(distr,amazonia)
res$distr_amaz[i]<-sum(t[[1]],na.rm = TRUE)
res$prop_distr[i]<-res$distr_amaz[i]/res$distr[i]
}
write.table(res, file = "E:\\IPAM\\model_iucn\\distrib_amaz.csv",
            quote = F, col.names = TRUE, row.names = FALSE, dec = ".", sep = ",")

#######################################################################
########## Calcular riqueza de espécies ################
################################################
th<-read.table("C:\\IPAM\\Modelos\\cccma\\maxentResults.csv", header = TRUE, dec = ".", sep=";")
distr<-raster(paste("C:\\IPAM\\Modelos\\cccma\\",th$spp[1],".asc",sep=""))
mask<- rasterize(brazil, distr, field=1) 
mask[!is.na(mask)]<-0
riqatual <- mask
#head(res)
#th<-subset(res, prop_distr>=0.2 , select=c(spp, n, AUC, th_min, th_roc, th_shp, distr, distr_amaz, prop_distr)) 

for (i in 1:nrow(th)){
atual<-raster(paste("C:\\IPAM\\Modelos\\cccma\\",th$spp[i],".asc",sep=""))
atual[is.na(atual)]<-0
fun1=function(x){x[x >= th$th_shp[i]] <- 1; x[x < th$th_shp[i]] <- 0; return(x)}
atual<-calc(atual,fun1)
riqatual<-riqatual+atual
}
writeRaster(riqatual, paste("C:\\IPAM\\Modelos\\cccma\\","riqatual.asc",sep='') , format="ascii", overwrite=TRUE, NAflag=-9999)

  

#######################################################################
# Criar tabela para análise mudanças climáticas - IPAM usando shapes da IUCN ###
# importa tabela dbf do shape de pontos da distrib das spp.
#####################################################################

files <- list.files(path=choose.dir(), pattern='dbf', full.names=T )
pts<-data.frame(GRID_CODE=NA,POINT_X=NA,POINT_Y=NA)
for (i in 1:length(files)) {
  pts1<-read.dbf(files[[i]], as.is = FALSE)
  a<-unlist(strsplit(unlist(strsplit(files[[i]], "/"))[2],".dbf"))[1]
  pts1$GRID_CODE<-a
  pts<-rbind(pts,pts1[,2:4])
}
head(pts)
pts2<-pts[2:nrow(pts),1:3]
write.table(ptss12, file = "G:\\Nat_ICMBIO\\Distribuicao_Poligonos_Shapes\\shapes_spp\\pts12_maxent.csv",
            quote = F, col.names = TRUE, row.names = FALSE, dec = ".", sep = ",")
x.sub <- subset(pts1, pts1$sp==c(factor(pts1$sp(1:unique(pts1$sp)[80])))
                x.sub <- grep(as.character(unique(pts1$sp)[80]), pts1$sp)
                ptss11<-pts1[1:754251,1:3]
                ptss12<-pts1[754252:nrow(pts1),1:3]
                sp<-pts1$sp=unique(pts1$sp)[80]
                

#######################################################################
# extrair valores de um raster de acordo com uma máscara em shapefile #
################################################
                
amazonia <- readShapePoly("C:\\Laboratorio\\Imagens\\Base_ambiental\\Biomes\\amaz_legal_Dis.shp",
  proj4string=CRS("+proj=longlat +datum=WGS84"))
th<-read.table("E:\\IPAM\\model_iucn\\hadcm3\\th_total.csv", header = TRUE, dec = ".", sep=";")
res<-data.frame(spp=th$Species, n=th$n, AUC=th$AUC, th_min=th$th_min, th_roc=th$th_roc, distr=NA, distr_amaz=NA, prop_distr=NA)
                
for (i in 1:nrow(th)){
  distr<-raster(paste("E:\\IPAM\\model_iucn\\hadcm3\\",th$Species[i],".asc",sep=""))
  fun=function(x){x[x >= th$th_roc[i]] <- 1; x[x < th$th_roc[i]] <- 0; return(x)}
  s<-calc(distr,fun)
  s <- extract(s,brazil)
  res$distr[i]<-sum(s[[1]],na.rm = TRUE)
  t <- extract(distr,amazonia)
  res$distr_amaz[i]<-sum(t[[1]],na.rm = TRUE)
  res$prop_distr[i]<-res$distr_amaz[i]/res$distr[i]
}
write.table(res, file = "G:\\model_iucn\\hadcm3\\distrib_amaz.csv",
quote = F, col.names = TRUE, row.names = FALSE, dec = ".", sep = ",")
                
                
                
                
           
                
#amazonia <- readShapePoly("C:\\Laboratorio\\Imagens\\Base_ambiental\\Biomes\\amaz_legal_Dis1.shp",
#        proj4string=CRS("+proj=longlat +datum=WGS84")) 
#amaz<- rasterize(amazonia, mask, field=1)
#writeRaster(amaz, "C:\\IPAM\\dados_amazonia\\raster\\amaz_legal_5min2.asc" , format="ascii", overwrite=TRUE, NAflag=-9999)
                
#######################################################################
################ Riqueza Atual e Futura #########################
# Calculo da riqueza de espécies, atual e futura, com e sem dispersão #
#######################################################################

rm(list = ls())
brazil <- readShapePoly("C:\\Laboratorio\\Imagens\\Base_cartografica\\Brazil.shp",
    proj4string=CRS("+proj=longlat +datum=WGS84"))
sp<-read.table("C:\\IPAM\\Paper_TI_biodiversity\\Modelos\\no85bi70\\maxentResults.csv", header = TRUE, dec = ".", sep=";")
mask<- rasterize(brazil, raster(paste("C:\\IPAM\\Paper_TI_biodiversity\\Modelos\\no85bi70\\",sp$Species[1],".asc",sep="")), field=1) 
mask[!is.na(mask)]<-0
amazonia<-raster("C:\\IPAM\\COP\\dados_amazonia\\raster\\amaz_legal_5min2.asc")
ti<-raster("C:\\IPAM\\COP\\dados_amazonia\\raster\\ti_5min.asc")
                
riqatual <- mask; # Riqueza atual
riqfut <- mask;   dif_riq <- mask;  # Riqueza futura e Dif. riqueza com dispersão ilimitada
riqSdisp <- mask;  dif_riqS <- mask;   #Riqueza Dif. riqueza de espécies sem dispersão 0
                  
res<-data.frame(spp=sp$Species, n=sp$N, AUC=sp$AUC, th_roc=sp$th_roc, amaz_atual=NA, amaz_fut=NA, ti_atual=NA, ti_fut=NA)
                            
for (i in 1:nrow(sp)){
  atual<-raster(paste("C:\\IPAM\\Paper_TI_biodiversity\\Modelos\\no85bi70\\",res$spp[i],".asc",sep=""))
  fut<-raster(paste("C:\\IPAM\\Paper_TI_biodiversity\\Modelos\\no85bi70\\",res$spp[i],"_no85bi70",".asc",sep="")) 
  extent(fut)<-extent(atual)  
   #Transformar mapa de adequabilidades em presença/ausência de bcordo com um th
  fun1=function(x){x[x >= sp$th_roc[i]] <- 1; x[x < sp$th_roc[i]] <- 0; return(x)}
  throc<-as.numeric(subset(sp, res$spp==as.character(res$spp[i]), select=c(th_roc)))
  #thmin<-as.numeric(subset(sp, res$spp==as.charbcter(res$spp[i]), select=c(th_min)))
  #fun1=function(x){x[x >= thmin] <- 1; x[x < thmin] <- 0; return(x)}
  atual<-calc(atual,fun1)
  riqatual<-riqatual+atual
  fut<-calc(fut,fun1)
  riqfut<-riqfut+fut
  riqSdisp<-riqSdisp+(atual*fut)
  
  x<-amazonia*atual
  res$amaz_atual[i]<-sum(as.vector(x[!is.na(x)]))
  x<-amazonia*fut
  res$amaz_fut[i]<-sum(as.vector(x[!is.na(x)]))
  x<-amazonia*ti*atual
  res$ti_atual[i]<-sum(as.vector(x[!is.na(x)]))
  x<-amazonia*ti*fut
  res$ti_fut[i]<-sum(as.vector(x[!is.na(x)]))
  
  print(i);# rm(s1,s2,s3,s4)
  #par(mfrow=c(2,2))
  #print(plot(riqatual,main='atual'));  #print(plot(riqfut,main='fut'))
  #print(plot(atual,main='atual'));  #print(plot(fut,main='fut'))
}

dif_riq<-riqatual-riqfut;   #dif_riq[!is.na(dif_riq)]<-riqatual[!is.na(riqatual)]-riqfut[!is.na(riqfut)]
dif_riqS<-riqatual-riqSdisp;   #dif_riqS[!is.na(dif_riqS)]<-riqatual[!is.na(riqatual)]-riqSdisp[!is.na(riqSdisp)]

writeRaster(riqatual, paste("C:\\IPAM\\Paper_TI_biodiversity\\Modelos\\no85bi70\\","riqatualno85roc.asc",sep='') , format="ascii", overwrite=TRUE, NAflag=-9999)
writeRaster(riqfut, paste("C:\\IPAM\\Paper_TI_biodiversity\\Modelos\\no85bi70\\","riqfutno85roc.asc",sep='') , format="ascii", overwrite=TRUE, NAflag=-9999)
writeRaster(riqSdisp, paste("C:\\IPAM\\Paper_TI_biodiversity\\Modelos\\no85bi70\\","riqSdispno85roc.asc",sep='') , format="ascii", overwrite=TRUE, NAflag=-9999)
writeRaster(dif_riq, paste("C:\\IPAM\\Paper_TI_biodiversity\\Modelos\\no85bi70\\","dif_riqno85roc.asc",sep='') , format="ascii", overwrite=TRUE, NAflag=-9999)
writeRaster(dif_riqS, paste("C:\\IPAM\\Paper_TI_biodiversity\\Modelos\\no85bi70\\","dif_riqSno85roc.asc",sep='') , format="ascii", overwrite=TRUE, NAflag=-9999)

write.table(res, file = "C:\\IPAM\\Paper_TI_biodiversity\\Modelos\\no85bi70\\distrib_amaz_ti.csv",
                            quote = F, col.names = TRUE, row.names = FALSE, dec = ".", sep = ",")

#######################################################################               
################ Média da Riqueza #########################
# Calculo da riqueza média de todos os modelos climáticos #
#######################################################################               
rm(list = ls())
dirs<- c("ac85bi70","bc85bi70","cc85bi70","cn85bi70","gf85bi70","gs85bi70","hd85bi70","he85bi70",
         "hg85bi70","in85bi70","ip85bi70","mc85bi70","mg85bi70","mi85bi70","mp85bi70","mr85bi70","no85bi70")
names<-c("ac85","bc85","cc85","cn85","gf85","gs85","hd85","he85",
        "hg85","in85","ip85","mc85","mg85","mi85","mp85","mr85","no85")

mask<- raster(paste("C:\\IPAM\\Paper_TI_biodiversity\\Modelos\\ac85bi70\\","riqatualac85roc.asc",sep=""))
mask[!is.na(mask)]<-0
riqatual <- mask; # Riqueza atual
riqfut <- mask;   dif_riq <- mask;  # Riqueza futura e Dif. riqueza com dispersão ilimitada
riqSdisp <- mask;  dif_riqS <- mask;   #Riqueza Dif. riqueza de espécies sem dispersão 0

for (i in 1:length(dirs)){ 
  riqatual1 <- raster(paste("C:\\IPAM\\Paper_TI_biodiversity\\Modelos\\",dirs[i], "\\riqatual",names[i],"roc.asc",sep=""))
  riqfut1 <- raster(paste("C:\\IPAM\\Paper_TI_biodiversity\\Modelos\\",dirs[i], "\\riqfut",names[i],"roc.asc",sep=""))
  riqSdisp1 <- raster(paste("C:\\IPAM\\Paper_TI_biodiversity\\Modelos\\",dirs[i], "\\riqSdisp",names[i],"roc.asc",sep=""))
  dif_riq1 <- raster(paste("C:\\IPAM\\Paper_TI_biodiversity\\Modelos\\",dirs[i], "\\dif_riq",names[i],"roc.asc",sep=""))
  dif_riqS1 <- raster(paste("C:\\IPAM\\Paper_TI_biodiversity\\Modelos\\",dirs[i], "\\dif_riqS",names[i],"roc.asc",sep="")) 
  
  riqatual <- riqatual + riqatual1
  riqfut <- riqfut + riqfut1
  riqSdisp <- riqSdisp + riqSdisp1
  dif_riq <- dif_riq + dif_riq1
  dif_riqS <- dif_riqS + dif_riqS1
}
  riqatual <- riqatual/length(dirs)
  riqfut <- riqfut/length(dirs)
  riqSdisp <- riqSdisp/length(dirs)
  dif_riq <- dif_riq/length(dirs)
  dif_riqS <- dif_riqS/length(dirs)

writeRaster(riqatual, paste("C:\\IPAM\\Paper_TI_biodiversity\\Modelos\\","riqatualMeanroc.asc",sep='') , format="ascii", overwrite=TRUE, NAflag=-9999)
writeRaster(riqfut, paste("C:\\IPAM\\Paper_TI_biodiversity\\Modelos\\","riqfutMeanroc.asc",sep='') , format="ascii", overwrite=TRUE, NAflag=-9999)
writeRaster(riqSdisp, paste("C:\\IPAM\\Paper_TI_biodiversity\\Modelos\\","riqSdispMeanroc.asc",sep='') , format="ascii", overwrite=TRUE, NAflag=-9999)
writeRaster(dif_riq, paste("C:\\IPAM\\Paper_TI_biodiversity\\Modelos\\","dif_riqMeanroc.asc",sep='') , format="ascii", overwrite=TRUE, NAflag=-9999)
writeRaster(dif_riqS, paste("C:\\IPAM\\Paper_TI_biodiversity\\Modelos\\","dif_riqSMeanroc.asc",sep='') , format="ascii", overwrite=TRUE, NAflag=-9999)

#######################################################################
################ Delta do Clima #########################
# Calcular o delta do clima para a região amazônica #
#########################################################
  rm(list = ls())
  dirs<- c("ac85bi70","bc85bi70","cc85bi70","cn85bi70","gf85bi70","gs85bi70","hd85bi70","he85bi70",
  "hg85bi70","in85bi70","ip85bi70","mc85bi70","mg85bi70","mi85bi70","mp85bi70","mr85bi70","no85bi70")
  amazonia<-raster("C:\\IPAM\\COP\\dados_amazonia\\raster\\amaz_legal_5min2.asc") 
  atual<-stack(list.files("C:\\Laboratorio\\Datasets\\CMIP5\\worldclim\\atual5m\\", pattern='.asc', full.names=T ))

  for (i in 1:length(dirs)){    
    assign(dirs[i], stack(list.files(path=paste("C:\\Laboratorio\\Datasets\\CMIP5\\worldclim\\mundo5m\\",dirs[[i]],"\\", sep=""), pattern='.tif', full.names=T ))) 
    
    a <- crop(get(dirs[i]), extent(atual))
    extent(a) <- extent(atual)
    assign(dirs[i], a)
                
    #for (j in 1:19) {
    #  name<-  unlist(strsplit(names(get(dirs[i])[[j]]), "bi70"))[2]
    #  writeRaster(get(dirs[i])[[j]], paste("C:\\Laboratorio\\Datasets\\CMIP5\\worldclim\\mundo5m\\",dirs[[i]],"\\","bio",name,"_n5.asc" ,sep='') , format="ascii", overwrite=TRUE, NAflag=-9999)
    #}   
  }               
  rm(a)             
  for (i in 1:length(dirs)) { # diferença->futuro - atual, criar stack para as diferentes variáveis
     for (j in 1:19) {  
       print(names(atual[[j]])); print(names(get(dirs[i])[[j]])) ### !!! bio<-names(atual[[j]])
       if (i==1) { assign(names(atual[[j]]), get(dirs[i])[[j]] - atual[[j]] )  } #- atual[[j]] 
       else assign(names(atual[[j]]), stack(get(names(atual[[j]])), (get(dirs[i])[[j]] - atual[[j]]) )) #- atual[[j]] 
  }
    } 
  rm(i,j)
                
      for (j in 1:19){
        rm(x)
        print(names(atual[[j]]))
        x<- get(names(atual[[j]])) * amazonia
        x<-calc(x, mean) 
       # a <- crop(x, extent(amazonia))
        writeRaster(x, paste("C:\\Laboratorio\\Datasets\\CMIP5\\worldclim\\bio5min_mean\\","mean_",names(atual[[j]]),".asc" ,sep='') , format="ascii", overwrite=TRUE, NAflag=-9999);
        
        x<- get(names(atual[[j]])) * amazonia
        x<-calc(x, sd) 
        
        writeRaster(x, paste("C:\\Laboratorio\\Datasets\\CMIP5\\worldclim\\bio5min_sd\\","sd_",names(atual[[j]]),".asc" ,sep='') , format="ascii", overwrite=TRUE, NAflag=-9999);  
      }           
      


      
      } }
                
      prec<- stack(d_cccma_12m, d_csiro_12m, d_had_12m)              
      x12<-calc(prec, mean) 
      temp<- stack(d_cccma_1m, d_csiro_1m, d_had_1m)              
      x1<-calc(temp, mean)          
                
                b1_atual<-raster("C:\\Laboratorio\\Datasets\\bioclim\\Neotrop5\\bio1_n5.asc") 
b12_atual<-raster("C:\\Laboratorio\\Datasets\\bioclim\\Neotrop5\\bio12_n5.asc") 
b1_cccma<- raster("C:\\Laboratorio\\Datasets\\CIAT_5\\cccma_b2a_2050\\bio1_n5.asc") 
b12_cccma<- raster("C:\\Laboratorio\\Datasets\\CIAT_5\\cccma_b2a_2050\\bio12_n5.asc")
b1_csiro<- raster("C:\\Laboratorio\\Datasets\\CIAT_5\\csiro_b2a_2050\\bio1_n5.asc") 
b12_csiro<- raster("C:\\Laboratorio\\Datasets\\CIAT_5\\csiro_b2a_2050\\bio12_n5.asc")
b1_had<- raster("C:\\Laboratorio\\Datasets\\CIAT_5\\hadcm3_b2a_2050\\bio1_n5.asc") 
b12_had<- raster("C:\\Laboratorio\\Datasets\\CIAT_5\\hadcm3_b2a_2050\\bio12_n5.asc")

amaz<-amazonia;     amaz[amaz==0]<-NA;
                
  d_cccma_1<- ((b1_cccma - b1_atual)/10)
  d_cccma_12<- b12_cccma - b12_atual             
  d_csiro_1<- ((b1_csiro - b1_atual)/10)
  d_csiro_12<- b12_csiro - b12_atual
  d_had_1<- ((b1_had - b1_atual)/10)
  d_had_12<- b12_had - b12_atual
                
  d_cccma_1m<- ((b1_cccma - b1_atual)/10)*amaz
  d_cccma_12m<- (b12_cccma - b12_atual)*amaz
  d_csiro_1m<- ((b1_csiro - b1_atual)/10)*amaz
  d_csiro_12m<- (b12_csiro - b12_atual)*amaz
  d_had_1m<- ((b1_had - b1_atual)/10)*amaz
  d_had_12m<- (b12_had - b12_atual)*amaz

  writeRaster(d_cccma_1, paste("C:\\IPAM\\Clima\\","d_cccma_1.asc",sep='') , format="ascii", overwrite=TRUE, NAflag=-9999);
  writeRaster(d_cccma_12, paste("C:\\IPAM\\Clima\\","d_cccma_12.asc",sep='') , format="ascii", overwrite=TRUE, NAflag=-9999);
  writeRaster(d_csiro_1, paste("C:\\IPAM\\Clima\\","d_csiro_1.asc",sep='') , format="ascii", overwrite=TRUE, NAflag=-9999);
  writeRaster(d_csiro_12, paste("C:\\IPAM\\Clima\\","d_csiro_12.asc",sep='') , format="ascii", overwrite=TRUE, NAflag=-9999);
  writeRaster(d_had_1, paste("C:\\IPAM\\Clima\\","d_had_1.asc",sep='') , format="ascii", overwrite=TRUE, NAflag=-9999);
  writeRaster(d_had_12, paste("C:\\IPAM\\Clima\\","d_had_12.asc",sep='') , format="ascii", overwrite=TRUE, NAflag=-9999);
  
writeRaster(d_cccma_1m, paste("C:\\IPAM\\Clima\\","d_cccma_1am.asc",sep='') , format="ascii", overwrite=TRUE, NAflag=-9999);
writeRaster(d_cccma_12m, paste("C:\\IPAM\\Clima\\","d_cccma_12am.asc",sep='') , format="ascii", overwrite=TRUE, NAflag=-9999);
writeRaster(d_csiro_1m, paste("C:\\IPAM\\Clima\\","d_csiro_1am.asc",sep='') , format="ascii", overwrite=TRUE, NAflag=-9999);
writeRaster(d_csiro_12m, paste("C:\\IPAM\\Clima\\","d_csiro_12am.asc",sep='') , format="ascii", overwrite=TRUE, NAflag=-9999);
writeRaster(d_had_1m, paste("C:\\IPAM\\Clima\\","d_had_1am.asc",sep='') , format="ascii", overwrite=TRUE, NAflag=-9999);
writeRaster(d_had_12m, paste("C:\\IPAM\\Clima\\","d_had_12am.asc",sep='') , format="ascii", overwrite=TRUE, NAflag=-9999);

writeRaster(x1, paste("C:\\IPAM\\Clima\\","mean_difbio1.asc",sep='') , format="ascii", overwrite=TRUE, NAflag=-9999);
                

 prec<- stack(d_cccma_12m, d_csiro_12m, d_had_12m)              
   x12<-calc(prec, mean) 
temp<- stack(d_cccma_1m, d_csiro_1m, d_had_1m)              
    x1<-calc(temp, mean) 
                
                
#################################
