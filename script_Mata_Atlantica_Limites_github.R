###############################################
# 2017 PC8 
# Renata Muylaert
# Limites da Mata Atlântica
# Comparação dos Limites da MA
# limite_ma_lei_mata_atlantica_albers_sad69
# limite_ma_miltinho_albers_sad69
# limite_ma_mma_albers_sad69
# limite_ma_wwf_200ecprregions_albers_sad69
###############################################

# diretorio
# setwd('D:/__________mestrado/_dissertacao/02_variaveis_ambientais/__limite_ma')
# setwd("D://Bases//Mata_Atlantica_albers_sad69//limites_af//")
setwd("")
# install and require packages

ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, 'Package'])]
    if (length(new.pkg)) 
        install.packages(new.pkg, dep = T)
    sapply(pkg, require, character.only = T)}

packages <- c('vegan', 'ggplot2', 'raster', 'plyr', 'reshape2', 'RColorBrewer', 
	      'scales', 'grid', 'maptools', 'dismo', 'bbmle', 'foreign', 'rgdal')

ipak(packages)

# clean and memory size and vanish with scinot
rm(list = ls())
gc() 
memory.limit(size = 100000000) 
options(digits=7, scipen=999)

###=========================================================================================###

# Abrindo os shapes

lei2006 <- readOGR('.', 'limite_ma_lei_mata_atlantica_albers_sad69')
miltinho2009 <- readOGR('.', 'limite_ma_miltinho_albers_sad69')
mma2004 <- readOGR('.', 'limite_ma_mma_albers_sad69')
wwf2001 <- readOGR('.', 'limite_ma_wwf_200ecprregions_albers_sad69')
#WWF só no Brasil (pra poder comparar direito)
wwf2001BRA <- shapefile("limite_ma_wwf_BRASIL_200ecprregions_albers_sad69.shp")

par(mfrow = c(2, 3))

plot(lei2006)
plot(miltinho2009)
plot(mma2004)
plot(wwf2001, col= "green")
plot(wwf2001BRA, col="red", add=TRUE)

# Qual extent é mais abrangente?

extent(lei2006)
extent(miltinho2009)
extent(mma2004)
extent(wwf2001)

# Nenhum, então melhor fazer um extent abrangente:
# xmin    : 207854.7 
# xmax    : 3071511
# ymin    : -70735.41
# ymax    : 3207565

# Extensão que pega todos
# extent(c(207854.7, 3071511, -70735.41, 3207565))
# Adding values to each shape
wwf2001@data$Value <- c(1)
wwf2001BRA@data$Value <- c(1)
mma2004@data$Value <- c(10)
lei2006@data$Value <- c(100)
miltinho2009@data$Value <- c(1000)


###=========================================================================================###

setwd("")
# Criando rasters, se necessário. Caso já tenha criado (dropbox), vá para o próximo passo


# wwf2001BRAr

r <- raster()
r <- raster(extent(c(207854.7, 3071511, -70735.41, 3207565)), res = 1000, crs = crs(wwf2001))
wwf2001BRAr <- rasterize(wwf2001BRA, r, field = 'Value')
wwf2001BRAr[is.na(wwf2001BRAr)] <- 0

# wwf2001r

r <- raster()
r <- raster(extent(c(207854.7, 3071511, -70735.41, 3207565)), res = 1000, crs = crs(wwf2001))
wwf2001r <- rasterize(wwf2001, r, field = 'Value')
wwf2001r[is.na(wwf2001r)] <- 0
plot(wwf2001r)

# mma2004, tanto faz a projeção base, pq todas estão iguais

r <- raster()
r <- raster(extent(c(207854.7, 3071511, -70735.41, 3207565)), res = 1000, crs = crs(mma2004))
mma2004r <- rasterize(mma2004, r, field = 'Value')
mma2004r[is.na(mma2004r)] <- 0
plot(mma2004r)

# miltinho2009

r <- raster()
r <- raster(extent(c(207854.7, 3071511, -70735.41, 3207565)), res = 1000, crs = crs(mma2004))
miltinho2009r <- rasterize(miltinho2009, r, field = 'Value')
miltinho2009r[is.na(miltinho2009r)] <- 0
plot(miltinho2009r)

# lei2006

r <- raster()
r <- raster(extent(c(207854.7, 3071511, -70735.41, 3207565)), res = 1000, crs = crs(mma2004))
lei2006r <- rasterize(lei2006, r, field = 'Value')
lei2006r[is.na(lei2006r)] <- 0
plot(lei2006r)

# Plotando os rasters:

par(mfrow = c(2, 3))

plot(lei2006r)
plot(miltinho2009r)
plot(mma2004r)
plot(wwf2001r)

###========================================================================================###

# exportando rasters
# criando pasta para histogramas
dir.create('rasters')
# directory
#setwd('./rasters')
#getwd()
#writeRaster(Sumr, 'MA_Limites_Soma_BRA_1000', format = 'GTiff')
#writeRaster(Sumr_int, 'MA_Limites_Soma_INTERNACIONAL_1000', format = 'GTiff')
#writeRaster(wwf2001BRAr, 'wwf2001BRAr_1000', format = 'GTiff')
#writeRaster(wwf2001r, 'wwf2001r_1000', format = 'GTiff')
#writeRaster(mma2004r, 'mma2004r_1000', format = 'GTiff')
#writeRaster(lei2006r, 'lei2006r_1000', format = 'GTiff')
#writeRaster(miltinho2009r, 'miltinho2009r_1000', format = 'GTiff')

###=========================================================================================###

# Importando os rasters gerados
setwd("")
#wwf2001r<-raster('wwf2001r_1000.tif')
#wwf2001BRAr<-raster('wwf2001BRAr_1000.tif')
#mma2004r<-raster('mma2004r_1000.tif')
#lei2006r<-raster('lei2006r_1000.tif')
#miltinho2009r<-raster('miltinho2009r_1000.tif')

###========================================================================================###

# LIMITES  INTERNATIONAL: áre bruta dos limites

Sumr_int <- raster(extent(c(207854.7, 3071511, -70735.41, 3207565)), res = 1000, crs = crs(mma2004r))

Sumr_int <- lei2006r + miltinho2009r + mma2004r + wwf2001r

par(mfrow = c(1, 2))
plot(Sumr_int)
hist(Sumr_int)

###========================================================================================###

# Gerando cortes dos limites para a malha municipal brasileira

brasil <- shapefile("brasil_malha_2016_albers_sad69.shp")

# Comparando por curiosidade o shape oficial do Brasil (IBGE 2016) com o do maptools

data(wrld_simpl)
brazil_maptools<- subset(wrld_simpl, NAME=="Brazil")
par(mfrow=c(1,2))
plot(brazil_maptools)
plot(brasil)

# Area do Brasil Oficial

brasil$area_sqkm <- area(brasil) / 1000000

# Area do Brasil pelo maptools

brazil_maptools$area_sqkm <- area(brazil_maptools) / 1000000

8502740-8449313

# Clipando os rasters pelo Brasil Oficial
# Clipando os rasters pelo Brasil Oficial
# Clipando os rasters pelo Brasil Oficial
# Clipando os rasters pelo Brasil Oficial

list <- c("wwf2001r", "mma2004r", "lei2006r", "miltinho2009r")

bra_wwf2001r <- mask(wwf2001r, brasil) #1
bra_mma2004r <- mask(mma2004r, brasil) #10
bra_lei2006r <- mask(lei2006r, brasil) #100
bra_miltinho2009r <- mask(miltinho2009r, brasil) #1000


# Juntando os rasters considerando o limite político BRASILEIRO

Sumr <- raster(extent(c(207854.7, 3071511, -70735.41, 3207565)), res = 1000, crs = crs(mma2004r))

Sumr <- bra_lei2006r + bra_miltinho2009r + bra_mma2004r + bra_wwf2001r

###########################################################
# Codes
#1	Exclusivo do WWF
#10	Exclusivo do MMA
#100	Exclusivo da Lei da Mata Atlântica
#1000	Exclusivo do mapa de Ribeiro et al. 2009
#11	WWF e MMA
#101	WWF e Lei da Mata Atlântica
#110	MMA e Lei da Mata Atlântica
#1001	WWF e Ribeiro et al. 2009
#1010	MMA e Ribeiro et al. 2009
#1100	Lei da Mata Atlântica e Ribeiro et al. 2009
#111	WWF, MMA e Lei da Mata Atlântica
#1011	WWF, MMA e Ribeiro et al. 2009
#1101	WWF, Lei da Mata Atlântica e Ribeiro et al. 2009
#1110	MMA, Lei da Mata Atlântica e Ribeiro et al. 2009
#1111	WWF, MMA, Lei da Mata Atlântica e Ribeiro et al. 2009
###########################################################


# barplot limites cortados pelo brasil

f <- values(Sumr)
f <- f[f > 0]
table(f)
f.df<-data.frame(table(f))
str(f.df)

f.df$Valores<-ifelse(f.df$f=="1", "Exclusivo do WWF",
	ifelse(f.df$f=="10", "Exclusivo do MMA",
	ifelse(f.df$f=="100", "Exclusivo da Lei da Mata Atlântica",
	ifelse(f.df$f=="1000","Exclusivo do mapa de Ribeiro et al. 2009",
		ifelse(f.df$f=="11","WWF e MMA",
		ifelse(f.df$f=="101","WWF e Lei da Mata Atlântica",
		ifelse(f.df$f=="110", "MMA e lei da Mata Atlântica",
		ifelse(f.df$f=="1001", "WWF e Ribeiro et al. 2009",
		ifelse(f.df$f=="1010", "MMA e Ribeiro et al. 2009",
		ifelse(f.df$f=="1100", "Lei da Mata Atlântica e Ribeiro et al. 2009",
		ifelse(f.df$f=="111", "WWF, MMA e Lei da Mata Atlântica",
		ifelse(f.df$f=="1011", "WWF, MMA e Ribeiro et al. 2009",
		ifelse(f.df$f=="1101", "WWF, Lei da Mata Atlântica e Ribeiro et al. 2009",
		ifelse(f.df$f=="1110", "MMA, Lei da Mata Atlântica e Ribeiro et al. 2009",
		ifelse(f.df$f=="1111", "WWF, MMA, Lei da Mata Atlântica e Ribeiro et al. 2009",NA)
		))))))))))))))

# Encurtando lables

f.df$Valores_curtos<-ifelse(f.df$f=="1", "Exclusivo do WWF",
	ifelse(f.df$f=="10", "Exclusivo do MMA",
	ifelse(f.df$f=="100", "Exclusivo da Lei",
	ifelse(f.df$f=="1000","Exclusivo de Ribeiro et al. 2009",
		ifelse(f.df$f=="11","WWF e MMA",
		ifelse(f.df$f=="101","WWF e Lei",
		ifelse(f.df$f=="110", "MMA e Lei",
		ifelse(f.df$f=="1001", "WWF e Ribeiro et al. 2009",
		ifelse(f.df$f=="1010", "MMA e Ribeiro et al. 2009",
		ifelse(f.df$f=="1100", "Lei e Ribeiro et al. 2009",
		ifelse(f.df$f=="111", "WWF, MMA e Lei da Mata Atlântica",
		ifelse(f.df$f=="1011", "WWF, MMA e Ribeiro et al. 2009",
		ifelse(f.df$f=="1101", "WWF, Lei e Ribeiro et al. 2009",
		ifelse(f.df$f=="1110", "MMA, Lei e Ribeiro et al. 2009",
		ifelse(f.df$f=="1111", "WWF, MMA, Lei e Ribeiro et al. 2009",NA)
		))))))))))))))

write.table(f.df, 'table_MA_POLITICO_BRASIL_2016_1km.txt', quote = F, sep = '\t', row.names = T, col.names = T)
head(f.df)

attach(f.df)
f.df <- f.df[order(Freq),] 

# Histograma para dentro do Brasil

getwd()
tiff('Fig_pixels_MA_Brasil.tif', width=23, height=16, unit="cm", res=300)
par(mar=c(10,10,5,5))
barplot(height = rev(f.df$Freq), names.arg = rev(f.df$Valores_curtos), horiz = T, cex.axis = 0.9, 
	 cex.lab = 0.7, cex = 0.5, las = 1, xlab = expression("Área"~ (km^2)), 
	#col = c('black', rep('plum', 4), rep('light blue', 6), rep('darkolivegreen2', 4)),
	 main = "", cex.main = 1.0
	, xlim=c(0, max(f.df$Freq)))

dev.off()


#Margens melhores
#tiff('pixels_codigo.tif', width=15, height=12, unit="cm", res=300)
#barplot(height = rev(f.df$Freq), names.arg = rev(f.df$f), horiz = T, cex.axis = 0.9, #
#	 cex.lab = 1, cex = 1, las = 1, xlab = 'Área (km2)', col = c('tomato', #
#	 rep('plum', 4), rep('light blue', 6), rep('darkolivegreen2', 4)),
##
#	, xlim=c(0, max(f.df$Freq)))
#dev.off()

###========================================================================================###

### COM a área total dos limites

f <- values(Sumr_int)
f <- f[f > 0]
table(f)

# Atualizar valores

f.df <- data.frame(n = c(148472, 1788, 25197, 148268, 
				 1331, 35839, 21758, 16318, 3177, 27147,
				 11994, 8250, 93663, 51258, 
				 1018241), 
			 f = factor(c('1', '10', '100', '1000',
	 			        '11', '101', '110', '1001', '1010', '1100',
					  '111', '1011', '1101', '1110',
					  '1111')))

f.df
str(f.df)
f.df$Novo<-ifelse(f.df$f=="1", "Exclusivo do WWF",
	ifelse(f.df$f=="10", "Exclusivo do MMA",
	ifelse(f.df$f=="100", "Exclusivo da Lei",
	ifelse(f.df$f=="1000","Exclusivo de Ribeiro et al. 2009",
		ifelse(f.df$f=="11","WWF e MMA",
		ifelse(f.df$f=="101","WWF e Lei",
		ifelse(f.df$f=="110", "MMA e Lei",
		ifelse(f.df$f=="1001", "WWF e Ribeiro et al. 2009",
		ifelse(f.df$f=="1010", "MMA e Ribeiro et al. 2009",
		ifelse(f.df$f=="1100", "Lei e Ribeiro et al. 2009",
		ifelse(f.df$f=="111", "WWF, MMA e Lei da Mata Atlântica",
		ifelse(f.df$f=="1011", "WWF, MMA e Ribeiro et al. 2009",
		ifelse(f.df$f=="1101", "WWF, Lei e Ribeiro et al. 2009",
		ifelse(f.df$f=="1110", "MMA, Lei e Ribeiro et al. 2009",
		ifelse(f.df$f=="1111", "WWF, MMA, Lei e Ribeiro et al. 2009",NA)
		))))))))))))))


f.df$Percentagem_consensual <- 100*(f.df$n / 1620000)
f.df$Percentagem_consensual_round <- round(100*(f.df$n / 1620000))
sum(f.df$Percentagem_consensual )
sum(f.df$Percentagem_consensual_round )


# write.table(f.df, 'Histograma_pixels_MA_internacional.txt', quote = F, sep = '\t', row.names = T, col.names = T)
attach(f.df)
f.df <- f.df[order(n),] 
str(f.df)

#tiff('Fig_pixels_MA_internacional_v02.tif', width=23, height=16, unit="cm", res=300)

par(mar=c(10,10,5,5))
xx<-barplot(height = rev(f.df$n), names.arg = rev(f.df$Novo), horiz = T, cex.axis = 0.9, 
	 cex.lab = 0.7, cex = 0.5, las = 1, xlab = expression("Área"~ (km^2)), 
	#col = c('black', rep('plum', 4), rep('light blue', 6), rep('darkolivegreen2', 4)),
	 main = "", cex.main = 1.0
	, xlim=c(0, max(f.df$n)))

#
text(x = xx, y = rev(f.df$Percentagem_consensual),
label = rev( f.df$Percentagem_consensual),
	pos = 4, cex = 0.8, col = "black")

dev.off()

###Teste
xx <- barplot(rev(f.df$n), xaxt = 'n', xlab = '', width = 0.85, 
              main = "", 
              ylab = expression("Área"~ (km^2)), )
## Add text at top of bars	
text(x = xx, y = rev(f.df$Percentagem_consensual), label = rev(f.df$Percentagem_consensual_round), pos = 3, cex = 0.8,
 col = "black")
## Add x-axis labels 
axis(1, at=xx, labels= rev(f.df$Novo), tick=FALSE, las=2, line=-0.5, cex.axis=0.5)

#
tiff('ggplot_Fig_pixels_MA_internacional_v08.tif', width=25, height=13, unit="cm", res=300)


p<-ggplot(data=f.df, aes(x=reorder(Novo, -n), y=n)) +
	geom_bar(stat="identity")+
	coord_flip()+
 xlab("") +
 ylab( expression("Área"~ (km^2)))+
 scale_fill_manual(values=c("grey60"))+
  theme_bw()+
geom_text(aes(label=round(f.df$Percentagem_consensual,2)), vjust=0.1, hjust=-0.1, color="black", size=3.0)
+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+
 theme(plot.margin = unit(c(0.8,0.8,0.8,1),"cm"))

print(p)

dev.off()

sum(round(f.df$Percentagem_consensual,2))

###========================================================================================###


