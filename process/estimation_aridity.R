#=========================================================================#
#=============        ESTIMATION REGIMEN AND ARIDITY INDEX    ============#
#=============               ATLAS ARIDITY IN PERU            ============#
#=========================================================================#
# Author:       Ing. Gutierrez Lope Leonardo Flavio
# Contact:      lgutierrezlf@gmail.com
# Institution:  SENAMHI-Peru
#=========================================================================#

## Import packages
library(lubridate)
library(raster)
library(ggplot2)
source('src/fnc_arid.R')

## E_01: Parameters aridity --------------------------------------------
PERU_DEP <- shapefile("data/raw/spatial/PERU_BL.shp")
list_pcp <- list.files('data/raw/PPT/', full.names = T)
list_etp <- list.files('data/raw/ETP/', full.names = T)

dh_mes <- list()
eh_mes <- list()
pcp_mes <- list()
etp_mes <- list()
for (i in 1:12) {
  pcp  <- raster(list_pcp[i])
  pcp <- mask(crop(pcp,PERU_DEP),PERU_DEP)
  pcp[pcp == Inf] <- NA
  etp  <- raster(list_etp[i])
  etp <- mask(crop(etp,PERU_DEP),PERU_DEP)
  etp[etp == Inf] <- NA
  pcp_mes[[i]] <- pcp
  etp_mes[[i]] <- etp
  dh_mes[[i]] <- etp-pcp
  eh_mes[[i]] <-pcp-etp
}
dh_mes  <- do.call('stack',dh_mes)
eh_mes  <- do.call('stack',eh_mes)
pcp_mes <- do.call('stack',pcp_mes)
etp_mes <- do.call('stack',etp_mes)

# Precipitation,Evapotranspiration, Water deficit and Water excess yearly
PPA   <- sum(pcp_mes)
PETA  <- sum(etp_mes)
DHA   <- sum(dh_mes)
EHA   <- sum(eh_mes)
# Aridity index (0.05)
IA5   <- reclassify(PPA/PETA, matrix(c(-Inf, 0.0499, 100, 0.0499, Inf, 0.001),ncol = 3,byrow = TRUE))
# 2500 < P < 2500
PPAcl <- reclassify(PPA, matrix(c(-Inf, 2499.99, 0.5, 2499.99, Inf, 0.25),ncol = 3,byrow = TRUE))
# Months dry
MSC   <- pcp_mes/etp_mes
nm_sc <- reclassify(MSC, recls <- matrix(c(-Inf, 0.499, 1,0.499, Inf, 0),ncol = 3,byrow = TRUE))
ASECO <- sum(nm_sc)

## E_02: Aridity regimen --------------------------------------------
CLASAR <- PPAcl+IA5+ASECO
reclsf <- matrix(c(0, 0.3, 1,      #Hiper Hidrico
                   0.3, 0.99, 2,   #Hidrico
                   0.99, 2.99, 3,  #Hiper Humedo 
                   2.99, 4.99, 4,  #Humedo
                   4.99, 6.99, 5,  #Subhumedo
                   6.99, 8.99, 6,  #Semiarido
                   8.99, 10.99, 7, #Arido
                   10.99, 100, 8,  #Hiper arido
                   100, Inf, 9),   #Xerico
                 ncol = 3,byrow = TRUE)

ARIDEZ_REG <- reclassify(CLASAR, reclsf)
writeRaster(ARIDEZ_REG, "data/processed/RA_PRS.tif","GTiff",overwrite=TRUE)

# GRAFICO RA
ARIDEZ_REG_sp   <- as.data.frame(ARIDEZ_REG, xy = TRUE)
ARIDEZ_REG_sp   <- na.omit(ARIDEZ_REG_sp)

RA_STT <- RA_Stat(data_ra = ARIDEZ_REG_sp)
ggsave(plot=RA_STT, "figures/RA_STT.png", units = "mm", width = 100, height = 45, dpi = 600)

# E_03: Aridity index --------------------------------------------
recl_ia <- matrix(c(-Inf, 0.05, 1,  #Hiper-arido
                    0.05, 0.20, 2,  #Arido
                    0.20, 0.50, 3,  #Semi-arido
                    0.50, 0.65, 4,  #Sub-humedo seco
                    0.65, 1, 5,     #subhumedo humedo
                    1, Inf, 6),     #Humedo
                  ncol = 3,byrow = TRUE)

ARIDEZ_IND <- reclassify(PPA/PETA, recl_ia)
writeRaster(ARIDEZ_IND, "data/processed/IA_PRS.tif","GTiff",overwrite=TRUE)

# GRAFICO IA
ARIDEZ_IND_sp   <- as.data.frame(ARIDEZ_IND, xy = TRUE)
ARIDEZ_IND_sp   <- na.omit(ARIDEZ_IND_sp)

IA_STT <- IA_Stat(data_ia = ARIDEZ_IND_sp)
ggsave(plot=IA_STT, "figures/IA_STT.png", units = "mm", width = 100, height = 50, dpi = 600) 
