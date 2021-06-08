RA_Stat <- function(data_ra = ARIDEZ_REG_sp){
  ARIDEZ_REG_sp2 <- mutate(count(data_ra, layer),perc = n / nrow(data_ra))
  breaks <- c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5)
  ARIDEZ_REG_sp2$clasd <- cut(ARIDEZ_REG_sp2$layer,
                              breaks = breaks,
                              right = FALSE,
                              labels=c("Hiper Hidrico", "Hidrico", "Hiper Humedo",'Humedo',"Subhumedo","Semiarido",'Arido','Hiper arido','Xerico'))
  
  RARIDEZ_STT <- ggplot(ARIDEZ_REG_sp2, aes(x = clasd, y = perc, fill = clasd)) + 
    geom_bar(stat = "identity")+
    geom_text(data=ARIDEZ_REG_sp2, aes(label=paste0(round(perc*100,1),"%"),
                                       y=perc+0.01), size=2.8)+
    labs(x = "Regimen de Aridez")+
    theme_bw()+
    theme(legend.position="none")
  return(RARIDEZ_STT)
}

IA_Stat <- function(data_ia = ARIDEZ_IND_sp){
  breaks <- c(0.5,1.5,2.5,3.5,4.5,5.5,6.5)
  data_ia$clasd <- cut(data_ia$layer,
                             breaks = breaks,
                             right = FALSE,
                             labels=c("Hiperarido",'Arido', "Semiarido", 'Subhumedo seco', "Subhumedo humedo",'Humedo'))
  
  ARIDEZ_IND_sp2 <- mutate(count(data_ia, clasd),perc = n / nrow(data_ia))
  
  IARIDEZ_STT <- ggplot(ARIDEZ_IND_sp2, aes(x = clasd, y = perc, fill = clasd)) + 
    geom_bar(stat = "identity")+
    geom_text(data=ARIDEZ_IND_sp2, aes(label=paste0(round(perc*100,1),"%"), y=perc+0.03), size=3.2)+
    labs(x = "Indice de Aridez")+
    theme_bw()+
    theme(legend.position="none")
  return(IARIDEZ_STT)
}