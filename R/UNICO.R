# ------------------------------------------------
# Programa Unico
# Practicas actuariales
# ------------------------------------------------
library(tidyverse)
library(gdata)

# Parametros

# tablas de Mortalidad  TablasExpMex
# Columna 2  EM67M      Experiencia Mexicana 67  Modificada
# Columna 3  EM89M      Experiencia Mexicana 89  Modificada
# Columna 4  CNSF2000I  Comision Nacional de Seguros y Fianzas 2000 Individual
# Columna 5  CNSF2000G  Comision Nacional de Seguros y Fianzas 2000 Grupo
# Columna 6  AMIS20005H Asociacion Mexicana de Instituciones de Seguros 2005 Hombres
# Columna 7  AMIS20005M Asociacion Mexicana de Instituciones de Seguros 2005 Mujeres


  # --------------------------------------------------------------------
  #
  #                            Funciones
  #
  # --------------------------------------------------------------------
  #
  #                            Prima de riesgo
  #
  TablasMx <- read.csv("~/GitHub/Practicas04/TablasExpMex.csv")
  PRiesgo<-function(Edad,Tabla,Tasa,PlazoSeg,PlazoPag=0,Dotal=0,Fsel=0,Persis=0){
      Vectasa<-numeric()
      Vecqx<-numeric()
      Veckpx<-numeric()
      Vecdot<-numeric()
      Vecsel<-numeric()
      Vecper<-numeric()


      TablaMx<-data.frame(Edad=TablasMx$Edad,qx=TablasMx[,Tabla])

      TablaMx<-TablaMx[-which(is.na(TablaMx$qx)),]

      if (max(TablaMx$qx)>1) {
        TablaMx$qx<-TablaMx$qx/1000
      }

      TablaMx$px<-1-TablaMx$qx

      FacIni<-first(TablaMx$Edad)-1  # Factor a descontar en la edad para obtener el primer componente
      EdadMax<-max(TablaMx$Edad)

      Tasa[which(Tasa>1)]<-Tasa[which(Tasa>1)]/100

      if (length(Tasa)==1) {
        Vectasa <- cumprod( c(1 , rep(1/(1+Tasa),PlazoSeg)))
      } else {
        Tasa<-c(Tasa,rep(last(Tasa),PlazoSeg-length(Tasa)))
        Vectasa <- cumprod( c(1 , 1/(1+Tasa)))
      }

      tasa<-max(Tasa)

      if (PlazoSeg==99){
        PlazoSeg<-EdadMax-Edad
      }

      if (PlazoPag==0){
        PlazoPag<-PlazoSeg
      }

      if (PlazoPag>PlazoSeg){
        PlazoPag<-PlazoSeg
      }

      Edadt <- Edad-FacIni

      Edads <- Edadt+PlazoSeg  # Edad a la que termina el plazo de seguro

      Edadp <- Edadt+PlazoPag  # Edad a la que termina el plazo de pago

      Vecdot<-rep(0,PlazoSeg)

      if (Dotal==1){
        Vecdot[PlazoSeg]<-1
      }
      # Factores de selección
      Vecsel<-rep(1,(Edads-Edadt))
      if (Fsel!=0){
        switch (Fsel,
                FacS<-c(.6,.85),
                FacS<-c(.75,.90),
                FacS<-c(.8,.95)
        )
        Vecsel[1:length(FacS)]<-FacS
      }

      # Factores de Persistencia
      Vecper<-rep(1,(Edads-Edadt))
      if (Persis!=0){
        switch (Persis,
                FacP<-c(1,.75,.85,0.90,0.92,0.94,0.96),
                FacP<-c(1,.70,.80,0.85,0.90,0.92,0.95),
                FacP<-c(1,.80,.88,0.92,0.94,0.96,0.98)
        )
        Vecper<-c(FacP,rep(last(FacP),(length(Vecper)-length(FacP))-1))
      }

      Vecqx<-TablaMx$qx[Edadt:(Edads-1)]*Vecsel*cumprod(Vecper)                        #

      Veckpxs<-cumprod(c(1,TablaMx$px[Edadt:(Edads-2)]))*cumprod(Vecper)

      Veckpxp<-cumprod(c(1,TablaMx$px[Edadt:(Edadp-2)]))*cumprod(Vecper)[1:(Edadp-Edadt)]

      PriesgoNumerador <- (Vecqx*Veckpxs+Vecdot*Veckpxs )%*% Vectasa[2:(1+length(Vecqx))]

      PriesgoDenominador <-(Veckpxp %*% Vectasa[1:(length(Veckpxp))])

      PrimaRiesgo <- (tasa/log(1+tasa))*(PriesgoNumerador/PriesgoDenominador)

      return(PrimaRiesgo)

  }

  PTarifa<-function(Edad,Tabla,Tasa,PlazoSeg,PlazoPag=0,Dotal=0,Fsel=0,Persis=0,
                    Gvar=.08,Gfij=1500,Comis=1,Sap=2000000){

    PRisk<-PRiesgo(Edad,Tabla,Tasa,PlazoSeg,PlazoPag,Dotal,Fsel,Persis)

    TablaMx<-data.frame(Edad=TablasMx$Edad,qx=TablasMx[,Tabla])

    TablaMx<-TablaMx[-which(is.na(TablaMx$qx)),]

    if (max(TablaMx$qx)>1) {
      TablaMx$qx<-TablaMx$qx/1000
    }

    TablaMx$px<-1-TablaMx$qx

    FacIni<-first(TablaMx$Edad)-1  # Factor a descontar en la edad para obtener el primer componente
    EdadMax<-max(TablaMx$Edad)

    Tasa[which(Tasa>1)]<-Tasa[which(Tasa>1)]/100

    if (length(Tasa)==1) {
      Vectasa <- cumprod( c(1 , rep(1/(1+Tasa),PlazoSeg+1)))
    } else {
      Tasa<-c(Tasa,rep(last(Tasa),PlazoSeg-length(Tasa)+1))
      Vectasa <- cumprod( c(1 , 1/(1+Tasa)))
    }

    if (PlazoSeg==99){
      PlazoSeg<-EdadMax-Edad
    }

    if (PlazoPag==0){
      PlazoPag<-PlazoSeg
    }

    if (PlazoPag>PlazoSeg){
      PlazoPag<-PlazoSeg
    }

    Edadt <- Edad-FacIni

    Edads <- Edadt+PlazoSeg  # Edad a la que termina el plazo de seguro

    Edadp <- Edadt+PlazoPag  # Edad a la que termina el plazo de pago

    Gadminfijo <- Gfij/(Sap/1000)   # Gastos de administración fijo

    Gadminvar <- Gvar         # Gastos de administración variables

    Veccom<-rep(1,(Edadp-Edadt+1))
    switch (Comis,
            FacC<-c(0.6,0.20,0.10,0.08,0.04),
            FacC<-c(0.4,0.15,0.08,0.04,0.02),
            FacC<-c(0.3,0.12,0,06,0.04,0.02)
      )
    Veccom<-c(FacC,rep(last(FacC),(length(Veccom)-length(FacC))))

    # Factores de Persistencia
    Vecper<-rep(1,(Edads-Edadt+1))
    if (Persis!=0){
      switch (Persis,
              FacP<-c(1,0.75,0.85,0.90,0.92,0.94,0.96),
              FacP<-c(1,0.70,0.80,0.85,0.90,0.92,0.95),
              FacP<-c(1,0.80,0.88,0.92,0.94,0.96,0.98)
      )
      Vecper<-c(FacP,rep(last(FacP),length(Vecper)-length(FacP)))
    }

    Veckpxp<-cumprod(c(1,TablaMx$px[Edadt:(Edadp-1)]))*cumprod(Vecper)[1:(Edadp-Edadt+1)]

    Gadq <- (Veccom * Veckpxp) %*%  Vectasa[1:(length(Veckpxp))]/(Veckpxp %*%  Vectasa[1:(length(Veckpxp))])

    PrimaTarifa <- (PRisk + Gadminfijo) / (1 - Gadminvar - Gadq)

    return(PrimaTarifa)

  }

  Reservas<-function(Edad,Tabla,Tasa,PlazoSeg,PlazoPag=0,Dotal=0,Fsel=0,Persis=0,PrimaRiesgo){

    Rva<-numeric()                  # Inicializa la variable Rva Reserva

    TablaMx<-data.frame(Edad=TablasMx$Edad,qx=TablasMx[,Tabla])

    TablaMx<-TablaMx[-which(is.na(TablaMx$qx)),]

    if (max(TablaMx$qx)>1) {
      TablaMx$qx<-TablaMx$qx/1000
    }

    TablaMx$px<-1-TablaMx$qx

    FacIni<-first(TablaMx$Edad)-1  # Factor a descontar en la edad para obtener el primer componente
    EdadMax<-max(TablaMx$Edad)

    Tasa[which(Tasa>1)]<-Tasa[which(Tasa>1)]/100

    if (length(Tasa)==1) {
      Vectasa <- cumprod( c(1 , rep(1/(1+Tasa),PlazoSeg+1)))
    } else {
      Tasa<-c(Tasa,rep(last(Tasa),PlazoSeg-length(Tasa)+1))
      Vectasa <- cumprod( c(1 , 1/(1+Tasa)))
    }

    if (PlazoSeg==99){
      PlazoSeg<-EdadMax-Edad
    }

     if (PlazoPag==0){
      PlazoPag<-PlazoSeg
    }

    if (PlazoPag>PlazoSeg){
      PlazoPag<-PlazoSeg
    }


    Vecdot<-rep(0,PlazoSeg)

    if (Dotal==1){
      Vecdot[PlazoSeg]<-1
    }

    Edadt <- Edad-FacIni

    Edads <- Edadt+PlazoSeg  # Edad a la que termina el plazo de seguro

    Edadp <- Edadt+PlazoPag

   for (t in 1:(PlazoSeg)){

     Vecqx<-TablaMx$qx[(Edadt+t):Edads]

     Vecdot<-rep(0,length(Vecqx))

     if (Dotal==1) Vecdot[length(Vecdot)]<-1

     if (t<PlazoSeg) {Veckpx<-cumprod(c(1,TablaMx$px[(Edadt+t):(Edads-1)]))} else Veckpx<-1

     PriesgoNumerador <- (Vecqx*Veckpx+Vecdot*Veckpx) %*% Vectasa[2:(1+length(Vecqx))]   # Equivale a A x+t

     PriesgoDenominador <-(Veckpx %*% Vectasa[1:(length(Vecqx))])        # Equivale a x+t

     Rva[t]<- PriesgoNumerador - PrimaRiesgo*PriesgoDenominador

   }

    return(Rva)

  }

  # -------------------------------------------------------
  #                Calculo estado de resultados
  # Para este ejercicio hay una serie de variables pre cargadas

  # TablaMx      Tabla de Mortalidad
  # tasa         Tasa de interes tecnico 4%
  # SAP          Suma Asegurada Promedio  2,000,000
  # VecPersis    Vector de factores persistencia acumulada
  # Vectasa      Vector de factores de descuento anuales
  # Gadminfijo   Tantos de gastos de administración fijo 1500/SAP
  # GadmVar      Factor de gastos de administración variables
  # Comis        Vectpr de porcentaje de comisiones
  # PersisAnual  Vector de porcentajes de persistencia anual

  # Edad         Edad del asegurado
  # Plazoseguro  Plazo que el seguro estara vigente
  # Plazopago    Plazo e pago primas
  # SumaAseg     Suma asegurada / capital en riesgo  3,000,000

CashFlow<-function(Veckpx,Vecqx,Vecper,tasa,Plazo,Prima,Veccom,Gadminfijo,Gadminvar,SumaAseg){

  Plazoseguro<-Plazo
  CargoRescate <-c(1,1,seq(1,0,-.04),rep(0,62))

  # Inicia ciclo for

  #     Inicializa parametros y vectores

  IngPrima<-EgrSiniestros<-EgrGadminfij<-EgrGadminvar<-vector()

  EgrGadq<-IngLiber<-EgrRescate<-ProdFin<-Saldofinal<-SaldoInicio<-vector()

  SaldoInicio<-Saldofinal<-rep(0,Plazoseguro)

  for (t in 1:Plazoseguro){

    IngPrima[t]<-Prima*Veckpx[t]*VecPersis[t]

    EgrSiniestros[t]<-VecPersis[t]*Veckpx[t]*Vecqx[t]*SumaAseg

    EgrGadminfij[t]<-Gadminfijo*VecPersis[t]*Veckpx[t]

    EgrGadminvar[t]<- IngPrima[t]* GadmVar

    EgrGadq[t]<-IngPrima[t]*Comis[t]

    if (t==1) IngLiber[t]<-0 else IngLiber[t]<-(VecPersis[t-1]*Veckpx[t-1]-
                                                  VecPersis[t]*Veckpx[t])*SaldoInicio[t]
    #IngLiber[t]<-0

    EgrRescate[t]<-IngLiber[t]* (1-CargoRescate[t])

    ProdFin[t]<-SaldoInicio[t]*tasa+(IngPrima[t] - EgrGadminfij[t] -
                                       EgrGadminvar[t] - EgrGadq[t]-EgrSiniestros[t])*tasa/2

    Saldofinal[t]<-SaldoInicio[t]+IngPrima[t] -  EgrSiniestros[t] -
      EgrGadminfij[t] - EgrGadminvar[t] - EgrGadq[t] +
      IngLiber[t] -EgrRescate[t]+ProdFin[t]

    if (t<Plazoseguro) SaldoInicio[t+1]<-Saldofinal[t]

  }

  AssetShare<-data.frame( SaldoI=SaldoInicio,
                          Prima=IngPrima,
                          Siniestros=EgrSiniestros,
                          GtoAdmF=EgrGadminfij,
                          GtoAdmV=EgrGadminvar,
                          GtoAdq=EgrGadq,
                          Liber=IngLiber,
                          Devol=EgrRescate,
                          Intereses=ProdFin,
                          SaldoF=Saldofinal
  )

  ValPre<-data.frame(Prima=Vectasa[1:Plazoseguro]%*%IngPrima,
                     Siniestros=Vectasa[1:Plazoseguro]%*%EgrSiniestros,
                     GtoAdmF=Vectasa[1:Plazoseguro]%*%EgrGadminfij,
                     GtoAdmV=Vectasa[1:Plazoseguro]%*%EgrGadminvar,
                     GtoAdq=Vectasa[1:Plazoseguro]%*%EgrGadq,
                     Liber=Vectasa[1:Plazoseguro]%*%IngLiber,
                     Devol=Vectasa[1:Plazoseguro]%*%EgrRescate,
                     Intereses=Vectasa[1:Plazoseguro]%*%ProdFin,
                     SaldoF=Vectasa[Plazoseguro]*Saldofinal[Plazoseguro]
  )

  return(list(AssetShare,ValPre))

}


  AnalisisTabla<-function(Tablas){
    # Recupera tabla de mortalidad

    TablasMx <- read.csv("~/PracticasActuariales/TablasExpMex.csv",header=TRUE)

    TablasMx<-mutate(TablasMx,grupo=cut(Edad,6))

    Titulos<-paste0("Edad ",levels(TablasMx$grupo))
    names(Titulos)<-levels(TablasMx$grupo)

    TablasMx %>% ggplot(aes(x=Edad))+
      geom_line(aes(y=EM67M,color='darkred'))+
      geom_line(aes(y=EM89M,color='orange'))+
      geom_line(aes(y=CNSF2000I,color='steelblue'))+
      ylab('qx')+
      scale_colour_manual(name = 'Tabla',
                          values = c('darkred'='darkred','orange'='orange','steelblue'='steelblue'),
                          labels = c('EM67','EM89','CNSF2000I')) +
      facet_wrap(~grupo,scales="free",labeller=labeller(grupo=Titulos))+
      labs(title="Comparativo de Tablas de mortalidad")

    df<-data.frame(Edad=TablasMx$Edad,
                   FACIH=(1000*TablasMx$AMIS2005H)/TablasMx$CNSF2000I,
                   FACIG=TablasMx$CNSF2000G/TablasMx$CNSF2000I,
                   grupo=TablasMx$grupo)
    df %>% ggplot(aes(x=Edad))+
      geom_line(aes(y=FACIH,color='steelblue')) +
      geom_line(aes(y=FACIG,color='purple')) +
      ylab('qx')+
      scale_colour_manual(name = 'Tabla',
                          values = c('steelblue'='steelblue','purple'='purple'),
                          labels = c('Factor H/I','Factor G/I')) +
      #       facet_wrap(~grupo,scales="free",labeller=labeller(grupo=Titulos))+
      labs(title="Factores de Comparación de tablas de mortalidad")

  }
