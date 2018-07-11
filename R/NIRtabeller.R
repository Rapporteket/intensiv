# Må det kanskje komme en overornet tittel her?
#---------------------------------------------

#' Funksjoner for å lage tabeller Group of functions page title
#' 
#' Fil som beregner div tabeller.Group of functions Description section
#' 
#' Detaljer. kommer senereGroup of functions Details paragraph.
#'
#' @section Finne reinnleggelser After function section:
#' Despite its location, this actually comes after the function section.
#' Fil som inneholder funksjoner for å lage tabeller, i første rekke tellinger av personer 
#' Aktuelle tabeller:
#' Belegg (samlerapport), antall opphold per år og enhet, ant. pasienter per år og enhet, ant opph. per måned og enhet.
#' 
#' @param RegData data
#' @param PasientID Variabelen som angir pasientidentifikasjon
# @inheritParams NIRFigAndeler
#' @return Div tabeller
#' @name NIRtabeller
NULL
#' @rdname NIRtabeller
#' @export

dato <- as.POSIXlt(Sys.Date(), tz='UTC')
datoFraMnd <- as.Date(paste0(1900+dato$year,'-', ifelse(dato$mon==0, 11, dato$mon), '-', '01')) #dato - 

   datoFra12mnd <- as.Date(paste0(as.numeric(substr(input$datoTil,1,4))-1, substr(input$datoTil,5,8), '01'), tz='UTC')
    Data12mnd <- RegData[RegData$InnDato < as.POSIXlt(input$datoTil, tz='UTC')
                                  & RegData$InnDato > as.POSIXlt(datoFra12, tz='UTC'), ]
AntOpphTidEnh <- function(RegData, tidsenhet='Aar',  ){
      #RegData må inneholde DateAdmittedIntensive, DateDischargedIntensive og PasientID
    tabAvd12mnd <- addmargins(table(RegData12mnd[, c('ShNavn', 'Mnd')]))
    colnames(tabAvd12mnd) <- substring(colnames(tabAvdSiste12mnd),1,3)
    xtable::xtable(tabAvdSiste12mnd)
  },
  rownames = TRUE, digits=0 #, align = c('l', rep('r', ncol(tabAvdSiste12mnd)))
  )
      
      return(RegDataSort)
}


  AarNaa <- as.numeric(format(Sys.Date(), "%Y"))
  # Nye variable:
  RegData$Mnd <- RegData$InnDato$mon +1
  RegData$Kvartal <- ceiling(RegData$Mnd/3)
  RegData$Halvaar <- ceiling(RegData$Mnd/6)
  aarFra <- paste0(1900+as.POSIXlt(Sys.Date())$year-5, '-01-01')
  reshIDdummy <- 601161





  #Velge ferdigstillelse og tidsintervall.
  output$tabAvdSkjema12 <- renderTable({
    SkjemaDataFerdig <- SkjemaData[SkjemaData$SkjemaStatus ==1, ]
    #Flyttes til overvåkning
    datoFra12 <- as.Date(paste0(as.numeric(substr(input$datoTil,1,4))-1, substr(input$datoTil,5,8), '01'))

    #datoFra12 <- '2017-03-01'
    #SkjemaData12mnd <- SkjemaDataFerdig[SkjemaDataFerdig$InnDato < as.POSIXlt('2018-04-30')
    SkjemaData12mnd <- SkjemaDataFerdig[SkjemaDataFerdig$InnDato < as.POSIXlt(input$datoTil)
                                        & SkjemaDataFerdig$InnDato > as.POSIXlt(datoFra12), ]
    LegeSkjema <- table(SkjemaData12mnd[SkjemaData12mnd$SkjemaRekkeflg==2, 'Sykehusnavn'])
    PasientSkjema <- table(SkjemaData12mnd[SkjemaData12mnd$SkjemaRekkeflg==1, 'Sykehusnavn'])
    Oppf3mnd <- table(SkjemaData12mnd[SkjemaData12mnd$SkjemaRekkeflg==3, 'Sykehusnavn'])
    Oppf12mnd <- table(SkjemaData12mnd[SkjemaData12mnd$SkjemaRekkeflg==4, 'Sykehusnavn'])

    tabAvd12MndNskjemaDum <- cbind(
      Lege = LegeSkjema,
      Pasient = PasientSkjema,
      'Oppf3mnd' = Oppf3mnd,
      'Oppf12mnd' = Oppf12mnd)

    tabAvd12MndNskjemaDum <- addmargins(tabAvd12MndNskjemaDum, margin=1)

    tabAvd12MndNskjema <- cbind(
      tabAvd12MndNskjemaDum[ ,1:2],
      'Pasient (%)' =  sprintf('%1.1f', tabAvd12MndNskjemaDum[,'Pasient']/tabAvd12MndNskjemaDum[,'Lege']*100, 1),
      'Oppfølging 3 mnd.' = tabAvd12MndNskjemaDum[ ,3],
      'Oppfølging 3 mnd. (%)' = sprintf('%1.1f', tabAvd12MndNskjemaDum[,'Oppf3mnd']/tabAvd12MndNskjemaDum[,'Lege']*100, '%'),
      'Oppfølging 12 mnd.' = tabAvd12MndNskjemaDum[ ,4],
      'Oppfølging 12 mnd. (%)' =  sprintf('%1.1f', tabAvd12MndNskjemaDum[,'Oppf12mnd']/tabAvd12MndNskjemaDum[,'Lege']*100, 1)
    )
    #sprintf('%1.3f'
    xtable::xtable(tabAvd12MndNskjema,  align = c('l', rep('r', ncol(tabAvd12MndNskjema))),
                   caption= paste0('Tidsperiode: ', as.POSIXlt(datoFra12), 'til', as.POSIXlt(input$datoTil)))
  },
  rownames = T, align= 'r' #
  ) #digits=1,


  output$tabAvdNAar5 <- renderTable({

    tabAvdAarN <- addmargins(table(RegData[which(RegData$Aar %in% (AarNaa-4):AarNaa), c('ShNavn','Aar')]))
    rownames(tabAvdAarN)[dim(tabAvdAarN)[1] ]<- 'TOTALT, alle avdelinger:'
    colnames(tabAvdAarN)[dim(tabAvdAarN)[2] ]<- 'Siste 5 år'
    xtable::xtable(tabAvdAarN)
    #xtable::xtable(tabAvdAarN)
  },
  rownames = T, digits=0)

#output$tekstDash <- c('Figurer med kvalitetsindikatorer',
#                      'hente ned månedsrapport'),
  output$mndRapp.pdf = downloadHandler(
     filename = 'MndRapp.pdf',
    #content = function(file) file.copy(system.file('NakkeMndRapp.pdf', package = 'Nakke'), file, overwrite = TRUE),
    content = function(file) {
      # permission to the current working directory
      src <- normalizePath(system.file('NakkeMndRapp.Rnw', package='Nakke'))
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'NakkeMndRapp.Rnw', overwrite = TRUE)

        texfil <- knitr::knit(system.file('NakkeMndRapp.Rnw', package='Nakke'), encoding = 'UTF-8')
        texi2pdf(system.file(texfil, package='Nakke'),clean = TRUE) #"NakkeMndRapp.tex"
      # #help(render_latex)
#       out = system.file('NakkeMndRapp.pdf', package = 'Nakke')
         #knit2pdf(system.file('NakkeMndRapp.Rnw', package='Nakke'), clean = TRUE, encoding = 'UTF-8')
#      file.rename(out, file) # move pdf to file for downloading
        #file.copy(system.file('NakkeMndRapp.pdf', package='Nakke'), file)
        file.copy('NakkeMndRapp.pdf', file)

    },
    contentType = 'application/pdf'
  )
#  If you already have made the PDF file, you can just copy it to file, i.e.
#  content = function(file) file.copy('your_existing.pdf', file, overwrite = TRUE)

  output$kvalIndFig1 <- renderPlot({

    NakkeFigAndelTid(RegData=RegData, preprosess=0, reshID = reshIDdummy,
                   valgtVar=input$valgtVarKvalInd, datoFra = input$datoFraKvalInd,
                   myelopati = as.numeric(input$myelopatiKvalInd),
                   fremBak = as.numeric(input$fremBakKvalInd),
                   enhetsUtvalg = as.numeric(input$enhetsUtvalgKvalInd), tidsenhet = input$tidsenhetKvalInd)
 } )

  output$kvalIndFig2 <- renderPlot(
    NakkeFigAndelerGrVar(RegData=RegData, preprosess=0,
                         valgtVar=input$valgtVarKvalInd, datoFra = input$datoFraKvalInd,
                         myelopati = as.numeric(input$myelopatiKvalInd),
                         fremBak = as.numeric(input$fremBakKvalInd))
  )


  output$fordelinger <- renderPlot({

    NakkeFigAndeler(RegData=RegData, preprosess = 0, valgtVar=input$valgtVar,
                    reshID=reshIDdummy, enhetsUtvalg=as.numeric(input$enhetsUtvalg),
                    datoFra=input$datovalg[1], datoTil=input$datovalg[2],
                    minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]),
                    erMann=as.numeric(input$erMann), myelopati = as.numeric(input$myelopati),
                    fremBak = as.numeric(input$fremBak))
  })


  output$andelerGrVar <- renderPlot({

    NakkeFigAndelerGrVar(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarAndelGrVar,
                    reshID=reshIDdummy,
                    datoFra=input$datovalgAndelGrVar[1], datoTil=input$datovalgAndelGrVar[2],
                    minald=as.numeric(input$alderAndelGrVar[1]), maxald=as.numeric(input$alderAndelGrVar[2]),
                    erMann=as.numeric(input$erMannAndelGrVar), myelopati = as.numeric(input$myelopatiAndelGrVar),
                    fremBak = as.numeric(input$fremBakAndelGrVar))
  })

  output$andelTid <- renderPlot({

    NakkeFigAndelTid(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarAndelGrVar,
                         reshID=reshIDdummy,
                         datoFra=input$datovalgAndelGrVar[1], datoTil=input$datovalgAndelGrVar[2],
                         minald=as.numeric(input$alderAndelGrVar[1]), maxald=as.numeric(input$alderAndelGrVar[2]),
                         erMann=as.numeric(input$erMannAndelGrVar),
                         myelopati = as.numeric(input$myelopatiAndelGrVar),
                         fremBak = as.numeric(input$fremBakAndelGrVar),
                     tidsenhet = input$tidsenhetAndelTid,
                     enhetsUtvalg = input$enhetsUtvalgAndelTid)
  })

  output$gjsnGrVar <- renderPlot({
    NakkeFigGjsnGrVar(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarGjsn,
                         reshID=reshIDdummy,
                         datoFra=input$datovalgGjsn[1], datoTil=input$datovalgGjsn[2],
                         minald=as.numeric(input$alderGjsn[1]), maxald=as.numeric(input$alderGjsn[2]),
                         erMann=as.numeric(input$erMannGjsn), myelopati = as.numeric(input$myelopatiGjsn),
                         fremBak = as.numeric(input$fremBakGjsn),
                         valgtMaal = input$sentralmaal)
  })

  output$gjsnTid <- renderPlot({
    NakkeFigGjsnTid(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarGjsn,
                      reshID=reshIDdummy,
                      datoFra=input$datovalgGjsn[1], datoTil=input$datovalgGjsn[2],
                      minald=as.numeric(input$alderGjsn[1]), maxald=as.numeric(input$alderGjsn[2]),
                      erMann=as.numeric(input$erMannGjsn), myelopati = as.numeric(input$myelopatiGjsn),
                      fremBak = as.numeric(input$fremBakGjsn),
                    valgtMaal = input$sentralmaal,
                    tidsenhet = input$tidsenhetGjsn,
                    enhetsUtvalg = input$enhetsUtvalgGjsn)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

