#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(knitr)
#ibrary(shinyBS) # Additional Bootstrap Controls

# ui <- shinyUI(basicPage(
#   downloadButton('report')
# ))
#
# server <- function(input, output) {
#   output$report = downloadHandler(
#     filename = 'MndRapp.pdf',
#     content = function(file) {
#       out = knit2pdf('C:/ResultattjenesteGIT/Nakke/inst/NakkeMndRapp.Rnw', encoding = 'UTF-8', clean = TRUE)
#       file.rename(out, file) # move pdf to file for downloading
#     },
#     contentType = 'application/pdf'
#   )
#
# }


# Define UI for application that draws figures
ui <- fluidPage( #"Hoved"Layout for alt som vises på skjermen

  # Application title
  titlePanel("Testing testing, Nakke"),

  # Velge sykehus og vise antall
  #sidebarLayout( #Definerer overordnet layout med en sidekolonne og ett hovedpanel.
    #sidebarPanel( #Området som viser "valgbokser"
	fluidRow(column(width = 3, #Første kolonne. Alternativ til sidebarLayout(sidebarPanel())
      conditionalPanel( #Ønsker ulike valgmuligheter for ulike faner/ark
        'input.ark == "Tabeller"',
        dateInput(inputId = 'datoTil', value = Sys.Date(), min = '2012-01-01',
                  label = "Velg sluttdato", language="nb"),
        #helpText("Denne endrer bare tabellen med 12-månedersoversikt"),
        selectInput(inputId = "status", label="Ferdigstilt/kladd (legeskjema), kun tab. med månedsoversikt:",
                    choices = c("Ikke valgt"=2, "Ferdigstilt"=1, "Kladd"=0))
      ),

      conditionalPanel(
        'input.ark == "Viktigste resultater"',
        selectInput(inputId = "valgtVarKvalInd", label="Velg variabel",
                    choices = c('Komplikasjon, stemme' = 'KomplStemme3mnd',
                                'Komplikasjon, svelging' = 'KomplSvelging3mnd')),
        dateInput(inputId = "datoFraKvalInd", label='Velg startdato', value = "2017-01-01"),
        selectInput(inputId = "tidsenhetKvalInd", label="Velg tidsenhet",
                    choices = rev(c('År'= 'Aar', 'Halvår' = 'Halvaar',
                                'Kvartal'='Kvartal', 'Måned'='Mnd'))),
        selectInput(inputId = "myelopatiKvalInd", label="Myelopati",
                    choices = c("Ikke valgt"=2, "Ja"=1, "Nei"=0)),
        selectInput(inputId = "fremBakKvalInd", label="Tilgang ",
                    choices = c("Alle"=0, "Fremre"=1, "Bakre"=2)),
        selectInput(inputId = 'enhetsUtvalgKvalInd', label='Egen enhet og/eller landet',
                    choices = c("Egen mot resten av landet"=1, "Hele landet"=0, "Egen enhet"=2)
        )
      ),
      conditionalPanel( #Denne skal bare vises for figursamlinger
        'input.ark == "Fordelinger"',
        #'input.ark === "Fordelinger" || input.ark === "Sykehusvise andeler" ',

        selectInput(inputId = "valgtVar", label="Velg variabel",
                    choices = c('Alder' = 'Alder', 'Antall nivå operert' = 'AntallNivaaOpr',
                                'Antibiotika' = 'Antibiotika', 'Arbeidstaus før operasjon' = 'ArbeidstausPreOp','Arbeidstaus 3 mnd. etter' = 'Arbeidstaus3mnd',
                                'Arbeidstaus 12 mnd. etter' = 'Arbeidstaus12mnd', 'ASA-grad' = 'ASAgrad',
                                'BMI' = 'BMI', 'Angst (EQ5D) før operasjon' = 'EqAngstPreOp',
                                'Søkt erstatning før operasjon' = 'ErstatningPreOp',
                                'Fornoydhet med behandlinga, 3 mnd. etter' = 'FornoydBeh3mnd',
                                'Fornoydhet med behandlinga, 12 mnd. etter' = 'FornoydBeh12mnd' ,
                                'Komorbiditet' = 'Komorbiditet',
                                'Komplikasjoner, pas.rapp. 3 mnd. etter' = 'Kompl3mnd',
                                'Kompllikasjoner ved operasjon' = 'KomplOpr',
                                'Liggedøgn, postoperativt' = 'LiggeDognPostop',
                                'Liggedøgn, totalt' = 'LiggeDognTotalt',
                                'Morsmål' = 'Morsmal',
                                'Nytte av operasjon, 3 mnd. etter' = 'NytteOpr3mnd',
                                'Nytte av operasjon, 12 mnd. etter' = 'NytteOpr12mnd',
                                'Operasjonskategori' = 'OperasjonsKategori',
                                'Operasjonsindikasjon' = 'OprIndik',
                                'Operasjonsindiaksjon, paresegrad' = 'OprIndikPareseGrad',
                                'Operasjonsindiaksjon, myelopati' = 'OprIndikMyelopati',
                                'Operasjonsindiaksjon, smerter' = 'OprIndikSmerter',
                                'Radiologi' = 'Radiologi', 'Røyker' = 'Roker',
                                'Snuser' = 'Snuser', 'Sivilstatus' = 'SivilStatus', 'Sårdren' = 'Saardren',
                                'Smertestill, bruk preoperativt' = 'SmertestillBrukPreOp',
                                'Symptomvarighet, armsmerter' = 'SymptVarighetArmer',
                                'Symptomvarighet, nakke/hodesmerter' = 'SymptVarighetNakkeHode',
                                'Tidligere operert' = 'TidlOpr',
                                'Tidligere operert, antall' = 'TidlOprAntall',
                                'Uforetrygdet før operasjon' = 'UforetrygdPreOp',
                                'Utdanning' = 'Utdanning') #c('Alder'='Alder', "Ant. nivå operert" = 'AntallNivaaOpr')
        ),
        dateRangeInput(inputId = 'datovalg', start = "2017-01-01", end = Sys.Date(),
                       label = "Tidsperiode", separator="t.o.m.", language="nb"),
        selectInput(inputId = "erMann", label="Kjønn",
                    choices = c("Begge"=2, "Menn"=1, "Kvinner"=0)
        ),
        sliderInput(inputId="alder", label = "Alder", min = 0,
                    max = 110, value = c(0, 110)
        ),
        selectInput(inputId = "myelopati", label="Myelopati",
                    choices = c("Ikke valgt"=2, "Ja"=1, "Nei"=0)),
        selectInput(inputId = "fremBak", label="Tilgang ",
                    choices = c("Alle"=0, "Fremre"=1, "Bakre"=2)),
        selectInput(inputId = 'enhetsUtvalg', label='Egen enhet og/eller landet',
                    choices = c("Egen mot resten av landet"=1, "Hele landet"=0, "Egen enhet"=2)
        )
        #sliderInput(inputId="aar", label = "Årstall", min = 2012,  #min(RegData$Aar),
        #           max = as.numeric(format(Sys.Date(), '%Y')), value = )
      ),

      conditionalPanel( #
        'input.ark == "Andeler"',
        #'input.ark === "Fordelinger" || input.ark === "Andeler" ',
       selectInput(inputId = "valgtVarAndelGrVar", label="Velg variabel",
                     choices = c('Alder' = 'Alder',
                     'Andre sykdommer' = 'AndreRelSykdommer',
                     'Antibiotika' = 'Antibiotika',
                     'Arbeidstaus før operasjon' = 'ArbeidstausPreOp',
                     'Arbeidstaus 3 mnd. etter' = 'Arbeidstaus3mnd',
                     'Arbeidstaus 12 mnd. etter' = 'Arbeidstaus12mnd',
                     'ASA-grad' = 'ASAgrad', 'BMI' = 'BMI',
                     'Komplikasjoner, pasientrapportert 3 mnd. etter' = 'EnhverKompl3mnd',
                     'Søkt erstatning før operasjon' = 'ErstatningPreOp',
                     'Fornøydhet med behandlinga, 3 mnd. etter' = 'FornoydBeh3mnd',
                     'Fornøydhet med behandlinga, 12 mnd. etter' = 'FornoydBeh12mnd',
                     'Misfornøyd, 3 mnd. etter' = 'Misfor3mnd',
                     'Misforøyd, 12 mnd. etter' = 'Misfor12mnd',
                     'Komplikasjon, dyp infeksjon, 3 mnd. etter' = 'KomplinfekDyp3mnd',
                     'Komplikasjon, overfladisk infeksjon, 3 mnd. etter' = 'KomplinfekOverfl3mnd',
                     'Komplikasjon med stemme, 3 mnd. etter' = 'KomplStemme3mnd',
                     'Komplikasjon med svelging, 3 mnd. etter' = 'KomplSvelging3mnd',
                     'NDIendring over 30%, 12 mnd. etter' = 'NDIendr12mnd30pst',
                     'Nytte av operasjon, 3 mnd. etter' = 'NytteOpr3mnd',
                     'Nytte av operasjon, 12 mnd. etter' = 'NytteOpr12mnd',
                     'NRSendring, smerter i arm, 12.mnd.' = 'NRSsmerteArmEndr12mnd',
                     'Forverring, 3 mnd. etter' = 'Verre3mnd',
                     'Forverring, 12 mnd. etter' = 'Verre12mnd',
                     'Operasjonsindikasjon, myelopati' = 'OprIndikMyelopati',
                     'Røyker' = 'Roker', 'Sårdren' = 'Saardren',
                     'Smertestillende, preoperativt' = 'SmertestillPreOp',
                     'Symptomvarighet, armsmerter' = 'SymptVarighetArmer',
                     'Symptomvariaghet, nakke/hodesmerter' = 'SymptVarighetNakkeHode',
                     'Søkt uføretrygd før operasjon' = 'UforetrygdPreOp',
                     'Utdanning' = 'Utdanning')
        ),
        dateRangeInput(inputId = 'datovalgAndelGrVar', start = "2017-01-01", end = Sys.Date(),
                       label = "Tidsperiode", separator="t.o.m.", language="nb"),
        selectInput(inputId = "erMannAndelGrVar", label="Kjønn",
                    choices = c("Begge"=2, "Menn"=1, "Kvinner"=0)
        ),
        sliderInput(inputId="alderAndelGrVar", label = "Alder", min = 0,
                    max = 110, value = c(0, 110)
        ),
        selectInput(inputId = "myelopatiAndelGrVar", label="Myelopati",
                    choices = c("Ikke valgt"=2, "Ja"=1, "Nei"=0)),
        selectInput(inputId = "fremBakAndelGrVar", label="Tilgang ",
                    choices = c("Alle"=0, "Fremre"=1, "Bakre"=2)),
       br(),
       p(em('Følgende utvalg gjelder bare figuren som viser utvikling over tid')),
       selectInput(inputId = 'enhetsUtvalgAndelTid', label='Egen enhet og/eller landet',
                   choices = c("Egen mot resten av landet"=1, "Hele landet"=0, "Egen enhet"=2)
       ),
       selectInput(inputId = "tidsenhetAndelTid", label="Velg tidsenhet",
                   choices = rev(c('År'= 'Aar', 'Halvår' = 'Halvaar',
                                   'Kvartal'='Kvartal', 'Måned'='Mnd')))

      ),
      conditionalPanel( #
        'input.ark == "Gjennomsnitt"',
        #'input.ark === "Fordelinger" || input.ark === "Andeler" ',
        selectInput(inputId = "valgtVarGjsn", label="Velg variabel",
                    choices = c('Alder' = 'Alder',
                                'EMS før operasjon, myelopatipasienter' = 'EMSscorePreOp',
                                'EMS-forbedring, myelopati, 12 mnd.' = 'EMSendr12mnd',
                                'EMS-forbedring, myelopati, 3 mnd.' = 'EMSendr3mnd',
                                'EQ5D før operasjon' = 'Eq5DScorePreOp',
                                'EQ5D-forbedring, 12 mnd.' = 'EQ5Dendr12mnd',
                                'EQ5D-Forbedring, 3 mnd.' = 'EQ5Dendr3mnd',
                                'Liggetid etter operasjon' = 'LiggeDognPostop',
                                'Liggetid, totalt' = 'LiggeDognTotalt',
                                'NDI før operasjon' = 'NDIscorePreOp',
                                'NDI-forbedring, 3 mnd.' = 'NDIendr3mnd',
                                'NDI-forbedring, 12 mnd.' = 'NDIendr12mnd',
                                'NSR, arm før operasjon' = 'NRSsmerteArmPreOp',
                                'NSR, nakke før operasjon' = 'NRSsmerteNakkePreOp',
                                'Total knivtid' = 'KnivtidTotalMin'
                    )
        ),
        dateRangeInput(inputId = 'datovalgGjsn', start = "2017-01-01", end = Sys.Date(),
                       label = "Tidsperiode", separator="t.o.m.", language="nb"),
        selectInput(inputId = "erMannGjsn", label="Kjønn",
                    choices = c("Begge"=2, "Menn"=1, "Kvinner"=0)
        ),
        sliderInput(inputId="alderGjsn", label = "Alder", min = 0,
                    max = 110, value = c(0, 110)
        ),
        selectInput(inputId = "myelopatiGjsn", label="Myelopati",
                    choices = c("Ikke valgt"=2, "Ja"=1, "Nei"=0)),
        selectInput(inputId = "fremBakGjsn", label="Tilgang ",
                    choices = c("Alle"=0, "Fremre"=1, "Bakre"=2)),
        selectInput(inputId = "sentralmaal", label="Velg gjennomsnitt/median ",
                    choices = c("Gjennomsnitt"='Gjsn', "Median"='Med')),
        br(),
        p(em('Følgende utvalg gjelder bare figuren som viser utvikling over tid')),
        selectInput(inputId = 'enhetsUtvalgGjsn', label='Egen enhet og/eller landet',
                    choices = c("Egen mot resten av landet"=1, "Hele landet"=0, "Egen enhet"=2)
        ),
        selectInput(inputId = "tidsenhetGjsn", label="Velg tidsenhet",
                    choices = rev(c('År'= 'Aar', 'Halvår' = 'Halvaar',
                                    'Kvartal'='Kvartal', 'Måned'='Mnd')))

      )
	), #sidebarPanel/kolonna til venstre




    # Vise det vi har valgt...
    column(width = 7, #mainPanel(
      tabsetPanel( #
        id='ark',

        tabPanel("Viktigste resultater",
                 #fluidRow(
                   #column(width=5,
                 h2("Månedsrapport"), #),
                   #column(width=2,
                 downloadButton(outputId = 'mndRapp.pdf', label='FUNKER pt IKKE', class = "butt"),
                 tags$head(tags$style(".butt{background-color:#6baed6;} .butt{color: white;}")), # background color and font color
                   #)),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 h2("Kvalitetsindikatorer", align='center' ),
                 br(),
                 h3(em("Utvikling over tid")),
                 plotOutput("kvalIndFig1"),
                 br(),
                 h3(em("Sykehusvise resultater")),
                 plotOutput("kvalIndFig2")),
        tabPanel("Tabeller",
                 h2("Antall registreringer per måned og avdeling"),
                 tableOutput("tabAvdMnd12"),
                 br(),
                 h2("Ferdigstilte skjema ved hver avdeling for valgte 12 måneder"),
                 p(em("Velg tidsperiode ved å velge sluttdato i menyen til venstre")),
                 tableOutput("tabAvdSkjema12"),
                 br(),
                 h2("Antall registreringer per år og avdeling, siste 5 år"),
                 tableOutput("tabAvdNAar5")
        ),
        tabPanel("Fordelinger",
                 h3("Fordeling av valgt variabel"),
                 h5("Hvilken variabel man ønsker å se resultater for, velges fra rullegardinmenyen
                    til venstre. Man kan også gjøre ulike filtreringer."),
                 br(),
                 br(),
                 plotOutput("fordelinger")),
        tabPanel("Andeler",
                 h2("Sykehusvise andeler og utvikling over tid for valgt variabel"),
                 h5("Hvilken variabel man ønsker å se resultater for, velges fra rullegardinmenyen
                    til venstre. Man kan også gjøre ulike filtreringer."),
                 br(),
                 br(),
                 plotOutput("andelerGrVar"),
                 plotOutput("andelTid")),
      tabPanel("Gjennomsnitt",
               h2("Sykehusvise gjennomsnitt/median og utvikling over tid for valgt variabel"),
               h5("Hvilken variabel man ønsker å se resultater for, velges fra rullegardinmenyen
                  til venstre. Man kan også gjøre ulike filtreringer."),
               br(),
               br(),
               plotOutput("gjsnGrVar"),
               plotOutput("gjsnTid"))
      )
#    )
	) #mainPanel
  ) #xxrLayout
) #fluidpage, dvs. alt som vises på skjermen




#----------------- Define server logic required to draw a histogram
server <- function(input, output) {

  library(Nakke)
  library(lubridate)
  library(zoo)
  system.file('inst/NakkeMndRapp.Rnw', package='Nakke')
  #load('A:/Nakke/NakkeAarsrapp2016.Rdata') #Preprossesserte data
  dato <- '2018-03-16'
  fil <- paste0('A:/Nakke/AlleVarNum',dato,'.csv')
  #RegData <- read.table(fil, sep=';', header=T, encoding = 'UTF-8')
  #Funker:
  data('NakkeRegDataSyn', package = 'Nakke')
  #try(data(package = "Nakke"))

  RegData <- NakkePreprosess(RegData = RegData)
  datoTil <- as.POSIXlt(Sys.Date())
  AarNaa <- as.numeric(format(Sys.Date(), "%Y"))
  # Nye variable:
  RegData$Mnd <- RegData$InnDato$mon +1
  RegData$Kvartal <- ceiling(RegData$Mnd/3)
  RegData$Halvaar <- ceiling(RegData$Mnd/6)
  aarFra <- paste0(1900+as.POSIXlt(Sys.Date())$year-5, '-01-01')
  reshIDdummy <- 601161

  #SkjemaRekkeflg #1-pasientskjema, 2-legeskjema, 3- Oppf. 3mnd, 4 - Oppf. 12mnd
  fil <- paste0('A:/Nakke/SkjemaOversikt',dato,'.csv')
  data('SkjemaDataSyn', package = 'Nakke')
  #SkjemaData <- read.table(fil, sep=';', header=T, encoding = 'UTF-8')
  SkjemaData <- SkjemaData[SkjemaData$SkjemaStatus > -1, ]
  SkjemaData$InnDato <- as.POSIXlt(SkjemaData$HovedDato, format="%Y-%m-%d")
  SkjemaData$Aar <- 1900 + strptime(SkjemaData$InnDato, format="%Y")$year
  SkjemaData$Mnd <- as.yearmon(SkjemaData$InnDato)
  SkjemaData$Sykehusnavn <- as.factor(SkjemaData$Sykehusnavn)

  #  texfil <- knitr::knit(system.file('NakkeMndRapp.Rnw', package='Nakke'), encoding = 'UTF-8')
  #  texi2pdf(system.file(texfil, package='Nakke'),clean = TRUE) #"NakkeMndRapp.tex"



  #Felles reaktive tabeller
  #   reactive({
  #   SkjemaData <- SkjemaData[which(SkjemaData$SkjemaStatus == input$status), ]
  #   SkjemaData12mnd <- SkjemaData[as.POSIXlt(SkjemaData$HovedDato, format="%Y-%m-%d") > as.POSIXlt(datoFra12), ]
  #
  # })

  output$tabAvdMnd12 <- renderTable({
    datoFra12 <- as.Date(paste0(as.numeric(substr(input$datoTil,1,4))-1, substr(input$datoTil,5,8), '01'))
    SkjemaData12mnd <- SkjemaData[SkjemaData$InnDato < as.POSIXlt(input$datoTil)
                                  & SkjemaData$InnDato > as.POSIXlt(datoFra12), ]
    if (as.numeric(input$status) %in% 0:1) {SkjemaData12mnd <-
      SkjemaData12mnd[which(SkjemaData12mnd$SkjemaStatus == as.numeric(input$status)), ]
    }
    #Flyttes til overvåkning
    tabAvdSiste12mnd <- addmargins(table(SkjemaData12mnd[SkjemaData12mnd$SkjemaRekkeflg==2, c('Sykehusnavn', 'Mnd')]))
    colnames(tabAvdSiste12mnd) <- substring(colnames(tabAvdSiste12mnd),1,3)
    xtable::xtable(tabAvdSiste12mnd)
  },
  rownames = TRUE, digits=0 #, align = c('l', rep('r', ncol(tabAvdSiste12mnd)))
  )



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

