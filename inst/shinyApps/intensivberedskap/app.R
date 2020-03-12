#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(magrittr)
library(rapbase)
library(intensiv)

addResourcePath('rap', system.file('www', package='rapbase'))
context <- Sys.getenv("R_RAP_INSTANCE") #Blir tom hvis jobber lokalt
paaServer <- context %in% c("DEV", "TEST", "QA", "PRODUCTION")

#options(knitr.table.format = "html")
idag <- Sys.Date() #as.Date('2018-11-30') #
datoTil <- as.POSIXlt(idag)
startDato <- '2020-03-01'  #paste0(as.numeric(format(idag-90, "%Y")), '-01-01') 
#AarNaa <- as.numeric(format(idag, "%Y"))

regTitle <- ifelse(paaServer, 
                   'Norsk Intensivregister, Beredskapsregistrering',
                   'Norsk Intensivregister med FIKTIVE data')


#---------Hente data------------
#CoroData <- read.table(file='A:/Intensiv/ReadinessFormDataContract2020-03-12.csv', header=T, stringsAsFactors=FALSE, sep=';') #,encoding = 'UTF-8')

if (paaServer) {
  #CoroData <- NIRRegDataSQL(datoFra='2011-01-01', skjema=4) #, session = session) #datoFra = datoFra, datoTil = datoTil)
  qCoro <- 'SELECT *  from ReadinessFormDataContract'
  CoroData <- rapbase::LoadRegData(registryName= "nir", query=qCoro, dbType="mysql")
  
  #repLogger(session = session, 'Hentet alle data fra intensivregisteret')
} #hente data på server

CoroData <- NIRPreprosess(RegData = CoroData, skjema=4)


#-----Definere utvalgsinnhold og evt. parametre som er statiske i appen----------


#Definere utvalgsinnhold
#sykehusNavn <- sort(c('',unique(CoroData$ShNavn)), index.return=T)
#sykehusValg <- c(0,unique(CoroData$ReshId))[sykehusNavn$ix]
hfNavn <- sort(unique(CoroData$ShNavn), index.return=T)
sykehusNavn <- sort(unique(CoroData$ShNavn), index.return=T)
sykehusValg <- unique(CoroData$ReshId)[sykehusNavn$ix]
sykehusValg <- c(0,sykehusValg)
names(sykehusValg) <- c('Ikke valgt',sykehusNavn$x)

enhetsUtvalg <- c("Egen region" = 7,
                  "Hele landet"=0, 
              #    "Eget HF mot egen region" = 6, 
                  "Egen region mot resten" = 8)


ui <- tagList(
  navbarPage(
    title = div(img(src="rap/logo.svg", alt="Rapporteket", height="26px"),
                regTitle),
    windowTitle = regTitle,
    theme = "rap/bootstrap.css",
    
    tabPanel("Oversikt",
             sidebarPanel(
                 width = 3,
                 h3('Dokument med samling av resultater'),
                 h5('Dette kan man få regelmessig tilsendt på e-post. 
                    Gå til fanen "Abonnement" for å bestille dette.'),
                 br(),
                 h3("Coronarapport"), #),
                 downloadButton(outputId = 'CoroRapp.pdf', label='Last ned Coronarapport', class = "butt"),
                 tags$head(tags$style(".butt{background-color:#6baed6;} .butt{color: white;}")), # background color and font color
                 br()
             ),
             mainPanel(width = 12,
                       #shinyalert::useShinyalert(),
                       #h3('Risikofaktorer'),
                       #tableOutput('tabRisikofaktorer')
                       
                       # appNavbarUserWidget(user = uiOutput("appUserName"),
                       #                     organization = uiOutput("appOrgName")
                                           
             )
    ),
    
#-----------Abonnement--------------------------------
    tabPanel("Abonnement"
             ,
             sidebarLayout(
               sidebarPanel(width = 3,
                 selectInput("subscriptionRep", "Dokument:", c("Corona")),
                 selectInput("subscriptionFreq", "Frekvens:",
                             list(Månedlig="month", Ukentlig="week", Daglig="DSTday"),
                             selected = "week"),
                 actionButton("subscribe", "Bestill!")
               ),
                mainPanel(
               #   uiOutput("subscriptionContent")
                )
             )
    )
    
  ) # navbarPage
) # tagList
#----------Slutt ui-del--------------


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Last inn data
  # regData <- getFakeRegData()

  #-----------Div serveroppstart------------------  
 # raplog::appLogger(session = session, msg = "Starter Corona-app")
  
  reshID <- ifelse(paaServer, as.numeric(rapbase::getUserReshId(session)), 0)
  rolle <- ifelse(paaServer, rapbase::getUserRole(shinySession=session), 'SC')
  brukernavn <- ifelse(paaServer, rapbase::getUserName(shinySession=session), 'brukernavn')
  
  if (reshID %in% unique(CoroData$ReshId)) {
    indReshEgen <- match(reshID, CoroData$ReshId)
    egetShNavn <- as.character(CoroData$ShNavn[indReshEgen])
    egetRHF <- as.character(CoroData$RHF[indReshEgen])
    egetHF <- as.character(CoroData$HF[indReshEgen])
    egenLokalitet <- c(0, 2, 4, 7)
    names(egenLokalitet) <- c('hele landet', egetShNavn, egetRHF)
  }
  
  
  
  # widget
  if (paaServer) {
    output$appUserName <- renderText(rapbase::getUserFullName(session))
    output$appOrgName <- renderText(paste0('rolle: ', rolle(), 
                                           '<br> ReshID: ', reshID,
                                           '<br> Org: ', egenOrg) )}
  
  # # User info in widget
  # userInfo <- rapbase::howWeDealWithPersonalData(session)
  # observeEvent(input$userInfo, {
  #   shinyalert::shinyalert("Dette vet Rapporteket om deg:", userInfo,
  #                          type = "", imageUrl = "rap/logo.svg",
  #                          closeOnEsc = TRUE, closeOnClickOutside = TRUE,
  #                          html = TRUE, confirmButtonText = rapbase::noOptOutOk())
  # })
  # 

  # Figur og tabell
  ## Figur
  #output$distPlot <- renderPlot({
  #  makeHist(df = regData, var = input$var, bins = input$bins)
  #})
  
  ## Tabell
  #output$distTable <- renderTable({
  #  makeHist(df = regData, var = input$var, bins = input$bins, makeTable = TRUE)
  #})
  
  
  #-------- Laste ned Samlerapport------------
  
  output$CoroRapp.pdf <- downloadHandler(
    filename = function(){
      paste0('CoronaRapport', Sys.time(), '.pdf')}, 
    content = function(file){
      henteSamlerapporter(file, rnwFil="BeredskapCorona.Rnw", 
                          reshID = reshID) #Vurder å ta med tidsinndeling eller startdato
    }
  )
  
  #----------Tabeller----------------------------
  
  
  #------------- Abonnement----------------
  ## rekative verdier for å holde rede på endringer som skjer mens
  ## applikasjonen kjører
  rv <- reactiveValues(
    subscriptionTab = rapbase::makeUserSubscriptionTab(session))
  
  ## lag tabell over gjeldende status for abonnement
  output$activeSubscriptions <- DT::renderDataTable(
    rv$subscriptionTab, server = FALSE, escape = FALSE, selection = 'none',
    options = list(dom = 'tp', ordning = FALSE), rownames = FALSE
  )
  
  ## lag side som viser status for abonnement, også når det ikke finnes noen
  output$subscriptionContent <- renderUI({
    userFullName <- rapbase::getUserFullName(session)
    userEmail <- rapbase::getUserEmail(session)
    if (length(rv$subscriptionTab) == 0) {
      p(paste("Ingen aktive abonnement for", userFullName))
    } else {
      tagList(
        p(paste0("Aktive abonnement som sendes per epost til ", userFullName,
                 "(",userEmail, "):")),
        DT::dataTableOutput("activeSubscriptions")
      )
    }
  })
  
  ## nye abonnement
  # observeEvent (input$subscribe, {
  #   package <- "rapRegTemplate"
  #   owner <- getUserName(session)
  #   interval <- strsplit(input$subscriptionFreq, "-")[[1]][2]
  #   intervalName <- strsplit(input$subscriptionFreq, "-")[[1]][1]
  #   runDayOfYear <- rapbase::makeRunDayOfYearSequence(
  #     interval = interval)
  #   
  #   rapbase::getUserEmail(session)
  #   organization <- rapbase::getUserReshId(session)
  #   
  #   if (input$subscriptionRep == "Samlerapport1") {
  #     synopsis <- "Automatisk samlerapport1"
  #     fun <- "samlerapport1Fun"
  #     paramNames <- c("p1", "p2")
  #     paramValues <- c("Alder", 1)
  #     
  #   }
  #   if (input$subscriptionRep == "Samlerapport2") {
  #     synopsis <- "Automatisk samlerapport2"
  #     fun <- "samlerapport2Fun"
  #     paramNames <- c("p1", "p2")
  #     paramValues <- c("BMI", 2)
  #   }
  #   rapbase::createAutoReport(synopsis = synopsis, package = package,
  #                             fun = fun, paramNames = paramNames,
  #                             paramValues = paramValues, owner = owner,
  #                             email = email, organization = organization,
  #                             runDayOfYear = runDayOfYear,
  #                             interval = interval, intervalName = intervalName)
  #   rv$subscriptionTab <- rapbase::makeUserSubscriptionTab(session)
  # })
  
  ## slett eksisterende abonnement
  observeEvent(input$del_button, {
    selectedRepId <- strsplit(input$del_button, "_")[[1]][2]
    rapbase::deleteAutoReport(selectedRepId)
    rv$subscriptionTab <- rapbase::makeUserSubscriptionTab(session)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

