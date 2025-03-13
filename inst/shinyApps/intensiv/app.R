#library(intensiv)
# gjør Rapportekets www-felleskomponenter tilgjengelig for applikasjonen
addResourcePath('rap', system.file('www', package='rapbase'))

context <- Sys.getenv("R_RAP_INSTANCE") #Blir tom hvis jobber lokalt
paaServer <- (context %in% c("DEV", "TEST", "QA", "PRODUCTION")) #rapbase::isRapContext()
regTitle = 'Norsk Intensivregister, datahenting'

ui <- navbarPage( #fluidPage( #"Hoved"Layout for alt som vises på skjermen

  # lag logo og tittel som en del av navbar
  title = div(a(includeHTML(system.file('www/logo.svg', package='rapbase'))),
              regTitle),
  # sett inn tittel også i browser-vindu
  windowTitle = regTitle,
  theme = "rap/bootstrap.css",


  tabPanel(p("Registeradministrasjon", title='Registeradministrasjonens side for registreringer og resultater'),
           value = "Registeradministrasjon",
           h3('Siden er bare synlig for SC-bruker', align = 'center'),

           tabPanel(
             h4("Eksport av krypterte data"),
             sidebarLayout(
               sidebarPanel(
                 rapbase::exportUCInput("intensivExport")
               ),
               shiny::mainPanel(
                 rapbase::exportGuideUI("intensivExportGuide")
               )
             )
           ) #Eksport-tab
  ) #tab SC

) #ui-del



#----- Define server logic required to draw a histogram-------
server <- function(input, output, session) {


    #----------- Eksport ----------------
  rapbase::exportUCServer("intensivExport", registryName = "nir",
                          repoName = "intensiv",
                          eligible = (rapbase::getUserRole(session) == "SC")
  )
  ## veileding
  rapbase::exportGuideServer("intensivExportGuide", registryName = "intensiv")

} #server
# Run the application
shinyApp(ui = ui, server = server)

