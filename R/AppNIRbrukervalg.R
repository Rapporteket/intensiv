#' Funksjon som inneholder alle brukervalg for Intensivregisteret 
#'
#' Denne funksjonen beregner ikke noe. :-|
#' 
#' Mer info?
#'
#'  @param id namespace for this module
#'  @param label Label for this module
#' @return Visning av brukervalg
#'
#' @export
AppNIRbrukervalg  <- function(inputId = 'type valg', label = "Brukervalg") { #id - hvis skal bruke namespace, dat, 

#Ser ut til at denne må deles opp i flere funksjoner elller...      
            #  @param RegData En dataramme med alle nødvendige variabler fra registeret
      #uiInputModule <- 
            
            # create namespace
            #ns <- NS(id)
            
            # make values and lables
            #years <- sort(dplyr::distinct(dat, Aar)$Aar)
            #hospital_names <- sort(dplyr::distinct(dat, ShNavn)$ShNavn)
            
            tagList(
                  # selectInput(inputId = ns("year"), label = "År:",
                  #             choices = years,
                  #             selected = years,
                  #             multiple = TRUE
                  # ),
                 # input.ark == "Fordelinger"',
                  
                  selectInput(inputId = "valgtVar", label="Velg variabel",
                              choices = c('Alder' = 'alder', 
                                          'Innkomstmåte' = 'InnMaate',
                                          'Hemodynamisk overvåkn.' = 'ExtendedHemodynamicMonitoring',
                                          'Inklusjonskriterier' = 'inklKrit',
                                          'Isolasjon, type' = 'isolering',
                                          'Isolasjon, varighet' = 'isoleringDogn',
                                          'Liggetid' = 'liggetid',
                                          'Nas-skår (sykepleierakt.)' = 'Nas24',
                                          'NEMS-skår (ressursbruk)' = 'NEMS24',
                                          'Nyrebeh., type' = 'nyreBeh',
                                          'Nyrebeh., varighet' = 'nyreBehTid',
                                          'Primærårsak' = 'PrimaryReasonAdmitted',
                                          'Respiratortid' = 'respiratortid',
                                          'Respiratortid, ikke-invasiv' = 'respiratortidNonInv',
                                          'Respiratortid, invasiv m/overf.' = 'respiratortidInvMoverf',
                                          'Respiratortid, invasiv u/overf.' = 'respiratortidInvUoverf',
                                          'SAPSII-skår (alvorlighet av sykd.)' = 'SAPSII',
                                          'Spesielle tiltak' = 'spesTiltak')
                  ),
                  dateRangeInput(inputId = 'datovalg', start = "2017-01-01", end = Sys.Date(),
                                 label = "Tidsperiode", separator="t.o.m.", language="nb"),
                  selectInput(inputId = "erMann", label="Kjønn",
                              choices = c("Begge"=2, "Menn"=1, "Kvinner"=0)
                  ),
                  sliderInput(inputId="alder", label = "Alder", min = 0,
                              max = 110, value = c(0, 110)
                  ),
                  selectInput(inputId = 'enhetsUtvalg', label='Egen enhet og/eller landet',
                              choices = c("Egen mot resten av landet"=1, "Hele landet"=0, "Egen enhet"=2)
                  )
            )
}
                  
      