#' Funksjon som inneholder alle brukervalg for Intensivregisteret 
#'
#' Denne funksjonen beregner ikke noe. :-|
#' 
#' Mer info?
#'
#'  @param id namespace for this module
#'  @param inputId id for brukervalget til den aktuelle figur/tabelltypen
#'  @param label Label for this module
#' @return Visning av brukervalg
#'
#' @export
AppNIRbrukervalg  <- function(ark = 'angi sideID') { #id - hvis skal bruke namespace, dat, 

#Ser ut til at denne må deles opp i flere funksjoner elller...      
            #  @param RegData En dataramme med alle nødvendige variabler fra registeret
      #uiInputModule <- 
            
            # create namespace
            #ns <- NS(id)
            
            # make values and lables
            #years <- sort(dplyr::distinct(dat, Aar)$Aar)
            #hospital_names <- sort(dplyr::distinct(dat, ShNavn)$ShNavn)
 if (ark == 'Fordelinger') {           
      tagList(
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
      )}
            
if (ark=='Andeler') {      
      tagList(
            selectInput(inputId = "valgtVarAndelGrVar", label="Velg variabel",
                        choices = c('Alder minst 80 år' = 'alder_over80',
                                    'Alder under 18år' = 'alder_u18',
                                    'Død innen 30 dager' = 'dod30d',
                                    'Døde på intensiv' = 'dodeIntensiv',
                                    'Isolasjon av pasient' = 'isolering',
                                    'Liggetid, døde' = 'liggetidDod',
                                    'Nyrebehandling' = 'nyreBeh',
                                    'Reinnleggelse' = 'reinn',
                                    'Respiratorstøtte' = 'respStotte',
                                    'Respiratortid, inv. < 2,5d m/overf.' = 'respiratortidInvMoverf',
                                    'Respiratortid, inv. < 2,5d u/overf.' = 'respiratortidInvUoverf',
                                    'Respiratortid, døde' = 'respiratortidDod',
                                    'Utvidet hemodyn. overvåkning' = 'ExtendedHemodynamicMonitoring',
                                    'Trakeostomi' = 'trakeostomi',
                                    'Trakeostomi, åpen' = 'trakAapen')
            ),
            dateRangeInput(inputId = 'datovalgAndelGrVar', start = "2017-01-01", end = Sys.Date(),
                           label = "Tidsperiode", separator="t.o.m.", language="nb"),
            selectInput(inputId = "erMannAndelGrVar", label="Kjønn",
                        choices = c("Begge"=2, "Menn"=1, "Kvinner"=0)),
            sliderInput(inputId="alderAndelGrVar", label = "Alder", min = 0,
                        max = 110, value = c(0, 110)),
            br(),
            p(em('Følgende utvalg gjelder bare figuren som viser utvikling over tid')),
            selectInput(inputId = 'enhetsUtvalgAndelTid', label='Egen enhet og/eller landet',
                        choices = c("Egen mot resten av landet"=1, "Hele landet"=0, "Egen enhet"=2)),
            selectInput(inputId = "tidsenhetAndelTid", label="Velg tidsenhet",
                        choices = rev(c('År'= 'Aar', 'Halvår' = 'Halvaar',
                                        'Kvartal'='Kvartal', 'Måned'='Mnd')))
      )}

}
                  
      