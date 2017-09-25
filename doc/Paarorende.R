rm(list=ls())
library(intensiv)



PaarorData <- read.table(file='C:/Registre/NIR/data/PaarorendeMars2017.csv', header=T, sep=';',encoding = 'UTF-8')
#-1 = Velg verdi
#1 = Fremragende
#2 = Meget godt
#3 = Godt
#4 = Noenlunde
#5 = Dårlig
#6 = Ikke aktuelt
verdi <- c(-1,1:6)
txt <- c('None', 'SvaertBra', 'MegetBra', 'Bra', 'GanskeBra', 'Daarlig', 'IkkeRelevant')
mapping <- data.frame(verdi,txt)

PaarorData$BehandlingHoeflighetRespektMedfoelelseNum <- mapping$verdi[match(PaarorData$BehandlingHoeflighetRespektMedfoelelse, mapping$txt)]
PaarorData$BehandlingBesvarerHoeflighetRespektMedfoelelseNum <- mapping$verdi[match(PaarorData$BehandlingBesvarerHoeflighetRespektMedfoelelse, mapping$txt)]
PaarorData$InformasjonsOverensstemmelseNum <- mapping$verdi[match(PaarorData$InformasjonsOverensstemmelse, mapping$txt)]

txt <- c('None', 'MegetOfte', 'Ofte', 'AvOgTil', 'Sjelden', 'Aldri', 'IkkeRelevant') 
mapping <- data.frame(verdi,txt)
PaarorData$LegeInformasjonFrekvensNum <- mapping$verdi[match(PaarorData$LegeInformasjonFrekvens, mapping$txt)]
#-1 = Velg verdi
#1 = Svært ofte
#2 = Ofte
#3 = Av og til
#4 = Sjelden
#5 = Aldri
#6 = Ikke relevant

PreData <- read.table(file='C:/Registre/NIR/data/Main2017-03-20.csv', header=T, sep=';',encoding = 'UTF-8')
PaarorData$PreskjemaGUID <- toupper(PaarorData$PreskjemaGUID)
hovedVar <- c('SkjemaGUID','DateAdmittedIntensive', 'DaysAdmittedIntensiv','Respirator','TransferredStatus', 
               'Saps2Score','Saps2ScoreNumber', 'TypeOfAdmission', 'Nems', 'Morsdato')
RegData <- merge(PaarorData,PreData[ ,hovedVar], 
                 by.x='PreskjemaGUID', by.y='SkjemaGUID', all.x = TRUE, all.y = FALSE)
write.table(RegData, file = 'PaarorData.csv', row.names=FALSE, sep = ';', fileEncoding = "UTF-8")

#Definerer variabel hvor 1 - før intervensjon, 2-etter intervensjon (innleggelse f.o.m. 1.okt. 2016)
#Fyrste måleperiode var pasientar innlagde 01.10.15-31.12.15 og andre 01.10.16-31.12.16.  
#Basaldata gjeld altså data for alle opphald før 01.10.16, og kontroll etter intervensjon er 
#data for alle opphald etter 01.10.16.
#DETTE BLE GJORT MANUELT I FILA PaarorData.csv

#RegData$PrePost <- 0
#RegData$PrePost[as.POSIXlt(RegData$InnDato) < as.POSIXlt('2016-10-01')] <- 1
#RegData$PrePost[as.POSIXlt(RegData$InnDato) >=as.POSIXlt('2016-10-01')] <- 2
#feil: RegData$PrePost[is.na(RegData$InnDato) & (as.POSIXlt(RegData$FormDate, format='%Y.%m.%d %H:%M') < as.POSIXlt('2016-10-01'))] <- 1
#RegData$PrePost[is.na(RegData$InnDato) & (as.POSIXlt(RegData$FormDate, format='%Y.%m.%d %H:%M') >= as.POSIXlt('2016-10-01'))] <- 2







#--------------start her____________
PaarorData <- read.table(file='C:/Registre/NIR/data/PaarorData.csv', header=T, sep=';',encoding = 'UTF-8')
RegData <- NIRPreprosess(RegData=PaarorData)


variable <- c('BehandlingHoeflighetRespektMedfoelelseNum', 
              'BehandlingBesvarerHoeflighetRespektMedfoelelseNum','InformasjonsOverensstemmelseNum', 'LegeInformasjonFrekvensNum')
valgtVar <- 'LegeInformasjonFrekvensNum'
for (valgtVar in variable) {
      outfile <- paste0(valgtVar, '.pdf')
      NIRFigPrePostMars(RegData=RegData, valgtVar=valgtVar, outfile=outfile)
}

NIRFigPrePostMars  <- function(RegData, valgtVar, outfile='')	
{

      RegData$VariabelGr <- RegData[ ,valgtVar]
      RegData$VariabelGr[RegData$VariabelGr==-1] <- 9
      RegData$VariabelGr[RegData$VariabelGr==6] <- 9
      gr <- c(1:5,8:9)
      RegData$VariabelGr <- factor(RegData$VariabelGr, levels=gr)
      #      -1:Velg verdi, 1:Fremragende, 2:Meget godt, 3:Godt, 4:Noenlunde, 5:Dårlig, 6:Ikke aktuelt	
if (valgtVar %in% c('BehandlingHoeflighetRespektMedfoelelseNum', 
                    'BehandlingBesvarerHoeflighetRespektMedfoelelseNum','InformasjonsOverensstemmelseNum')) {
      grtxt <- c('Fremragende', 'Meget godt', 'Godt', 'Noenlunde', 'Dårlig', '','Ikke svart')

   }


if (valgtVar == 'LegeInformasjonFrekvensNum') {
#      -1:Velg verdi, 1:Svært ofte, 2: Ofte, 3: Av og til, 4: Sjelden, 5: Aldri, 6: Ikke relevant	
      grtxt <- c('Svært ofte', 'Ofte', 'Av og til', 'Sjelden', 'Aldri', '','Ikke svart')	
}
tittel <- switch(valgtVar,
    BehandlingHoeflighetRespektMedfoelelseNum = c('Hvor godt ble pasienten ivaretatt', 
                                                  'med hensyn til høflighet, respekt og medfølelse?'),
    BehandlingBesvarerHoeflighetRespektMedfoelelseNum = c('Hvordan ble du møtt av intensivpersonalet', 
                                                          'med hensyn til høflighet, respekt og medfølelse?'),
    InformasjonsOverensstemmelseNum = c('Hvor stor overensstemmelse var det i informasjonen', 
                                        'du fikk om tilstanden til pasienten?') ,
    LegeInformasjonFrekvensNum	= 'Hvor ofte snakket legene med deg om pasientens tilstand?')
    


#Skal sammenlikne før og etter intervensjon. Definert i variabelen PrePost.
#Gjør beregninger selv om det evt ikke skal vise figur ut. Trenger utdata.
AggVerdier <- list(Pre = 0, Post =0)
N <- list(Pre = 0, Post =0)
Ngr <- list(Pre = 0, Post =0)
ind <- list(Pre = which(RegData$PrePost==1), Post = which(RegData$PrePost==2))

Ngr$Pre <- table(RegData$VariabelGr[ind$Pre])
N$Pre <- sum(Ngr$Pre)	#length(ind$Pre)- Kan inneholde NA
AggVerdier$Pre <- 100*Ngr$Pre/N$Pre

Ngr$Post <- table(RegData$VariabelGr[ind$Post])
N$Post <- sum(Ngr$Post)
AggVerdier$Post <- 100*Ngr$Post/N$Post



#grtxt <- paste0(rev(NIRVarSpes$grtxt), ' (', rev(sprintf('%.1f',AggVerdier$Pre)), '%)') 
grtxt2 <- paste0(sprintf('%.1f',AggVerdier$Pre),' / ', sprintf('%.1f',AggVerdier$Post),'%')
grtxt2[6] <- ''
yAkseTxt='Andel pasienter (%)'

#NIRFigSoyler(RegData, AggVerdier, Ngr, tittel=tittel, hovedgrTxt='Før intervensjon', 
#                   smltxt='Etter intervensjon', Ngr = Ngr, KImaal <- '',
#                   N=N, retn='V', utvalgTxt='', grtxt=grtxt, grtxt2=grtxt2, cexgr=0.9, medSml=1, 
#                   xAkseTxt='', yAkseTxt=yAkseTxt, 
#                   outfile=outfile)	



AndelerPP <- list(Pre=0, Post=0)
NPre <- N$Pre
NPost <- N$Post
AndelerPP$Pre <- cbind(AggVerdier$Pre, AggVerdier$Post)

#-----------Definisjoner----------------------------


      #-----------Figur---------------------------------------

      #Plottspesifikke parametre:
      FigTypUt <- figtype(outfile, fargepalett='BlaaOff')	 
      #NutvTxt <- length(utvalgTxt)
      #vmarg <- switch(retn, V=0, H=max(0, strwidth(grtxt, units='figure', cex=cexgr)*0.7))
      #par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1+length(tittel)-1)))	#Har alltid datoutvalg med
      
      farger <- FigTypUt$farger
      antGr <- length(grtxt)
      #Ngr <- matrix(c(AntPre, AntPost), antGr, 2)
      lwdPost <- 3	#tykkelse på linja som repr. landet
      cexleg <- 0.9	#Størrelse på legendtekst
      cexpt <- 2	#Størrelse på punkter (resten av landet)
      
            #Vertikale søyler eller linje
            ymax <- min(max(c(AndelerPP$Pre, AndelerPP$Post),na.rm=T)*1.25, 110)
            pos <- barplot(t(AndelerPP$Pre), beside=TRUE, las=1, ylab="Andel pasienter (%)",	
                           cex.names=0.8, names.arg=grtxt, col=farger[c(3,1)], border='white', ylim=c(0, ymax))	# 
            mtext(at=pos[1,], grtxt2, side=1, las=1, cex=0.75, adj=0.2, line=0)
            legend('top', c(paste0('Før intervensjon, N=', NPre), 
                       paste0('Etter intervensjon, N=', NPost)), bty='n',
                         fill=farger[c(3,1)], border=NA, ncol=2, cex=cexleg)
      

      title(tittel, font.main=1)	#line=0.5, 
      
      par('fig'=c(0, 1, 0, 1)) 
      if ( outfile != '') {dev.off()}
}














