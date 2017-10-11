#Skåring
rm(list=ls())

PaarorData <- read.table(file='A:/Intensiv/QuestionaryFormDataContract2017-09-18.csv', header=T, sep=';',encoding = 'UTF-8')
load('A:/Intensiv/MainFormDataContract2017-09-18.Rdata') #read.table(file='C:/Registre/NIR/data/Main2017-03-20.csv', header=T, sep=';',encoding = 'UTF-8')
#PaarorData$PreskjemaGUID <- toupper(PaarorData$HovedskjemaGUID)
hovedVar <- c('SkjemaGUID','DateAdmittedIntensive', 'DaysAdmittedIntensiv','Respirator','TransferredStatus', 
              'Saps2Score','Saps2ScoreNumber', 'TypeOfAdmission', 'Nems', 'Morsdato', 
              'PatientTransferredFromHospital', 'PatientTransferredToHospital', 'ShNavn')
varUT <- which(names(PaarorData) %in% c('Forslag', 'Kommentar', 'Personalet'))
RegData <- merge(PaarorData[ ,-varUT], RegData[ ,hovedVar], 
                 by.x='HovedskjemaGUID', by.y='SkjemaGUID', all.x = TRUE, all.y = FALSE)
#write.table(RegData, file = 'PaarorData.csv', row.names=FALSE, sep = ';', fileEncoding = "UTF-8")

#Definerer variabel hvor 1 - før intervensjon, 2-etter intervensjon (innleggelse f.o.m. 1.okt. 2016)
#Fyrste måleperiode var pasientar innlagde 01.10.15-31.12.15 og andre 01.10.16-31.12.16.  
#Basaldata gjeld altså data for alle opphald før 01.10.16, og kontroll etter intervensjon er 
#data for alle opphald etter 01.10.16.

RegData$PrePost <- 0
RegData$PrePost[as.POSIXlt(RegData$DateAdmittedIntensive) < as.POSIXlt('2016-10-01')] <- 1
RegData$PrePost[as.POSIXlt(RegData$DateAdmittedIntensive) >=as.POSIXlt('2016-10-01')] <- 2
#feil: 
RegData$PrePost[is.na(RegData$DateAdmittedIntensive) & 
                      (as.POSIXlt(RegData$FormDate, format='%Y-%m-%d %H:%M') < as.POSIXlt('2016-10-01'))] <- 1
RegData$PrePost[is.na(RegData$DateAdmittedIntensive) & 
                      (as.POSIXlt(RegData$FormDate, format='%Y-%m-%d %H:%M') >= as.POSIXlt('2016-10-01'))] <- 2

Del1 <- c('BehandlingHoeflighetRespektMedfoelelse',
          'SymptomSmerte',
          'SymptomPustebesvaer',
          'SymptomUro',
          'BehandlingBesvarerBehov',
          'BehandlingBesvarerStoette',
          'BehandlingSamarbeid',
          'BehandlingBesvarerHoeflighetRespektMedfoelelse',
          'SykepleierOmsorg',
          'SykepleierKommunikasjon',
          'LegeBehandling',
          'AtmosfaerenIntensivAvd',
          'AtmosfaerenPaaroerenderom',
          'OmfangetAvBehandlingen')
Del2 <- c('LegeInformasjonFrekvens',
          'SvarPaaSpoersmaal',
          'ForklaringForstaaelse',
          'InformasjonsAerlighet',
          'InformasjonOmForloep',
          'InformasjonsOverensstemmelse',
          'BeslutningsInvolvering',
          'BeslutningsStoette',
          'BeslutningsKontroll',
          'BeslutningsTid',
          'LivsLengde',
          'LivssluttKomfor',
          'LivssluttStoette')



#----- OM SPØRSMÅLENE----------
#-1: Ikke besvart 
#Del1: Alle spm 1-5, 6:ikke aktuelt
#Del2: Spm 1-6:  1-5, 6:ikke aktuelt
#Spm 7-13[-10]: 1-5, 
#Spm 10 1-2
#Dvs. alle spm har spenn 1-5, unntatt spm.10 del 2
#Spørsmål som skal snus: Del2, spm.7-13 1:5 = 0:100


#Standard: 1:5 -> 100:0
verdi5 <- c(100, 75, 50, 25, 0)

library(plyr)
Del1Skaar <- paste0(Del1,'Skaar')
Del2Skaar <- paste0(Del2,'Skaar')
RegData[,c(Del1Skaar,Del2Skaar)] <- NA

Spm <- c(Del1,Del2[1:6])
Skaar <- paste0(Spm,'Skaar')
for (nr in 1:length(Spm)) { RegData[,Skaar[nr]] <- mapvalues(RegData[ ,Spm[nr]], from = c(-1,1:6), to = c(NA,verdi5,NA))}

RegData[ ,Del2Skaar[10]] <- mapvalues(RegData[ ,Del2[10]], from = c(-1,1:2), to = c(NA,0,100))

Spm <- Del2[c(7:9,11:13)]
Skaar <- paste0(Spm,'Skaar')
for (nr in 1:length(Spm)) { RegData[ ,Skaar[nr]] <-  mapvalues(RegData[ ,Spm[nr]], from = c(-1,1:5), to = c(NA,rev(verdi5)))}

PaarorDataSkaar <- 'A:/Intensiv/PaarorDataSkaar.csv'
write.table(RegData, file = PaarorDataSkaar, row.names=FALSE, sep = ';', fileEncoding = "UTF-8")



#--------------------------- Figurtilrettelegging og figur--------------------------------------

#--------------start her____________
PaarorData <- read.table(file=PaarorDataSkaar, header=T, sep=';',encoding = 'UTF-8')
library(intensiv)
RegData <- NIRPreprosess(RegData=PaarorData)


#variable <- c('BehandlingHoeflighetRespektMedfoelelseNum', 'BehandlingBesvarerHoeflighetRespektMedfoelelseNum','InformasjonsOverensstemmelseNum', 'LegeInformasjonFrekvensNum')
variable <- c(Del1, Del2)

#for (valgtVar in variable) {
#      outfile <- paste0(valgtVar, '.pdf')
#      NIRFigPrePostMars(RegData=RegData, valgtVar=valgtVar, outfile=outfile)
#}
valgtVar <- 'LegeInformasjonFrekvens'

NIRFigPrePostPaaror  <- function(RegData, valgtVar, outfile='')	
{
      
      RegData$VariabelGr <- RegData[ ,valgtVar]
      #RegData$VariabelGr[RegData$VariabelGr==-1] <- 9
      #RegData$VariabelGr[RegData$VariabelGr==6] <- 9
      RegData$VariabelGr[RegData$VariabelGr %in% c(-1,6)] <- 9
      
      gr <- c(1:5,8:9)
      RegData$VariabelGr <- factor(RegData$VariabelGr, levels=gr)
      #      -1:Velg verdi, 1:Fremragende, 2:Meget godt, 3:Godt, 4:Noenlunde, 5:Dårlig, 6:Ikke aktuelt	
      if (valgtVar %in% c('BehandlingHoeflighetRespektMedfoelelse', 
                          'BehandlingBesvarerHoeflighetRespektMedfoelelse','InformasjonsOverensstemmelseNum')) {
            grtxt <- c('Fremragende', 'Meget godt', 'Godt', 'Noenlunde', 'Dårlig', '','Ikke svart')
            
      }
     #--------HENT grtxt FRA KODEBOK   NIRkodebokPaarorSkjema.csv ? Manuelt først. Kjør løkke og spy ut resultater
      
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
      

      AndelerPP <- list(Pre=0, Post=0)
      NPre <- N$Pre
      NPost <- N$Post
      AndelerPP$Pre <- cbind(AggVerdier$Pre, AggVerdier$Post)

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









