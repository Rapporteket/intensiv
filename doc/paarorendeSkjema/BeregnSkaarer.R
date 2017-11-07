#Skåring
# Sp.mål 1-9 og 11-14 del 1 og 2-6 del 2 :
#       Fremragende   100
# Meget godt 	75
# Godt 		50
# Noenlunde 	25
# Dårlig 		 0
# 
# Spørsmål 10:
#       Svært ofte       100
# Ofte 		75
# Av og til	 	50
# Sjelden 		25
# Aldri 		 0
# 
# Svært ofte       100
# Ofte 		75
# Av og til	 	50
# Sjelden 		25
# Aldri 		 0
# 
# Sp.mål 7-9 del 2:
#       0 (fyrste/øvste/dårlegaste alternativ)
# 25
# 50
# 75
# 100 siste/nedste/beste alternativ)
# 
# Spm10
# Jeg kunne ha trengt mer tid: 0
# Jeg hadde tilstrekkelig med tid: 0
# 
# Sp.mål 11-13 del 2:
#       0 (fyrste/øvste/dårlegaste alternativ)
# 25
# 50
# 75
# 100 (siste/nedste/beste alternativ)








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
#Dette må senere gjøres om til et brukervalg, dvs. at brukeren kan velge hvilke perioder 
#som skal sammenlignes

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



library(plyr)
Del1Skaar <- paste0(Del1,'Skaar')
Del2Skaar <- paste0(Del2,'Skaar')
RegData[,c(Del1Skaar,Del2Skaar)] <- NA

#Standard: 1:5 -> 100:0
verdi5 <- c(100, 75, 50, 25, 0)

Spm <- c(Del1,Del2[1:6])
Skaar <- paste0(Spm,'Skaar')
for (nr in 1:length(Spm)) { RegData[,Skaar[nr]] <- mapvalues(RegData[ ,Spm[nr]], from = c(-1,1:6), to = c(NA,verdi5,NA))}

RegData[ ,Del2Skaar[10]] <- mapvalues(RegData[ ,Del2[10]], from = c(-1,1:2), to = c(NA,0,100))

Spm <- Del2[c(7:9,11:13)]
Skaar <- paste0(Spm,'Skaar')
for (nr in 1:length(Spm)) { RegData[ ,Skaar[nr]] <-  mapvalues(RegData[ ,Spm[nr]], from = c(-1,1:5), to = c(NA,rev(verdi5)))}

PaarorDataSkaar <- 'A:/Intensiv/PaarorDataSkaar.csv'
#write.table(RegData, file = PaarorDataSkaar, row.names=FALSE, sep = ';', fileEncoding = "UTF-8")



#--------------------------- Figurtilrettelegging og figur--------------------------------------

#--------------start her____________
rm(list=ls())
#Last inn PaarorDataSkaar, Del1 og Del2
PaarorData <- read.table(file=PaarorDataSkaar, header=T, sep=';',encoding = 'UTF-8')
library(intensiv)
RegData <- NIRPreprosess(RegData=PaarorData)


#variable <- c('BehandlingHoeflighetRespektMedfoelelseNum', 'BehandlingBesvarerHoeflighetRespektMedfoelelseNum','InformasjonsOverensstemmelseNum', 'LegeInformasjonFrekvensNum')
variable <- c(Del1, Del2)
setwd('C:/ResultattjenesteGIT/intensiv/doc/paarorendeSkjema/Figurer/')

for (valgtVar in variable) {
      outfile <- paste0(valgtVar, '.png')
      NIRFigPrePostPaaror(RegData=RegData, valgtVar=valgtVar, outfile=outfile)
}
valgtVar <- 'ForklaringForstaaelse'

NIRFigPrePostPaaror  <- function(RegData, valgtVar, outfile='')	
{
      
      #--------HENT grtxt FRA KODEBOK   NIRkodebokPaarorSkjema.csv ? 
	  #Kjører nå Manuelt. Kjør løkke og spy ut resultater
      
      if (valgtVar %in% c(Del1[c(1:5,7:9,11:12)],Del2[2:5])) {
	        # -1 = Velg verdi	1 = Fremragende	2 = Meget godt	3 = Godt	4 = Noenlunde	5 = Dårlig	6 = Ikke aktuelt
	        # 1 = Fremragende	2 = Meget godt	3 = Godt	4 = Noenlunde	5 = Dårlig	8:'', 9: = Ikke aktuelt
            grtxt <- c('Fremragende', 'Meget godt', 'Godt', 'Noenlunde', 'Dårlig', '','Ikke svart')	
      }
      if (valgtVar %in% c(Del1[c(6,13)],Del2[c(4,6)])) {
            # -1 = Velg verdi	1 = Fremragende	2 = Meget godt	3 = Godt	4 = Noenlunde	5 = Dårlig	6 = Ikke aktuelt
            # 1 = Fremragende	2 = Meget godt	3 = Godt	4 = Noenlunde	5 = Dårlig	8:'', 9: = Ikke aktuelt
            grtxt <- c('Fremragende', 'Meget god', 'Godt', 'Noenlunde', 'Dårlig', '','Ikke svart')	
      }
      if (valgtVar %in% c(Del1[10],Del2[1])) {
            #      -1:Velg verdi, 1:Svært ofte, 2: Ofte, 3: Av og til, 4: Sjelden, 5: Aldri, 6: Ikke relevant	
            # 1 = Fremragende	2 = Meget godt	3 = Godt	4 = Noenlunde	5 = Dårlig	8:'', 9: = Ikke aktuelt
            grtxt <- c('Svært ofte', 'Ofte', 'Av og til', 'Sjelden', 'Aldri', '','Ikke svart')	
      }
      if (valgtVar %in% Del1[14]) {
            #-1 = Velg verdi	1 = Svært fornøyd	2 = Meget fornøyd	3 = Middels fornøyd	4 = Ganske misfornøyd	5 = Svært misfornøyd	6 = Ikke relevant
            # 1 = Fremragende	2 = Meget godt	3 = Godt	4 = Noenlunde	5 = Dårlig	8:'', 9: = Ikke aktuelt
            grtxt <- c('Svært fornøyd', 'Meget fornøyd', 'Middels fornøyd', 
                       'Ganske misfornøyd', 'Svært misfornøyd', '','Ikke svart')	
      }

      tittel <- switch(valgtVar,
                       
                 BehandlingHoeflighetRespektMedfoelelse = c('Hvor godt ble pasienten ivaretatt', 
                                                               'med hensyn til høflighet, respekt og medfølelse?'),
                 SymptomSmerte = c('Hvor godt vurderte og behandlet intensivpersonalet ', 
                                   'symptomene til pasienten med hensyn til smerte'),
                 SymptomPustebesvaer = c('Hvor godt vurderte og behandlet intensivpersonalet ', 
                                         'symptomene til pasienten med hensyn til pustebesvær'),
                 SymptomUro = c('Hvor godt vurderte og behandlet intensivpersonalet ', 
                                'symptomene til pasienten med hensyn til uro'),
                 BehandlingBesvarerBehov	= 'Hvor godt viste intensivpersonalet interesse for dine behov?',
                 BehandlingBesvarerStoette = c('Hvor god var den følelsesmessige støtten', 
                                                      'som du fikk av intensivpersonalet?'),
                 BehandlingSamarbeid = c('Hvordan samarbeidet intensivpersonalet som ivaretok', 
                                         'og behandlet pasienten?'),
                 BehandlingBesvarerHoeflighetRespektMedfoelelse = c('Hvordan ble du møtt av intensivpersonalet', 
                                                                       'med hensyn til høflighet, respekt og medfølelse?'),
                 SykepleierOmsorg = 'Hvor godt synes du sykepleierne ivaretok pasienten?',
                 SykepleierKommunikasjon = 'Hvor ofte snakket sykepleierne med deg om pasientens tilstand?',
                 LegeBehandling = 'Hvor godt synes du legene ivaretok pasienten?',
                 AtmosfaerenIntensivAvd = 'Atmosfæren i intensivavdelingen var:',
                 AtmosfaerenPaaroerenderom = 'Atmosfæren på pårørenderommet/venterommet var:',
                 OmfangetAvBehandlingen =	c('Hvor tilfreds var du med nivå eller omfang av', 
                                    'pleie og behandling som pasienten fikk på intensivavdelingen?'),
                 LegeInformasjonFrekvens	= 'Hvor ofte snakket legene med deg om pasientens tilstand?',
                 SvarPaaSpoersmaal	= 'Hvor villig var intensivpersonalet til å svare på dine spørsmål?',
                 ForklaringForstaaelse = 'Hvor godt klarte intensivpersonalet å gi deg forklaringer som du forsto?',
                 InformasjonsAerlighet = c('Hvor ærlig synes du informasjonen du fikk', 
                                                      'om tilstanden til pasienten var?'),
                 InformasjonOmForloep = c('Hvor godt ble du informert om hva som skjedde med pasienten', 
                                                'og hvorfor ting ble gjort?'),
                 InformasjonsOverensstemmelse = c('Hvor stor overensstemmelse var det i informasjonen', 
                                                        'du fikk om tilstanden til pasienten?')
                       )
      
      
      
      if (valgtVar == 'BeslutningsInvolvering') { #Del2[7]
            #-1:5
      tittel <-  'Følte du deg involvert i beslutningsprosessen?'	
      grtxt <- c('veldig utelatt', 'noe utelatt', 'verken eller', 'noe involvert', 
                 'veldig involvert', '','Ikke svart') #paste0('Jeg følte meg ', )
      }
      if (valgtVar == 'BeslutningsStoette') { #Del2[8]
            #-1:5
            tittel <-  'Følte du at du fikk støtte når beslutningene ble tatt?'	
            grtxt <- c('ikke fikk støtte', 'fikk liten støtte', 'fikk en viss støtte', 
                       'fikk støtte', 'fikk mye støtte', '','Ikke svart') #paste0('Jeg følte at jeg ', )
      }
	
      if (valgtVar == 'BeslutningsKontroll') { #Del2[9]
            #-1:5
            tittel <- 'Følte du at du hadde innflytelse på den behandlingen som ditt familiemedlem fikk?'	
            grtxt <- c('helt uten innflytelse', 'liten innflytelse', 'verken eller', 
                 'en viss innflytelse', 'god innflytelse', '','Ikke svart')
      }
      if (valgtVar == 'BeslutningsTid') { #Del2[10]
            #-1:2
            tittel <- c('Når beslutninger skulle tas, hadde du tilstrekkelig med tid til ', 
                        'å uttrykke dine bekymringer og få besvart dine spørsmål?')
      grtxt <- c('trengte mer tid', 'tilstrekkelig med tid', '','Ikke svart')
      koder <- c(1,2,8,9)
      }
      if (valgtVar == 'LivsLengde') { #Del2[11], #-1:5
            tittel <- 'Hvilket utsagn beskriver best din oppfatning \nang. livet til pasienten:'
      grtxt <- c('unødvendig forlenget', 'forlenget litt lenger enn nødvendig', 
                 'passe', 'forkortet litt mer enn nødvendig', 'unødvendig forkortet')
      }
      if (valgtVar == 'LivssluttKomfor') { #Del2[12] #-1:5
            tittel <- 'Under de siste timene av livet til pasienten, 
                              \nhvilket utsagn beskriver best din oppfatning om hvordan han/hun hadde det:'
            grtxt <- c('ukomfortabelt', 'noe ukomfortabelt', 'stort sett komfortabelt',
                       'svært komfortabelt', 'fullstendig komfortabelt')
      }
      if (valgtVar == 'LivssluttStoette') { #Del2[13] #-1:5
           tittel <- 'Under de siste timene før pasienten døde, 
                              \nhvordan følte at du ble involvert beslutningsprosessen?'
            grtxt <- c('veldig utelatt', 'noe utelatt', 'verken eller', 'noe involvert', 'veldig involvert')
      }
      
      RegData$VariabelGr <- RegData[ ,valgtVar]
      #RegData$VariabelGr[RegData$VariabelGr==-1] <- 9
      #RegData$VariabelGr[RegData$VariabelGr==6] <- 9
      RegData$VariabelGr[RegData$VariabelGr %in% c(-1,6)] <- 9
      
      RegData$VariabelGr <- factor(RegData$VariabelGr, levels=c(1:(length(grtxt)-2),8:9))
      
      
      
      #Skal sammenlikne før og etter intervensjon. Definert i variabelen PrePost.
      #Gjør beregninger selv om det evt ikke skal vise figur ut. Trenger utdata.
      AggVerdier <- list(Pre = 0, Post =0)
      N <- list(Pre = 0, Post =0)
      Ngr <- list(Pre = 0, Post =0)
      TotSkaar <- list(Pre = 0, Post =0)
      ind <- list(Pre = which(RegData$PrePost==1), Post = which(RegData$PrePost==2))
      
      Ngr$Pre <- table(RegData$VariabelGr[ind$Pre])
      N$Pre <- sum(Ngr$Pre)	#length(ind$Pre)- Kan inneholde NA
      AggVerdier$Pre <- 100*Ngr$Pre/N$Pre
      
      Ngr$Post <- table(RegData$VariabelGr[ind$Post])
      N$Post <- sum(Ngr$Post)
      AggVerdier$Post <- 100*Ngr$Post/N$Post
      #Gjennomsnittsskårer
      varSkaar <- paste0(valgtVar,'Skaar')
      TotSkaar$Pre <- sprintf('%.1f', mean(RegData[ind$Pre,varSkaar], na.rm = T))
      TotSkaar$Post <- sprintf('%.1f', mean(RegData[ind$Post,varSkaar], na.rm = T))
      
      
      
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
      legend('top', c(paste0('Før intervensjon, N=', NPre), paste0('Gj.sn. skår: ',TotSkaar$Pre),
                      paste0('Etter intervensjon, N=', NPost), paste0('Gj.sn. skår: ',TotSkaar$Post)), 
             bty='n', fill=farger[c(3,NA,1,NA)], border=NA, ncol=2, cex=cexleg)
      
      
      title(tittel, font.main=1)	#line=0.5, 
      
      par('fig'=c(0, 1, 0, 1)) 
      if ( outfile != '') {dev.off()}
}









