NIRFigPrePostPaaror  <- function(RegData, valgtVar, outfile='')	
{
      
      #--------HENT grtxt FRA KODEBOK   NIRkodebokPaarorSkjema.csv ? 
      #Kjører nå Manuelt. Kjør løkke og spy ut resultater
      
      if (valgtVar %in% c(Del1[c(1:5,7:9,11)],Del2[2:5])) {
            # -1 = Velg verdi	1 = Fremragende	2 = Meget godt	3 = Godt	4 = Noenlunde	5 = Dårlig	6 = Ikke aktuelt
            # 1 = Fremragende	2 = Meget godt	3 = Godt	4 = Noenlunde	5 = Dårlig	8:'', 9: = Ikke aktuelt
            grtxt <- c('Fremragende', 'Meget godt', 'Godt', 'Noenlunde', 'Dårlig', '','Ikke svart')	
      }
      if (valgtVar %in% c(Del1[c(6,12,13)],Del2[c(4,6)])) {
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

