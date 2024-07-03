
```{r Tall til tekst, Nord, include=FALSE}
KjonnPst_Nord <- paste(sprintf('%.1f', prop.table(table(RegData20_21$erMann))*100), '%')
N20_21_Nord <- dim(RegData20_21)[1]
under18_Nord <- sprintf('%.1f', 100*sum(RegData20_21$Alder<18)/N20_21_Nord)
over80_Nord <- sprintf('%.1f', 100*sum(RegData20_21$Alder > 80)/N20_21_Nord)
medAlder_Nord <- median(RegData$Alder, na.rm = T)
medLiggetid_Nord <- median(RegData20_21$liggetid, na.rm = T)

respStotte_Nord <- sprintf('%.1f', 100*sum(RegData20_21$MechanicalRespirator==1)/N20_21_Nord)

iLiveUt_Nord <- sprintf('%.1f', 100*sum(RegData20_21$DischargedIntensiveStatus==0)/N20_21_Nord) #0: I live, 1: Død intensiv
iLive30d_Nord <- sprintf('%.1f', 100*sum(RegData20_21$Dod30==0)/N20_21_Nord)

alderDIV_Nord <- summary(RegData20_21$Alder)[1:6]
names(alderDIV_Nord) <- c('Minimum:', '25% av pasientene er under:', 'Median:','Gjennsomsnitt:',
                          '25% av pasientene er over:', 'Maksimum:')
```

(NB: Tromsø registrerte ikke data i 2019)

For perioden 2020-21 var `r KjonnPst_Nord[1]` av intensivpasientane kvinner og `r KjonnPst_Nord[2]` menn. Median alder var `r medAlder_Nord`. Pasienter over 80 år stod for `r over80_Nord` % av alle intensivopphold, og barn under 18 år stod for `r under18_Nord` % av alle intensivopphold. Median liggjetid var `r medLiggetid_Nord` døger. Andel pasienter som fekk respiratorstøtte var `r respStotte_Nord` %. Ved `r iLiveUt_Nord` % av oppholdene på intensiv ble pasientene utskrivne fra intensiv i live, og `r iLive30d_Nord` % var i live 30 dagar etter innleggelse på intensiv.


```{r Tall til tekst, Hele landet, include=FALSE}
KjonnPst <- paste(sprintf('%.1f', prop.table(table(RegDataLandet20_21$erMann))*100), '%')
N20_21 <- dim(RegDataLandet20_21)[1]
under18 <- sprintf('%.1f', 100*sum(RegDataLandet20_21$Alder<18)/N20_21)
over80 <- sprintf('%.1f', 100*sum(RegDataLandet20_21$Alder > 80)/N20_21)
medAlder <- median(RegDataLandet20_21$Alder, na.rm = T)
medLiggetid <- median(RegDataLandet20_21$liggetid, na.rm = T)

respStotte <- sprintf('%.1f', 100*sum(RegDataLandet20_21$MechanicalRespirator==1)/N20_21)

iLiveUt <- sprintf('%.1f', 100*sum(RegDataLandet20_21$DischargedIntensiveStatus==0)/N20_21) #0: I live, 1: Død intensiv
iLive30d <- sprintf('%.1f', 100*sum(RegDataLandet20_21$Dod30==0)/N20_21)

alderDIV <- summary(RegDataLandet20_21$Alder)[1:6]
names(alderDIV) <- c('Minimum:', '25% av pasientene er under:', 'Median:','Gjennsomsnitt:',
                     '25% av pasientene er over:', 'Maksimum:')

```

For perioden 2020-21 var `r KjonnPst[1]` av intensivpasientane kvinner og `r KjonnPst[2]` menn. Median alder var `r medAlder`. Pasienter over 80 år stod for `r over80` % av alle intensivopphold, og barn under 18 år stod for `r under18` % av alle intensivopphold. Median liggjetid var `r medLiggetid` døger. Andel pasienter som fekk respiratorstøtte var `r respStotte` %. Ved `r iLiveUt` % av oppholdene på intensiv ble pasientene utskrivne fra intensiv i live, og `r iLive30d` % var i live 30 dagar etter innleggelse på intensiv.



```{r Covid-pasienter Nord, include=FALSE}

KjonnPstCov <- paste(sprintf('%.1f', prop.table(table(OpphCov$erMann))*100), '%')
N <- dim(OpphCov)[1]
medAlderCov <- round(median(OpphCov$Alder, na.rm = T), 1)
under18Cov <- sprintf('%.1f', 100*sum(OpphCov$Alder<18)/N)
over80Cov <- sprintf('%.1f', 100*sum(OpphCov$Alder > 80)/N)
medLiggetidCov <- round(median(OpphCov$Liggetid, na.rm = T), 1)

```

For pasienter med covid-19 er det registrert `r dim(OpphCov)[1]` intensivopphold fordelt på `r length(unique(OpphCov$PersonId))` pasienter i 2020. Andel opphold med kvinneleg pasient var `r KjonnPstCov[1]` og del opphold med mannleg pasient var `r KjonnPstCov[2]`. Median alder var `r medAlderCov` år og median liggetid på intensiv var `r medLiggetidCov` døgn. For `r over80Cov` % av oppholdene var alder ved innlegging på intensiv 80 år eller høyere, mens alder var under 18 år i `r under18Cov` % av oppholdene.\

**Intensivopphold i 2020 og 2021 hvor pasienten er innlagt med Covid-19, hele landet**\

```{r Covid-pasienter hele landet, include=FALSE}

KjonnPstCov <- paste(sprintf('%.1f', prop.table(table(OpphCovLandet$erMann))*100), '%')
N <- dim(OpphCovLandet)[1]
medAlderCov <- round(median(OpphCovLandet$Alder, na.rm = T), 1)
under18Cov <- sprintf('%.1f', 100*sum(OpphCovLandet$Alder<18)/N)
over80Cov <- sprintf('%.1f', 100*sum(OpphCovLandet$Alder > 80)/N)
medLiggetidCov <- round(median(OpphCovLandet$Liggetid, na.rm = T), 1)

```

For pasienter med covid-19 er det registrert `r dim(OpphCov)[1]` intensivopphold fordelt på `r length(unique(OpphCov$PersonId))` pasienter i 2020. Andel opphold med kvinneleg pasient var `r KjonnPstCov[1]` og del opphold med mannleg pasient var `r KjonnPstCov[2]`. Median alder var `r medAlderCov` år og median liggetid på intensiv var `r medLiggetidCov` døgn. For `r over80Cov` % av oppholdene var alder ved innlegging på intensiv 80 år eller høyere, mens alder var under 18 år i `r under18Cov` % av oppholdene.\
\
