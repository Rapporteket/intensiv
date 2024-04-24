---
title: "Akutt bukkirurgioperasjoner før/etter utvidelse av aktuttstuetilbud"
author: "Lena"
date: 
output:
  word_document: 
    keep_md: yes
  html_document: default
  pdf_document: default
editor_options:
  chunk_output_type: console
---




## Sammendrag

Heile oppgåva/kvalitetssikringsprosjektet dreiar seg om å samanlikne to toårs-kohortar av akuttinngrep på gastro(mage-tarm)-seksjonen på Haukeland Sjå vedlagde protokoll, som vi ikkje kjem til å fylgje opp 100%. Alle samanlikningar der det ikkje er fullstendige data, skal gjerast med utgangspunkt i dei data vi har (n vil altså varierer etter kva vi ser på).
Målet er ein artikkel i tidsskriftet BMJ Quality & Safety.

Gastrokirurgiske pasientar, toårs-kohortar: 

 *Kohort 1 2018-2019*: 
 
- Dei som gjennomgjekk akutt bukkirurgi i tidsrommet 01.01.2018-31.12.2019
- inngrep/pasientar før tilgang på ei ekstra akuttstove på dagtid som rutine på måndag-fredager
- totalt 1199 inngrep, og 959 pasientar

*Kohort 2, 1.4.2020-31.03.2022* 

- Dei som gjennomgjekk akutt bukkirurgi i tidsrommet 01.04.2020-31.03.2022
- inngrep etter slik tilgang var etablert.
- 1292 inngrep og 1002 pasientar.

 Hovudpoenget er at det fram til februar 2020 var avgrensa tilgang til "akuttstove" på dagtid, dvs. at det var avsett eit eige team til å ta seg av akuttoperasjonar på dagtid berre kring 2 dagar/veke.
Frå februar 2020 vart dette tilbodet utvida til fem dagar/veke (måndag til fredag).


- Fil på pasientnivå - kun siste inngrep
- Fil med alle inngrep 




 Vi ynskjer å sjå om vi finn att effekt av denne endringa, og undersøkjer ein del variablar som har med logistikk å gjere, eks: 
 - om fraksjonen av operasjonar på dagtid vs. på vakttid er blitt endra, 
 - om fleire pasientar vert opererte innan den tidsfristen kirurgen ynskjer seg
 - ...
 og også nokre variablar som har med utfall å gjere:
- operajonstid, 
- overleving 7, 30 og 365 dagar, 
- grad av blødning, 
- førekomst av reoperasjonar innan 7 døger
 
I tillegg må vi samanlikne kohortane for ein del basalkarakteristika: kjønn, alder, "funksjonsklasse" gradert som ASA.


## Deskriptiv statistikk av pasientpopulasjonene
Ønsker først å gjere ei case-mix-samanlikning av pasientane,  (dvs. dei 959 mot dei 1002).



### Alder og kjønn
Resultater basert på pasienter
Sammenligne fordeling, aldersgrupper:

Andel menn i de to gruppene er hhv. 53.1 %, 49.9 %, p-verdi: 0.17


![](StudOppg_files/figure-docx/sign Alder-1.png)<!-- -->

```
## Warning in ks.test.default(x = as.numeric(Pas$Alder[indPasFor]), y =
## as.numeric(Pas$Alder[indPasEtter])): p-value will be approximate in the
## presence of ties
```


- Før: Median: 62  Gjennomsnitt: 59.1, SD: 19.5
- Etter: Median: 63  Gjennomsnitt: 59.8, SD: 19.5
- p-verdi fra t-test, gjennomsnitt: 0.1
- p-verdi fra Wilcox-test, median: 0.08

Basert på test av gjennomsnitt og median, er det forskjell i alder mellom før og etter, hvis vi velger signifikansnivå 10%.

Vi ser at det er høyere andel av eldre i gruppa etter intervensjon. Hvis vi tester om andelen over 70 år er forskjellig (kji-kvadrat test), 
får vi en p-verdi på 0.0716982
Hvis vi tester om andelen av 70+åringer er høyere etter (ensidig test), får vi en p-verdi på 0.0358491.

Hvis vi tester selve aldersfordelingene med Kolmogorov-Smirnovs test, får vi en p-verdi på 0.7580881


**ASA-klassefordeling** 
Resultater basert på inngrep.

```
## Warning in ks.test.default(x = Inngr$ASAnum[indInngrFor], y =
## Inngr$ASAnum[indInngrEtter]): p-value will be approximate in the presence of
## ties
```



|                                                                      |  gr1|  gr2|
|:---------------------------------------------------------------------|----:|----:|
|ASA-klasse 1 Frisk pasient                                            | 10.2|  9.0|
|ASA-klasse 2 Pasient med lettere grad av systemsykdom                 | 33.4| 36.1|
|ASA-klasse 3 Pasient med alvorlig, begrensende grad av systemsykdom   | 41.5| 38.9|
|ASA-klasse 4 Pasient med livstruende alvorlighetsgrad av systemsykdom | 12.0| 13.4|
|ASA-klasse 5 Moribund pasient                                         |  0.3|  0.5|
|NULL                                                                  |  2.7|  2.2|

 

*Test:* Ved Kolmogorov-Smirnovs test kan man teste om to utvalg er fra samme fordeling, noe som gir en p-verdi på 0.9952827
Basert på dette kan vi konkludere at det ikke er forskjell i ASA-grad.

(For å verifisere metoden, gjorde jeg tilsvarende test for ASA-gradverdiene fra artikkelen og kom ut med omtrent samme p-verdi som de fikk i artikkelen.)


## Resultater 
Ynskjer å samanlikna dei to kohortane av **inngrep**.


#### Mors (døde)
Resultater basert på pasienter siden man dør bare en gang.


*Mors* fortel om pasienten er registrert som død (=1) då vi gjorde datauttrekka. 


- Kan du også sjekke om det er skilnad i mortalitet etter 7, 30 og 365 dagar i dei to kohortane når det gjeld aldersgruppene? Her er vi ute etter om særleg dei eldre og presumptivt mekir skrøpelege har fått endra overleving etter innføring av akuttstove (det ser førebels ut for oss som at totalmortaliteten og også ASA-skåre har auka frå kohort 1 til 2.

Andel (%) døde etter 7, 30 og 365 dager i ulike aldersgrupper.


          gr1    gr1    gr1   gr2    gr2    gr2
-------  ----  -----  -----  ----  -----  -----
18-39     0.0    0.5    1.5   1.1    1.1    2.7
40-49     0.9    0.9    3.7   2.7    2.7    6.4
50-59     0.7    0.7    9.6   0.7    4.2   15.5
60-69     2.2    6.2   19.7   5.1    7.4   18.3
70-79     3.8   10.2   25.3   3.2   11.5   24.4
80+       9.6   16.4   36.3   9.3   16.3   34.9
Totalt    2.8    5.9   16.3   3.8    7.7   17.9



Det ser ut til at mortaliteten er litt høyere etter, for de under 60, mens den er bittelitt lavere for de >60. Totalt sett litt høyere.
Jeg har gjort test ved de aktuelle tidspunktene (7, 30 og 365 dager) og det er ikke signifikant forekjell for noen av tidspunktene.

*Reidar: Her er det svært interessant for oss å sjå på om skilnader i mortalitet kan knytast til
aldersfordeling. Alle ser jo det som er openbert (og forventa), at mortaliteten stig med
stigande alder, både i gr. 1 og gr.2. Sidan vi finn litt høgare totalmortalitet i gr.2, blir
spørsmålet:
1. Kan dette forklarast med at det er ein større andel i dei høge aldersgruppene i gr.2?
2. Kan dette også hengje saman med ASA-klasse (som jo ei ei slags risikovurdering,
dess høgare ASA, dess høgare risiko)?
3. Det er kanskje komplisert, men ideellt sett skulle vi samanlikna mortalitet før og
etter (altså i gr. 1 og gr. 2) og samstundes korrigert for alder og ASA-klasse
Vi har ikkje rekna med at det skal vere tydelege skilnader i mortalitet, men det er i
utgangspunktet litt overraskande at totalmortalitet er litt høgare i gr. 2. Samstundes kan ei
høgst plausibel forklaring vere at det er auka tal på inngrep på «dårlege»
pasientar/høgrisikopasientar i gr. 2. (det er jo noko høgare totaltal i gr. 2). Pasientar med
høg alder/høg ASA-klasse/høg risiko er noko ein vil unngå å operere på vakttid – og ofte
kan det hende ein avstår frå kirurgi, fordi ein trur pasienten har lite å hente på å bli operert
i det heile. Dersom det er auka kapasitet på dagtid, kan det vere at ein i større grad likevel
prøver seg på slike «tvilsame inngrep». Det kan også forklare den auka gjennomsnittlege
operasjonstida i gr. 2.*

Jeg har gjort overlevelsesanalyse justert for alder og ASA. Se neste avsnitt.

## Overlevelsesanalyser
Resultater basert på pasienter.

Har beregnet total eksponeringstid i studien for pasienter som har overlevd. Dette basert på differansen mellom operasjonsdato og uttrekksdato (14.09.2023).

![](StudOppg_files/figure-docx/Overlevelse-1.png)<!-- -->

```
## Loading required namespace: riskRegression
```

```
## Loading required namespace: pammtools
```

![](StudOppg_files/figure-docx/Overlevelse-2.png)<!-- -->


I det første plottet ser vi det vi har observert, dvs. at mortaliteten er en anelse høyere i gruppe to.

Det andre plottet viser overlevelseskurvene (inkl. konfidensintervall) etter justering for alder og ASA. Justeringa gjør at kurvene bytter plass, dvs. at overlevelsen
er litt bedre etter, men ingen signifikant forskjell.


### Prioritet ("Valgt hastegrad")
Resultater basert på inngrep.

- **Prioritet** = Valgt hastegrad, dvs. kor raskt kirurgen har ynskt inngrepet gjort etter at det er meldt inn. 

Ynsker vi å sjå om det er skilnader mellom kohort 1 og 2 når det gjeld dei ulike prioriteringskategoriane

![](StudOppg_files/figure-docx/Prioritet-1.png)<!-- -->



|    | 0-1t| 1-6t| 6-24t| 24-72t| 72+t|
|:---|----:|----:|-----:|------:|----:|
|gr1 |  6.6| 41.0|  41.1|   10.3|  1.0|
|gr2 |  7.0| 46.3|  37.1|    9.1|  0.5|



Flere får prioritet 0-6t gruppe 2. Skyldes høyere sykelighet…?
*Reidar: Kanskje. Men det kan også
vere «uynskt» ved at kirurgane veit at det har vore litt større høve til å få pas. fort, og
dermed kan ha vorte freista til å setje høgare prioritet (dvs. kortare tidsfrist) for å få
inngrepet gjort. Er skilnadene her signifikante?*

Det er signifikant høyere andel som prioriteres til 0-6t etter kapasitetsøkninga, sammenlignet med før. P-verdi, ved tosidig test: 0.0049775

### Operert innen gitt prioriteringstid
 **Opr innan prio** Eit 1-tal her tyder at inngrepet er starta innan den tidsfristen kirurgen ynskte. Vi ynskjer å samanlikne om det totalt sett er skilnad i graden av oppfylling av tidsfristar, dvs. fraksjonen av inngrep (med talet 1 i kolonne I) mellom dei to kohortane. 


Andel som opereres innen fristen, hhv. før og etter: 0.6780651, 0.7004644 
Totalt sett opereres ca 2%-poeng flere innen fristen.
p-verdi: 0.2443879

*Reidar: Er det signifikante endringar for dei med kortast tidsfrist, 
dvs. under 2 og under 6 timar?
Det er fyrst og fremst her ein kan forvente ei viss betring ved større tilgang på akuttstove.*

Ja, hvis vi ser på endring for pasienter med kort tidsfrist, dvs. "innen 6 timer", har vi ei signifikant endring:
Andel som opereres innen 0-6t hhv. før og etter: 
36.4 %, 41.3 %, med p-verdi: 0.0156409

Dette er vel et relativt sterkt funn siden vi både har signifikant flere som får prioritet 0-6t OG det er signifikant flere som får denne fristen innfridd.

#### Knivtid
Resultat basert på inngrep.

Knivtid = den tida kirurgen brukte på inngrepet i minuttar. 
Vi ynskjer å samanlikne gjennomsnitta og medianar i kohort 1 og kohort 2 her.



- Før: Median: 73  Gjennomsnitt: 85.6, interkvartil: 35 - 122
- Etter: Median: 81  Gjennomsnitt: 94.6, interkvartil: 40 - 132
- p-verdi for forskjell mellom gjennomsnittlig knivtid: 8\times 10^{-4}
- p-verdi for forskjell mellom median knivtid: 0.0045

#### Blødning i ml 
Analyse basert på inngrep.

– kor mykje blødning som er registrert i løpet av inngrepet. Dette er skeivfordelte data, så er det best å samanlikne median/interkvartilar?



- Før: Median: 50  Gjennomsnitt: 132.7, interkvartil: 4 - 100
- Etter: Median: 40  Gjennomsnitt: 165.6, interkvartil: 5 - 100
- p-verdi for forskjell mellom gjennomsnittlig blødning: 0.2107
- p-verdi for forskjell mellom median blødning: 0.9809


#### Andel akuttinngrep på dagtid
Analyse basert på inngrep.

Oppstart på dagtid har eit 1-tal i variabelen «Dagtid». Det skal då vere 0 i "helg-helgedag", dvs. laurdag/sundag og bevegelege heilagdagar).
Kolonne «Kveld» kan vere 1 samstundes som det er 1 i kolonne «Dagtid» - då er det eit inngrep som strekkjer seg over både dag og kveld/natt.  
Dersom det er 1 i kolonne «Kveld» utan at det er 1 i «Dagtid», er inngrepet starta på vakttid og ikkje dagtid.
*Ut fra dette forstår jeg at man bare trenger å se på variabelen Dagtid når man ser på operasjoner gjort på dagtid*



Kor stor andel av desse akuttinngrepa ble utført på
kvardagsdøger måndag-fredag (akuttstove tilgjengeleg kvar dag for kohort 2, og meir sporadisk for kohort 1)


|    |    0|    1|
|:---|----:|----:|
|gr1 | 26.7| 73.3|
|gr2 | 25.1| 74.9|



Kor stor andel av desse akuttinngrepa på ble utført på dagtid (dvs. vart starta på dagtid)? 
Basert på variabel *Dagtid*:


|    |    0|    1|
|:---|----:|----:|
|gr1 | 65.1| 34.9|
|gr2 | 59.5| 40.5|


p-verdi: 0.0043987


#### Totaltid
Analyse basert på inngrep.

- Før: Median: 188  Gjennomsnitt: 196.7, interkvartil: 131.5 - 248
- Etter: Median: 197  Gjennomsnitt: 207.6, interkvartil: 136 - 265
- p-verdi for forskjell mellom gjennomsnittlig totaltid: 0.0023
- p-verdi for forskjell mellom median totaltid: 0.0153


#### Liggjetid 
Analyse basert på inngrep.

Liggjetid på sjukehus = totalt tal på døger innlagt. Dette er også ein skeivfordelt variabel som vi gjerne vil samanlikne mellom dei to kohortane. Variabel: *Liggedager*



- Før: Median: 10  Gjennomsnitt: 19, interkvartil: 5 - 23
- Etter: Median: 10  Gjennomsnitt: 16.5, interkvartil: 4 - 21
- p-verdi for forskjell mellom gjennomsnittlig liggetid: 0.0065
- p-verdi for forskjell mellom median liggetid: 0.1109


#### Reoperasjoner
Basert på inngrep


*Reidars tall: 12,3 (147) i gr 1 og 13,3 (172)*
Prosent reoperasjoner innen 7 dager: Før: 12.3, Etter: 13.3, p-verdi: 0.4681811

(Jeg har ikke lagt til reoperasjoner i pasientfila.)
