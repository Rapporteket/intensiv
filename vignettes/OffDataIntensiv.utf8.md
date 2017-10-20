---
title: "Offentlige data, intensivregisteret"
author: "Lena Ringstad Olsen"
date: "2017-08-17"
#output_format: pdf_document
output_file: 'C:/ResultattjenesteGIT/intensiv/vignettes/OffDataIntensiv.pdf'
output:
  bookdown::pdf_document2: 
      keep_tex: yes
      fig_caption: true
encoding: 'UTF-8'
knit_root_dir: 'C:/ResultattjenesteGIT/intensiv'
---


## Bakgrunn
Intensivregisteret ønsker å offentliggjøre kvalitetsindikatorer fra registeret. Man ønsker også å kunne oppdatere kvalitetsindikatorene jevnlig, samt at det skal være mulig å se på kvalitetsindikatorer for ulike utvalg som kjønn, aldersgrupper osv. Hvis også datagrunnlaget skal være tilgjengelig, må dataene være anonyme.

## Kvalitetsindikatorer, Intensivregisteret

Intensivregisteret har definert tre kvalitetsindikatorer fra registeret:  
* Reinnleggelse til intensiv innen 72 timar. Mål:  <4%  
* Median respiratortid, invasiv respiratorstøtte. Mål: <2,5døgn (Tilsvarer:Andel med respiratortid < 2,5 døgn. Mål: >50%)  
* Standardisert mortalitetsratio (SMR). Mål: < 0,7  

Både reinnleggelse og respiratortid kan representeres som indikatorvariable (representeres med en 01-variabel). SMR er observert dødelighet (innen 30 dager) ift. estimert dødelighet (standardisert SAPSII-skår). Estimert dødelighet kan ikke representeres med en indikatorvariabel. Variabelen SMR har heller ikke god nok kvalitet, i følge registerledelsen, og vil derfor ikke bli benyttet som offentlig kvalitetsindikator.

## Anonymisert datasett

Et datasett er ikke anonymt hvis registreringene er bakeveisidentifiserbare, dvs. at man gjennom opplysningene som gis og kunnskap om den aktuelle personen, kan identifisere denne og skaffe seg nye opplysninger om personen. For at man skal få noen ny opplysning om en person, må man på forhånd vite at denne var innlagt på intensiv og at oppholdet er relevant for å bli med i kvalitetsindikatoren. Den nye personopplysninga vil da være om oppholdet oppfylte kvalitetsindikatoren eller ikke. F.eks. om personen ble reinnlagt eller ikke. Men selv om man tror man har identifisert en person, kan man ikke vite det sikkert om opplysninga gjelder den aktuelle personen siden registert ikke har 100% dekningsgrad.  

Det er en akseptert tommelfingerregel at data kan betraktes som anonyme hvis enhver gruppering av dataene inneholder minst 5 observasjoner. Tidligere var det akseptert å benytte minst 3 som nedre grense for at data skal være anonyme. Hvis alle personene i ei gruppe har samme resultat for den aktuelle variablene, får man samme informasjon om en enkeltperson uansett hvor mange det er i gruppa. 


Det anonyme datasettet inneholder noen utvalgs-/grupperingsvariable, samt kvalitetsindikatoren det skal vises resultat for. Innholdet i kvalitetsindikatoren er kun 0 og 1-verdier. (Eks. 1 hvis reinnlagt, 0 hvis ikke).
Datasettet inneholder  hendelsesentydige data (ett opphold er en hendelse), dvs. vi har ei rad for hver hendelse. Dette for at vi skal kunne gjøre ulike utvalg og grupperinger av dataene. Alle rader hvor ei gruppering på laveste nivå av utvalgs-/grupperingsvariablene gir færre enn 5 hendelser, er fjernet fra datafila. Dette medfører at vi mister noen observasjoner. Hvor mye resultatet påvirkes av manglende observasjoner, avhenger av hvor mye data vi må sensurere. Hvor mye som må sensureres avhenger av størrelsen på registeret (totalt antall observasjoner) og hvor mange utvalgsparametre man ønsker.

Hvis en kvalitetsindikator er en andel, trenger man bare en indikatorvariabel for kvalitetsindikatoren, samt de variable man ønsker å gruppere på eller å gjøre utvalg for.
Eksemplet under viser hendelsesentydige data for  en kvalitetsindikatorer fra intensivregisteret. Kvalitetsindikatoren er gitt i kolonna *Variabel*.


```
##   Aar Kvartal erMann           ShNavn ShType Variabel AldersGr
##  2016       3      1  Haukel. Kir int      3        0    18-79
##  2016       1      0            Hamar      1        1    18-79
##  2016       4      1     RH Gen Int 2      3        1    18-79
##  2016       2      0  Haukel. Kir int      3        0    18-79
##  2016       2      1  Ullevål Kir int      3        1    18-79
##  2016       1      1      Lillehammer      2        0    18-79
##  2016       1      1            Hamar      1        1    18-79
##  2016       4      1            Hamar      1        1    18-79
##  2016       2      1 RH Barneintensiv      3        0     0-17
##  2016       2      0      KalnesØstf.      2        0    18-79
##  2016       2      0  Haukel. Kir int      3        1    18-79
##  2016       4      1 Tromsø Kir. int.      3        1    18-79
##  2016       1      1 RH Barneintensiv      3        1     0-17
##  2016       3      0 Tromsø Kir. int.      3        0    18-79
##  2016       2      1         Tønsberg      2        1    18-79
```




## Sensurering

I 2016 hadde Intensivregisteret 14358 registreringer fordelt på 48 enheter. Totalantallet for de ulike kvalitetsindikator vil være ulikt siden ikke alle opphold er relevant for alle kvalitetsindikatorer. Eksempelvis er det totalt 5608 opphold for 2016 som inngår i kvalitetsindikatoren reinnleggelse.

Eksemplet under viser hvor stor andel av registreringene vi må sensurere for kvalitetsindikatoren reinnleggelse basert på hvor mange utvalgsparametre vi ønsker, og om vi baserer sensuren på <5 eller <3 registreringer i hver gruppe.
Antall registreringer som må fjernes, øker raskt når vi inkluderer flere (grupperings-/utvalgs)variable.





Gruppering                                | N<5           | N<3
------------------------------------------|---------------|-----------  |
Enhet, kjønn:                             | 0.3   | 0.2|
Enhet, kjønn, aldersgrupper               | 2.2   | 1.0|
Enhet, måned, kjønn                       | 15.2| 5.7 |
Enhet, kjønn, aldersgruppe, innmåte, død  | 14.3   | 7.1 |
Enhet, kvartal, kjønn, aldersgruppe       | 12.4| 5.6 |


Vi ser at man mister relativt mye mer hvis man sensurerer alle grupper med <5 observasjoner, kontra å sensurere grupper med <3 observasjoner. Tidligere var det vanlig å sensurere <3.

Intensivregisteret har rundt 15000 registreringer i året. Det er andre registre som har nesten like mange avdelinger, men langt færre registreringer. (Eksempelvis Korsbånd med rundt 1700 registreringer.) I tabellen under har vi derfor gjort simuleringer for et register med 2000 registreringer og like mange avdelinger som intensivregisteret for å se på effenten av sensur.





Gruppering                                | N<5           | N<3
------------------------------------------|---------------|-----------  |
Enhet, kjønn:                             | 2.0   | 1.0|
Enhet, kjønn, aldersgrupper               | 5.5   | 3.8|
Enhet, måned, kjønn                       | 35.9| 24.2 |
Enhet, kjønn, aldersgruppe, innmåte, død  | 19.4   | 11.1 |
Enhet, kvartal, kjønn, aldersgruppe       | 23.6| 12.4 |


## Anonymt datasett og grad av sensur
For intensivregisteret har vi valgt å benytte utvalgs-/grupperingsvariablene:

* Enhet (sykehusavdeling/-enhet)
* Sykehustype (region- eller lokal-/sentralsykehus
* Alder (Tre aldersgrupper: 0-17, 18-79, 80+)
* Kjønn (mann/kvinne)
* År og kvartal (tidsangivelse for innleggelsestidspunkt)

*Reidar: Disse har jeg valgt, men du må si hvilke utvalg det er mest aktuelt å prioritere? *

Alle registreringer som faller i ei gruppe hvor det blir <5 observasjoner i gruppa er sensurert F.eks. hvis utvalgskombinasjonen: Sykehus A, aldersgruppe 2, kvinne, 2.kvartal 2016 inneholder færre enn 5 observasjoner, sensureres alle observasjonene i denne gruppa. Som vi ser fra tabellen med grad av sensur for intensivregisteret, betyr det at 12.4 \% av registreringene er sensurert. Figurene under viser resultatene for kvalitetsindikatoren respiratortid for hhv fullstendig og sensurert datasett.

Det er viktig å huske at graden av sensur vil være forskjellig for de ulike enhetene. Små enheter vil få sensurert relativt mer av sine opphold slik at resultatene for de små enhetene vil påvirkes mye mer enn for de store enhetene. Vi bør derfor ha en lav grense for hvor stor grad av sensur som er akseptabelt. I figurene under ser vi at f.eks. resultatet for Bærum endres fra 52,6% til 45,5%. Det betyr at med det fullstendige datasettet har de oppfylt målet for kvalitetsindikatoren (50%), mens i det sensurerte datasettet har de ikke det. Figur \@ref(fig:FigRespTid)


![(\#fig:FigRespTid)\label{fig:fig1}This is a caption](C:\Users\lro2402unn\AppData\Local\Temp\RtmpaYdrbO\preview-1e305c521aa8.dir\OffDataIntensiv_files/figure-latex/FigRespTid-1.pdf) ![(\#fig:FigRespTid)\label{fig:fig1}This is a caption](C:\Users\lro2402unn\AppData\Local\Temp\RtmpaYdrbO\preview-1e305c521aa8.dir\OffDataIntensiv_files/figure-latex/FigRespTid-2.pdf) 

En kvalitetsindikator hvor hendelsene er relativt få (reinnleggelser) vil kunne påvirkes mer av sensur enn når hendelsene er hyppige (respiratortid < 2,5 døgn). Figuren under viser resultater for kvalitetsindikatoren reinnleggelse. Total andel sensur er lavere for reinnleggelse siden de aller fleste opphold kan vurderes for reinnleggelse og følgelig alle registreringer er med i totalantallet (N). Andelen sensur for reinnleggelse er 3.6 \%. 
For Bodø ser vi at resultatet endres fra 6,4% reinnleggelse basert på det fullstendige datasettet, mens de har 10,7% reinnleggelse om vi ser på det sensurerte datasettet.


![](C:\Users\lro2402unn\AppData\Local\Temp\RtmpaYdrbO\preview-1e305c521aa8.dir\OffDataIntensiv_files/figure-latex/FigReinn-1.pdf)<!-- --> ![](C:\Users\lro2402unn\AppData\Local\Temp\RtmpaYdrbO\preview-1e305c521aa8.dir\OffDataIntensiv_files/figure-latex/FigReinn-2.pdf)<!-- --> 

## Konklusjon

Det er vanskelig å sette noen absolutt grense for hvor stort frafall som er akseptabelt i forhold til om man kan vise resultatene. Frafallet vil kunne påvirke ulikt fra gang til gang avhengig av innhold i dataene. Grensa bør settes lavt. Man bør også vurdere om det i det hele tatt er nødvendig å sensurere data. Subsidiært om det ikke holder å sensurere ved <3 observasjoner i ei gruppe.

(Det vil også være mulig å ikke vise resultater for enkeltsykehus som får høy grad av sensur, men det er relativt arbeidskrevende...)

