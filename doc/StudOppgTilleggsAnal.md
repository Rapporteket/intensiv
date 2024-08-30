---
title: "Akutt bukkirurgioperasjoner før/etter utvidelse av aktuttstuetilbud, tilleggsanalyser"
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




# Tilleggsanalyser



I xl-fila er alle inngrepa i 2018-2019 (kohort 1) og 2020-2022 (kohort 2). Kolonne "Operasjonskoder" som i originalfilene.         
Der det er registrert ein eller fleire laparoskopikodar, har eg markert dette med eit 1-tal i ei ny kolonne "LapKode". Det skal vere registret 139 inngrep med laparoskopikode i 2018-19 og 215 i 2020-2022.
 
Kan du sjekke fylgjande:
Er det ein signifikant auke i andel laparoskopiar frå kohort 1 til kohort 2? 139/1199 (11,6%) vs. 215/1292 (16,6%)?
Ja, det er signifikant, p-verdi: 3.8846207\times 10^{-4}


Sammenlign alle inngrep med laparoskopikode  (139 + 215 = 354) og med alle inngrep utan laparoskopikode (1060 + 1077 = 2137). 
Vil samanlikne inngrepa med laparoskopikode med inngrepa utan slik kode med tanke på




## Total tid, begge kohorter samlet
- Uten laparoskopi: Median: 191  Gjennomsnitt: 200.9, interkvartil: 125 - 258
- Med laparoskopi: Median: 197  Gjennomsnitt: 211.3, interkvartil: 165 - 247.75
- p-verdi for forskjell mellom gjennomsnittlig totaltid: 0.015
- p-verdi for forskjell mellom median totaltid: 0.003

Både t-test og ikke-parametrisk test angir signifikant forskjell.


## Knivtid, begge kohorter samlet
- Uten laparoskopi: Median: 76  Gjennomsnitt: 89.2, interkvartil: 33 - 128
- Med laparoskopi: Median: 83  Gjennomsnitt: 96.9, interkvartil: 56 - 119.75
- p-verdi for forskjell mellom gjennomsnittlig knivtid: 0.023
- p-verdi for forskjell mellom median knivtid: 2\times 10^{-4}

Både t-test og ikke-parametrisk test angir signifikant forskjell.

 
Vi veit (sjå tabell 3 i manuset) at desse gjennomsnitta var høgare i kohort 2 - dvs. at ein i kohort 2 brukte gjennomsnittleg meir totaltid på operasjonsstova og også gjennomsnittleg meir tid på sjølve operasjonen (knivtid). Vi har altså lyst til å undersøkje om ein (signifikant?) høgare andel laparoskopiar i kohort 2 kan vere noko av forklaringa på den auka tidsbruken i kohort 2.

