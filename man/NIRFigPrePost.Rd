% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NIRFigAndelerPrePost.R
\name{NIRFigPrePost}
\alias{NIRFigPrePost}
\title{Søylediagram som fordeling av valgt variabel, målt både før og etter hjerneslaget}
\usage{
NIRFigPrePost(
  RegData,
  valgtVar,
  datoFra = "2011-01-01",
  datoTil = Sys.Date(),
  minald = 0,
  maxald = 110,
  erMann = "",
  diagnose = "",
  innl4t = "",
  NIHSSinn = "",
  outfile = "",
  reshID,
  enhetsUtvalg = 1,
  preprosess = 1,
  hentData = 0,
  ...
)
}
\arguments{
\item{RegData}{En dataramme med alle nødvendige variabler fra registeret}

\item{valgtVar}{Hvilken variabel som skal visualiseres
Boligforh: Boligforhold ved innleggelse og 3 mnd etter slaget
Bosituasjon: Bosituasjon ved innleggelse og 3 mnd etter slaget
Toalett: Toalettbesok før innleggelse og 3 mnd etter slaget
Forflytning: Evne til forflytning før innleggelse og 3 mnd etter slaget}

\item{datoFra}{Tidligste dato i utvalget (vises alltid i figuren). Standard 2019-01-01.
Registeret inneholder registreringer f.o.m. 2011}

\item{datoTil}{Seneste dato i utvalget (vises alltid i figuren).}

\item{minald}{Alder, fra og med (Standardverdi: 0)}

\item{maxald}{Alder, til og med (Standardverdi: 110)}

\item{erMann}{Kjønn, standard: alt annet enn 0/1 gir begge kjønn
0: Kvinner
1: Menn}

\item{outfile}{Navn på fil figuren skrives til. Standard: '' (Figur skrives
til systemets standard utdataenhet (som regel skjerm))}

\item{reshID}{Parameter følger fra innlogging helseregister.no og angir
hvilken enhet i spesialisthelsetjenesten brukeren tilhører}

\item{enhetsUtvalg}{Gjør gruppeutvalg med eller uten sammenlikning. Se \strong{Details} for oversikt.}

\item{preprosess}{Preprosesser data
FALSE: Nei
TRUE: Ja (Standard)}

\item{hentData}{Gjør spørring mot database
0: Nei, RegData gis som input til funksjonen (Standard)
1: Ja}
}
\value{
Søylediagram som fordeling av valgt variabel, målt både før og etter hjerneslaget
}
\description{
Funksjon som genererer en figur med som viser endring i en variabels fordeling fra før til etter hjerneslaget.
}
\details{
Detajer: Her bør man liste opp hvilke variable funksjonen benytter...
}
