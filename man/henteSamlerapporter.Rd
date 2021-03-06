% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NIRhjelpefunksjoner.R
\name{henteSamlerapporter}
\alias{henteSamlerapporter}
\title{Funksjon som produserer rapporten som skal sendes til mottager.}
\usage{
henteSamlerapporter(
  filnavn,
  rnwFil,
  reshID = 0,
  datoFra = Sys.Date() - 180,
  datoTil = Sys.Date()
)
}
\arguments{
\item{filnavn}{sdf}

\item{rnwFil}{Navn på fila som skal kjøres. Angis uten ending (\emph{dvs uten  ".Rnw"})}

\item{reshID}{Aktuell reshid}

\item{datoFra}{dato}

\item{parametre}{Liste med valgfrie parametre, avhengig av type rapport}
}
\value{
Full path of file produced
}
\description{
Funksjon som produserer rapporten som skal sendes til mottager.
}
