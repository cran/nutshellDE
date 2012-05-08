\name{ZR.Lebensmittelpreise}
\Rdversion{1.1}
\alias{ZR.Truthahnpreise}
\alias{ZR.Schinkenpreise}
\docType{data}
\title{
Truthahn- und Schinkenpreise in den USA
}
\description{
  Zwei univariate Zeitreihen mit monatlich gemittelten Einzelhandelspreisen 
  für Truthahnfleisch („\code{ZR.Truthahnpreise}“) und Schinken 
  („\code{ZR.Schinkenpreise}“). Die Daten decken den Zeitraum zwischen Januar 
  2001 und April 2008 ab (88 Beobachtungen, Gleitkommazahl).
}
\usage{
  data(ZR.Truthahnpreise)
  data(ZR.Schinkenpreise)
}
\details{
  Die Daten beziehen sich auf den US-amerikanischen Einzelhandel und 
  stammen aus der \emph{Retail Scanner Prices for Meat}-Datenbank des 
  \emph{Livestock Market Information Center} (\acronym{LMIC}), 
  alle Preisangaben in US-Dollar.
  }
\source{
  Die Daten wurden über den folgenden Informationsdienst der 
  US-Landwirtschaftsbehörde abgerufen: \cr
  \url{http://www.lmic.info/meatscanner/meatscanner.shtml}.
}
\references{
  Die Daten sind Teil der Beispiele der deutschen Ausgabe von 
  \emph{R in a Nutshell} des O'Reilly Verlags. Rechtliche Informationen 
  finden Sie in der Datei \code{LICENSE} sowie hier: \link{nutshellDE-Paket}. 

  \emph{R in a Nutshell}, Kap. 7, 14, 23. 
}
\seealso{
  \code{\link{Verzehr}}, 
  \code{\link{Google.Suche.nach.Bacon}}. 
}
\examples{
\dontrun{
## Daten laden 
data(ZR.Truthahnpreise)
data(ZR.Schinkenpreise)

## Truthahnpreise 
start(ZR.Truthahnpreise)
end(ZR.Truthahnpreise)
frequency(ZR.Truthahnpreise)
plot(ZR.Truthahnpreise)

## Kreuzkorrelation 
ccf(ZR.Truthahnpreise, ZR.Schinkenpreise, plot = FALSE)
}
}
\keyword{datasets}
