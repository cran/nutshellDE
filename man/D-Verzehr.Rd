\name{Verzehr}
\Rdversion{1.1}
\alias{Verzehr}
\docType{data}
\title{
Pro-Kopf-Verzehr von Nahrungsmitteln in den USA, 1980-2005
}
\description{
  Datenrahmen mit Angaben zum Pro-Kopf-Verzehr bestimmter Nahrungsmittelgruppen 
  in den USA für den Zeitraum von 1980 bis 2005. Die Daten sind dem 
  \emph{Statistical Abstract of the United States}, Jahrgang 2009, entnommen.
  Die Reihenfolge der Kategorien der Variable \code{Nahrungsmittel} ist willkürlich. 
}
\usage{data(Verzehr)}
\format{
  Datenrahmen mit 48 Beobachtungen der folgenden 4 Variablen:
  \describe{
    \item{\code{Nahrungsmittel}}{[\code{Food}] Nahrungsmittelklasse (Faktor mit 
                              8 Kategorien: 
                              „Kalorische Süßungsmittel“, 
                              „Molkereiprodukte“, „Eier“, 
                              „Fisch und Schalentiere“, 
                              „Backwaren“, „Getreide“, 
                              „Geflügel“, „Rotes Fleisch“).}
    \item{\code{Jahr}}{[\code{Year}] Jahresangabe (Integer, Zeitraum 1980 bis 2005).}
    \item{\code{Menge}}{[\code{Amount}] Pro Kopf verzehrte Menge (Gleitkommazahl).}
    \item{\code{Einheit}}{[\code{Units}] Gemessene Einheit (Faktor mit 2 Kategorien, 
                              „Stück“ oder „Pfund“).}
  }
}
\source{
  Die Originaldaten können über die Internetseite der amerikanischen Behörde für 
  Bevölkerungsstatistik (\emph{Census Bureau}) unter der folgender Rubrik 
  abgerufen werden: \cr
  \url{http://www.census.gov/compendia/statab/2009/cats/health_nutrition/food_consumption_and_nutrition.html} 
  (Tabelle 209). Der Direktlink zur Excel-Datei lautet: \cr
  \url{http://www.census.gov/compendia/statab/2009/tables/09s0209.xls}. \cr
  Die Tabelle ist auch als PDF herunterladbar. 
}
\references{
  Die Daten sind Teil der Beispiele der deutschen Ausgabe von 
  \emph{R in a Nutshell} des O'Reilly Verlags. Rechtliche Informationen 
  finden Sie in der Datei \code{LICENSE} sowie hier: \link{nutshellDE-Paket}. 

  \emph{R in a Nutshell}, Kap. 3, 7. 
}
\seealso{
  \code{\link{ZR.Truthahnpreise}}, 
  \code{\link{ZR.Schinkenpreise}}, 
  \code{\link{Google.Suche.nach.Bacon}}. 
}
\examples{
\dontrun{
if(require(lattice)) { 

## Daten laden 
data(Verzehr)

## Nahrungsmittelverzehr 1980-2005, getrennt nach Kategorien 
dotplot(Menge ~ Jahr | Nahrungsmittel, data = Verzehr, 
        scales = list(relation = "sliced", cex = 0.4), 
        aspect = "xy")
}
}
}
\keyword{datasets}
