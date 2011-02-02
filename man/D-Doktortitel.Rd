\name{Doktortitel}
\Rdversion{1.1}
\alias{Doktortitel}
\docType{data}
\title{
Doktortitel, USA 2001-2006
}
\description{
  Der Datenrahmen „\code{Doktortitel}“ enthält die Zahlen der in den Jahren 
  2001 bis 2006 in den USA verliehenen Doktortitel, aufgeschlüsselt nach 
  sechs Kategorien. Die Daten sind dem \emph{Statistical Abstract of the 
  United States}, Jahrgang 2009, entnommen.
}
\usage{data(Doktortitel)}
\format{
  Datenrahmen mit 6 Beobachtungen (eine pro Jahr) der folgenden 7 
  Variablen:
  \describe{
    \item{\code{Jahr}}{[\code{year}] Jahresangabe (Integer).}
    \item{\code{Ingenieursw.}}{[\code{engineering}] Ingenieurswissenschaften 
                              (Integer).}
    \item{\code{Naturw.}}{[\code{science}] Naturwissenschaften 
                              (Integer).}
    \item{\code{Erziehungsw.}}{[\code{education}] Erziehungswissenschaften 
                              (Integer).}
    \item{\code{Gesundheitsw.}}{[\code{health}] Gesundheitswissenschaften 
                              (Integer).}
    \item{\code{Humanw.}}{[\code{humanities}] Humanwissenschaften 
                              (Integer).}
    \item{\code{Sonstige}}{[\code{other}] Sonstige Berufsfelder 
                              (Integer).}
  }
}
\details{
  Die Daten in der Originaldatenquelle des \emph{Statistical Abstract} sind 
  stärker aufgeschlüsselt als die hier vorliegenden -- Unterkategorien wurden 
  entfernt. Ebenfalls nicht berücksichtigt wurden die Angaben aus dem Jahr 2000. 
}
\source{
  Die Originaldaten können über die Internetseite der amerikanischen Behörde für 
  Bevölkerungsstatistik (\emph{Census Bureau}) unter der folgender Rubrik 
  abgerufen werden: \cr
  \url{http://www.census.gov/compendia/statab/2009/cats/science_technology.html} 
  (Tabelle 785). Der Direktlink zur Excel-Datei lautet: \cr
  \url{http://www.census.gov/compendia/statab/2009/tables/09s0785.xls}. \cr
  Die Tabelle ist auch als PDF herunterladbar. 
}
\references{
  Die Daten sind Teil der Beispiele der deutschen Ausgabe von 
  \emph{R in a Nutshell} des O'Reilly Verlags. Rechtliche Informationen 
  finden Sie in der Datei \code{LICENSE} sowie hier: \link{nutshellDE-Paket}. 

  \emph{R in a Nutshell}, Kap. 14. 
}
\examples{
## Daten laden 
data(Doktortitel)

## In Matrix transformieren:
Doktortitel.m <- as.matrix(Doktortitel[2:7])
rownames(Doktortitel.m) <- Doktortitel[, 1]

## Horizontal gruppiertes Balkendiagramm 
Rand.alt <- par("mar")
barplot(Doktortitel.m, beside = TRUE, horiz = TRUE, legend.text = TRUE, 
        names.arg = sub("w\\\\.$", "-\\nwissenschaften", colnames(Doktortitel.m)), 
        cex.names = .75, las = 1, mar = par("mar" = Rand.alt + c(0, 2, 0, 0)))

## Vertikal gestapeltes Balkendiagramm 
barplot(t(Doktortitel.m), ylim = c(0, 66000), 
        legend.text = TRUE, args.legend = list(ncol = 2))
}
\keyword{datasets}
