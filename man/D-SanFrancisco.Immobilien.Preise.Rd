\name{SanFrancisco.Immobilien.Preise}
\Rdversion{1.1}
\alias{SanFrancisco.Immobilien.Preise}
\alias{SF.Immo}
\alias{SF.Immo.Training.inx}
\alias{SF.Immo.Test.inx}
\docType{data}
\title{
Immobilienpreise von Wohnobjekten im Raum San Francisco
}
\description{
  Eine Sammlung von Wohnobjekten (Häuser, Eigentumswohnungen), die im 
  Zeitraum zwischen 13. Februar 2008 und 14. Juli 2009 im Großraum San Franciso 
  verkauft wurden. Die Daten wurden aus verschiedenen Quellen zusammengestellt, 
  u.a. aus den Immobilienrubriken der \emph{San Jose Mercury News} und des 
  \emph{San Francisco Chronicle}, mithilfe von \emph{Yahoo Maps} und dem 
  \emph{Zillow Neighborhood Boundaries}-Dienst.
}
\usage{data(SanFrancisco.Immobilien.Preise)}
\format{
  Der Datenrahmen „\code{SF.Immo}“ besteht aus 3.281 Beobachtungen von 
  17 Variablen. 
  \describe{
    \item{\code{ObjektID}}{[\code{line}, nur in Version 1.0-EN] Eindeutige 
                              Kennung des Datensatzes (Integer).}
    \item{\code{Datum}}{[\code{date} in Version 1.0-EN; \code{saledate} in 
                              Version 1.01-EN] Verkaufsdatum des Objekts 
                              (Datum).}
    \item{\code{JahrMonat}}{[\code{month}, nur in Version 1.0-EN] Aus dem 
                              Verkaufsdatum abgeleitete Variable nur mit 
                              Jahres- und Monatsangabe (Text).}
    \item{\code{ZIP}}{[\code{zip}] ZIP-Code/amerikanische Postleitzahl 
                              des Objekts (Faktor mit 26 Kategorien).}
    \item{\code{County}}{[\code{county}, nur in Version 1.0-EN] County, in 
                              dem sich das Objekt befindet; durchgängig als 
                              „San Francisco County“ kodiert (Text).}
    \item{\code{Ort}}{[\code{city}] Ortsangabe des Objekts; durchgängig als 
                              „San Francisco“ kodiert (Text).}
    \item{\code{Straße}}{[\code{street}] Straßenangabe des Objekts (Text).}
    \item{\code{Wohngegend}}{[\code{neighborhood} in Version 1.0-EN] 
                              Name der Wohngegend, in dem sich das Objekt 
                              befindet (Faktor mit 34 Kategorien).}
    \item{\code{ZIP.Wohngegend}}{[\code{neighborhood} in Version 1.01-EN] 
                              Auflistung der Wohngegenden im Bereich des 
                              jeweiligen ZIP-Codes (Faktor mit 26 Kategorien).}
    \item{\code{GeoBreite}}{[\code{latitude}, nur in Version 1.0-EN] 
                              Geografische Breite des Wohnobjekts 
                              (Gleitkommazahl).}
    \item{\code{GeoLänge}}{[\code{longitude}, nur in Version 1.0-EN] 
                              Geografische Länge des Wohnobjekts 
                              (Gleitkommazahl).}
    \item{\code{Baujahr}}{[\code{year} in Version 1.0-EN; \code{yearbuilt} 
                              in Version 1.01-EN] Baujahr des Objekts 
                              (Integer).}
    \item{\code{Eigentumswohnung}}{[\code{condolike}, nur in Version 1.01-EN] 
                              Handelt es sich bei dem Objekt um eine Eigentumswohnung bzw. ein vergleichbares 
                              Objekt? 
                              (Faktor mit zwei Kategorien, kodiert als „J“ 
                              für \emph{Ja} oder „N“ für \emph{Nein}).}
    \item{\code{Schlafzimmer}}{[\code{bedrooms}] Anzahl der Schlafräume 
                              (Integer).}
    \item{\code{Grundfläche}}{[\code{squarefeet}] Grundfläche des Objekts, 
                              vermutlich die gesamte umbaute Fläche des Objekts, 
                              in Quadratmetern (Integer).}
    \item{\code{Grundstück}}{[\code{lotsize}] Grundstücksgröße des Objekts 
                              in Quadratmetern (Integer).}
    \item{\code{Preis}}{[\code{price}] Verkaufspreis des Objekts in US-Dollar 
                              (Integer).}
    }
}
\details{
  Zur Anpassung verschiedener statistischer Modelle wurde der Gesamtdatensatz 
  in zwei Teilstichproben aufgeteilt, einen Trainings- und einen Testdatensatz. 
  Mit den beiden Integer-Vektoren „\code{SF.Immo.Training.inx}“ und 
  „\code{SF.Immo.Test.inx}“ lassen sich diese Stichproben rekonstruieren: sie 
  enthalten die Zeilenindizes der betreffenden Fälle, mit diesen lässt sich 
  der Gesamtdatensatz aufteilen. 

  Spalten \code{Grundfläche} (\code{squarefeet}) und \code{Grundstück} 
  (\code{lotsize}) lagen ursprünglich in Quadratfuß vor. Sie wurden für die 
  deutsche Ausgabe in Quadratmeter umgerechnet und ganzzahlig gerundet. Dadurch 
  kommt eine leichte Unschärfe in die Daten, die hier aber in Kauf genommen 
  wurde. 
}
\note{
  Zwischen den englischen Beispieldaten der Paketversionen 1.0 und 1.01 und den 
  im Buch dargestellten Daten und Beispielen gab es erhebliche Diskrepanzen. 
  Diese Probleme wurden in der deutschen Ausgabe behoben, Hinweise auf die 
  Inkonsistenzen finden Sie oben (Abschnitt „\emph{Format}“) und in der Datei 
  \code{ChangeLog}. 
}
\source{
  Die Immobiliendaten wurden u.a. aus den folgenden Quellen zusammengetragen: \cr
  \url{http://www.sfgate.com} \cr
  \url{http://www.mercurynews.com} \cr
  Geografische und Nachbarschaftsdaten wurden mithilfe der folgenden 
  Online-Dienste ermittelt: \cr
  \url{http://maps.yahoo.com/} \cr
  \url{http://www.zillow.com/howto/api/neighborhood-boundaries.htm}
}
\references{
  Die Daten sind Teil der Beispiele der deutschen Ausgabe von 
  \emph{R in a Nutshell} des O'Reilly Verlags. Rechtliche Informationen 
  finden Sie in der Datei \code{LICENSE} sowie hier: \link{nutshellDE-Paket}. 

  \emph{R in a Nutshell}, Kap. 15, 16, 20, 22. 
}
\seealso{
  \code{\link{Shiller.Index}}.
}
\examples{
\dontrun{
if(require(lattice)) { 

## Daten laden 
data(SanFrancisco.Immobilien.Preise)

## Postleitzahlenbereich einschränken 
## (mindestens 50 Objekte pro ZIP) 
(SF.Immo.ZIPs <- table(subset(SF.Immo, 
                              subset = !is.na(Grundfläche), 
                              select = ZIP)))

(Ausschluss.ZIPs <- names( SF.Immo.ZIPs[SF.Immo.ZIPs < 50] ))

## Ein paar Hilfsparameter berechnen 
max.Preis <- 4000000;  max.Grundfläche <- 600;  Skalen.cex <- 0.625

## Streudiagramm anlegen 
xyplot(Preis ~ Grundfläche | ZIP, 
  data = SF.Immo, 
  subset = ((! ZIP \%in\% Ausschluss.ZIPs) & 
            (Preis <= max.Preis) & 
            (Grundfläche <= max.Grundfläche)), 
  scales = list(cex = Skalen.cex), 
  par.strip.text = list(cex = Skalen.cex, 
                        lines = 1.25), 
  layout = c(4, NA))
}
}
}
\keyword{datasets}
