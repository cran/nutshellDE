\name{Audioscrobbler}
\Rdversion{1.1}
\alias{Audioscrobbler}
\docType{data}
\title{
Audioscrobbler-Nutzerdaten (bis Mai 2005)
}
\description{
  \emph{Audioscrobbler} ist ein Online-Dienst zur Erfassung von 
  Musikhörgewohnheiten: über ein Plugin wird registriert, welche 
  Interpreten ein Anwender hört, und wie oft. 

  Der Datensatz „\code{Audioscrobbler}“ enthält die Nutzerprofile von 20.000 
  Personen, die per Zufall aus der \emph{Audioscrobbler}-Datenbank, Stand Anfang 
  Mai 2005, gezogen wurden. Der Datensatz hat die S4-Klasse \code{transactions} 
  und enthält pro Nutzerkennung die Namen aller Interpreten, die vom jeweiligen 
  Anwender konsumiert wurden. (Auf die Angabe, \emph{wie oft} ein Anwender einen 
  bestimmten Interpreten hört, wurde hier verzichtet.) 
}
\details{
  Der \emph{Audioscrobbler}-Dienst inklusive der Datenbank wurde vor einiger 
  Zeit von \emph{last.fm} übernommen, aktuelle Datenbestände sind seitdem 
  nicht mehr frei zugänglich. Der alte Datenbestand -- bis Anfang Mai 2005 -- 
  steht allerdings unter einer Creative-Commons-Lizenz zum Download im Netz. \cr
  Diese \emph{Audioscrobbler}-Datenbank besteht aus drei relationalen Tabellen, 
  eine mit Profildatensätzen (Benutzerkennung plus Interpret und 
  Häufigkeitszähler, insgesamt mehr als 24 Millionen Datensätze für 150.000 
  Personen), eine mit Interpretendaten (rund 1,8 Millionen Datensätze) sowie 
  eine weitere Tabelle zur Identifizierung von Interpreten, die mehrfach in 
  unterschiedlicher Schreibweise im Datenbestand auftauchen (über 
  193.000 Datensätze). 

  Aus dieser Datenbank wurden per Zufall 20.000 Personen ausgewählt, ihre 
  Nutzerprofile bilden den Datenrahmen „\code{Audioscrobbler}“. Der Datensatz 
  lässt sich gut zur Demonstration von Assoziationsregeln einsetzen 
  (bspw. \code{apriori()} oder \code{eclat()} 
  aus dem Paket \pkg{arules}). 
}
\source{
  \emph{Audioscrobbler}-Datenbestand bis Anfang Mai 2005: \cr
  \url{http://www-etud.iro.umontreal.ca/~bergstrj/audioscrobbler_data.html} \cr
  (135 MB komprimiert, 500 MB entpackt).
}
\references{
  Die Daten sind Teil der Beispiele der deutschen Ausgabe von 
  \emph{R in a Nutshell} des O'Reilly Verlags. Rechtliche Informationen 
  finden Sie in der Datei \code{LICENSE} sowie hier: \link{nutshellDE-Paket}. 

  \emph{R in a Nutshell}, Kap. 11, 22. 
}
\examples{
\dontrun{
if(require(arules)) { 

## Daten laden 
data(Audioscrobbler)

## Assoziationsmodell anpassen; Modellparameter sichten 
Audioscrobbler.apriori <- apriori(
  data = Audioscrobbler, 
  parameter = new("APparameter", 
                  support = 0.0645, confidence = 0.8))

inspect(Audioscrobbler.apriori)
}
}
}
\keyword{datasets}
