\name{Toxine.und.Krebs}
\Rdversion{1.1}
\alias{Toxine.und.Krebs}
\docType{data}
\title{
Daten zu Toxin-Emissionen und Krebserkankungen in den USA
} % Kodierungsprobleme in PDF-Ausgabe mit R 2.12.1 => ... Mortalität durch ...
\description{
  Der Datenrahmen „\code{Toxine.und.Krebs}“ enthält Informationen zur Emission 
  toxischer Chemikalien im Jahr 2006 und zu (geschätzten) Neuerkrankungen an 
  Krebs und krebsbedingten Todesfällen im Jahr 2008, aufgeschlüsselt nach 
  US-Bundesstaat. Die Daten wurden aus verschiedenen Tabellen des 
  \emph{Statistical Abstract of the United States}, Jahrgang 2009, 
  zusammengestellt. Populations- und Flächenangaben sind mutmaßlich von Joseph 
  Adler ergänzt worden (ohne Angabe zu Datenquelle und/oder Maßeinheit). 
}
\usage{data(Toxine.und.Krebs)}
\format{
  Datenrahmen mit 41 Beobachtungen der folgenden 15 Variablen:
  \describe{
    \item{\code{Bundesstaat}}{[\code{State}] US-Bundesstaat (Faktor mit 
                              51 Kategorien: Namen der Bundesstaaten).}
    \item{\code{Fläche}}{[\code{Surface_Area}] Größe des Bundesstaats 
                              (Gleitkommazahl; keine Angabe zur Maßeinheit.}
    \item{\code{Population}}{[\code{Population}] Einwohnerzahl gesamt (Integer).}
    \item{\code{Toxine.gesamt}}{[\code{total_toxic_chemicals}] Gesamtmenge der 
                              Toxinbelastung für diesen Bundesstaat 
                              (Gleitkommazahl).}
    \item{\code{Gesamt.vor.Ort}}{[\code{total_on_site}] Gesamtmenge der 
                              in diesem Bundesstaat freigesetzten Toxine 
                              (Gleitkommazahl).}
    \item{\code{Luft.vor.Ort}}{[\code{air_on_site}] Gesamtmenge über Luftemission 
                              in diesem Bundesstaat freigesetzter Toxine 
                              (Gleitkommazahl).}
    \item{\code{Andere.vor.Ort}}{[\code{other_on_site}] Oberflächenbelastung 
                              in diesem Bundesstaat, die durch sonstigen 
                              Toxineintrag verursacht wurde (Gleitkommazahl).}
    \item{\code{Externer.Eintrag}}{[\code{off_site}] Toxinbelastung, die 
                              durch Freisetzung außerhalb des Bundesstaates 
                              verursacht wurde (Gleitkommazahl).}
    \item{\code{Inzidenz.gesamt}}{[\code{new_total}] Gesamtneuerkrankungen 
                              2008 an Krebs (Integer).}
    \item{\code{Inzidenz.Brust}}{[\code{new_breast}] Neuerkrankungen 2008 
                              an Brustkrebs (Integer).}
    \item{\code{Inzidenz.Lunge}}{[\code{new_lung}] Neuerkrankungen 2008 
                              an Lungenkrebs (Integer).}
    \item{\code{Krebstote.gesamt}}{[\code{deaths_total}] Todesfälle 2008 
                              infolge Krebs (Integer).}
    \item{\code{Krebstote.Brust}}{[\code{deaths_breast}] Todesfälle 2008 
                              infolge Brustkrebs (Integer).}
    \item{\code{Krebstote.Lunge}}{[\code{deaths_lung}] Todesfälle 2008 
                              infolge Lungenkrebs (Integer).}
    \item{\code{BStaat}}{[\code{State_Abbrev}] US-Bundesstaat (Faktor mit 
                              51 Kategorien: Kürzel der Bundesstaaten).}
  }
}
\details{
  Kategorie 48 in der Variablen \code{Bundesstaat} (resp. \code{BStaat}) ist 
  „Washington, D.C.“ (resp. „DC“). 

  Neuerkrankungen und Todesfälle werden in Tausenden berichtet, der Toxinausstoß 
  in Millionen Pfund. 

  Laut Dokumentation der Originaldatenquelle addieren sich die Werte in den Spalten 
  \code{Luft.vor.Ort} und \code{Andere.vor.Ort} \emph{nicht} zur Spalte 
  \code{Gesamt.vor.Ort}, da in die Gesamtspalte weitere, nicht näher 
  aufgeschlüsselte Daten eingehen. 
}
\source{
  Die Originaldaten können über die Internetseite der amerikanischen Behörde für 
  Bevölkerungsstatistik (\emph{Census Bureau}) unter den folgenden Rubriken 
  abgerufen werden: 
  
  \url{http://www.census.gov/compendia/statab/2009/cats/geography_environment/solid_waste_hazardous_waste_and_superfund.html} 
  (Tabelle 366). Der Direktlink zur Excel-Datei lautet: \cr
  \url{http://www.census.gov/compendia/statab/2009/tables/09s0366.xls}. 
  
  \url{http://www.census.gov/compendia/statab/2009/cats/health_nutrition/health_conditions_diseases.html} 
  (Tabelle 174). Der Direktlink zur Excel-Datei lautet: \cr
  \url{http://www.census.gov/compendia/statab/2009/tables/09s0174.xls}. 
  
  Die Tabellen sind auch als PDF herunterladbar. 
}
\references{
  Die Daten sind Teil der Beispiele der deutschen Ausgabe von 
  \emph{R in a Nutshell} des O'Reilly Verlags. Rechtliche Informationen 
  finden Sie in der Datei \code{LICENSE} sowie hier: \link{nutshellDE-Paket}. 

  \emph{R in a Nutshell}, Kap. 14, 18. 
}
\seealso{
  \code{\link{Sterbestatistik.2006.Stp}}. 
}
\examples{
## Daten laden; Datensatz in Suchpfad einhängen 
data(Toxine.und.Krebs)
attach(Toxine.und.Krebs)

## Streudiagramm relative Gesamtbelastung vs. Mortalität/Krebs
plot(x = Toxine.gesamt/Fläche, 
     y = Krebstote.gesamt/Population)

## Streudiagramm relative Luftbelastung vs. Mortalität/Lungenkrebs
plot(x = Luft.vor.Ort/Fläche, 
     y = Krebstote.Lunge/Population, 
     xlab = "Luftemission chemisch-toxischer Stoffe", 
     ylab = "Mortalität/Lungenkrebs (pro Kopf)")

text(x = Luft.vor.Ort/Fläche, 
     y = Krebstote.Lunge/Population, 
     labels = BStaat, 
     cex = 0.5, adj = c(0, -1))

## Datensatz aus Suchpfad entfernen 
detach(Toxine.und.Krebs)
}
\keyword{datasets}
