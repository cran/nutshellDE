\name{Geburtenstatistik.2006.Stp}
\Rdversion{1.1}
\alias{Geburtenstatistik.2006.Stp}
\alias{Geburten.2006}
\alias{Geburten.2006.inx}
\docType{data}
\title{
Zufallsstichprobe aus der US-Geburtenstatistik 2006
}
\description{
  Der Datenrahmen „\code{Geburten.2006}“ enthält verschiedene Informationen aus 
  der Geburtenstatistik der USA des Jahres 2006. Aus dem Gesamtdatenbestand 
  dieses Jahrgangs (über 4,2 Millionen Datensätze) wurde eine 10\%-Zufallsstichprobe 
  gezogen. Jeder Datensatz entspricht einem Säugling (nicht einer Schwangerschaft). 
}
\usage{data(Geburtenstatistik.2006.Stp)}
\format{
  Der Datenrahmen „\code{Geburten.2006}“ besteht aus 427.323 Beobachtungen der 
  folgenden 13 Variablen:
  \describe{
    \item{\code{Geburt.Monat}}{[\code{DOB_MM}] Geburtsmonat (Integer: 
                              Januar = „1“, Dezember = „12“).}
    \item{\code{Geburt.Wtag}}{[\code{DOB_WK}] Wochentag der Geburt (Faktor mit 7 
                              Kategorien, „Montag“ bis „Sonntag“).}
    \item{\code{Mutter.Alter}}{[\code{MAGER}] Alter der Mutter (Integer: 
                              Alter \eqn{\le} 12 als „12“, Alter \eqn{\ge} 50 als 
                              „50“ kodiert).}
    \item{\code{Geburten.gesamt}}{[\code{TBO_REC}] Gesamtanzahl Geburten (Integer: 
                              \eqn{\ge} 8 Geburten als „8“ kodiert).}
    \item{\code{Mutter.Zunahme}}{[\code{WTGAIN}] Gewichtszunahme der Mutter in 
                              Pfund (Integer: Zunahme \eqn{\ge} 98 Pfund als „98“ 
                              kodiert).}
    \item{\code{Geschlecht}}{[\code{SEX}] Geschlecht des Kindes (Faktor mit den 
                              beiden Kategorien „m“ und „w“).}
    \item{\code{Apgar5}}{[\code{APGAR5}] Apgar-Score, 5 Minuten nach Entbindung 
                              (Integer, Werte von „0“ bis „10“).}
    \item{\code{Mutter.Bildung}}{[\code{DMEDUC}] Bildung der Mutter (Faktor mit 
                              20 Kategorien).}
    \item{\code{PränatalDiagn}}{[\code{UPREVIS}] Pränataldiagnostik, Anzahl der 
                              Untersuchungen (Integer).}
    \item{\code{Schwangerschaft}}{[\code{ESTGEST}] Geschätzte Schwangerschaftsdauer 
                              in Wochen (Integer).}
    \item{\code{Entbindung}}{[\code{DMETH_REC}] Entbindungsmethode (Faktor mit 3  
                              Kategorien, „Vaginal“, „Kaiserschnitt“, 
                              „unbekannt“/Missing).}
    \item{\code{AnzahlSäuglinge}}{[\code{DPLURAL}] Anzahl der entbundenen Säuglinge 
                             (Faktor mit 6 Kategorien; \eqn{\ge} 5 Säuglinge sind 
                             in Kategorie 5 zusammengefasst; Kategorie 6 ist als 
                             „unbekannt“/Missing kodiert).}
    \item{\code{Geburtsgewicht}}{[\code{DBWT}] Geburtsgewicht in Gramm (Integer).}
  }
}
\details{
  Die Indizes der Fälle, die für die Zufallsstichprobe ausgewählt wurden, liegen 
  im Integervektor „\code{Geburten.2006.inx}“, die im Buch verwendete Stichprobe 
  lässt sich also zu Übungszwecken aus den Originaldaten rekonstruieren. 
}
\source{
  Originaldaten und Dokumentation können über die Internetseite der US-amerikanischen 
  Zentren für Prävention und Kontrolle von Krankheiten (\emph{Centers for Disease 
  Control and Prevention}, \acronym{CDC}) abgerufen werden: \cr
  \url{http://www.cdc.gov/nchs/data_access/Vitalstatsonline.htm} \cr
  (Jahrgang 2006). Die Datendatei ist komprimiert knapp 180 MB und nach dem 
  Entpacken 3,3 GB groß. Die Dokumentation des Datensatzes wird als separater 
  Download zur Verfügung gestellt. 
}
\references{
  Die Daten sind Teil der Beispiele der deutschen Ausgabe von 
  \emph{R in a Nutshell} des O'Reilly Verlags. Rechtliche Informationen 
  finden Sie in der Datei \code{LICENSE} sowie hier: \link{nutshellDE-Paket}. 

  \emph{R in a Nutshell}, Kap. 15, 16, 18. 
}
\seealso{
  Datensatz \code{\link{Sterbestatistik.2006.Stp}}; \cr
  Hilfsfunktionen \code{\link{faktorisieren}()} und 
  \code{\link{auf.NA.setzen}()}. 
}
\examples{
\dontrun{
if(require(lattice)) { 

## Daten laden 
data(Geburtenstatistik.2006.Stp)

## Histogramme des Geburtsgewichts, getrennt 
## nach Anzahl der entbundenen Säuglinge 
histogram(~ Geburtsgewicht | AnzahlSäuglinge, 
  data = Geburten.2006, 
  layout = c(1, 5))

## Dichtekurven des Geburtsgewichts, getrennt 
## nach Anzahl der entbundenen Säuglinge 
densityplot(~ Geburtsgewicht | AnzahlSäuglinge, 
  data = Geburten.2006, 
  layout = c(1, 5), 
  plot.points = FALSE)

## Überlagerte Dichtekurven des Geburtsgewichts 
## nach Anzahl der entbundenen Säuglinge 
densityplot(~ Geburtsgewicht, 
  groups = AnzahlSäuglinge[, drop = TRUE], 
  data = Geburten.2006, 
  plot.points = FALSE, auto.key = TRUE)
}
}
}
\keyword{datasets}
