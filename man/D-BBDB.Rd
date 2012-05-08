\name{Baseball-Databank}
\Rdversion{1.1}
\alias{BBDB}
\alias{BBDB.sqlite}
\docType{data}
\title{
Die "Baseball-Databank", Ausgabe 2008
} % Kodierungsprobleme in PDF-Ausgabe mit R 2.12.1
\description{
  Dieser Datensatz enthält den gesamten Bestand der \emph{Baseball Databank} 
  (\acronym{BBDB}), Stand 2008, aufbereitet als SQLite-Datenbank. In der 
  \emph{Baseball Databank} werden ab dem Jahr 1871 (!) vollständige Spieler-, 
  Team-, Spiel- sowie weitere Statistiken des Major League Baseball (\acronym{MLB}) 
  erfasst und frei zugänglich gemacht.
}
\details{
  Die \emph{Baseball Databank} wird als relationale Datenbank mit 27 Tabellen 
  und insgesamt 357 Spalten gepflegt. Sie verwaltet in der Ausgabe 2008 rund 
  500.000 Datensätze. 

  Für das \pkg{nutshellDE}-Paket wurden diese Daten als SQLite-Datenbank 
  aufbereitet. Der Zugriff aus \R erfolgt am einfachsten mit dem Erweiterungspaket 
  \pkg{RSQLite} -- in diesem Fall muss nicht einmal eine separate SQLite-Bibliothek 
  installiert werden, das Paket bringt den Quellcode für dieses \acronym{RDBMS} 
  mit. (Ein weiterer Vorteil ist, dass SQLite-Datenbanken für den \emph{Bioconductor} 
  eine wichtige Rolle spielen, das Paket wird deshalb permanent gepflegt). 

  Alternative Verbindungen zur \emph{Baseball Databank} können über die Pakete 
  \pkg{RODBC} oder \pkg{RJDBC} aufgebaut werden. Beide Lösungen setzen 
  neben einer lauffähigen SQLite-Installation zusätzliche \acronym{ODBC}- respektive 
  \acronym{JDBC}-Treibersoftware für SQLite voraus. 
}
\source{
  Die Datenbank kann unter folgender Adresse in verschiedenen Formaten 
  geladen werden, auch ein Archiv mit früheren Ausgaben wird zugänglich 
  gemacht: \cr
  \url{http://www.baseball-databank.org/}. \cr
  Die Datenbank-Dumps (Daten plus Strukturanweisungen) haben komprimiert 
  eine Größe von 5 bis 10 MB. 

  Englische Dokumentation zur Struktur einer \emph{annähernd} identisch 
  aufgebauten relationalen Datenbank ist hier zu finden (die Unterschiede 
  der Datenbankstrukturen sind marginal): \cr
  \url{http://www.baseball1.com/}. \cr
}
\references{
  Als allgemeine Hintergrundinformation zu Baseball-Statistiken hat sich 
  das \emph{Handbuch der Statistikerstellung} von Sven Müncheberg als hilfreich 
  erwiesen, insbesondere zur Entschlüsselung der zahlreichen Abkürzungen: \cr
  \url{http://stats.baseball-softball.de/Handbuch_der_Statistikerstellung.zip}. \cr
  (Das ZIP-Archiv enthält eine Datei im PDF-Format.) 

  Die Daten sind Teil der Beispiele der deutschen Ausgabe von 
  \emph{R in a Nutshell} des O'Reilly Verlags. Rechtliche Informationen 
  finden Sie in der Datei \code{LICENSE} sowie hier: \link{nutshellDE-Paket}. 

  \emph{R in a Nutshell}, Kap. 12, 13, 16, 20. 
}
\seealso{
  \code{\link{Batting.2008}}, 
  \code{\link{Team.Batting.00bis08}}. 
}
\examples{
\dontrun{
if(require(RSQLite)) { 

## Treiber-Objekt und Datenbankverbindung aufbauen 
Trb <- dbDriver("SQLite")
BBDB <- dbConnect(Trb, 
  dbname = system.file("extdata", "BBDB.sqlite", package = "nutshellDE"))

## Namen aller Tabellen abfragen 
dbListTables(BBDB)

## Namen aller Felder in Tabelle "Master" abfragen 
dbListFields(BBDB, name = "Master")

## Datenbankverbindung trennen 
## und Treiber-Objekt entsorgen 
dbDisconnect(BBDB)
dbUnloadDriver(Trb)
}
}
}
\keyword{datasets}
