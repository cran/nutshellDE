\name{Batting.2008}
\Rdversion{1.1}
\alias{Batting.2008}
\docType{data}
\title{
Batting-Daten der Spieler im Major League Baseball, Saison 2008
}
\description{
  Der Datenrahmen „\code{Batting.2008}“ ist das Resultat einer SQL-Abfrage der 
  \code{\link{BBDB}}-Datenbank: für alle in der Saison 2008 angetretenen 
  Spieler im Major League Baseball (\acronym{MLB}) wurden verschiedene Eigenschaften 
  und Merkmale aus der Relation \code{Master} mit den \code{Batting}-Statistiken 
  (aus der Relation gleichen Namens) zusammengeführt. 
}
\usage{data(Batting.2008)}
\format{
  Datenrahmen mit 1.384 Beobachtungen der folgenden 32 Variablen:
  \describe{
    \item{\code{Nachname}}{[\code{nameLast}] Vorname des Spielers (Text).}
    \item{\code{Vorname}}{[\code{nameFirst}] Nachname des Spielers (Text).}
    \item{\code{GeburtsJahr}}{[\code{birthYear}] Geburtsjahr (Integer).}
    \item{\code{Debut}}{[\code{debut}] Datum des Debuts (Textvariable, ISO-kodiert: 
                              \code{JJJJ-MM-TT}).}
    \item{\code{Größe}}{[\code{height}] Größe in Zoll (Gleitkommazahl).}
    \item{\code{Gewicht}}{[\code{weight}] Gewicht in Pfund (Integer).}
    \item{\code{Schlaghand}}{[\code{bats}] Schlaghand (Textvariable mit den 
                              beiden Kategorien „R“ und „L“).}
    \item{\code{Wurfhand}}{[\code{throws}] Wurfhand (Textvariable mit den 
                              beiden Kategorien „R“ und „L“).}
    \item{\code{SpielerID}}{[\code{playerID}] Eindeutige Kennung des Spielers 
                              in der Datenbank (Text).}
    \item{\code{JahrID}}{[\code{yearID}] Jahr/Saison (Integer).}
    \item{\code{GastID}}{[\code{stint}] Hilfsfeld zur Kennzeichnung der Reihenfolge, 
                              wenn der Spieler für mehr als ein Team angetreten ist 
                              (Integer).}
    \item{\code{TeamID}}{[\code{teamID}] Eindeutige Kennung des Teams, für das der 
                              Spieler angetreten ist (Textvariable mit 30 
                              Kategorien).}
    \item{\code{LigaID}}{[\code{lgID}] Kennung der Liga (Textvariable mit den 
                              beiden Kategorien „AL“ (American League) 
                              und „NL“ (National League)).}
    \item{\code{G}}{Insgesamt absolvierte Spiele (\emph{Games}; 
                              Integer).}
    \item{\code{Batting}}{[\code{G_batting}] In wie vielen Spielen als Batter 
                              angetreten (Integer)?}
    \item{\code{AB}}{At-Bats-Statistik (Integer).}
    \item{\code{R}}{Runs (Integer).}
    \item{\code{H}}{Hits (Integer).}
    \item{\code{2B}}{Doubles (Integer).}
    \item{\code{3B}}{Triples (Integer).}
    \item{\code{HR}}{Homeruns (Integer).}
    \item{\code{RBI}}{Runs Batted In (Integer).}
    \item{\code{SB}}{Stolen Bases (Integer).}
    \item{\code{CS}}{Caught Stealing (Integer).}
    \item{\code{BB}}{Base on Balls (Integer).}
    \item{\code{SO}}{Strikeouts (Integer).}
    \item{\code{IBB}}{Intentional Walks (Integer).}
    \item{\code{HBP}}{Hit by Pitch (Integer).}
    \item{\code{SH}}{Sacrifice Hits (Integer).}
    \item{\code{SF}}{Sacrifice Flies (Integer).}
    \item{\code{GIDP}}{Grounded into Double Plays (Integer).}
    \item{\code{G_alt}}{[\code{G_old}] überholte Version des \code{G}-Felds 
                              (Integer).}
  }
}
\source{
  Internetseite der \emph{Baseball Databank} (\acronym{BBDB}): \cr
  \url{http://www.baseball-databank.org/}. 
}
\references{
  Die Daten sind Teil der Beispiele der deutschen Ausgabe von 
  \emph{R in a Nutshell} des O'Reilly Verlags. Rechtliche Informationen 
  finden Sie in der Datei \code{LICENSE} sowie hier: \link{nutshellDE-Paket}. 

  \emph{R in a Nutshell}, Kap. 12, 13, 14, 18. 
}
\seealso{
  \code{\link{BBDB}}, 
  \code{\link{Team.Batting.00bis08}}. 
}
\examples{
\dontrun{
library(RSQLite)

## Daten laden (Variante 1: aus der Datenbank) 
## Verbindung zur Datenbank einrichten und 
## Datensatz über SQL-Abfrage aufbauen 
Trb <- dbDriver("SQLite")
BBDB <- dbConnect(Trb, 
  dbname = system.file("extdata", "BBDB.sqlite", package = "nutshellDE"))

Batting.2008.sql <- 'SELECT  m.Nachname, m.Vorname, 
                             m.GeburtsJahr, m.Debut, 
                             m.Größe, m.Gewicht, 
                             m.Schlaghand, m.Wurfhand, 
                             b.* 
                       FROM  Master m 
                       INNER JOIN Batting b 
                         ON  b.SpielerID = m.SpielerID 
                       WHERE b.JahrID = 2008 
                       ORDER BY m.SpielerID' 
Batting.2008 <- dbGetQuery(BBDB, statement = Batting.2008.sql)

## Datenbankverbindung trennen 
## und Treiber-Objekt entsorgen 
dbDisconnect(BBDB)
dbUnloadDriver(Trb)
}


## Daten laden (Variante 2: aus dem Paket) 
data(Batting.2008)


## Mittelwerte und Standardabweichungen der Home-Runs 2008, 
## gruppiert nach Team 
data.frame(MW = tapply(Batting.2008$HR, 
                       INDEX = Batting.2008$TeamID, 
                       FUN = mean, na.rm = TRUE), 
           S  = tapply(Batting.2008$HR, 
                       INDEX = Batting.2008$TeamID, 
                       FUN = sd, na.rm = TRUE)
           )
}
\keyword{datasets}
