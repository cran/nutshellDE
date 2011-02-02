\name{Team.Batting.00bis08}
\Rdversion{1.1}
\alias{Team.Batting.00bis08}
\docType{data}
\title{
Batting-Daten der Teams im Major League Baseball, 2000-2008
}
\description{
  Der Datenrahmen „\code{Team.Batting.00bis08}“ wurde durch eine SQL-Abfrage der 
  \code{\link{BBDB}}-Datenbank gewonnen: für alle Teams im Major League Baseball 
  (\acronym{MLB}) wurden die aggregierten Offensivstatistiken des Zeitraums 
  2000 bis 2008 angefordert (Relation \code{Teams}). 
}
\usage{data(Team.Batting.00bis08)}
\format{
  Datenrahmen mit 270 Beobachtungen der folgenden 13 Variablen:
  \describe{
    \item{\code{TeamID}}{[\code{teamID}] Eindeutige Kennung des Teams, für das der 
                              Spieler angetreten ist (Text).}
    \item{\code{JahrID}}{[\code{yearID}] Jahr/Saison (Integer).}
    \item{\code{Runs}}{[\code{runs}] Runs (Integer).}
    \item{\code{Singles}}{[\code{singles}] Singles (Integer).}
    \item{\code{Doubles}}{[\code{doubles}] Doubles (Integer).}
    \item{\code{Triples}}{[\code{triples}] Triples (Integer).}
    \item{\code{Homeruns}}{[\code{homeruns}] Homeruns (Integer).}
    \item{\code{Walks}}{[\code{walks}] Walks (Integer).}
    \item{\code{Stolen.Bases}}{[\code{stolenbases}] Stolen Bases (Integer).}
    \item{\code{Caught.Stealings}}{[\code{caughtstealing}] Caught Stealings (Integer).}
    \item{\code{Sacrifice.Flies}}{[\code{sacrificeflies}] Sacrifice Flies (Integer).}
    \item{\code{Hit.by.Pitch}}{[\code{hitbypitch}] Hit by Pitch (Integer).}
    \item{\code{At.Bats}}{[\code{atbats}] At-Bats-Statistik (Integer).}
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

  \emph{R in a Nutshell}, Kap. 16, 20. 
}
\seealso{
  \code{\link{BBDB}}, 
  \code{\link{Batting.2008}}. 
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

Team.Batting.sql <- 
  'SELECT  TeamID, 
           JahrID, 
           R                    AS "Runs", 
           H - `2B` - `3B` - HR AS "Singles", 
           `2B`                 AS "Doubles", 
           `3B`                 AS "Triples", 
           HR                   AS "Homeruns", 
           BB                   AS "Walks", 
           SB                   AS "Stolen.Bases", 
           CS                   AS "Caught.Stealings", 
           SF                   AS "Sacrifice.Flies", 
           HBP                  AS "Hit.by.Pitch", 
           AB                   AS "At.Bats" 
     FROM  Teams 
     WHERE JahrID BETWEEN 2000 AND 2008 
     ORDER BY TeamID;'

Team.Batting.00bis08 <- dbGetQuery(BBDB, 
                                   statement = Team.Batting.sql)

## Datenbankverbindung trennen 
## und Treiber-Objekt entsorgen 
dbDisconnect(BBDB)
dbUnloadDriver(Trb)
}


## Daten laden (Variante 2: aus dem Paket) 
data(Team.Batting.00bis08)


## Mittelwerte und Standardabweichungen der Home-Runs 2000-2008, 
## gruppiert nach Team 
data.frame(MW = tapply(Team.Batting.00bis08$Homeruns, 
                       INDEX = Team.Batting.00bis08$TeamID, 
                       FUN = mean, na.rm = TRUE), 
           S  = tapply(Team.Batting.00bis08$Homeruns, 
                       INDEX = Team.Batting.00bis08$TeamID, 
                       FUN = sd, na.rm = TRUE))
}
\keyword{datasets}
