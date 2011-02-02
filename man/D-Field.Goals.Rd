\name{Field.Goals}
\Rdversion{1.1}
\alias{Field.Goals}
\docType{data}
\title{
Field-Goal-Versuche in der National Football League, Saison 2005
}
\description{
  Der Datenrahmen „\code{Field.Goals}“ fasst für die Saison 2005 Informationen 
  zu allen 982 Field-Goal-Versuchen in der amerikanischen \emph{National Football 
  League} (\acronym{NFL}) zusammen.
}
\usage{data(Field.Goals)}
\format{
  Datenrahmen mit 982 Beobachtungen der folgenden 10 Variablen:
  \describe{
    \item{\code{Woche}}{[\code{week}] Laufende Woche der Saison (Integer).}
    \item{\code{Heimteam}}{[\code{home.team}] Heimteam (Faktor mit 32 Kategorien).}
    \item{\code{Gastteam}}{[\code{away.team}] Gastteam (Faktor mit 32 Kategorien).}
    \item{\code{Quarter}}{[\code{qtr}] Quarter, in dem der Field-Goal-Versuch stattgefunden 
                              hat (Integer, Werte zwischen 1 und 4 sowie 5 bei 
                              Verlängerung).}
    \item{\code{Offense}}{[\code{offense}] Offense/angreifende Mannschaft (Faktor mit 32 
                              Kategorien).}
    \item{\code{Defense}}{[\code{defense}] Defense/Verteidigung (Faktor mit 32 Kategorien).}
    \item{\code{Spielzug}}{[\code{play.type}] Spielzug resp. Ergebnis des Field-Goal-Versuchs 
                              (Faktor mit 4 Kategorien, „FG abgebrochen“, 
                              „FG geblockt“, „FG erzielt“ und „FG verfehlt“).}
    \item{\code{Spieler}}{[\code{player}] Name des ausführenden Spielers (Faktor mit 
                              49 Kategorien).}
    \item{\code{Yards}}{[\code{yards}] Distanz bei Ausführung des Field-Goal-Versuchs, 
                              in Yards (Gleitkommazahl).}
    \item{\code{Stadion}}{[\code{stadium.type}] Stadiontyp (Faktor mit 3 Kategorien, 
                              „Freiluft“, „Halle“ und „beides“).}
  }
}
\source{
  Die Daten wurden von Aaron Schatz bereitgestellt, Chefredakteur der 
  \emph{Football Outsiders}, einer Internetseite über American Football. 
  Die Adresse lautet: \cr
  \url{http://www.footballoutsiders.com}. 
}
\references{
  Die Daten sind Teil der Beispiele der deutschen Ausgabe von 
  \emph{R in a Nutshell} des O'Reilly Verlags. Rechtliche Informationen 
  finden Sie in der Datei \code{LICENSE} sowie hier: \link{nutshellDE-Paket}. 

  \emph{R in a Nutshell}, Kap. 3, 6, 16, 18, 21. 
}
\examples{
## Daten laden 
data(Field.Goals)


## Histogramm und Q-Q-Plot der Field-Goal-Distanzen 
par(mfcol = c(1, 2), ps = 10, cex.main = 1, cex.axis = 0.8)

hist(Field.Goals$Yards, breaks = 25, 
  main = "Histogramm\\nField-Goal-Distanzen", 
  xlab = "Field-Goal-Distanzen (yd)", 
  ylab = "Häufigkeit")

qqnorm(Field.Goals$Yards, pch = ".", 
  main = "Q-Q-Plot\\nField-Goal-Distanzen", 
  xlab = "Quantile Normalverteilung", 
  ylab = "Quantile Distanzen (yd)")


## Streudiagramm: Zusammenhang zwischen Distanz und Erfolgsquote 
Field.Goals.Klasse <- transform(Field.Goals, 
  FG.Ausgang = factor(
    ifelse(Spielzug == "FG erzielt", "FG +", "FG -"), 
    levels = c("FG -", "FG +")))

(Field.Goals.XTab <- xtabs(~ FG.Ausgang + Yards, 
                           data = Field.Goals.Klasse))

plot(colnames(Field.Goals.XTab), 
     (Field.Goals.XTab["FG +",] / colSums(Field.Goals.XTab) * 100), 
     xlab = "Entfernung (Yards)", 
     ylab = "Erfolg (Prozent)", 
     xaxp = c(20, 60, 8))
}
\keyword{datasets}
