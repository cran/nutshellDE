\name{Dow.Jones}
\Rdversion{1.1}
\alias{Dow.Jones}
\alias{DJ}
\alias{DJ.Notierungen}
\docType{data}
\title{
Kurse der im Dow-Jones-Index notierten Unternehmen
}
\description{
  Der Datenrahmen „\code{DJ}“ enthält die täglichen Kurse aller 30 im 
  Dow-Jones-Index zusammengefassten Unternehmen zwischen Juli 2009 und 
  Juli 2010. Die Daten wurden über den Online-Dienst \emph{Yahoo!-Finanzen} 
  abgefragt und direkt in \R eingelesen. 
}
\usage{data(Dow.Jones)}
\format{
  Der Datenrahmen „\code{DJ}“ besteht aus 7.560 Beobachtungen der 
  folgenden 8 Variablen:
  \describe{
    \item{\code{Ticker}}{[\code{symbol}] Tickersymbol (Kürzel) des notierten Wertpapiers 
                              (Faktor mit 30 Kategorien).}
    \item{\code{Datum}}{[\code{Date}] Datum der Kursnotierung (Datumsvariable).}
    \item{\code{Eröffnung}}{[\code{Open}] Eröffnungskurs des Wertpapiers 
                              (Gleitkommazahl).}
    \item{\code{Hoch}}{[\code{High}] Höchster Kurs des Wertpapiers 
                              (Gleitkommazahl).}
    \item{\code{Tief}}{[\code{Low}] Niedrigster Kurs des Wertpapiers 
                              (Gleitkommazahl).}
    \item{\code{Schluss}}{[\code{Close}] Schlusskurs des Wertpapiers 
                              (Gleitkommazahl).}
    \item{\code{Volumen}}{[\code{Volume}] Volumen (Börsenumsatz) des Wertpapiers 
                              (Gleitkommazahl).}
    \item{\code{Adj.Schluss}}{[\code{Adj.Close}] Angepasster Schlusskurs, 
                              bereinigt um Splits und Dividendenausschüttungen 
                              (Gleitkommazahl).}
  }
}
\details{
  Der Datenrahmen „\code{DJ}“ wurde durch einen Aufruf der Funktion 
  \code{\link{Wertpapiere.abfragen}()} erzeugt. 
  
  Der Zeichenvektor „\code{DJ.Notierungen}“ enthält die 30 Tickersymbole der 
  Dow-Jones-notierten Unternehmen (Stand Juli 2010). Er kann verwendet werden, 
  um den Beispieldatensatz zu rekonstruieren oder andere Zeiträume abzufragen. 
}
\note{
  Es liegt in der Natur des \emph{Dow-Jones-Index}, dass sich die Liste der 30 
  notierten Unternehmen von Zeit zu Zeit ändert. Es ist also nicht auszuschließen, 
  dass Sie irgendwann einmal Warnhinweise zu sehen bekommen, wenn Sie versuchen, 
  \emph{Dow-Jones}-Kurse abzufragen und dazu den Vektor „\code{DJ.Notierungen}“ 
  nehmen. 
}
\source{
  Historische Börsenkurse lassen sich mit dem Online-Dienst \emph{Yahoo!-Finanzen} \cr
  (\url{http://de.finance.yahoo.com/}) abfragen, die Schnittstelle kann aber auch 
  direkt über die Funktion \code{Wertpapiere.abfragen()} angesteuert werden. 
}
\references{
  Die Daten sind Teil der Beispiele der deutschen Ausgabe von 
  \emph{R in a Nutshell} des O'Reilly Verlags. Rechtliche Informationen 
  finden Sie in der Datei \code{LICENSE} sowie hier: \link{nutshellDE-Paket}. 

  \emph{R in a Nutshell}, Kap. 13, 16. 
}
\seealso{
  Hilfsfunktionen \code{\link{Wertpapiere.abfragen}()}, 
  \code{\link{Kurse.abfragen}()}. 
}
\examples{
## Daten laden 
data(Dow.Jones)


## Mittelwert, Minimum und Maximum des Eröffnungskurses
mean(DJ$Eröffnung)
min(DJ$Eröffnung)
max(DJ$Eröffnung)


## Minimum und Maximum in einem Aufruf ermitteln
## Achtung: in R bedeutet "range" nicht Spannweite
range(DJ$Eröffnung)

## Differenz zwischen Minimum und Maximum = Spannweite 
diff(range(DJ$Eröffnung))


## Quantile: 
## Per Voreinstellung: Minimum, Quartile und Maximum
quantile(DJ$Eröffnung)

## Vielleicht wollen wir _nur_ die Quartile\ldots 
quantile(DJ$Eröffnung, probs = seq(0.25, 0.75, by = 0.25))


## Interquartilsabstand 
IQR(DJ$Eröffnung)


## Tukey's Five-Number-Statistik 
fivenum(DJ$Eröffnung)
}
\keyword{datasets}
