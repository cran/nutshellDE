\name{Shiller.Index}
\Rdversion{1.1}
\alias{Shiller.Index}
\docType{data}
\title{
Robert J. Shillers US-Hauspreisindex
}
\description{
  Robert J. Shiller ist Professor für Wirtschaftswissenschaften an der Yale 
  University und gilt als einer der führenden Experten für Immobilien- resp. 
  Wohnraumpreise in den USA. Der Datenrahmen „\code{Shiller.Index}“ enthält 
  den von ihm zusammengestellten Hauspreisindex für den Zeitraum zwischen 
  1890 und 2010. Die Kennziffern sind inflationsbereinigt; ab 2007 werden 
  Quartalsmittelwerte berichtet.
}
\usage{data(Shiller.Index)}
\format{
  Datenrahmen mit 126 Beobachtungen der folgenden 5 Variablen:
  \describe{
    \item{\code{Jahr}}{[\code{Year}] Jahresangabe (Gleitkommazahl). 
                              Ab 2007 werden die Kennziffern pro Quartal erfasst 
                              und zur jeweiligen Quartalsmitte im Datensatz abgelegt).}
    \item{\code{US.Bevölkerung}}{[\code{U.S.Population}] US-Bevölkerung in Millionen 
                              (Gleitkommazahl).}
    \item{\code{Hauspreisindex}}{[\code{Real.Home.Price.Index}] Hauspreisindex 
                              (Gleitkommazahl).}
    \item{\code{Baukostenindex}}{[\code{Real.Building.Cost.Index}] Baukostenindex 
                              (Gleitkommazahl).}
    \item{\code{Verfallrendite}}{[\code{Long.Bond.Rate}] Verfallrendite 
                              (Gleitkommazahl).}
  }
}
\details{
  Der Datenrahmen „\code{Shiller.Index}“ entspricht der Kombination aus den beiden 
  Datenrahmen „\code{schiller.index}“ und „\code{schiller.other.data}“ der 
  amerikanischen Ausgabe von \emph{R in a Nutshell}.
}
\source{
  Die Originaldaten können über die Internetseite des Autors Prof. Shiller 
  abgerufen werden: \cr
  \url{http://www.irrationalexuberance.com/} \cr
  (zweiter Eintrag in der Aufzählung auf der Startseite). 
  Der folgende Link führt direkt zur Datei: \cr
  \url{http://www.econ.yale.edu/~shiller/data/Fig2-1.xls}
}
\references{
  Die Daten sind Teil der Beispiele der deutschen Ausgabe von 
  \emph{R in a Nutshell} des O'Reilly Verlags. Rechtliche Informationen 
  finden Sie in der Datei \code{LICENSE} sowie hier: \link{nutshellDE-Paket}. 

  \emph{R in a Nutshell}, Kap. 20. 
}
\seealso{
  \code{\link{SanFrancisco.Immobilien.Preise}}. 
}
\examples{
## Daten laden 
data(Shiller.Index)

## lineare Regression anpassen 
Hauspreise.LM  <-  lm(Hauspreisindex ~ Jahr, data = Shiller.Index)

## Streudiagramm anlegen und Regressionsgerade eintragen 
plot(Shiller.Index[, c("Jahr", "Hauspreisindex")], 
     pch = 19, cex = 0.4)

abline(reg = Hauspreise.LM,  lty = 1)
}
\keyword{datasets}
