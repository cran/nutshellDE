\name{Yahoo-Finanzen}
\alias{Yahoo-Finanzen}
\alias{Wertpapiere.abfragen}
\alias{Kurse.abfragen}
\title{
Abfragen von historischen Aktienkursen mit Yahoo!-Finanzen
} % Kodierungsprobleme in PDF-Ausgabe mit R 2.12.1 
\description{
Die Funktionen \code{Kurse.abfragen()} und \code{Wertpapiere.abfragen()} 
übernehmen Symbole börsennotierter Unternehmen und fragen über den Online-Dienst 
\emph{Yahoo!-Finanzen} (\url{http://de.finance.yahoo.com/}) historische Aktienkurse 
für einen gewünschten Zeitraum ab. 
}
\usage{
# Funktion zum Abfragen von historischen Aktienkursen 
# eines Wertpapiers über Yahoo!-Finanzen 
Kurse.abfragen(ticker, 
               von = if (missing(bis)) (Sys.Date() - 365) else (bis - 365), 
               bis = if (missing(von))  Sys.Date()        else (von + 365), 
               intervall = "d")

# Hilfsfunktion zum Abfragen von historischen Aktienkursen 
# einer Liste von Wertpapieren 
Wertpapiere.abfragen(ticker, 
               von = if (missing(bis)) (Sys.Date() - 365) else (bis - 365), 
               bis = if (missing(von))  Sys.Date()        else (von + 365), 
               intervall = "d")
}
\arguments{
  \item{ticker}{
  Symbol \emph{des} (\code{Kurse.abfragen()}) 
  oder \emph{der} (\code{Wertpapiere.abfragen()}) 
  abgefragten Unternehmen (einzelner Textwert oder Zeichenvektor). 
}
  \item{von}{
  Startzeitpunkt des angeforderten Intervalls (Datumswert). 
}
  \item{bis}{
  Endzeitpunkt des angeforderten Intervalls (Datumswert). 
}
  \item{intervall}{
  Mit welcher zeitlichen Auflösung sollen die Daten abgefragt werden (Textwert)? 
  Die \emph{Yahoo!-Finanzen}-API akzeptiert hier die Codes "d" („daily“/„täglich“), 
  "w" („weekly“/„wöchentlich“), "m" („monthly“/„monatlich“) 
  und "v" (Dividenden-ausschüttung). 
}
}
\details{
Während \code{Kurse.abfragen()} im Argument \code{ticker} nur ein einzelnes 
Wertpapier pro Aufruf verarbeiten kann, ist \code{Wertpapiere.abfragen()} in der 
Lage, Symbollisten mit mehreren Wertpapieren gleichzeitig zu verarbeiten. 
Die Kurse werden in der angeforderten Reihenfolge zurückgegeben. 

Wir haben versucht, die Standardwerte der Argumente \code{von} und \code{bis} 
so festzulegen, dass sich für möglichst viele Fälle automatisch sinnvolle Intervalle 
ergeben. Eine optimale und in jedem Fall fehlerfreie Lösung wäre aber nur mit (für 
den vorgegebenen didaktischen Rahmen) unverhältnismäßig mehr Aufwand zu erreichen 
gewesen. Wenn Sie im Ergebnis Zeiträume sehen, die nicht Ihren Erwartungen entsprechen, 
versuchen Sie, beim Aufruf stattdessen explizitere Werte zu übergeben. 

Ein Textwert anstelle eines Datums in mindestens einem der Argumente \code{von} 
oder \code{bis} führt zu einem Fehler. Datumsangaben lassen sich am einfachsten 
mit \code{\link[base]{as.Date}()} aus Textwerten erzeugen. Achten Sie aber auf 
die Tücken der Übergabeformats -- am sichersten und kürzesten arbeitet man mit 
der ISO-Notation „\code{JJJJ-MM-TT}“ (s. Beispiele). 
}
\value{
Die Funktion \code{Kurse.abfragen} gibt einen Datenrahmen mit den Kursen 
eines Wertpapiers im angeforderten Zeitraum und mit der gewünschten zeitlichen 
Auflösung zurück. Konnte die Anfrage nicht von \emph{Yahoo!-Finanzen} bearbeitet 
werden, wird \code{NULL} zurückgegeben, außerdem sehen Sie einen Hinweis in der 
Konsole. 

Die Funktion \code{Wertpapiere.abfragen} hat im Prinzip die gleiche Rückgabe. 
Alle erfolgreichen Anfragen werden in einem Datenrahmen zusammengeführt. 
Eine fehlgeschlagene Anfrage löst einen Hinweis aus, das betreffende Wertpapier 
wird im Datenrahmen übersprungen und ignoriert. Ist keine einzige Anfrage 
erfolgreich, ist die Rückgabe \code{NULL}. 
}
\note{
Beide Funktionen sind zu Demonstrationszwecken vorgesehen und aus diesem Grund 
kurz gehalten. An vielen Stellen wären Verbesserungen möglich, insbesondere für 
die Behandlung der Eingabeargumente sind robustere Lösungen vorstellbar 
(Konvertierung und/oder Zurückweisung ungeeigneter Argumente). 
Die \emph{Yahoo!-Finanzen}-API ist unvollständig implementiert und bietet mehr 
Möglichkeiten als die hier dargestellten. 
}
\author{
  Joseph Adler <rinanutshell@gmail.com> (Original) \cr
  Jörg Beyer <R-Programmierung@t-online.de> (Korrekturen und Fehlertoleranz; Übersetzung) 
}
\references{
  Die Funktionen sind Teil der Beispiele der deutschen Ausgabe von 
  \emph{R in a Nutshell} des O'Reilly Verlags. Rechtliche Informationen 
  finden Sie in der Datei \code{LICENSE} sowie hier: \link{nutshellDE-Paket}. 
  
  \emph{R in a Nutshell}, Kap. 13. 
}
\seealso{
  Datensatz \code{\link{Dow.Jones}}. 
}
\examples{
\dontrun{
## Dow-Jones-Daten laden und 
## Tickersymbole anzeigen 
data(Dow.Jones)
DJ.Notierungen

## Bis zu diesem Datum möchten wir die Kurse haben 
Ende <- as.Date("2010-07-02")
DJ.Test <- Wertpapiere.abfragen(DJ.Notierungen, bis = Ende)
head(DJ.Test, n = 5)
}
}
\keyword{utilities}
