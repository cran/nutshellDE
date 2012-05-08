\name{Rekodierung}
\alias{faktorisieren}
\alias{auf.NA.setzen}
\title{
Hilfsfunktionen zum Rekodieren von Vektoren
}
\description{
Zwei einfache Hilfsfunktionen zum Rekodieren von Vektoren. 
\code{faktorisieren()} wandelt einen Vektor in einen Faktor um 
(eine kategoriale Variable); \code{auf.NA.setzen()} ersetzt 
in einem Vektor bestimmte Werte durch \code{NA}s. 
}
\usage{
# Hilfsfunktion zum Umwandeln eines Vektors in einen Faktor 
faktorisieren(x, codes, ordered = FALSE, exclude = NA)

# Hilfsfunktion zum Entfernen bestimmter Werte aus einem Vektor 
auf.NA.setzen(x, na.codes, drop.unused.levels = FALSE)
}
\arguments{
  \item{x}{
  Datenvektor beliebigen Typs (in der überwiegenden Zahl der Fälle 
  vermutlich \code{integer} oder \code{character}). 
}
  \item{codes}{
  Flache Liste mit Schlüssel/Wert-Paaren oder ein benannter Vektor. Aus den 
  Namen der Elemente werden die Bezeichnungen der Faktorstufen gebildet, über 
  die Werte wird die Zuordnung der Eingabedaten vorgenommen. 
}
  \item{ordered}{
  Logischer Wert, der steuert, ob der Ergebnisvektor als Faktor ohne oder mit 
  Beachtung der Rangreihe angelegt werden soll (per Voreinstellung \emph{ohne}). 
}
  \item{exclude}{
  Vektor mit distinkten Werten des Eingabevektors, die aus den Kategorien des 
  Ergebnisvektors ausgeschlossen werden sollen. 
}
  \item{na.codes}{
  Entweder ein logischer Vektor gleicher Länge wie \code{x}; oder ein Vektor 
  beliebiger Länge, aber vom gleichen Datentyp wie \code{x}. Übereinstimmende 
  Positionen resp. Werte in \code{x} werden auf \code{NA} gesetzt (s. Details). 
}
  \item{drop.unused.levels}{
  Wenn x ein Faktor ist und durch das Ersetzen leere Kategorien entstehen, 
  sollen diese aus dem Faktor entfernt werden (logischer Wert, per 
  Voreinstellung \code{FALSE})? 
}
}
\details{
Die Funktion \code{faktorisieren()} ist nicht viel mehr als eine Hüllfunktion 
für \code{factor()}. Während \code{factor()} erwartet, dass Werte und Kategorien 
getrennt übergeben werden, übernimmt \code{faktorisieren()} im Unterschied dazu 
beide Bestandteile \emph{gemeinsam}: das Argument \code{codes} kann entweder 
als flache Liste mit Schlüssel/Wert-Paaren oder analog als benannter Vektor 
verarbeitet werden. Diese Form der Übergabe ist in vielen Fällen intuitiver und 
insbesondere bei vielen Kategorien fehlerunanfälliger als die Handhabung in der 
\code{factor()}-Funktion von \R. 


Die Funktion \code{auf.NA.setzen()} ist für zwei Anwendungsfälle definiert. 
Im ersten Fall übernimmt sie im Argument \code{na.codes} einen logischen Vektor, der so lang sein muss wie der Datenvektor \code{x} und als Indexvektor wirkt: alle Positionen im Datenvektor, die durch diesen Indexvektor ausgewählt werden, werden auf \code{NA} gesetzt. 

Der zweite Anwendungsfall für \code{auf.NA.setzen()} ergibt sich aus dem Umstand, dass \R nichts mit benutzerdefinierten fehlenden Werten (Missing-Codes) anfangen kann. Hier werden im Argument \code{na.codes} Ausschlusswerte übergeben: entsprechende Werte, die im Datenvektor \code{x} gefunden werden, werden auf \code{NA} gesetzt. Als Bedingung muss der Vektor mit den Ausschlusswerten vom gleichen Datentyp wie der Datenvektor \code{x} sein. (Ob überhaupt Ausschlusswerte im Datenvektor enthalten sind, ist irrelevant.)

Handelt es sich beim Datenvektor \code{x} um einen Faktor, kann es vorkommen, dass nach dem Ersetzen von Missing-Codes durch \code{NA}s leere Kategorien entstehen. Durch Übergabe des Arguments \code{drop.unused.levels = TRUE} können diese leeren Faktorstufen aus dem Ergebnisvektor entfernt werden. 
}
\value{
Die Funktion \code{faktorisieren()} gibt einen Faktor der gleichen Länge wie 
die Eingabedaten zurück, je nach Aufruf entweder als einfachen Faktor 
(„\emph{nominal}“) oder mit Beachtung der Rangreihe („\emph{ordinal}“). 

Die Funktion \code{auf.NA.setzen()} gibt einen Vektor gleichen Datentyps und 
gleicher Länge des Eingabevektors zurück, die Missing-Codes sind durch 
\code{NA}-Werte ersetzt. 
}
\author{
  Jörg Beyer <R-Programmierung@t-online.de> 
}
\references{
  \emph{R in a Nutshell}, Kap. 15, 18. 
}
\seealso{
  Datensätze \code{\link{Geburtenstatistik.2006.Stp}}, 
  \code{\link{Sterbestatistik.2006.Stp}}; \cr
  Funktionen \code{\link{factor}()} und 
  \code{\link{ordered}()}. 
}
\examples{
\dontrun{
## Funktion "faktorisieren()" 
## Beispiel 1: numerische Eingabedaten faktorisieren 
Kodierung <- c("w" = 1, "m" = 2); 
(Dummy <- rep(c(2, 1, 99), 10))
table(faktorisieren(Dummy, codes = Kodierung))

## Beispiel 2: Textdaten faktorisieren 
Kodierung <- c("w" = "a", "m" = "b"); 
(Dummy <- rep(c("a", "b"), 10))
table(faktorisieren(Dummy, codes = Kodierung))


## Funktion "auf.NA.setzen()" 
## Beispiel Anwendungsfall 1: Werte auf bestimmten Positionen auf NA setzen 
## Eingabedaten anlegen
(Dummy <- c(1:10, rep(99, 2)))

## Wir wollen keine 99er Codes 
(Ausschluss <- Dummy \%in\% 99)
auf.NA.setzen(Dummy, na.codes = Ausschluss)


## Beispiel Anwendungsfall 2: bestimmte Werte auf NA setzen 
## Eingabedaten anlegen
(Dummy <- c(1:10, rep(99, 2)))

## Wir wollen weder 99er noch 999er (!) Codes 
auf.NA.setzen(Dummy, na.codes = c(99, 999))
}
}
\keyword{utilities}
