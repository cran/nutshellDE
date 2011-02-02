\name{0-Initialisierung}
\Rdversion{1.1}
\alias{Initialisierung}
\alias{Paketverwaltung}
\docType{data}
\title{
Initialisierung des Pakets nutshellDE nach der Installation
}
\description{
\strong{Beim ersten Laden des Pakets nach der Installation müssen automatische 
Anpassungsarbeiten durchgeführt werden} (Details s. nächster Abschnitt). 
Es kann u.U. eine Weile dauern, bis \R die begleitenden Statusmeldungen 
anzeigt und zur Eingabezeile zurückkehrt. Wenn man nicht damit rechnet, 
kann es so wirken, als hätte sich der Ladevorgang aufgehängt. 

\strong{Bitte warten Sie einfach ab, bis \R Ihnen wieder den Prompt zeigt.} 
Wenn die Initialisierung erfolgreich abgeschlossen werden kann, wird jede 
spätere Aktivierung des Pakets ohne spürbare Verzögerung ablaufen. 
}
\details{
\subsection{Problem}{
Eines der lästigsten Probleme, von denen Softwaresysteme geplagt werden, die 
mit alphanumerischen Daten umgehen, betrifft die Frage, wie sich die 
Zeichenkodierungen der unterschiedlichen Betriebssysteme und die jeweiligen 
\emph{Locales} (Gebiets- bzw. Sprachschemaeinstellungen) so kontrollieren lassen, 
dass der Anwender auch dann lesbare Schriftzeichen statt unleserlicher „Byte-Suppe“ 
vor sich sieht, wenn er Daten von einem Fremdsystem übernimmt. Liegt ein Text z.B. 
in der nativen deutschen Kodierung von Mac OS X vor, wird auf einem deutschen 
Windows-Rechner ein „Š“ statt eines „ä“ und ein „§“ statt eines „ß“ angezeigt. 

\R bildet in dieser Hinsicht keine Ausnahme. Das \pkg{nutshellDE}-Paket wurde 
unter OS X übersetzt und bearbeitet. Ohne geeignete Gegenmaßnahmen bekäme ein 
Windows- oder Linux-Benutzer also an diversen Stellen statt der korrekten 
Umlaute und anderer Nicht-ASCII-Zeichen unerwartete Schriftzeichen zu sehen 
(oder, um ganz präzise zu sein, \emph{zwei} Zeichen pro Nicht-ASCII-Zeichen, 
weil R unter OS X standardmäßig mit der Unicode-Kodierung UTF-8 arbeitet). 
}
\subsection{Lösung}{
In der Theorie ist die Lösung relativ geradlinig: eine Software kann 
Schriftzeichen genau dann korrekt darstellen, wenn ihr als Zusatzinformation 
die Zeichenkodierung des Texts (oder Textfragments) mitgeteilt wird. (Wir 
unterstellen hier stillschweigend, dass die betreffende Software überhaupt 
in der Lage ist, Zeichenkodierungsinformationen zu verarbeiten.)

\emph{Ein} Teil dieses Problems lässt sich in \R einfach lösen und betrifft 
den Inhalt von Datensätzen: Spaltennamen, Attribute und alle Daten, 
die in irgendeiner Form Text enthalten. In diesen Bestandteilen lässt man 
alle Nicht-ASCII-Zeichenketten mit der Kennung ihrer Zeichenkodierung 
markieren (s. \code{\link[base]{iconv}()}). 
Textdarstellungsprobleme auf Systemen, die mit einer anderen als der 
Ursprungskodierung arbeiten, lassen sich auf diesem Weg beheben. Der Anwender 
merkt davon nichts, weil sich \R automatisch hinter den Kulissen um alles 
nötige kümmern kann, sobald die Daten markiert sind. 

Für den \emph{Objektnamen}, unter dem ein Datensatz in \R abgespeichert wird, 
funktioniert das leider nicht so einfach. Objektnamen können nicht mit ihrer 
Zeichenkodierung markiert werden, zwangsläufig werden sie also immer in der 
Kodierung der aktuellen \R-Umgebung interpretiert. So etwas wie einen 
Bezeichner, der aus einer UTF-8-Umgebung stammt und in einer beliebig 
konfigurierten \R-Umgebung garantiert korrekt angezeigt wird, gibt es leider 
nicht. 

Für Namen von Datensätzen mit Umlauten etc. bleibt als Alternative deshalb nur 
deren Umbenennung. Dazu muss sich der betreffende Datensatz allerdings bereits 
auf der Zielplattform befinden, also dort, wo das Paket installiert und 
eingesetzt wird. Als Zeitpunkt bietet sich das erste interaktive Laden des 
Pakets nach seiner Installation an. 
}
\subsection{Umsetzung}{
Im \pkg{nutshellDE}-Paket wurde das Problem mit den Objektnamen so gelöst, 
dass das Paket bei jedem Laden automatisch selbst überprüft, ob problematische 
Datensätze bereits angepasst wurden (was sich das Paket merkt). Hat diese 
Initialisierung noch nicht stattgefunden, reagiert es selbständig und nimmt 
die nötigen Änderungen vor (Datensätze umbenennen und unter dem neuen Namen 
abspeichern). 

Über Verlauf und Status dieses Initialierungsprozesses werden auf der Konsole 
hinreichend detaillierte Meldungen ausgegeben. Bedingt durch die Größe 
mancher Datensätze kann es beim ersten Laden des Pakets eine Weile dauern, 
bis \R diese Arbeiten abgeschlossen hat. 
}
\subsection{Allerdings\ldots}{
Bedauerlicherweise zeigt \R Ihnen die Statusmeldungen in manchen (allen?) 
Konsolenprogrammen verzögert an, genauer gesagt erst kurz bevor es zum 
Prompt zurückkehrt -- wenn man nicht damit rechnet, wirkt es, als hätte 
sich der Ladevorgang aufgehängt. 

Diese Verzögerung lässt sich nach unserem Kenntnisstand 
nicht vom Programmierer beeinflussen. Die Wahl der Ausgabefunktion 
(\code{\link[base]{cat}()}; \code{\link[base]{writeLines}()}; 
\code{\link[base]{message}()}; \code{\link[base]{warning}()}) 
macht beispielsweise keinen Unterschied, auch 
\code{\link[base]{flush}()} oder \code{\link[base]{options}("warn" = 1)} 
bleiben wirkungslos. 

Bitte warten Sie also beim ersten Laden des Pakets geduldig ab, bis \R 
Ihnen wieder den Prompt zeigt -- länger als etwa eine halbe Minute 
(± \emph{x}) sollte das nicht dauern. Alle späteren Ladevorgänge werden 
ohne spürbare Verzögerung ablaufen. 
}
}
\note{
Die Verwaltungsfunktionen, die sich um diese Arbeiten kümmern, sind 
ausschließlich zum internen Gebrauch vorgesehen, nicht zum direkten Aufruf 
durch den Anwender. 
}
\author{
Jörg Beyer <R-Programmierung@t-online.de> (Initialisierungscode und Dokumentation)
}
\keyword{package}
% (\code{.initialize.package()}, 
% \code{.getPkgAdminData()} und 
% \code{.adjust.dataobject.names()} 
