\name{SPECint2006}
\Rdversion{1.1}
\alias{SPECint2006}
\docType{data}
\title{
Integer-Benchmark SPECint2006
}
\description{
  Der Datenrahmen „\code{SPECint2006}“ gibt die Ergebnisse des gleichnamigen 
  \acronym{SPEC}-Benchmarks wieder. \acronym{SPEC} ist eine Non-Profit-Organisation, 
  die standardisiert erhobene Leistungsdaten von Computer-Hard- und -Software 
  anbietet. Die Organisation hat eine Reihe Tests für unterschiedliche 
  Anwendungsbereiche definiert. Der \code{SPECint2006} ist ein Integer-Benchmark 
  für typische Aufgaben eines Desktop-Computers, Ergebnisse liegen für 
  unterschiedliche Fabrikate und Rechnersysteme vor. 
}
\usage{data(SPECint2006)}
\format{
  Der Datenrahmen „\code{SPECint2006}“ enthält 1.233 Beobachtungen der 
  folgenden 9 Variablen:
  \describe{
    \item{\code{Benchmark}}{[\code{Benchmark}] Benchmark-Test 
                              (Faktor mit einer Kategorie, „CINT2006“).}
    \item{\code{Hardware.Hersteller}}{[\code{Hardware.Vendor}] Fabrikat des 
                              getesteten Rechners (Faktor mit 23 Kategorien).}
    \item{\code{System}}{[\code{System}] Genaue Produktbezeichnung des 
                              getesteten Rechners (Text).}
    \item{\code{Kerne}}{[\code{Num.Cores}] Gesamtanzahl Prozessorkerne (Integer).}
    \item{\code{Chips}}{[\code{Num.Chips}] Anzahl der Prozessoren/CPUs (Integer).}
    \item{\code{Kerne.pro.Chip}}{[\code{Num.Cores.Per.Chip}] Anzahl der Kerne pro 
                              CPU (Integer).}
    \item{\code{Baseline}}{[\code{Baseline}] Ergebnis des Benchmark-Tests 
                              \emph{ohne} produktspezifische Optimierung 
                              (Gleitkommazahl).}
    \item{\code{Ergebnis}}{[\code{Result}] Ergebnis des Benchmark-Tests 
                              \emph{mit} produktspezifischen Optimierungen 
                              (Gleitkommazahl).}
    \item{\code{Veröffentlichung}}{[\code{Published}] Veröffentlichungsmonat des 
                              Benchmark-Tests (Faktor mit 37 Kategorien, 
                              „Aug. 2007“ bis „Aug. 2009“).}
  }
}
\source{
  Die Adresse der \acronym{SPEC}-Seite im Internet lautet: \cr
  \url{http://www.spec.org/cpu2006/}. \cr
  Die Originaldaten können unter \cr
  \url{http://www.spec.org/cgi-bin/osgresults?conf=cpu2006} \cr
  abgerufen werden (Link: \emph{Dump All Records As CSV}), die Datei 
  ist lediglich etwas über 1 MB groß. 
}
\references{
  Die Daten sind Teil der Beispiele der deutschen Ausgabe von 
  \emph{R in a Nutshell} des O'Reilly Verlags. Rechtliche Informationen 
  finden Sie in der Datei \code{LICENSE} sowie hier: \link{nutshellDE-Paket}. 

  \emph{R in a Nutshell}, Kap. 18. 
}
\examples{
## Daten laden 
data(SPECint2006)

## Analysedaten für t-Test zusammenstellen 
## (Rechner mit 1 Prozessor und 2 Kernen)
SPECint2006.1x2 <- subset(SPECint2006, 
       subset = (Chips == 1 & Kerne == 2), 
       select = c("Baseline", "Ergebnis"))

## Ein paar deskriptive Maße zur Orientierung 
summary(SPECint2006.1x2)

## Kolmogoroff-Smirnov-Test 
ks.test(jitter(SPECint2006.1x2$Baseline), 
        jitter(SPECint2006.1x2$Ergebnis))

## t-Test  Baseline vs. Optimierung 
t.test(SPECint2006.1x2$Baseline, 
       SPECint2006.1x2$Ergebnis, 
       paired = TRUE, 
       alternative = "less")
}
\keyword{datasets}
