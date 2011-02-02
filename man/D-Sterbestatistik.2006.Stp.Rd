\name{Sterbestatistik.2006.Stp}
\Rdversion{1.1}
\alias{Sterbestatistik.2006.Stp}
\alias{Sterbefälle.2006}
\docType{data}
\title{
Zufallsstichprobe aus der US-Sterbestatistik 2006
}
\description{
  Der Datenrahmen „\code{Sterbefälle.2006}“ enthält verschiedene Informationen aus 
  der Sterbestatistik der USA des Jahres 2006. Aus dem Gesamtdatenbestand dieses 
  Jahrgangs (über 2,4 Millionen Datensätze) wurde eine 10\%-Zufallsstichprobe 
  gezogen. Jeder Datensatz entspricht einem Sterbefall. 
}
\usage{data(Sterbestatistik.2006.Stp)}
\format{
  Der Datenrahmen „\code{Sterbefälle.2006}“ besteht aus 243.073 Beobachtungen der 
  folgenden 36 Variablen:
  \describe{
    \item{\code{EinwohnerStatus}}{[\code{ResidentStatus}] Einwohner-Status 
                              (Integer, Kodierung „1“ bis „4“).}
    \item{\code{Bildung.KL1989}}{[\code{Education1989}] Bildung, 
                              Klassifikationsschema nach Rev. 1989 
                              (Integer, Kodierung „0“ bis „17“ 
                              sowie „99“/Missing).}
    \item{\code{Bildung.KL2003}}{[\code{Education2003}] Bildung, 
                              Klassifikationsschema nach Rev. 2003 
                              (Integer, Kodierung „1“ bis „8“ 
                              sowie „9“/Missing).}
    \item{\code{BildungsCode}}{[\code{EducationFlag}] Format der 
                              Bildungsinformation auf dem Urbeleg 
                              (Integer, Kodierung „0“ oder „1“ 
                              sowie „2“/Missing).}
    \item{\code{Tod.Monat}}{[\code{MonthOfDeath}] Monat des Todesdatums 
                              (Integer, Kodierung „1“ bis „12“).}
    \item{\code{Geschlecht}}{[\code{Sex}] Geschlecht (Faktor mit den beiden 
                              Kategorien „m“ und „w“).}
    \item{\code{Alter.exakt}}{[\code{AgeDetail}] alphanumerisch kodiertes Alter 
                              zum Todeszeitpunkt (Text; s. 
                              Originaldokumentation).}
    \item{\code{Alter.ersetzt}}{[\code{AgeSubstitution}] Altersanagabe durch 
                              errechnetes Alter ersetzt? (Integer, Kodierung 
                              „NA“ (weder berichtetes noch berechnetes Alter) 
                              oder „1“ (Alter berechnet).)}
    \item{\code{Alter.Kl52}}{[\code{AgeRecode52}] Altersklasse 
                              (Integer-Kodierung mit 52 Kategorien, 
                              letzte Kategorie Missing).}
    \item{\code{Alter.Kl27}}{[\code{AgeRecode27}] Altersklasse 
                              (Integer-Kodierung mit 27 Kategorien, 
                              letzte Kategorie Missing).}
    \item{\code{Alter.Kl12}}{[\code{AgeRecode12}] Altersklasse 
                              (Integer-Kodierung mit 12 Kategorien, 
                              letzte Kategorie Missing).}
    \item{\code{Alter.Kinder.Kl22}}{[\code{AgeRecodeInfant22}] Altersklasse 
                              von Kindern bis 1 Jahr (Integer-Kodierung mit 22 
                              Kategorien).}
    \item{\code{TodesOrt}}{[\code{PlaceOfDeath}] Ort des Todes (Integer, 
                              Kodierung „1“ bis „7“ sowie 
                              „9“/Missing).}
    \item{\code{Zivilstand}}{[\code{MaritalStatus}] Zivilstand (Faktor mit 
                              5 Kategorien, letzte Kategorie Missing).}
    \item{\code{Tod.Wochentag}}{[\code{DayOfWeekofDeath}] Wochentag des 
                              Todesdatums (Integer, Kodierung „1“ bis
                              „7“ sowie „9“/Missing).}
    \item{\code{Datensatz.Jahr}}{[\code{CurrentDataYear}] Jahr des Todesdatums 
                              (Integer, hier durchgängig „2006“).}
    \item{\code{ArbeitsUnfall}}{[\code{InjuryAtWork}] Arbeitsunfall mit 
                              Todesfolge? (Faktor mit 3 Kategorien, 
                              „J“, „N“ sowie „unbekannt“/Missing.)}
    \item{\code{Ursache.Kategorie}}{[\code{MannerOfDeath}] Todesursache 
                              (Integer-Kodierung mit 8 Kategorien, 
                              letzte Kategorie Missing).}
    \item{\code{BestattungsArt}}{[\code{MethodOfDisposition}] Bestattungsart 
                              (Faktor mit 7 Kategorien).}
    \item{\code{Autopsie}}{[\code{Autopsy}] Wurde eine Autopsie durchgeführt? 
                              (Faktor mit 3 Kategorien, „J“, „N“
                              sowie „unbekannt“/Missing.)}
    \item{\code{TätigkeitsArt}}{[\code{ActivityCode}] Tätigkeit zum 
                              Todeszeitpunkt (Integer-Kodierung mit 8 
                              Kategorien).}
    \item{\code{UnfallOrt}}{[\code{PlaceOfInjury}] Ort, an dem der Unfall 
                              geschehen ist (Integer-Kodierung mit 11 
                              Kategorien).}
    \item{\code{ICD.Code}}{[\code{ICDCode}] Code der Todesursache nach ICD-10 
                              (Faktor mit mehr als 3.700 Kategorien).}
    \item{\code{Ursache.Kl358}}{[\code{CauseRecode358}] Rekodierung des 
                              ICD-Codes in 358 Kategorien (Integer).}
    \item{\code{Ursache.Kl113}}{[\code{CauseRecode113}] Rekodierung des 
                              ICD-Codes in 113 Kategorien (Integer).}
    \item{\code{Ursache.Kl130}}{[\code{CauseRecode130}] Rekodierung des 
                              ICD-Codes in 130 Kategorien, nur Kinder 
                              (Integer).}
    \item{\code{Ursache.Kl39}}{[\code{CauseRecord39}] Rekodierung des 
                              ICD-Codes in 39 Kategorien (Integer).}
    \item{\code{Ethnizität}}{[\code{Race}] Ethnizität (Faktor mit 14 
                              Kategorien).}
    \item{\code{Ethn.synchr}}{[\code{BridgeRaceFlag}] 
                              Ethnizitätskodierung synchronisiert? 
                              (Integer, Kodierung „NA“ oder „1“.)}
    \item{\code{Ethn.Imputation}}{[\code{RaceImputationFlag}] 
                              Ethnizitätskodierung imputiert? 
                              (Integer, Kodierung „NA“, „1“ oder „2“.)}
    \item{\code{Ethn.Kl3}}{[\code{RaceRecode3}] Rekodierung der 
                              Volksgruppenzugehörigkeit 
                              in 3 Kategorien (Integer).}
    \item{\code{Ethn.Kl5}}{[\code{RaceRecord5}] Rekodierung der 
                              Volksgruppenzugehörigkeit 
                              in 5 Kategorien (Integer).}
    \item{\code{Ethn.LatAm}}{[\code{HispanicOrigin}] Lateinamerikanische 
                              Volksgruppenzugehörigkeit (Integer-Kodierung).}
    \item{\code{Ethn.LatAm.Kl}}{[\code{HispanicOriginRecode}] 
                              Lateinamerikanische Volksgruppenzugehörigkeit, 
                              Rekodierung (Integer, Kodierung „1“ 
                              bis „8“ sowie „9“/Missing).}
    \item{\code{Alter}}{[\code{age}] Alter in ganzen Jahren, Rekodierung J. 
                              Adler (Integer).}
    \item{\code{Ursache}}{[\code{cause}] Todesursache, Rekodierung J. Adler 
                              (Faktor mit 10 Kategorien).}
  }
}
\details{
  Im Gegensatz zur Geburtenstatistik liegen hier die Indizes der Fälle, die per 
  Zufall aus dem Originaldatensatz ausgewählt wurden, nicht vor. Die Stichprobe 
  lässt sich nicht aus den Originaldaten rekonstruieren. 
}
\source{
  Originaldaten und Dokumentation können über die Internetseite der US-amerikanischen 
  Zentren für Prävention und Kontrolle von Krankheiten (\emph{Centers for Disease 
  Control and Prevention}, \acronym{CDC}) abgerufen werden: \cr
  \url{http://www.cdc.gov/nchs/data_access/Vitalstatsonline.htm} \cr
  (Jahrgang 2006). Die Datendatei ist komprimiert knapp 90 MB und nach dem 
  Entpacken 1,2 GB groß. Die Dokumentation des Datensatzes wird als separater 
  Download zur Verfügung gestellt. 
}
\references{
  Die Daten sind Teil der Beispiele der deutschen Ausgabe von 
  \emph{R in a Nutshell} des O'Reilly Verlags. Rechtliche Informationen 
  finden Sie in der Datei \code{LICENSE} sowie hier: \link{nutshellDE-Paket}. 

  \emph{R in a Nutshell}, Kap. 12, 18. 
}
\seealso{
  Datensätze \code{\link{Geburtenstatistik.2006.Stp}}, 
  \code{\link{Toxine.und.Krebs}}; \cr
  Hilfsfunktionen \code{\link{faktorisieren}()} und 
  \code{\link{auf.NA.setzen}()}. 
}
\examples{
\dontrun{
## Daten laden 
data(Sterbestatistik.2006.Stp)

## Deskriptive Statistiken für die Todesursache 
tapply(Sterbefälle.2006$Alter, 
       INDEX = list(Sterbefälle.2006$Ursache), 
       FUN = summary)

## Varianzanalyse der Todesursache 
## (1) Modell anpassen 
Alter.Todesursache <- 
   aov(Alter ~ Ursache, data = Sterbefälle.2006)
Alter.Todesursache

## (2) Signifikanztest 
anova(Alter.Todesursache)

## (3) Verteilung der Mittelwerte 
model.tables(Alter.Todesursache, type = "means")
}
}
\keyword{datasets}
