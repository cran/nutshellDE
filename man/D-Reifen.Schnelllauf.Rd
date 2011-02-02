\name{Reifen.Schnelllauf}
\Rdversion{1.1}
\alias{Reifen.Schnelllauf}
\docType{data}
\title{
Daten eines Reifen-Schnelllauftests (NHTSA, 2003)
}
\description{
  Datenrahmen mit den Ergebnissen des Reifenschnelllauftests, der 
  2003 im Rahmen einer breit angelegten Untersuchung zur Haltbarkeit und 
  Ausfallsicherheit von Autoreifen von der \emph{National Highway Traffic 
  Safety Administration} (\acronym{NHTSA}) durchgeführt wurde. Alle Reifen dieser 
  Stichprobe stammen aus Phase 1 der Studie (durchgeführt in Phoenix, Arizona). 
  Berücksichtigt wurden neben unbenutzter Lagerware bereits in Gebrauch 
  befindliche Original- oder Austauschreifen sowie Ersatzreifen. 
}
\usage{data(Reifen.Schnelllauf)}
\format{
  Datenrahmen mit 66 Beobachtungen der folgenden 27 Variablen:
  \describe{
    \item{\code{Phase}}{[\code{Phase}] Phase resp. Ort der Untersuchung, hier 
                              durchgängig als „1“, für Phoenix, Arizona, kodiert 
                              (Integer).}
    \item{\code{Reifentyp}}{[\code{Tire_Type}] Reifentyp (Faktor mit den 6 
                              Kategorien „B“, „C“, „D“, „E“, „H“ und „L“).}
    \item{\code{Barcode}}{Barcode (Integer).}
    \item{\code{DOT.Code}}{[\code{Dot_Number}] DOT-Code (oder DOT-Nummer) des 
                              Reifens (Text).}
    \item{\code{DOT.WocheJahr}}{[\code{Dot_MidWeekDate}] Produktionswoche des 
                              Reifens (Datum, Mittwoch der betreffenden 
                              Kalenderwoche nach amerikanischer Konvention).}
    \item{\code{Probe.Datum}}{[\code{Collection_Date}] Zeitpunkt der 
                              Stichprobennahme (Datum).}
    \item{\code{DOT.Alter}}{[\code{DOT_Age}] Alter der Stichprobe in (Dezimal-) 
                              Jahren (Gleitkommazahl).}
    \item{\code{Test.01}}{[\code{X1st_Task}] Interne Testkennung für den 
                              Schnelllauftest (Faktor mit den drei Kategorien 
                              „HS_AIR“ (Schnelllauftest, Lagerware), 
                              „HS_AIR_F“ (Schnelllauftest, in Gebrauch befindliche 
                              Original- oder Austauschreifen) 
                              und „HS_AIR_F_S“ (Schnelllauftest, Ersatzreifen)).}
    \item{\code{Test.01.Status}}{[\code{X1st_Task_Status}] Wurde der Reifen 
                              getestet? (Faktor mit einer Kategorie, 
                              „getestet“).}
    \item{\code{Position}}{Reifenposition (Faktor mit 6 Kategorien).}
    \item{\code{Ausstattungscode}}{[\code{ORN}] Faktor mit 3 Kategorien, 
                              „Neu“ (für Lagerware), „OEM“ (für bereits in 
                              Gebrauch befindliche Originalreifen) 
                              und „Austausch“.}
    \item{\code{AZ.Servicecode}}{[\code{AZ_Use}] Service bzw. Wartung in Arizona? 
                              (Integer, Kodierungen „0“ und „1“).}
    \item{\code{DOT.Laufleistung.mi}}{[\code{DOT_Est_Mileage_mi}] 
                              Laufleistung des Reifens zum Zeitpunkt des 
                              Schnelllauftests, in Meilen: 0 für Lagerware; nach 
                              Zählerstand für Originalreifen; geschätzt für 
                              Austausch- und Ersatzreifen (Integer).}
    \item{\code{DOT.Laufleistung.km}}{[\code{DOT_Est_Mileage_km}] 
                              Laufleistung des Reifens zum Zeitpunkt des 
                              Schnelllauftests, in Kilometern (Integer).}
    \item{\code{Luftdruck.initial}}{[\code{Initial_IP_kPa}] Reifenluftdruck zu Beginn des 
                              Tests, in kPa (Integer).}
    \item{\code{Tragfähigkeit}}{[\code{Load_kg}] Gemessene Tragfähigkeit des Reifens, 
                              in Kilogramm (Integer).}
    \item{\code{Defekt.Zeit}}{[\code{Time_To_Failure}] Zeit bis zum Ausfall des Reifens, 
                              in Stunden (Gleitkommazahl).}
    \item{\code{Defekt.Geschwindigkeit}}{[\code{Speed_At_Failure_km_h}] Prüfgeschwindigkeit 
                              bei Auftreten des Defekts, in Stundenkilometern 
                              (Integer).}
    \item{\code{Defekt.Laufleistung}}{[\code{Mileage_At_Failure_km}] Im Rahmen des Tests 
                              vom Reifen absolvierte Laufleistung, in Kilometern 
                              (Integer).}
    \item{\code{Defekt.Zyklen}}{[\code{Millions_Cycles_At_Failure}] Umdrehungs- resp. 
                              Verformungszyklen des Reifens bis zum Auftreten des 
                              Defekts, in Millionen (Gleitkommazahl).}
    \item{\code{Defekt.Typ}}{[\code{Failure_Type}] Beschreibung des Defekts 
                              (Text).}
    \item{\code{Defekt.Anmerkungen}}{[\code{Failure.Notes}] Anmerkungen zum Defekt 
                              (Text).}
    \item{\code{Photo.1}}{Pfadangabe zu Photo 1 des Reifens, hier irrelevant (Text).}
    \item{\code{Photo.2}}{vergl. \code{Photo.1}}
    \item{\code{Photo.3}}{vergl. \code{Photo.1}}
    \item{\code{Photo.4}}{vergl. \code{Photo.1}}
    \item{\code{RechnungsDatum}}{[\code{Invoice_Date}] Rechnungsdatum (Datum).}
  }
}
\source{
  Originaldaten und Dokumentation können über die Internetseite der US-amerikanischen 
  \emph{National Highway Traffic Safety Administration} (\acronym{NHTSA}) abgerufen 
  werden: \cr
  \url{http://www.nhtsa.gov/Research/Vehicle+Research+&+Testing+(VRTC)/Tires/} \cr
  (Link \emph{NHTSA Phoenix Tire Dataset 4.1}). 
}
\references{
  Die Daten sind Teil der Beispiele der deutschen Ausgabe von 
  \emph{R in a Nutshell} des O'Reilly Verlags. Rechtliche Informationen 
  finden Sie in der Datei \code{LICENSE} sowie hier: \link{nutshellDE-Paket}. 

  \emph{R in a Nutshell}, Kap. 15, 18. 
}
\examples{
library(lattice)

## Daten laden 
data(Reifen.Schnelllauf)

## Cleveland-Punktdiagramm anlegen 
dotplot(factor(Defekt.Geschwindigkeit) ~ Defekt.Zeit | Reifentyp, 
  data = Reifen.Schnelllauf, 
  xlab = "Zeit bis zum Defekt (Std.)", 
  ylab = "Geschwindigkeit (km/h)")
}
\keyword{datasets}
