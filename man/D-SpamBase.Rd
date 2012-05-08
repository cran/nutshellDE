\name{SpamBase}
\Rdversion{1.1}
\alias{SpamBase}
\alias{SpamBase.Strata}
\docType{data}
\title{
Datensatz "Spambase" der Hewlett-Packard-Labs 
} % Kodierungsprobleme in PDF-Ausgabe mit R 2.12.1
\description{
  Der originale „\code{Spambase}“-Datensatz wurde in den \emph{Hewlett-Packard Labs} 
  von Mark Hopkins, Erik Reeber, George Forman und Jaap Suermondt zusammengestellt. 
  Der Datensatz besteht aus Wort- und Zeichenanalysen von insgesamt 4.601 E-Mails, 
  von denen 1.813 als Spam gekennzeichnet sind. Nicht-Spam-Nachrichten stammen 
  aus den Nachrichtenarchiven der Autoren, Spam-Nachrichten wurden vom Postmaster 
  beigesteuert. Aus diesem Pool wurden insgesamt 58 Merkmale extrahiert, die 
  sich für die Klassifizierung von E-Mail-Nachrichten bewährt haben. 
}
\usage{data(SpamBase)}
\format{
  Der Datenrahmen „\code{SpamBase}“ enthält 4.601 Beobachtungen der folgenden 
  58 Variablen: 
  \describe{
    \item{\code{Hf.W.make}}{[\code{word_freq_make}] Häufigkeit des Wortes 
                              „\emph{make}“ (Gleitkommazahl).}
    \item{\code{Hf.W.address}}{[\code{word_freq_address}] Häufigkeit des Wortes 
                              „\emph{address}“ (Gleitkommazahl).}
    \item{\code{Hf.W.all}}{[\code{word_freq_all}] Häufigkeit des Wortes 
                              „\emph{all}“ (Gleitkommazahl).}
    \item{\code{Hf.W.3d}}{[\code{word_freq_3d}] Häufigkeit des Wortes 
                              „\emph{3d}“ (Gleitkommazahl).}
    \item{\code{Hf.W.our}}{[\code{word_freq_our}] Häufigkeit des Wortes 
                              „\emph{our}“ (Gleitkommazahl).}
    \item{\code{Hf.W.over}}{[\code{word_freq_over}] Häufigkeit des Wortes 
                              „\emph{over}“ (Gleitkommazahl).}
    \item{\code{Hf.W.remove}}{[\code{word_freq_remove}] Häufigkeit des Wortes 
                              „\emph{remove}“ (Gleitkommazahl).}
    \item{\code{Hf.W.internet}}{[\code{word_freq_internet}] Häufigkeit des Wortes 
                              „\emph{internet}“ (Gleitkommazahl).}
    \item{\code{Hf.W.order}}{[\code{word_freq_order}] Häufigkeit des Wortes 
                              „\emph{order}“ (Gleitkommazahl).}
    \item{\code{Hf.W.mail}}{[\code{word_freq_mail}] Häufigkeit des Wortes 
                              „\emph{mail}“ (Gleitkommazahl).}
    \item{\code{Hf.W.receive}}{[\code{word_freq_receive}] Häufigkeit des Wortes 
                              „\emph{receive}“ (Gleitkommazahl).}
    \item{\code{Hf.W.will}}{[\code{word_freq_will}] Häufigkeit des Wortes 
                              „\emph{will}“ (Gleitkommazahl).}
    \item{\code{Hf.W.people}}{[\code{word_freq_people}] Häufigkeit des Wortes 
                              „\emph{people}“ (Gleitkommazahl).}
    \item{\code{Hf.W.report}}{[\code{word_freq_report}] Häufigkeit des Wortes 
                              „\emph{report}“ (Gleitkommazahl).}
    \item{\code{Hf.W.addresses}}{[\code{word_freq_addresses}] Häufigkeit des Wortes 
                              „\emph{addresses}“ (Gleitkommazahl).}
    \item{\code{Hf.W.free}}{[\code{word_freq_free}] Häufigkeit des Wortes 
                              „\emph{free}“ (Gleitkommazahl).}
    \item{\code{Hf.W.business}}{[\code{word_freq_business}] Häufigkeit des Wortes 
                              „\emph{business}“ (Gleitkommazahl).}
    \item{\code{Hf.W.email}}{[\code{word_freq_email}] Häufigkeit des Wortes 
                              „\emph{email}“ (Gleitkommazahl).}
    \item{\code{Hf.W.you}}{[\code{word_freq_you}] Häufigkeit des Wortes 
                              „\emph{you}“ (Gleitkommazahl).}
    \item{\code{Hf.W.credit}}{[\code{word_freq_credit}] Häufigkeit des Wortes 
                              „\emph{credit}“ (Gleitkommazahl).}
    \item{\code{Hf.W.your}}{[\code{word_freq_your}] Häufigkeit des Wortes 
                              „\emph{your}“ (Gleitkommazahl).}
    \item{\code{Hf.W.font}}{[\code{word_freq_font}] Häufigkeit des Wortes 
                              „\emph{font}“ (Gleitkommazahl).}
    \item{\code{Hf.W.000}}{[\code{word_freq_000}] Häufigkeit der Zeichenkette 
                              „\emph{000}“ (Gleitkommazahl).}
    \item{\code{Hf.W.money}}{[\code{word_freq_money}] Häufigkeit des Wortes 
                              „\emph{money}“ (Gleitkommazahl).}
    \item{\code{Hf.W.hp}}{[\code{word_freq_hp}] Häufigkeit des Wortes 
                              „\emph{hp}“ (Gleitkommazahl).}
    \item{\code{Hf.W.hpl}}{[\code{word_freq_hpl}] Häufigkeit des Wortes 
                              „\emph{hpl}“ (Gleitkommazahl).}
    \item{\code{Hf.W.george}}{[\code{word_freq_george}] Häufigkeit des Wortes 
                              „\emph{george}“ (Gleitkommazahl).}
    \item{\code{Hf.W.650}}{[\code{word_freq_650}] Häufigkeit des Wortes 
                              „\emph{650}“ (Gleitkommazahl).}
    \item{\code{Hf.W.lab}}{[\code{word_freq_lab}] Häufigkeit des Wortes 
                              „\emph{lab}“ (Gleitkommazahl).}
    \item{\code{Hf.W.labs}}{[\code{word_freq_labs}] Häufigkeit des Wortes 
                              „\emph{labs}“ (Gleitkommazahl).}
    \item{\code{Hf.W.telnet}}{[\code{word_freq_telnet}] Häufigkeit des Wortes 
                              „\emph{telnet}“ (Gleitkommazahl).}
    \item{\code{Hf.W.857}}{[\code{word_freq_857}] Häufigkeit des Wortes 
                              „\emph{857}“ (Gleitkommazahl).}
    \item{\code{Hf.W.data}}{[\code{word_freq_data}] Häufigkeit des Wortes 
                              „\emph{data}“ (Gleitkommazahl).}
    \item{\code{Hf.W.415}}{[\code{word_freq_415}] Häufigkeit des Wortes 
                              „\emph{415}“ (Gleitkommazahl).}
    \item{\code{Hf.W.85}}{[\code{word_freq_85}] Häufigkeit des Wortes 
                              „\emph{85}“ (Gleitkommazahl).}
    \item{\code{Hf.W.technology}}{[\code{word_freq_technology}] Häufigkeit des Wortes 
                              „\emph{technology}“ (Gleitkommazahl).}
    \item{\code{Hf.W.1999}}{[\code{word_freq_1999}] Häufigkeit des Wortes 
                              „\emph{1999}“ (Gleitkommazahl).}
    \item{\code{Hf.W.parts}}{[\code{word_freq_parts}] Häufigkeit des Wortes 
                              „\emph{parts}“ (Gleitkommazahl).}
    \item{\code{Hf.W.pm}}{[\code{word_freq_pm}] Häufigkeit des Wortes 
                              „\emph{pm}“ (Gleitkommazahl).}
    \item{\code{Hf.W.direct}}{[\code{word_freq_direct}] Häufigkeit des Wortes 
                              „\emph{direct}“ (Gleitkommazahl).}
    \item{\code{Hf.W.cs}}{[\code{word_freq_cs}] Häufigkeit des Wortes 
                              „\emph{cs}“ (Gleitkommazahl).}
    \item{\code{Hf.W.meeting}}{[\code{word_freq_meeting}] Häufigkeit des Wortes 
                              „\emph{meeting}“ (Gleitkommazahl).}
    \item{\code{Hf.W.original}}{[\code{word_freq_original}] Häufigkeit des Wortes 
                              „\emph{original}“ (Gleitkommazahl).}
    \item{\code{Hf.W.project}}{[\code{word_freq_project}] Häufigkeit des Wortes 
                              „\emph{project}“ (Gleitkommazahl).}
    \item{\code{Hf.W.re}}{[\code{word_freq_re}] Häufigkeit des Wortes 
                              „\emph{re}“ (Gleitkommazahl).}
    \item{\code{Hf.W.edu}}{[\code{word_freq_edu}] Häufigkeit des Wortes 
                              „\emph{edu}“ (Gleitkommazahl).}
    \item{\code{Hf.W.table}}{[\code{word_freq_table}] Häufigkeit des Wortes 
                              „\emph{table}“ (Gleitkommazahl).}
    \item{\code{Hf.W.conference}}{[\code{word_freq_conference}] Häufigkeit des Wortes 
                              „\emph{conference}“ (Gleitkommazahl).}
    \item{\code{Hf.Z.Semikolon}}{[\code{char_freq_semicolon}] Häufigkeit des 
                              Semikolons in der Nachricht (Gleitkommazahl).}
    \item{\code{Hf.Z.Klammer.rund.links}}{[\code{char_freq_left_paren}] Häufigkeit 
                              der öffnenden runden Klammer in der Nachricht 
                              (Gleitkommazahl).}
    \item{\code{Hf.Z.Klammer.eckig.links}}{[\code{char_freq_left_bracket}] Häufigkeit 
                              der öffnenden eckigen Klammer in der Nachricht 
                              (Gleitkommazahl).}
    \item{\code{Hf.Z.Ausrufezeichen}}{[\code{char_freq_exclamation}] Häufigkeit 
                              des Ausrufezeichens in der Nachricht 
                              (Gleitkommazahl).}
    \item{\code{Hf.Z.Dollar}}{[\code{char_freq_dollar}] Häufigkeit 
                              des Dollarzeichens in der Nachricht 
                              (Gleitkommazahl).}
    \item{\code{Hf.Z.Gatter}}{[\code{char_freq_pound}] Häufigkeit 
                              des Gatter-Zeichens (Raute, „#“) in der Nachricht 
                              (Gleitkommazahl).}
    \item{\code{Majuskel.Kette.Durchschnitt}}{[\code{capital_run_length_average}]  
                              Zusammenhängende Ketten von Großbuchstaben (Majuskeln): 
                              durchschnittliche Länge (Gleitkommazahl).}
    \item{\code{Majuskel.Kette.max}}{[\code{capital_run_length_longest}] 
                              Zusammenhängende Ketten von Großbuchstaben (Majuskeln): 
                              maximale Länge (Integer).}
    \item{\code{Majuskel.Kette.gesamt}}{[\code{capital_run_length_total}] 
                              Zusammenhängende Ketten von Großbuchstaben (Majuskeln): 
                              Gesamtlänge (Integer).}
    \item{\code{ist.Spam}}{[\code{is_spam}] Klassifizierungsvariable: Handelt es 
                              sich bei der Nachricht um Spam oder nicht? 
                              (Faktor mit den beiden Kategorien „Ja“ und „Nein“.)}
  }
}
\details{
  Die Benennung der deutschen Variablen folgt folgender Konvention: 
  \itemize{
    \item{
    „\code{Hf.}“ bedeutet \emph{(relative) Häufigkeit} (als Prozentangabe), 
    } 
    \item{
    „\code{W.}“ bedeutet \emph{Wort} (i.S. einer zusammenhängenden, in sich 
    abgeschlossenen Kette alphanumerischer Zeichen), 
    } 
    \item{
    „\code{Z.}“ bedeutet \emph{Zeichen} bzw. \emph{Satzzeichen}, 
    } 
    \item{
    „\code{Majuskel.}“ sind \emph{Großbuchstaben}, 
    }
    \item{
    „\code{Kette.}“ entspricht einer zusammenhängenden Kette bestimmter Zeichen \cr
    (hier: Großbuchstaben). 
    }
  }
  Die Variable \code{ist.Spam} (\code{is_spam}) ist in den Originaldaten mit 
  „0“ (\emph{kein Spam}) resp. „1“ (\emph{Spam}) kodiert. Dieses Schema wurde 
  als Faktorkodierung mit den Kategorien „1“ => „Ja“ und „0“ => „Nein“ umgesetzt. 
  
  Der Datenrahmen „\code{SpamBase.Strata}“ enthält Informationen zur Extraktion einer 
  stratifizierten 70\%-Zufallsstichprobe aus den \code{SpamBase}-Daten. Die Stichprobe 
  wurde mit der Funktion \code{strata()} aus dem Paket \pkg{sampling} 
  gezogen. Die Fallindizes liegen in der Spalte \code{ID_unit}. 
}
\source{
  Das Original dieses Datensatzes wurde auf dem \emph{UCI Machine Learning Repository} 
  veröffentlicht: \cr
  \url{http://archive.ics.uci.edu/ml/datasets/Spambase}. \cr
  Weitere Informationen zum Datensatz sowie zur Zitierweise finden Sie unter der 
  gleichen Adresse. 
}
\references{
  Die Daten sind Teil der Beispiele der deutschen Ausgabe von 
  \emph{R in a Nutshell} des O'Reilly Verlags. Rechtliche Informationen 
  finden Sie in der Datei \code{LICENSE} sowie hier: \link{nutshellDE-Paket}. 

  \emph{R in a Nutshell}, Kap. 21. 
}
\examples{
\dontrun{
if(require(MASS)) { 

## Daten laden 
data(SpamBase)

## \dots und in Trainings- und Testdaten aufteilen 
Trainingsfälle <- 1:nrow(SpamBase) \%in\% SpamBase.Strata$ID_unit
SpamBase.Training <- SpamBase[ Trainingsfälle, ]
SpamBase.Test     <- SpamBase[!Trainingsfälle, ]

## quadratische Diskriminanzanalyse (QDA) anpassen 
Spam.qda <- qda(ist.Spam ~ ., data = SpamBase.Training)
summary(Spam.qda)

## Treffer in den Trainingsdaten
table(Tatsächlich  = SpamBase.Training$ist.Spam, 
      Vorhergesagt = predict(Spam.qda, 
                             newdata = SpamBase.Training)$class)

## Treffer in den Testdaten
table(Tatsächlich  = SpamBase.Test$ist.Spam, 
      Vorhergesagt = predict(Spam.qda, 
                             newdata = SpamBase.Test)$class)
}
}
}
\keyword{datasets}
