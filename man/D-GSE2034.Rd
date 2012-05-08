\name{GSE2034}
\Rdversion{1.1}
\alias{GSE2034}
\docType{data}
\title{
GSE2034 -- Patientendaten einer Brustkrebsstudie
} % Kodierungsprobleme in PDF-Ausgabe mit R 2.12.1
\description{
  Der Datenrahmen „\code{GSE2034}“ ist Teil einer klinischen Studie zu genetischen 
  Merkmalen von Brustkrebspatientinnen. Er enthält Informationen zu 286 
  Lymphknoten-negativen Patientinnen, von denen 180 rückfallfrei waren und 106 
  entfernte Metastasen entwickelt haben. Zu den erfassten Daten gehören die Zeit 
  bis zum diagnostizierten Rückfall oder der Follow-up-Untersuchung (ohne dass 
  ein Rückfall aufgetreten wäre), zwei Rückfall-Marker sowie ein einfacher Marker 
  zur Expression des Östrogenrezeptor-Gens (\emph{positiv} versus \emph{negativ}). 
}
\usage{data(GSE2034)}
\format{
  Datenrahmen mit 286 Beobachtungen der folgenden 7 Variablen:
  \describe{
    \item{\code{PID}}{[\code{PID}] Eindeutige Patientenkennung (Integer).}
    \item{\code{GEO.Zugangscode}}{[\code{GEO.asscession.number} (\emph{sic!})] Eindeutiger 
                              Zugangscode, unter dem die Microarray-Daten der Patientin in 
                              der GEO-Datenbank abgespeichert sind (Text).}
    \item{\code{Rezeptor.Status}}{[\code{ER.Status}] Östrogenrezeptor-Status (Faktor mit 
                              den Kategorien „ER–“ (Rezeptor negativ) und „ER+“ 
                              (Rezeptor positiv)).}
    \item{\code{Lymphknoten.Status}}{[\code{lymph.node.status}] Lymphknoten-Status (Faktor 
                              mit den Kategorien „negativ“ und „positiv“). Bedingt durch 
                              das Design der Studie wird nur die „negativ“-Kodierung 
                              verwendet.}
    \item{\code{Rückfall}}{[\code{relapse}] Logischer Vektor, der anzeigt, ob die Patientin 
                              einen Rückfall hatte oder nicht.}
    \item{\code{Rückfall.Hirn}}{[\code{Brain.relapses}] Logischer Vektor, der anzeigt, ob 
                              das Hirn von dem Rückfall betroffen ist oder nicht.}
    \item{\code{Rückfall.o.FollowUp}}{[\code{months.to.relapse.or.last.followup}] 
                              Zeitraum bis Rückfall oder Follow-up-Untersuchung, 
                              in Monaten (Integer).}
  }
}
\details{
  Bei den vorliegenden Daten handelt es sich um die begleitenden Patientendaten 
  einer Genexpressionsstudie zur Bedeutsamkeit einer Östrogenrezeptor-Beteiligung 
  bei Brustkrebspatientinnen. Die Genexpressionsdaten selbst sind \emph{nicht} 
  Teil dieses Datensatzes. Alle Daten der Studie sind aber öffentlich zugänglich 
  und können über die Webschnittstellen der \emph{Gene-Expression-Omnibus}-Datenbank 
  (\acronym{GEO}) geladen werden. 
}
\source{
  Die Originaldaten können über die Webschnittstellen der 
  \emph{Gene-Expression-Omnibus}-Datenbank (\acronym{GEO}) des \emph{National Center 
  for Biotechnology Information} (\acronym{NCBI}) abgerufen werden: \cr
  \url{http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE2034} \cr
  (Tabelle am Ende der Seite). Der Direktlink zu den Patientendaten lautet \cr
  \url{http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?view=data&acc=GSE2034&id=40089&db=GeoDb_blob26}, 
  die Daten liegen als tabulatorgetrenntes Textformat vor. 
}
\references{
  Die Daten sind Teil der Beispiele der deutschen Ausgabe von 
  \emph{R in a Nutshell} des O'Reilly Verlags. Rechtliche Informationen 
  finden Sie in der Datei \code{LICENSE} sowie hier: \link{nutshellDE-Paket}. 

  \emph{R in a Nutshell}, Kap. 20, 24. 
}
\examples{
\dontrun{
if(require(survival)) { 

## Daten laden 
data(GSE2034)

## Überlebensdaten aufbereiten 
GSE2034.Surv <- transform(GSE2034, 
  Surv = Surv(time  = GSE2034$Rückfall.o.FollowUp, 
              event = GSE2034$Rückfall, 
              type  = "right") 
  )


## Überlebenskurve anpassen 
(GSE2034.Fit <- survfit(Surv ~ Rezeptor.Status, 
                       data = GSE2034.Surv))

Linien <- c(1, 3)
Label <- levels(GSE2034.Surv$Rezeptor.Status)

plot(GSE2034.Fit, ylim = c(0.6, 1.0), lty = Linien, log = FALSE)
legend(150, 1, legend = Label, lty = Linien, cex = 0.5)


## Cox-Regression anpassen 
(GSE2034.Cox <- coxph(Surv ~ Rezeptor.Status, 
                      data = GSE2034.Surv))
}
}
}
\keyword{datasets}
