\name{Intern}
\alias{Admin}
\alias{.nutshellDE.admin}

\alias{.initialize.package}
\alias{.getPkgAdminData}
\alias{.adjustDataobjectNames}

\alias{Sterbefaelle.2006}
\title{
Interne Hilfs- und Verwaltungsfunktionen
}
\description{
  Interne Hilfs- und Verwaltungsfunktionen des Pakets \pkg{nutshellDE}. 
  Die Funktionen sind überwiegend undokumentiert und nur im Ausnahmefall 
  für den Endanwender des Pakets vorgesehen (was ein vorheriges Studium 
  des Quellcodes voraussetzt). 
}
\usage{
# 
.initialize.package(pkgname, admin.data = "Admin.rda") 

# 
.getPkgAdminData(pkgname, admin.data = "Admin.rda") 

# 
.adjustDataobjectNames(pkgname, admin.data, envir) 
}
\arguments{
  \item{pkgname}{
  Name des Pakets, das initialisiert werden soll (Textwert). 
}
  \item{admin.data}{
  Name der \emph{Datei} mit den Verwaltungsdaten, ohne Pfad, aber mit 
  Dateiendung (Textwert). Die Datei muss im Unterordner \code{data} 
  des Pakets liegen. 
}
  \item{envir}{
  \emph{Datenobjekt}, das die Verwaltungsdaten enthält, eine 
  Auswertungsumgebung mit dem Namen \code{".<paket>.admin"} 
  (wobei "<paket>" dem Paketnamen entspricht). Kann als 
  Auswertungsumgebungsobjekt oder als Name übergeben werden. Es 
  wird vorausgesetzt, dass sich dieses Datenobjekt im \code{.GlobalEnv} 
  befindet. 
}
}
\author{
  Jörg Beyer <R-Programmierung@t-online.de> 
}
\seealso{
  \code{\link{Initialisierung}}. 
}
\keyword{package}
\keyword{internal}
\keyword{utilities}
\keyword{datasets}
