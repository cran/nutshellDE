\name{Google.Suche.nach.Bacon}
\Rdversion{1.1}
\alias{Google.Suche.nach.Bacon}
\docType{data}
\title{
Google-Anfragen mit dem Suchbegriff "Bacon"
} % Kodierungsprobleme in PDF-Ausgabe mit R 2.12.1
\description{
  Der Datenrahmen „\code{Google.Suche.nach.Bacon}“ besteht aus den Namen der 
  15 amerikanischen Städte, in denen mit \emph{Google} am häufigsten nach dem 
  Begriff „Bacon“ (Schinken) gesucht wurde. 
  Die Städte sind nach Häufigkeit in eine Rangreihe gebracht, ein höherer 
  Index entspricht mehr Anfragen aus dieser Region. (Die Ränge enthalten 
  Bindungen, die Rangziffern reichen von 80 bis 100.)
}
\usage{data(Google.Suche.nach.Bacon)}
\format{
  Datenrahmen mit 15 Beobachtungen der folgenden 2 Variablen:
  \describe{
    \item{\code{Stadt}}{[\code{city}] Name der Stadt (Text).}
    \item{\code{Rang}}{[\code{rank}] Rang der Anfrage (Gleitkommazahl).}
  }
}
\details{
  Der Datensatz wurde mithilfe von \emph{Google Insights} gewonnen. Die Daten 
  umfassen den Zeitraum von 2004 bis 2009, die Abfrage wurde am 5. September 
  2009 durchgeführt. 
}
\source{
  Die Originaldaten wurden mit der HTTP-Anfrage \cr
  \url{http://www.google.com/insights/search/#q=bacon&cmpt=q} \cr
  über \emph{Google Insights} gewonnen. 
}
\references{
  Die Daten sind Teil der Beispiele der deutschen Ausgabe von 
  \emph{R in a Nutshell} des O'Reilly Verlags. Rechtliche Informationen 
  finden Sie in der Datei \code{LICENSE} sowie hier: \link{nutshellDE-Paket}. 

  \emph{R in a Nutshell}, Kap. 7. 
}
\seealso{
  \code{\link{ZR.Truthahnpreise}}, 
  \code{\link{ZR.Schinkenpreise}}, 
  \code{\link{Verzehr}}. 
}
\examples{
\dontrun{
## Daten laden
data(Google.Suche.nach.Bacon)

## Unterschied zwischen \link[base]{class} und \link[base]{typeof}
class(Google.Suche.nach.Bacon)
typeof(Google.Suche.nach.Bacon)
}
}
\keyword{datasets}
