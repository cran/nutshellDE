\name{Yosemite}
\Rdversion{1.1}
\alias{Yosemite}
\docType{data}
\title{
Profildaten des Yosemite Valley
} % Kodierungsprobleme in PDF-Ausgabe mit R 2.12.1 => Höhenprofildaten ...
\description{
  Die Matrix „\code{Yosemite}“ enthält Höhenprofildaten des Yosemite Valley im 
  Bereich des \emph{Half Dome}. Die Daten wurden über den \emph{National Map 
  Seamless Server} der Kartografiebehörde der Vereinigten Staaten gewonnen. 
}
\usage{data(Yosemite)}
\details{
  Das Raster des geographischen Ausschnitts, das von der Matrix „\code{Yosemite}“ 
  abgebildet wird, hat eine Ausdehnung von 562 Spalten mal 253 Zeilen. 

  Die Höheninformationen werden vom Messdatenserver im \emph{GridFloat}-Format 
  geliefert, das topologische Daten als fortlaufenden binären Strom aus 
  4-Byte-Gleitkommazahlen speichert. Der Datenstrom folgt der Datenmatrix 
  oben beginnend zeilenweise von links nach rechts. 
  Zu den Rohdaten gehört immer eine Header-Datei mit Angaben zu den Dimensionen 
  des Ausschnitts und der Byte-Anordnung (\emph{Endianness}) -- ohne diese 
  zusätzlichen Angaben lässt sich ein \emph{GridFloat}-Binärstrom nicht korrekt 
  einlesen und interpretieren. 

  Die Daten wurden zunächst mit der Funktion \code{\link[base]{readBin}()} 
  in einen Vektor importiert und anschließend mit \code{\link[base]{matrix}()} 
  in eine Matrix der korrekten Dimensionalität überführt. Da \code{matrix()} 
  die Datenstruktur per Voreinstellung \emph{spaltenweise} füllt, ist darauf 
  zu achten, dass beim Umwandeln eines Vektors mit \emph{GridFloat}-Daten der 
  Parameter \code{matrix(byrow = TRUE)} gesetzt wird -- andernfalls wird der 
  Vektor in eine \emph{gestürzte} Matrix eingelesen! 

  Die \code{Yosemite}-Daten eigen sich sehr gut zur Veranschaulichung von 
  3D-Grafikfunktionen in \R. Für eine korrekte Darstellung müssen die Daten 
  i.d.R. gestürzt und „auf den Kopf gestellt“ werden, weil die 3D-Funktionen 
  in \R die Zeilen und Spaltenindizes der Datenmatrix als 
  \emph{x-/y-}Koordinaten mit Ursprung unten links interpretieren 
  (vergl. Beispiel). 

  Genaue Anweisungen zur Erzeugung dieses und ähnlicher Datensätze erhalten 
  Sie in Kap. 14 des Buchs. 
}
\source{
  Die „\code{Yosemite}“-Daten wurden über die Web-Schnittstelle des 
  \emph{National Map Seamless Server} der Kartografiebehörde der 
  Vereinigten Staaten abgerufen: \cr
  \url{http://seamless.usgs.gov/website/seamless/viewer.htm}. 
}
\references{
  Die Daten sind Teil der Beispiele der deutschen Ausgabe von 
  \emph{R in a Nutshell} des O'Reilly Verlags. Rechtliche Informationen 
  finden Sie in der Datei \code{LICENSE} sowie hier: \link{nutshellDE-Paket}. 

  \emph{R in a Nutshell}, Kap. 14. 
}
\seealso{
  \code{\link[base]{readBin}()}, 
  \code{\link[base]{matrix}()}. 
}
\examples{
\dontrun{
## Daten laden 
data(Yosemite)

## Rechte Spaltenindizes für quadratischen Ausschnitt bestimmen 
Spalten.rechts <- seq(from = (ncol(Yosemite) - nrow(Yosemite) + 1), 
                      to   =  ncol(Yosemite))

## Rechte/östlich gelegene Yosemite-Daten mit Half Dome auswählen 
HalfDome <- Yosemite[, Spalten.rechts]

## Hilfsvektor mit den umgekehrten Zeilenindizes anlegen 
Zeilen.kopfunter <- seq(from = nrow(Yosemite), to = 1, by = -1)

## Half-Dome-Daten transformieren und plotten 
persp(x = t(HalfDome[Zeilen.kopfunter, ]), 
      xlab = "Half Dome", 
      col = gray(0.25), border = NA, 
      theta  = 225, phi  = 20, expand = 0.15, 
      ltheta =  45, lphi = 20, shade  = 0.75)
}
}
\keyword{datasets}
