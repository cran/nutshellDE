# Wenn Sie diese Datei mit den Anweisungen 
# 
#   skript <- system.file(package = "nutshellDE", 
#                         "extras", "Paketliste_einlesen.R") 
#   source(skript, encoding = "UTF-8") 
# 
# einlesen, finden Sie einen Datenrahmen mit dem Namen "Paketliste" 
# im Arbeitsbereich. 

datafile 				<- system.file(package = "nutshellDE", 
									"extras", "Paketliste.csv"); 

Paketliste 				<- read.csv2(datafile, stringsAsFactors = FALSE, 
									fileEncoding = "UTF-8", 
									na.strings = c("NA", "")); 

Paketliste$Plattform 	<- factor(Paketliste$Plattform, 
									levels = c("R", "Windows")); 

Paketliste$Status 		<- factor(Paketliste$Status, 
									levels = c("verwendet", "erw\u00E4hnt")); 

Paketliste$Level 		<- factor(Paketliste$Level, 
									levels = c("\u00DCbungen", "Voraussetzung")); 



