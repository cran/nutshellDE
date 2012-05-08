# ============================================================================== 
# Paket-Initialisierungscode: 
# Laden der Verwaltungsdaten und -- abh\u00E4ngig von den dort abgelegten 
# Informationen -- Anpassen des Pakets an Umgebungsbedingungen. 
# Wird im Normalfall von ".First.lib()", *nicht* vom Anwender 
# aufgerufen. 
# Autor: J\u00F6rg Beyer (Copyright 2010 - heute)
# angelegt: 2010-07-25 
# ge\u00E4ndert: 2012-05-07 19:33:33 +0200 (MESZ)

.initialize.package <- function(pkgname, admin.data = "Admin.rda") 
{ 
	# (0)   Eingabeargumente pr\u00FCfen 
	if(! is.character(pkgname) || length(pkgname) != 1) { 
		errStr 			<- paste(sep = "", 
			"Paketname ist kein Textwert der L\u00E4nge 1: ", 
			paste(pkgname, sep = "", collapse = ", ") 
			); 
		stop(errStr); 
	} 
	
	if(! is.character(admin.data) || length(admin.data) != 1) { 
		errStr 			<- paste(sep = "", 
			"Name der Verwaltungsdatendatei ist kein Textwert der L\u00E4nge 1: ", 
			paste(pkgname, sep = "", collapse = ", ") 
			); 
		stop(errStr); 
	} 
	
	
	
	# (1)   Status des Pakets: M\u00FCssen wir \u00FCberhaupt etwas tun? 
	# (1.1) Wenn die Funktion *nicht* aus einer interaktiven Umgebung heraus 
	#       aufgerufen wird  =>  R\u00FCckgabe NULL und Ende 
	if(! interactive()) { 
		return(NULL); 
	} 
	
	
	# (1.2) Datensatz mit Informationen zur Paketverwaltung laden 
	adminEnvirName 		<- .getPkgAdminData(pkgname, admin.data = admin.data);  
	adminEnvir 			<- get(adminEnvirName, envir = .GlobalEnv);  
	on.exit( remove(list =     adminEnvirName, envir = .GlobalEnv)); 
	
	
	# (1.3) Wird das Paket gerade zum ersten Mal geladen? 
	#       Soll es \u00FCberhaupt initialisiert werden? 
	#       Wenn nein, unauff\u00E4llig wieder raus aus dem Aufruf... 
	if (get("is.initialized", envir = adminEnvir, inherits = FALSE) || 
		get("skip.init",      envir = adminEnvir, inherits = FALSE)) { 
		return(NULL); 
	} 
	
	
	
	# (2)   Paket soll initialisiert werden
	if (! get("skip.init",    envir = adminEnvir, inherits = FALSE)) { 
		
		# (2.0) Vorbereitung... 
		# ... sonst erfolgt die Ausgabe verz\u00F6gert! 
## ***  Funktioniert um's Verrecken nicht -- Ausgabe erfolgt *immer* verz\u00F6gert! 
		oldWarn 		<- options("warn"); 
		options("warn" = 1L); 
		on.exit(options(oldWarn), add = TRUE); 
		
		msgStr 			<- paste(sep = "", 
			"Das Paket \"", pkgname, "\" wurde noch nicht dauerhaft an Ihre Plattform angepasst. \n", 
			"Dieser Schritt l\u00E4uft automatisch ab und dauert normalerweise einen Moment. \n", 
			"Pr\u00FCfung l\u00E4uft -- bitte etwas Geduld... \n", 
			""); 
		
		packageStartupMessage(msgStr, appendLF = FALSE); 
		flush(stderr()); 
		
		
		# (2.1) Namen von Datenobjekten anpassen 
		#       (plattformabh\u00E4ngige Zeichenkodierung von Umlauten) 
		if (get("rename.Sets", envir = adminEnvir, inherits = FALSE)) { 
			res 		<- .adjustDataobjectNames(pkgname, 
													admin.data = admin.data, 
													envir = adminEnvir); 
		} 
		
		
		
		# (3)   War die Initialisierung erfolgreich? 
		# (3.1) Abschlie\u00dfende Statusmeldung montieren und ausgeben 
		if ((sum(      res,  na.rm = TRUE) == length(res)) || 
			(sum(is.na(res), na.rm = TRUE) == length(res))) { 
			
			# Dem Paket dauerhaft mitteilen, dass die Initialisierung 
			# erfolgreich war; Verwaltungsdaten speichern 
			assign("is.initialized",  TRUE, envir = adminEnvir); 
			
			if(sum(res) >= 1) { 
				msgStr 	<- paste(sep = "", 
				"\n", 
				"Alle n\u00F6tigen Daten wurden aktualisiert. Wenn Sie das Paket \n",  
				"in Zukunft laden, werden Sie diese Meldung nicht mehr sehen. \n", 
				"\n"); 
			} 
			else { 
				msgStr 	<- paste(sep = "", 
				"\n", 
				"Es mussten keine Daten aktualisiert werden. Wenn Sie das Paket \n",  
				"in Zukunft laden, werden Sie diese Meldung nicht mehr sehen. \n", 
				"\n"); 
			} 
			packageStartupMessage(msgStr, appendLF = FALSE); 
			flush(stderr()); 
			
			initRes <- TRUE; 
		} 
		
		else { 
			msgStr 		<- paste(sep = "", 
				"\n", 
				"Die Initialisierung des Pakets war leider nicht erfolgreich. \n", 
				"Sie k\u00F6nnen (mit kleinen Einschr\u00E4nkungen) mit den Beispieldaten \n", 
				"arbeiten, werden diese Meldungen aber bei jedem Laden des Pakets \n", 
				"sehen, bis das Problem identifiziert und behoben ist. \n", 
				"\n"); 
			packageStartupMessage(msgStr, appendLF = FALSE); 
			flush(stderr()); 
			
			initRes <- FALSE; 
		} 
		
		
		# (3.2) 
		if(get("changed.admindata", envir = adminEnvir, inherits = FALSE)) { 
			saveRes 	<- save(list = adminEnvirName, 
								envir = .GlobalEnv, 
								file  = system.file(package = pkgname, 
													"extdata", admin.data)); 
		} 
		return(initRes); 
	} 
} 



# ============================================================================== 
# Hilfsfunktion: 
# Verwaltungsdaten des Pakets laden 
# Wird im Normalfall von ".initialize.package()", *nicht* vom Anwender 
# aufgerufen. 
# Autor: J\u00F6rg Beyer (Copyright 2010 - heute)
# angelegt: 2010-07-25 
# ge\u00E4ndert: 2012-05-07 19:33:33 +0200 (MESZ)

.getPkgAdminData <- function(pkgname, admin.data = "Admin.rda") 
{ 
	# 
	adminEnvir 			<- sprintf(".%s.admin", pkgname); 
	if( exists(adminEnvir, envir = .GlobalEnv)) { 
		invisible(get("pkg.admindata", 
						envir = get(adminEnvir, 
									envir = .GlobalEnv), 
						inherits = FALSE)); 
	} 
	
	
	# 
	conditionHandler 	<- function(cond) 
	{ 
		errStr 			<- paste(sep = "", 
			"\n", 
			"Die Datei mit den Paketverwaltungsdaten ", 
			sprintf("(\"%s\", \"%s\") ",  get("admin.data"), get("datapath"  )), "\n", 
			"konnte nicht geladen werden. \n", 
			"Bitte pr\u00FCfen Sie, ob diese Datei existiert. \n", 
			""); 
		
## *** Logging wird z.Z. nicht unterst\u00FCtzt 
##		.write.pkgAdminLog(msgStr); 
		stop(errStr, call. = FALSE); 
	} 
	
	
	# 
	datapath 			<- system.file(package = pkgname, "extdata"); 
	datafile 			<- system.file(package = pkgname, "extdata", admin.data); 
	
	adminEnvir 			<- tryCatch(load(datafile, envir = .GlobalEnv), 
								error   = conditionHandler, 
								warning = conditionHandler); 
	
	assign("changed.admindata", FALSE, envir = get(adminEnvir, envir = .GlobalEnv)); 
	invisible(adminEnvir); 
} 



# ============================================================================== 
# Hilfsfunktion: 
# Anpassen von Objektnamen mit Umlauten an die Plattform, auf der 
# das Paket installiert ist. 
# Wird im Normalfall von ".initialize.package()", *nicht* vom Anwender 
# aufgerufen. 
# Autor: J\u00F6rg Beyer (Copyright 2010 - heute)
# angelegt: 2010-07-25 
# ge\u00E4ndert: 2012-05-07 19:33:33 +0200 (MESZ)

.adjustDataobjectNames <- function(pkgname, admin.data, envir) 
{ 
	# (0)   Eingabeargumente pr\u00FCfen 
## *** Zweite Bedingung vielleicht zu liberal... 
	if(missing(envir) || 
		(! class(envir) %in% c("character", "environment"))) { 
		envir 			<- sprintf(".%s.admin", pkgname); 
	} 
	
## *** Hier fehlt evtl. noch eine Abbruchbedingung... (s. auch Anm. oben) 
	if(is.character(envir)) { 
		adminEnvir 		<- get(envir, envir = .GlobalEnv); 
	} 
	else if(is.environment(envir)) { 
		adminEnvir 		<- envir; 
	} 
	
	
	
	# (1)   Objektnamen anpassen 
	# (1.1) ... oder vielleicht doch nicht? 
	Sets.2b.renamed 	<- get("Sets.2b.renamed", 
								envir = adminEnvir, inherits = FALSE); 
	sets.chk 			<- vector(length = length(names(Sets.2b.renamed))); 
	if (! length(sets.chk)) { 
		return(NULL); 
	} 
	
	
	# (1.2) Erste Statusmeldung f\u00FCr den Anwender 
	msgStr 				<- paste(sep = "", 
		"\n", 
		"=>  Objektnamen mit Umlauten und anderen Nicht-ASCII-Zeichen m\u00FCssen \n", 
		"neu eingerichtet werden. \n", 
		""); 
	packageStartupMessage(msgStr, appendLF = FALSE); 
	flush(stderr()); 
	
	
	processObjectNames <- function(set, obj.inx, obj.chk) 
	{ 
		paste(sprintf("    \"%s\"  =>  \"%s\" ", 
						set$old.names[ obj.inx[ obj.chk ]], 
						set$new.names[ obj.inx[ obj.chk ]] ), 
				sep = "", collapse = "\n"); 
	} 
	
	
	processObjectsMessage <- function(set, obj.chk) 
	{ 
		# 
		obj.inx 		<- which(! set$is.renamed ); 
		passed 			<- obj.inx[  obj.chk ]; 
		failed 			<- obj.inx[! obj.chk ]; 
		msg.p <- msg.f <- ""; 
		
		
		# 
		if (length(passed) == 1) { 
			msg.p <- paste(sep = "", 
				"o   Das folgende Datenobjekt wurde erfolgreich umbenannt: \n", 
				processObjectNames(set, obj.inx, passed) 
				); 
		} 
		
		else if (length(passed) > 1) { 
			msg.p <- paste(sep = "", 
				sprintf(
				"o   Die folgenden %s Datenobjekte wurden erfolgreich umbenannt: \n", 
				length(passed)), 
				processObjectNames(set, obj.inx, passed)); 
		} 
		
		
		# 
		if (length(failed) == 1) { 
			msg.f <- paste(sep = "", 
				"o   Das folgende Datenobjekt konnte nicht umbenannt werden: \n", 
				processObjectNames(set, obj.inx, failed)); 
		} 
		
		else if (length(failed) >  1) { 
			if  (length(passed) >= 1) { 
				msg.f <- paste(sep = "", 
				sprintf(
				"o   Die folgenden %s Datenobjekte konnten nicht umbenannt werden: \n", 
				length(failed)), 
				processObjectNames(set, obj.inx, failed)); 
			} 
			
			else { 
				msg.f <- paste(sep = "", 
				sprintf(
				"o   Keins der folgenden %s Datenobjekte konnte umbenannt werden: \n", 
				length(obj.inx)), 
				processObjectNames(set, obj.inx, failed)); 
			} 
		} 
		
		
		# 
		if      (msg.p != "" && msg.f == "") { 
			return(msg.p); 
		} 
		else if (msg.p == "" && msg.f != "") { 
			return(msg.f); 
		} 
		else if (msg.p != "" && msg.f != "") { 
			return(paste(sep = "", 
				msg.p, "\n\n", msg.f)); 
		} 
	} 
	
	
	processFileMessage <- function(objectStr, fileStatusStr, 
									setname, datapath) 
	{ 
		msgStr 			<- paste(sep = "", 
				objectStr, 
				"\n\n", 
				"Die Datendatei \n", 
				sprintf("    \"%s\"  (\"%s\") \n%s. \n", 
						setname, datapath, fileStatusStr), 
				"\n"); 
	} 
	
	
	# (1.3) Schleife \u00FCber Liste mit Datendateien, die bearbeitet werden m\u00FCssen 
	#       => plattformabh\u00E4ngige Zeichenkodierung von Umlauten in Objektnamen 
	for (s in seq(along.with = names(Sets.2b.renamed))) 
	{ 
		# Datendatei in neue Sandbox laden (um jede Interferenz 
		# mit der Benutzerumgebung sicher auszuschlie\u00dfen) 
		setname 		<- names(Sets.2b.renamed)[ s ]; 
		set 			<- Sets.2b.renamed[[ setname ]]; 
		obj.inx 		<- which(! set$is.renamed ); 
		obj.chk 		<- vector(length = length(obj.inx)); 
		if(length(obj.chk) == 0) { 
			next; 
		} 
		
		datapath 		<- system.file(package = pkgname, "data"); 
		datafile 		<- system.file(package = pkgname, "data", 
										sprintf("%s.%s", setname, set$extension)); 
		if (! file.exists(datafile)) { 
			msgStr 		<- paste(sep = "", 
				"\n", 
				"Die Datendatei ", 
				"  \"", sprintf("%s.%s", setname, set$extension), "\"  ", 
				"(\"", datapath, "\") \n", 
				"wurde nicht gefunden. Sie enth\u00E4lt ", 
				ifelse(   length(obj.chk) == 1, 
					"1 Objekt, das umbenannt werden sollte. \n", 
					paste(length(obj.chk), 
						"Objekte, die umbenannt werden sollten. \n")), 
				"Das Problem kann behoben werden, indem Sie sicherstellen, dass R die \n", 
				"gesuchte Datei finden kann (Ablageort kontrollieren, ggf. Datei umbenennen). \n", 
				""); 
			packageStartupMessage(msgStr, appendLF = FALSE); 
			flush(stderr()); 
			
			next; 
		} 
		
		sandbox 		<- new.env(hash = TRUE); 
		load(datafile, envir = sandbox); 
		
		
		# Schleife: Objekte dieser Datendatei durchlaufen und umbenennen 
		for (n in seq(along.with = obj.chk)) 
		{ 
			# Abbruch des aktuellen Schleifenlaufs, wenn das Objekt 
			# nicht gefunden werden kann 
			o 			<- obj.inx[n]; 
			if(! exists(set$old.names[o], envir = sandbox, inherits = FALSE)) { 
				next; 
			} 
			
			# Altes Objekt dem neuen zuweisen, altes l\u00F6schen 
			assign(     set$new.names[o], 
					get(set$old.names[o], envir = sandbox, inherits = FALSE), 
										  envir = sandbox); 
			remove(list=set$old.names[o], envir = sandbox); 
			
			# Statusinformationen der Datensatzdatei aktualisieren 
			Sets.2b.renamed[[ setname ]][["is.renamed"]][o] <- TRUE; 
			obj.chk[n] 	<- TRUE; 
		} 
		
		
		# Gesamten Inhalt der Sandbox in aktuelle Datendatei sichern, 
		# wenn mindestens ein Objekt umbenannt wurde 
		if (sum(obj.chk, na.rm = TRUE) >= 1) { 
			Sets.2b.renamed[[ setname ]][["old.names" ]] <- 
			Sets.2b.renamed[[ setname ]][["old.names" ]][obj.inx[! obj.chk]]; 
			
			Sets.2b.renamed[[ setname ]][["new.names" ]] <- 
			Sets.2b.renamed[[ setname ]][["new.names" ]][obj.inx[! obj.chk]]; 
			
			Sets.2b.renamed[[ setname ]][["is.renamed"]] <- 
			Sets.2b.renamed[[ setname ]][["is.renamed"]][obj.inx[! obj.chk]]; 
			
			assign("changed.admindata", TRUE, envir = adminEnvir); 
			save(list = ls(sandbox), envir = sandbox, file = datafile); 
		} 
		
		
		# Statusmeldung montieren und ausgeben 
		msgStr 			<- processObjectsMessage(set, obj.chk); 
		if (sum(obj.chk, na.rm = TRUE) == length(obj.chk)) { 
			msgStr 		<- processFileMessage( 
					msgStr, "wurde aktualisiert und gespeichert", 
					setname, datapath); 
			
			# Statusinformationen der Datensatzdatei aktualisieren 
			sets.chk[s] <- TRUE; 
		} 
		
		else if (sum(obj.chk, na.rm = TRUE) >= 1) { 
			msgStr 		<- processFileMessage( 
					msgStr, "wurde nicht vollst\u00E4ndig aktualisiert", 
					setname, datapath); 
		} 
		
		else if (sum(obj.chk, na.rm = TRUE) == 0) { 
			msgStr 		<- processFileMessage( 
					msgStr, "wurde nicht aktualisiert", 
					setname, datapath); 
		} 
		
		packageStartupMessage(msgStr, appendLF = FALSE); 
		flush(stderr()); 
	} 
	
	
	# Status der Datenpakete in den Verwaltungsdaten aktualiseren 
	# R\u00FCckgabe des Kontrollvektors 
	assign("Sets.2b.renamed", Sets.2b.renamed, 	envir = adminEnvir); 
	return(sets.chk); 
}



# ============================================================================== 
