# ====================================================================
# Funktionen zum Abfragen von Yahoo!-Finanzen: 
# --------------------------------------------------------------------
# Funktion zum Abfragen von Börsenkursen 
# Autoren: Joseph Adler 
#          J\u00F6rg Beyer (Fehlerkorrekturen, Fehlertoleranz) 
# angelegt: 2009
# ge\u00E4ndert: 2011-01-18 

Kurse.abfragen <- function(ticker, 
  von = if (missing(bis)) (Sys.Date() - 365) else (bis - 365), 
  bis = if (missing(von))  Sys.Date()        else (von + 365), 
  intervall = "d") 
{ 
  if (von > bis) { 
    Spanne <- c(von, bis); von <- Spanne[2]; bis <- Spanne[1]; 
  }; 
  
  # URL-Bausteine definieren 
  URL.Basis     <- "http://ichart.finance.yahoo.com/table.csv?";
  TickerCode    <- paste(sep = "", "s=", ticker);
  
  # !!! Monate m\u00FCssen von '00' bis '11' durchnummeriert sein, also 
  #     muss auf die korrekte Formatierung geachtet werden !!! 
  von.MM        <- paste(sep = "", "&a=", 
    formatC((as.integer(format(von, "%m")) - 1), width = 2, flag = "0"));
  von.TT        <- paste(sep = "", "&b=", format(von, "%d"));
  von.JJJJ      <- paste(sep = "", "&c=", format(von, "%Y"));
  
  bis.MM        <- paste(sep = "", "&d=", 
    formatC((as.integer(format(bis, "%m")) - 1), width = 2, flag = "0"));
  bis.TT        <- paste(sep = "", "&e=", format(bis, "%d"));
  bis.JJJJ      <- paste(sep = "", "&f=", format(bis, "%Y"));
  
  Intervall     <- paste(sep = "", "&g=", intervall);
  URL.Rest      <- "&ignore=.csv";
  
  # URL zusammensetzen 
  URL           <- URLencode(paste(sep = "", 
                                   URL.Basis, TickerCode, 
                                   von.MM, von.TT, von.JJJJ, 
                                   bis.MM, bis.TT, bis.JJJJ, 
                                   Intervall, URL.Rest));
  
  # Daten bei Yahoo!-Finanzen abholen 
  dumm.gelaufen <- function(Error) { 
    cat("Warnmeldung:\n", 
        Error[[1]], "\n", 
        "Ticker \"", ticker, "\" wurde nicht gefunden (", URL, ")\n\n", 
        sep = "");
    NULL;
  } 
  Yahoo         <- tryCatch(read.csv(URL), warning = dumm.gelaufen);
  
  # Ticker-K\u00FCrzel hinzuf\u00FCgen (und Spaltennamen \u00FCbersetzen) 
  if (! is.null(Yahoo)) { 
    Yahoo   <- cbind(Ticker = ticker, Yahoo);
    colnames(Yahoo) <- c("Ticker", "Datum", "Er\u00F6ffnung", "Hoch", "Tief", 
                         "Schluss", "Volumen", "Adj.Schluss");
    Yahoo$Datum <- as.Date(Yahoo$Datum, format = "%Y-%m-%d");
  } 
  return(Yahoo);
} 



# --------------------------------------------------------------------
# Funktion zum Abfragen von Wertpapieren 
# Autoren: Joseph Adler 
#          J\u00F6rg Beyer (Fehlertoleranz) 
# angelegt: 2009
# ge\u00E4ndert: 2011-01-18 

Wertpapiere.abfragen <- function(ticker, 
  von = if (missing(bis)) (Sys.Date() - 365) else (bis - 365), 
  bis = if (missing(von))  Sys.Date()        else (von + 365), 
  intervall = "d") 
{ 
  Yahoo <- NULL; 
  for (tkr in ticker) { 
    if (is.null(Yahoo)) { 
      Yahoo <- Kurse.abfragen(ticker = tkr, 
                              von = von, bis = bis, 
                              intervall = intervall);
    } else { 
      Yahoo <- rbind(Yahoo, 
               Kurse.abfragen(ticker = tkr, 
                              von = von, bis = bis, 
                              intervall = intervall));
    } 
  } 
  return(Yahoo);
} 



# ====================================================================
