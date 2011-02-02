# ====================================================================
# Hilfsfunktion zum Faktorisieren von Vektoren 
# Die Zuordnung von Werten und Kategorien wird als Liste \u00FCbergeben, 
# alles weitere wird \u00FCber einen Aufruf von "factor()" erledigt. 
# Autor: J\u00F6rg Beyer (Copyright 2010 - heute)
# angelegt: Juli 2010
# ge\u00E4ndert: 2011-01-18 


faktorisieren <- function(x, codes, ordered = FALSE, exclude = NA) {
  
  if (class(codes) == "list") { codes <- unlist(codes) };
  if (! is.null(exclude)) {
    if (any(is.na(exclude))) {
      codes <- codes[! is.na(codes)];
    }
    codes <- codes[! codes %in% exclude];
  }
  
  x <- factor(x, levels = codes, labels = names(codes), 
              ordered = ordered, exclude = exclude);
  return(x);
}



# ====================================================================
# Hilfsfunktion zum Umwandeln von Missing-Codes in NAs 
# Autor: J\u00F6rg Beyer (Copyright 2010 - heute)
# angelegt: Juli 2010
# ge\u00E4ndert: 2011-01-18 

auf.NA.setzen <- function(x, na.codes, drop.unused.levels = FALSE) {
  if (is.logical(na.codes)) {
    is.na(x)[        na.codes ] <- TRUE;
  } else {
    if(class(x) != class(na.codes)) {
      msgStr <- 
      "Die Argumente \"x\" und \"na.codes\" m\u00FCssen vom gleichen Datentyp sein.\n"; 
      stop(msgStr);
    } else {
      is.na(x)[ x %in% na.codes ] <- TRUE;
    }
  }
  
  if (drop.unused.levels == TRUE && is.factor(x)) {
    x <- x[, drop = TRUE];
  }
  return(x);
}



# ====================================================================
