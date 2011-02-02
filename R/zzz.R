# ============================================================================== 
# Paketinitialisierungscode, wird beim Laden automatisch ausgef\u00FChrt. 
# Autor: J\u00F6rg Beyer 
# angelegt: 2010-07-25 
# ge\u00E4ndert: 2011-01-17 

.First.lib <- function(libname, pkgname)
{ 
	if(interactive()) { 
		init <- .initialize.package(pkgname, admin.data = "Admin.rda"); 
	} 
	invisible(TRUE); 
} 



# ============================================================================== 
