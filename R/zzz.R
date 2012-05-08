# ============================================================================== 
# Paketinitialisierungscode, wird beim Laden automatisch ausgef\u00FChrt. 
# Autor: J\u00F6rg Beyer 
# angelegt: 2010-07-25 
# ge\u00E4ndert: 2012-05-07 18:26:09 +0200 (MESZ)

.onAttach <- function(libname, pkgname)
{ 
	if(interactive()) { 
		init <- .initialize.package(pkgname, admin.data = "Admin.rda"); 
	} 
	invisible(TRUE); 
} 



# ============================================================================== 
