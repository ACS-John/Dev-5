def library fnwait(message$*40,stopable)
	autoLibrary
	if trim$(message$)="" then message$="Please wait..."
	fnStatus(message$)
fnend

