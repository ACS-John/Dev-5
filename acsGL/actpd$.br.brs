! Replace S:\acsGL\actpd$.br
 
def library fnactpd$(;actpd$)
		autoLibrary
		get=1 : put=2
		if trim$(actpd$)="" then get_or_put=1 else get_or_put=2
		fncno(cno)
		open #tmp=fngethandle: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,outIn,relative
		if get_or_put=get then : _
			read #tmp,using "Form POS 270,C 6",rec=1: actpd$ noRec CLOSE_TMP
		if get_or_put=put then : _
			rewrite #tmp,using "Form POS 270,C 6",rec=1: actpd$
CLOSE_TMP: close #tmp:
		fnactpd$=actpd$
Xit: !
fnend
