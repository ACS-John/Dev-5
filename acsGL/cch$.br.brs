! Replace S:\acsGL\cch$.br
 
def library fncch$*20(;cch$*20)
		autoLibrary
		dim cch$*20
		fncno(cno)
		get=1 : put=2
		if trim$(cch$)="" then get_or_put=get else get_or_put=put
		open #tmp=fngethandle: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,outIn,relative
		if get_or_put=get then : _
			read #tmp,using "Form POS 276,C 20",rec=1: cch$ noRec CLOSE_TMP
		if get_or_put=put then : _
			rewrite #tmp,using "Form Pos 276,C 20",rec=1: cch$
CLOSE_TMP: close #tmp:
		fncch$=cch$
Xit: !
fnend
