def library fncch$*20(;cch$*20,___,Getting,hTmp)
	autoLibrary
	if trim$(cch$)='' then Getting=1 else Getting=0
	open #hTmp=fnH: 'Name=[Q]\GLmstr\Company.h[cno],Shr',i,outi,r
	if Getting then
		read #hTmp,using 'form pos 276,C 20',rec=1: cch$ noRec ignore
	else
		rewrite #hTmp,using 'form pos 276,C 20',rec=1: cch$
	end if
	close #hTmp:
	fncch$=cch$
fnend
