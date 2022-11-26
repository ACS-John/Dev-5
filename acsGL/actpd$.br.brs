! Replace S:\acsGL\actpd$.br

def library fnactpd$(; actpd$,___,getting,tmp)
	autoLibrary
	if trim$(actpd$)='' then getting=1
	open #tmp=fnH: 'Name=[Q]\GLmstr\Company.h[cno],Shr',i,outi,r
	if getting then
		read #tmp,using 'form pos 270,C 6',rec=1: actpd$ noRec CLOSE_TMP
	else
		rewrite #tmp,using 'form pos 270,C 6',rec=1: actpd$
	end if
	CLOSE_TMP: !
	close #tmp:
	fnactpd$=actpd$
fnend
