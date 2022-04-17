autoLibrary
on error goto Ertn
fnTop(program$)
open #hAcct=fnH: 'Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno],Shr',i,i,k
fnopenprn
pr #255,using 'form pos 1,C 20,Cc 90': date$('mm/dd/yy'),env$('cnam')
pr #255,using 'form pos 1,C 20,Cc 90': time$,'Chart of Accounts'
pr #255,using 'form pos 1,Cc 130': date$('Month DD, CCYY')
pr #255:
gosub PrSecondaryHeaders
MainLoopTop: ! r:
	dim d$*50
	read #hAcct,using L430: dno,ano,sno,d$,br,sbr,ir,sir,fr,sfr eof Finis
	L430: form pos 1,n 3,n 6,n 3,c 50,6*pd 3
	gl3$=cnvrt$('N 3',dno)&cnvrt$('N 6',ano)&cnvrt$('N 3',sno)
	if trim$(gl1$)<>'' and gl3$<gl1$ then goto MainLoopTop
	if trim$(gl2$)<>'' and gl3$>gl2$ then goto Finis
		pr #255,using L540: dno,ano,sno,d$ pageoflow PgOf
		L540: form pos 1,pic(zzz),x 1,n 6,x 1,pic(zzz),pos 20,c 50,skip 1
goto MainLoopTop ! /r
Finis: ! r:
	close #hAcct:
	fncloseprn
goto Xit ! /r
PgOf: ! r:
	pr #255: newpage
	pr #255,using 'form pos 1,C 20,Cc 90': date$('mm/dd/yy'),env$('cnam')
	pr #255: time$;tab(57);'Chart of Accounts'
	pr #255,using 'form pos 1,Cc 130': date$('Month DD, CCYY')
	pr #255:
	gosub PrSecondaryHeaders
Continue ! /r
PrSecondaryHeaders: ! r:
		pr #255: ''
		pr #255,using L750: '  Account','Description'
		L750: form pos 2,c 9,pos 38,c 11
return ! /r

Xit: fnXit

include: ertn
