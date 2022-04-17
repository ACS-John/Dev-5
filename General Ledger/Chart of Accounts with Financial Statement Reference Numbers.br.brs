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
		pr #255,using L500: dno,ano,sno,d$,br,ir,fr,sbr,sir,sfr pageoflow PgOf
		L500: form pos 1,pic(zzz),x 1,n 6,x 1,pic(zzz),pos 20,c 50,x 3,n 5,x 5,n 5,x 5,n 5,x 7,n 5,x 5,n 5,x 5,n 5
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
		pr #255,using L700: '********** Primary *********','********* Secondary ********'
		L700: form pos 71,c 28,pos 103,c 28,skip 2
		pr #255,using L720: 'Bal Sheet','Income','Chg Fin','Bal Sheet','Income','Chg Fin'
		L720: form pos 71,c 9,x 4,c 6,x 2,c 7,x 4,c 9,x 4,c 6,x 2,c 7
		pr #255,using L800: '  Account','Description','Reference','Reference','Position','Reference','Reference','Position'
		L800: form pos 2,c 9,pos 38,c 11,pos 71,c 9,x 1,c 9,x 1,c 8,x 4,c 9,x 1,c 9,x 1,c 8
		pr #255: ''
return ! /r

Xit: fnXit

include: ertn
