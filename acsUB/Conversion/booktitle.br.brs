! Replace S:\acsUB\conversion\booktitle
! this program converts a field from ALL CAPITAL LETTERS to Book Title Capitalization

autoLibrary
on error goto Ertn


open #1: 'Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubindex.h[cno],Shr',i,outIn,k
for j=1 to lrec(1)
	dim nam$*30
	read #1,using 'form pos 71,c 30': nam$  eof Done
	rewrite #1,using 'form pos 71,c 30': fn_booktitle$(nam$) 
next j
goto Done
Done: !
	close #1:
	pr 'company number [cno] completed successfully'
goto Xit
def fn_booktitle$*80(x$*80)
		x$=lwrc$(trim$(x$)) : olda=0
		x$(1:1)=uprc$(x$(1:1))
! capitalize anthing after a SPACE
	L130: a=pos(x$,' ',olda) : if a then a+=1 : x$(a:a)=uprc$(x$(a:a)) : olda=a : goto L130 else a=olda=0
	L150: a=pos(x$,'-',olda) : if a then a+=1 : x$(a:a)=uprc$(x$(a:a)) : olda=a : goto L150 else a=olda=0
	L170: a=pos(x$,'/',olda) : if a then a+=1 : x$(a:a)=uprc$(x$(a:a)) : olda=a : goto L170 else a=olda=0
	L190: a=pos(x$,'\',olda) : if a then a+=1 : x$(a:a)=uprc$(x$(a:a)) : olda=a : goto L190 else a=olda=0
	L210: a=pos(x$,'.',olda) : if a then a+=1 : x$(a:a)=uprc$(x$(a:a)) : olda=a : goto L210 else a=olda=0
fn_booktitle$=x$
fnend

Xit: stop

include: ertn

