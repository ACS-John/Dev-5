	autoLibrary
	on error goto Ertn
 
	fnTop(program$,"Client Directory")
	fnRead30Categories(mat cat$)
	dim flo$(10)
	dim fli$(10)
	for j=1 to 10
		flo$(j)=str$(j+10)&",30,C 30,N"
		fli$(j)=str$(j+10)&",62,N 1,U,N"
	next j
	dim cat$(30)*30
	dim catcode(10)
	namtab=44-len(rtrm$(env$('cnam')))/2
	open #1: "Name=S:\Core\Data\acsllc\Client.h[cno],KFName=S:\Core\Data\acsllc\Client-Idx.h[cno],Shr",i,i,k

	pr newpage
	pr f "3,15,C 65,N": "You have the option to get the client directory sorted by "
	pr f "4,10,c 70,n": "category.  Place a 1 by any category you want printed.  Leave all"
	pr f "5,10,c 70,n": "items blank for numeric order."
	pr f mat flo$: mat cat$(1:10)
	L340: !
	input fields mat fli$: mat catcode conv L340
	if sum(catcode)=0 then numprint=1
	pr newpage
	! pr f "10,25,c 48,n": "CLIENT DIRECTORY IN PROCESS"
	! pr f "23,2,c 30,n": "Press F5 to stop"
	fnopenprn

	if numprint=1 then goto L440
	for j=1 to 10
		if catcode(j)=0 then goto L790
		L440: !
		gosub PrHeading
		do
			dim z$*5,a$(5)*30,ph$*12
			dim cm$*70
			dim dd(10)
			dim ph2$*12
			read #1,using L460: z$,mat a$,ph$,pno,mye,mat dd,ph2$,cm$ eof Eof1
			L460: form pos 1,c 5,5*c 30,c 12,pos 179,n 9,n 2,pos 190,10*pd 3,pos 260,c 12,pos 305,c 70
			if numprint =1 or dd(j)>0 then 
			
				pr #255,using L500: z$,a$(1),"Bus Phone:",ph$,mye,pno
				L500: form pos 2,c 5,pos 8,c 30,pos 39,c 10,pos 50,c 12,pos 70,pic(zzz),pos 76,pic(zzzzzzzzz),skip 1
				pr #255,using L520: a$(2),"Home Phone:",ph2$
				L520: form pos 8,c 30,pos 39,c 11,pos 51,c 30,skip 1
				pr #255,using L540: a$(3),"Contact:",a$(4) pageoflow PgOf
				L540: form pos 8,c 30,pos 39,c 8,pos 48,c 30,skip 1
				pr #255,using L560: cm$
				L560: form pos 8,c 70,skip 2
				goto L450
				PgOf: pr #255: newpage
				gosub PrHeading
				L450: !
			end if
		loop

		Eof1: !
		if numprint=1 then goto L800
		pr #255: newpage
		restore #1,key>="     ": nokey L800
		L790: !
	next j
	L800: !
	close #1: ioerr ignore
	fncloseprn
goto Xit

PrHeading: ! r:
	p1=p1+1
	pr #255,using L630: date$,env$('cnam'),"PAGE",p1
	L630: form skip 3,pos 1,c 8,pos namtab,c 40,pos 76,c 5,n 4,skip 1
	pr #255,using L650: time$,"CLIENT DIRECTORY"
	L650: form pos 1,c 8,pos 36,c 16,skip 1
	if numprint = 1 then cattab=37 else cattab=44-len(rtrm$(cat$(j)))/2
	if numprint = 1 then pr #255,using L680: "Numeric Order" else pr #255,using L680: cat$(j)
	L680: form pos cattab,c 30,skip 1
	pr #255,using L700: date('mm/dd/ccyy')
	L700: form pos 40,c 10,skip 3
	pr #255,using L720: "Client","Year","Employee"
	L720: form pos 2,c 6,pos 70,c 4,pos 77,c 8,skip 1
	pr #255,using L740: "Number","Name and Address","Client Information","End","In charge"
	L740: form pos 2,c 6,pos 12,c 16,pos 42,c 19,pos 71,c 3,pos 76,c 9,skip 2
return ! /r

Xit: fnXit
include: ertn
