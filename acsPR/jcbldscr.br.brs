! Replace S:\acsPR\jcBldScr
! builds the jcScrn file and then chains to S:\acsPR\rpNames

	autoLibrary
	on error goto Ertn

	dim fl1$(9),io1$(9),fl2$(15),io2$(15),sc3$(12),fl3$(13),io3$(12)

	for j=1 to 9
		fl1$(j)=str$(j+2)&',2,cr 20,n'
		if j<=1 then io1$(1)=str$(j+2)&',23,C 6,UT,N' : goto L240
		if j>2 then goto L170
		io1$(j)=str$(j+2)&',23,C 40,CUT,N' : goto L240
L170: if j>5 then goto L190
		io1$(j)=str$(j+2)&',23,C 30,UT,N' : goto L240
L190: if j>6 then goto L210
		io1$(j)=str$(j+2)&',23,N 6,UT,N' : goto L240
L210: if j>8 then goto L230
		io1$(j)=str$(j+2)&',23,N 14.2,UT,N' : goto L240
L230: io1$(j)=str$(j+2)&',23,N 2,UT,N'
L240: next j
	for j=1 to 15
		fl2$(j)=str$(j+2)&',2,Cr 20,N'
		if j>1 then goto L290
		io2$(j)=str$(j+2)&',23,N 5,UT,N' : goto L340
L290: if j>2 then goto L310
		io2$(j)=str$(j+2)&',23,C 25,UT,N' : goto L340
		L310: !
		if j>13 then goto L330
		io2$(j)=str$(j+2)&',23,N 14.2,UT,N'
		goto L340
		L330: !
		io2$(j)=str$(j+2)&',23,N 3,UT,N'
		L340: !
	next j
	for j=1 to 12 : fl3$(j)=str$(j+3)&',2,cr 30' : next j
	fl3$(13)='2,5,c 70,h,n'
	data 'Ref/Emp #:'
	data 'Job Number:'
	data 'Category:'
	data 'Sub-Category:'
	data 'P/R Dept #:'
	data 'Date:'
	data 'Reg Hours:'
	data 'Ovt Hours:'
	data 'Units:'
	data 'Payroll Tax:'
	data 'Amount:'
	data 'Description:'
	read mat sc3$
	io3$(1)='4,34,C 12,UT,N'
	io3$(2)='5,34,C 6,UT,N'
	io3$(3)='6,34,N 5,UT,N'
	io3$(4)='7,34,N 2,UT,N'
	io3$(5)='8,34,N 3,UT,N'
	io3$(6)='9,34,N 6,UT,N'
	for j=7 to 10
		io3$(j)=str$(j+3)&',34,N 8.2,UT,N'
	next j
	io3$(11)='14,34,N 10.2,UT,N'
	io3$(12)='15,34,C 30,UT,N'
	open #1: 'Name=[Q]\PRmstr\JCSCRN.h[cno]',i,i ioerr L630
	close #1,free:
	L630: !
	open #1: 'Name=[Q]\PRmstr\JCSCRN.h[cno],Size=0,RecL=1530',internal,output
	write #1,using L650: mat fl1$,mat io1$,mat fl2$,mat io2$,mat sc3$,mat fl3$,mat io3$
	L650: form pos 1,85*c 18
	close #1:
chain 'S:\acsPR\rpNames'

include: ertn

Xit: chain 'Menu'
