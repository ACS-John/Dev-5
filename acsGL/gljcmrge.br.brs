! Replace S:\acsGL\gljcMrge
! GENERAL LEDGER JOB COST MERGE Charges; attempts to merge job cost : _
	! and then chains to regular general ledger merge program.
 
	autoLibrary
	on error goto Ertn
 
	dim rn$*12,jn$*6,ji2(3),cn$*11,l(13),ta(2),tr(9),empnum$*12,empnam$*30
 
	fnTop(program$,"Post Transactions")
	fncno(cno)
	open #1: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,input,relative: read #1,using 'Form POS 382,N 2',rec=1: jccode : _
	close #1:
	if jccode<>1 then goto L460
	pr newpage
	pr f "10,15,Cc 60,N": "GENERAL LEDGER JOB COST MERGE CHARGES IN PROCESS"
	open #2: "Name=[Q]\PRmstr\JCCAT.H[cno],KFName=[Q]\PRmstr\CatIndx.h[cno],Shr",internal,outIn,keyed
	open #3: "Name=[Q]\GLmstr\GL_Work_"&env$('acsUserId')&".h[cno]",internal,input
	open #5: "Name=[Q]\PRmstr\JCTRANS.h[cno],Shr",internal,outIn,relative
L180: read #3,using L190: dat,ji2(3),postc,rn$,empnam$,jn$,ji2(1),ji2(2) eof L430,ioerr L430
L190: form pos 13,n 6,pd 6.2,pos 27,n 2,c 12,c 30,pos 79,c 6,n 5,n 3
	if postc=9 or rn$="999999999999" then goto L180
	if ltrm$(rtrm$(rn$))="-1" or ji2(3)=0 then goto L180
	jn$=lpad$(rtrm$(jn$),6)
	cn$=jn$&lpad$(str$(ji2(1)),5)
	read #2,using L250,key=cn$: mat l,mat ta nokey L180
L250: form pos 37,11*pd 5.2,2*pd 2,2*pd 3
	nc1=0
	l(6)=l(6)+ji2(3)
	l(9)=l(9)+ji2(3)
	goto L310
	nc1=1
L310: read #5,using L320,rec=1,reserve: ot5
L320: form pos 86,pd 3
	empnum$=lpad$(rtrm$(rn$),12)
L340: ot5=lrec(5)+1
	write #5,using L360,rec=ot5,reserve: empnum$,jn$,ji2(1),ji2(2),0,dat,0,0,0,0,ji2(3),empnam$,0 duprec L340
L360: form pos 1,c 12,c 6,n 5,pd 3,pd 2,n 6,4*pd 4.2,pd 5.2,c 30,pd 3
	if ta(2)=0 then ta(1)=ot5 else rewrite #5,using L320,rec=ta(2): ot5
	rewrite #5,using L320,rec=1,release: ot5
	ta(2)=ot5
	if nc1=0 then rewrite #2,using L250,key=cn$: mat l,mat ta
	goto L180
 
L430: close #2:
	close #3:
	close #5:
L460: fnchain("S:\acsGL\ACGLMRGE")
 
Xit: fnXit
 
include: Ertn
