! Replace S:\acsPR\newjcMrgC
! JOB COST MERGE CHARGES

	autoLibrary
	on error goto Ertn

	dim rn$*12,jn$*6,ji2(3),cn$*11,l(13),ta(2),tr(9),empnum$*12,empnam$*30

	open #2: "Name=[Q]\PRmstr\JCCAT.h[cno],KFName=[Q]\PRmstr\CatIndx.h[cno],Shr",i,outIn,k
	open #3: "Name=jccharges."&wsid$,i,i
	open #5: "Name=[Q]\PRmstr\JCTRANS.h[cno],Shr",i,outi,r
L170: read #3,using L180: rn$,dat,jn$,mat ji2,empnam$ eof L400
L180: form pos 1,c 12,pd 4,c 6,2*pd 3,pd 5.2,c 30
	if ltrm$(rtrm$(rn$))="-1" or ji2(3)=0 then goto L170
	jn$=lpad$(rtrm$(jn$),6)
	cn$=jn$&lpad$(str$(ji2(1)),5)
	read #2,using L230,key=cn$: mat l,mat ta nokey L280
L230: form pos 37,11*pd 7.2,2*pd 2,2*pd 3
	nc1=0
	l(6)=l(6)+ji2(3)
	l(9)=l(9)+ji2(3)
	goto L290
L280: nc1=1
L290: read #5,using L300,rec=1,reserve: ot5
L300: form pos 86,pd 3
	empnum$=lpad$(rtrm$(rn$),12)
L320: ot5=lrec(5)+1
	write #5,using L340,rec=ot5,reserve: empnum$,jn$,ji2(1),ji2(2),0,dat,0,0,0,0,ji2(3),empnam$,0 duprec L320
L340: form pos 1,c 12,c 6,n 5,pd 3,pd 2,n 6,4*pd 4.2,pd 5.2,c 30,pd 3
	if ta(2)=0 then ta(1)=ot5 else rewrite #5,using L300,rec=ta(2): ot5
	rewrite #5,using L300,rec=1,release: ot5
	ta(2)=ot5
	if nc1=0 then rewrite #2,using L230,key=cn$: mat l,mat ta
	goto L170
L400: close #2:
	close #3:
	close #5:
Xit: fnXit

ERTN: fnerror(program$,err,line,act$,"NO")
	if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
	execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
	pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
ERTN_EXEC_ACT: execute act$ : goto ERTN

