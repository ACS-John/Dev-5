! Replace S:\acsGL\TexasUC
! Texas Unemployment Compensation Report (Not on a menu, but can leave in)
 
	autoLibrary
	on error goto Ertn
 
	dim k(1),k$(3)*25,l$(1)*11,d(14),m(20),n(2),m$*5
	dim cap$*128,message$*40,cnam$*40
	dim a$(3)*40,b$(2)*12,c$*5,e(2),e$(2)*11
 
	fnTop(program$,cap$="Texas Unemployment Compensation Report")
	fncno(cno,cnam$)
	open #1: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,input  : _
	read #1,using 'Form POS 1,3*C 40,2*C 12,C 5,POS 188,PD 7.2': mat a$,mat b$,c$,ucm : _
	close #1:
	if fnprocess=1 then goto L220
SCR1: !
	fnwin3b(win=101,cap$,5,44,1,3,5)
	pr #win,fields "4,2,C 36,N": "Quarterly Period Ending Date (q-yy):"
L180: input #win,fields "4,39,C 5,UT,N": m$ conv L180
	close #win:
	if cmdkey=5 or cmdkey=99 then goto Xit
 
L220: open #2: "Name=[Q]\GLmstr\PRmstr.h[cno],KFName=[Q]\GLmstr\PRIndex.h[cno],Shr",internal,input,keyed
	message$="Printing: please wait..." : _
	fnwait(message$,1)
	on fkey 5 goto Xit
	fnopenprn
	gosub L400
L270: read #2,using L280: mat k,mat k$,mat l$,mat m eof L480
L280: form pos 1,n 4,3*c 25,c 11,18*pd 5.2,2*n 5
	if m(2)=0 or k(1)=0 then goto L380
	if p1<61 then goto L340
	gosub L620
	pr #255: newpage
	gosub L400
L340: gosub L530
	t1=t1+m(2)
	t2=t2+h3
	t3=t3+h2
L380: goto L270
 
L400: p2=p2+1
	pr #255,using L420: b$(2)(1:11),b$(1)(1:11),m$
L420: form skip 6,pos 5,c 11,pos 49,c 11,pos 60,c 5,skip 4
	pr #255,using L440: a$(1),p2
L440: form pos 5,c 40,pos 51,n 3,skip 6
	p1=16
return
 
L480: gosub L620
	close #2:
	fncloseprn
	fnchain("S:\acsGL\PRSTATUC")
 
L530: p3=p3+1
	for ln=len(rtrm$(k$(1))) to 1 step -1
		if k$(1)(ln:ln)=" " then goto L570
	next ln
L570: pr #255,using L580: l$(1),k$(1)(1:1),k$(1)(ln+1:ln+17),m(2)
L580: form pos 6,c 11,pos 20,c 1,pos 27,c 17,pos 46,n 10.2,skip 2
	p1=p1+2
return
 
L620: p1=p1+1
	for j1=1 to 63-p1
		pr #255:
		p1=p1-1
	next j1
	pr #255,using L680: t1
L680: form pos 46,n 10.2
	pr #255: newpage
	t1=0
return
 
Xit: fnXit
 
include: Ertn
 
