! Replace S:\acsGL\acglRev
! Used to reverse specific entries : _
	! enter the date and reference under "youref' and 'yourdate'
 
	autoLibrary
	fnTop(program$,"Special Reversing")
	on error goto Ertn
 
	dim adr(2),ta(2),prg$*20
	dim t$*12,n(2),l$*12,p$*30
	dim cnam$*40
 
	fncno(cno,cnam$)
	open #20: "Name=CNo.H"&wsid$,internal,outIn,relative  : _
	read #20,using 'form POS 43,C 20,POS 137,N 2,POS 141,N 1',rec=1: prg$,systype,process : _
	close #20:
 
	open #1: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno],Shr",internal,outIn,keyed
	open #2: "Name=[Q]\GLmstr\GLTrans.h[cno],Shr",internal,outIn,relative
	open #3: "Name=[Q]\GLmstr\GL_Work_"&env$('acsUserId')&".h[cno],NoShr",internal,outIn
	pr newpage
	x=lrec(2)
	for j=1 to x
L210: read #2,using L220,rec=j: t$,s,k,mat n,l$,p$,ven$ eof L420 noRec L410
L220: form pos 1,c 12,n 6,pd 6.2,n 2,n 2,c 12,c 30,c 8
		if s=yourdate and n(1)=3 and rtrm$(ltrm$(tr$))="yourref#" then goto L240 else goto L410
L240: k=-k
		s=newdate
		if k=0 and uprc$(ltrm$(rtrm$(p$)))<>"VOID" then goto L210
		if val(t$(1:3))=0 and val(t$(4:9))=0 and val(t$(10:12))=0 and k=0 then goto L210
		if t$(3:3)=" " then t$(3:3)="0"
		if t$(12:12)=" " then t$(12:12)="0"
		read #1,using L310,key=t$: cb,mat ta nokey L410
L310: form pos 87,pd 6.2,pos 333,2*pd 3
L320: lr2=lrec(2)+1
		write #2,using L390,rec=lr2: t$,s,k,mat n,l$,p$,0 duprec L320
		if ta(1)=0 then ta(1)=lr2
		if ta(2)>0 then rewrite #2,using L400,rec=ta(2): lr2
		ta(2)=lr2
		cb=cb+k
		rewrite #1,using L310,key=t$: cb,mat ta
L390: form pos 1,c 12,n 6,pd 6.2,n 2,n 2,c 12,c 30,pd 3
L400: form pos 71,pd 3
L410: next j
L420: ! second pass
L430: read #3,using L440: t$,s,k,mat n,l$,p$,ven$ eof Xit
L440: form pos 1,c 12,n 6,pd 6.2,n 2,n 2,c 12,c 30,c 8
	if s=yourdate and n(1)=3 and rtrm$(ltrm$(tr$))="yourref#" then goto L460 else goto L630
L460: k=-k
	s=newdate
	if k=0 and uprc$(ltrm$(rtrm$(p$)))<>"VOID" then goto L430
	if val(t$(1:3))=0 and val(t$(4:9))=0 and val(t$(10:12))=0 and k=0 then goto L430
	if t$(3:3)=" " then t$(3:3)="0"
	if t$(12:12)=" " then t$(12:12)="0"
	read #1,using L530,key=t$: cb,mat ta nokey L630
L530: form pos 87,pd 6.2,pos 333,2*pd 3
L540: lr2=lrec(2)+1
	write #2,using L610,rec=lr2: t$,s,k,mat n,l$,p$,0 duprec L540
	if ta(1)=0 then ta(1)=lr2
	if ta(2)>0 then rewrite #2,using L620,rec=ta(2): lr2
	ta(2)=lr2
	cb=cb+k
	rewrite #1,using L530,key=t$: cb,mat ta
L610: form pos 1,c 12,n 6,pd 6.2,n 2,n 2,c 12,c 30,pd 3
L620: form pos 71,pd 3
L630: goto L430
Xit: fnXit
 
ERTN: fnerror(program$,err,line,act$,"NO")
	if lwrc$(act$)<>"pause" then goto L700
	execute "list -"&str$(line) : _
	pause  : _
	goto L700
	pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause
L700: execute act$
	goto ERTN
 
