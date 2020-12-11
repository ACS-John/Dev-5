! Replace S:\acsPR\jcRemove
! Remove Job Cost Payroll Jobs
 
	autoLibrary
	on error goto Ertn
 
	dim jn$*6,n$*40,a$(3)*30,b(4),cn$*11,k$*25,l(13),ta(2),eno$*12,jno$*6
	dim tr(9),pd$*30,tn$*6,n$*40,cap$*128,message$*40,msgline$(2)*60
	dim response$(5)*1
 
	fnTop("S:\acsPR\jcRemove",cap$="Remove Completed Jobs")
	fncno(cno)
 
	fnconsole(1)
	open #1: "Name=[Q]\PRmstr\JCMSTR.h[cno]",internal,outIn: close #1:
 
	execute "Copy [Q]\PRmstr\JCMSTR.h[cno] JCMSTR.X -n"
	execute "Copy [Q]\PRmstr\JCTRANS.h[cno] JCTRANS.X -n"
	execute "Copy [Q]\PRmstr\JCCAT.h[cno] JCCAT.X -n"
 
	open #1: "Name=JCMSTR.X,KFName=[Q]\PRmstr\JCIndx.h[cno]",internal,outIn,keyed
 
L220: pr newpage
	fnopenwin(win=101,10,20,14,59,cap$)
	pr #101,fields "4,2,C 21,N": "Job Number to Remove:"
	pr f "15,35,C 09,B,5": "Done (F5)"
L260: input #101,fields "4,24,C 6,UT,N": jn$
	jn$=lpad$(rtrm$(jn$),6)
	if cmdkey=5 then goto L390
	if ltrm$(jn$)="" or ltrm$(rtrm$(jn$))="0" then goto L260
	read #1,using L310,key=jn$: n$ nokey L220
L310: form pos 7,c 40
	msgline$(1)="Are you sure you wish to delete Job Number "&ltrm$(jn$)
	msgline$(2)=n$
	fnoldmsgbox(mat response$,cap$, mat msgline$,2)
	if response$(1)="Y" then goto L360 else goto L220
L360: rewrite #1,using 'Form POS 157,N 2',key=jn$: 9 nokey L220
	goto L220
 
L390: pr newpage
	message$="Removeing completed Jobs..."
	fnwait(message$,0)
	restore #1:
	open #2: "Name=JCCAT.X,KFName=[Q]\PRmstr\CatIndx.h[cno]",internal,input,keyed
	open #3: "Name=JCTRANS.X",internal,input,relative
	open #11: "Name=[Q]\PRmstr\JCMSTR.h[cno]",internal,output
	close #11,free:
	open #11: "Name=[Q]\PRmstr\JCMSTR.h[cno],SIZE=0,RecL=300",internal,output
	open #12: "Name=[Q]\PRmstr\JCCAT.h[cno]",internal,output
	close #12,free:
	open #12: "Name=[Q]\PRmstr\JCCAT.h[cno],SIZE=0,RecL=123",internal,output
	open #13: "Name=[Q]\PRmstr\JCTRANS.h[cno]",internal,output
	close #13,free:
	open #13: "Name=[Q]\PRmstr\JCTRANS.h[cno],SIZE=0,RecL=88",internal,outIn,relative
	ot4=1
	write #13,using L560,rec=1: " ","",mat tr," ",ot4
L560: form pos 1,c 12,c 6,n 5,pd 3,pd 2,n 6,4*pd 4.2,pd 5.2,c 30,pd 3
L570: read #1,using L580: jn$,n$,mat a$,mat b eof EOF1
L580: form pos 1,c 6,c 40,3*c 30,n 6,2*pd 7.2,n 2
	if b(4)=9 then goto L570
	cn$=jn$&"     "
	read #2,using L620,key>=cn$: cn$,k$,mat l,mat ta nokey L570
L620: form pos 1,c 11,c 25,11*pd 7.2,2*pd 2,2*pd 3
	goto L650
L640: read #2,using L620: cn$,k$,mat l,mat ta eof L820
L650: if jn$><cn$(1:6) then goto L820
	if ta(1)=0 then goto L800
	adr=ta(1)
	mat ta=(0)
L690: read #3,using L560,rec=adr: eno$,jno$,mat tr,pd$,nta
	ot4=ot4+1
	if nta>0 then ota=ot4+1 else ota=0
	write #13,using L560,rec=ot4: eno$,jno$,mat tr,pd$,ota
	rewrite #13,using L740,rec=1: ot4
L740: form pos 86,pd 3
	if ta(1)=0 then ta(1)=ot4
	ta(2)=ot4
	if nta=0 then goto L800
	adr=nta
	goto L690
L800: write #12,using L620: cn$,k$,mat l,mat ta
	goto L640
L820: write #11,using L580: jn$,n$,mat a$,mat b
	goto L570
 
EOF1: close #1,free:
	close #2,free:
	close #3,free:
	close #11:
	close #12:
	close #13:
	execute "Index [Q]\PRmstr\JCMSTR.h[cno],[Q]\PRmstr\JCIndx.h[cno],1,6,Replace,DupKeys -n"
	execute "Index [Q]\PRmstr\JCCAT.h[cno],[Q]\PRmstr\CatIndx.h[cno],1,11,Replace,DupKeys -n"
	goto Xit
 
Xit: fnXit
 
include: ertn
 
