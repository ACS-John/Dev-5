! Replace S:\acsPR\jcMerge
! Posting to Jobs...
 
	autoLibrary
	on error goto Ertn
 
	dim h(7),ji1(6),jn$*6,ji2(6),cn$*11,l(13),ta(2),tr(9),empnum$*12
	dim empnam$*30,cap$*128,message$*40
 
	fnTop("S:\acsPR\jcMerge",cap$="Job Cost Merge")
	fncno(cno)
 
 
	open #2: "Name=[Q]\PRmstr\JCCAT.h[cno],KFName=[Q]\PRmstr\CatIndx.h[cno],Shr",i,outIn,k
	open #3: "Name=[Temp]\Work."&session$,i,i
	open #4: "Name=[Q]\PRmstr\JCPRH1.h[cno],Shr",internal,output
	open #5: "Name=[Q]\PRmstr\JCTRANS.h[cno],Shr",i,outi,r
L220: read #3,using L230: mat ji1,jn$,mat ji2,empnam$,sal eof L720
L230: form pos 1,n 8,n 1,pd 4,pd 2,2*pd 4.2,c 6,2*pd 3,pd 5.2,n 2,2*pd 4.2,c 30,pd 4.2
	if ji1(1)=-1 then goto L220
	if ji2(4)<1 or ji2(4)>10 then goto L280
	ji2(6)=ji2(3)
	ji2(3)=0
L280: jn$=lpad$(rtrm$(jn$),6)
	if h(7)<1 or h(7)>11 then goto L300 else goto L340
L300: if h(1)=ji1(1) and h(3)=ji1(4) and dt2=ji1(3) then goto L400
	if h(1)=0 then goto L360
	h(6)=sal
	dt2=fndate_mmddyy_to_ccyymmdd(ji1(3))
L340: write #4,using L350: mat h,dt2,jn$
L350: form pos 1,n 8,n 1,pd 2,2*pd 4.2,pd 5.2,n 2,n 8,c 6
L360: mat h=(0)
	h(1)=ji1(1)
	h(2)=ji1(2)
	h(3)=ji1(4)
L400: h(4)=h(4)+ji1(5)
	h(5)=h(5)+ji1(6)
	if ji2(4)<1 or ji2(4)>10 then h(6)=ji2(3) else h(6)=ji2(6)
	h(7)=ji2(4)
	if ltrm$(jn$)="" or rtrm$(ltrm$(jn$))="0" then goto L220
	cn$=jn$&lpad$(str$(ji2(1)),5)
	read #2,using L470,key=cn$: mat l,mat ta nokey L600
L470: form pos 37,11*pd 7.2,2*pd 2,2*pd 3
	if ji1(5)+ji1(6)=0 then goto L560
	l(4)=l(4)+ji2(3)
	l(7)=l(7)+ji2(3)
	l(5)=l(5)+ji1(5)+ji1(6)
	l(8)=l(8)+ji1(5)+ji1(6)
	l(6)=l(6)+ji2(6)
	l(9)=l(9)+ji2(6)
	goto L580
L560: l(6)=l(6)+ji2(3)+ji2(6)
	l(9)=l(9)+ji2(3)+ji2(6)
L580: l(10)=l(10)+ji2(5)
	goto L600
L600: read #5,using L610,rec=1,reserve: ot5
L610: form pos 86,pd 3
	empnum$=lpad$(rtrm$(str$(ji1(1))),12)
L630: ot5=lrec(5)+1
	write #5,using L650,rec=ot5,reserve: empnum$,jn$,ji2(1),ji2(2),ji1(4),ji1(3),ji1(5),ji1(6),ji2(5),ji2(6),ji2(3),empnam$,0 duprec L630
L650: form pos 1,c 12,c 6,n 5,pd 3,pd 2,n 6,4*pd 4.2,pd 5.2,c 30,pd 3
	if ta(2)=0 then ta(1)=ot5 else rewrite #5,using L610,rec=ta(2),reserve: ot5
	rewrite #5,using L610,rec=1,release: ot5
	ta(2)=ot5
	rewrite #2,using L470,key=cn$: mat l,mat ta
	goto L220
 
L720: dt2=fndate_mmddyy_to_ccyymmdd(ji1(3))
	if h(1)><0 then write #4,using L350: mat h,dt2,jn$
	goto Xit
 
include: ertn
 
Xit: ! fnXit
	close #3,free:
	fnXit
 
