	on fkey 5 goto L470
	on error goto Ertn
	autoLibrary
	fnTop(program$,cap$="Service Production Report")
	fnconsole(1)
	fnopenprn
	dim cat$(30)*30
	dim sc$*4,ds$*30,catno$*2,cap$*128
	namtab=41-int(len(rtrm$(env$('cnam')))/2)
	open #1: "Name=S:\Core\Data\acsllc\SCMSTR.h[cno],KFName=S:\Core\Data\acsllc\SCIndex.h[cno],Shr",internal,input,keyed
	fnRead30Categories(mat cat$)
	open #8: "Name=S:\Core\Data\acsllc\pedate.h[cno],RecL=20,use,Shr",internal,outIn,relative
	if lrec(8)=0 then write #8,using "form pos 1,n 6": d1 else read #8,using "form pos 1,n 6",rec=1,release: dat
	pr newpage
	pr f "10,5,c 57,n": "ENTER DATE FOR SERVICE PRODUCTION REPORT IN MMDDYY FORMAT"
	pr f "10,65,n 6,n": dat
	pr f "13,30,c 20": "Press F5 to Stop"
L230: input fields "10,65,n 6,eu,n": dat conv L230
	if cmdkey=5 then goto Xit
	if dat<10100 or dat>123199 then goto L230
	rewrite #8,using "form pos 1,n 6",rec=1: dat
	close #8:
	pr newpage
	pr f "10,15,c 57,n": "PRINT SERVICE PRODUCTION REPORT IN PROCESS"
	pr f "23,2,C 30,N": "Press F5 to stop"
	gosub L800
L320: read #1,using L330: sc$,ds$,th,sf eof L460
L330: form pos 1,c 4,c 30,pd 4.2,pd 5.2
	catno$=sc$(1:2)
	catno=val(catno$)
	if fst=1 then goto L390
	fst=1
	gosub L500
L390: if hcatno><catno then goto L420
	gosub L540
	goto L320
L420: !
	gosub L700
	gosub L500
	gosub L540
	goto L320
L460: gosub L700
L470: !
close #1: ioerr ignore
fncloseprn
Xit: fnXit
L500: pr #255,using L510: catno*100,cat$(catno)
L510: form pos 2,pic(zzzz),pos 8,c 30,skip 1
	hcatno=catno
	return
L540: if th><0 then goto L600
	pr #255,using L640: sc$,ds$,th,sf,0 pageoflow L570
	goto L650
L570: pr #255: newpage
	gosub L800
	goto L650
L600: pr #255,using L640: sc$,ds$,th,sf,sf/th pageoflow L620
	goto L650
L620: pr #255: newpage
	gosub L800
L640: form pos 5,c 4,pos 14,c 30,pos 53,n 13.2,n 13.2,n 13.2,skip 1
L650: b1=b1+th
	b2=b2+sf
	m$=r$
	hcatno=catno
	return
L700: if b1=0 then goto L730
	b0=b2/b1
	if b0<10000 then goto L740
L730: b0=0
L740: pr #255,using L750: "TOTAL",cat$(hcatno),b1,b2,b0
L750: form pos 17,c 5,pos 23,c 30,pos 53,n 13.2,n 13.2,n 13.2,skip 1
	b1=0
	b2=0
	pr #255:
return
L800: !
	p1=p1+1
	pr #255,using L820: date$,env$('cnam'),"PAGE",p1
	L820: form skip 3,pos 1,c 8,pos namtab,c 40,pos 75,c 5,pic(zzz),skip 1
	pr #255,using L840: time$,"SERVICE PRODUCTION REPORT"
	L840: form pos 1,c 8,pos 29,c 25,skip 1
	pr #255,using L860: "FOR YEAR-TO-DATE",dat
	L860: form pos 29,c 16,pos 46,pic(zz/zz/zz),skip 4
	pr #255,using L880: "SERVICE","CHARGED","AVERAGE"
	L880: form pos 4,c 7,pos 70,c 7,pos 83,c 7,skip 1
	pr #255,using L900: "CODE","DESCRIPTION","HOURS","AT STANDARD","HOURLY RATE"
	L900: form pos 5,c 4,pos 18,c 11,pos 60,c 5,pos 68,c 11,pos 81,c 11,skip 3
return
include: ertn
