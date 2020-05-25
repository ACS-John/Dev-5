on error goto Ertn
autoLibrary
fnTop(program$,cap$="Post General Ledger")
fncno(cno,cnam$)
fndat(dat$)
dim p$*5,iv$*12,gl(3),gh(3),td$*30,tr$*12,a$*40,cap$*128,dat$*20
td$="AR SUMMARY"
tr6=5
open #8: "Name=S:\Core\Data\acsllc\pedate.h[cno],RecL=20,use,Shr",internal,outIn,relative 
if lrec(8)=0 then write #8,using "form pos 1,n 6": dat else read #8,using "form pos 1,n 6",rec=1,release: dat
pr newpage
pr f "10,15,c 60": "POSITION PAPER FOR G/L DISTRIBUTION REPORT"
pr f "12,12,c 60": "ENTER THE AS OF DATE: FORMAT = MMDDYY"
pr f "12,53,n 6,N": dat
L170: input fields "12,53,n 6,ue": tr4 conv L170
	if tr4<10100 or tr4>123199 then goto L170
	rewrite #8,using "form pos 1,n 6",rec=1: d1
	pa=43-int(len(rtrm$(a$))/2)
	open #1: "Name="&env$('Temp')&"\Addr."&session$,internal,input 
	open #2: "Name="&env$('temp')&"\Work."&session$,internal,input,relative 
	pr newpage
	pr f "10,15,c 50,h,n": "PRINT A/R GENERAL LEDGER DISTRIBUTION IN PROCESS"
	pr f "23,2,C 30,N": "Press F5 to stop"
	gosub L590
L270: read #1,using L280: r1 eof L670 
L280: form pos 1,pd 3
	read #2,using L300,rec=r1: p$,iv$,tr1,tr3,mat gl 
L300: form pos 1,c 5,c 12,n 6,pd 5.2,n 3,n 6,n 3
	gosub L330
	goto L430
L330: if gh(2)=gl(2) or gh(2)=0 then goto L420
L340: pr #255,using L350: mat gh,"TOTAL ",tdb,tcr
L350: form skip 1,pic(zzzz),pic(zzzzzzz),pic(zzzz),x 5,c 27,2*n 15.2,skip 2
	tt=tdb-tcr
	if tt=0 then goto L400
	gtdb=gtdb+tdb
	gtcr=gtcr+tcr
L400: tdb=0
	tcr=0
L420: return 
L430: adb=0
	acr=0
	if tr3>0 then adb=tr3 else acr=-tr3
	if adb=0 then goto L500
	pr #255,using L480: mat gl,p$,iv$,tr1,adb pageoflow L560
L480: form pos 1,pic(zzzz),n 7,pic(zzzz),x 2,c 8,c 14,pic(zz/zz/zz),n 15.2
	goto L520
L500: pr #255,using L510: mat gl,p$,iv$,tr1,acr pageoflow L560
L510: form pos 1,pic(zzzz),n 7,pic(zzzz),x 2,c 8,c 14,pic(zz/zz/zz),x 15,n 15.2
L520: tdb=tdb+adb
	tcr=tcr+acr
	mat gh=gl
	goto L270
L560: pr #255: newpage
	gosub L590
	continue 
L590: pr #255,using L600: date$,a$
L600: form skip 2,pos 1,c 8,pos pa,c 40,skip 1
	pr #255,using L620: time$,"G/L DISTRIBUTION FOR ACCOUNTS RECEIVABLE"
L620: form pos 1,c 8,pos 20,c 45,skip 1
	pr #255,using L640: "AS OF ",tr4
L640: form pos 33,c 6,pic(zz/zz/zz),skip 2
	pr #255: "  G/L ACCOUNT # CLIENT #    INVOICE #    DATE           DEBITS        CREDITS"
	return 
L670: gosub L340
	pr #255,using L350: 0,0,0,"FINAL TOTAL ",gtdb,gtcr
close #1: ioerr ignore
close #2: ioerr ignore
if nw=1 then close #255: else pr #255: newpage
XIT: fnxit
include: Ertn
