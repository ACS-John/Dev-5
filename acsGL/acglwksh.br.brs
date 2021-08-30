! Replace S:\acsGL\acglWkSh
! pr Trial Balance Worksheet
 
	autoLibrary
	on error goto Ertn
 
	dim a$(9)*3,glsum$(2)*12,u$*12,address$(2)*40,b$(2)*12,c$*5,d(2),p$*62
	dim cnam$*40,d$*50,tr(7),tr$*12,td$*30,n$*12,t$*12,x$*3
	dim cogl$*12,pedat$*20,cap$*128
 
	right=1
	fnTop(program$,cap$="Trial Balance Worksheet")
	fnconsole(off=0)
	first=1
	fncno(cno,cnam$)
	open #20: "Name=[Q]\GLmstr\Company.h[cno]",i,i,r  : _
	read #20,using 'Form POS 150,2*N 1',rec=1: d(1) : _
	read #20,using 'Form POS 176,C 12',rec=1: cogl$ : _
	close #20:
	a$(1)="C/D" : a$(2)="C/R" : a$(3)="ADJ" : _
	a$(4)="A/P" : a$(5)="PR" : a$(6)="A/R" : _
	a$(7)="S/J" : a$(8)="P/J" : a$(9)=" "
	p$="|        *         |         *         |         *        |"
	open #glmstr=1: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\glIndex.h[cno],Shr",i,i,k
	if fnprocess=1 or d(1)=0 then goto START_REPORT : _
		! Skip Cost Center Question if not applicable or in Automatic Processing
SCREEN1: !
	fnTos(sn$="GLTBwksheet") : _
	mylen=12 : mypos=mylen+2
	fnLbl(1,1,"Cost Center:",mylen,right)
	fnTxt(1,mypos,3,0,0,'number') : _
	resp$(1)=""
	fnCmdSet(3)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	costcent=val(resp$(1)) : _
	n$=lpad$(str$(costcent),3)&"     0  0"
	read #glmstr,using 'Form POS 1,C 12,C 50,POS 81,2*PD 6.2',key>=n$: n$,d$,bb,cb nokey SCREEN1
! pr NEWPAGE
! pr f "10,2,C 78": "Enter the Cost Center (Fund or Company Num) If you wish to only pr a work"
! pr f "11,2,C 69": "sheet for one Cost Center (Blank for All)"
! pr f "13,34,C 11,B,5": "Cancel (F5)"
! Input Fields "11,70,Nz 3,EUT,N": COSTCENT Conv 250
! If CMDKEY=5 Then goto Xit
! n$=LPAD$(STR$(COSTCENT),3)&"     0  0"
! Read #GLmstr,Using 'Form POS 1,C 12,C 50,POS 81,2*PD 6.2',Key>=N$: N$,D$,BB,CB Nokey SCREEN1
START_REPORT: !
! fnwait : _
	! pr f "10,25,C 30,R,N": "G/L WORKSHEET IN PROCESS" : _
	! pr f "12,20,Cc 30,B,5": "Cancel (F5)" : _
	on fkey 5 goto END1
	fnopenprn
	gosub HDR
	if fnprocess=1 or d(1)=0 then goto READ_GLMSTR else goto L500
 
READ_GLMSTR: !
	read #glmstr,using 'Form POS 1,C 12,C 50,POS 81,2*PD 6.2': n$,d$,bb,cb eof END1
	if first=0 and n$(1:3)<>oldn$(1:3) then gosub TOTALS
	first=0
	oldn$=n$
	if costcent><0 and val(n$(1:3))><costcent then goto END1
L500: dno=val(n$(1:3)) : ano=val(n$(4:9)) : sno=val(n$(10:12))
	gosub SOMETHING
	if n$<>cogl$ then goto READ_GLMSTR
	pr #255,using L540: "**Net Income or Loss",drtotal+crtotal
L540: form pos 17,c 20,pos 38,pic(---,---,---.##),skip 2
	goto READ_GLMSTR
 
TOTALS: pr #255:
	pr #255: tab(13);"Worksheet Proof Totals";
	oldn$=n$
	pr #255,using L610: drtotal,crtotal,page$ : _
	pr #255: ""
L610: form pos 39,pic(---,---,---.##),pos 53,pic(---,---,---.##),c 80
	if end1=1 then return
	drtotal=crtotal=0
	pr #255: newpage
	gosub HDR
return
 
END1: end1=1
	gosub TOTALS
	close #glmstr:
	fncloseprn
	goto Xit
 
HDR: !
	pr #255,using "Form pos 1,C 20,Cc 72": date$("mm/dd/yy"),cnam$
	pr #255,using "form pos 1,c 20,cc 72": time$,cap$
	pr #255,using 'Form Pos 21,Cc 72': fnpedat$
	pr #255: ""
	pr #255: tab(6);"Account";
	pr #255: tab(50);"Trial Balance";tab(73);"Adjustments";
	pr #255: tab(90);"Profit and Loss";tab(112);"Balance Sheet"
	pr #255,using 'Form POS 6,C 6,POS 17,C 11,POS 47,C 5,POS 60,C 6,POS 69,C 5,SKIP 0': "Number","Description","Debit","Credit","Debit"
	pr #255,using 'Form POS 80,C 6,POS 89,C 5,POS 101,C 6,POS 111,C 5,POS 121,C 6': "Credit","Debit","Credit","Debit","Credit" : _
	pr #255: "" : _
	pr #255: ""
return
 
SOMETHING: !
	if cb<0 then : _
		crtotal+=cb : p1=54 else : _
		drtotal+=cb : p1=40
	pr #255,using 'Form Pos 1,pic(ZZZ),X 1,pic(ZZZZZZ),X 1,pic(ZZZ),X 2,C 22,POS P1,N 12.2,POS 68,C 62': dno,ano,sno,d$(1:22),cb,p$ pageoflow PGOF : _
	pr #255,using "Form Pos 68,C 62": p$ pageoflow PGOF
return
 
PGOF: pr #255: newpage : gosub HDR : continue
 
Xit: fnXit
 
include: ertn
