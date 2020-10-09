! Replace S:\acsGL\glBRecon
! Bank Reconciliation File  (maintains the bank rec file)
 
	autoLibrary
	on error goto Ertn
 
	dim fl2$(7),sc2$(7)*38
	dim flo2$(3),sa$(2),sb$(3),sc$(9),sd$(7),se$(7)*20,sf$(4)
	dim cnam$*40,dat$*20,cap$*128
	dim gl$*12,c$*12,p$*30,s$*2,a(3),dcode$*24,glc$*24,holdgc$*24,currgl$*12
 
	fnTop(program$,cap$="Bank Reconciliation")
	fnTop("S:\acsGL\glBRecon",cap$="Bank Reconciliation")
	fncno(cno,cnam$)
	fndat(dat$)
	for j=1 to 7: fl2$(j)=str$(j+3)&",2,C 38": next j
	sc2$(1)="1. Initial File Preparation"
	sc2$(2)="2. Add"
	sc2$(3)="3. Edit"
	sc2$(4)="4. Enter Cleared Checks"
	sc2$(5)="5. pr Bank Reconciliation"
	sc2$(6)="6. Listing of Checks"
	sc2$(7)="7. Change Bank Account Number"
	fl2$(3)="6,2,C 38,C,N"
	flo2$(1)="5,31,C 3,UT,N"
	flo2$(2)="5,35,C 6,UT,N"
	flo2$(3)="5,42,C 3,UT,N"
	sc$(1)="5,21,G 3,UT,N"
	sc$(2)="5,25,G 6,UT,N"
	sc$(3)="5,32,G 3,UT,N"
	sc$(4)="6,23,c 12,UT,N"
	sc$(5)="7,16,C 30,UT,N"
	sc$(6)="8,22,C 2,UT,N"
	sc$(7)="9,22,N 6,UT,N"
	sc$(8)="10,22,N 10.2,UT,N"
	sc$(9)="11,31,N 1,UT,N"
	for j=1 to 7: sd$(j)=str$(j+4)&",9,C 20" : next j
	se$(1)="G/L Number:"
	se$(2)="Check Number:"
	se$(3)="Payee:"
	se$(4)="Source:"
	se$(5)="Date:"
	se$(6)="Amount:"
	se$(7)="Cleared Bank (1=YES)"
OPEN_FILES: !
	open #1: "Name=[Q]\GLmstr\GLBREC.h[cno],KFName=[Q]\GLmstr\GLRecIdx.h[cno],Shr",internal,outIn,keyed ioerr L2660
	goto L2390
L480: pr newpage
	close #101: ioerr L500
L500: open #101: "SROW=3,SCOL=20,EROW=13,ECOL=59,BORDER=DR,CAPTION=<Bank Reconciliation File",display,outIn
	pr #101,fields "1,1,Cc 40,R,N": cnam$
	pr #101,fields "2,1,Cc 40,R,N": "Company Number [cno]"
	pr f "14,35,C 09,B,5": "Exit (F5)"
	rinput #101,select mat fl2$,attr "H": mat sc2$
	scode=curfld
	if cmdkey=5 then goto L2610
L570: on scode goto L590,L770,L810,L1460,L1760,L1770,L2390
 
L590: pr newpage
	close #101: ioerr L610
L610: open #101: "SROW=5,SCOL=20,EROW=15,ECOL=59,BORDeR=SR,CAPTION=Initial File Preparation",display,outIn
	pr #101,fields "1,1,Cc 40,R,N": cnam$
	pr #101,fields "2,1,Cc 40,R,N": "Company Number [cno]"
	pr #101,fields "4,1,Cc 40,R,N": "* * *   WARNING   * * *"
	pr #101,fields "5,1,Cc 40,N": "This selection will destroy all"
	pr #101,fields "6,1,Cc 40,N": "existing Bank Reconciliation records"
	pr f "16,34,C 11,B,5": "Cancel (F5)"
	pr #101,fields "8,2,C 24,N": "Enter ERASE to continue:"
L690: input #101,fields "8,27,Cu 5,UT,N": pas$
	if cmdkey=5 then goto L480
	if pas$><"ERASE" then goto L690
	new1=1
	close #1: ioerr L740
L740: open #1: "Name=[Q]\GLmstr\GLBREC.h[cno],Size=0,RecL=68,Replace",internal,outIn,relative
	goto L2620
L760: form pos 1,n 3,n 6,n 3,c 12,c 30,c 2,n 6,pd 5.2,n 1
L770: new1=1
	goto L1000
 
L800: form pos 1,c 12,c 12
L810: sf$(1)="11,10,N 3,UT,N"
	sf$(2)="11,14,N 6,UT,N"
	sf$(3)="11,21,N 3,UT,N"
	sf$(4)="11,30,C 12,CUE,N"
	pr newpage
	close #101: ioerr L870
L870: open #101: "SROW=9,SCOL=8,EROW=12,ECOL=43,BORDER=SR,CAPTION=<Maintain Bank Records",display,outIn
	pr #101: newpage
	pr f mat sf$: val(currgl$(1:3)),val(currgl$(4:9)),val(currgl$(10:12)),""
	pr f "10,10,C 14,R,N": "Bank GL Number"
	pr f "10,30,C 12,R,N": "Check Number"
	pr f "13,35,C 09,B,5": "Stop (F5)"
L930: input fields mat sf$: gl1,gl2,gl3,c$ conv L930
	if cmdkey=5 then goto L480
	c$=lpad$(rtrm$(c$),12)
	dcode$=lpad$(str$(gl1),3)&lpad$(str$(gl2),6)&lpad$(str$(gl3),3)&c$
	read #1,using L980,key=dcode$: gl$,c$,p$,s$,mat a nokey L930
L980: form pos 1,c 12,c 12,c 30,c 2,n 6,pd 5.2,n 1
	holdgc$=gl$&c$
L1000: pr newpage
	close #101: ioerr L1020
L1020: open #101: "SROW=2,SCOL=8,EROW=12,ECOL=49,Border=Sr,Caption=<Maintain Bank Reconciliation Records",display,outIn
	pr #101: newpage
	pr f "3,10,Cc 38,R,N": "Enter Check Number as 0 to Delete"
	pr f mat sd$: mat se$
	pr f "13,30,C 09,B,1": "Next (F1)"
	pr f "13,41,C 09,B,5": "Stop (F5)"
	if scode=2 then goto L1110
	pr f mat sc$: gl$(1:3),gl$(4:9),gl$(10:12),c$,p$,s$,mat a
	ce=5: goto L1170
L1110: pr f mat sc$: currgl$(1:3),currgl$(4:9),currgl$(10:12)
	ce=4: goto L1170
L1130: input fields mat sc$: gl1,gl2,gl3,c$,p$,s$,mat a conv L1130
	if ce>0 then sc$(ce)(ce1:ce2)="U": ce=0
	if cmdkey>0 then goto L1220 else ce=curfld+1
	if ce>udim(sc$) then ce=1
L1170: sc$(ce)=rtrm$(uprc$(sc$(ce))) : ce1=pos(sc$(ce),"U",1)
	ce2=ce1+1 : sc$(ce)(ce1:ce1)="UC" : goto L1130
CONVC: if ce>0 then sc$(ce)(ce1:ce2)="U"
	ce=cnt+1
ERRC: pr f "24,78,C 1": bell : goto L1170
L1220: if scode=2 and (ltrm$(c$)="" or ltrm$(c$)="0") then goto L480
	c$=lpad$(rtrm$(c$),12)
	if a(1)<10100 or a(1)>123199 then ce=7: goto ERRC
	if a(3)<0 or a(3)>1 then ce=9: goto ERRC
	if scode=2 then goto L1380
	if ltrm$(c$)="" or ltrm$(c$)="0" then goto L1280 else goto L1350
L1280: pr f "18,10,C 40,R,N": hex$("07")&" ENTER 1 TO DELETE; ENTER 2 TO RE-ENTER"
L1290: input fields "18,51,N 1,UE,N": dcode conv L1290
	on dcode goto L1330,L1310 none L1290
L1310: c$=holdgc$(13:24)
	goto L1000
L1330: delete #1,key=holdgc$:
	goto L1440
L1350: if holdgc$=lpad$(str$(gl1),3)&lpad$(str$(gl2),6)&lpad$(str$(gl3),3)&c$ then goto L1360 else goto L1380
L1360: rewrite #1,using L760,key=gl$&c$: gl1,gl2,gl3,c$,p$,s$,mat a
	goto L570
L1380: dcode$=lpad$(str$(gl1),3)&lpad$(str$(gl2),6)&lpad$(str$(gl3),3)&c$
	read #1,using L800,key=dcode$: gl$,c$ nokey L1410
	ce=3: goto ERRC
L1410: if scode=2 then goto L1430
	delete #1,key=holdgc$:
L1430: write #1,using L760: gl1,gl2,gl3,c$,p$,s$,mat a
L1440: new1=1
	goto L570
L1460: sa$(1)="10,43,C 12,UT,N"
	sa$(2)="12,43,N 12.2,UT,N"
L1480: pr newpage
	close #101: ioerr L1500
L1500: open #101: "SROW=8,SCOL=24,EROW=13,ECOL=55,BORDER=DR,CAPTION=<Enter Cleared Checks",display,outIn
	pr f "8,24,C 32,R,N": "      Bank #: "&currgl$
	pr f "10,25,C 25": "  Check Number:"
	pr f "12,25,C 25": "Amount to Verify:"
	pr f "14,35,C 09,B,5": "Stop (F5)"
L1550: input fields mat sa$: c$,amt conv L1550
	if cmdkey=5 then goto L480
	c$=lpad$(rtrm$(c$),12)
	pr f "15,5,C 70,N": ""
	pr f "16,5,C 70,N": ""
	if ltrm$(c$)="" or ltrm$(c$)="0" then goto L480
	read #1,using L1620,key=currgl$&c$: a2,a3 nokey L1740
L1620: form pos 63,pd 5.2,n 1
	if a3=1 then pr f "16,10,C 54,R,N": hex$("07")&" Check Number "&c$&" has been coded as cleared" else goto L1650
	goto L1550
L1650: if amt=0 then goto L1710
	if amt=a2 then goto L1710
	pr f "16,5,C 75,R,N": hex$("07")&" Amount entered does not equal amount on file!"
	pr f "17,5,C 70,R,N": " Enter 1 to code as cleared or 2 to bypass"
L1690: input fields "17,75,N 1,UE,N": goon conv L1690
	on goon goto L1710,L1480 none L1690
L1710: rewrite #1,using L1720,key=currgl$&c$: 1
L1720: form pos 68,n 1
	goto L1480
L1740: pr f "16,10,C 69,R,N": hex$("07")&" Check Number "&rtrm$(c$)&" in Bank Number "&ltrm$(currgl$)&" is not on file"
	goto L1550
L1760: fnchain("S:\acsGL\glCkRec")
L1770: pr newpage
	sb$(1)="2,23,C 20,UT,N"
	sb$(2)="3,37,Nz 6,UT,N"
	sb$(3)="4,37,Nz 6,UT,N"
	close #101: ioerr L1820
L1820: open #101: "SROW=9,SCOL=9,EROW=15,ECOL=71,BORDER=DR,CAPTION=<Print Listing of Checks written for Bank # "&ltrm$(currgl$),display,outIn
	pr #101: newpage
	pr #101,fields "2,2,C 20,N": "Report Heading Date:"
	pr #101,fields "3,2,Cr 34,N": "Starting Date to appear on report:"
	pr #101,fields "4,2,Cr 34,N": "Ending Date to appear on report:"
	pr f "16,30,C 09,B,1": "Next (F1)"
	pr f "16,41,C 09,B,5": "Stop (F5)"
L1890: rinput #101,fields mat sb$: dat$,begdat,enddat conv L1890
	if ce>0 then sb$(ce)(ce1:ce2)="U": ce=0
	if cmdkey>0 then goto L1980 else ce=curfld+1
	if ce>udim(sb$) then ce=1
L1930: sb$(ce)=rtrm$(uprc$(sb$(ce))) : ce1=pos(sb$(ce),"U",1)
	ce2=ce1+1 : sb$(ce)(ce1:ce1)="UC" : goto L1890
CONVB: if ce>0 then sb$(ce)(ce1:ce2)="U"
	ce=cnt+1
ERRB: pr f "24,78,C 1": bell : goto L1930
L1980: if cmdkey=5 then goto L480
	if fndate_mmddyy_to_ccyymmdd(begdat)>fndate_mmddyy_to_ccyymmdd(enddat) then goto L1890
	namtab=66-int(len(rtrm$(cnam$))/2)
	dattab=66-int(len(rtrm$(dat$))/2)
	pr newpage
	close #101: ioerr L2040
L2040: open #101: "SROW=08,SCOL=18,EROW=12,ECOL=58,BORDeR=SR,CAPTION=<Print Listing of Checks",display,outIn
	pr #101: newpage
	pr #101,fields "1,1,Cc 41,R,N": cnam$
	pr #101,fields "2,1,Cc 41,R,N": "Company Number [cno]"
	pr #101,fields "4,1,Cc 41,N": "Printing..."
	pr f "13,34,C 11,B,5": "Cancel (F5)"
	on fkey 5 goto L2250
	restore #1,key>=currgl$&"            ": nokey L2370
	fnopenprn
	gosub L2290
L2140: read #1,using L980: gl$,c$,p$,s$,mat a eof L2250
	if currgl$=gl$ then goto L2160 else goto L2250
L2160: if fndate_mmddyy_to_ccyymmdd(a(1))<fndate_mmddyy_to_ccyymmdd(begdat) or fndate_mmddyy_to_ccyymmdd(a(1))>fndate_mmddyy_to_ccyymmdd(enddat) then goto L2140
	if a(3)=1 then pr #255,using L2180: c$,p$,a(2),a(1),s$,"Yes" pageoflow L2210 else pr #255,using L2180: c$,p$,a(2),a(1),s$,"No" pageoflow L2210
L2180: form pos 1,c 12,pos 15,c 30,pos 48,n 11.2,pos 65,pic(zz/zz/zz),pos 75,c 2,pos 83,c 3,skip 1
L2190: tot=tot+a(2)
	goto L2140
L2210: pr #255: newpage
	gosub L2290
	goto L2190
 
L2250: pr #255,using L2260: "------------",tot,"============"
L2260: form pos 47,c 12,skip 1,pos 47,n 12.2,skip 1,pos 47,c 12,skip 1
	goto L2350
 
L2290: pr #255,using L2300: cnam$,"Report of Checks Written",currgl$(1:3),currgl$(4:9),currgl$(10:12),dat$
L2300: form pos namtab,c 40,skip 1,pos 54,c 24,skip 1,pos 59,c 3,x 1,c 6,x 1,c 3,skip 1,pos dattab,c 20,skip 2
	pr #255,using L2320: "Check Number","Payee","Amount","Date","Source","Cleared"
L2320: form pos 1,c 12,pos 15,c 5,pos 53,c 6,pos 67,c 4,pos 73,c 6,pos 81,c 7,skip 2
return
 
L2350: fncloseprn
	on fkey 5 ignore
L2370: tot=0
	goto L480
L2390: pr newpage
	sb$(1)="2,31,N 3,UT,N"
	sb$(2)="2,35,N 6,UT,N"
	sb$(3)="2,42,N 3,UT,N"
	close #101: ioerr L2440
L2440: open #101: "SROW=9,SCOL=18,EROW=11,ECOL=62,BORDER=DR,CAPTION=<Working Bank Account Number",display,outIn
	pr #101: newpage
	pr f "12,30,C 09,B,1": "Next (F1)"
	pr f "12,41,C 09,B,5": "Exit (F5)"
	pr #101,fields "2,2,C 28,N": "Working Bank Account Number:"
L2490: input #101,fields mat sb$: gl1,gl2,gl3 conv L2490
	if ce>0 then sb$(ce)(ce1:ce2)="U": ce=0
	if cmdkey>0 then goto L2580 else ce=curfld+1
	if ce>udim(sb$) then ce=1
L2530: sb$(ce)=rtrm$(uprc$(sb$(ce))) : ce1=pos(sb$(ce),"U",1)
	ce2=ce1+1 : sb$(ce)(ce1:ce1)="UC" : goto L2490
CONV1: if ce>0 then sb$(ce)(ce1:ce2)="U"
	ce=cnt+1
ERR1: pr f "24,78,C 1": bell : goto L2530
L2580: if cmdkey=5 then goto Xit
	currgl$=lpad$(str$(gl1),3)&lpad$(str$(gl2),6)&lpad$(str$(gl3),3)
	goto L480
L2610: if new1=1 or z1=1 then goto L2620 else goto Xit
L2620: close #1:
	fnIndex("[Q]\GLmstr\GLBREC.h[cno]","[Q]\GLmstr\GLRecIdx.h[cno]","1 24")
	if z1=1 then goto OPEN_FILES else goto Xit
 
L2660: if err=4152 then goto L740 else goto ERTN
 
Xit: fnXit
 
include: ertn
