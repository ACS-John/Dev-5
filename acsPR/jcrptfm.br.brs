! Replace S:\acsPR\JCRptFM
! Job Cost User-Designed Report File
 
	autoLibrary
	on error goto Ertn
 
	dim cap$*128,message$*40,msgline$(2)*60,response$(5)*1
	dim bk$(20)*28,nam$*28,ios$(2),wrds$(2)*30,iom$(3),scm$(3)*40
	dim io1$(10),io2$(7),fd$(20),rptemp(20),tempch$(4)*256,rptn$*6,rnew$*6
	dim rt$*51,ch$(2)*132,psc(100),f$(20)*50,pp(20),ppr(20),dp(20),fc(20)
	dim tcj(20),tcs(20),rno$(50)*2,em$*40,wrd3$(2)*23,io3$(2)
 
	fnTop("S:\acsPR\JCRptFM",cap$="User Designed Reports")
	fncno(cno)
	fnconsole(1)
	pg=3
 
	open #1: "Name=S:\acsPR\JCREPORT.MST,KFName=S:\acsPR\JCREPORT.idx,Shr",internal,outIn,keyed
 
MENU1: pr newpage
	fnopenwin(win=101,09,20,15,59,cap$)
	scm$(1)="1. Add or Edit" 
	scm$(2)="2. pr Proof List" 
	scm$(3)="3. Search"
	for j=1 to udim(scm$) : iom$(j)=str$(j+3)&",2,C 38,N" : next j
	pr f "16,35,C 09,B,5": "Exit (F5)"
L240: rinput #win,select mat iom$,attr "H": mat scm$ 
	ti=curfld
	if cmdkey=5 then goto Xit
	on ti+1 goto Xit,L290,L2180,SRCH none L240
	goto Xit
 
L290: pr newpage
	win=102
	fnopenwin(win,10,20,14,59,cap$)
	pr #win,fields "4,02,C 23,N": "Job Cost Report Number:"
	pr f "15,35,C 09,B,5": "Done (F5)"
L340: input #win,fields "4,26,Nz 2,UT,N": rptn conv L340
	rx=rptn
	if cmdkey=5 then goto Xit
	if rptn=0 then goto L340
	rn=rptn
	rptn$=lpad$(str$(rptn),2)
	read #1,using L410,key=rptn$: rn,rt$,mat ch$,ips,sd,cp,sc,mat psc,mat f$,mat pp,mat ppr,mat dp,mat fc,mat tcj,mat tcs nokey L440
L410: form pos 1,n 2,c 51,x 27,2*c 132,n 3,3*n 1,100*pd 6.3,20*c 50,40*pd 2,80*n 1
	goto L520
 
L440: msgline$(1)="Report Number "&ltrm$(rptn$)&" was not found."
	msgline$(2)="Do you wish to add it now? (Y/N)"
	fnoldmsgbox(mat response$,cap$,mat msgline$,2)
	if response$(1)="Y" then goto L500
	if response$(1)="N" then goto L290
 
L500: rt$="" : mat ch$=("") : ips=sd=cp=sc=0 : mat ps=(0) 
	mat f$=("") : mat pp=(0) : mat ppr=(0) : mat dp=(0) : mat fc=(0) 
	mat tcj=(0) : mat tcs=(0)
	write #1,using L410: rn,rt$,mat ch$,ips,sd,cp,sc,mat psc,mat f$,mat pp,mat ppr,mat dp,mat fc,mat tcj,mat tcs
L520: tempch$(1)=ch$(1)(1:66)
	tempch$(2)=ch$(1)(67:132)
	tempch$(3)=ch$(2)(1:66)
	tempch$(4)=ch$(2)(67:132)
L560: pr newpage
	fnopenwin(win=103,2,6,22,73,cap$)
	pr #win: newpage
	pr #win,fields "02,02,Cr 14,N": "Report Number:"
	pr #win,fields "03,02,Cr 14,N": "Report Title:"
	pr #win,fields "05,01,Cc 68,R,N": "Column Headings (line 1)"
	pr #win,fields "06,02,C 66,N": "    5   10   15   20   25   30   35   40   45   50   55   60    66"
	pr #win,fields "08,02,C 66,N": "  70   75   80   85   90   95  100  105  110  115  120  125    132"
	pr #win,fields "11,01,Cc 68,R,N": "Column Headings (line 2)"
	pr #win,fields "13,02,C 66,N": "    5   10   15   20   25   30   35   40   45   50   55   60    66"
	pr #win,fields "15,02,C 66,N": "  70   75   80   85   90   95  100  105  110  115  120  125    132"
	pr #win,fields "17,2,Cr 51,N": "Item Number for pr Selection (blank for all):"
	pr #win,fields "18,2,Cr 51,N": "Summarize Category Records (1=Y):"
	pr #win,fields "19,2,Cr 51,N": "Use Condensed pr (1=Y):"
	pr #win,fields "20,2,Cr 51,N": "Selection Codes: (1.=  2.>=  3.<=  4.number range):"
	io1$(01)="02,17,Nz 2,UT,N"
	io1$(02)="03,17,C 51,CUT,N"
	io1$(03)="07,02,C 66,UT,N"
	io1$(04)="09,02,C 66,UT,N"
	io1$(05)="12,02,C 66,UT,N"
	io1$(06)="14,02,C 66,UT,N"
	io1$(07)="17,54,Nz 3,UT,N"
	io1$(08)="18,54,Cu 1,UT,N"
	io1$(09)="19,54,Cu 1,UT,N"
	io1$(10)="20,54,Nz 1,UT,N"
L810: mat fkey$=("") 
	fkey$(1)="Next" 
	fkey$(4)="Delete" 
	fkey$(5)="Done" 
	em$="" 
	fnfkey(23,mat fkey$,mat disfk,em$)
	if sd=1 then sd$="Y" else sd$="N"
	if cp=1 then cp$="Y" else cp$="N"
L840: rinput #win,fields mat io1$: rn,rt$,mat tempch$,ips,sd$,cp$,sc conv CONV1
	if ce>0 then io1$(ce)(ce1:ce2)="U": ce=0
	if cmdkey>0 then goto L930 else ce=curfld
L870: ce=ce+1: if ce>udim(io1$) then ce=1
L880: io1$(ce)=rtrm$(io1$(ce)) : ce1=pos(io1$(ce),"U",1) 
	if ce1=0 then goto L870
	ce2=ce1+1 : io1$(ce)(ce1:ce1)="UC" : goto L840
CONV1: if ce>0 then io1$(ce)(ce1:ce2)="U"
	ce=cnt+1
ERR1: pr f "24,78,C 1": bell : goto L880
L930: if cmdkey=5 then goto MENU1
	if sd$<>"Y" and sd$<>"N" then ce=8 : goto ERR1
	if sd$="Y" then sd=1 else sd=0
	if cp$<>"Y" and cp$<>"N" then ce=9 : goto ERR1
	if cp$="Y" then cp=1 else sd=0
	if rn<1 then ce=1 : goto ERR1
	if cmdkey=4 then goto L1080
	if rn=rptn then goto L1120
	rnew$=lpad$(str$(rn),2)
	read #1,using L1030,key=rnew$: rnew nokey L1120
L1030: form pos 1,n 2
	pr f "2,40,C 38,N": "DUPLICATE REPORT NUMBER.  PRESS ENTER."
L1050: input fields "2,79,C 1,N": cnt$ conv L1050
	goto L810
 
L1080: rptn$=lpad$(str$(rptn),2)
	delete #1,key=rptn$:
	goto L290
 
L1120: if ips<0 or ips>124 then goto L1130 else goto L1160
L1130: ce=7
goto L2910

L1160: if sd<0 or sd>1 then goto L1170 else goto L1200
L1170: ce=8
goto L2910

L1200: if cp<0 or cp>1 then goto L1210 else goto L1240
L1210: ce=9
	goto L2910

L1240: if sc<0 or sc>4 then goto L1250 else goto L1280
L1250: ce=10
	goto L2910

L1280: ch$(1)=tempch$(1)&tempch$(2)
	ch$(2)=tempch$(3)&tempch$(4)
	if ips=0 then goto L1570
	pr newpage
	for j=1 to 20
		fd$(j)=str$(j+1)&",40,N 12.3,UT,N"
	next j
	for w=1 to 5
		for j=2 to 21
			pr f "1,25,C 50": "Job Cost Report Number "&str$(rptn)
			pr f str$(j)&",5,C 30,N": "Print Selection Criteria"
		next j
		if w=1 then goto L1410 else goto L1430
L1410: k=0
		goto L1440
L1430: k=k+20
L1440: for q=1 to 20
			rptemp(q)=psc(q+k)
		next q
L1470: rinput fields mat fd$: mat rptemp conv L1470
		for q=1 to 20
			psc(q+k)=rptemp(q)
		next q
		if rptemp(20)><0 then goto L1530
		lst=1
L1530: mat rptemp=(0)
		pr newpage
		if lst=1 then goto L1570
	next w
L1570: for j=1 to 20
L1580: pr newpage
		win=105
		fnopenwin(win,07,04,19,77,cap$)
		pr #win,fields "4,2,Cr 21,N": "Report Number:" 
		pr #win,fields "4,24,C 2,N": str$(rptn)
		pr #win,fields "5,2,Cr 21,N": "Column Number:" 
		pr #win,fields "5,24,C 10,N": str$(j)
		pr #win,fields "06,2,Cr 21,N": "Formula for Printing:"
		pr #win,fields "07,2,Cr 21,N": "Starting Position:"
		pr #win,fields "08,2,Cr 21,N": "Field Size:"
		pr #win,fields "09,2,Cr 21,N": "Decimal Positions:"
		pr #win,fields "10,2,Cr 21,N": "Detail pr (Y/N):"
		pr #win,fields "11,2,Cr 21,N": "Total by Job (Y/N):"
		pr #win,fields "12,2,Cr 21,N": "Grand Totals (Y/N):"
		mat fkey$=("") 
		fkey$(1)="Next" 
		fkey$(2)="Back" 
		fkey$(3)="Screen 1" 
		fkey$(4)="Completed" 
		em$="" 
		fnfkey(20,mat fkey$,mat disfk,em$)
		io2$(1)="06,24,C 50,UT,N"
		io2$(2)="07,24,Nz 3,UT,N"
		io2$(3)="08,24,Nz 3,UT,N"
		io2$(4)="09,24,N 01,UT,N"
		io2$(5)="10,24,Cu 1,UT,N"
		io2$(6)="11,24,Cu 1,UT,N"
		io2$(7)="12,24,Cu 1,UT,N"
		if fc(j)=1 then detailprint$="N" else detailprint$="Y"
		if tcj(j)=1 then totalbyjob$="Y" else totalbyjob$="N"
		if tcs(j)=1 then grandtotal$="Y" else grandtotal$="N"
L1810: rinput #win,fields mat io2$: f$(j),pp(j),ppr(j),dp(j),detailprint$,totalbyjob$,grandtotal$ conv CONV2
		if ce>0 then io2$(ce)(ce1:ce2)="U": ce=0
		if cmdkey>0 then goto L1900 else ce=curfld
L1840: ce=ce+1: if ce>udim(io2$) then ce=1
L1850: io2$(ce)=rtrm$(io2$(ce)) 
		ce1=pos(io2$(ce),"U",1) 
		if ce1=0 then goto L1840
		ce2=ce1+1 : io2$(ce)(ce1:ce1)="UC" : goto L1810
CONV2: if ce>0 then io2$(ce)(ce1:ce2)="U"
		ce=cnt+1
ERR2: pr f "24,78,C 1": bell : goto L1850
L1900: if rtrm$(f$(j))="" then goto L2020
		if detailprint$="Y" then fc(j)=0 else fc(j)=1
		if totalbyjob$="Y" then tcj(j)=1 else tcj(j)=0
		if grandtotal$="Y" then tcs(j)=1 else tcs(j)=0
		if detailprint$<>"Y" and detailprint$<>"N" then ce=5 : goto ERR2
		if totalbyjob$<>"Y" and totalbyjob$<>"N" then ce=6 : goto ERR2
		if grandtotal$<>"Y" and grandtotal$<>"N" then ce=7 : goto ERR2
		if pp(j)<1 or pp(j)>198 then ce=2: goto L3000
		if ppr(j)<1 or ppr(j)>198 then ce=3: goto L3000
		if fc(j)<0 or fc(j)>1 then ce=5: goto L3000
		if tcj(j)<0 or tcj(j)>1 then ce=6: goto L3000
		if tcs(j)<0 or tcs(j)>1 then ce=7: goto L3000
L2020: if cmdkey=4 then goto L2080
		if cmdkey=3 then goto L560
		if cmdkey=2 then j=j-1 else goto L2060
		if j>0 then goto L1580
L2060: if cmdkey=1 then goto L2070
L2070: next j
L2080: if rptn=rn then goto L2120
	delete #1,key=rptn$:
	write #1,using L410: rn,rt$,mat ch$,ips,sd,cp,sc,mat psc,mat f$,mat pp,mat ppr,mat dp,mat fc,mat tcj,mat tcs
	goto L2130
L2120: rewrite #1,using L410,key=rptn$: rn,rt$,mat ch$,ips,sd,cp,sc,mat psc,mat f$,mat pp,mat ppr,mat dp,mat fc,mat tcj,mat tcs
L2130: close #1:
	fnrx(rn)
	execute "INDEX S:\acsPR\JCREPORT.MST,S:\acsPR\JCREPORT.idx,1,2,Replace,DupKeys -n"
	fnchain('S:\acsPR\jcRptS1')
 
L2180: pr newpage
	restore #1,key>="  ": nokey L2850
	fnopenwin(win=102,10,28,15,52,cap$)
	wrd3$(1)="Print All Report Files"
	wrd3$(2)="Select Reports to Print"
	io3$(1)="4,2,C 23,N"
	io3$(2)="5,2,C 23,N"
	pr f "16,34,C 11,B,5": "Cancel (F5)"
	rinput #win,select mat io3$,attr "H": mat wrd3$
	prtall=curfld-1
	close #win: ioerr L2290
L2290: if cmdkey=5 then goto MENU1
	if prtall=0 then goto L2400
	for j=1 to 20
		fnopenwin(win=103,10,20,15,59,cap$)
		if j>1 then pr #win,fields "6,1,Cc 40,R,N": "Last Report Number Entered was "&rno$(j-1)
		pr #win,fields "4,2,C 23,N": "Report Number to Print:"
		pr f "16,35,C 09,B,5": "Done (F5)"
L2360: input #win,fields "4,26,N 2,UET,N": rno(j) conv L2360
		rno$(j)=lpad$(str$(rno(j)),2)
		if cmdkey=5 or rno(j)=0 then goto L2400
	next j
L2400: pr newpage
	fnwait(message$="Printing Proof List...",1)
	on fkey 5 goto L2850
	fnopenprn
	k=0
L2450: if prtall=0 then goto L2500
L2460: k=k+1
	if val(rno$(k))=0 then goto L2850
	read #1,using L2510,key=rno$(k): rn,rt$,mat ch$,ips,sd,cp,sc,mat psc,mat f$,mat pp,mat ppr,mat dp,mat fc,mat tcj,mat tcs nokey L2460
	goto L2520
L2500: read #1,using L2510: rn,rt$,mat ch$,ips,sd,cp,sc,mat psc,mat f$,mat pp,mat ppr,mat dp,mat fc,mat tcj,mat tcs eof L2850
L2510: form pos 1,n 2,c 51,x 27,2*c 132,n 3,3*n 1,100*pd 6.3,20*c 50,40*pd 2,80*n 1
L2520: pr #255,using L2530: "Job Cost Report File Proof List"
L2530: form skip 2,pos 50,c 32
	pr #255,using L2550: "Report Number",rn
L2550: form pos 1,c 13,pos 20,pic(zz)
	pr #255,using L2570: "Report Title",rt$
L2570: form pos 1,c 12,pos 13,cc 66
	pr #255,using L2590: "Column Headings",ch$(1)
L2590: form pos 1,c 15,skip 2,c 132
	pr #255,using L2610: ch$(2)
L2610: form pos 1,c 132,skip 2
	pr #255,using L2630: "Item # for Selection",ips
L2630: form pos 1,c 20,pos 30,pic(zz#)
	pr #255,using L2650: "Summarize Categories",sd
L2650: form pos 1,c 26,pos 32,pic(#)
	pr #255,using L2650: "Condense Print",cp
	pr #255,using L2650: "Selection Code",sc
	pr #255,using L2690: "Print Selection Criteria"
L2690: form skip 1,pos 1,c 30,skip 2
	for j=1 to 20
		pr #255,using L2720: psc(j),psc(j+20),psc(j+40),psc(j+60),psc(j+80)
L2720: form pos 1,5*n 20.3
	next j
	pr #255,using L2750: "Formula for Value","Starting","# of pr Positions","# of Decimal","Skip Detail Print","Total Column","Overall Totals"
L2750: form skip 1,pos 1,c 17,pos 39,c 8,pos 48,c 20,pos 71,c 12,pos 84,c 17,pos 103,c 12,pos 119,c 14
	pr #255,using L2770: "to be Printed","Print Position","Required","Positions","by Job","by System"
L2770: form pos 1,c 13,pos 38,c 14,pos 53,c 8,pos 72,c 9,pos 107,c 6,pos 123,c 9,skip 2
	for j=1 to 20
		pr #255,using L2800: f$(j),pp(j),ppr(j),dp(j),fc(j),tcj(j),tcs(j)
L2800: form pos 1,c 50,pos 52,n 3,pos 56,n 3,pos 76,n 1,pos 93,n 1,pos 110,n 1,pos 127,n 1
	next j
	pr #255: newpage
	goto L2450
 
L2850: fncloseprn
	on fkey 5 ignore
	goto MENU1
 
	if ce>0 then io1$(ce)(ce1:ce2)="U"
	ce=cnt+1
L2910: pr f "24,80,C 1,N": bell
	io1$(ce)=rtrm$(io1$(ce))
	ce1=pos(uprc$(io1$(ce)),"U",1)
	ce2=ce1+1
	io1$(ce)(ce1:ce1)="RC"
	goto L840
 
	if ce>0 then io2$(ce)(ce1:ce2)="U"
	ce=cnt+1
L3000: pr f "24,80,C 1,N": bell
	io2$(ce)=rtrm$(io2$(ce))
	ce1=pos(io2$(ce),"U",1)
	ce2=ce1+1
	io2$(ce)(ce1:ce1)="RC"
	goto L1810
 
SRCH: !
	bk=0
L3090: pr newpage
	fnopenwin(win=102,10,15,14,65,cap$)
	prtall=0
	pr #win,fields "4,2,C 39,N": "Starting Report Number (blank for all):"
	pr f "15,34,C 11,B,5": "Cancel (F5)"
L3140: input #win,fields "4,42,C 2,UT,N": nam$
	if cmdkey=5 then goto SRCHEND
	nam$=lpad$(rtrm$(nam$),2)
	restore #1,search>=nam$: nokey L3140
	close #win: ioerr L3190
L3190: pr newpage
	pr f "1,2,C 6,R,N": "Rep #:"
	pr f "1,9,C 40,R,N": "Report Name"
	cde=0
	for j=1 to 20
		read #1,using L410: rn,rt$,mat ch$,ips,sd,cp,sc,mat psc,mat f$,mat pp,mat ppr,mat dp,mat fc,mat tcj,mat tcs eof SREND
		cde=1
		pr f str$(j+1)&",2,N 2,N": rn
		pr f str$(j+1)&",9,C 40,N": rt$(1:40)
		if j>1 then goto L3320
		bk=bk+1
		if bk>20 then bk=1
		bk$(bk)=bl$(2)(1:28)
L3320: next j
SREND: if j>1 then j=j-1
	mat in2$(j)
	mat fkey$=("") 
	fkey$(1)="Next" 
	fkey$(2)="Back" 
	fkey$(5)="Stop" 
	em$="or Select Report Number:" 
	es=2 
	fnfkey(24,mat fkey$,mat disfk,em$,es)
L3360: input fields "24,67,C 2,UT,N": k$
	if cmdkey=5 then goto SRCHEND
	if rtrm$(k$)><"" then bl$=k$ : goto SRCHEND
	if cmdkey><2 then goto L3440
	bk=bk-1
	if bk<1 then goto L3460
	restore #1,key>=bk$(bk): nokey L3460
	bk=bk-1
L3440: selclp=1
	goto L3190
L3460: selclp=0
	goto L3090
 
SRCHEND: if rtrm$(k$)="" then goto L3550
	rn=rx=rptn=val(k$) conv L3550
	rptn$=lpad$(rtrm$(k$),2)
	read #1,using L410,key=rptn$: rn,rt$,mat ch$,ips,sd,cp,sc,mat psc,mat f$,mat pp,mat ppr,mat dp,mat fc,mat tcj,mat tcs nokey L3360
	ti=1 : goto L520
	close #101: ioerr L3550
L3550: goto MENU1
 
Xit: fnXit
 
include: Ertn
 
