! Replace S:\acsPR\jcIpBil
! Enter Billings
 
	autoLibrary
	on error goto Ertn
 
	dim msgline$(2)*60,response$(5)*1,wrd2$(4)*38,wrds$(2)*20,n$*40
	dim io1$(4),jn$*6,inp(3),n$*40,a$(3)*30,b(4),cap$*128,message$*40
 
	fnTop("S:\acsPR\jcIpBil",cap$="Enter Billings")
	fncno(cno)
 
	c1=3
	fnconsole(1)
 
	open #1: "Name=[Q]\PRmstr\JCMSTR.h[cno],KFName=[Q]\PRmstr\JCIndx.h[cno],Shr",internal,outIn,keyed
	open #2: "Name="&env$('temp')&"\Work."&session$,internal,output ioerr L180
	close #2,free:
L180: open #2: "Name="&env$('temp')&"\Work."&session$&",SIZE=0,RecL=17,Replace",internal,outIn,relative
L190: shoption=1
L200: pr newpage
	win=101
	fnopenwin(win,09,08,16,71,cap$)
	pr #win,fields "4,2,Cr 14,N": "Job Number:"
	pr #win,fields "5,2,Cr 14,N": "Amount:"
	pr #win,fields "6,2,Cr 14,N": "Date (mmddyy):"
	pr #win,fields "7,2,Cr 14,N": "Status:"
	io1$(1)="4,17,C 6,UT,N"
	io1$(2)="5,17,N 10.2,UT,N"
	io1$(3)="6,17,Nz 6,UT,N"
	io1$(4)="7,17,N 2,UT,N"
	if c1=2 then pr #win,fields mat io1$: jn$,mat inp
	if shoption=1 then pr f "17,24,C 09,B,1": "Next (F1)"
	if shoption=1 then pr f "17,34,C 09,B,5": "Done (F5)"
	if shoption=1 then pr f "17,44,C 11,B,6": "Search (F6)"
	if shoption=2 then pr f "17,17,C 09,B,1": "Next (F1)"
	if shoption=2 then pr f "17,27,C 11,B,4": "Delete (F4)"
	if shoption=2 then pr f "17,39,C 11,B,5": "Cancel (F5)"
	if shoption=2 then pr f "17,51,C 11,B,6": "Search (F6)"
L390: input #win,fields mat io1$: jn$,mat inp conv L390
	if ce>0 then io1$(ce)(ce1:ce2)="U": ce=0
	if cmdkey>0 then goto L510 else ce=curfld
	if ce<>1 then goto L450
	jn$=lpad$(rtrm$(jn$),6) : _
	read #1,using L440,key=jn$: n$ nokey L600 : _
	pr #win,fields "4,24,C 40,H,N": n$
L440: form pos 7,c 40
L450: ce=ce+1 : if ce>udim(io1$) then ce=1
L460: io1$(ce)=rtrm$(uprc$(io1$(ce))) : ce1=pos(io1$(ce),"U",1) : _
	if ce1=0 then goto L450
	ce2=ce1+1 : io1$(ce)(ce1:ce1)="UC" : goto L390
CONV1: if ce>0 then io1$(ce)(ce1:ce2)="U"
	ce=cnt+1
ERR1: pr f "24,78,C 1": bell : goto L460
L510: if cmdkey=5 then goto L730
	if cmdkey=6 then gosub SRCH : win=101 : _
		pr #win,fields io1$(1): k$ : c1=3: goto L390
	if c1=2 and cmdkey=4 then goto L1150
	if inp(2)<10100 or inp(2)>123199 then ce=3: goto ERR1
	if inp(1)=0 then ce=2: goto ERR1
	jn$=lpad$(rtrm$(jn$),6)
	read #1,using 'Form POS 150,PD 7.2,N 2',key=jn$: b3 nokey L600
	goto L660
 
L600: msgline$(1)="Job Number not found."
	msgline$(2)="Please reselect."
	fnoldmsgbox(mat response$,cap$,mat msgline$,1)
	ce=1
	goto ERR1
 
L660: ta=ta+inp(1)
	if c1=2 then goto L1120
L680: rw=lrec(2)+1
	write #2,using L700,rec=rw: jn$,mat inp duprec L680
L700: form pos 1,c 6,pd 5.2,pd 4,n 2
	goto L200
 
L730: pr newpage
	win=102
	fnopenwin(win,07,20,15,59,cap$)
	pr #win,fields "9,1,C 40,R,N": " Total of Amounts Entered:"
	pr #win,fields "9,28,N 10.2,R,N": ta
	wrd2$(1)="1. pr Billing Proof List"
	wrd2$(2)="2. Corrections"
	wrd2$(3)="3. Additional Entries"
	wrd2$(4)="4. Post to Job Cost File"
	for j=1 to udim(wrd2$)
		io2$(j)=str$(j+3)&",2,C 38,N"
	next j
	pr f "16,27,C 25,B,5": "Exit without posting (F5)"
L860: rinput #win,select mat io2$,attr "H": mat wrd2$
	c1=curfld
	if cmdkey=5 then goto Xit
	on c1 goto PROOFLIST,L1160,L190,L1300 none L860
 
PROOFLIST: !
	pr newpage
	message$="Printing Proof List..."
	fnwait(message$,1)
	on fkey 5 goto L1080
	fnopenprn(cp,58,220,process)
	pr #255,using L980: "Job Cost Input Billings Proof List"
L980: form skip 2,pos 10,c 60,skip 1
	pr #255: tab(10);"Date:  ";date$;"   Time:  ";time$
	pr #255:
	pr #255: "Ref #   Job #      Amount     Date     Status"
	for j=1 to lrec(2)
		read #2,using L700,rec=j: jn$,mat inp noRec L1100
		if ltrm$(jn$)="" or ltrm$(rtrm$(jn$))="0" then goto L1070
		pr #255,using L1060: j,jn$,mat inp
L1060: form pos 1,n 5,x 2,c 8,n 10.2,2*n 10,skip 1
L1070: next j
L1080: fncloseprn
	on fkey 5 ignore
L1100: goto L730
 
L1120: rewrite #2,using L700,rec=rr: jn$,mat inp
	goto L1160
 
L1150: rewrite #2,using L700,rec=rr: "",0,0,0
L1160: pr newpage
	win=103
	fnopenwin(win,10,20,14,59,cap$)
	pr #win,fields "4,2,C 28,N": "Reference Number to correct:"
	pr f "15,35,C 09,B,5": "Done (F5)"
L1210: input #win,fields "4,31,Nz 5,UT,N": rr conv L1210
	if cmdkey=5 then goto L730
	if rr=0 then goto L1210
	if rr<1 or rr>rw then goto L1210
	read #2,using L700,rec=rr: jn$,mat inp
	ta=ta-inp(1)
	shoption=2
	goto L200
 
L1300: for j=1 to rw
		read #2,using L700,rec=j: jn$,mat inp noRec Xit
		if ltrm$(jn$)="" or ltrm$(rtrm$(jn$))="0" then goto L1370
		read #1,using 'Form POS 150,PD 7.2,N 2',key=jn$: b3,b4 nokey L1370
		b3=b3+inp(1)
		if inp(3)><0 then b4=inp(3)
		rewrite #1,using 'Form POS 150,PD 7.2,N 2',key=jn$: b3,b4
L1370: next j
	goto Xit
 
Xit: fnXit
 
SRCH: !
	bk=0
	hce=ce
	close #103: ioerr L1460
L1460: open #103: "SROW=1,SCOL=1,EROW=24,ECOL=80",display,output
	pr #103: newpage
L1480: pr newpage
	win=102
	fnopenwin(win,06,10,10,69,wrds$(1))
	prtall=0
	pr #win,fields "4,2,C 38,N": "Beginning Search Data (blank for all):"
	pr f "11,34,C 11,B,5": "Cancel (F5)"
L1540: input #win,fields "4,41,C 6,UT,N": nam$
	if cmdkey=5 then goto SEARCHEND
	nam$=lpad$(rtrm$(nam$),6)
	restore #1,search>=nam$: nokey L1540
! Close #WIN: Ioerr 1790
L1590: pr newpage
	pr f "1,02,Cc 06,R,N": "Job No"
	pr f "1,09,Cc 36,R,N": "Job Name"
	pr f "1,46,Cc 08,R,N": "Est.Date"
	pr f "1,55,Cc 12,R,N": "Ct.Amount"
	pr f "1,68,Cc 12,R,N": "Bld.Amount"
	cde=0
	for j=1 to 20
		read #1,using L1680: jn$,n$,mat a$,mat b eof SREND
L1680: form pos 1,c 6,c 40,3*c 30,n 6,2*pd 7.2,n 2 ! pd 7.2 was pd 5.2
		cde=1
		pr f str$(j+1)&",2,C 6,N": jn$
		pr f str$(j+1)&",9,C 36,N": n$(1:36)
		pr f str$(j+1)&",46,PIC(ZZ/ZZ/ZZ),N": b(1)
		pr f str$(j+1)&",55,N 12.2,N": b(2)
		pr f str$(j+1)&",68,N 12.2,N": b(3)
		if j>1 then goto L1790
		bk=bk+1
		if bk>20 then bk=1
		bk$(bk)=bl$(2)(1:28)
L1790: next j
SREND: if j>1 then j=j-1
	mat in2$(j)
	pr f "24,09,C 09,B,1": "Next (F1)"
	pr f "24,19,C 09,B,2": "Back (F2)"
	pr f "24,29,C 09,B,5": "Stop (F5)"
	pr f "24,39,C 25,R,N": "or Select Account Number:"
	k$=""
	rinput fields "24,65,C 6,UT,N": k$
	alp=0
	if cmdkey=5 then goto SEARCHEND
	if cmdkey=1 then k$=""
	if rtrm$(k$)><"" then jn$=k$ : goto SEARCHEND
	if cmdkey><2 then goto L1970
	bk=bk-1
	if bk<1 then goto L2000
	restore #1,key>=bk$(bk): nokey L2000
	bk=bk-1
L1970: selclp=1
	goto L1590
 
L2000: selclp=0
	goto L1480
 
SEARCHEND: close #103: ioerr L2040
L2040: return
 
include: Ertn
 
