! Replace S:\acsPR\JCInput
! Enter (Job Cost) Time
! ______________________________________________________________________
	library 'S:\Core\Library': fntop,fnxit, fnopenwin,fnwait,fnoldmsgbox, fnopenprn,fncloseprn,fnerror,fnxit,fnchain,fntop,fnconsole
	on error goto ERTN
! ______________________________________________________________________
	dim em$(3)*30,sub$*30,nam$*28,wrd1$(2)*38,wrd3$(4)*38,ln$*132
	dim cn$*11,k$*6,n$*40,en$*8,hr(2),empnam$*30,io2b$(2)*20
	dim ji1(6),jn$*6,ji2(5),label2$(12)*26,iolabel2$(12),io2$(12),iosc$(20)
	dim bk$(20)*28,nam$*28,ios$(2),wrds$(2)*30,b(4),a$(3)*30,sc$(20)*80
	dim message$*40
! ______________________________________________________________________
	fntop(program$,"Enter Time")

	fnconsole(1)
! ___________________________
	for j=1 to 20 : iosc$(j)=str$(j+1)&",10,C 40,UT,N" : next j
	label2$(1)="Employee Number:"
	label2$(2)="Method of Payment:"
	label2$(3)="Date (mmddyy):"
	label2$(4)="Payroll Department Number:"
	label2$(5)="Regular Hours:"
	label2$(6)="O.T. Hours:"
	label2$(7)="Job Number:"
	label2$(8)="Category:"
	label2$(9)="Sub-Category Code:"
	label2$(10)="Amount:"
	label2$(11)="Deduction/Addition Code:"
	label2$(12)="Units:"
! ___________________________
	io2$(1)="6,28,G 8,UT,N"
	io2$(2)="7,28,n 1,UT,N"
	io2$(3)="8,28,n 6,UT,N"
	io2$(4)="9,28,n 3,UT,N"
	io2$(5)="10,28,n 8.2,UT,N"
	io2$(6)="11,28,n 8.2,UT,N"
	io2$(7)="12,28,c 6,UT,N"
	io2$(8)="13,28,n 5,UT,N"
	io2$(9)="14,28,n 2,UT,N"
	io2$(10)="15,28,n 10.2,UT,N"
	io2$(11)="16,28,n 2,UT,N"
	io2$(12)="17,28,n 8.2,UT,N"
! ___________________________
	for j=1 to 12 : iolabel2$(j)=str$(j+3)&",2,Cr 26,N" : next j
! ______________________________________________________________________
	open #1: "Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-no.h[cno],Shr",internal,input,keyed 
	open #5: "Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-name.h[cno],Shr",internal,input,keyed 
	open #2: "Name=[Q]\PRmstr\RPTRAIL.h[cno],Shr",internal,input,relative 
	open #3: "Name="&env$('temp')&"\Work."&session$&",SIZE=0,RecL=84,Replace",internal,outIn,relative 
	open #11: "Name=[Q]\PRmstr\JCMSTR.h[cno],KFName=[Q]\PRmstr\JCIndx.h[cno],Shr",internal,input,keyed 
	open #14: "Name=[Q]\PRmstr\JCMSTR.h[cno],KFName=[Q]\PRmstr\JCINDX2.H[cno],Shr",internal,input,keyed 
	open #12: "Name=[Q]\PRmstr\JCCAT.H[cno],KFName=[Q]\PRmstr\CatIndx.h[cno],Shr",internal,input,keyed 
	open #13: "Name=[Q]\PRmstr\SCMSTR.h[cno],KFName=[Q]\PRmstr\SCIndex.h[cno],Shr",internal,input,keyed 
! ______________________________________________________________________
SCR1: ! 
	pr newpage
	fnopenwin(win=101,09,20,14,59,env$('program_caption'))
	wrd1$(1)="1. Regular input"
	wrd1$(2)="2. Input from Diskette"
	io1$(1)="4,2,C 38,N"
	io1$(2)="5,2,C 38,N"
	pr f "15,35,C 09,B,5": "Exit (F5)"
L630: rinput #win,select mat io1$,attr "H": mat wrd1$
	if cmdkey=5 then goto XIT
	on curfld goto SCREENFORINPUT1,L3220 none L630
! ______________________________________________________________________
SCREENFORINPUT1: ! 
	shoption=1 ! john did this to remove some other stuff
SCREENFORINPUT2: ! 
	pr newpage
	fnopenwin(win=102,5,4,20,75,env$('program_caption'))
	io2$(01)="04,29,G 8,UT,N"
	io2$(02)="05,29,N 1,UT,N"
	io2$(03)="06,29,N 6,UT,N"
	io2$(04)="07,29,N 3,UT,N"
	io2$(05)="08,29,N 8.2,UT,N"
	io2$(06)="09,29,N 8.2,UT,N"
	io2$(07)="10,29,C 6,UT,N"
	io2$(08)="11,29,N 5,UT,N"
	io2$(09)="12,29,N 2,UT,N"
	io2$(10)="13,29,N 10.2,UT,N"
	io2$(11)="14,29,N 2,UT,N"
	io2$(12)="15,29,N 8.2,UT,N"
	pr #win,fields mat iolabel2$: mat label2$
	pr #win,fields "5,31,C 28,N": "(1.Salary, 2.Hourly, 3.Both)"
	pr #win,fields "14,32,C 40,N": "(0.Regular, 1-10.Deduct #, 11.Extra Pay)"
	if shoption=1 then pr f "21,25,C 09,B,1": "Next (F1)"
	if shoption=1 then pr f "21,35,C 09,B,5": "Stop (F5)"
	if shoption=1 then pr f "21,45,C 09,B,6": "Help (F6)"
	if shoption=2 then pr f "21,19,C 09,B,1": "Next (F1)"
	if shoption=2 then pr f "21,29,C 11,B,2": "Delete (F2)"
	if shoption=2 then pr f "21,41,C 09,B,5": "Stop (F5)"
	if shoption=2 then pr f "21,51,C 09,B,6": "Help (F6)"
	if c1=2 or jcsrch=1 then pr #win,fields mat io2$: mat ji1,jn$,mat ji2
	jcsrch=0
	if ji1(1)=0 then io2$(4)="7,29,N 3,UT,N" : goto L1000
L970: pr #win,fields mat io2$: ji1(1),ji1(2),ji1(3)
! io2$(4)="7,29,N 3,CUT,N"
	if ck1=6 and ji1(2)=2 then goto L1110
L1000: input #win,fields mat io2$: en$,ji1(2),ji1(3),ji1(4),ji1(5),ji1(6),jn$,mat ji2 conv CONV1
	ck1=cmdkey
	ji1(1)=val(en$) conv L1040
	goto L1070
L1040: ck1=6
	ce4=1 : nam$=uprc$(rtrm$(en$))
	goto L2870
L1070: if ce>0 then io2$(ce)(ce1:ce2)="U": ce=0
	ce4=curfld
	if ck1>0 then goto L1230 else ce=curfld
	if ce<>1 then goto L1130
L1110: en$=lpad$(str$(ji1(1)),8)
	read #1,using L1390,key=en$: empnam$,ta1 nokey ERR1 
	pr #win,fields "4,38,C 30,N": empnam$
L1130: if ce<>7 then goto L1150
	read #11,using L1500,key=lpad$(rtrm$(jn$),6): n$ nokey ERR1 
	pr #win,fields "10,36,C 30,N": n$(1:30)
L1150: if ce<>9 then goto L1170
	read #13,using L1590,key=lpad$(str$(ji2(2)),3): sub$ nokey ERR1 
	pr #win,fields "12,32,C 30,N": sub$
L1170: ce=ce+1: if ce>udim(io2$) then ce=1
L1180: io2$(ce)=rtrm$(uprc$(io2$(ce))) 
	ce1=pos(io2$(ce),"U",1) 
	if ce1=0 then goto L1130
	ce2=ce1+1 
	io2$(ce)(ce1:ce1)="UC" 
	goto L1000
CONV1: if ce>0 then io2$(ce)(ce1:ce2)="U"
	ce=cnt+1
ERR1: pr f "24,78,C 1": bell : goto L1180
L1230: if ck1=5 then goto WHATNOWSCREEN
	io2$(8)="11,29,N 5,UT,N"
	if ck1=6 and ce4=1 then goto L2790
	if ck1=6 and ce4=7 then goto L3880
	if ck1=2 and c1=2 then goto L1760
	if ck1><1 then ce=curfld : goto L1180
	ptp=0
	mat hr=(0)
	er=0
	if ji1(2)<1 or ji1(2)>3 then ce=2: goto ERR1
	if ji1(3)<10100 or ji1(3)>123199 then ce=3: goto ERR1
	if ji1(1)=0 then goto L1460
	en$=lpad$(str$(ji1(1)),8)
	ce=1
	read #1,using L1390,key=en$: empnam$,ta1 nokey ERR1
	ce=0
L1390: form pos 9,c 30,pos 173,pd 3
L1400: read #2,using L1410,rec=ta1: tdn,ptp,sal,mat hr,nta
L1410: form pos 9,n 3,pos 30,n 6.3,pos 58,3*pd 4.2,pos 468,pd 3
	if tdn=ji1(4) then goto L1460
	if nta=0 then ce=4: goto ERR1
	ta1=nta
	goto L1400
L1460: if ji2(3)=0 then ji2(3)=hr(1)*ji1(5)+hr(2)*ji1(6)
	if rtrm$(jn$)="" and ji2(1)=0 then goto L1560
	ce=7
	read #11,using L1500,key=lpad$(rtrm$(jn$),6): n$ nokey ERR1
L1500: form pos 7,c 40
	ce=8
	cn$=lpad$(rtrm$(jn$),6)&lpad$(str$(ji2(1)),5)
	read #12,using L1540,key=cn$: kc$ nokey ERR1
L1540: form pos 1,c 11
	ce=0
L1560: if ji2(2)=0 then goto L1610
	ce=9
	read #13,using L1590,key=lpad$(str$(ji2(2)),3): sub$ nokey ERR1
L1590: form pos 4,c 30
	ce=0
L1610: if ji2(3)=0 then ce=10: goto ERR1
	if ji2(4)<1 or ji2(4)>10 then pt=ptp/100*ji2(3) else pt=0
	if c1=2 then goto L1740
L1640: rw=lrec(3)+1
	write #3,using L1660,rec=rw: mat ji1, jn$, mat ji2, pt, empnam$, sal duprec L1640
L1660: form pos 1,n 8,n 1,pd 4,pd 2,2*pd 4.2,c 6,2*pd 3,pd 5.2,n 2,2*pd 4.2,c 30,pd 4.2
	for j=1 to 5
		ji2(j)=0
	next j
	ji1(5)=0
	ji1(6)=0
	jn$=""
	goto SCREENFORINPUT2
L1740: rewrite #3,using L1660,rec=rr: mat ji1,jn$,mat ji2,pt,empnam$,sal
	goto L1790
L1760: rewrite #3,using L1660,rec=rr: -1,0,0,0,0,0,"",0,0,0,0,0,0,"",0
	goto L1790
L1780: shoption=2 ! john did this to remove some other stuff
L1790: pr newpage
	win=104
	fnopenwin(win,10,20,14,59,env$('program_caption'))
	pr #win,fields "04,2,C 28,N": "Reference Number to correct:"
	pr f "15,35,C 09,B,5": "Done (F5)"
L1840: input #win,fields "04,31,N 5,UT,N": rr conv L1840
	if cmdkey=5 then goto WHATNOWSCREEN
	if rr<1 or rr>rw then goto L1840
	close #win: ioerr L1880
L1880: read #3,using L1660,rec=rr: mat ji1,jn$,mat ji2,pt,empnam$,sal
	goto SCREENFORINPUT2
! ______________________________________________________________________
WHATNOWSCREEN: ! 
	pr newpage
	win=103
	fnopenwin(win,8,20,15,59,env$('program_caption'))
	wrd3$(1)="1. pr Input Proof List"
	wrd3$(2)="2. Corrections"
	wrd3$(3)="3. Additional Entries"
	wrd3$(4)="4. Post to Job Cost File"
	for j=1 to udim(wrd3$)
		io3$(j)=str$(j+3)&",2,C 38,N"
	next j
	pr f "16,27,C 25,B,5": "Exit Without Posting (F5)"
	rinput #win,select mat io3$, attr "H": mat wrd3$
	close #win: ioerr L970
	if cmdkey=5 then goto XIT
	c1=curfld
	on c1 goto PRINTPROOFLIST,L1780,SCREENFORINPUT1,POSTTOJOBS none WHATNOWSCREEN
! ______________________________________________________________________
PRINTPROOFLIST: ! 
	pr newpage
	message$="Printing Input Proof List..."
	fnwait(message$,1)
	on fkey 5 goto PROOF_LIST_DONE
	fnopenprn(cp,58,220,process)
	goto L2250
! ______________________________________________________________________
PROOF_LIST_HDR: ! 
	pr #255,using L2210: env$('cnam')
	pr #255,using L2210: "Job Cost Input Proof List"
	pr #255,using L2210: "Date: "&date$&"      Time: "&time$
L2210: form pos 1,cc 113,skip 1
	pr #255: "Ref #   Emp #  Method-Pay  Date   Dept  Reg-Hrs   OT-Hrs  Job #   Category  Sub-Category   Amount  Ded-Add  Units"
	return 
! ______________________________________________________________________
L2250: gosub PROOF_LIST_HDR
	for j=1 to lrec(3)
		read #3,using L1660,rec=j: mat ji1,jn$,mat ji2,pt
		if j=1 then goto L2320
		if ji1(1)=en then goto L2360
		pr #255,using L2310: " ________"," ________"," ____________",t5,t6,t10 pageoflow PROOF_LIST_NWPG
L2310: form pos 38,2*c 9,x 29,c 13,skip 1,pos 8,"Total",pos 38,2*n 9.2,x 29,n 13.2,skip 2
L2320: en=ji1(1)
		t5=0
		t6=0
		t10=0
L2360: pr #255,using L2370: j,mat ji1,jn$,mat ji2 pageoflow PROOF_LIST_NWPG
L2370: form pos 1,n 5,n 8,n 6,n 13,n 5,2*n 9.2,x 2,c 6,n 11,n 10,n 13.2,n 6,n 10.2,skip 1
		t5=t5+ji1(5)
		t6=t6+ji1(6)
		t10=t10+ji2(3)
		gt5=gt5+ji1(5)
		gt6=gt6+ji1(6)
		gt10=tg10+ji2(3)
	next j
	pr #255,using L2310: " ________"," ________"," ____________",t5,t6,t10
	pr #255,using L2470: " ________"," ________"," ____________",gt5,gt6,gt10
L2470: form pos 38,2*c 9,x 29,c 13,skip 1,pos 8,"Grand Totals",pos 38,2*n 9.2,x 29,n 13.2,skip 2
PROOF_LIST_DONE: ! 
	gt5=gt6=gt10=0
	fncloseprn
	goto WHATNOWSCREEN
! ______________________________________________________________________
POSTTOJOBS: ! 
	close #1: 
	close #2: 
	close #3: 
	close #11: 
	close #12: 
	close #13: 
	fnchain("S:\acsPR\JCMerge")
! ______________________________________________________________________
PROOF_LIST_NWPG: ! 
	pr #255: newpage
	gosub PROOF_LIST_HDR
	continue 
	if err=61 then goto SCREENFORINPUT2
! ______________________________________________________________________
XIT: fnxit
! ______________________________________________________________________
L2790: pr newpage
	close #101: ioerr L2810
L2810: open #101: "SROW=6,SCOL=8,EROW=08,ECOL=75,BORDER=DR,CAPTION=ALPHA NAME SEARCH",display,outIn 
	prtall=0
	pr f "7,9,C 45,H,N": "Employee Name (blank for all):"
	pr f "9,34,C 11,B,5": "Cancel (F5)"
L2850: input fields "7,47,C 28,UET,N": nam$
	if cmdkey=5 then goto SCREENFORINPUT1
L2870: l1=max(1,len(rtrm$(nam$)))
	if rtrm$(nam$)="" then prtall=1
	restore #5,search>=nam$(1:l1): nokey L2850
L2900: form pos 1,c 8,c 30
L2910: pr newpage
	close #101: ioerr L2930
L2930: open #101: "SROW=2,SCOL=8,EROW=23,ECOL=60,BORDER=DR",display,outIn 
	pr f "1,10,C 40,R,N": "  Number  Name                       "
	cde=0
	mat sc$(20)
	for j=1 to 20
		read #5,using L2900,release: a$,n$ eof L3030
		if nam$(1:l1)=n$(1:l1) or prtall=1 then goto L3000 else goto L3030
L3000: cde=1
		sc$(j)=a$&"  "&n$
	next j
L3030: if cde=0 then goto L3190
	if j<1 then j=1 else j=j-1
	mat sc$(j)
	pr f mat iosc$: mat sc$
	pr f "24,08,C 45,R,N": " Enter to SELECT; F1 TO CONTINUE; F5 to stop"
L3080: input select mat iosc$: mat sc$
	if cmdkey=5 then goto L2790
	if cmdkey=1 then goto L3160
	ent$=sc$(curfld)(1:8)
	read #1,using L3130,key=ent$: eno,mat em$ nokey L3080
L3130: form pos 1,n 8,3*c 30
	ent=eno
	ji1(1)=eno: ji1(2)=2: goto SCREENFORINPUT2
L3160: if cmdkey=5 then goto L2790
	selclp=1
	goto L2910
L3190: selclp=0
	goto L2790
! ______________________________________________________________________
L3220: pr newpage ! INPUT FROM DISKETTE FILE
	win=102
	fnopenwin(win,10,20,15,59,env$('program_caption'))
	io2b$(1)="8,42,CU 1,UET,N"
	io2b$(2)="10,43,Nz 6,UET,N"
	pr #win,fields "4,2,Cr 26,N": "Diskette Drive:"
	pr #win,fields "5,2,Cr 26,N": "Transaction Date (mmddyy):"
	io2b$(1)="4,29,Cu 1,UT,N"
	io2b$(2)="5,29,Nz 6,UT,N"
	pr f "16,30,C 09,B,1": "Next (F1)"
	pr f "16,41,C 11,B,5": "Cancel (F5)"
	dv$="A"
L3340: rinput #win,fields mat io2b$: dv$,td1 conv CONV2
	if ce>0 then io2b$(ce)(ce1:ce2)="U": ce=0
	if cmdkey>0 then goto L3430 else ce=curfld
L3370: ce=ce+1: if ce>udim(io2b$) then ce=1
L3380: io2b$(ce)=rtrm$(io2b$(ce)) 
	ce1=pos(io2b$(ce),"U",9) 
	if ce1=0 then goto L3370
	ce2=ce1+1 : io2b$(ce)(ce1:ce1)="UC" : goto L3340
CONV2: if ce>0 then io2b$(ce)(ce1:ce2)="U"
	ce=cnt+1
ERR2: pr f "24,78,C 1": bell : goto L3380
L3430: if cmdkey=5 then goto SCR1
	if dv$="A" or dv$="B" then goto L3450 else ce=1: goto ERR2
L3450: if td1<10100 or td1>123199 then ce=2: goto ERR2
	open #9: "Name="&dv$&":TIMEDAT.EXP",display,input 
L3470: linput #9: ln$ eof L3850
	if ln$(1:1)=>"0" and ln$(1:1)<="9" then goto R1
	if rtrm$(ln$(1:5))="" and ln$(6:7)=>"01" and ln$(6:7)<="99" then goto R2
	if rtrm$(ln$(1:10))="" and ln$(11:11)=>"0" and ln$(11:11)<="9" then goto R3
R1: j1=val(ln$(1:8)) conv L3470
	if j1>0 then goto L3540
	goto L3470
L3540: if j1=ji1(1) then goto L3470 ! TOTAL RECORD
	mat ji1=(0)
	mat ji2=(0)
	ji1(1)=j1 ! EMPLOYEE #
	ji1(2)=2 ! HOURLY
	ji2(1)=1 ! Category - LABOR
	goto L3470
R2: ji2(2)=ji1(4)=val(ln$(1:10))
	ji1(3)=td1
	goto L3470
R3: jn$=rtrm$(ltrm$(ln$(11:18)))
	for j=1 to len(jn$)
		if jn$(j:j)<"0" or jn$(j:j)>"9" then jn$(j:j)=""
	next j
	jn$=cnvrt$("N 6",val(jn$)) conv L3470
	ji1(5)=val(ln$(72:82)) ! REG HRS
	ji1(6)=val(ln$(83:90)) ! OVT HRS
	empnam$=""
	en$=cnvrt$("N 8",ji1(1))
	read #1,using L3740,key=en$: empnam$,ta1 nokey L3830
L3740: form pos 9,c 30,pos 173,pd 3
L3750: read #2,using L3760,rec=ta1: tdn,ptp,sal,mat hr,nta
L3760: form pos 9,n 3,pos 30,n 6.3,pos 58,3*pd 4.2,pos 468,pd 3
	if tdn=ji1(4) then goto L3800
	if nta=0 then goto L3830
	ta1=nta : goto L3750
L3800: ji2(3)=hr(1)*ji1(5)+hr(2)*ji1(6)
	if ji2(4)<1 or ji2(4)>10 then pt=ptp/100*ji2(3) else pt=0
L3820: rw=lrec(3)+1
L3830: write #3,using L1660,rec=rw: mat ji1,jn$,mat ji2, pt, empnam$, sal duprec L3820
	goto L3470
L3850: close #9: 
	sh$(1)="   JOB COST INPUT TIME FROM DISKETTE"
	goto WHATNOWSCREEN
L3880: ! ______________________________________________________________________
SRCH: bk=0 : pr newpage
	wrds$(1)="1. JOB # SEARCH" : skl(1)=6
	wrds$(2)="2. JOB NAME SEARCH" : skl(2)=25
	seq=2 : fil=14: goto L4020
	for j=1 to udim(ios$): ios$(j)=str$(j+12)&",25,C 30,N": next j
	close #101: ioerr L3950
L3950: open #101: "SROW=12,SCOL=24,EROW="&str$(udim(ios$)+13)&",ECOL=55,BORDER=SR,CAPTION=SELECT TYPE OF SEARCH",display,outIn 
	pr #101: newpage
	pr f mat ios$: mat wrds$
	pr f str$(udim(ios$)+14)&",32,C 16,R,N": "PRESS F5 TO STOP"
	input select mat ios$: mat wrds$
	seq=curfld
	if cmdkey=5 then goto SRCHEND
L4020: pr newpage
	close #101: ioerr L4040
L4040: open #101: "SROW=6,SCOL=10,EROW=10,ECOL=69,BORDER=DR,CAPTION="&wrds$(seq),display,outIn 
	prtall=0
	pr f "7,11,C 22,N": "Beginning Search Data:"
	pr f "9,11,C 58,H,N": "NOTE: Enter Beginning Search Data as blank for all records"
	pr f "11,32,C 16,R,N": "PRESS F5 TO STOP"
L4090: input fields "7,34,C "&str$(skl(seq))&",UT,N": nam$
	if cmdkey=5 then goto SRCHEND
	if seq=1 then nam$=lpad$(rtrm$(nam$),skl(1))
	if seq=2 then nam$=rpad$(ltrm$(nam$),skl(2))
	restore #14,search>=nam$: nokey L4090
	close #101: ioerr L4150
L4150: pr newpage
	pr f "1,2,C 6,R,N": "JOB #:"
	pr f "1,9,C 40,R,N": "JOB NAME"
	pr f "1,50,C 8,R,N": "EST.Date"
	pr f "1,59,C 10,R,N": "EST.Date"
	pr f "1,59,C 10,R,N": " CT.AMOUNT"
	pr f "1,70,C 10,R,N": "BLD.AMOUNT"
	cde=0
	for j=1 to 20
		read #14,using L4250: jn$,n$,mat a$,mat b eof SREND
L4250: form pos 1,c 6,c 40,3*c 30,n 6,2*pd 7.2,n 2
		cde=1
		pr f str$(j+1)&",2,C 6,UT,N": jn$
		pr f str$(j+1)&",9,C 40,UT,N": n$
		pr f str$(j+1)&",50,PIC(ZZ/ZZ/ZZ),UT,N": b(1)
		pr f str$(j+1)&",59,N 10.2,UT,N": b(2)
		pr f str$(j+1)&",70,N 10.2,UT,N": b(3)
		if j>1 then goto L4360
		bk=bk+1
		if bk>20 then bk=1
		bk$(bk)=bl$(2)(1:28)
L4360: next j
SREND: if j>1 then j=j-1
	mat in2$(j)
	pr f "24,02,C 67,R,N": "ENTER TO CONTINUE; F2 TO BACKUP; F5 TO STOP; Select ACCOUNT Number:"
L4400: input fields "24,70,C 6,RE,N": k$
	alp=0
	if cmdkey=5 then goto SRCHEND
	if rtrm$(k$)><"" then bl$=k$ : goto SRCHEND
	if cmdkey><2 then goto L4490
	bk=bk-1
	if bk<1 then goto L4510
	restore #14,key>=bk$(bk): nokey L4510
	bk=bk-1
L4490: selclp=1
	goto L4150
L4510: selclp=0
	goto L4020
SRCHEND: if rtrm$(k$)="" or len(rtrm$(k$))>6 then alp=0: goto L4580
	jn$=lpad$(rtrm$(k$),6)
	read #11,using L4250,key=jn$: hjn$,n$,mat a$,mat b nokey L4400
	ti=alp=3 : jcsrch=1: io2$(8)="11,29,N 5,UC,N": restore #11: : goto SCREENFORINPUT2
	close #101: ioerr L4580
L4580: restore #11: : goto SCREENFORINPUT2
include: Ertn
