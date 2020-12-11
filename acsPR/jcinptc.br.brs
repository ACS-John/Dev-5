! Replace S:\acsPR\jcInptC
! Input (Job Cost) Charges
 
	autoLibrary
	on error goto Ertn
 
	dim cn$*11,k$*11,n$*40,en$*8,hr(2),empnam$*30,ds$*30,a$(3)*30,b(4)
	dim rn$*12,jn$*6,ji2(3),d$*30,label1$(7)*24,iolabel1$(7),io1$(7)
	dim bk$(20)*28,nam$*28,ios$(2),wrds$(2)*30,cnt$*25,cap$*128
	dim msgline$(2)*60,response$(5)*1,wrd2$(4)*38
 
	fncno(cno)
 
	cap$="Input (Job Cost) Charges"
	fnTop("S:\acsPR\jcInptC", "Enter Charges")
	fnconsole(1)
 
	data "Reference Number:"
	data "Date (mmddyy):"
	data "Job Number:"
	data "Category:"
	data "Sub-Category:"
	data "Amount:"
	data "Description:"
	read mat label1$
 
	io1$(1)="4,20,C 12,UT,N"
	io1$(2)="5,20,Nz 6,UT,N"
	io1$(3)="6,20,c 6,UT,N"
	io1$(4)="7,20,n 5,UT,N"
	io1$(5)="08,20,N 2,UT,N"
	io1$(6)="09,20,n 10.2,UT,N"
	io1$(7)="10,20,C 30,UT,N"
 
	for j=1 to 7
		iolabel1$(j)=str$(j+3)&",2,Cr 17,N"
	next j
 
	open #3: "Name=[Temp]\Work."&session$&",SIZE=0,RecL=63,Replace",internal,outIn,relative
	open #11: "Name=[Q]\PRmstr\JCMSTR.h[cno],KFName=[Q]\PRmstr\JCIndx.h[cno],Shr",internal,input,keyed
	open #12: "Name=[Q]\PRmstr\JCCAT.h[cno],KFName=[Q]\PRmstr\CatIndx.h[cno],Shr",internal,input,keyed
	open #13: "Name=[Q]\PRmstr\SCMSTR.h[cno],KFName=[Q]\PRmstr\SCIndex.h[cno],Shr",internal,input,keyed
 
INPUTSCREEN1: !
	shoption=1
INPUTSCREEN2: !
	pr newpage
	win=102
	fnopenwin(win,08,08,18,72,cap$)
	pr #win,fields mat iolabel1$: mat label1$
	pr #win,fields io1$(2): dat
	if shoption=1 then pr f "19,24,C 09,B,1": "Next (F1)"
	if shoption=1 then pr f "19,34,C 09,B,5": "Stop (F5)"
	if shoption=1 then pr f "19,44,C 11,B,5": "Search (F6)"
	if shoption=2 then pr f "19,18,C 09,B,1": "Next (F1)"
	if shoption=2 then pr f "19,28,C 11,B,4": "Delete (F4)"
	if shoption=2 then pr f "19,40,C 09,B,5": "Stop (F5)"
	if shoption=2 then pr f "19,50,C 11,B,5": "Search (F6)"
	if c1=2 then pr #win,fields mat io1$: rn$,dat,jn$,mat ji2,d$
L580: input #win,fields mat io1$: rn$,dat,jn$,mat ji2,d$ conv L1770
	if ce>0 then io1$(ce)(ce1:ce2)="U": ce=0
	if cmdkey>0 then goto L780 else ce=curfld
	if ce<>3 then goto L640
L620: read #11,using L900,key=lpad$(rtrm$(jn$),6): n$ nokey L630 : _
	pr #win,fields "6,27,C 38,H,N": n$(1:38) : _
	goto L640
L630: msgline$(1)="Job Number not on file." : _
	msgline$(2)="Please select a different Job Number" : _
	fnoldmsgbox(mat response$,cap$,mat msgline$,1) : _
	ce=3 : _
	goto ERR1
L640: if ce<>4 then goto L690
	jn$=lpad$(rtrm$(jn$),6)
	cat$=jn$&lpad$(rtrm$(str$(ji2(1))),5)
	read #12,using L2700,key=cat$: jn2$,cn$,cnt$ nokey L680 : _
	pr #win,fields "7,27,C 38,H,N": cnt$(1:38) : _
	goto L690
L680: msgline$(1)="Invalid Category Number" : _
	msgline$(2)="Please select a different Category Number" : _
	fnoldmsgbox(mat response$,cap$,mat msgline$,1) : _
	ce=4 : _
	goto ERR1
L690: if ce<>5 then goto L720
	read #13,using L1020,key=lpad$(str$(ji2(2)),3): ds$ nokey L710 : _
	pr #win,fields "8,27,C 38,H,N": ds$(1:38) : _
	goto L720
L710: msgline$(1)="Invalid Sub-Category Number" : _
	msgline$(2)="Please select a different Sub-Category Number" : _
	fnoldmsgbox(mat response$,cap$,mat msgline$,1) : _
	ce=5 : _
	goto ERR1
L720: ce=ce+1: if ce>udim(io1$) then ce=1
L730: io1$(ce)=rtrm$(io1$(ce)) : _
	ce1=pos(io1$(ce),"U",1) : _
	if ce1=0 then goto L640
	ce2=ce1+1 : io1$(ce)(ce1:ce1)="UC" : goto L580
CONV1: if ce>0 then io1$(ce)(ce1:ce2)="U"
	ce=cnt+1
ERR1: pr f "24,78,C 1": bell : goto L730
L780: if cmdkey=5 then goto WHATNEXT
	if cmdkey<>6 then goto L850
	ce=curfld
	on ce-2 gosub SRCH,CATEGORY_SEARCH,SRCH3 none L620
	if ce=3 then pr #win,fields io1$(3): jn$
	goto L620
 
L850: if cmdkey=4 and c1=2 then goto DEL
	if ltrm$(rtrm$(rn$))="-1" then ce=1 : goto ERR1
	if dat<10100 or dat>123199 then ce=2: goto ERR1
	if rtrm$(jn$)="" and ji2(1)=0 then goto L1000
	read #11,using L900,key=lpad$(rtrm$(jn$),6): n$ nokey L920
L900: form pos 7,c 40
	goto L930
L920: ce=3: goto ERR1
L930: cn$=lpad$(rtrm$(jn$),6)&lpad$(str$(ji2(1)),5)
	read #12,using L950,key>=cn$: k$ nokey L990
L950: form pos 1,c 11
	if cn$(1:6)<>k$(1:6) then goto L990
	if cn$(7:11)<>k$(7:11) then ce=4: goto L1790
	goto L1000
L990: ce=3: goto L1790
L1000: if ji2(2)=0 then goto L1030
	read #13,using L1020,key=lpad$(str$(ji2(2)),3): ds$ nokey L1040
L1020: form pos 4,c 30
L1030: goto L1050
L1040: ce=5: goto L1790
L1050: if ji2(3)=0 then ce=6: goto L1790
	pt=ptp*ji2(3)
	if c1=2 then goto L1120
L1080: rw=lrec(3)+1
	write #3,using L1100,rec=rw: rn$,dat,jn$,mat ji2,d$ duprec L1080
L1100: form pos 1,c 12,pd 4,c 6,2*pd 3,pd 5.2,c 30
	goto INPUTSCREEN2
L1120: rewrite #3,using L1100,rec=rr: rn$,dat,jn$,mat ji2,d$
	goto L1210
 
DEL: !
	rewrite #3,using L1100,rec=rr: "-1",0,"",0,0,0,""
	goto L1210
 
ADDITIONALENTRIES: !
	shoption=2
L1210: pr newpage
	fnopenwin(win=103,10,20,14,59,cap$)
	pr #win,fields "4,2,C 28,N": "Reference Number to correct:"
	pr f "15,35,C 09,B,5": "Done (F5)"
L1250: input #win,fields "4,31,N 5,UT,N": rr conv L1250
	if cmdkey=5 then goto WHATNEXT
	if rr<1 or rr>rw then goto L1250
	close #win: ioerr L1290
L1290: read #3,using L1100,rec=rr: rn$,dat,jn$,mat ji2,d$
	goto INPUTSCREEN2
 
WHATNEXT: !
	pr newpage
	fnopenwin(win=102,09,20,16,59,cap$)
	wrd2$(1)="1. pr Input Proof List"
	wrd2$(2)="2. Corrections"
	wrd2$(3)="3. Additional Entries"
	wrd2$(4)="4. Post to Job Cost File"
	for j=1 to udim(wrd2$)
		io2$(j)=str$(j+3)&",2,C 38,N"
	next j
	pr f "17,27,c 25,B,5": "Exit without posting (F5)"
L1430: rinput #win,select mat io2$,attr "H": mat wrd2$
	c1=curfld
	if cmdkey=5 then goto Xit
	on c1 goto PRINTPROOFLIST,ADDITIONALENTRIES,INPUTSCREEN1,POSTTOJOB none L1430
 
PRINTPROOFLIST: !
	pr newpage
	fnwait(message$,1)
	fnopenprn
	pr #255,using L1530: "Job Cost Input Proof List"
L1530: form pos 1,cc 80,skip 1
	pr #255,using L1530: "Date: "&date$&"     Time: "&time$
	pr #255: "Ref #   Reference #    Date    Job #     Category   Sub-Category     Amount   Description"
	for j=1 to rw
		read #3,using L1100,rec=j: rn$,dat,jn$,mat ji2,d$
		pr #255,using L1590: j,rn$,dat,jn$,mat ji2,d$
L1590: form pos 1,n 5,x 3,c 12,n 8,x 3,c 8,n 7,n 14,n 15.2,x 3,c 30,skip 1
		ta=ta+ji2(3)
	next j
	pr #255,using L1630: " ____________",ta
L1630: form pos 63,c 13,skip 1,pos 63,n 13.2
	ta=0
	fncloseprn
	goto WHATNEXT
 
POSTTOJOB: !
	close #3:
	close #11:
	close #12:
	close #13:
	chain "S:\acsPR\JCMRGC"
 
Xit: fnXit
 
L1770: if ce>0 then io1$(ce)(ce1:ce2)="U"
	ce=cnt+1
L1790: pr f "24,1,C 7,N": bell
	io1$(ce)=rtrm$(io1$(ce))
	ce1=pos(uprc$(io1$(ce)),"U",1)
	ce2=ce1+1
	io1$(ce)(ce1:ce1)="UC"
	goto L580
 
SRCH: bk=0 : hce=ce
	close #103: ioerr L1880
L1880: open #103: "SROW=1,SCOL=1,EROW=24,ECOL=80",display,output
	pr #103: newpage
	wrds$(1)="1. JOB # SEARCH" : skl(1)=6
	wrds$(2)="2. JOB NAME SEARCH" : skl(2)=25
	seq=11 : goto L2020
	for j=1 to udim(ios$): ios$(j)=str$(j+12)&",25,C 30,N": next j
L1940: close #101: ioerr L1950
L1950: open #101: "SROW=12,SCOL=24,EROW="&str$(udim(ios$)+13)&",ECOL=55,BORDER=SR,CAPTION=SELECT TYPE OF SEARCH",display,outIn
	pr #101: newpage
	pr f mat ios$: mat wrds$
	pr f str$(udim(ios$)+14)&",35,C 09,B,5": "Stop (F5)"
	input select mat ios$,attr "H": mat wrds$
	seq=curfld
	if cmdkey=5 then goto SRCHEND
L2020: pr newpage
	win2=101
	fnwin3b(win2,wrds$(seq-10),5,41+skl(seq-10),1,1)
	prtall=0
	pr #win2,fields "4,2,C 38,N": "Beginning Search Data (blank for all):"
L2070: input #win2,fields "4,41,C "&str$(skl(seq-10))&",UT,N": nam$
	if cmdkey=5 then goto L1940
	if seq=11 then nam$=lpad$(rtrm$(nam$),skl(1))
	restore #seq,search>=nam$: nokey L2070
	close #win2: ioerr L2120
L2120: pr newpage
	pr f "1,2,C 6,R,N": "Job #:"
	pr f "1,9,C 40,R,N": "Job Name"
	pr f "1,50,C 8,R,N": "Est.Date"
	pr f "1,59,C 10,R,N": "Est.Date"
	pr f "1,59,C 14,R,N": "   Ct.Amount"
	cde=0
	for j=1 to 20
		read #seq,using L2210: jn$,n$,mat a$,mat b eof SREND
L2210: form pos 1,c 6,c 40,3*c 30,n 6,2*pd 7.2,n 2
		cde=1
		pr f str$(j+1)&",2,C 6,UT,N": jn$
		pr f str$(j+1)&",9,C 40,UT,N": n$
		pr f str$(j+1)&",50,PIC(ZZ/ZZ/ZZ),UT,N": b(1)
		pr f str$(j+1)&",59,N 14.2,UT,N": b(2)
		if j>1 then goto L2310
		bk=bk+1
		if bk>20 then bk=1
		bk$(bk)=bl$(2)(1:28)
L2310: next j
SREND: if j>1 then j=j-1
	mat in2$(j)
	pr f "24,02,C 16,B,1": "Continue (Enter)"
	pr f "24,19,C 09,B,2": "Back (F2)"
	pr f "24,29,C 09,B,5": "Stop (F5)"
	pr f "24,39,C 25,R,N": "or Select Account Number:"
	input fields "24,65,C 6,UT,N": k$
	alp=0
	if cmdkey=5 then goto SRCHEND
	if rtrm$(k$)><"" then jn$=k$ : _
		pr #win,fields io1$(3): k$ : goto SRCHEND
	if cmdkey><2 then goto L2470
	bk=bk-1
	if bk<1 then goto L2500
	restore #seq,key>=bk$(bk): nokey L2500
	bk=bk-1
L2470: selclp=1
	goto L2120
 
L2500: selclp=0
	goto L2020
 
SRCHEND: close #103: ioerr L2540
L2540: return
 
CATEGORY_SEARCH: !
	bk=0
	close #103: ioerr L2590
L2590: open #103: "SROW=2,SCOL=47,EROW=23,ECOL=79,BORDER=DR",display,output
	pr #103: newpage
	jn$=lpad$(rtrm$(jn$),6)
	read #11,using L2630,key=jn$: jn$,n$,mat a$,mat b nokey L2750
L2630: form pos 1,c 6,c 40,3*c 30,n 6,2*pd 7.2,n 2
	restore #12,search>=jn$: nokey L2750
L2650: ln=0
	pr #103: newpage
	pr f "2,47,C 33,R,N": " "&n$(1:31)
L2680: read #12,using L2700: jn2$,cn$,cnt$ eof L2750
	if jn2$<>jn$ then goto L2750
L2700: form pos 1,c 6,c 5,c 25
	ln=ln+1
	pr f str$(ln+2)&",48,C 5,UT,N": cn$
	pr f str$(ln+2)&",54,C 25,UT,N": cnt$
	if ln<20 then goto L2680
L2750: pr f "23,47,Cc 33,B,1": "Next (Enter)"
	pr f "24,48,C 25,R,N": "or enter Category Number:"
	pr f "24,74,C 5,UT,N": ""
	input fields "24,74,C 5,UT,N": k$
	ji2(1)=val(k$) conv L2810
	if rtrm$(k$)<>"" then pr #win,fields io1$(4): ji2(1)
L2810: if rtrm$(k$)><"" then goto SRCHEND
	if ln<20 then goto SRCHEND
	goto L2650
 
SRCH3: bk=0
	close #103: ioerr L2870
L2870: open #103: "SROW=2,SCOL=47,EROW=23,ECOL=79,BORDER=DR",display,output
L2880: pr #103: newpage
	pr f "2,47,C 33,R,N": " "&n$(1:31)
	restore #13:
	ln=0
L2920: read #13,using L2930: sc$,cnt$ eof L2980
L2930: form pos 1,c 3,c 25
	ln=ln+1
	pr f str$(ln+2)&",49,C 3,UT,N": sc$
	pr f str$(ln+2)&",54,C 25,UT,N": cnt$
	if ln<20 then goto L2920
L2980: pr f "23,47,C 33,R,N": " PRESS ENTER TO CONTINUE"
	pr f "24,47,C 33,R,N": " OR ENTER SUB-CAT #:"
	input fields "24,69,C 2,RE,N": k$
	ji2(2)=val(k$) conv L3030
	if rtrm$(k$)<>"" then pr #win,fields io1$(5): ji2(2)
L3030: if rtrm$(k$)><"" then goto SRCHEND
	if ln<20 then goto SRCHEND
	goto L2880
 
include: ertn
 
