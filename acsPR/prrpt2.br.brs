! Replace S:\acsPR\prRpt2
! Payroll Report File - Add Records
 
	autoLibrary
	fnTop("S:\acsPR\prRpt2",cap$="Design Reports")
	on error goto Ertn
 
	dim rt$*78,ch$(2)*132,psc(100),inp(20),pp(20),ti(20)
	dim io1$(9),fb$(20),io2$(60),rptemp(20),tempch$(4)*256
	dim temp(20,3),cnam$*40,cap$*128,response$(5)*1,cap$*128,msgline$(2)*60
 
	fncno(cno,cnam$)
	for z=1 to 20
		z$=str$(z+1)
		c$=str$(z+1)
		fb$(z)=z$&",40,N 13.3,UT,N"
		io2$((z-1)*3+1)=c$&",15,Nz 3,UT,N"
		io2$((z-1)*3+2)=c$&",38,Nz 3,UT,N"
		io2$((z-1)*3+3)=c$&",62,N 1,UT,N"
	next z
	open #1: "Name=[Q]\PRmstr\PRReport.h[cno],KFName=[Q]\PRmstr\prrptidx.h[cno],Shr",internal,outIn,keyed
 
SCR1: pr newpage
	close #101: ioerr L250
L250: open #101: "SRow=4,SCol=6,ERow=22,ECol=74,Border=Dr,Caption=<"&cap$,display,outIn
	pr #101,fields "02,2,Cr 14,N": "Report Number:"
	pr #101,fields "03,2,Cr 14,N": "Report Title:"
	pr #101,fields "05,2,Cc 66,R,N": "Column Headings"
	pr #101,fields "06,2,C 66,N": "    5   10   15   20   25   30   35   40   45   50   55   60    66"
	pr #101,fields "08,2,C 66,N": "  70   75   80   85   90   95  100  105  110  115  120  125    132"
	pr #101,fields "10,2,C 66,N": "    5   10   15   20   25   30   35   40   45   50   55   60    66"
	pr #101,fields "12,2,C 66,N": "  70   75   80   85   90   95  100  105  110  115  120  125    132"
	pr #101,fields "16,2,Cr 48,N": "Item Number for pr Selection (blank for all):"
	pr #101,fields "17,2,Cr 50,N": "Summarize Departmental Records (Y/N): N"
	pr #101,fields "18,2,Cr 50,N": "Use Condensed pr (Y/N): N"
	io1$(1)="02,17,Nz 2,UT,N"
	io1$(2)="03,17,C 40,UT,N" ! used to be 78... but john decided no one ever need one that long!
	io1$(3)="07,2,C 66,UT,N"
	io1$(4)="09,2,C 66,UT,N"
	io1$(5)="11,2,C 66,UT,N"
	io1$(6)="13,2,C 66,UT,N"
	io1$(7)="16,51,N 3,UT,N"
	io1$(8)="17,51,Cu 1,UT,N"
	io1$(9)="18,51,Cu 1,UT,N"
	pr f "23,30,C 09,B,1": "Next (F1)"
	pr f "23,41,C 09,B,5": "Exit (F5)"
L470: input #101,fields mat io1$: rn,rt$,mat tempch$,ips,sd$,cp$ conv L470
	if ce>0 then io1$(ce)(ce1:ce2)="U": ce=0
	if cmdkey>0 then goto L560 else ce=curfld
L500: ce=ce+1: if ce>udim(io1$) then ce=1
L510: io1$(ce)=rtrm$(io1$(ce)) : ce1=pos(io1$(ce),"U",1) : if ce1=0 then goto L500
	ce2=ce1+1 : io1$(ce)(ce1:ce1)="UC" : goto L470
CONV1: if ce>0 then io1$(ce)(ce1:ce2)="U"
	ce=cnt+1
ERR1: pr f "24,78,C 1": bell : goto L510
L560: if cmdkey=5 then goto DONE
	read #1,using L580,key=lpad$(str$(rn),2),release: rn nokey L640
L580: form pos 1,n 2
! pr f "2,40,C 35,N": "This Report Number Already Exists"
	msgline$(1)="Report Number "&str$(rn)&" already exists"
	msgline$(2)="Please use a different Report Number."
	fnoldmsgbox(mat resonse$,cap$,mat msgline$,1)
	ce=1 : goto ERR1
L640: if ips<0 or ips>126 then goto L470
	if ips>1 and ips<6 then goto L470
	if sd<0 or sd>1 then goto L470
	if sd$="Y" then sd=1 else sd=0
	if sd$<>"Y" and sd$<>"N" then ce=8 : goto ERR1
	if cp$="Y" then cp=1 else cp=0
	if cp$<>"Y" and cp$<>"N" then ce=9 : goto ERR1
	if cp<0 or cp>1 then goto L470
	ch$(1)=tempch$(1)&tempch$(2)
	ch$(2)=tempch$(3)&tempch$(4)
	if rn=0 then ce=1 : goto ERR1
	if ips=0 then goto L930
	pr newpage
	for w=1 to 5
		for j=1 to 20
			pr f str$(j+1)&",5,C 30,N": "PRINT SELECTION CRITERIA"
		next j
L810: input fields mat fb$: mat rptemp conv L810
		on w goto L830,L850,L850,L850,L850
L830: k=0
		goto L860
L850: k=k+20
L860: for q=1 to 20
			if rptemp(q)=0 then goto L930
			psc(q+k)=rptemp(q)
		next q
		mat rptemp=(0)
		pr newpage
	next w
L930: pr newpage
	close #102: ioerr L950
L950: open #102: "SRow=2,SCol=9,ERow=23,ECol=72,Border=Sr,Caption=<"&cap$,display,outIn
	pr #102: newpage
	for j=1 to 20
		pr #102,fields str$(j+1)&",2,C 12,N": "Item Number:"
		pr #102,fields str$(j+1)&",22,C 15,N": "Print Position:"
		pr #102,fields str$(j+1)&",45,C 16,N": "Total this Item:"
	next j
	pr f "24,30,C 09,B,1": "Save (F1)"
	pr f "24,40,C 11,B,5": "Cancel (F5)"
L1040: input #102,fields mat io2$: mat temp conv CONV2
	if ce>0 then io2$(ce)(ce1:ce2)="U": ce=0
	if cmdkey>0 then goto L1130 else ce=curfld
L1070: ce=ce+1: if ce>udim(io2$) then ce=1
L1080: io2$(ce)=rtrm$(io2$(ce)) : ce1=pos(io2$(ce),"U",1) : if ce1=0 then goto L1070
	ce2=ce1+1 : io2$(ce)(ce1:ce1)="UC" : goto L1040
CONV2: if ce>0 then io2$(ce)(ce1:ce2)="U"
	ce=cnt+1
ERR2: pr f "24,78,C 1": bell : goto L1080
L1130: !
	if cmdkey=5 then goto SCR1
	for j=1 to 20
		if temp(j,1)>126 then ce=j*3+1 : goto CONV2 ! Goto 1040 ! j*3+1
		if temp(j,2)>132 then ce=j*3+2 : goto CONV2 ! Goto 1040 ! j*3+2
		if temp(j,3)<0 or temp(j,3)>1 then ce=j*3+3 : goto CONV2 ! Goto 1040
		inp(j)=temp(j,1)
		pp(j)=temp(j,2)
		ti(j)=temp(j,3)
	next j
	write #1,using L1240: rn,rt$,mat ch$,ips,sd,cp,mat psc,mat inp,mat pp,mat ti
L1240: form pos 1,n 2,c 78,2*c 132,n 3,2*n 1,100*pd 6.3,40*pd 2,20*n 1
	goto SCR1
DONE: pr newpage
	close #1:
	open #99: "Name=PROC."&wsid$&",SIZE=0,Replace",display,output
L1290: form pos 1,c 70
	pr #99,using L1290: "CLEAR"
	pr #99,using L1290: "PROCERR RETURN"
	pr #99,using L1290: "Index [Q]\PRmstr\PRReport.h[cno]"&' '&"[Q]\PRmstr\prrptidx.h[cno] 1 2 Replace DupKeys"
	pr #99,using L1290: "PROC S:\acsPR\PRMENU"
	close #99:
	chain "proc=PROC."&wsid$&""
 
Xit: fnXit
 
include: ertn
 
