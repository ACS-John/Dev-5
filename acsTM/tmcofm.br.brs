 
	on error goto L750
	autoLibrary
	fnTop(program$,cap$="Company Information")
	fncno(cno,cnam$)
	dim co$(4)*40,co(6),ag(4),dat$*20,gln(4,3)
	dim fl1$(20),sc1$(19)*256,io1$(27),hd1$*50,cap$*128
 
	data COMPANY NAME
	data CO ADDRESS 1
	data CO ADDRESS 2
	data CITY STATE ZIP
	data GL INSTALLED                                             (1=YES)
	data FUND NUMBER USED                                         (1=YES)
	data SUB ACCOUNT USED                                         (1=YES)
	data pr DETAILS ON STATEMENTS                              (1=YES)
	data FINANCE CHARGE RATE                        (ENTER 10% AS 10.000)
	data # OF DAYS BEFORE FIN CHG APPLICABLE
	data AGING PERIOD 1
	data AGING PERIOD 2
	data AGING PERIOD 3
	data AGING PERIOD 4
	data GL # CASH
	data GL # AR
	data GL # FINANCE CHARGE
	data GL # STANDARD CHARGE
	data REPORT HEADING DATE
	read mat sc1$
	for j=1 to 19
		fl1$(j)=str$(j+2)&",2,C 66"
		if j<5 then io1$(j)=str$(j+2)&",20,C 40,U,N"
		if j>4 and j<9 then io1$(j)=str$(j+2)&",42,N 1,U,N"
		if j>9 and j<15 then io1$(j)=str$(j+2)&",40,N 3,U,N"
	next j
	fl1$(20)="1,5,C 60,H,N"
	io1$(9)="11,37,N 6.3,U,N"
	for j=1 to 4
		io1$(j*3+12)=str$(j+16)&",30,N 3,U,N"
		io1$(j*3+13)=str$(j+16)&",37,N 6,U,N"
		io1$(j*3+14)=str$(j+16)&",47,N 3,U,N"
	next j
	io1$(27)="21,30,C 20,U,N"
	hd1$="TIME MANAGEMENT CHANGE COMPANY INFORMATION"
	open #1: "Name=S:\Core\Data\acsllc\Company.h[cno]",internal,outIn,relative ioerr L480
	goto L500
	close #1,free:
L480: open #1: "Name=S:\Core\Data\acsllc\Company.h[cno],SIZE=0,RecL=245",internal,outIn,relative ioerr L750
	write #1,using L510,rec=1: mat co$,mat co,mat ag,mat gln,dat$
L500: read #1,using L510,rec=1: mat co$,mat co,mat ag,mat gln,dat$ ioerr L750
L510: form pos 1,4*c 40,4*n 1,pd 3.3,5*pd 2,n 3,n 6,n 3,n 3,n 6,n 3,n 3,n 6,n 3,n 3,n 6,n 3,c 20
	pr newpage
	pr f mat fl1$: mat sc1$,hd1$
	pr f mat io1$: mat co$,mat co,mat ag,mat gln,dat$
	pr f "23,34,c 20": "F1 Continue"
L550: input fields mat io1$: mat co$,mat co,mat ag,mat gln,dat$ conv L670
	if ce>0 then io1$(ce)(ce1:ce2)="U": ce=0
	if rtrm$(co$(1))="" then ce=1: goto L690
	for j=1 to 4
		if co(j)<0 or co(j)>1 then ce=j+4: goto L690
		if j=1 then goto L620
		if ag(j-1)>ag(j) then ce=j+10: goto L690
L620: next j
	if co(5)<0 or co(5)>99 then ce=9: goto L690
	rewrite #1,using L510,rec=1: mat co$,mat co,mat ag,mat gln,dat$
	close #1:
	goto Xit
L670: if ce>0 then io1$(ce)(ce1:ce2)="U"
	ce=cnt+1
L690: pr f "24,78,C 1": bell
	io1$(ce)=rtrm$(io1$(ce))
	ce1=pos(uprc$(io1$(ce)),"U",1)
	ce2=ce1+1
	io1$(ce)(ce1:ce1)="CR"
	goto L550
L750: if err=61 then pr f "23,3,C 75,N": "THIS PROGRAM IS TRYING TO ACCESS A RECORD THAT IS IN USE!" else goto L770
	goto L810
L770: pr newpage
	if err=4148 then pr f "23,3,C 78,N": "THIS PROGRAM IS TRYING TO ACCESS A FILE THAT IS IN USE AND CANNOT BE SHARED!" else goto L800
	goto L810
L800: pr f "23,3,C 75,N": "YOU HAVE A WORKSTATION BASIC ERROR # "&str$(err)&" AT LINE # "&str$(line)&"."
L810: pr f "24,3,C 70,N": "PRESS ENTER TO RETRY; ELSE ENTER  Q  TO QUIT"
	input fields "24,60,C 1,N": quitcode$
	if rtrm$(uprc$(quitcode$))="Q" then goto Xit
	pr f "23,3,C 78,N": ""
	pr f "24,3,C 78,N": ""
	retry
Xit: fnXit
