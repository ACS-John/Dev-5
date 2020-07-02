!  Replace S:\acsPR\TimeCard
! Extend Time Card
 
	dim sc1$(7),fl1$(7),io1$(101),inp(20,5),hrs(20),em$*30
	dim p$(20)*50,wrd2$(2)*35,message$*40
 
	autoLibrary
	on error goto Ertn
 
	fnTop("S:\acsPR\TimeCard","Extend Time Card")
	fnconsole(1)
!
 
	sc1$(1)="Employee #:": fl1$(1)="1,2,C 20,N"
	sc1$(2)=" Date": fl1$(2)="3,15,C 6,R,N"
	sc1$(3)="Time  In": fl1$(3)="2,28,C 8,R,N"
	sc1$(4)="Time Out": fl1$(4)="2,42,C 8,R,N"
	sc1$(5)="Hrs  Min": fl1$(5)="3,28,C 8,R,N"
	sc1$(6)="Hrs  Min": fl1$(6)="3,42,C 8,R,N"
	sc1$(7)=" Hours": fl1$(7)="3,55,C 6,R,N"
	io1$(1)="1,21,N 9,UET,N"
	for j=2 to 21
		io1$(j*5-8)=str$(j+2)&",15,N 6,UT,N"
		io1$(j*5-7)=str$(j+2)&",28,N 2,UT,N"
		io1$(j*5-6)=str$(j+2)&",34,N 2,UT,N"
		io1$(j*5-5)=str$(j+2)&",42,N 2,UT,N"
		io1$(j*5-4)=str$(j+2)&",48,N 2,UET,N"
	next j
	open #1: "Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-no.h[cno],Shr",internal,input,keyed
	pr newpage
	fnopenwin(win=101,10,22,15,58,env$('program_caption'))
	io2$(1)="4,2,C 35,N"
	io2$(2)="5,2,C 35,N"
	wrd2$(1)="Use Hours and Minutes"
	wrd2$(2)="Use Hours and Hundredths of an Hour"
	pr f "16,35,C 09,B,5": "Exit (F5)"
	rinput #win,select mat io2$,attr "H": mat wrd2$
	if cmdkey=5 then goto Xit
	ti1=curfld
	if ti1=2 then sc1$(5)=sc1$(6)="Hrs  Hnd"
L400: pr newpage
	pr f mat fl1$: mat sc1$
	pr f "24,11,C 09,B,1": "Next (F1)"
	pr f "24,21,C 10,B,2": "Print (F2)"
	pr f "24,32,C 26,B,3": "Skip to Next Employee (F3)"
	pr f "24,59,C 09,B,5": "Stop (F5)"
L460: input fields mat io1$: eno,mat inp conv CONV1
	if ce>0 then io1$(ce)(ce1:ce2)="U": ce=0
	if cmdkey>0 then goto L620 else ce=curfld
	if ce>1 then goto L540
	read #1,using L510,key=lpad$(str$(eno),8): em$ nokey ERR1
L510: form pos 9,c 30
	pr f "1,35,C 30,R,N": em$
	goto L550
L540: if int((ce-1)/5)=(ce-1)/5 then goto L650
L550: ce=ce+1
	if ce>udim(io1$) then ce=1
L570: io1$(ce)=rtrm$(uprc$(io1$(ce))) : ce1=pos(io1$(ce),"U",1)
	ce2=ce1+1 : io1$(ce)(ce1:ce1)="UC" : goto L460
CONV1: if ce>0 then io1$(ce)(ce1:ce2)="U"
	ce=cnt+1
ERR1: pr f "24,78,C 1": bell : goto L570
L620: if cmdkey=2 then goto L860
	if cmdkey=3 then goto L1080
	if cmdkey=5 or eno=0 then goto END1
L650: ln=currow
	cn=curcol
	if ln=1 then goto L460
	if ti1=1 then hrs(ln-3)=round((inp(ln-3,4)+(inp(ln-3,5)/60))-(inp(ln-3,2)+(inp(ln-3,3)/60)),2) else hrs(ln-3)=inp(ln-3,4)+(inp(ln-3,5)*.01)-inp(ln-3,2)-(inp(ln-3,3)*.01)
	if hrs(ln-3)<0 then hrs(ln-3)=hrs(ln-3)+12
	if inp(ln-3,2)<1 or inp(ln-3,2)>12 then ce=ln*5-17: goto ERR1
	if inp(ln-3,3)<0 or inp(ln-3,3)>59 then ce=ln*5-16: goto ERR1
	if inp(ln-3,4)<1 or inp(ln-3,4)>12 then ce=ln*5-15: goto ERR1
	if inp(ln-3,5)<0 or inp(ln-3,5)>59 then ce=ln*5-14: goto ERR1
	if inp(ln-3,1)<10101 or inp(ln-3,1)>123199 then ce=ln*5-18: goto ERR1
	if hrs(ln-3)>12 then ce=ln*5-15: goto ERR1
	pr f str$(ln)&",55,N 6.2,H,N": hrs(ln-3)
	ce=ln*5-12
	ce1=pos(io1$(ce),"U",1)
	ce2=ce1+1
	io1$(ce)(ce1:ce1)="CU"
	if inp(ln-3,4)>2 and inp(ln-3,4)<8 then dt=inp(ln-3,1)+100 else dt=inp(ln-3,1)
	if ln<23 then pr f str$(ln+1)&",15,N 6,UT,N": dt
	pr f "2,55,N 6.2,R,N": sum(hrs)
	goto L460
 
L860: fnopenprn
	fnwait(message$,0)
	pr #255,using L890: mat sc1$
L890: form pos 1,4*c 14,skip 1,pos 29,3*c 14
	t1=t2=0
	for j=1 to 20
		if j>1 then eno=0
		if hrs(j)=0 then goto L1030
		pr #255,using L950: eno,inp(j,1),inp(j,2),inp(j,3),inp(j,4),inp(j,5),hrs(j)
L950: form pos 1,pic(zzzzzz),x 7,pic(zz/zz/zz),x 5,2*n 5,x 4,2*n 5,n 12.2,skip 1
		t1=t1+hrs(j)
		if j=20 then goto L990
		if inp(j,1)=inp(j+1,1) then goto L1030
L990: pr #255,using L1000: "__________",t1
L1000: form pos 53,c 10,skip 1,pos 53,n 10.2,skip 2
		t2=t2+t1
		t1=0
L1030: next j
	pr #255,using L1050: "__________",t2,"=========="
L1050: form pos 53,c 10,skip 1,pos 53,n 10.2,skip 1,pos 53,c 10,skip 2
	pr #255: newpage
	fncloseprn
L1080: mat hrs=(0)
	mat inp=(0)
	goto L400
 
END1: fncloseprn
Xit: fnXit
include: Ertn
