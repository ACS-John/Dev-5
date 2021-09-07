
on error goto Ertn
autoLibrary
fnTop(program$,"Enter Time")
fnconsole(1)

dim xinp(7)
dim iv$*12
dim a1$*30
dim app(20)
dim scr2$(7)*36
dim e$*9
dim e1$*25
dim he$*9
dim r(11)
dim des$*30
dim de$*30
dim a$(5)*30
dim ph$*12
dim ss$*11
dim dd(10)
dim sc(10)

dim ph2$*12
dim ss2$*11
dim r(5),arta(2),cm$*70,app(40)
dim bk$(20)*30,a1$*30
ar(5)=1 ! set any new customers up as a balance forward
ntab=60-int(len(ltrm$(rtrm$(env$('cnam'))))/2)
dim fl1$(12)
fl1$(11)="2,10,C 60,H,N"
fl1$(12)="3,6,C 60,H,N"

dim fl2$(10)
fl2$(5)="2,10,C 60,H,N"
fl2$(6)="14,10,C 60,H,N"
fl2$(7)="15,10,C 60,H,N"

dim io1$(10)
io1$(1)="5,25,N 5,EUT,N"
io1$(2)="6,25,N 9,EuT,N"
io1$(3)="7,25,N 7.2,XeuT,N"
io1$(4)="8,25,N 7.2,peu,N"
io1$(5)="9,25,N 10.2,EuT,N"
io1$(6)="10,25,N 6,EuT,N"
io1$(7)="11,25,N 2,EuT,N"
io1$(8)="12,25,N 2,EuT,N"
io1$(9)="13,25,N 4,EuT,N"
io1$(10)="14,25,C 30,EuT,N"
for j=1 to 10
	fl1$(j)=str$(j+4)&",10,C 20"
	if j<8 then fl2$(j)=str$(j+4)&",2,C 40"
next j
fl2$(8)="5,25,N 10.2,ut,N"
fl2$(9)=fl1$(11)
fl2$(10)="15,2,C 60"
dim scr1$(10)
data "Client"
data "Employee"
data "Hours"
data "Rate"
data "Amount"
data "Date"
data "Category"
data "Month Code"
data "Service Code"
data "Description"
read mat scr1$
data "total hours entered is "
data "enter 1 to input time sheets"
data "      2 to input other charges"
data "      3 to input adjustments"
data "      4 for a listing of entriES"
data "      5 to make corrections"
data "      6 to merge transactions"
read mat scr2$
open #1: "Name=S:\Core\Data\acsllc\Client.h[cno],KFName=S:\Core\Data\acsllc\Client-Idx.h[cno],Shr",internal,outIn,keyed
open #32: "Name=S:\Core\Data\acsllc\Client.h[cno],KFName=S:\Core\Data\acsllc\CLIndx2.h[cno],Shr",internal,outIn,keyed
open #2: "Name=S:\Core\Data\acsllc\TMWK[acsUserId].h[cno],RecL=86,REPLACE",i,outi,r
open #7: "Name=S:\Core\Data\acsllc\SCMSTR.h[cno],KFName=S:\Core\Data\acsllc\SCIndex.h[cno],Shr",i,i,k
open #8: "Name=S:\Core\Data\acsllc\EMmstr.h[cno],KFName=S:\Core\Data\acsllc\EMIndex.h[cno],Shr",internal,outIn,keyed

L600: !
	pr newpage
	dim scrid$(2)*60
	scrid$(1)="TIME MANAGEMENT INPUT SUB-MENU"
	scrid$(2)="ENTER THE SELECTION NUMBER OF YOUR CHOICE"
	pr f mat fl2$: mat scr2$,thrs,mat scrid$
	pr f "22,2,Cc 60,B,1": "Continue (F1)"
	pr f "24,2,Cc 60,B,5": "Abort and drop all inputed transactions (F5)"
	L660: !
	input fields "15,45,N 1,EU,N": b7 conv L660
	if cmdkey=5 then goto Xit
	if cmdkey>1 then goto L660
	chg=0
	mat xinp=(0)
on b7 goto L720,L720,L720,L2230,L2110,L2620 none L660

L720: !
	scrid$(1)="TIME MANAGEMENT "&scr2$(b7+1)(12:36)
	scrid$(2)="Enter CLIENT # as -1 to return to sub menu."
L740: !
	xinp(1)=0
	xinp(3)=0
	xinp(4)=0
	xinp(7)=0
	xinp(5)=0
	expcode=0
	des$=""
	sc=0
	b8=0
L830: !
	pr newpage
	L840: !
	pr f mat fl1$: mat scr1$,mat scrid$
	if chg=2 and sum(xinp)=0 then xinp(1)=-1
	io1$(2)="6,25,N 9,UET,N"
	io1$(3)="7,25,N 7.2,XeuT,N"
	io1$(5)="9,25,N 10.2,uT,N"
	e1$=a1$=" "
	pr f mat io1$: mat xinp,b8,sc,des$
	pr f "22,13,C 45,R,N": "F1 Continue; F4 Search; F5 Complete"
	if chg=2 then goto L1280
L930: xinp(4)=75
	sno=1: rinput fields mat io1$,attr "R": mat xinp,b8,sc,des$ conv CONV1
	if cmdkey=4 then goto TMSRCH
	if cmdkey=5 then xinp(1)=-1
	if ce>0 then io1$(ce)(ce1:ce2)="U": ce=0
	if cmdkey>0 or curfld=2 then goto L1020 else ce=curfld
L960: ce=ce+1: if ce>udim(io1$) then ce=1
L970: io1$(ce)=rtrm$(uprc$(io1$(ce))) : ce1=pos(io1$(ce),"U",1) : if ce1=0 then goto L960
	ce2=ce1+1 : io1$(ce)(ce1:ce1)="UC" : goto L930
CONV1: if ce>0 then io1$(ce)(ce1:ce2)="U"
	ce=cnt+1
ERR1: pr f "24,78,C 1": bell : goto L970
L1020: if cmdkey=6 then hce=curfld : goto SRCH1
	if xinp(1)<0 then goto L1390
	if xinp(1)<=0 then goto L1180
	k$=rpad$(str$(xinp(1)),5)
	if cmdkey=2 and err=4272 then 
		goto L1070 
	else 
		goto L1140
	end if
	
	L1070: !
	pr f "4,45,c 30,n": "Enter customer name:"
	input fields "5,50,c 30,ue,n": a$(1)
	dim ca(10)
	dim xma(40)
	write #1,using L1100: k$,mat a$,ph$,ss$,pno,mye,mat dd,mat sc,mat ca,ph2$,ss2$,mat ar,mat arta,cm$,mat app,mat xma
	L1100: form pos 1,c 5,5*c 30,c 12,c 11,n 9,n 2,10*pd 3,10*n 1,10*pd 3,c 12,c 11,2*pd 5.2,pd 4.3,2*n 1,2*pd 3,c 70,20*n 1,x 60,20*n 1,pos 474,20*pd 3.2,x 20,20*pd 3.2
	pr f "4,45,c 30,n": " "
	pr f "5,50,c 30,n": " "
	pr f "23,45,C 35,N": " "
	
	L1140: !
	read #1,using L1560,key=k$: a1$ nokey L1160
goto L1180
L1160: !
	ce=1
goto L1320
L1180: !
	if xinp(2)=0 then goto L1240
	e$=lpad$(str$(xinp(2)),9)
	read #8,using L1670,key=e$,release: e1$,mat r nokey L1220
goto L1240
L1220: !
	ce=2
goto L1320
L1240: !
	pr f "5,40,c 30,n": a1$
	pr f "6,40,c 25,n": e1$
	io1$(2)="6,25,N 9,u,N"
	ce=3
goto L1320
L1280: !
	xinp(4)=75 ! current rate
	L1282: !
	sno=2
	rinput fields mat io1$,attr "R": mat xinp,b8,sc,des$ conv CONV2
	if ce>0 then io1$(ce)(ce1:ce2)="U": ce=0
	if cmdkey>0 then goto L1370 else ce=curfld
	if ce=3 then xinp(5)=round(xinp(3)*xinp(4),2)
	if ce=7 and xinp(7)=6 then sc=601
	if ce=7 and xinp(7)=2 then sc=201
	L1310: !
	ce+=1 : if ce>udim(mat io1$) then ce=1
	L1320: !
	io1$(ce)=rtrm$(uprc$(io1$(ce)))
	ce1=pos(io1$(ce),"U",1)
	if ce1=0 then goto L1310
	ce2=ce1+1
	io1$(ce)(ce1:ce1)="UC"
	goto L1282
	CONV2: !
	if ce>0 then io1$(ce)(ce1:ce2)="U"
	ce=cnt+1
	ERR2: !
	pr f "24,78,C 1": bell : goto L1320
	L1370: !
	if cmdkey=6 then hce=curfld : goto SRCH1
	if env$('client')="ACS" and (xinp(7)=6 or xinp(7)=2) and b8=0 then ce=8: goto ERR2
	L1390: !
	if xinp(1)=-1 and chg><2 then goto L600
	if xinp(1)=-1 then mat xinp=(0) else goto L1480
	b6=0
	b7=0
	b8=0
	sc=0
	iv$=" "
	nta=0
goto L2090
L1480: !
	if rtrm$(des$)<>"" then goto L1490
L1490: !
	if b8<0 or b8>29 then ce=8: goto ERR2
	if xinp(7)<1 or xinp(7)>30 then ce=7: goto ERR2
	if xinp(6)<10182 or xinp(6)>123199 then ce=6: goto ERR2
	if xinp(1)=0 then goto L1620
	ce=1
	k$=rpad$(str$(xinp(1)),5)
	read #1,using L1560,key=k$: a1$,mat app nokey ERR2
	L1560: form pos 6,c 30,pos 375,20*n 1,x 60,20*n 1
	if env$('client')="ACS" and (b8=0 or app(b8)=1) then goto L1610
	if b8=0 then goto L1610
	if b8>0 and b8<31 then goto L1610
	ce=8
	goto ERR2
L1610: ce=0
L1620: if b7=2 then goto L1790
	ce=2
	e$=lpad$(str$(xinp(2)),9)
	if e$=he$ then goto L1680
	read #8,using L1670,key=e$,release: e1$,mat r nokey ERR2
	L1670: form pos 10,c 25,pos 578,11*pd 3.2
L1680: !
	ce=0
	he$=e$
	if xinp(7)>10 then r1=11 else r1=xinp(7)
	if r1=0 then goto L1760
	if xinp(3)=0 then goto L1780
	if xinp(4)=0 then xinp(4)=r(r1) else goto L1750
goto L1760
L1750: !
	if r(r1)=0 then r(r1)=xinp(4): rewrite #8,using L1670,key=e$: e1$,mat r
L1760: !
	if xinp(4)=0 then ce=4: goto ERR2
	if xinp(5)<>0 then goto L1790
L1780: !
	if xinp(3)=0 and xinp(4)=0 then goto L1790 else xinp(5)=xinp(3)*xinp(4)
L1790: !
	de$=""
	if sc then
		ce=9
		read #7,using L1840,key=lpad$(str$(sc),4): de$ nokey ERR2
		L1840: form pos 5,c 30
		if ltrm$(des$)="" then des$=de$
		ce=0
	end if
	if chg=2 then goto L2090
	rw+=1
	pause
	if expcode=0 then
		write #2,using L1900,rec=rw: mat xinp,b6,b7,b8,sc,iv$,0,des$
	else
		write #2,using L1900,rec=rw: mat xinp,b6, 2,b8,sc,iv$,0,des$
	end if
	L1900: form pos 1,n 5,n 9,2*pd 3.2,pd 4.2,n 6,n 2,pd 2,pd 1,n 2,n 4,c 12,pd 3,c 30
	thrs+=xinp(3)
	if env$('client')<>"ACS" then goto L2080 ! only ask phone on acs
	expcode=1
	if pe$="Y" then goto L2080
	! close #101: ioerr ignore
	! open #101: "SROW=20,SCOL=14,EROW=20,ECOL=60,BORDER=DR",display,outIn
	! pr #101: newpage
	! pr f "20,15,C 44": "Do you wish to record PHONE EXPENSE (Y/N):"
	! L1980: !
	pe$="N" ! input fields "20,58,CU 1,UEA,N": pe$
	! if pe$="Y" then
	! 	close #101:
	! 	des$="TELEPHONE EXPENSE"
	! 	xinp(5)=xinp(3)*10.
	! 	xinp(3)=xinp(4)=0
	! 	ce=5
	! 	pr f mat io1$: mat xinp,b8,sc,des$
	! 	goto L1320
	! else if pe$="N" then
		close #101:
		goto L2080
	! end if
! goto L1980

L2080: !
	pe$=" "
goto L740

L2090: !
	rewrite #2,using L1900,rec=rr: mat xinp,b6,b7,b8,sc,iv$,nta,des$
	thrs=thrs+xinp(3)

L2110: !
	pr newpage
	pr f "10,10,c 60": "ENTER REF # TO CORRECT; ENTER 0 WHEN COMPLETED"
	pr f "12,12,C 40": "LAST REF # ON FILE IS: "&str$(lrec(2))
	L2140: !
	input fields "10,60,n 5,eu,n": rr conv L2140
	chg=2
	if rr=0 then goto L600
	if rr>rw or rr<1 then goto L2140
	scrid$(1)="TIME MANAGEMENT CORRECTION SCREEN"
	scrid$(2)="Enter CLIENT # as -1 to DELETE THIS ENTRY."
	read #2,using L1900,rec=rr: mat xinp,b6,b7,b8,sc,iv$,nta,des$
	thrs=thrs-xinp(3)
goto L830

L2230: ! r: correction listing
	pr newpage
	pr f "10,10,c 60,h,n": "TIME MANAGEMENT CORRECTION LISTING IN PROCESS"
	fnopenprn
	if rtrm$(file$(255))(1:4)<>"PRN:" then goto L2280
	if cp=1 then pr #255,using L2280: hex$("2B0205000F1042") else pr #255,using L2280: hex$("2B0205000F1042")
	L2280: form pos 1,c 9,skip 0
	form c 9,skip 0
	pr #255,using L2310: date$,"TIME MANAGEMENT INPUT LISTING ",time$,env$('cnam')
	L2310: form pos 1,c 8,pos 40,c 40,skip 1,pos 1,c 8,pos ntab,c 40,skip 2
	pr #255: "REF #  CLIENT #  EMPLOYEE #    HOURS       RATE     AMOUNT    DATE   CATEGORY   MONTH CODE     SERVICE-CODE   TYPE  DESCRIPTION"
	inp2=0
	tinp3=0
	totexp=0
	tottime=0
	thrs=0
	for j=1 to rw
		read #2,using L1900,rec=j: mat xinp,b6,b7,b8,sc,iv$,nta,des$
		if xinp(7)=0 then goto L2570

		if inp2 and inp2><xinp(2) then
			pr #255,using L2430: tinp3,tottime,totexp
			L2430: form pos 21,"      ----------",skip 1,pos 10,"TOTAL HOURS",pos 27,n 10.2,skip 1,pos 10,"TOTAL LABOR",pos 27,n 10.2,skip 1,pos 10,"TOTAL EXPENSE",pos 27,n 10.2,skip 2
			tinp3=0
			totexp=0
			tottime=0
		end if

		tinp3=tinp3+xinp(3)
		if b7=2 then totexp=totexp+xinp(5) else tottime=tottime+xinp(5)
		k$=rpad$(str$(xinp(1)),5)
		a1$=""
		read #1,using L1560,key=k$: a1$ nokey L2530 ioerr L2530
		if des$(1:5)="TELEP" then a1$=des$
		L2530: !
		pr #255,using L2540: j,mat xinp,b8,sc,b7,a1$
		L2540: form pos 1,n 5,n 8,n 12,3*n 11.2,n 9,2*n 9,x 12,n 4,x 4,n 8,x 3,c 30,skip 1
		thrs=thrs+xinp(3)
		inp2=xinp(2)
		L2570: !
	next j
	pr #255,using L2430: tinp3,tottime,totexp
	dim sendto$*80
	fncloseprn ! sENDTO$=FILE$(255): pr #255: NEWPAGE : Close #255: : If SENDTO$(1:4)<>"PRN:" Then Execute "SY START /W "&SENDTO$ : Execute "DROP "&SENDTO$&" -N"
goto L600 ! /r
L2620: !
	close #1:
	close #2:
fnchain("S:\Time Management\Enter Time and Charges Merge")

SRCH1: ! r:
	s1=1 ! NAME SEARCH
	open #127: "SROW=1,SCOL=1,EROW=24,ECOL=80",display,outIn  ! SAVE SCREEN
L2820: !
	pr #127: newpage
	close #101: ioerr ignore
	open #101: "SROW=6,SCOL=3,EROW=08,ECOL=78,BORDER=DR,CAPTION=BUSINESS NAME SEARCH",display,outIn
	prtall=0
	pr f "7,4,C 55,H,N": "Enter beginning search info. or blank for all:"
	pr f "9,32,C 16,R,N": "Press F5 to stop"
	dim nam$*25
L2880: !
	input fields "7,50,C 25,UE,N": nam$
	if cmdkey=5 then goto SRCHEND
	nam$=rtrm$(nam$)
	l1=len(nam$)
	restore #32,search>=nam$: nokey L2880
	close #101: ioerr ignore
L2940:!
	pr newpage
	pr f "1,10,C 5,R,N": "ACCT#"
	pr f "1,17,C 30,R,N": "COMPANY NAME"
	cde=0
	for j=1 to 20
		read #32,using L3000,release: k$,a1$ eof L3100
		L3000: form pos 1,c 5,c 30
		if a1$(1:l1)=nam$ or prtall=1 then goto L3020 else goto L3100
L3020: !
	cde=1
		pr f str$(j+1)&",10,C 5,ut,N": k$
		pr f str$(j+1)&",17,C 30,ut,N": a1$
		if j>1 then goto L3090
		bk=bk+1
		if bk>20 then bk=1
		bk$(bk)=a1$
		L3090: !
	next j
L3100: !
	if j>1 then j=j-1
	mat in2$(j)
	pr f "24,08,C 60,R,N": "Enter to continue; F5 to stop or enter ACCOUNT #:"
L3130: !
	input fields "24,58,N 5,RE,N": k1 conv L3130
	alp=0
	if cmdkey=5 then goto SRCHEND
	if rtrm$(k$)="" then goto L3200
	k$=rpad$(str$(k1),5)
	read #1,using L1560,key=k$: a1$,mat app nokey L3130
	goto SRCHEND
L3200: if cmdkey><2 then goto L3250
	bk=bk-1
	if bk<1 then goto L3270
	restore #32,key>=bk$(bk): nokey L3270
	bk=bk-1
L3250: selclp=1
	goto L2940
L3270: selclp=0
	goto L2820
SRCHEND: close #101: ioerr L3300
L3300: close #127: ioerr L3310
L3310: ce=hce
	if k1=0 then goto L3350
	pr f io1$(1): k1
	pr f "5,40,c 30,n": a1$
	L3350: !
on sno goto L970,L1320
! /r
TMSRCH: ! r: search for customer #
	dim selection$*70
	fnSearch(32,"form pos 1,c 5,pos 6,c 30,pos 66,c 15,pos 283,pd 5.2",'pic($$$,$$$.##)',selection$,5)
	k$=z$=selection$ ! pull key from first field in search line
	xinp(1)=0
	xinp(1)=val(selection$) conv ignore
goto L840 ! /r
Xit: fnXit
include: Ertn
