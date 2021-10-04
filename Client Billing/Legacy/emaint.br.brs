! REPLACE S:\Client Billing\Legacy\EMAINT
!
	on error goto Ertn
	autoLibrary
	fnTop(program$,"Employee")
!
	dim ccat$(10)*30,nccat$(20)*30
	dim cat$(30)*30
	fnRead30Categories(mat cat$)
	mat ccat$=cat$(1:10)
	mat nccat$=cat$(11:30)
	
	dim scr3$(15)*25,fl3$(17),ot3$(15),in3$(15),scrid3$(2)*80
	dim fl4$(23),ot4$(40),in4$(40),scrid4$(3)*80
	dim fl5$(22),ot5$(40),in5$(40),scrid5$(2)*80
	dim fl6$(22),ot6$(40),in6$(40),scrid6$(2)*80
	dim s(10),t(10),u(10),v(10),w(20),x(20),y(20),z(20),e(4,30)
	dim eno$*9,e$*25,d(2),r(11),tcat$*20
	dim hlp$(20)*78,flh$(22)*18,hhd$*60,a$*5
	open #1: "Name=S:\Client Billing\Legacy\TMSCRN.EM,Shr",internal,input
L180: form pos 1,103*c 18
	read #1,using L180: mat fl3$,mat ot3$,mat in3$
	read #1,using L180: mat fl4$,mat ot4$,mat in4$
	read #1,using L180: mat fl5$,mat ot5$,mat in5$
	read #1,using L180: mat fl6$,mat ot6$,mat in6$
	close #1:
	
	gosub L1270
	open #1: "Name=S:\Core\Data\acsllc\EMmstr.h[cno],KFName=S:\Core\Data\acsllc\EMIndex.h[cno],Shr",internal,outIn,keyed ioerr L2370
L300: pr newpage
	on fkey 5 goto L300
	pr f "3,9,C 60": "Employee Master File"
	pr f "4,6,C 72": "Company Number "&env$('cno')&"  "&ltrm$(env$('cnam'))
	pr f "6,9,C 60": "1 = Initial File Preparation"
	pr f "7,9,C 60": "2 = Add New Records"
	pr f "8,9,C 60": "3 = File Maintenance / Inquiry"
	pr f "9,9,C 60": "4 = pr Proof Listing"
	pr f "10,9,C 60": "5 = Staff Directory Report"
	pr f "11,9,C 60": "6 = Remove Terminated Employees"
	pr f "13,9,C 60": "0 = Completed (Return To T/M Menu)"
	pr f "15,9,C 18": "Enter Selection #:"
L420: input fields "15,35,N 1,UE,N": ti conv L420
	if ti=0 then goto L2190
L440: on ti goto L2220,L470,L520,PrListing,L450,L460 none L300
L450: chain "S:\Client Billing\Legacy\TMSTAFDR"
L460: chain "S:\Client Billing\Legacy\EMREMTER"
L470: !
	scrid3$(1)="* ADD EMPLOYEE RECORDS * SCREEN 1 * "&date$&" * "&time$
	scrid3$(2)="  ENTER EMPLOYEE # AS 0 WHEN COMPLETED"
	mat d=(0): mat s=(0): mat w=(0): mat t=(0): mat x=(0): mat u=(0): mat y=(0): mat v=(0): mat z=(0): mat r=(0)
	eno$=" ": e$=" "
	goto L630
L520: !
	scrid3$(1)="* MAINTAIN EMPLOYEES * SCREEN 1 * "&date$&" * "&time$
	scrid3$(2)="  ENTER EMPLOYEE # AS 0 TO DELETE"
L540: !
	pr newpage
	pr f "10,10,c 53,n": "ENTER EMPLOYEE NUMBER, ENTER 0 WHEN COMPLETE"
L560: !
	input fields "10,60,N 9,UE,N": eno conv L560
	if eno=0 then goto L300
	eno$=lpad$(str$(eno),9)
	read #1,using L600,key=eno$: eno$,e$,mat d,mat s,mat w,mat t,mat x,mat u,mat y,mat v,mat z,mat r nokey L540
L600: form pos 1,c 9,c 25,pd 2,n 1,60*pd 4.2,60*pd 5.2,11*pd 3.2
	eno=val(eno$)
	holdeno$=eno$
L630: pr newpage
	pr f mat fl3$: mat scr3$,mat scrid3$
	pr f mat ot3$: eno$,e$,mat d,mat r
	pr f "24,1,C 80,H,N": "F1=SCREEN 1, F2=SCREEN 2, F3=SCREEN 3, F4=SCREEN 4, F5=COMPLETED"
L670: input fields mat in3$: x1,e$,mat d,mat r conv L1400
	if cv>0 then in3$(cv)(cv1:cv2)="U": cv=0
	if ti=2 and x1=0 then goto L300
	eno$=lpad$(str$(x1),9)
	if x1=eno or ti>2 then goto L880
	if x1=0 then goto L790
	eno$=lpad$(str$(x1),9)
	read #1,using L600,key=eno$: eno$ nokey L880
	pr f "4,38,c 38,r,n": "DUPLICATE EMPLOYEE; ENTER to continue."
	input fields "4,78,c 1,ae,n": pause$
	pr f "4,38,c 38,h,n": " "
	goto L1260
L790: pr newpage
	pr f "10,10,c 45,n": "EMPLOYEE NUMBER "&holdeno$&" WILL BE DELETED"
	pr f "11,10,c 45,n": "ENTER 1 TO DELETE, ENTER 2 TO RE-ENTER"
L820: input fields "11,60,N 1,UE,N": ans conv L820
	if ans=2 then goto L1260
	if ans><1 then goto L790
	delete #1,key=holdeno$: nokey L870
	new1=1
L870: goto L1260
L880: on cmdkey goto L630,L890,L990,L1080,L1170 none L890
L890: scrid4$(1)="EMPLOYEE:"&ltrm$(eno$)&" * "&rtrm$(e$)&" * SCREEN 2 * "&date$&" * "&time$
	scrid4$(2)="                        CURRENT CHARGEABLE HOURS     YTD CHARGEABLE HOURS"
	scrid4$(3)="                       CURRENT CHARGEABLE AMOUNT     YTD CHARGEABLE AMOUNT"
	pr newpage
	pr f mat fl4$: mat ccat$,mat ccat$,mat scrid4$
	pr f mat ot4$: mat s,mat t,mat u,mat v
	pr f "24,1,C 80,H,N": "F1=SCREEN 1, F2=SCREEN 2, F3=SCREEN 3, F4=SCREEN 4, F5=COMPLETED"
L960: input fields mat in4$: mat s,mat t,mat u,mat v conv L1480
	if cv>0 then in4$(cv)(cv1:cv2)="U": cv=0
	on cmdkey goto L630,L890,L990,L1080,L1170 none L990
L990: scrid5$(1)="EMPLOYEE:"&ltrm$(eno$)&" * "&rtrm$(e$)&" * SCREEN 3 * "&date$&" * "&time$
	scrid5$(2)="           NON-CHARGEABLE HOURS - CURRENT           NON-CHARGEABLE HOURS- YTD"
	pr newpage
	pr f mat fl5$: mat nccat$,mat scrid5$
	pr f mat ot5$: mat w,mat x
	pr f "24,1,C 80,H,N": "F1=SCREEN 1, F2=SCREEN 2, F3=SCREEN 3, F4=SCREEN 4, F5=COMPLETED"
L1050: input fields mat in5$: mat w,mat x conv L1560
	if cv>0 then in5$(cv)(cv1:cv2)="U": cv=0
	on cmdkey goto L630,L890,L990,L1080,L1170 none L1080
L1080: scrid6$(1)="EMPLOYEE:"&ltrm$(eno$)&" * "&rtrm$(e$)&" * SCREEN 4 * "&date$&" * "&time$
	scrid6$(2)="         NON-CHARGEABLE AMOUNT - CURRENT           NON-CHARGEABLE AMOUNT - YTD"
	pr newpage
	pr f mat fl6$: mat nccat$,mat scrid6$
	pr f mat ot6$: mat y,mat z
	pr f "24,1,C 80,H,N": "F1=SCREEN 1, F2=SCREEN 2, F3=SCREEN 3, F4=SCREEN 4, F5=COMPLETED"
L1140: input fields mat in6$: mat y,mat z conv L1640
	if cv>0 then in6$(cv)(cv1:cv2)="U": cv=0
	on cmdkey goto L630,L890,L990,L1080,L1170 none L1170
L1170: if ti=2 then goto L1200
	if x1=eno then goto L1250
	delete #1,key=holdeno$:
L1200: write #1,using L600: eno$,e$,mat d,mat s,mat w,mat t,mat x,mat u,mat y,mat v,mat z,mat r
	mat d=(0): mat s=(0): mat w=(0): mat t=(0): mat x=(0): mat u=(0): mat y=(0): mat v=(0): mat z=(0): mat r=(0)
	eno$=" ": e$=" "
	new1=1
	goto L1260
L1250: rewrite #1,using L600,key=eno$: eno$,e$,mat d,mat s,mat w,mat t,mat x,mat u,mat y,mat v,mat z,mat r
L1260: goto L440
L1270: scr3$(1)="EMPLOYEE NUMBER"
	scr3$(2)="EMPLOYEE NAME"
	scr3$(3)="PRIMARY DEPARTMENT #"
	scr3$(4)="EMPLOYEE STATUS"
	for j=1 to 10
		if rtrm$(ccat$(j))="" then goto L1360
		tcat$=rtrm$(ccat$(j)(1:20))
		scr3$(j+4)=tcat$&" RATE"
		goto L1370
L1360: scr3$(j+4)=" "
L1370: next j
	scr3$(15)="NON - CHARGEABLE RATE"
	return
L1400: if cv>0 then in3$(cv)(cv1:cv2)="U"
	cv=cnt+1
	pr f "24,78,C 1": bell
	in3$(cv)=rtrm$(in3$(cv))
	cv1=pos(uprc$(in3$(cv)),"U",1)
	cv2=cv1+1
	in3$(cv)(cv1:cv1)="CR"
	goto L670
L1480: if cv>0 then in4$(cv)(cv1:cv2)="U"
	cv=cnt+1
	pr f "24,78,C 1": bell
	in4$(cv)=rtrm$(in4$(cv))
	cv1=pos(uprc$(in4$(cv)),"U",1)
	cv2=cv1+1
	in4$(cv)(cv1:cv1)="RC"
	goto L960
L1560: if cv>0 then in5$(cv)(cv1:cv2)="U"
	cv=cnt+1
	pr f "24,78,C 1": bell
	in5$(cv)=rtrm$(in5$(cv))
	cv1=pos(uprc$(in5$(cv)),"U",1)
	cv2=cv1+1
	in5$(cv)(cv1:cv1)="RC"
	goto L1050
L1640: if cv>0 then in6$(cv)(cv1:cv2)="U"
	cv=cnt+1
	pr f "24,78,C 1": bell
	in6$(cv)=rtrm$(in6$(cv))
	cv1=pos(uprc$(in6$(cv)),"U",1)
	cv2=cv1+1
	in6$(cv)(cv1:cv1)="RC"
 goto L1140
PrListing: ! r:
	pr newpage
	fnopenprn
	dim dat$*20
	fndat(dat$)
	pr f "7,15,c 50,h,n": "POSITION PAPER FOR EMPLOYEE PROOF LISTING"
	pr f "10,10,c 40,n": "ENTER AS OF DATE FOR EMPLOYEE PROOF LIST"
	pr f "10,52,c 20,n": dat$
	L1760: !
	input fields "10,52,c 20,eu,n": dat$ conv L1760
	dattab=66-int(len(rtrm$(dat$))/2)
	pr newpage
	! on fkey 5 goto L2140
	pr f "10,20,c 50,h,n": "EMPLOYEE PROOF LIST IN PROCESS"
	pr f "23,2,C 60": "Press F5 to STOP"
	restore #1,key>="         ": nokey PrListFinis
	L1830: !
	read #1,using L1840: eno$,e$,mat d,mat e,mat r eof PrListFinis
	L1840: form pos 1,c 9,c 25,pd 2,n 1,60*pd 4.2,60*pd 5.2,11*pd 3.2
	gosub L2160
	pr #255,using L1870: "EMPLOYEE NUMBER",eno$
	L1870: form pos 1,c 20,pos 25,c 9,skip 1
	pr #255,using L1890: "EMPLOYEE NAME",e$
	L1890: form pos 1,c 20,pos 25,c 25,skip 1
	pr #255,using L1910: "PRIMARY DEPARTMENT #",d(1)
	L1910: form pos 1,c 20,pos 25,n 4,skip 1
	pr #255,using L1930: "EMPLOYEE STATUS",d(2)
	L1930: form pos 1,c 20,pos 25,n 1,skip 1
	pr #255,using L1950: "NON-CHARGEABLE RATE",r(11)
	L1950: form pos 1,c 20,pos 25,n 6.2,skip 5
	pr #255,using L1970: "CHARGEABLE HOURS","CHARGEABLE AMOUNT"
	L1970: form pos 39,c 16,pos 76,c 17,skip 1
	pr #255,using L1990: "CURRENT            YTD","CURRENT            YTD","RATES"
	L1990: form pos 36,c 25,pos 73,c 25,pos 100,c 5,skip 2
	for j=1 to 10
		pr #255,using L2020: ccat$(j),e(1,j),e(2,j),e(3,j),e(4,j),r(j)
		L2020: form pos 1,c 30,pos 35,n 8.2,pos 50,n 8.2,pos 70,n 10.2,pos 85,n 10.2,pos 99,n 6.2,skip 1
	next j
	pr #255,using L2050: "NON-CHARGEABLE HOURS","NON-CHARGEABLE AMOUNT"
	L2050: form skip 3,pos 37,c 20,pos 73,c 21,skip 1
	pr #255,using L2070: "CURRENT            YTD","CURRENT            YTD"
	L2070: form pos 36,c 25,pos 73,c 25,skip 2
	for j=1 to 20
		pr #255,using L2100: nccat$(j),e(1,j+10),e(2,j+10),e(3,j+10),e(4,j+10)
		L2100: form pos 1,c 30,pos 35,n 8.2,pos 50,n 8.2,pos 70,n 10.2,pos 85,n 10.2,skip 1
	next j
	pr #255: newpage
goto L1830
L2140: !
	pr #255: newpage
PrListFinis: !
	fncloseprn
goto L300
L2160: pr #255,using L2161: env$('cnam')
L2161: form pos 47,cc 40,skip 1
	pr #255,using L2170: "EMPLOYEE PROOF LISTING",dat$
L2170: form pos 56,c 26,skip 1,pos dattab,c 34,skip 2
	return
L2190: close #1:
	if new1=1 then goto L2340
	goto Xit
L2220: pr newpage
	pr f "8,22,c 31,r,n": "  ********  WARNING  ********"
	pr f "11,10,c 70": "THIS SELECTION ERASES ALL EMPLOYEE RECORDS ON THIS FILE."
	pr f "13,5,c 72": "ENTER PASSWORD TO CONTINUE; ELSE PRESS ENTER TO RETURN TO EMPLOYEE MENU."
	L2260: !
	input fields "15,10,C 5,IE,N": a$ conv L2260
	if uprc$(a$)="THINK" then goto L2280 else goto L300
	L2280: !
	pr newpage
L2290: !
	close #1: ioerr ignore
	fnFree('S:\Core\Data\acsllc\EMmstr.h[cno]')
	open #1: "Name=S:\Core\Data\acsllc\EMmstr.h[cno],SIZE=0,RecL=610",internal,output
	close #1:
 
L2340: !
	execute "Index S:\Core\Data\acsllc\EMmstr.h[cno],S:\Core\Data\acsllc\EMIndex.h[cno],1,9,REPLACE,DupKeys"
	if ti<2 then chain 'S:\Client Billing\Legacy\EMAINT' else goto Xit
	goto Xit
L2370: !
	if err=4152 then goto L2290
Xit: fnXit
include: ertn
