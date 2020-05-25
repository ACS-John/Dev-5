 
	on error goto L990
	autoLibrary
	dim cde$*6,des$*55,gl(3),cnam$*40,a$*5,cap$*128
	fnTop(program$,cap$="Invoice Description")
	fncno(cno,cnam$)
	fnconsole(1)
	hp1=43-int(len(rtrm$(cnam$))/2)
 
	io1$(1)="14,5,C 55,U,N": io1$(2)="16,40,N 10.2,U,N"
	io1$(3)="18,33,N 3,U,N": io1$(4)="18,38,N 6,U,N": io1$(5)="18,46,N 3,U,N"
	open #1: "Name=S:\Core\Data\acsllc\IVDesc.h[cno],KFName=S:\Core\Data\acsllc\IVDIndex.h[cno],Shr",internal,outIn,keyed ioerr L980
L130: pr newpage
	pr f "3,9,C 50,N": "INVOICE DESCRIPTION FILE MENU"
	pr f "4,6,C 72,N": "COMPANY NUMBER [cno]  "&ltrm$(cnam$)
	pr f "6,9,C 60": "1 = INITIAL FILE PREPARATION"
	pr f "7,9,C 60": "2 = ADD OR FILE MAINTENANCE / INQUIRY"
	pr f "8,9,C 60": "3 = pr PROOF LIST"
	pr f "10,9,C 60": "0 = COMPLETED (RETURN TO T/M MENU)"
	pr f "12,9,C 20": "ENTER SELECTION #:"
L210: input fields "12,28,N 1,UE,N": ti conv L210
	if ti=0 then goto L390
	on ti goto L240,L510,L740 none L210
L240: pr newpage
	ctab=40-int(len(ltrm$(rtrm$(cnam$)))/2)
	pr f "7,"&str$(ctab)&",C 40,N": cnam$
	pr f "10,31,C 19,RB,N": "  *** WARNING ***"
	pr f "12,4,C 70": "YOU HAVE CHOSEN TO INITIALLY PREPARE THE INVOICE DESCRIPTION FILE."
	pr f "13,1,C 78": "IF YOU CONTINUE ALL EXISTING INVOICE DESCRIPTION RECORDS WILL BE ERASED."
	pr f "15,6,C 70": "ENTER PASSWORD TO CONTINUE; ELSE PRESS ENTER TO RETURN TO MENU"
L310: input fields "15,75,C 5,IE,N": a$ conv L310
	if uprc$(a$)="THINK" then goto L330 else goto L130
L330: close #1: ioerr L340
L340: open #1: "Name=S:\Core\Data\acsllc\IVDesc.h[cno],NoShr",internal,outIn ioerr L360
	close #1,free:
L360: open #1: "Name=S:\Core\Data\acsllc\IVDesc.h[cno],Replace,RecL=84",internal,output ioerr L990
	ti=1
	new1=1
L390: close #1:
	if new1=0 then goto Xit
	execute "Index S:\Core\Data\acsllc\IVDesc.h[cno],S:\Core\Data\acsllc\IVDIndex.h[cno],1,6,REPLACE,DupKeys" ioerr L480
L480: if ti=1 then chain 'S:\acsTM\IVDMAINT'
	close #1: ioerr ignore
	goto Xit
L510: pr newpage
	pr f "10,5,C 60": "ENTER DESCRIPTION CODE OR BLANK TO STOP:"
	input fields "10,50,C 6,UE,N": cde$
	if rtrm$(cde$)="" then goto L130
	cde$=uprc$(lpad$(rtrm$(cde$),6))
	des$=""
	da=0
	mat gl=(0)
	read #1,using L600,key=cde$: cde$,des$,da,mat gl nokey L630 ioerr L990
L600: form pos 1,c 6,c 55,pd 5.2,n 3,n 6,n 3
	pr f "7,15,C 26,N": "***  FILE MAINTENANCE  ***"
	goto L640
L630: pr f "7,15,C 26,N": "********   ADD   *********"
L640: pr f "12,5,C 60": "ENTER INVOICE DESCRIPTION OR BLANK TO DELETE THIS ENTRY"
	pr f "16,5,C 60": "STANDARD CHARGE AMOUNT OR ZERO:"
	pr f "18,9,C 60": "GENERAL LEDGER NUMBER:"
	pr f mat io1$: des$,da,mat gl
L680: input fields mat io1$: des$,da,mat gl conv L680
	if rtrm$(des$)="" then delete #1,key=cde$: nokey L510: new1=1: goto L510
	rewrite #1,using L600,key=cde$: cde$,des$,da,mat gl nokey L720
	goto L510
L720: write #1,using L600: cde$,des$,da,mat gl: new1=1
	goto L510
L740: pr newpage
	fnopenprn
	pr f "10,20,C 45": "INVOICE DESCRIPTION PROOF LISTING IN PROCESS"
	pr f "24,3,C 30,N": "Press F5 to stop!"
	on fkey 5 goto L880
	restore #1,key>="      ": nokey L130
	gosub L910
L810: read #1,using L600: cde$,des$,da,mat gl eof L880 ioerr L990
	pr #255,using L830: cde$,des$,da,mat gl pageoflow L850
L830: form pos 1,c 8,c 57,n 10.2,n 5,n 7,n 4,skip 1
	goto L810
L850: pr #255: newpage
	gosub L910
	continue
L880: fncloseprn
	on fkey 5 ignore
	goto L130
L910: pr #255,using L920: date$,cnam$
L920: form skip 2,pos 1,c 8,pos hp1,c 40,skip 1
	pr #255,using L940: time$,"INVOICE DESCRIPTION PROOF LISTING"
L940: form pos 1,c 8,pos 26,c 33,skip 2
	pr #255: "  CODE  DESCRIPTION                                              STD AMOUNT   GL  NUMBER"
	pr #255: "------  -------------------------------------------------------  ----------  --- ------ ---"
return
L980: if err=4152 then goto L360
L990: if err=61 then pr f "23,3,C 75,N": "THIS PROGRAM IS TRYING TO ACCESS A RECORD THAT IS IN USE!" else goto L1010
	goto L1050
L1010: pr newpage
	if err=4148 then pr f "23,3,C 78,N": "THIS PROGRAM IS TRYING TO ACCESS A FILE THAT IS IN USE AND CANNOT BE SHARED!" else goto L1040
	goto L1050
L1040: pr f "23,3,C 75,N": "YOU HAVE A WORKSTATION BASIC ERROR # "&str$(err)&" AT LINE # "&str$(line)&"."
L1050: pr f "24,3,C 70,N": "PRESS ENTER TO RETRY; ELSE ENTER  Q  TO QUIT"
	input fields "24,60,C 1,N": quitcode$
	if rtrm$(uprc$(quitcode$))="Q" then goto Xit
	pr f "23,3,C 78,N": ""
	pr f "24,3,C 78,N": ""
	retry
Xit: fnXit
IGNORE: continue
