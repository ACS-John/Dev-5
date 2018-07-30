! Replace S:\acsSU\Company
! maintain company information file for checkbook management
! ______________________________________________________________________
	library 'S:\Core\Library': fntop,fnxit, fnerror,fncursys$,fnTos,fnLbl,fnAcs,fnCmdSet,fnTxt,fncombof,fnChk,fnButton,fnFra
	on error goto ERTN
	dim a$(3)*40,b$(2)*12,c$*5,d(2),e$(2)*12,tb$*30
	dim miscname$(10)*20,dedcode(10),dedfed(10),dedfica(10),dedst(10)
	dim prgl(5,3)
	dim d$(2)*1
	dim deduc(10),miscgl$(10)*12,actr$*1
	dim resp$(150)*40

	fntop(program$)
	right=1 : center=2
	cancel=5 : save=1
	open #company=1: "Name=[Q]\"&fncursys$&"mstr\Company.h[cno],Shr",internal,outIn,relative ioerr BUILD_COMPANY
	goto READ_COMPANY
READ_COMPANY: ! 
	read #company,using 'Form POS 1,3*C 40,2*C 12,C 5,2*N 1,N 2,N 1,C 9,C 12,N 3,N 6,N 3,PD 7.2,C 30,POS 298,15*PD 4,POS 382,N 2,N 2,PD 5.3,PD 5.2,PD 5.3,PD 5.2,G 1,PD 5.3,PD 5.2,N 1,10*C 20,50*N 1,10*C 12',rec=1: mat a$,mat b$,c$,mat d,wbc,ar1,mat e$,a1,a2,a3,ucm,tb$,mat prgl,jccode,nap,ficarate,ficawage,feducrat,feducwag,actr$,mcr,mcm,reccode,mat miscname$,mat dedcode,mat dedfed,mat dedfica,mat dedst,mat deduc,mat miscgl$
	if actr$="0" or actr$="1" then actr=val(actr$)
	if uprc$(actr$)="Y" then actr=1 else if uprc$(actr$)="N" then actr=0
	gosub NEWSCREEN 
	if ckey=save then 
		gosub SAVE 
		goto XIT 
	else if ckey=cancel then 
		goto XIT 
	else if ckey=page1 then 
		page=1 : gosub NEWSCREEN 
	else if ckey=page2 then 
		page=2 : gosub NEWSCREEN 
	else if ckey=page3 then
		page=3 : gosub NEWSCREEN
	else if ckey=page4 then
		page=4 : gosub NEWSCREEN
	else if ckey=save then
		gosub SAVE 
		goto XIT 
	else if ckey=cancel then 
		goto XIT
	end if
! /r
NEWSCREEN: ! r:
	fnTos(sn$='Company-Pg'&str$(page)) 
	lc=0
	page1=6 : page2=07 : page3=08 : page4=09
	page=1 : gosub PAGE1
	fnCmdSet(4) ! Save and Cancel
	fnAcs(sn$,0,mat resp$,ckey)
	if page=1 then 
		a$(1)=resp$(1) 
		a$(2)=resp$(2) 
		a$(3)=resp$(3) 
		b$(1)=resp$(4) 
		b$(2)=resp$(5) 
		tb$=resp$(6) 
		nap=val(resp$(7)) 
		wbc=val(resp$(8)(1:2))
		if resp$(9)='True' then prenum=1 else prenum=0
		if resp$(10)='True' then reccode=1 else reccode=0
	end if 
	if ckey=save then 
		gosub SAVE 
		goto XIT 
	else if ckey=cancel then 
		goto XIT
	end if
goto NEWSCREEN ! /r
PAGE1: ! r:
	lc=3 : mylen=40 : mypos=mylen+2
	fnLbl(lc+=1,1,'Company Name:',mylen,right)
	fnTxt(lc,mypos,40,0,0) 
	resp$(1)=a$(1)
	fnLbl(lc+=1,1,'Address:',mylen,right)
	fnTxt(lc,mypos,40,0,0) 
	resp$(2)=a$(2)
	fnLbl(lc+=1,1,'City State and Zip Code:',mylen,right)
	fnTxt(lc,mypos,40,0,0) 
	resp$(3)=a$(3)
	fnLbl(lc+=1,1,'Federal Identification Number:',mylen,right)
	fnTxt(lc,mypos,12,0,0) 
	resp$(4)=b$(1)
	fnLbl(lc+=1,1,'State Identification Number:',mylen,right)
	fnTxt(lc,mypos,12,0,0) 
	resp$(5)=b$(2)
	fnLbl(lc+=1,1,'Type of Business:',mylen,right)
	fnTxt(lc,mypos,30,0,0) 
	resp$(6)=tb$
	fnLbl(lc+=1,1,'Number of Periods:',mylen,right)
	fnTxt(lc,mypos,30,0,0,number$) 
	resp$(7)=str$(nap)
return ! /r
BUILD_COMPANY: ! r:
	open #company=1: "Name=[Q]\TMmstr\Company.h[cno],Size=0,RecL=882,Replace",internal,outIn,relative 
	write #company,using 'Form POS 1,3*C 40,2*C 12,C 5,2*N 1,N 2,N 1,C 9,C 12,N 3,N 6,N 3,PD 7.2,C 30,POS 298,15*PD 4,POS 382,N 2,N 2,PD 5.3,PD 5.2,PD 5.3,PD 5.2,G 1,PD 5.3,PD 5.2,N 1,10*C 20,50*N 1,10*C 12',rec=1: mat a$,mat b$,c$,mat d,1,0,mat e$,a1,a2,a3,ucm,tb$,mat prgl,jccode,nap,ficarate,ficawage,feducrat,feducwag,actr,mcr,mcm,reccode,mat miscname$,mat dedcode,mat dedfed,mat dedfica,mat dedst,mat deduc,mat miscgl$
goto READ_COMPANY ! /r
SAVE: ! r:
	rewrite #company,using 'Form POS 1,3*C 40,2*C 12,C 5,2*N 1,N 2,N 1,C 9,C 12,N 3,N 6,N 3,PD 7.2,C 30,POS 298,15*PD 4,POS 382,N 2,N 2,PD 5.3,PD 5.2,PD 5.3,PD 5.2,G 1,PD 5.3,PD 5.2,N 1,10*C 20,50*N 1,10*C 12',rec=1: mat a$,mat b$,c$,mat d,wbc,ar1,mat e$,a1,a2,a3,ucm,tb$,mat prgl,jccode,nap,ficarate,ficawage,feducrat,feducwag,actr,mcr,mcm,reccode,mat miscname$,mat dedcode,mat dedfed,mat dedfica,mat dedst,mat deduc,mat miscgl$
return  ! /r
XIT: ! r:
	close #company: 
fnxit ! /r
include: ertn