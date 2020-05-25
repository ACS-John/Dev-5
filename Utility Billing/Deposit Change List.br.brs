! formerly S:\acsUB\ubDepChg
! -- Customer Deposit Change Listing
 
	autoLibrary
	on error goto Ertn
	fnTop(program$)
	dim resp$(2)*20
	dim dp$*70
 
MENU1: ! r:
	fnTos(sn$:="ubDepChg")
	fnLbl(1,28," ",1,1)
	fnLbl(1,1,"Starting Date:",16,1)
	fnTxt(1,18,8,0,0,"1",0,"Use mmddyy format for the oldest date to be listed.")
	resp$(1)=str$(bd1)
	fnLbl(2,1,"Ending Date:",16,1)
	fnTxt(2,18,8,0,0,"1",0,"Use mmddyy format for the latest date to be listed.")
	resp$(2)=str$(ed1)
	fnCmdSet(3)
	fnAcs2(mat resp$,ckey)
	if ckey=5 then goto Xit
	bd1=val(resp$(1))
	bd1=fndate_mmddyy_to_ccyymmdd(bd1)
	ed1=val(resp$(2))
	ed1=fndate_mmddyy_to_ccyymmdd(ed1)
goto Report ! /r
Report: ! r: start report
	if bd1=20000000 then bd1=0
	if ed1=20000000 then ed1=0
	open #hDeposit2:=fngethandle: 'Name=[Q]\UBmstr\Deposit2.h[cno],KFName=[Q]\UBmstr\Deposit2Index.h[cno],Shr,Use,RecL=73,KPs=1,KLn=10',internal,outIn,keyed ! "Name=[Q]\UBmstr\Deposit2.h[cno],Shr",internal,outIn,relative
	fDeposit2: form pos 1,c 10,n 8,c 32,2*n 10.2,pd 3
	fnopenprn
	gosub HDR
	dim da(2)
	! gosub LegacyWay
	gosub NewWay
goto DONE ! /r
NEWPGE: pr #255: newpage : gosub HDR : continue
HDR: ! r:
	pg=pg+1
	pr #255: "\qc  {\f181 \fs22 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f181 \fs28 \b "&env$('program_caption')&"}"
	pr #255: "\qc  {\f181 \fs18 \b From: "&cnvrt$("PIC(####/##/##)",bd1)&"   Thru: "&cnvrt$("pic(####/##/##)",ed1)&"}"
	pr #255,using 'form pos 1,c 12,pos 75,c 10': "\ql "&date$,"Page "&str$(pg)
	pr #255: ""
	pr #255: "{\ul Account No}  {\ul    Date   }  {\ul Description                     }  {\ul  Before }  {\ul   After }"
return ! /r
DONE: ! r:
	pr #255: ""
	pr #255,using "Form POS 16,C 21,N 12.2": "Net Amount of Change:",t1
	fncloseprn
goto Xit ! /r
Xit: fnXit
include: Ertn
! LegacyWay: ! r: old way whcich uses next transaction address technology
!   read #hDeposit1,using "Form POS 1,C 10,2*PD 3": rk$,mat da eof FinisLegacyWay
!   ! read #hDeposit1,using "Form POS 1,C 10": rk$ eof DONE
!   r32=da(1)
!   do
!   if r32=0 then goto LegacyWay
!   read #hDeposit2,using fDeposit2,rec=r32: k32$,dt1,dp$,dp1,dp2,n32
!   if dt1>=bd1 or bd1=0 then
!     if dt1<=ed1 or ed1=0 then
!       pr #255,using 'form pos 1,c 12,pic(####/##/##bb),c 32,2*n 10.2': k32$,dt1,dp$,dp1,dp2 pageoflow NEWPGE
!       t1=t1+dp2-dp1
!     end if
!   end if
!   r32=n32
!   loop
!   FinisLegacyWay: !
! return ! /r
NewWay: ! r: new way whcich uses indexes
	do
	read #hDeposit2,using fDeposit2: k32$,dt1,dp$,dp1,dp2 eof FinisNewWay
	if dt1>=bd1 or bd1=0 then
		if dt1<=ed1 or ed1=0 then
			pr #255,using 'form pos 1,c 12,pic(####/##/##bb),c 32,2*n 10.2': k32$,dt1,dp$,dp1,dp2 pageoflow NEWPGE
			t1=t1+dp2-dp1
		end if
	end if
	loop
	FinisNewWay: !
return ! /r
