autoLibrary
on error goto Ertn
fnTop(program$)
! r: read company for xd1,cogl$
	dim cogl$*12
	open #hCompany=fnH: 'Name=[Q]\GLmstr\Company.h[cno]',i,i,r
	read #hCompany,using 'form pos 150,N 1,pos 176,C 12',rec=1: xd1,cogl$
	close #hCompany:
	hCompany=0
! /r
open #hAccount=1: 'Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\glIndex.h[cno],Shr',i,i,k
if fnProcess=1 or xd1=0 then goto StartReport		! skip Cost Center Question if not applicable or in Automatic Processing

ScrAskCostCenter: ! r: returns n$,d$,bb,cb, sets rec in hAccount
	fnTos
	mylen=12 : mypos=mylen+2
	fnLbl(1,1,'Cost Center:',mylen,1)
	fnTxt(1,mypos,3,0,0,'number')
	resp$(1)=''
	fnCmdSet(3)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	costcent=val(resp$(1))
	dim n$*12
	n$=lpad$(str$(costcent),3)&'     0  0'
	dim d$*50
	read #hAccount,using 'form pos 1,C 12,C 50,pos 81,2*PD 6.2',key>=n$: n$,d$,bb,cb nokey ScrAskCostCenter
! pr NEWPAGE
! pr f '10,2,C 78': 'Enter the Cost Center (Fund or Company Num) If you wish to only pr a work'
! pr f '11,2,C 69': 'sheet for one Cost Center (Blank for All)'
! pr f '13,34,C 11,B,5': 'Cancel (F5)'
! Input Fields '11,70,Nz 3,EUT,N': COSTCENT Conv 250
! If CMDKEY=5 Then goto Xit
! n$=LPAD$(STR$(COSTCENT),3)&'     0  0'
! Read #hAccount,Using 'form pos 1,C 12,C 50,pos 81,2*PD 6.2',Key>=N$: N$,D$,BB,CB Nokey ScrAskCostCenter ! /r
StartReport: ! r:
! fnwait
	! pr f '10,25,C 30,R,N': 'G/L WORKSHEET IN PROCESS'
	! pr f '12,20,Cc 30,B,5': 'Cancel (F5)'
	on fkey 5 goto Finis
	fnOpenPrn
	isFirst=1
	gosub Header
	if fnProcess=1 or xd1=0 then
		goto ReadAccount
	else
		goto AfterAccountRead
	end if
! /r

Header: ! r:
	pr #255,using 'form pos 1,C 20,Cc 72': date$('mm/dd/yy'),env$('cnam')
	pr #255,using 'form pos 1,c 20,cc 72': time$,env$('program_caption')
	pr #255,using 'form pos 21,Cc 72': fnpedat$
	pr #255: ''
	pr #255: tab(6);'Account';
	pr #255: tab(50);'Trial Balance';tab(73);'Adjustments';
	pr #255: tab(90);'Profit and Loss';tab(112);'Balance Sheet'
	pr #255,using 'form pos 6,C 6,pos 17,C 11,pos 47,C 5,pos 60,C 6,pos 69,C 5,skip 0': 'Number','Description','Debit','Credit','Debit'
	pr #255,using 'form pos 80,C 6,pos 89,C 5,pos 101,C 6,pos 111,C 5,pos 121,C 6': 'Credit','Debit','Credit','Debit','Credit'
	pr #255: ''
	pr #255: ''
return ! /r
ReadAccount: ! r:
	read #hAccount,using 'form pos 1,C 12,C 50,pos 81,2*PD 6.2': n$,d$,bb,cb eof Finis
	if ~isFirst and n$(1:3)<>oldn$(1:3) then gosub Totals
	isFirst=0
	oldn$=n$
	if costcent><0 and val(n$(1:3))><costcent then goto Finis

AfterAccountRead: !
	dno=val(n$(1:3)) : ano=val(n$(4:9)) : sno=val(n$(10:12))
	! r: SOMETHING
		if cb<0 then
			crtotal+=cb : p1=54
		else
			drtotal+=cb : p1=40
		end if
		dim p$*62
		p$='|        *         |         *         |         *        |'
		pr #255,using 'form pos 1,pic(ZZZ),X 1,pic(ZZZZZZ),X 1,pic(ZZZ),X 2,C 22,pos P1,N 12.2,pos 68,C 62': dno,ano,sno,d$(1:22),cb,p$ pageoflow PgOf
		pr #255,using 'form pos 68,C 62': p$ pageoflow PgOf
	! /r

	if n$<>cogl$ then goto ReadAccount
	pr #255,using L540: '**Net Income or Loss',drtotal+crtotal
	L540: form pos 17,c 20,pos 38,pic(---,---,---.##),skip 2
goto ReadAccount ! /r
Finis: ! r:
	Finis=1
	gosub Totals
	close #hAccount:
	fnClosePrn
goto Xit ! /r
Totals: ! r:
	pr #255:
	pr #255: tab(13);'Worksheet Proof Totals';
	oldn$=n$
	pr #255,using L610: drtotal,crtotal,page$
	L610: form pos 39,pic(---,---,---.##),pos 53,pic(---,---,---.##),c 80
	pr #255: ''
	if Finis<>1 then
		drtotal=crtotal=0
		pr #255: newpage
		gosub Header
	end if
return ! /r
PgOf: pr #255: newpage : gosub Header : continue
Xit: fnXit
include: ertn
