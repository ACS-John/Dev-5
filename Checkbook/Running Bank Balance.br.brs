autoLibrary
on error goto Ertn

dim dat$*20,de$*35,bn$*30,ml$(0)*100

fnTop(program$)
fndat(dat$,1)
open #20: 'Name=[Q]\CLmstr\Company.h[cno],Shr',i,outi,r
read #20,using 'form pos 152,N 2',rec=1,release: bank_code
close #20:
MAIN: !
	fnTos
	respc=0
	fnLbl(1,40,'',1,1)
	fnLbl(1,1,'Starting Date:',31,1)
	fnTxt(1,33,10,0,1,'3')
	resp$(respc+=1)=''
	fnLbl(2,1,'Beginning Checkbook Balance:',31,1)
	fnTxt(2,33,12,0,1,'10')
	resp$(respc+=1)=''
	fnLbl(3,1,'Bank Number to Print:',31,1)
	fnTxt(3,33,2,0,1,'30')
	resp$(respc+=1)=str$(bank_code)
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	d1=val(resp$(1))
	b1=val(resp$(2))
	bank_code=val(resp$(3))
 
	open #20: 'Name=[Q]\CLmstr\BankMstr.h[cno], KFName=[Q]\CLmstr\BankIdx1.h[cno],Shr', internal, outin, keyed
	read #20,using 'form pos 3,C 30',key=lpad$(str$(bank_code),2),release: bn$ nokey MAIN
	close #20:
 
	close #trmstr: ioerr ignore
	execute 'Index [Q]\CLmstr\TrMstr.h[cno] [Q]\CLmstr\Tridx3.h[cno] 16/12/4 2/4/8 Replace DupKeys -n' ! index in year,monthday,reference
 
	open #trmstr=5: 'Name=[Q]\CLmstr\TrMstr.h[cno], KFName=[Q]\CLmstr\Tridx3.h[cno],Shr', internal, outin, keyed ioerr ignore
	fnOpenPrn
	gosub HDR
	goto READ_1
 
READ_1: !
	key$=cnvrt$('pic(########)',d1)(3:8): key$=key$&'        ' ! kEY$=KEY$(3:6)&KEY$(1:2)&'        '
! Restore #TRMSTR,Key>=KEY$: Ioerr 440 ! need message box   (no dates in this range)
	restore #trmstr: ioerr L440
	goto READ_2
L440: mat ml$(2)
	ml$(1)='There are no transactions for'
	ml$(2)='the date entered.  Check the date.'
	fnMsgBox(mat ml$)
goto MAIN
READ_2: !
	read #trmstr,using 'form pos 1,N 2,N 1,C 8,g 6,PD 10.2,pos 36,C 35': tbank_code,tcde,checkNumber$,d2,amt,de$ eof ENDALL
	if fndate_mmddyy_to_ccyymmdd(d2)<d1 then goto READ_2
	if tbank_code<>bank_code then goto READ_2
	if tcde=2 or tcde=3 then p1=68 else p1=56
	if tcde=2 or tcde=3 then b1=b1+amt else b1=b1-amt
	pr #255,using 'form pos 1,C 10,PIC(ZZ/ZZ/ZZ),X 2,C 35,pos P1,N 12.2,pos 80,N 12.2': checkNumber$,d2,de$,amt,b1 pageoflow NEWPGE
goto READ_2
 
NEWPGE: pr #255: newpage: gosub HDR : continue
 
HDR: ! r:
	pr #255,using 'form pos 1,C 8,Cc 76': date$,env$('cnam')
	pr #255,using 'form pos 1,C 8,pos 36,C 40': time$,'Running Bank Balance'
	pf2=46-int(len(rtrm$(bn$))/2)
	pr #255,using 'form pos PF2,C 30': bn$
	pr #255,using 'form pos 1,C 4,N 4,Cc 76': 'Page',pg+=1,dat$
	pr #255: ''
	pr #255: 'Ref-Numb    Date    Payee/Description                      Checks     Deposits    Balance '
	pr #255: '________  ________  ___________________________________  __________  __________  __________'
	if p1=0 then
		d1$=cnvrt$('pic(####/##/##)',d1)
		d3=val(d1$(6:7))*10000+val(d1$(9:10))*100+val(d1$(3:4))
		pr #255,using 'form pos 1,C 10,pic(zz/zz/zz),X 2,C 35,pos 80,N 12.2': '',d3,'Beginning Balance',b1
	end if
return ! /r
ENDALL: ! r:
	fnClosePrn
	close #trmstr:
goto Xit ! /r
Xit: fnXit
include: ertn
