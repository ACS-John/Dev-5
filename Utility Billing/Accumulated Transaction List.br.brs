autoLibrary
on error goto Ertn
fnTop(program$)
dim dat$*20
fndat(dat$)

Screen1: ! r:
	dim resp$(3)*128
	fnTos
	mylen=26
	mypos=mylen+2
	fnLbl(1,1,"Report Heading Date:",mylen,1)
	fnTxt(1,mypos,20)
	resp$(1)=dat$
	fnLbl(2,1,"Starting Date (mmddyy):",mylen,1)
	fnTxt(2,mypos,8,0,0,"1")
	resp$(2)=""
	fnLbl(3,1,"Ending Date (mmddyy):",mylen,1)
	fnTxt(3,mypos,8,0,0,"1")
	resp$(3)=""
	fnLbl(5,1,"You may leave Starting Date and/or Ending Date blank to indicate all.",75)
	fnCmdSet(3)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	dat$=resp$(1)
	d(1)=val(resp$(2))
	d(2)=val(resp$(3))
	if d(1)<>0 then d(1)=fndate_mmddyy_to_ccyymmdd(d(1))
	if d(2)<>0 then d(2)=fndate_mmddyy_to_ccyymmdd(d(2))
	fndat(d$(1),2)
goto MainLoop ! /r

MainLoop: ! r:
open #hCustomer=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,outIn,keyed
Fcustomer: form pos 1,c 10,pos 41,c 30,pos 292,pd 4.2
open #hTrans=fnH: "Name=[Q]\UBmstr\UBTransVB.h[cno],KFName=[Q]\UBmstr\UBTrIndx.h[cno],Shr",i,i,k
Ftran: form pos 1,c 10,n 8,n 1,pd 4.2,pd 4.2
! on fkey 5 goto Finis
fnopenprn
gosub PrHeader
READ_CUSTOMER: !
dim e2$*30
read #hCustomer,using Fcustomer: z$,e2$,bal eof Finis
restore #hTrans,key>=z$&"         ": nokey READ_CUSTOMER
first_trans_per_act=1 ! True
do
	! READ_TRANS: !
	read #hTrans,using Ftran: p$,tdate,tcode,tamount,tbal eof READ_CUSTOMER
	! If TRIM$(P$)="100550.00" Then FNPAUSE
	if trim$(p$)<>trim$(z$) then goto READ_CUSTOMER
	if (d(1)<>0 and tdate<d(1)) or (d(2)<>0 and tdate>d(2)) then
		! goto READ_TRANS
	else
		gosub PrTrans
		first_trans_per_act=0 ! false
	end if
loop ! /r
Finis: ! r:
	close #hCustomer: ioerr ignore
	close #hTrans: ioerr ignore
	fncloseprn
goto Xit ! /r

PrHeader: ! r:
	p2+=1
	pr #255: "\qc {\b "&env$('cnam')
	pr #255: env$('program_caption')
	pr #255: dat$
	if d(1)<>0 then
		pr #255: "Starting Date: "&cnvrt$("pic(zzzz/zz/zz)",d(1))
	end if
	if d(2)<>0 then
		pr #255: "Ending Date: "&cnvrt$("pic(zzzz/zz/zz)",d(2))
	end if
	pr #255: "\qr Page "&str$(p2)
	pr #255: "\qc {\ul Account   } {\ul Name                        } {\ul Trans. Type} {\ul   Date  } {\ul       Amount} {\ul       Balance}}"
return ! /r
PrTrans: ! r:
	if ~prTransSetup then 
		prTransSetup=1
		code$(1)="Charge"
		code$(2)="Penalty"
		code$(3)="Collection"
		code$(4)="Credit Memo"
		code$(5)="Debit Memo"
		code$(6)="INVALID !?!"
	end if
	if tcode<1 or tcode>5 then tcode=6
	dim pe2$*30
	if first_trans_per_act=1 then
		pz$=z$ : pe2$=e2$
	else
		pz$="" : pe2$=""
	end if
	pr #255,using L700: pz$,pe2$,code$(tcode),tdate,tamount,tbal pageoflow PgOf
	L700: form pos 1,c 10,x 1,c 30,cr 11,nz 9,n 13.2,n 14.2
return ! /r
PgOf: ! r:
	pr #255: newpage
	gosub PrHeader
continue ! /r

Xit: fnXit
include: ertn
