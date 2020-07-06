! Replace S:\acsUB\ubAccTr
! -- Accumulated Transaction Listing
autoLibrary
on error goto Ertn
fnTop(program$,"Accumulated Transaction List")
 
dim dat$*20,resp$(3)*20,pe2$*30,e2$*30,cnam$*40,text$*80
fndat(dat$)
code$(1)="Charge"
code$(2)="Penalty"
code$(3)="Collection"
code$(4)="Credit Memo"
code$(5)="Debit Memo"
code$(6)="INVALID !?!"
 
SCREEN1: !
	sn$ = "ubAccTr"
	fnTos(sn$)
	mylen=26
	mypos=mylen+2
	text$="Report Heading Date:"
	fnLbl(1,1,text$,mylen,1)
	fnTxt(1,mypos,20)
	resp$(1)=dat$
	text$="Starting Date (mmddyy):"
	fnLbl(2,1,text$,mylen,1)
	fnTxt(2,mypos,8,0,0,"1")
	resp$(2)=""
	text$="Ending Date (mmddyy):"
	fnLbl(3,1,text$,mylen,1)
	fnTxt(3,mypos,8,0,0,"1")
	resp$(3)=""
	text$="You may leave Starting Date and/or Ending Date blank to indicate all."
	fnLbl(5,1,text$,75)
	fnCmdSet(3)
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	dat$=resp$(1)
	d(1)=val(resp$(2))
	d(2)=val(resp$(3))
	if d(1)<>0 then d(1)=fndate_mmddyy_to_ccyymmdd(d(1))
	if d(2)<>0 then d(2)=fndate_mmddyy_to_ccyymmdd(d(2))
	fndat(d$(1),2)
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,outIn,keyed
	open #2: "Name=[Q]\UBmstr\UBTransVB.h[cno],KFName=[Q]\UBmstr\UBTrIndx.h[cno],Shr",internal,input,keyed
	on fkey 5 goto DONE
	fnopenprn
	gosub HDR
READ_CUSTOMER: !
	read #1,using L370: z$,e2$,bal eof DONE
L370: form pos 1,c 10,pos 41,c 30,pos 292,pd 4.2
	restore #2,key>=z$&"         ": nokey READ_CUSTOMER
	first_trans_per_act=1 ! True
READ_TRANS: !
	read #2,using L430: p$,tdate,tcode,tamount,tbal eof READ_CUSTOMER
! If TRIM$(P$)="100550.00" Then FNPAUSE
L430: form pos 1,c 10,n 8,n 1,pd 4.2,pd 4.2
	if trim$(p$)<>trim$(z$) then goto READ_CUSTOMER
	if (d(1)<>0 and tdate<d(1)) or (d(2)<>0 and tdate>d(2)) then
		goto READ_TRANS
	end if
	gosub PRINT_TRANS
	first_trans_per_act=0 ! false
	goto READ_TRANS
!
DONE: !
	close #1: ioerr L520
L520: close #2: ioerr L530
L530: close #3: ioerr L540
L540: fncloseprn
	goto Xit
!
HDR: !
	p2=p2+1
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
	pr #255: "\qc {\ul Act.Number} {\ul Account Name                } {\ul Trans. Type} {\ul   Date  } {\ul       Amount} {\ul       Balance}}"
	return
!
PRINT_TRANS: !
	if tcode<1 or tcode>5 then tcode=6
	if first_trans_per_act=1 then
		pz$=z$ : pe2$=e2$
	else
		pz$="" : pe2$=""
	end if
	pr #255,using L700: pz$,pe2$,code$(tcode),tdate,tamount,tbal pageoflow PGOF
L700: form pos 1,c 10,x 1,c 30,cr 11,nz 9,n 13.2,n 14.2
	return
!
PGOF: !
	pr #255: newpage
	gosub HDR
continue
!
Xit: fnXit
include: Ertn
