! Replace S:\acsUB\Escrow1
 
	autoLibrary
	on error goto Ertn
 
	dim cnam$*40,customer_name$*30,cap$*128,resp$(2)*20
 
	fnTop("S:\acsUB\escrow1",cap$="Escrow Balance Report")
	fncno(cno,cnam$)
	fndat(resp$(1))
 
	fnTos(sn$="escrow1") : _
	mylen=20 : mypos=mylen+2 : lc=0
	fnLbl(lc+=1,1,"Report Heading Date:",mylen,1)
	fnTxt(lc,mypos,20)
	fnLbl(lc+=1,1,"Sort by:",mylen,1)
	opt$(1)="1. Account" : opt$(2)="2. Name" : mat opt$(2) : _
	fncomboa("acc_or_nam",lc,mypos,mat opt$) : _
	resp$(2)=opt$(1)
	fnCmdSet(3)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	fndat(resp$(1),put=2)
	customer=1 : _
	if resp$(2)=opt$(1) then : _
		open #customer: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",i,i,k else : _
		if resp$(2)=opt$(2) then : _
			open #customer: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\UBIndx2.h[cno],Shr",i,i,k
	fnopenprn
	on pageoflow goto PGOF
	gosub HDR
READ_CUSTOMER: !
	read #customer,using 'Form POS 1,C 10,pos 41,C 30,POS 1859,PD 5.2': z$, customer_name$, escrow_bal eof DONE
	if escrow_bal=0 then goto READ_CUSTOMER
	pr #255,using 'Form POS 1,C 12,C 30,N 12.2': z$,customer_name$,escrow_bal
	total_escrow+=escrow_bal
	goto READ_CUSTOMER
 
PGOF: pr #255: newpage : gosub HDR : continue
 
HDR: !
	pr #255,using 'Form POS 20,Cc 40': "",cnam$
	pr #255,using 'Form POS 1,C 10,pos 20,Cc 40': "Page "&str$(pg+=1),cap$
	pr #255,using 'Form POS 1,C 10,pos 20,Cc 40': date$,resp$(1)
	pr #255: ""
	pr #255: "Account No  Customer Name                   Escrow Bal"
	pr #255: "__________  ______________________________  __________"
return
 
DONE: !
	pr #255: tab(43);"  __________"
	pr #255,using 'Form POS 43,N 12.2': total_escrow
	fncloseprn
	goto Xit
 
Xit: fnXit
 
include: ertn
 
