! formerly S:\acsUB\ubNoUsage
autoLibrary
on error goto Ertn
dim z$*10,e$(4)*30,resp$(10)*40,d(15)
dim serviceName$(10)*20,a(7)
fnTop(program$)
fnLastBillingDate(d1)
fnGetServices(mat serviceName$)
MAIN: ! r:
	fnTos
	mylen=20
	mypos=mylen+2
	fnLbl(2,1,"Billing Date:",mylen,1)   :	fnTxt(2,mypos,8,8,0,"1")   :	resp$(1)=str$(d1)
	fnLbl(3,1,"Route Number:",mylen,1)   :	fncmbrt2(3,mypos)          :	resp$(2)="[All]"
	fnChk(4,23,"Print Meter Address:",1) :  resp$(3)="True"
	fnCmdSet(3)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	d1 = val(resp$(1))
	if resp$(2)="[All]" then
		prtbkno=0
	else
		prtbkno=val(resp$(2))
	end if
	if resp$(3)="True" then printadr=1 ! wants meter address printed
	if d1<10100 or d1>123199 then goto MAIN
	fnopenprn
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx5.h[cno],Shr",i,i,k
	gosub HDR
	if prtbkno=0 then goto READ_CUSTOMER
	prtbkno$=lpad$(str$(prtbkno),2)&"       "
	startcd=1
	restore #1,key>=prtbkno$: nokey TOTALS
goto READ_CUSTOMER ! /r
READ_CUSTOMER: ! r:
	read #1,using L450: z$,mat e$,final,bal,f,route,mat d,mat a eof TOTALS
	L450: form pos 1,c 10,4*c 30,pos 1821,n 1,pos 292,pd 4.2,pd 4,pos 1741,n 2,pos 217,15*pd 5,pos 143,7*pd 2
	if f<>d1 then goto READ_CUSTOMER
	if a(1)>0 and trim$(serviceName$(1))="Water" and d(3)=0 then ! if have water and zero usage then list
		currentread=d(1)
		priorread=d(2)
	else if a(3)>0 and trim$(serviceName$(3))="Electric" and d(7)=0 then  ! if have electric and zero usage then list
		currentread=d(5)
		priorread=d(6)
	else if a(4)>0 and trim$(serviceName$(4))="Gas" and d(11)=0 then ! if have gas and zero usage then list
		currentread=d(9)
		priorread=d(10)
	else
		goto READ_CUSTOMER
	end if
	if startcd=1 and prtbkno<>route then goto TOTALS
	if printadr=1 then
		pr #255,using L550: z$,e$(2),currentread,priorread,e$(1)(1:25) pageoflow PgOf
		L550: form pos 1,c 10,pos 13,c 30,pos 43,n 13,x 4,n 13,x 3,c 25
	else
		pr #255,using L550: z$,e$(2),currentread,priorread pageoflow PgOf
	end if
	tbal=tbal+bal
goto READ_CUSTOMER ! /r
HDR: ! r:
	p2=p2+1
	pr #255: "\qc  {\f181 \fs18 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f181 \fs24 \b "&env$('program_caption')&"}"
	pr #255: "\qc  {\f181 \fs16 \b "&date$("Month DD, CCYY")&"}"
	pr #255,using L650: "\ql "&date$,"Page "&str$(p2)
	L650: form pos 1,c 82,c 10
	pr #255: ""
	if printadr<>1 then pr #255: " {\ul Number   }  {\ul Name             }          {\ul Current Reading}    {\ul Prior Reading}"
	if printadr=1 then pr #255: " {\ul Number   }  {\ul Name             }           {\ul Current Reading}    {\ul Prior Reading}  {\ul Meter Address}"
	pr #255: ""
return ! /r
PgOf: ! r:
	pr #255: newpage
	gosub HDR
continue ! /r
TOTALS: ! r:
! pr #255: RPT$(" ",55)&"{\ul             }"
	! pr #255,Using "Form POS 56,N 12.2": TBAL
	! pr #255: RPT$(" ",55)&"{\ul \strike             }"
goto DONE ! /r
DONE: ! r:
	close #1: ioerr ignore
	fncloseprn
goto Xit ! /r
Xit: fnXit
include: ertn
