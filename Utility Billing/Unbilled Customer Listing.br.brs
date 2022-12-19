! formerly S:\acsUB\ubUnBill
! r: setup
	autoLibrary
	on error goto Ertn
 
	dim z$*10,e$(4)*30,resp$(10)*40
 
	fnLastBillingDate(d1)
	fnTop(program$)
goto MAIN ! /r
MAIN: ! r: main screen
	fnTos(sn$:="UBUnBill")
	mylen=20
	mypos=mylen+2
	fnLbl(2,1,"Billing Date:" ,mylen,1)
	fnTxt(2,mypos,8,8,0,"1")
	resp$(1)=str$(d1)
	fnLbl(3,1,"Route Number:" ,mylen,1)
	fncmbrt2(3,mypos)
	resp$(2)="[All]"
	fnChk(4,23,"Print Meter Address:",1)
	resp$(3)='True'
	fnCmdSet(3)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
goto Initialize ! /r
Initialize: ! r:
	d1=val(resp$(1))
	if resp$(2)="[All]" then prtbkno=0 else prtbkno = val(resp$(2))
	if resp$(3)='True' then printadr=1 ! wants meter address printed
	if d1<10100 or d1>123199 then goto MAIN
	fnOpenPrn
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx5.h[cno],Shr",i,i,k
	gosub HDR
	if prtbkno=0 then goto READ_CUSTOMER
	prtbkno$=lpad$(str$(prtbkno),2)&"       "
	startcd=1
	restore #1,key>=prtbkno$: nokey TOTALS
goto READ_CUSTOMER ! /r
READ_CUSTOMER: ! r: main loop
	read #1,using L420: z$,mat e$,final,bal,f,route eof TOTALS
	L420: form pos 1,c 10,4*c 30,pos 1821,n 1,pos 292,pd 4.2,pd 4,pos 1741,n 2
	if f=d1 then goto READ_CUSTOMER
	if final=1 or final=2 then goto READ_CUSTOMER           ! skip if InActive
	if startcd=1 and prtbkno<>route then goto TOTALS
	if final=3 then final$="Final=3" else final$=""
	if final=4 then final$="Final=4"
	if printadr=1 then pr #255,using L490: z$,e$(2),f,bal,e$(1)(1:25),final$ pageoflow PgOf else pr #255,using L491: z$,e$(2),f,bal,final$ pageoflow PgOf
	L490: form pos 1,c 10,pos 13,c 30,pos 45,pic(zz/zz/zz),n 15.2,x 2,c 25,x 2,c 8
	L491: form pos 1,c 10,pos 13,c 30,pos 45,pic(zz/zz/zz),n 15.2,x 2,c 8
	tbal=tbal+bal
goto READ_CUSTOMER ! /r
TOTALS: ! r:
	pr #255: rpt$(" ",55)&"{\ul             }"
	pr #255,using "form pos 56,N 12.2": tbal
	pr #255: rpt$(" ",55)&"{\ul \strike             }"
goto DONE ! /r
HDR: ! r:
	p2=p2+1
	pr #255: "\qc  {\f181 \fs18 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f181 \fs24 \b "&env$('program_caption')&"}"
	pr #255: "\qc  {\f181 \fs16 \b "&date$("Month DD, CCYY")&"}"
	pr #255,using L590: "\ql "&date$,"Page "&str$(p2)
	L590: form pos 1,c 82,c 10
	pr #255: ""
	! pr #255: "{\ul Account    }                               {\ul Date of}         Current"
	if printadr<>1 then pr #255: " {\ul Number   }  {\ul Name             }             {\ul Last Billing}      {\ul Balance}"
	if printadr=1 then pr #255: " {\ul Number   }  {\ul Name             }             {\ul Last Billing}      {\ul Balance}  {\ul Meter Address}"
	pr #255: ""
return ! /r
PgOf: ! r:
	pr #255: newpage
	gosub HDR
continue ! /r
DONE: ! r:
	close #1: ioerr ignore
	fnClosePrn
goto Xit ! /r
Xit: fnXit
include: ertn
