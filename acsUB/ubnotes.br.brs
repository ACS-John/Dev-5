autoLibrary
on error goto Ertn

dim e$(4)*30
dim resp$(2)*40,txt$*40,a$*1,line$*90

fnTop(program$,"Notes")
fnLastBillingDate(d1)
SCR1: ! 
	fnTos
	respc=0
	fnLbl(1,1,"Route Number:",31,1)
	fncmbrt2(1,33) 
	resp$(respc+=1)="1"
	fnLbl(2,1,"Billing Date (Blank for all):",31,1)
	fnTxt(2,33,8,0,0,"1") 
	resp$(respc+=1)=str$(d1)
	fnCmdSet(3) 
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	if resp$(1)="[All]" then rt1=0 else rt1=val(resp$(1))
	d1=val(resp$(2))
!
	open #h_customer:=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx5.h[cno],Shr",internal,input,keyed 
	fnopenprn
	gosub HEADER

	if rt1 then
		bk$=lpad$(str$(rt1),2)
		restore #h_customer,key>=bk$&"       ": nokey SCR1
	end if 
READ_CUSTOMER: !
	read #h_customer,using 'form pos 1,c 10,4*c 30,pos 296,pd 4,pos 1741,n 2,n 7': z$,mat e$,f,route,sequence eof DONE
	if rt1>0 and route<>rt1 then goto DONE
	if d1<>0 and f><d1 then goto READ_CUSTOMER
	close #31: ioerr ignore
	open #31: "Name=[Q]\UBmstr\notes.h[cno]\"&trim$(z$)&".txt",display,input ioerr READ_CUSTOMER
	pr #255: "" 
	pr #255: "{\b "&rpad$(trim$(z$),10)&"  Street Adr: "&e$(1)&"  Name: "&e$(2)&"}"
	do
		linput #31: line$ eof READ_CUSTOMER
		pr #255: line$ pageoflow PGOF
	loop

DONE: ! 
	fncloseprn
Xit: fnXit
HEADER: ! r:
	p2=p2+1
	pr #255: "\qc  {\f181 \fs22 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f181 \fs28 \b "&env$('program_caption')&"}"
	pr #255: "\qc  {\f181 \fs18 \b "&date$("Month DD, CCYY")&"}"
	pr #255,using 'form pos 1,c 82,c 10': "\ql "&date$,"Page "&str$(p2)
return ! /r
PGOF: ! r:
	pr #255: newpage
	gosub HEADER
continue ! /r
include: Ertn
