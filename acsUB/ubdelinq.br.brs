! Replace S:\acsUB\UBdelinq
! Past Due Balance Breakdown
 
	autoLibrary
	on error goto Ertn
 
	dim z$*10,e$*30,g(12),cap$*128,resp$(10)*60
	dim serviceName$(10)*20,hdr$*230,detail(11),t(11),gb(10),a$*20
	dim service$(10)*2,tax_code$(10)*1,penalty$(10)*1
	dim srv$(10)*2
 
	fnLastBillingDate(lbill)
	fnTop(program$,cap$="Past Due Balance Breakdown")
! need to build headings from this information
	fnGetServices(mat serviceName$, mat srv$, mat tax_code$,mat penalty$)
	fncreg_read('Route Low',bkno1$) : bkno1=val(bkno1$)
	fncreg_read('Route High',bkno2$) : bkno2=val(bkno2$)
	if trim$(serviceName$(3))<>"Electric" and trim$(srv$(3))="EL" then needelecused=1
	if trim$(serviceName$(4))<>"Gas" and trim$(srv$(4))="GA" then needgasused=1
	x=0
	hdr$="{\ul Account No}  {\ul Customer Name }       "
	for j=1 to 10
		if j=3 and needelecused=1 then goto L200
		if j=4 and needgasused=1 then goto L200
		if trim$(serviceName$(j))<>"" then : _
			hdr$=hdr$&" {\ul "&lpad$((trim$(serviceName$(j)(1:9))),9)&"}" : _
			services+=1
L200: next j
	mat detail(services+1)
	mat t(services+1)
 
MENU1: !
	fnTos(sn$="ubdelinq")
	fnLbl(1,1,"As of Date:",19,1)
	fnTxt(1,21,8,8,0,"1001") : _
	if d1<>0 then resp$(1)=str$(d1) else resp$(1)=date$("mmddyy")
	fnLbl(2,1,"Last Billing Date:",19,1)
	fnTxt(2,21,8,0,0,"1001") : _
	resp$(2)=str$(lbill)
	fnChk(3,1,"Skip customers who only owe current bill") : _
	resp$(3)="False"
	fnChk(4,1,"Skip customers with credit balance") : _
	resp$(4)="False"
	fnChk(5,1,"Only show past due amounts (not current month)") : _
	resp$(5)="True"
	fnChk(6,1,"Skip accounts with Zero balances") : _
	resp$(6)="True"
	fnCmdSet(3)
L350: fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	d1=val(resp$(1)) : _
	lbill=val(resp$(2))
	if uprc$(resp$(3))=uprc$("True") then skipcurrent=1
	if uprc$(resp$(4))=uprc$("True") then skipcredits=1
	if uprc$(resp$(5))=uprc$("True") then pastdueonly=1
	if uprc$(resp$(6))=uprc$("True") then skipzero=1
	if lbill<10100 or lbill>123199 then goto L350
	if d1<10100 or d1>123199 then goto L350
	d7=int(d1/10000)
	d6=d7*10000
	d5=int((d1-d6)/100)
	d8=d1-(d7*10000+d5*100)
	if d7<1 or d7>12 then goto L350
	if d5<1 or d5>31 then goto L350
 
	on fkey 5 goto DONE
	fnopenprn
	gosub HEADER
	v=bkno1
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed
L570: read #1,using L580: z$,e$,f,bal,mat g,mat gb eof DONE
L580: form pos 1,c 10,pos 41,c 30,pos 296,pd 4,pos 292,pd 4.2,pos 300,12*pd 4.2,pos 388,10*pd 5.2
	if f<>lbill then mat g=(0)
	if skipcurrent=1 and bal-g(11)=<0 then goto L570 : _
		! skip anyone who only owes last times bill
	if skipzero=1 and bal=0 then goto L570 : _
		! skip Zero Balance Accounts
	if skipcredits=1 and bal<=0 then goto L570 : _
		! skip credit balance accounts
	if pastdueonly=1 then goto L640 else goto L690
L640: for j=1 to 10 ! subtract current bill out of balance breakdown
		if penalty$(j)="Y" then goto L680 ! skip penalties
		if gb(j)=0 and j<10 then g(j+1)=g(j+1)+g(j) : g(j)=0 ! try to prevent negative amounts in columns
		gb(j)=gb(j)-g(j)
L680: next j
L690: x=0
	if pastdueonly=1 and skipzero=1 and sum(gb)=0 then goto L570 ! if only owe the current bill then consider it zero for the skipzero test and pastdueonly test
	for j=1 to 10
		if j=3 and needelecused=1 then goto L730
		if j=4 and needgasused=1 then goto L730
		if trim$(serviceName$(j))<>"" then detail(x+=1)=gb(j)
L730: next j
	detail(x+1)=sum(gb)
	pr #255,using L760: z$,e$(1:20),mat detail pageoflow L990
L760: form pos 1,c 12,c 21,11*n 10.2
	mat t=t+detail
	goto L570
 
HEADER: !
	pr #255: "\qc  {\f181 \fs22 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f181 \fs22 \b "&env$('program_caption')&"}"
	pr #255: "\qc {\f181 \fs18 \b As of "&cnvrt$("pic(zz/zz/zz)",d1)&"}"
	pagetab=41+services*10
	pr #255,using L860: "\ql "&date$,"Page "&str$(p2+=1)
L860: form pos 1,c 20,pos pagetab,c 10
	pr #255: hdr$&" {\ul     Total}"
return
 
PRINT_TOTALS: !
	pr #255: "" : _
	pr #255: "" : _
	pr #255,using L760: "","****** Grand Totals",mat t
return
 
DONE: close #1: ioerr L960
	gosub PRINT_TOTALS
L960: fncloseprn
Xit: fnXit
 
L990: pr #255: newpage
	gosub HEADER
	continue
 
include: ertn
 
