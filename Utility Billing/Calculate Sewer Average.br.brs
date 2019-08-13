! formerly S:\acsUB\Sewer ! dont forget to change lines 510 &  640
library 'S:\Core\Library': fntop,fnxit, fnAcs,fnwait,fnopenprn,fncloseprn,fnerror,fnmsgbox,fnTxt,fnLbl,fnTos,fngethandle,fncreg_read,fncreg_write,fnCmdKey,fnapply_default_rates
dim cd1(8),x(13),message$(5)*80,message$*60,tg(11),extra(23)
on error goto ERTN
fntop(program$)

open #h_trans:=fngethandle: "Name=[Q]\UBmstr\ubTransvb.h[cno],KFName=[Q]\UBmstr\ubTrIndx.h[cno],Shr",internal,input,keyed 
open #h_customer:=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,outIn,keyed 
read #h_customer,using L500: x$,customer_sewer_rate_code,oldavg eof DONE
gosub APPLY_DEFAULT_RATE
restore #h_trans,key>=x$&"         ": nokey L220
L160: ! 
read #h_trans,using F_TRANS: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof L220
if p$<>x$ then goto L220 ! history record must belong to this customer
if tcode<>1 then goto L160 ! charge transaction
j=j+1
if j>8 then goto L220
resp$(j)=str$(tdate)
goto L160
L220: ! 
restore #h_customer: 

SCR1: ! 
	fnTos(sn$:="ubsewer-1b")
	mylen=47 : mypos=49
	fnLbl(1,1,"Billing Dates for Months to Average:",mylen,1)
	for j=1 to 8
		fnTxt(j,mypos,10,0,0,"3")
		fncreg_read(sn$&'.billing date.'&str$(j),resp$(j))
	next j
	fnLbl(10,1,"Sewer code to average:",mylen,1)
	fnTxt(10,mypos,2,2,0,"20")
	fncreg_read(sn$&'.sewer code to average',resp$(9))
	fnCmdKey("&Clear Sewer Code Averages",3,0)
	fnCmdKey("&Next",1,1)
	fnCmdKey("&Cancel",5,0,1)
	fnAcs(sn$,0,mat resp$,ckey)
	if ckey=5 then goto XIT
	if ckey=3 then clear_averages=1 else clear_averages=0
	for j=1 to 8
L310: ! 
	x=pos(resp$(j),"/",1)
	if x>0 then 
		resp$(j)(x:x)=""
		goto L310
	end if 
next j
filter_sewer_code=val(resp$(9)) conv SCR1
if filter_sewer_code=0 and ~clear_averages then 
	mat message$(1)
	message$(1)="You must enter at least one date!"
	fnmsgbox(mat message$,resp$,'',0)
	goto SCR1
end if 
for j=1 to 8
	cd1(j)=val(resp$(j)) conv SCR1
	fncreg_write(sn$&'.billing date.'&str$(j),resp$(j))
next j
fncreg_write(sn$&'.sewer code to average',resp$(9))
if cd1(1)=0 and ~clear_averages then 
	mat message$(1)
	message$(1)="You must enter at least one date!"
	fnmsgbox(mat message$,resp$,'',0)
	goto SCR1
end if 

fnopenprn
message$="Calculating: please wait..."
fnwait(message$,1)
gosub HDR
L480: ! 
read #h_customer,using L500: x$,customer_sewer_rate_code,oldavg eof DONE
gosub APPLY_DEFAULT_RATE
if customer_sewer_rate_code<>filter_sewer_code then goto L480 ! only average certain rate codes
L500: form pos 1,c 10,pos 145,pd 2,pos 1822,n 9
! r: calculate average
	if clear_averages then 
		t3=0
	else 
		t1=t2=t3=x=0
		mat x=(0)
		restore #h_trans,key>=x$&"         ": nokey L480
		READ_TRANS: ! 
		read #h_trans,using F_TRANS: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof EO_TRANS
		F_TRANS: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
		if p$<>x$ then goto EO_TRANS
		if tcode<>1 then goto READ_TRANS ! only charge transactions
		! if trim$(x$)='306100.00' then pause
		for j1=1 to 8
			if cd1(j1)=tdate then 
				t1=t1+1
				t2=t2+wu
				x=x+1
				x(x)=wu
				goto READ_TRANS
			end if 
		next j1
		goto READ_TRANS

	EO_TRANS: ! 
		if t1>0 then t3=int((t2+.5)/t1) else t3=0
		if env$('client')="Monticello" and t3=0 then goto L670 ! skip record if no dates found  (multiple cycles)
		if t1=1 and env$('client')="Monticello" then t3=8 ! if only one month use 8
	end if 
	rewrite #h_customer,using "Form POS 1822,N 9": t3
	pr #255,using L660: x$,oldavg,t3,x(1),x(2),x(3),x(4) pageoflow PAGE
	L660: form pos 1,c 12,6*nz 9
	L670: ! 
goto L480 ! /r

DONE: ! 
	close #h_customer: 
	fncloseprn
XIT: fnxit

PAGE: ! r:
	pr #255: newpage
	gosub HDR
continue ! /r
HDR: ! r:
	p1=p1+1
	pr #255,using "Form POS 20,CC 40,POS 70,C 5,N 4": env$('cnam'),"Page ",p1
	pr #255,using "Form POS 20,CC 40": "Calculate Sewer Average"
	pr #255,using "Form POS 20,CC 40": "Sewer Averages for Sewer Code "&ltrm$(str$(filter_sewer_code))
	pr #255: ""
	pr #255: " Acct.Num.    Old Avg  New Avg "&cnvrt$("pic(zzzzz/zz/zz)",cd1(1))&cnvrt$("pic(zzzzz/zz/zz)",cd1(2))&cnvrt$("pic(zzzzz/zz/zz)",cd1(3))&cnvrt$("pic(zzzzz/zz/zz)",cd1(4))
	! pr #255: "__________    _______  _______ ___________ ___________ ___________ ___________"
return ! /r
APPLY_DEFAULT_RATE: ! r:
	a(2)=customer_sewer_rate_code
	fnapply_default_rates(mat extra, mat a)
	customer_sewer_rate_code=a(2)
return ! /r
include: ertn no
