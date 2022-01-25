! r: setup stuff
	autoLibrary
	on error goto Ertn
	dim z$*10
	dim e$(2)*30
	dim b_(11)
	dim c(4)
	dim dat$*20
	dim r(4)
	dim deposit$(4)*18
	dim totalb(11),s(4)
	dim message$*40
	dim services$(4)*16
	dim serviceName$(10)*20
	dim resp$(10)*80
	dim date_amount$*80
	
	dim filter_default$*31
	dim filter_option$(6)*31
	filter_option$(1)="[All]"
	filter_option$(2)="0 - Active"
	filter_option$(3)="1 - Inactive / Final Billed"
	filter_option$(4)="2 - Inactive / Deposit Refunded"
	filter_option$(5)="3 - Active / but Do Not Bill"
	filter_option$(6)="4 - Finaled / but Not Billed"
	
	fnTop(program$)
	fnGetServices(mat serviceName$ )
	for j=1 to 4
		serviceName$(j)=lpad$(rtrm$(serviceName$(j)),16)
	next j
	fndat(dat$,1)
	fncreg_read('ubcustdp.sequence',seq$) : seq=val(seq$)
	fncreg_read('ubcustdp.subtotal by route',subtotal$)
	fncreg_read('ubcustdp.filter choice',filter_default$)
	if filter_default$='' then filter_default$=filter_option$(1)
! /r
	fnTos
	rc=0 : mylen=30 : mypos=mylen+3
	fnLbl(1,1,"Report Heading Date:",mylen,1)
	fnTxt(1,mypos,20)
	resp$(1)=trim$(dat$)
	fnLbl(3,1,"Sequence:",mylen,1)
	fnOpt(3,mypos,"Name")
	if seq=1 then resp$(2)='True' else resp$(2)='False'
	fnOpt(4,mypos,"Route and Sequence Number")
	if seq<>1 then resp$(3)='True' else resp$(3)='False'
	fnChk(6,mylen+4,"Subtotal By Route:",1)
	if subtotal$='True' then resp$(resp_subtotal:=4)='True' else resp$(resp_subtotal:=4)='False'
	fnLbl(8,1,"Final Billing Code:",mylen,1)
	fnComboA("final_bill",8,mypos,mat filter_option$,"",25)
	resp$(resp_filter:=5)=filter_default$
	fnCmdSet(3)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	dat$=resp$(1)
	if resp$(2)='True' then seq=1 else seq=2 ! 1=name sequence  2= route sequence
	subtotal$=resp$(resp_subtotal)
	filter_choice=srch(mat filter_option$,resp$(resp_filter))
! r: save answers
	fndat(dat$,2)
	fncreg_write('ubcustdp.sequence',str$(seq))
	fncreg_write('ubcustdp.subtotal by route',subtotal$)
	fncreg_write('ubcustdp.filter choice',resp$(resp_filter))
! /r
fnopenprn
gosub Header
if seq=1 then
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\UBIndx2.h[cno],Shr",i,i,k  ! name sequence
else
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx5.h[cno],Shr",i,i,k  ! route sequence
end if
F_CUSTOMER: form pos 1,c 10,2*c 30,pos 157,11*pd 4.2,4*pd 4,pos 1741,n 2,n 7,pos 1821,n 1
do ! r: main loop
	CUSTOMER_READ: !
	holdroute=route
	read #1,using F_CUSTOMER: z$,mat e$,mat b_,mat c,route,sequence,final_billing_code eof PrintGrandTotals
	if c(3)=1 or c(3)=2 then c(3)=0 ! if deposit date previously used for final billing code, set it to zero
	if c(4)=1 or c(4)=2 then c(4)=0
	if filter_choice>1 then ! didn't select [All]
		if filter_choice=2 then ! 0 - Active
			if final_billing_code<>0 then goto CUSTOMER_READ
		else if filter_choice=3 then ! 1 - Inactive / Final Billed
			if final_billing_code<>1 then goto CUSTOMER_READ
		else if filter_choice=4 then ! 2 - Inactive / Deposit Refunded
			if final_billing_code<>2 then goto CUSTOMER_READ
		else if filter_choice=5 then ! 3 - Active / but Do Not Bill
			if final_billing_code<>3 then goto CUSTOMER_READ
		else if filter_choice=6 then ! 4 - Finaled / but Not Billed
			if final_billing_code<>4 then goto CUSTOMER_READ
		end if
	end if
	mat totalb=totalb+b_
	if seq=2 and subtotal$='True' then ! consider subtotals
		if holdroute>0 and holdroute<>route then gosub PrintSubTotals
	end if
	gosub PrintDetails
loop ! /r
PrintGrandTotals: ! r:
	pr #255: "{\b Totals:}"
	for j=1 to 4
		if trim$(serviceName$(j))<>"" then
			pr #255,using F_TOTALS: serviceName$(j),r(j)
		end if
		F_TOTALS: form pos 7,c 30,n 10.2
	next j
	close #1: ioerr ignore
	fncloseprn
goto Xit ! /r
Xit: fnXit
Header: ! r:
	p2=p2+1
	lnpg=0
	pr #255: "\qc  {\f181 \fs22 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f181 \fs28 \b "&env$('program_caption')&"}"
	pr #255: "\qc  {\f181 \fs18 \b "&trim$(dat$)&"}"
	pr #255,using L540: "\ql ","Page "&str$(p2)
	L540: form pos 1,c 5,pos 110,c 10
	pr #255: ""
	jp=0
	mat services$=("")
	for j=1 to 3
		x=pos(serviceName$(j),":",1)
		if x>0 then serviceName$(j)(x:x)=""
	next j
	date_amount$=''
	for j=1 to 4
		if j=1 and trim$(serviceName$(j))<>"Water" then goto L730
		if j=2 and trim$(serviceName$(j))<>"Sewer" then goto L730
		if j=3 and trim$(serviceName$(j))<>"Electric" then goto L730
		if j=4 and trim$(serviceName$(j))<>"Gas" then goto L730
		jp=jp+1
		p1=jp*19+40
		date_amount$&="  --Date--  Amount"
		x=pos(trim$(serviceName$(j))," ",1)
		if x=0 then x=len(serviceName$(j))
		services$(jp)=trim$(serviceName$(j))(1:x)
		L730: !
	next j
	pr #255,using 'form pos 62,4*cc 18': mat services$
	pr #255,using 'form pos 60,c 80': date_amount$
return  ! /r
PrintDetails: ! r:
	jp=0
	for j=1 to 4
		if j=1 and trim$(serviceName$(j))<>"Water" then goto L930
		if j=2 and trim$(serviceName$(j))<>"Sewer" then goto L930
		if j=3 and trim$(serviceName$(j))<>"Electric" then goto L930
		if j=4 and trim$(serviceName$(j))<>"Gas" then goto L930
		jp+=1
		p1=jp*19+40
		depdate(jp)=c(j)
		amount(jp)=b_(j+7)
		r(j)=r(j)+b_(j+7)
		s(j)=s(j)+b_(j+7)
		deposit$(jp)=cnvrt$("pic(zzz/zz/zz)",depdate(jp))&" "&cnvrt$("nz 8.2",amount(jp))
		L930: !
	next j
	mat deposit$(jp)
	if sum(amount) then
		pr #255,using L960: z$,e$(2)(1:22),e$(1)(1:22),mat deposit$ pageoflow NewPge
	end if
	L960: form c 12,2*c 23,x 2,jp*c 18
return  ! /r
PrintSubTotals: ! r:
	pr #255: "{\b Sub-totals:}"
	for j=1 to 4
		if trim$(serviceName$(j))<>"" then pr #255,using F_TOTALS: serviceName$(j),s(j)
		form pos 7,c 30,n 10.2
	next j
	mat s=(0)
return  ! /r
NewPge: ! r:
	pr #255: newpage
	gosub Header
continue  ! /r
include: ertn
