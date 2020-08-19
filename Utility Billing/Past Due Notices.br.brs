! formerly S:\acsUB\ubPdNot
! Past Due Notices
autoLibrary
on error goto Ertn
! r: dims
	dim z$*10
	dim meter_address$*30
	dim gb(10)
	dim d$(4)*20
	dim f$(3)*12,a(7)
	dim xb(11),c(4),d(15),g(12)
	dim resp$(15)*512
	dim ln$*8800,flname$*256
	dim r1$(120)*30
	dim extra$(11)*30,extra(23)
	dim tmp_rtf_filename$*1024
! /r
! r: top of programs, constants,initial setup, etc
	fnTop(program$)
	tmp_rtf_filename$=fnPrintFileName$
	if env$('client')='French Settlement' or env$('client')='Granby' then hard_coded=1
	fnLastBillingDate(d1)
	fndat(d$(4))
	dim at$(3)*40
	open #21: "Name=[Q]\UBmstr\Company.h[cno],Shr",internal,input
	read #21,using "Form POS 1,3*C 40": at$(1),at$(2),at$(3)
	close #21:
	z=21
	at$(1)=trim$(at$(1))(1:z)
	x=len(at$(1)) : y=z-x
	at$(1)=rpt$(" ",int(y/2))&at$(1)
	z=26
	for j=2 to udim(at$)
		at$(j)=trim$(at$(j))(1:z)
		x=len(at$(j)) : y=z-x
		at$(j)=rpt$(" ",int(y/2))&at$(j)
	next j

	deltype=0 : fnreg_read('UB - Past Due Notices - Delinquent Type',deltype$) : deltype=val(deltype$) conv ignore

	open #adrbil=3: "Name=[Q]\UBmstr\UBADRBIL.H[cno],KFName=[Q]\UBmstr\AdrIndex.h[cno],Shr",internal,input,keyed
	open #customer5=11: "Name=[Q]\UBmstr\Customer.H[cno],KFName=[Q]\UBmstr\UBINDx5.H[cno],Shr",internal,input,keyed
	open #customer1=fngethandle: "Name=[Q]\UBmstr\Customer.H[cno],KFName=[Q]\UBmstr\UBIndex.H[cno],Shr",internal,input,keyed
	F_CUSTOMER: form pos 1,c 10,c 30,x 90,c 12,pos 361,2*c 12,pos 143,7*pd 2,11*pd 4.2,4*pd 4,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,10*pd 5.2,pos 1741,n 2,pos 1821,n 1,pos 1741,n 2,n 7,2*n 6,n 9,pd 5.2,n 3,3*n 9,3*n 2,3*n 3,n 1,3*n 9,3*pd 5.2,c 30,7*c 12,3*c 30
goto MENU1 ! /r
MENU1: ! r:
	fnTos
	respc=0
	fnLbl(2,1,"@D1=Last Billing Date (mmddyy):",38,1)
	fnTxt(2,40,8,0,1,"1")
	resp$(respc+=1)=str$(d1)
	fnLbl(3,1,"@D2= Payment Due Date (mmddyy):",38,1)
	fnTxt(3,40,8,0,1,"1")
	resp$(respc+=1)=str$(d2)
	fnLbl(4,1,"@D3=    Shut Off Date (mmddyy):",38,1)
	fnTxt(4,40,8,0,1,"1")
	resp$(respc+=1)=str$(d3)
	fnLbl(5,1,"@D4=            Date of Notice:",38,1)
	fnTxt(5,40,20,20)
	resp$(respc+=1)=d$(4)
	fnLbl(6,1,"Minimum balance required:",38,1)
	fnTxt(6,40,10,10,0,"10")
	resp$(respc+=1)=""
	fnFra(8,5,6,50,"Print all customers who:")
	fnOpt(1,1,"Have not paid their current bill",0,1)
	if deltype<=1 then resp$(respc+=1)="True" else resp$(respc+=1)="False"
	fnOpt(2,1,"Have not paid their prior month's bill",0,1)
	if deltype=2 then resp$(respc+=1)="True" else resp$(respc+=1)="False"
	fnOpt(3,1,"Are Active",0,1)
	if deltype=3 then resp$(respc+=1)="True" else resp$(respc+=1)="False"
	fnOpt(4,1,"Are Final Billed Customers and have not paid",0,1)
	if deltype=4 then resp$(respc+=1)="True" else resp$(respc+=1)="False"
	fnOpt(5,1,"Have a balance",0,1)
	if deltype=5 then resp$(respc+=1)="True" else resp$(respc+=1)="False"
	fnOpt(6,1,"Who Were Billed This Month",0,1)
	if deltype=6 then resp$(respc+=1)="True" else resp$(respc+=1)="False"
	fnCmdSet(2)
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	d$(1)=cnvrt$("PIC(##/##/##)",d1=val(resp$(1)))
	d$(2)=cnvrt$("PIC(##/##/##)",d2=val(resp$(2)))
	d$(3)=cnvrt$("PIC(##/##/##)",d3=val(resp$(3)))
	d$(4)=rtrm$(d$(4)=resp$(4))
	minbal=val(resp$(5))
	for a=6 to 11
		if uprc$(resp$(a))=uprc$("True") then deltype=a-5
		resp$(a)=""
	next a
	fnreg_write('UB - Past Due Notices - Delinquent Type',str$(deltype))
goto UBFORM ! /r

PRINT_NEXT: ! r: the main read it and pr it routine
	if sel_indv$="Y" then goto ASK_NEXT_ACT
	if bk1>0 then
		read #customer5,using F_CUSTOMER: z$,meter_address$,mat f$,mat a,mat xb,mat c,mat d,bal,f,mat g,bra,mat gb,route,final,mat extra,mat extra$ eof EO_CUSTOMER
		if route>bk1 then goto EO_CUSTOMER
	else
		read #customer1,using F_CUSTOMER: z$,meter_address$,mat f$,mat a,mat xb,mat c,mat d,bal,f,mat g,bra,mat gb,route,final,mat extra,mat extra$ eof EO_CUSTOMER
	end if
	if deltype<3 and bal<=1 then goto PRINT_NEXT
	if bal<minbal and minbal>0 then goto PRINT_NEXT ! skip if under minimum balance
	! pr f "1,1,Cc 80,R,N": str$(rec(customer5))&"/"&str$(lrec(customer5))
	if deltype=3 and final=0 then goto READ_ADRBIL ! pr ALL ACTIVE CUSTOMERS
	if deltype=4 and final>0 and bal>0 then goto READ_ADRBIL ! pr ALL INACTIVE CUSTOMERS WITH BAL
	! IF UPRC$(NEWBIL$)="Y" AND F=D1 AND BAL=<G(11) THEN GOTO 440
	if deltype=1 and f=d1 and bal>0 then goto READ_ADRBIL ! pr ALL CUSTOMERS WHO HAVE NOT PAID THEIR MOST CURRENT BILL
	if deltype=2 and f=d1 and bal>g(11) then goto READ_ADRBIL ! pr all customers who owe more than last times bill
	! IF DELTYPE=2 AND F<>D1 AND BAL>0 THEN GOTO READ_ADRBIL ! pr all customers who owe a prior bill but didn't get billed this time
	if deltype=5 and bal>0 then goto READ_ADRBIL
	if deltype=6 and f=d1 then goto READ_ADRBIL ! pr all customers who were billed last billing cycle
goto PRINT_NEXT ! /r
	
READ_ADRBIL: ! r:
	dim addr$(4)*30
	fncustomer_address(z$,mat addr$)

	if reminder=1 then
		fn_vbprint
	else if env$('client')="Granby" then
		fn_print_granby
	else if env$('client')="French Settlement" then
		fn_french_settlement_gas
	else if env$('client')="Blucksberg" then
		fn_print_blucksberg(mat a,mat at$,mat mis$,mat f$,meter_address$,z$,mat addr$,bal,d1)
	! else if env$('client')="Merriam Woods" then
	!   fn_merriam_woods
	else if do_print_std_form=1 then
		fn_print_standard_form
	else
		fn_prnt1
	end if
	fn_report_add
	! fn_listFile_add(z$)
goto PRINT_NEXT ! /r
def fn_open_template
	if ~h_template then
		open #h_template=fngethandle: "Name=[Q]\UBmstr\"&flname$&",RecL=1",external,input,relative
	end if
	fn_open_template=h_template
fnend  ! fn_open_template
def fn_prnt1
	if ~h_prnt1 then
		open #h_prnt1=fngethandle: "Name="&tmp_rtf_filename$&",eol=none,Replace",display,output ! env$('at')&
	end if
	fn_bldr1
	r=0
	dim ln3$*1
	P1_NEXT_LN: !
	ln$=""
	do
		read #h_template,using "Form POS 1,C 1",rec=r+=1: ln3$ eof P1_END1 noRec P1_END1
		if ln3$=chr$(13) then goto P1_L2310
		ln$=ln$&ln3$
		if len(rtrm$(ln$))>3900 then
			pr #h_prnt1: ln$
			goto P1_NEXT_LN
		end if
	loop
	P1_L2310: !
	dim l2$*8800
	p3=len(ln$) : l2$="" : p1=0
	P1_L2320: !
	p2=pos(ln$,"@",p1)
	if p2=0 then goto P1_L2570
	if p1>len(l2$) then l2$=rpad$(l2$,p1)
	l2$=l2$&ln$(p1:p2-1) : p4=pos(ln$," ",p2)
	if p4=0 then p4=p3 else p4=p4-1
	P1_L2370: !
	if ln$(p4:p4)="." or ln$(p4:p4)="," or ln$(p4:p4)=":" or ln$(p4:p4)="\" or ln$(p4:p4)=";" then p4=p4-1 : goto P1_L2370
	if ln$(p2+2:p2+5)="\par" then p4=p2+1 ! if they don't space after the variable at the end of a line, it doesn't pr the variable
	if ln$(p2+3:p2+6)="\par" then p4=p2+2
	if ln$(p2+4:p2+7)="\par" then p4=p2+3
	if ln$(p2+5:p2+8)="\par" then p4=p2+3
	if ln$(p2+6:p2+9)="\par" then p4=p2+3
	v1=val(ln$(p2+1:p4)) conv P1_L2480
	if v1<>0 and v1<=udim(mat r1$) then l2$=l2$&r1$(v1)
	P1_L2450: !
	p1=p4+1
	goto P1_L2320

	P1_L2480: !
	! if uprc$(ln$(p2+1:p2+2))><"B4" then
	! end if
	if uprc$(ln$(p2+1:p2+1))><"D" then goto P1_L2550
	v1=val(ln$(p2+2:p2+2)) conv P1_L2550
	if v1<1 or v1>4 then goto P1_L2550
	l2$=l2$&d$(v1)
	P1_L2550: !
	goto P1_L2450

	P1_L2570: !
	l2$=l2$&ln$(p1:p3)
	pr #h_prnt1: l2$&chr$(13)
	goto P1_NEXT_LN

	P1_END1: !
	restore #h_template:
	pr #h_prnt1: "\page"
fnend
def fn_bldr1 ! BUILD RECORD IN pr ARRAY
	! if trim$(z$)='901246.40' then pr 'the beginning of it' : pause
	mat r1$=("")
	r1$(1)  =ltrm$(z$                  )
	r1$(2)  =rtrm$(meter_address$      ) ! meter address
	r1$(3)  =rtrm$(addr$(1)            ) ! name
	r1$(4)  =rtrm$(addr$(2)            ) ! rtrm$(e$(3)) ! address
	r1$(108)=rtrm$(addr$(3)            ) ! address
	r1$(5)  =rtrm$(addr$(4)            ) ! city st zip
	for j=1 to  3 : r1$(j+5)=f$(j)                                : next j
	for j=1 to  7 : r1$(j+9)=str$(a(j))                           : next j
	for j=1 to 11 : r1$(j+16)=ltrm$(cnvrt$("N 8.2",xb(j)))        : next j
	for j=1 to  4 : r1$(j+27)=ltrm$(cnvrt$("PIC(ZZ/ZZ/ZZ)",c(j))) : next j
	for j=1 to 15 : r1$(j+31)=ltrm$(cnvrt$("N 10",d(j)))          : next j
	r1$(47)=ltrm$(cnvrt$("N 10.2",bal))
	r1$(49)=ltrm$(cnvrt$("PIC(##/##/##)",f))
	for j=1 to 12 : r1$(j+49)=cnvrt$("N 10.2",g(j))               : next j
	for j=1 to 10 : r1$(j+61)=cnvrt$("N 10.2",gb(j))              : next j
	r1$(72)=cnvrt$("n 10.2",bal+(g(12)-g(11))) ! balance plus penalty (assume total penalties will be difference in gross and net bill)
	r1$(73)=ltrm$(cnvrt$("n 10.2",bal+(g(12)-g(11)))) ! balance plus penalty trimmed
	if env$('client')="White Hall" then r1$(73)=ltrm$(cnvrt$("n 10.2",bal+10)) ! balance plus $10 penalty trimmed
	r1$(74)=cnvrt$("n 10.2",bal-g(11)) ! past due balance
	r1$(75)=ltrm$(cnvrt$("n 10.2",bal-g(11))) ! past due balance trimed
	r1$(76)=cnvrt$("N 10.2",bal) ! balance not trimed
	r1$(77)=ltrm$(cnvrt$("PIC(##/##/##)"  ,extra(3)  )  ) ! date read current
	if balance <=0 then
		r1$(78)=ltrm$(cnvrt$("pic(zzzzzzz.##",0))           ! pay after amount (balance plus penalty trimmed) nothing if balance <0
	else
		r1$(78)=ltrm$(cnvrt$("pic(zzzzzzz.##",max(0,bal+(g(12)-g(11)))))
	end if
	r1$(80)=df$                                           ! bank draft Y
	r1$(81)=da$                                           ! bank Account
	r1$(82)=dc$                                           ! Account code
	r1$(83)=dc$                                           ! bank acct #
	r1$(84)=trim$(cnvrt$("N 2"            ,extra(1)  )  ) ! route number
	r1$(85)=trim$(cnvrt$("N 7"            ,extra(2)  )  ) ! sequence #
	r1$(86)=ltrm$(cnvrt$("PIC(ZZ/ZZ/ZZ)"  ,extra(3)  )  ) ! current reading date
	r1$(87)=ltrm$(cnvrt$("PIC(ZZ/ZZ/ZZ)"  ,extra(4)  )  ) ! prior   reading date
	r1$(88)=ltrm$(cnvrt$("PIC(zZzZZZZ)"   ,extra(5)  )  ) ! sewer rediction
	r1$(89)=ltrm$(cnvrt$("n 10.2"         ,extra(6)  )  ) conv ignore ! security light charge
	r1$(90)=ltrm$(cnvrt$("PIC(ZZzZZzZZ)"  ,extra(7)  )  ) ! security light count
	r1$(91)=ltrm$(cnvrt$("PIC(ZZZzZZzZZ)" ,extra(8)  )  ) ! electric multiplier
	r1$(92)=ltrm$(cnvrt$("PIC(ZZZzZZzZZ)" ,extra(9)  )  ) ! demand average usage
	r1$(93)=ltrm$(cnvrt$("PIC(ZZZzZZzZZ)" ,extra(10) )  ) ! gas multiplier
	r1$(94)=ltrm$(cnvrt$("PIC(ZZZzZZzZZ)" ,extra(11) )  ) ! service 6 rate code
	r1$(95)=ltrm$(cnvrt$("PIC(ZZZzZZzZZ)" ,extra(12) )  ) ! service 7 rate code
	r1$(96)=ltrm$(cnvrt$("PIC(ZZZzZZzZZ)" ,extra(13) )  ) ! service 8 rate code
	r1$(97)=ltrm$(cnvrt$("PIC(zZZ)"       ,extra(14) )  ) ! units per meter sewer
	r1$(98)=ltrm$(cnvrt$("PIC(zZZ)"       ,extra(15) )  ) ! units per meter electric
	r1$(99)=ltrm$(cnvrt$("PIC(zZZ)"       ,extra(16) )  ) ! units per meter gas
	! r1$(100)  skipped
	r1$(101)=ltrm$(cnvrt$("PIC(ZZ/zz/zz)" ,extra(17)))  ! final billing date
	r1$(102)=ltrm$(cnvrt$("PIC(ZZzzzzzzz)",extra(18))) ! average sewer usage
	r1$(103)=ltrm$(cnvrt$("PIC(ZZ/zz/zz)" ,extra(19)))  ! estimated date
	extra(20)=0: r1$(104)=ltrm$(cnvrt$("PIC(ZZzzzzzz)",extra(20))) ! extra
	extra(21)=0: r1$(105)=ltrm$(cnvrt$("PIC(ZZzzzzzz)",extra(21))) ! extra
	extra(22)=0: r1$(106)=ltrm$(cnvrt$("PIC(ZZzzzzzz)",extra(22))) ! extra
	if extra(23)<-10000 then extra(23)=0
	! r1$(107)=LTRM$(CNVRT$("PIC(n 10.2)",EXTRA(23))) ! escrow balance
	!   if trim$(extra$(1))<>"" then r1$(108)=extra$(1)
	for j=109 to 117
		r1$(j)=trim$(extra1$(j-107)) ! escrow balance thru end
	next j
	! if trim$(z$)='901246.40' then pr 'the end of it' : pause
fnend
def fn_vbopenprint
	fnpa_open
	lyne=3
	spacer=0
fnend  ! fn_vbopenprint
def fn_vbprint
	fnpa_fontbold(1)
	fnpa_fontsize(16)
	fnpa_font
	fnpa_txt(at$(1),10,lyne*4+spacer)
	fnpa_font("Lucida Console")
	fnpa_fontsize(12)
	fnpa_fontbold
	fnpa_txt(at$(2),10,lyne*6.5+spacer)
	fnpa_txt(at$(3),10,lyne*8+spacer)
	fnpa_fontbold(1)
	fnpa_fontsize(12)
	fnpa_line(115,lyne*12+spacer,75, 30,1)
	fnpa_txt("A Friendly Reminder....",100,lyne+spacer)
	fnpa_fontsize
	fnpa_fontbold
	fnpa_txt('If your check has already been mailed,please',100,lyne*3+spacer)
	fnpa_txt('disregard this notice.  If not, your remittance by mail',100,lyne*4+spacer)
	fnpa_txt('will be greatly appreciated.',100,lyne*5+spacer)
	fnpa_txt('Thank You!',150,lyne*7+spacer)
	fnpa_txt('Customer No: '&z$,125,lyne*14+spacer)
	fnpa_txt('Billing Date: '&cnvrt$("PIC(zZZ/ZZ/ZZ)",d1),125,lyne*16+spacer)
	fnpa_txt('Balance Due: '&cnvrt$("pic(---,---.##)",bal),125,lyne*18+spacer)
	fnpa_fontsize(13)
	fnpa_txt(addr$(2),20,lyne*16+spacer)
	fnpa_txt(addr$(3),20,lyne*17.5+spacer)
	fnpa_txt(addr$(3),20,lyne*19+spacer)
	fnpa_txt(addr$(4),20,lyne*20.5+spacer)
	checkcounter+=1
	spacer+=90
	if checkcounter=3 then
		fnpa_newpage
		checkcounter=0
		spacer=0
	end if  ! checkcounter=3
	fn_report_add
fnend
def fn_report_close
	if h_ra then
		dim ra_line$*256
		close #h_ra:
		open #h_ra: 'Name=[temp]\ubpdnot_summary_s[session].txt,RecL=256',display,input
		fnopenprn( 'Summary')
		gosub RC_HDR
		do
			linput #h_ra: ra_line$ eof RC_DONE
			pr #255: rtrm$(ra_line$) pageoflow RC_PGOF ! ,using 'form pos 1,c 256'
		loop
		RC_DONE: !
		fncloseprn
		close #h_ra,free:
		h_ra=0
	end if  ! h_ra
	goto RC_XIT
	RC_PGOF: ! r:
		pr #255: newpage
		gosub RC_HDR
	continue  ! /r
	RC_HDR: ! r:
		rc_page+=1
		pr #255: "\qc "&cnam$
		pr #255: "\qc  {\f181 \fs28 \b "&env$('program_caption')&"}"
		pr #255,using "form pos 1,c 70,cr 14": "\ql "&date$,"Page "&str$(rc_page)
		pr #255: "{\ul Account No}  {\ul Customer Name            }  {\ul       Balance}  {\ul  Meter Address  }"
	return  ! /r
	RC_XIT: !
fnend 
def fn_listFile_add(z$)
	if ~h_lf then
		open #h_lf:=fngethandle: 'Name=[temp]\pastDueNoticesLastPrintedAccounts.txt,RecL=10,replace',display,output
	end if
	pr #h_lf,using 'form pos 1,c 10': z$
fnend
def fn_report_add
	if ~h_ra then
		open #h_ra:=fngethandle: 'Name=[temp]\ubpdnot_summary_s[session].txt,RecL=256,replace',display,output
		rc_page=0
	end if  ! ~h_ra
	pr #h_ra,using 'form pos 1,c 256': z$&'  '&addr$(1)&cnvrt$("pic(---,---.##)",bal)&'  '&meter_address$&'  '

fnend
def fn_print_standard_form ! used by Blucksberg Mtn Water, possibly others
	if a(1)=0 then water$="     " else water$="Water"
	if a(4)=0 then gas$="   " else gas$="Gas"
	fnopenprn
	pr #255: ''
	pr #255: ''
	pr #255: ''
	pr #255: ''
	pr #255,using "Form pos 7,C 40": trim$(at$(1))
	pr #255,using "Form pos 7,C 40": trim$(at$(2))
	pr #255,using "Form pos 7,C 40": trim$(at$(3))
	pr #255: ''
	pr #255: ''
	pr #255: ''
	pr #255,using "Form pos 8,C 80": trim$(at$(1))&" Final Disconnect Notice   "&cnvrt$("pic(zz/zz/zz",d1)&"   "&trim$(z$)
	pr #255: ''
	pr #255,using "Form pos 9,C 73": mis$(1)
	pr #255,using "Form pos 9,C 73": mis$(2)
	pr #255,using "Form pos 9,C 73": mis$(3)
	pr #255,using "Form pos 9,C 73": mis$(4)
	pr #255: ''
	pr #255,using "Form pos 9,C 73": "Service  "&water$&"         "&gas$&"                Reconnection Fee: $"&cnvrt$('pic(###,##z.zz)',reconnect_fee)
	pr #255,using "Form POS 18,2*C 14,X 5,C 30": f$(1),f$(3),meter_address$
	pr #255: ''
	pr #255,using "Form pos 13,C 11,N 10.2": "Amount Due:",bal
	pr #255: ''
	pr #255: ''
	pr #255: ''
	pr #255: ''
	pr #255,using "Form pos 50,C 30": addr$(1)
	pr #255,using "Form pos 50,C 30": addr$(2)
	pr #255,using "Form pos 50,C 30": addr$(3)
	pr #255,using "Form pos 50,C 30": addr$(4)
	! 4 more lines from this point before next page
	pr #255: newpage
fnend
def fn_print_blucksberg(mat a,mat at$,mat mis$,mat f$,meter_address$*30,z$,mat addr$,bal,d1; ___,water$*5,gas$*3) ! 9/10/2018
	fnopenprn
	pr #255: ''
	pr #255: ''
	pr #255: ''
	pr #255: ''
	pr #255,using "Form pos 7,C 40": trim$(at$(1))
	pr #255,using "Form pos 7,C 40": trim$(at$(2))
	pr #255,using "Form pos 7,C 40": trim$(at$(3))
	pr #255: ''
	pr #255: ''
	pr #255: ''
	pr #255,using "Form pos 8,C 80": trim$(at$(1))&" Final Disconnect Notice   "&cnvrt$("pic(zz/zz/zz",d1)&"   "&trim$(z$)
	pr #255: ''
	pr #255,using "Form pos 9,C 73": mis$(1)
	pr #255,using "Form pos 9,C 73": mis$(2)
	pr #255,using "Form pos 9,C 73": mis$(3)
	pr #255,using "Form pos 9,C 73": mis$(4)
	pr #255: ''
	if a(1)=0 then water$="     " else water$="Water"
	if a(4)=0 then gas$="   " else gas$="Gas"
	pr #255,using "Form pos 9,C 73": "Service  "&water$&"         "&gas$&'                Reconnection Fee: $25.00'
	pr #255,using "Form POS 18,2*C 14,X 5,C 30": f$(1),f$(3),meter_address$
	pr #255: ''
	pr #255,using "Form pos 7,C": "Amount Due: "&cnvrt$('pic(---,$$$,$$z.zz)',bal)
	pr #255: ''
	pr #255: ''
	pr #255: ''
	pr #255: ''
	pr #255,using "Form pos 7,C 30": addr$(1)
	pr #255,using "Form pos 7,C 30": addr$(2)
	pr #255,using "Form pos 7,C 30": addr$(3)
	pr #255,using "Form pos 7,C 30": addr$(4)
	! 4 more lines from this point before next page
	pr #255: newpage
fnend
def fn_print_granby
	fnopenprn
	pr #255,using 'form pos 4,c 47,skip 4': e$(1)
	if gb(1)=0 then
		pr #255,using Fgranby1: "Your Utility Account is Past Due."
	else
		pr #255,using Fgranby2: "Your Utility Account is Past Due.","Water",gb(1)
	end if
	Fgranby1: form pos 4,c 47
	Fgranby2: form pos 4,c 47,pos 53,c 10,pos 65,n 10.2
	if gb(4)=0 then
		pr #255,using Fgranby1: "Please pay the amount due by "&ltrm$(d$(2))
	else
		pr #255,using Fgranby2: "Please pay the amount due by "&ltrm$(d$(2)),"Gas",gb(4)
	end if
	if gb(5)=0 then
		pr #255,using Fgranby1: "to avoid Utility Disconnection."
	else
		pr #255,using Fgranby2: "to avoid Utility Disconnection.","Sanitation",gb(5)
	end if
	if gb(2)=0 then pr #255: else pr #255,using Fgranby3: "Sewer",gb(2)
	if gb(7)=0 then pr #255: else pr #255,using Fgranby3: "Primacy",gb(7)
	if gb(8)=0 then pr #255: else pr #255,using Fgranby3: "Other",gb(8)
	Fgranby3: form pos 53,c 10,pos 65,n 10.2
	if gb(9)=0 then pr #255: else pr #255,using Fgranby3: "Sales Tax",gb(9)
	if gb(10)=0 then pr #255: "" else pr #255,using Fgranby4: "Penalty",gb(10)
	Fgranby4: form pos 53,c 10,pos 65,n 10.2
	pr #255: ''
	pr #255: ''
	pr #255: ''
	pr #255,using Fgranby5: e$(2)
	Fgranby5: form pos 10,c 30
	pr #255,using Fgranby5: e$(3)
	pr #255,using 'form pos 10,c 30,pos 52,c 10': e$(4),z$
	pr #255: ''
	pr #255,using 'form pos 63,n 10.2': bal
	granby_print_count+=1
	if granby_print_count/3=int(granby_print_count/3) then
		pr #255: newpage
	else
		pr #255: ''
		pr #255: ''
		pr #255: ''
		pr #255: ''
		pr #255: ''
	end if
fnend
def fn_french_settlement_gas
	fnopenprn
	! pre-print calculations__________________________________
	if pb<>0 then pb$="Prior Balance" else pb$=""
	if g(1)=0 then t1$="" else t1$="WTR"
	if g(2)=0 then t2$="" else t2$="SWR"
	if g(3)=0 then t3$="" else t3$="RPR"
	if g(4)=0 then t4$="" else t4$="GAS"
	if g(5)=0 then t5$="" else t5$="Purchased Gas Adj."
	if g(6)=0 then t6$="" else t6$="Inspection Fee"
	if g(7)=0 then t7$="" else t7$="Deposit Interest"
	if g(8)=0 then t8$="" else t8$="Other Charges"
	if g(8)<0 then t8$="Deposit Refund"
	if g(9)=0 then t9$="" else t9$="La. Sales Tax"
	! If D(10)=1 Then eST$="Bill Estimated" Else eST$=""
	if c4>0 then final$="Final Bill" else final$=""
	if bal<=0 then g(10)=0
	gross=max(bal+g(10),0)
	! actual Bill Printing____________________________________
	pos_amt=28
	pos_r=39
	L310: form pos 1,nz 6,nz 7,nz 7,x 2,c 3,pos pos_amt,nz 8.2,pos pos_r,c 30
	L320: form pos 1,c 20,pos pos_amt,nz 8.2,pos pos_r,c 30
	L340: form pos 1,pic(## ## ##),x 1,pic(## ## ##),n 8.2,pos 27,pic(-----.--),pos 40,n 7.2,x 3,pic(## ## ##),pic(-----.--),skip 1
	pr #255,using L391: z$
	L391: form pos 25,c 10
	pr #255,using L411: z$
	L411: form pos 38,c 10,skip 4
	pr #255,using L310: d(9),d(10),d(11),t4$,g(4),e$(1)
	pr #255,using L320: t5$,g(5),e$(2)
	pr #255,using L320: t6$,g(6),e$(3)
	pr #255,using L320: t7$,g(7),e$(4)
	! pr #255,Using 320: T8$,G(8)
	if budget>0 then bud$="Budgeted Amount:"&trim$(cnvrt$("Pic($$,$$$.##",budget)) else bud$=""
	pr #255,using L320: t9$,g(9),bud$(1:30)
	pr #255,using L320: pb$,bal-g(11)
	pr #255: ""
	pr #255,using L511: final$
	L511: form pos 22,c 10
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
	count+=1: if count=2 then pr #255: ""
	if count=3 then pr #255:
	pr #255,using L340: d(6),d(5),gross,bal,gross,d4,bal
	if count=1 then pr #255: : pr #255: : pr #255: : pr #255: : pr #255: : pr #255: ! EXTRA LINE BETWEEN 1ST and 2nd bills
	if count=2 then pr #255: : pr #255: : pr #255: : pr #255: : pr #255: ! EXTRA LINE BETWEEN 2nd & 3rd bill
	if count=3 then count=0 : pr #255: newpage
fnend
UBFORM: ! r: pr FROM TEXT FILE
	dim file_rtf$(1)*512
	fngetdir2("[Q]\UBmstr",mat file_rtf$, '/ON','*.rtf')
	fl1=udim(mat file_rtf$)
	for fl1=1 to udim(mat file_rtf$)
		file_rtf$(fl1)=file_rtf$(fl1)
	next fl1
	mat file_rtf$(fl1) : file_rtf$(fl1)="(Pre-Printed)"
	mat file_rtf$(fl1+=1) : file_rtf$(fl1)="(Reminder)"
goto SELECT_SCREEN ! /r
!  r: def fn_merriam_woods
!     library 'S:\acsUB\PrintBill_Merriam_Woods': fnpast_due_notice,fnpast_due_notice_finis
! ! pre-print calculations__________________________________
!     if pb<>0 then pb$="Prior Balance" else pb$=""
!     if g(1)=0 then t1$="" else t1$="WTR"
!     if g(2)=0 then t2$="" else t2$="SWR"
!     if g(3)=0 then t3$="" else t3$="RPR"
!     if g(4)=0 then t4$="" else t4$="GAS"
!     if g(5)=0 then t5$="" else t5$="Purchased Gas Adj."
!     if g(6)=0 then t6$="" else t6$="Inspection Fee"
!     if g(7)=0 then t7$="" else t7$="Deposit Interest"
!     if g(8)=0 then t8$="" else t8$="Other Charges"
!     if g(8)<0 then t8$="Deposit Refund"
!     if g(9)=0 then t9$="" else t9$="La. Sales Tax"
!     if c4>0 then final$="Final Bill" else final$=""
!     if bal<=0 then g(10)=0
!     gross=max(bal+g(10),0)
! ! actual Bill Printing____________________________________
!     fnpast_due_notice(z$)
! ! end_________________
! /r  fnend
SELECT_SCREEN: ! r:
	fn_report_close
	fnTos
	mat resp$=("")
	respc=0
	fnLbl(1,1,"File Name:",28,1)
	if hard_coded then
		fnTxt(1,30,10,0,0,'',1,'',0)
		resp$(respc+=1)='hard coded'
	else
		fncomboa("PDNOTRTF",1,30,mat file_rtf$)
		! resp$(respc+=1)=file_rtf$(1)
		if env$('client')='White Hall' then
			resp$(respc+=1)='White_Ha'
		else if env$('client')='Ash Grove' then
			resp$(respc+=1)='ashgrove.rtf'
		else
			respc+=1
			fncreg_read('ubpdnot_file_name',resp$(respc))
			if resp$(respc)='' or srch(mat file_rtf$,resp$(respc))<=0 then resp$(respc)=file_rtf$(1)
		end if
	end if
	fnLbl(3,1,"Route Number:",28,1)
	fncmbrt2(3,30)
	resp$(respc+=1)="[All]"
	fnLbl(4,1,"Beginning Account:",28,1)
	fncmbact(4,30,1)
	resp$(respc+=1)="[All]"
	fnChk(6,30,"Print only Selected Accounts")
	! fnCmdSet(102)
	fnCmdKey("&Print",1,1)
	if ~hard_coded then let fnCmdKey("E&dit",3)
	if ~hard_coded then let fnCmdKey("&Add",4)
	if ~hard_coded then let fnCmdKey("&Delete",7)
	if ~hard_coded then let fnCmdKey("&Refresh",6)
	fnCmdKey("&Cancel",5,0,1)
	fnAcs(mat resp$,ckey)
	do_print_std_form=0
	dim flname$*256
	flname$=rtrm$(resp$(1))
	fncreg_write('ubpdnot_file_name',flname$)
	if resp$(2)<>"[All]" then
		bk1=val(resp$(2))
		resp$(2)=""
	else
		bk1=0
		resp$(2)=""
	end if
	if trim$(resp$(3))<>"[All]" then
		sz$=lpad$(trim$(resp$(3)(1:10)),10)
		resp$(3)=""
	else
		sz$=""
		resp$(3)=""
	end if
	if resp$(4)="True" then
		resp$(4)=""
		sel_indv$="Y"
	else
		resp$(4)=""
		sel_indv$="N"
	end if
	if ckey=1 and resp$(1)="(Pre-Printed)" then
		do_print_std_form=1
		goto PRINTING_BEGIN
	else if ckey=1 then ! pr the Past Due Notices
		count=0
		if resp$(1)="(Reminder)" then
			fn_vbopenprint
			reminder=1
		else
			resp$(1)=""
			if ~hard_coded then let fn_open_template
		end if
		goto PRINTING_BEGIN
	else if ckey=5 then
		goto MENU1
	else if ckey=6 then
		goto UBFORM
	else if resp$(1)="(Pre-Printed)" or resp$(1)="(Reminder)" and ckey<>4 then
		goto SELECT_SCREEN ! CANT EDIT STANDARD FORM
	else if ckey=4 then ! r: Add
		fnTos
		respc=0
		fnLbl(1,1,"File Name:",15,1)
		fnTxt(1,17,40,64,1,"")
		resp$(respc+=1)=""
		fnCmdSet(2)
		fnAcs(mat resp$,ckey)
		if ckey=5 then goto MENU1
		dim newname$*256
		newname$=trim$(resp$(1))&'.rtf' ! &".rtf" ! trim$(resp$(1)(1:8))&".rtf"
		fnCopy("S:\Core\default\plain.rtf",os_filename$("[Q]\UBmstr\"&newname$))
		fnEditFile('atlantis',"[Q]\UBmstr\"&newname$)
		goto UBFORM ! /r
	else if ckey=3 then
		fnEditFile('atlantis',"[Q]\UBmstr\"&flname$)
		goto UBFORM
	else if ckey=7 then
		fnFree("[Q]\UBmstr\"&trim$(flname$))
		goto UBFORM
	else
		goto UBFORM
	end if
! /r  end of SELECT_SCREEN
PRINTING_BEGIN: ! r:
	checkcounter=0
	if trim$(sz$)="" and sel_indv$="Y" then goto ASK_NEXT_ACT ! selected to pick specific account but did not have one on screen
	if trim$(sz$)<>"" and sel_indv$="Y" then z$=sz$: goto READ_CUSTOMER ! selected to pirnt specific account and had an account on screen
	! if rtrm$(sz$)="" then goto L1330
	!  L1330: !
	if bk1=0 and trim$(sz$)="" then goto NEXT_RECORD
	if bk1=0 then goto L1360
	if trim$(sz$)="" or bk1>0 then ! restore_for_route
		sz$=cnvrt$("N 2",bk1)&"       " conv PRINTING_BEGIN
		restore #customer5,key>=sz$: nokey PRINTING_BEGIN
	else ! restore_for_customer
		restore #customer1,key=sz$: nokey PRINTING_BEGIN
	end if
	L1360: !
goto NEXT_RECORD ! /r
NEXT_RECORD: ! r:
	if sel_indv$="Y" then
		z$=lpad$(trim$(resp$(2)(1:10)),10)
		goto READ_CUSTOMER
	else
		goto PRINT_NEXT
	end if  ! /r
ASK_NEXT_ACT: ! r:
	fnTos
	respc=0
	fnLbl(1,1,"Next Account:",18,1)
	fncmbact(1,20,1)
	resp$(respc+=1)=""
	fnCmdSet(19)
	fnAcs(mat resp$,ckey)
	if ckey=5 then
		goto Xit
	else if ckey=2 then
		goto EO_CUSTOMER
	else
		sz$=lpad$(trim$(resp$(1)(1:10)),10)
		goto READ_CUSTOMER ! if ckey=1
	end if
! /r
READ_CUSTOMER: ! r:
	read #customer1,using F_CUSTOMER,key=sz$: z$,meter_address$,mat f$,mat a,mat xb,mat c,mat d,bal,f,mat g,bra,mat gb,route,final,mat extra,mat extra$ nokey ASK_NEXT_ACT
goto READ_ADRBIL ! /r
EO_CUSTOMER: ! r:
	! if env$('client')='Merriam Woods' then
	!   fnpast_due_notice_finis
	! else
	if ~reminder then
		restore #customer1: ! Close #customer1: Ioerr 980
		granby_print_count=0
		if h_prnt1 then
			close #h_prnt1: : h_prnt1=0
			fnEditFile('atlantis',tmp_rtf_filename$)
		else
			fncloseprn
		end if
		goto SELECT_SCREEN ! Xit
	end if
	close #customer1: ioerr ignore
	customer1=0
	fnpa_finis
	! end if

	if h_prnt1 then
		close #h_prnt1: : h_prnt1=0
		fnEditFile('atlantis',tmp_rtf_filename$)
	else
		fncloseprn
	end if  ! /r
Xit: ! r:
	fn_report_close
fnXit
! /r
include: Ertn