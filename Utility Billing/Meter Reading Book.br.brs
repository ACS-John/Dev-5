! formerly S:\acsUB\ubMetrBk
! pr Meter Reading Routes

autoLibrary
on error goto Ertn

dim z$*10,e$(4)*30,x$*10,f3$*12,dat$*20,service$(1)*2,bulksort$*12
dim a(7),option$(4)*25,extra(17),x_service$(3)*62,rm$*132,ft$*60
dim resp$(11)*40
dim notefile$*100,notedir$*100,ul$*60,d(15),snm$(10)*20,srv$(10)*2

fnTop(program$,"Meter Reading Book")
fndat(dat$,1)
! r: this section+the comboA on the first screen is just what you need  for a fnCurrently availableServiceTypeComboBox
	fnGetServices(mat snm$,mat srv$)
	x=0
	option$(x+=1)="[All]"
	for j=1 to 4
		if j=1 and trim$(snm$(j))="Water" then 
			option$(x+=1)=srv$(j)&"-"&snm$(j)(1:20)
			x_service=x_service+1
			mat service$(x_service)=service$
			service$(x_service)="WA"
		else if j=3 and (trim$(snm$(j))="Electric" or trim$(srv$(3)))="EL" then 
			option$(x+=1)=srv$(j)&"-"&snm$(j)(1:20)
			x_service=x_service+1
			mat service$(x_service)=service$
			service$(x_service)="EL"
		else if j=4 and (trim$(snm$(j))="Gas" or trim$(srv$(4))="GA") then 
			option$(x+=1)=srv$(j)&"-"&snm$(j)(1:20)
			x_service=x_service+1
			mat service$(x_service)=service$
			service$(x_service)="GA"
		end if 
	next j
	mat option$(x)
	mat x_service$(x_service)
	x_service =udim(x_service$)
	if env$('client')='Campbell' then fn_campbell_meter_book : goto Xit
! /r
MENU1: ! r:
	fnTos
	mylen=22 : mypos=mylen+2 : respc=lc=0
	fnLbl(lc+=1,1,"Route Number:",mylen,1)
	fncmbrt2(lc,mypos)
	resp$(1)=fn_reg_try$('ubMetrRt.route',"1")
	fnLbl(lc+=1,1,"Service Type:",mylen,1)
	fnComboA("ubrate3",lc,mypos,mat option$)
	resp$(2)=fn_reg_try$('ubMetrRt.service type',option$(1))
	lc+=1
	fnChk(lc+=1,mylen+2,"Print Footnotes:",1)
	resp$(3)=fn_reg_try$('ubMetrRt.print footnotes','False')
	fnChk(lc+=1,mylen+2,"Double Space:",1)
	resp$(4)=fn_reg_try$('ubMetrRt.double space','False')
	fnChk(lc+=1,mylen+2,"Blank Used Column:",1)
	resp$(5)=fn_reg_try$('ubMetrRt.blank used column','False')
	fnChk(lc+=1,mylen+2,"Show Disconnects:",1)
	resp$(6)=fn_reg_try$('ubMetrRt.show disconnects','False')
	fnChk(lc+=1,mylen+2,"Print Prior Usage:",1)
	resp$(7)=fn_reg_try$('ubMetrRt.print prior usage','False')
	fnChk(lc+=1,mylen+2,"skip Meter Number:",1)
	resp$(rc_disableMeterNo=8)=fn_reg_try$('ubMetrRt.skip meter number','False')
	lc+=1
	fnLbl(lc+=1,1,"Number of Blank Lines:",mylen-2,1)
	fnTxt(lc,mylen+2,2,0,0,'30',0,"If you want extra space between accounts, enter the number of lines.")
	resp$(9)=fn_reg_try$('ubMetrRt.# of blank lines',str$(extralines))
	lc+=1
	fnFra(lc+=1,10,2,26,"Order for printing","The billing journal can be printed if route # sequence, Account sequence or name sequence.",0)
	fnOpt(1,2,"Route Number Sequence",0,1)
	resp$(10)=fn_reg_try$('ubMetrRt.route number sequence','True')
	fnOpt(2,2,"Account Sequence",0,1)
	resp$(11)=fn_reg_try$('ubMetrRt.account sequence','False')
	fnCmdSet(3)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	if uprc$(resp$(1))=uprc$("[All]") then route=0 else route=val(resp$(1))
	svt$=resp$(2)
	if resp$(2)<>"[All]" then x_service=1 ! only printing one service if not answered as all
	remark=0          	: if resp$(3)(1:1)="T" then remark=1 ! yes want remarks printed
	double=0          	: if resp$(4)(1:1)="T" then double=1 ! yes want to double space
	usedcolumn=0      	: if resp$(5)(1:1)="T" then usedcolumn=1 ! yes want a gallons used column
	skipdisconnects=0 	: if resp$(6)(1:1)="T" then skipdisconnects=1 ! yes want show disconnects
	enablePrior=0     	: if resp$(7)(1:1)="T" then enablePrior=1 ! pr prior months usage
	enableMeterNo=0   	: if resp$(rc_disableMeterNo)(1:1)="F" then enableMeterNo=1 ! don't pr meter number
	extralines=val(resp$(9))
	if resp$(10)='True' then seq=1
	if resp$(11)='True' then seq=2
	fnreg_write('ubMetrRt.route',resp$(1))
	fnreg_write('ubMetrRt.service type',resp$(2))
	fnreg_write('ubMetrRt.print footnotes',resp$(3))
	fnreg_write('ubMetrRt.double space',resp$(4))
	fnreg_write('ubMetrRt.blank used column',resp$(5))
	fnreg_write('ubMetrRt.show disconnects',resp$(6))
	fnreg_write('ubMetrRt.print prior usage',resp$(7))
	fnreg_write('ubMetrRt.skip meter number',resp$(8))
	fnreg_write('ubMetrRt.# of blank lines',resp$(9))
	fnreg_write('ubMetrRt.route number sequence',resp$(10))
	fnreg_write('ubMetrRt.account sequence',resp$(11))
! /r
if seq=2 then 
	open #hCustomer=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",i,i,k 
else 
	open #hCustomer=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx5.h[cno],Shr",i,i,k 
end if 
fnopenprn
on pageoflow goto PgOf
gosub PrHeader
LOOP_TOP: ! r: main loop
	read #hCustomer,using 'form pos 1,C 10,4*C 30,C 12,pos 217,15*PD 5,pos 373,C 12,pos 361,C 12,pos 1741,N 2,pos 1821,N 1,pos 143,7*pd 2,pos 1806,3*n 2,pos 1821,n 1,pos 1942,c 12': z$,mat e$,f1$,mat d,f3$,f$(2),extra(1),fbc,mat a,extra(11),extra(12),extra(13),extra(17),bulksort$ eof Finis
	if extra(17)>0 and skipdisconnects=0 then goto LOOP_TOP
	if route=0 then goto L790
	if extra(1)<>route then goto LOOP_TOP
	if a(1)=0 and a(3)=0 and a(4)=0 then goto LOOP_TOP ! don't have any services
	L790: !
	x=0
	for j=1 to udim(x_service$)
		if usedcolumn=1 then 
			ul$="  {\ul          }  {\ul          }"
		else 
			ul$="  {\ul          }"
		end if 
		if enablePrior=1 then ul$="  {\ul          }"
		if enablePrior=1 and resp$(2)="[All]" and option$(j+1)(1:2)="WA" then 
			prior=d(3)
			x_service$(x+=1)=cnvrt$("pic(zzzzzzzzz)",d(1))&ul$&cnvrt$("pic(zzzzzzzzz)",prior)
		end if 
		if enablePrior=0 and resp$(2)="[All]" and option$(j+1)(1:2)="WA" then 
			x_service$(x+=1)=cnvrt$("pic(zzzzzzzzz)",d(1))&ul$
		end if 
		if enablePrior=1 and resp$(2)(1:2)="WA" then 
			prior=d(3)
			x_service$(x+=1)=cnvrt$("pic(zzzzzzzzz)",d(1))&ul$&cnvrt$("pic(zzzzzzzzz)",prior)
			mat x_service$(1)
			goto L940
		end if 
		if enablePrior=1 and resp$(2)="[All]" and option$(j+1)(1:2)="EL" then 
			prior=d(7)
			x_service$(x+=1)=cnvrt$("pic(zzzzzzzzz)",d(5))&ul$&cnvrt$("pic(zzzzzzzzz)",prior)
		end if 
		if enablePrior=0 and resp$(2)="[All]" and option$(j+1)(1:2)="EL" then 
			prior=d(7)
			x_service$(x+=1)=cnvrt$("pic(zzzzzzzzz)",d(5))&ul$
		end if 
		if enablePrior=1 and resp$(2)(1:2)="EL" then 
			prior=d(7)
			x_service$(x+=1)=cnvrt$("pic(zzzzzzzzz)",d(5))&ul$&cnvrt$("pic(zzzzzzzzz)",prior)
			mat x_service$(1)
			goto L940
		end if 
		if enablePrior=1 and resp$(2)="[All]" and option$(j+1)(1:2)="GA" then 
			prior=d(11)
			x_service$(x+=1)=cnvrt$("pic(zzzzzzzzz)",d(9))&ul$&cnvrt$("pic(zzzzzzzzz)",prior)
		end if 
		if enablePrior=0 and resp$(2)="[All]" and option$(j+1)(1:2)="GA" then 
			prior = d(11)
			x_service$(x+=1)=cnvrt$("pic(zzzzzzzzz)",d(9))&ul$
		end if 
		if enablePrior=1 and resp$(2)(1:2)="GA" and option$(j+1)(1:2)="GA" then 
			prior=d(11)
			x_service$(x+=1)=cnvrt$("pic(zzzzzzzzz)",d(9))&ul$&cnvrt$("pic(zzzzzzzzz)",prior)
			mat x_service$(1)
			goto L940
		end if 
		if enablePrior=0 and resp$(2)(1:2)="GA" and option$(j+1)(1:2)="GA" then 
			prior=d(11)
			x_service$(x+=1)=cnvrt$("pic(zzzzzzzzz)",d(9))&ul$: mat x_service$(1)
			goto L940
		end if 
	next j
	L940: ! If env$('client')="Thomasboro" AND FBC<>0 Then Goto 500
	if fbc><0 then e$(2)="Disconnect"
	! if env$('client')="Thomasboro" then 
	! 	pr #255,using 'form pos 1,C 10,X 1,C 18,X 1,C 20,X 1,X_service*c 43': z$,e$(2)(1:18),e$(1)(1:20),mat x_service$
	! 	goto L1050
	! end if
	
	if enableMeterNo then 
		if resp$(2)(1:2)='WA' then 
			meterNo$=f1$ 
		else if resp$(2)(1:2)='EL' then 
			meterNo$=f2$ 
		else 
			meterNo$=f3$
		end if
		
		if enablePrior=1 then 
			pr #255,using 'form pos 1,C 10,X 1,C 18,X 1,C 12,X 1,C 20,X 1,X_service*c 35,n 9': z$,e$(2)(1:18),meterNo$,e$(1)(1:20),mat x_service$
		else if usedcolumn=0 then 
			
			pr #255,using L1032: z$,e$(2)(1:18),meterNo$,e$(1)(1:20),mat x_service$
			L1032: form pos 1,c 10,x 1,c 18,x 1,c 12,x 1,c 20,x 1,x_service*c 26
		else if usedcolumn=1 then 
			pr #255,using 'form pos 1,C 10,X 1,C 18,X 1,C 12,X 1,C 20,X 1,X_service*c 43,x 3': z$,e$(2)(1:18),meterNo$,e$(1)(1:20),mat x_service$
		end if 
	else if enablePrior=1 then 
		pr #255,using 'form pos 1,C 10,X 1,C 18,X 1,C 20,X 1,X_service*c 35,n 9': z$,e$(2)(1:18),e$(1)(1:20),mat x_service$
	else if usedcolumn=0 then 
		pr #255,using 'form pos 1,C 10,X 1,C 18,X 1,C 20,X 1,X_service*c 26': z$,e$(2)(1:18),e$(1)(1:20),mat x_service$
	else if usedcolumn=1 then 
		pr #255,using 'form pos 1,C 10,X 1,C 18,X 1,C 20,X 1,X_service*c 43,x 3': z$,e$(2)(1:18),e$(1)(1:20),mat x_service$
	end if 
	! L1050: ! 
	if remark=1 then 
		gosub REMARK
		pr #255,using "form pos 1,c 60": ft$
	end if 
	if double=1 then pr #255: 
	if extralines>0 then 
		for j=1 to extralines
			pr #255: 
		next j
	end if 
goto LOOP_TOP ! /r
PgOf: pr #255: newpage : gosub PrHeader : continue 
Finis: ! r:
	close #hCustomer: ioerr ignore
	fncloseprn 
goto Xit ! /r
Xit: fnXit
def fn_reg_try$*256(field_name$*128,default_value$*256)
	dim rt_return$*256
	fnreg_read(field_name$,rt_return$, default_value$,1)
	fn_reg_try$=rt_return$
fnend  ! fn_reg_try$
PrHeader: ! r:
	! if env$('client')="Thomasboro" then 
	! 	pr #255: "\qc  {\f181 \fs22 \b "&env$('cnam')&"}"
	! 	pr #255: "\qc  {\f181 \fs18 \b "&trim$(dat$)&"}"
	! 	pr #255: ""
	! 	pr #255: "\ql {\ul Account No} {\ul Name              } ";
	! 	pr #255: "{\ul Meter Address          } ";
	! 	goto L1190
	! end if 
	pr #255: "\qc  {\f181 \fs22 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f181 \fs18 \b "&trim$(dat$)&"}"
	pr #255: "\qc  {\f181 \fs18 \b Page "&str$(page_count+=1)&"}"
	pr #255: ""
	if route<>0 then 
		pr #255: " Route: "&str$(route)
		pr #255: ""
	end if 
	pr #255: "\ql {\ul Account No} {\ul Name              } ";
	if enableMeterNo then 
		pr #255: "{\ul Meter Number} {\ul Meter Address          } ";
	else 
		pr #255: "{\ul Meter Address          } ";
	end if 
	! L1190: ! 
	for j=1 to x_service
		if enablePrior=1 then 
			pr #255: " {\ul Prior}    {\ul Current}    {\ul Usage}   ";
			goto L1230
		end if 
		if usedcolumn=0 then 
			pr #255: " {\ul Prior}    {\ul Current}   ";
		end if 
		if usedcolumn=1 then 
			pr #255: " {\ul Prior}    {\ul Current}      {\ul Used}   ";
		end if 
	L1230: ! 
	next j
	pr #255: " "
	if double=1 then 
		pr #255: ""
	end if 
return  ! /r
REMARK: ! r: ! read the footnote from the note file  (any note with * as first character
	ft$="                    "
	notedir$="[Q]\UBmstr\notes.h[cno]"
	notefile$=notedir$&"\"&trim$(z$)&".txt"
	if exists(notedir$)=0 then goto L1510
	open #20: "Name="&notefile$,display,input ioerr L1520
	do 
		linput #20: rm$ eof L1510
	loop until rm$(1:1)="*"
	ft$=rpad$(rm$(2:60),60)
	L1510: ! 
	close #20: ioerr ignore
	L1520: ! 
return  ! /r

def fn_campbell_meter_book ! Campbell Special Routine (printed once a year, one customer per page)
	dim z$*10,e$(4)*30,x$*10,f1$*12,f2$*12,f3$*12,message$*40,g(12),d(15)

	fnTos
	mylen=22 : mypos=mylen+2 : respc=lc=0
	fnLbl(lc+=1,1,"Route Number:",mylen,1)
	fncmbrt2(lc,mypos)
		resp$(1)=fn_reg_try$('ubMetrRt.route',"[All]")
	! fnLbl(lc+=1,1,"Service Type:",mylen,1)
	! fnTxt(lc,mypos,2, 0,0,'',1)
	! resp$(2)='WA'
	! fnComboA("ubrate3",lc,mypos,mat option$)
	fnCmdSet(3)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto CAMPBELL_XIT
	if uprc$(resp$(1))=uprc$("[All]") then route=0 else route=val(resp$(1))
	service$="W"
	fnreg_write('ubMetrRt.route',resp$(1))
	open #hCustomer=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",i,i,k 
	fnopenprn
	do
	CAMPBELL_LOOP_TOP: ! 
		dim g(12)
		! read #hCustomer,using F_CAMPBELL_CUSTOMER: z$,mat e$,f1$,watcode,elecode,litecode,mat d,f3$,c4,f$(2), mat g eof CAMPBELL_Finis
		! F_CAMPBELL_CUSTOMER:  form pos 1,c 10,4*c 30,c 12,pos 143,pd 2,pos 147,pd 2,pos 177,pd 4.2,pos 217,15*pd 5,pos 373,c 12,pos 213,pd 4,pos 361,c 12,pos 300,12*pd 4.2
		read #hCustomer,using 'form pos 1,C 10,4*C 30,C 12,pos 217,15*PD 5,pos 373,C 12,pos 361,C 12,pos 1741,N 2,pos 1821,N 1,pos 143,7*pd 2,pos 1806,3*n 2,pos 1821,n 1,pos 1942,c 12,pos 300,12*pd 4.2': z$,mat e$,f1$,mat d,f3$,f$(2),extra(1),fbc,mat a,extra(11),extra(12),extra(13),extra(17),bulksort$,mat g eof Finis
		watcode=a(1)
		! elecode=a(3) ! no longer used  
		! litecode=    ! no longer used
		if route and extra(1)<>route then goto CAMPBELL_LOOP_TOP
		if c4><0 then e$(2)="DISCONNECT"
		if uprc$(service$)="W" and watcode=0 then goto CAMPBELL_LOOP_TOP ! if water selected and water code=0 then skip
		if uprc$(service$)="W" and watcode>0 then goto CAMPBELL_EO_LOOP ! if water selected and water code>0 then print
	! if uprc$(service$)="E" and elecode>0 then goto CAMPBELL_EO_LOOP ! if elec selected and elec code>0 then print
	loop
	CAMPBELL_EO_LOOP: ! 
	!  if uprc$(service$)="W" then  ! WATER
	for j=1 to 39
		pr #255:''
	nex j
	pr #255,using F_CAMPBELL_OUT_W_1: D(1),D(3),G(1),G(2),G(5)
	F_CAMPBELL_OUT_W_1: form pos 8,n 9,x 2,n 8,n 8.2,n 6.2,n 6.2
	pr #255:''
	pr #255,using 'form pos 6,c 30': E$(1)
	pr #255:''
	pr #255,using 'form pos 6,c 10': Z$
	pr #255,using 'form pos 6,c 30': E$(2)
	pr #255,using 'form pos 6,c 30': E$(3) 
	pr #255,using 'form pos 6,c 30': E$(4) 
	pr #255: newpage
	goto CAMPBELL_LOOP_TOP
	CAMPBELL_Finis: ! 
	close #hCustomer: Ioerr ignore
	fncloseprn
	CAMPBELL_XIT: !
	close #hCustomer:
fnend
include: ertn
