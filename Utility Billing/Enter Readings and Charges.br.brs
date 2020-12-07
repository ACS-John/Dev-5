! formerly S:\acsUB\UBIpChg
fn_setup
fnTop(program$)
goto MENU1
def fn_setup
	if ~setup then
		setup=1
		autoLibrary
		on error goto Ertn
	! dims, constants, top, etc
		dim resp$(40)*256
		dim aname$*30
		dim d(15)
		dim alp$*1
		dim workFile$*256
		dim workFileIndex$*256
		dim t(16)
		dim message$(1)*128
		dim txt$*80
		dim txt$(6)*70
		dim extra(23)
		dim item$(30)*20
		dim extra$(11)*30
		dim x$*10
		dim px$*10
		dim x(15)
		dim ln$*256
		dim ft$*21
		dim rm$*60
		dim ra(2)
		dim colhdr$(30)*40
		dim cm$(30)
		dim est1(3,3)
		dim e1$*30
		dim e2$*30
		dim a(7)
		dim tg(11)
		dim cd1(8)
		dim penalty$(10)*1
		dim reporth$*300
		dim form$*300
		dim mroll(3) ! meter roll code from hand held file
		dim serviceoption$(10)*25
		dim srvnamc$(10)*21,srvnam$(10)*20,srv$(10)*2

		dim opt_final_billing$(5)*33
		opt_final_billing$(1)="0 = Not Finaled"
		opt_final_billing$(2)="1 = Final Bill"
		opt_final_billing$(3)="2 = Final & Refund Deposit"
		opt_final_billing$(4)="3 = Active, but do not Bill"
		opt_final_billing$(5)="4 = Finaled, but not billed"

		fnLastBillingDate(d1)
		if days(d1,'mmddyy')<days(date$('mmddyy'),'mmddyy')-23 then d1=0
		open #1: "Name=[Q]\UBmstr\Company.h[cno]",internal,input
		read #1,using "form pos 130,n 4": pcent ioerr ignore ! percent for unusual usage
		close #1:
		if pcent=0 then
			pcent=100
			open #1: "Name=[Q]\UBmstr\Company.h[cno]",internal,outIn
			rewrite #1,using "form pos 130,n 4": pcent ioerr ignore ! percent for unusual usage
			close #1:
		end if
		fncreg_read('unusual usage minimum water',uum_water$) : uum_water=val(uum_water$)
		fncreg_read('unusual usage minimum gas',uum_gas$) : uum_gas=val(uum_gas$)
		fncreg_read('unusual usage minimum electric',uum_electric$) : uum_electric=val(uum_electric$)

		pcent=pcent*.01 ! convert unusual usage % to decimal
		cancel=5
		workFile$="[Q]\UBmstr\Reads_and_Chgs.h[cno]"
		workFileIndex$="[Q]\UBmstr\Reads_and_Chgs-Key.h[cno]"
	! r: addmethod enumerations
		am_customersInSequence=1
		am_askAndEnterIndviduals=2
		am_loadHoldingFile=3
		am_estimateReadings=4
		am_fromHhFile=5
		am_importTabDelimited=6
	! /r
	! Open_Stuff: !
		fn_setup_service(mat service_enabled)
		open #hTrans=fnH: "Name=[Q]\UBmstr\ubTransVB.h[cno],KFName=[Q]\UBmstr\ubTrIndx.h[cno],Shr",internal,input,keyed
		open #hCustomer1=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,outIn,keyed  ! was file #1, but it was getting closed incorrectly
	F_CUSTOMER_C: form pos 1,c 10,pos 41,c 30,pos 143,7*pd 2,pos 1821,n 1,pos 217,15*pd 5,pos 354,c 1,pos 1741,n 2,n 7,2*n 6,n 9,pd 5.2,n 3,3*n 9,3*n 2,3*n 3,n 1,3*n 9,3*pd 5.2,pos 1954,c 12,pos 1906,c 12
		open #hWork=fnH: "Name="&workFile$&",KFName="&workFileIndex$&",Shr,Use,RecL=74,KPs=1,KLn=10",internal,outIn,keyed
		open #hCustomer2=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx2.h[cno],Shr",internal,outIn,keyed
		open #hCustomer3=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx3.h[cno],Shr",internal,outIn,keyed
		open #hCustomer4=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx4.h[cno],Shr",internal,outIn,keyed
		open #hCustomer5=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx5.h[cno],Shr",internal,outIn,keyed
		fncreg_read('Meter Reading Date Current',tmp$,date$("MMDDYY")) : d2=val(tmp$)
	end if
fnend
def fn_setup_service(mat service_enabled)
	! r: older stuff
	fnGetServices(mat srvnam$,mat srv$,mat tax_code$,mat penalty$)
	for j=1 to udim(mat srvnam$)
		srvnam$(j)=trim$(srvnam$(j))
		srvnamc$(j)=srvnam$(j)&":"
	next j
	if srvnamc$(6)="Bad Check Charge:" then srvnamc$(6)="Check Charge:"
	! /r
	! return explaind
	! +10    penalty$(   ) <>  "Y"
	! +100   srvnam$(    ) <>  ""
	mat service_enabled=(0)
	! r: set mat service_is_not_blank
	for x=1 to udim(mat srvnam$)
		if srvnam$(x)<>"" then service_is_not_blank(x)+=100
	next x
	! /r
	! r: set mat service_is_not_a_penalty
	for x=1 to udim(mat srvnam$)
		if penalty$(x) <>"Y" then service_is_not_a_penalty(x)+=100
	next x
	! /r
	! r: set mat service_type
	! service_type returns:
	!                         1   - Water
	!                         2   - Sewer
	!                         3   - Electric or SRV is EL
	!                         3.1 - Lawn Meter
	!                         3.2 - Reduc
	!                         4   -  Gas or SRV is GA
	!                         5   - Other
	for x=1 to udim(mat srvnam$)
		if srvnam$(x)='Water' then
			service_type(x)=1
		else if srvnam$(x)='Sewer' then
			service_type(x)=2
		else if srvnam$(x)="Electric" or srv$(x)="EL" then
			service_type(x)=3
		else if srvnam$(x)='Lawn Meter' then
			service_type(x)=3.1
		else if srvnam$(x)(1:5)='Reduc' then
			service_type(x)=3.2
		else if srvnam$(x)="Gas" or srv$(x)="GA" then
			service_type(x)=4
		else if uprc$(srvnam$(x)(1:5))="OTHER" then
			service_type(x)=5
		else if uprc$(srvnam$(x))="GAS CONNECT" then
			service_type(x)=6
		else if uprc$(srvnam$(x))="WATER CONNECT" then
			service_type(x)=7
		else if uprc$(srvnam$(x))="BAD CHECK CHARGE" then
			service_type(x)=8
		end if
	next x
	! /r
	! r: set the mat service_enabled
	for x=1 to udim(mat srvnam$)
		if srvnam$(x)='Water' then
			service_enabled(x)+=1
		end if
	next x
	if service_is_not_blank(1) then
		service_enabled(1)=1
	end if
	!
	if service_is_not_blank(2) then
		service_enabled(2)=1
	end if
	!
	if service_type(3)=3 then
		service_enabled(3)=1
	else if service_type(3)=3.1 then ! Lawn Meter" then
		service_enabled(3)=2
	else if service_type(3)=3.2 then ! Reduc" then
		service_enabled(3)=3
	end if
	!
	if service_type(4)=4 then
		service_enabled(4)=1
	end if

	if service_type(5)=5 or (service_is_not_blank(5) and service_is_not_a_penalty(5)) then
		service_enabled(5)=1
	end if

	if service_type(6)=5 or (service_is_not_blank(6) and service_is_not_a_penalty(6)) then
		service_enabled(6)=1
	end if

	if service_type(7)=5 or (service_is_not_blank(7) and service_is_not_a_penalty(7)) then ! Service 7 seems to incompletly implemented
		service_enabled(7)=1
	end if

	if service_type(8)=5 or (service_is_not_blank(8) and service_is_not_a_penalty(8)) then
		service_enabled(8)=1
	end if

	!   if service_is_not_blank(9) then
	!     service_enabled(9)=1
	!   end if
	!   !
	!   if service_is_not_blank(10) then
	!     service_enabled(10)=1
	!   end if
	! /r
	! r: set mat serviceoption$, serviceoption_count and service  (for fn_meter_change_out)
	serviceoption_count=0
	if trim$(srvnam$(1))="Water" then
		serviceoption$(serviceoption_count+=1)=srv$(1)&"-"&trim$(srvnam$(1)(1:20))
		service=service+1
	end if
	if trim$(srvnam$(3))="Electric" or trim$(srv$(3))="EL" then
		serviceoption$(serviceoption_count+=1)=srv$(3)&"-"&srvnam$(3)(1:20)
		service=service+1
	end if
	if (trim$(srvnam$(4))="Gas" or trim$(srv$(4))="GA") then
		serviceoption$(serviceoption_count+=1)=srv$(4)&"-"&srvnam$(4)(1:20)
		service=service+1
	end if
	mat serviceoption$(serviceoption_count)
	! /r
fnend
AUTO_REC: ! r:
	done_with_readings=0
	fnTos
	fnLbl(1,1,"Starting Account:" ,24,1)
	fncmbact(1,26,1)
	resp$(1)="[All]"
	fnCmdSet(2)
	fnAcs(mat resp$,ckey)
	if ckey=cancel then
		done_with_readings=1
		goto MENU1
	end if
	if uprc$(resp$(1))=uprc$("[All]") then resp$(1)=""
	x$=lpad$(trim$(resp$(1)(1:10)),10)
	px$=x$
	if trim$(x$)="" then goto SEL_ACC
	read #hCustomer1,using F_CUSTOMER_C,key=x$,release: x$,aname$,mat a,final,mat d,alp$,mat extra,extra$(3) nokey AUTO_REC
	if addmethod=am_customersInSequence and env$('client')<>"Choctaw" then
		seq$=cnvrt$("pic(zz)",extra(1))&cnvrt$("pic(zzzzzzz)",extra(2))
		read #hCustomer5,using F_CUSTOMER_C,key=seq$,release: x$,aname$,mat a,final,mat d,alp$,mat extra,extra$(7),extra$(3) nokey AUTO_REC
	end if
	fnapply_default_rates(mat extra, mat a)
	if final=1 or final=2 or final=3 or (trim$(px$)<>"" and x$<>px$) then goto READ_ROUTE_SEQUENCE ! ken  ! john -- this is so users can select an account without being pushed to the incorrect account with the same rt/seq
	px$=""
	goto EnterReadings
! /r
SEL_ACC: ! r:
! passcheckwater=passcheckgas=passcheckelec=0
	if addmethod=am_customersInSequence then
		goto READ_ROUTE_SEQUENCE
	end if
	x$=""
SEL_ACT_TOS: !
	ckey=fnask_account('ubipchg',x$,hCustomer1)
	if ckey=5 or ckey=cancel then
		addmethod=0
		goto MENU1
	end if
	if addmethod=am_customersInSequence then
		goto READ_ROUTE_SEQUENCE
	else
		read #hCustomer1,using 'form pos 41,c 30,pos 143,7*pd 2,pos 1821,n 1,pos 217,15*pd 5',key=x$,release: aname$,mat a,final,mat d nokey SEL_ACT_TOS
		fnapply_default_rates(mat extra, mat a)
		goto EnterReadings2
	end if
! /r
READ_ROUTE_SEQUENCE: ! r:
	if env$('client')="Choctaw" then ! read in Account order
		read #hCustomer1,using F_CUSTOMER_C: x$,aname$,mat a,final,mat d,alp$,mat extra,extra$(7),extra$(3) eof MENU1
	else
		read #hCustomer5,using F_CUSTOMER_C,release: x$,aname$,mat a,final,mat d,alp$,mat extra,extra$(7),extra$(3) eof MENU1
	end if
	fnapply_default_rates(mat extra, mat a)
	if final=1 or final=2 or final=3 or (trim$(px$)<>"" and x$<>px$) then goto READ_ROUTE_SEQUENCE
	px$=""
goto EnterReadings ! /r
def fn_meter_roll
	mat txt$(4)
	txt$(1)="Reading: "&str$(cur_read)&"   Prior: "&str$(prior_read)&"   Usage: "&str$(x0)
	txt$(2)="Account: "&x$&" - "&aname$
	txt$(3)="Negative Usage on "&sn$
	txt$(4)="Is this a Meter Roll?"
	fnmsgbox(mat txt$,resp$,'',35)
	if resp$="No" then
		passcheck=ckfail
		goto METER_ROLL_XIT
	else if mroll(1)=1 then
		goto L3110
	end if
	if uprc$(sn$)=uprc$(srvnam$(1)) then
		cde=1
	else
		goto L3110
	end if
	xcde=1 : xcd2=11 : mroll(1)=1
	goto METER_ROLL_DONE ! water
	L3110: if mroll(3)=1 then goto L3140
	if uprc$(sn$)=uprc$(srvnam$(3)) then
		cde=5
	else
		goto L3140
	end if
	xcde=3 : xcd2=10 : mroll(3)=1
	goto METER_ROLL_DONE ! electric
	L3140: !
	if mroll(2)=1 then
		goto METER_ROLL_XIT
	end if
	if uprc$(sn$)=uprc$(srvnam$(4)) then
		cde=9
	else
		goto METER_ROLL_XIT
	end if
	xcde=2 : xcd2=12 : mroll(2)=1
	METER_ROLL_DONE: !
	digits=len(str$(d(cde)))
	x(xcde+xcd2)=10**digits-d(cde)+x(xcde)
	! ** means to the power of
	METER_ROLL_XIT: !
fnend
def fn_print_readings(hWork; printReadings_altHeading$*40) ! pr proof of readings file
	totwat=totele=totgas=0
	fnopenprn
	fn_printReadings_Heading( printReadings_altHeading$)
	restore #hWork: ! ,search>="": nokey PR_TOTALS    <-- needs to work with or without an index
	do

		read #hWork,using Fwork: x$,mat x eof PR_TOTALS
		totwat+=x(1): totele+=x(3): totgas+=x(2)
		e1$=e2$=""
		read #hCustomer1,using F_CUSTOMER_B,key=x$: e1$,e2$,mat d,f,mat a nokey PR_CUSTOMER_NOKEY
		F_CUSTOMER_B: form pos 11,2*c 30,pos 217,15*pd 5,pos 296,pd 4,pos 143,7*pd 2
		! place usage in usage column if not usage already there so it shows on proof list
		! Water
		if f<>d1 then oldreading=d(1) else oldreading=d(2)
		! if x(12)=0 then x(12)=x(1)-oldreading
		if x(12)=0 and a(1)<>0 then x(12)=x(1)-oldreading ! A(1) checking was added 10/4/11 to prevent usage (and negative usage) on customers who have an (0) inactive rate code ! the whole line was commented out but added back in on 2/13/12
		! Electric
		if f<>d1 then oldreading=d(5) else oldreading=d(6)
		if x(13)=0 then x(13)=x(3)-oldreading
		! Gas
		if f<>d1 then oldreading=d(9) else oldreading=d(10)
		if x(14)=0 and a(4)<>0 then x(14)=max(0,x(2)-oldreading) ! A(4) checking was added 9/21/11 to prevent usage (and negative usage) on customers who have an (0) inactive rate code
		PR_CUSTOMER_NOKEY: !
		rc=0
		mat pc_data=(0)
		if service_enabled(1) then
			pc_data(rc+=1)=x(1)
			pc_data(rc+=1)=x(9)
			pc_data(rc+=1)=x(12)
		end if
		if service_enabled(2) then
			pc_data(rc+=1)=x(5)
		end if
		if service_enabled(3) then
			pc_data(rc+=1)=x(3)
			pc_data(rc+=1)=x(10)
			pc_data(rc+=1)=x(13)
			pc_data(rc+=1)=x(4)
		end if
		if service_enabled(4) then
			pc_data(rc+=1)=x(2)
			pc_data(rc+=1)=x(11)
			pc_data(rc+=1)=x(14)
		end if
		if service_enabled(5) then pc_data(rc+=1)=x(6)
		if service_enabled(6) then pc_data(rc+=1)=x(7)
		if service_enabled(7) then pc_data(rc+=1)=x(8)
		rc+=1
		if udim(mat pc_data)<rc then mat pc_data(rc)
		pc_data(rc)=x(15) ! Final Billing Code
		mat pc_data(rc)
		pr #255,using form$: x$,e2$(1:25),e1$(1:25),mat pc_data pageoflow PrintReadings_PgOf
	loop
	PR_TOTALS: !
	pr #255,using "form skip 2,c 30": "Batch Totals for Readings"
	pr #255,using "form pos 1,c 10,nz 20,skip 1,pos 1,c 10,nz 20,skip 1,pos 1,c 10,nz 20,skip 1": srvnam$(1)(1:10),totwat,srvnam$(3)(1:10),totele,srvnam$(4)(1:10),totgas
	fncloseprn
fnend
def fn_printReadings_Heading(;altHeading$*40)
	if altHeading$='' then altHeading$="Readings Proof List"
	pr #255,using 'Form Pos 20,Cc 40': altHeading$
	pr #255,using 'Form Pos 20,Cc 40': cnvrt$("pic(zz/zz/zz",d2)
	pr #255,using 'Form POS 1,C 220': reporth$
fnend
PrintReadings_PgOf: ! r:
	pr #255: newpage
	fn_printReadings_Heading( printReadings_altHeading$)
continue ! /r
MAKE_CORRECTIONS: ! r:
	read #hWork,using Fwork,key=x$: x$,mat x nokey MENU1
	t(1)-=1 ! SUBTRACT PROOF TOTALS
	for j1=1 to 15 : t(j1+1)-=x(j1) : next j1
	read #hCustomer1,using F_CUSTOMER_C,key=x$,release: x$,aname$,mat a,final,mat d,alp$,mat extra,extra$(3)
	fnapply_default_rates(mat extra, mat a)
	editmode=1
	goto EnterReadings3 ! /r
REWRITE_WORK: ! r:
	rewrite #hWork,using Fwork,key=x$: trim$(x$),mat x nokey L3900
	goto L3910
	L3900: !
	if trim$(uprc$(x$))<>trim$(uprc$("DELETED")) then fn_writeWork(hWork,x$,mat x)
	L3910: !
	if trim$(uprc$(x$))=trim$(uprc$("DELETED")) then goto MAKE_CORRECTIONS
	fn_accumulateProofTotals
	if editmode=1 then return
	goto MENU1 ! /r MAKE_CORRECTIONS
CHANGE_ACT_NUM: ! r:
	fnTos
	mylen=19 : mypos=mylen+2
	fnLbl(1,1,"New Account:",mylen)
	fnTxt(1,mypos,10)
	resp$(1)=""
	fnCmdSet(1)
	fnAcs(mat resp$,ckey)
	x$=trim$(resp$(1))
	read #hCustomer1,using "Form POS 36,C 25",key=x$,release: aname$ nokey CHANGE_ACT_NUM
	goto REWRITE_WORK ! /r
def fn_lo_pr_rec(x$,mat x)
	pr #255,using "form pos 1,c 10,x 2,4*pic(----------)": x$,x(1),x(2),x(3),x(4)
fnend  ! fn_lo_pr_rec
def fn_accumulateProofTotals
	t(1)+=1
	for j=1 to 15
		t(j+1)+=x(j)
	next j
fnend
def fn_checkWater
	if wr1=0 then fn_us1(x$,d1)
	if a(1)<>0 then ! skip routine if no water code
		sn$=srvnam$(1)
		if trim$(srvnam$(1))="" or mroll(1)=1 or (d(wr1)=0 and x(1)=0) then
			passcheck=ckpass
			goto CheckWaterFinis
		end if
		if trim$(sn$)="Water" and x(12)>0 then
			passcheck=ckpass
			goto CheckWaterFinis ! don't give warning if usage entered
		end if
		if env$('client')="Billings" and len(str$(x(1)))=6 and len(str$(d(wr1)))=7 then x(1)=val(str$(d(wr1))(1:1)&str$(x(1)))
		x4=x(1)-d(wr1)
		sn$=srvnam$(1) : x0=x4 : prior_read=d(wr1) : cur_read=x(1)
		if x4>=0 then goto CHECKWATER_L4260
		if x(12)>0 then sn$=srvnam$(1) : goto CheckWaterFinis
		if x4<0 then fn_meter_roll
	end if  ! a(1)<>0
	goto CheckWaterFinis
	CHECKWATER_L4260: !
	if d(3)=0 then goto CheckWaterFinis
	if uum_water<>0 and x0<uum_water then
		passcheck=ckpass
	else if x4<d(3)-d(3)*pcent or x4>d(3)+d(3)*pcent then
		passcheck=ckfail
	else
		passcheck=ckpass
	end if
	CheckWaterFinis: !
	fn_checkend
fnend
def fn_checkElec
	if er1=0 then fn_us1(x$,d1)
	if a(3)=0 then goto CHECKELEC_FINIS ! if no electric code skip
	if trim$(sn$)="Electric" and x(13)>0 then passcheck=ckpass : goto CHECKELEC_FINIS ! don't give warning if usage entered
	if (service_type(3)=3 or (service_type(3)=3.1 and env$('client')<>"Thomasboro")) then
		goto L4350
	else
		passcheck=ckpass
		goto CHECKELEC_FINIS
	end if
	L4350: !
	if x(3)=0 and d(er1)=0 then goto CHECKELEC_FINIS
	x2=x(3)-d(er1)
	sn$=srvnam$(3) : x0=x2 : : prior_read=d(er1) : cur_read=x(3)
	if x2>=0 then goto L4420
	if x(13)>0 then sn$=srvnam$(3) : goto CHECKELEC_FINIS
	if x2<0 then fn_meter_roll
	goto CHECKELEC_FINIS
	L4420: !
	if d(7)=0 then goto CHECKELEC_FINIS
	if uum_electric<>0 and x0<uum_electric then
		passcheck=ckpass
	else if x2<d(7)-d(7)*pcent or x2>d(7)+d(7)*pcent then
		passcheck=ckfail
	else
		passcheck=ckpass
	end if
	CHECKELEC_FINIS: !
	fn_checkend
fnend
def fn_checkGas
	if a(4)=0 then goto CHECKGAS_FINIS ! skip if no gas codes
	sn$=srvnam$(4)
	if trim$(srvnam$(4))<>"Gas" or mroll(2)=1 then
		passcheck=ckpass
		goto CHECKGAS_FINIS
	end if
	if trim$(sn$)="Gas" and x(14)>0 then passcheck=ckpass : goto CHECKGAS_FINIS ! don't give warning if usage entered
	if x(2)=0 and d(gr1)=0 then goto CHECKGAS_FINIS
	x3=x(2)-d(gr1)
	sn$=srvnam$(4): x0=x3 : prior_read=d(gr1): cur_read=x(2)
	if x3>=0 then goto CHECKGAS_L4580
	if x(14)>0 then sn$=srvnam$(4): goto CHECKGAS_FINIS
	if x3<0 then fn_meter_roll
	goto CHECKGAS_FINIS
	CHECKGAS_L4580: !
	if d(11)=0 then goto CHECKGAS_FINIS
	if uum_gas<>0 and x0<uum_gas then
		passcheck=ckpass
	else if x3<d(11)-d(11)*pcent or x3>d(11)+d(11)*pcent then
		passcheck=ckfail
	else
		passcheck=ckpass
	end if
	CHECKGAS_FINIS: !
	fn_checkend
fnend
def fn_checkend
	if addmethod=am_loadHoldingFile and passcheck=ckfail then
		fn_print_unusual
		goto CHECKEND_XIT
	end if
	if passcheck=ckpass then goto CHECKEND_XIT
	if passcheck=ckfail and x0>=0 then
		mat txt$(8) : txt$(1)=sn$&" - Unusual Usage Warning"
		txt$(2)="Account: "&x$&" - "&aname$ : txt$(3)=""
		txt$(4)="Reading: "&str$(cur_read)&"   Prior: "&str$(prior_read)&"   Calculated Usage: "&str$(x0) : txt$(5)=""
		txt$(6)="Yes = Continue, the usage is correct."
		txt$(7)="No = Go Back, so I can re-enter the reading;"
		txt$(8)="Cancel = Do not enter a reading for that Customer."
		fnmsgbox(mat txt$,resp$,'',51)
		if resp$="Yes" then
			passcheck=ckpass
		else if resp$="No" then
			passcheck=ckfail
		else if resp$="Cancel" then
			passcheck=ckfail
			editmode=0
		end if
	end if

	CHECKEND_XIT: !
fnend
def fn_print_unusual
	fnopenprn
	if ~setup_printunusual<=0 then
		setup_printunusual=1
		dim fmun$*80
		pr #255: " Account    Name                    Old Reading  New Reading   Usage"
		pr #255: "----------  ----------------------  -----------  -----------  -----------"
		fmun$="form c 12,c 22,3*n 13,x 2,c 20,skip 1"
	end if
	pr #255,using fmun$: x$,e2$(1:20),prior_read,cur_read,x0,sn$
fnend
def fn_hh_readings(ip1$; listonly) ! HH_READINGS: ! hand held routines
	dim device$*20
	device$=fnhand_held_device$
	if device$="Psion Workabout" then
		goto HH_WORKABOUT
	else if device$="Badger" or device$="Badger Connect C" then
		goto HH_BADGER
	else if device$="Boson" then
		goto HH_BOSON
	else if device$="Laptop" then
		gosub LAPTOP
		if listonly=1 then fn_lo_pr_rec(x$,mat x) : goto HH_W_NXT
		goto HH_CONTINUE
	else ! if device$='Master Meter' or device$='READy Water' or device$="AMR" or device$="Other" or device$="Sensus" or device$="Green Tree" or device$="Hersey" or device$="EZReader" or device$="Itron FC300" or device$="" then
		goto HH_OTHER
	end if
	HH_WORKABOUT: ! r: hand held routines for workabout
	open #h_readings=fnH: "Name=[Q]\UBmstr\Readings."&ip1$&",RecL=1",external,input,relative ioerr L4990
	goto L5000
	L4990: !
	restore #h_readings:
	L5000: !
	if listonly=1 then fnopenprn( 'Book '&ip1$)
	j1=29 : j2=97
	HH_W_READ: !
	ln$="" : mat x=(0)
	for j=j1 to j2
		read #h_readings,using "Form POS 1,C 1",rec=j: c$ noRec HH_W_END
		ln$=ln$&c$
	next j
	x$=lpad$(trim$(ln$(1:10)),10) : x(1)=val(ln$(11:19))
	mroll(1)=val(ln$(20:20)) : x(3)=val(ln$(21:29))
	mroll(3)=val(ln$(30:30)) : x(2)=val(ln$(31:39))
	mroll(2)=val(ln$(40:40)) : x(4)=val(ln$(41:49))
	ft$=rtrm$(ln$(50:69))
	if ft$="00000000000000000000" then ft$=""
	if listonly=1 then fn_lo_pr_rec(x$,mat x) : goto HH_W_NXT
	if x$(1:1)="0" then x$(1:1)=" " ! drop leading zero
	if file(255)=-1 and rtrm$(ft$)<>"" then
		fnopenprn
	end if
	if trim$(ft$)<>"" then
		pr #255: "NEW NOTE! "&x$&" - "&ft$
	end if
	goto HH_CONTINUE ! /r
	HH_BADGER: ! r: Hand Held routines for Badger (badger file is copied from                        \connect\connect\x to readings.x in the transfer from                           Hand Held routine)
	if listonly=1 then fnopenprn
	close #h_readings: ioerr ignore
	open #h_readings=fnH: "Name=[Q]\UBmstr\Readings."&ip1$,d,i ! &",RecL=256",display,input
	HH_BADGER_READ: !
	linput #h_readings: ln$ eof HH_W_END
	! pr ln$ : pause
	if ln$(1:1)="T" or ln$(1:1)="H" then goto HH_BADGER_READ
	mat x=(0)
	x$=lpad$(rtrm$(ln$(121:130)),10) conv HH_BADGER_READ ! Account Key
	ti1=1: ti1=val(ln$(64:64))       conv HH_BADGER_READ
	x(ti1)=val(ln$(96:104))          conv HH_BADGER_READ
	! if env$('client')="Moweaqua" Then x(TI1)=X(TI1)
	if listonly=1 then fn_lo_pr_rec(x$,mat x) : goto HH_W_NXT
	goto HH_CONTINUE ! /r

	HH_BOSON: ! r: Hand Held routines for Boson (boson file is copied from                        [Q]\UBmstr\outofpalm.txt in hhfro to readings.(route# (which is asked))
	dim last_ln$*256
	last_ln$=""
	if listonly=1 then fnopenprn
	close #h_readings: ioerr ignore
	open #h_readings=fnH: "Name=[Q]\UBmstr\Readings."&ip1$&",RecL=204",display,input
	HH_BOSON_READ: !
	if last_ln$="" then
		linput #h_readings: ln$ eof HH_W_END
	else
		ln$=last_ln$
		last_ln$=''
	end if
	if ln$(1:1)="T" or ln$(1:1)="H" then goto HH_BOSON_READ
	mat x=(0)
	ti$=ln$(14:14)
	if ti$="W" or ti$="G" or ti$="E" then
		x$=lpad$(rtrm$(ln$(4:13)),10) conv HH_BOSON_READ
	else
		x$=lpad$(rtrm$(ln$(5:14)),10) conv HH_BOSON_READ
		ti$=""
		ti1=1
	end if
	if uprc$(ti$)="W" then
		ti1=1
	else if uprc$(ti$)="G" then
		ti1=2
	else if uprc$(ti$)="E" then
		ti1=3
	end if
	L5420: x(ti1)=0: x(ti1)=val(ln$(89:97)) conv L5440 ! kj 120308 allow boson to place codes in field if cant read meter
	! if env$('client')="Billings" and ln$(91:91)=" " then x(ti1)=val("1"&ln$(92:97))
	if env$('client')="Moweaqua" and (a(1)=1 or a(1)=2) then
		x(ti1)=round(x(ti1)*.1,0)
	end if
	if ti$="" or ti$="W" then
		linput #h_readings: ln$ eof L5440
		if ti$="" then
			x_g$=lpad$(rtrm$(ln$(5:14)),10) conv L5440
		else
			x_g$=lpad$(rtrm$(ln$(4:13)),10) conv L5440
			ti$=ln$(14:14)
		end if
		if x_g$=x$ and ti$="" or ti$="G" then
			x(2)=val(ln$(89:97))
			last_ln$=""
		else
			last_ln$=ln$
		end if
	end if
	L5440: !
	goto HH_CONTINUE ! /r
	LAPTOP: ! r: readings from a laptop using acs meter reading software
		if listonly=1 then fnopenprn
		close #h_readings: ioerr ignore
		open #h_readings=fnH: "Name=[Q]\UBmstr\Readings."&ip1$&",RecL=50",display,input
		HH_LAPTOP_READ: linput #h_readings: ln$ eof HH_W_END
		mat x=(0)
		x$=lpad$(rtrm$(ln$(1:10)),10) conv HH_LAPTOP_READ ! Account Key
		!
		ti1=1: ti$=ln$(20:20) ! type of reading
		if uprc$(ti$)="W" then ti1=1
		if uprc$(ti$)="E" then ti1=2
		if uprc$(ti$)="G" then ti1=3
		x(ti1)=val(ln$(11:19)) conv HH_LAPTOP_READ
		read #hCustomer1,using F_CUSTOMER_C,key=x$,release: x$,aname$,mat a nokey ignore
	return ! if listonly=1 then fn_lo_pr_rec(x$,mat x) : goto HH_W_NXT
	! goto HH_CONTINUE ! /r
	HH_OTHER: ! r:
	if device$='AMR' then goto HH_OTHER_TYPE1
	if device$='EZReader' then goto HH_OTHER_TYPE1
	if device$='Green Tree' then goto HH_OTHER_TYPE1
	if device$='Hersey' then goto HH_OTHER_TYPE1
	if device$='Master Meter' then goto HH_OTHER_TYPE1
	if device$='READy Water' then goto HH_OTHER_TYPE1
	! if device$='Sensus' then goto HH_OTHER_TYPE1
	fn_hh_other_type2(ip1$,listonly)
	goto HH_W_END ! /r
	HH_OTHER_TYPE1: ! r:
	if listonly=1 then fnopenprn
	close #h_readings: ioerr ignore
	open #h_readings=fnH: "Name=[Q]\UBmstr\Readings."&ip1$&",RecL=30",display,input
	HH_OTHER_TYPE1_READ: !
	linput #h_readings: ln$ eof HH_W_END
	mat x=(0)
	x$=lpad$(rtrm$(ln$(1:10)),10) conv HH_OTHER_TYPE1_READ ! Account Key
	ti1=1 ! water
	x(ti1)=0
	if device$='READy Water' then
		x(ti1)=val(ln$(11:len(ln$))) conv ignore
	! else if env$('client')="Lovington" then
	! 	x(ti1)=val(ln$(11:19)) conv ignore
	else
		x(ti1)=val(ln$(11:20)) conv ignore
	end if
	read #hCustomer1,using F_CUSTOMER_C,key=x$,release: x$,aname$,mat a nokey ignore
	if listonly=1 then fn_lo_pr_rec(x$,mat x) : goto HH_W_NXT
	goto HH_CONTINUE ! /r

	HH_CONTINUE: ! Continue with standard Hand Held routine
	read #hCustomer1,using F_CUSTOMER_C,key=x$,release: x$,aname$,mat a,final,mat d,alp$,mat extra,extra$(3) nokey HH_W_NXT
	fn_us1(x$,d1)
	mat est1=(0)
	if x(1)=999999 then est1(1,1)=1 : est1(1,2)=100
	if x(2)=999999 then est1(3,1)=1 : est1(3,2)=100
	if x(3)=999999 then est1(2,1)=1 : est1(2,2)=100
	if sum(est1) then
		read #hCustomer1,using F_CUSTOMER_A,key=x$,release: x$,e2$,mat a,f,final,mat d,mat extra,extra$(3) nokey HH_W_NXT
		gosub EST2B
	end if
	gosub CheckUnusual
	if skiprec=1 then ! skip record
		skiprec=0 : goto HH_W_NXT
	else 
		fn_writeWork(hWork,x$,mat x)
		fn_accumulateProofTotals
		fn_rmk1
	end if
	HH_W_NXT: !
	if device$="Badger" or device$="Badger Connect C" then
		goto HH_BADGER_READ
	else if device$="Boson" then
		goto HH_BOSON_READ
	else if device$="Laptop" then
		goto HH_LAPTOP_READ
	else if device$="Psion Workabout" then
		j1+=72
		j2+=72
		goto HH_W_READ
	else ! if device$="Other" or device$="Sensus" or device$="AMR" or device$="Green Tree" or device$="Hersey" or device$="EZReader" then
		goto HH_OTHER_TYPE1_READ
	end if

	HH_W_END: !
	fncloseprn
	addmethod=am_askAndEnterIndviduals ! set back to regular readings
	close #h_readings:
fnend
EST1: ! r: ESTIMATEING ROUTINE
	close #hWork:
	execute 'Index '&workFile$&' '&workFileIndex$&' 1 10 Replace,DupKeys -n'
	open #hWork:=fnH: "Name="&workFile$&",KFName="&workFileIndex$,internal,outIn,keyed
	ASK_EST: !
	fnTos
	! services=0
	if srvnam$(1)="Water" then srvest$(1)=srvnam$(1) else srvest$(1)="Unused"
	if service_enabled(3) then srvest$(2)=srvnam$(3) else srvest$(2)="Unused"
	if service_enabled(4) then srvest$(3)=srvnam$(4) else srvest$(3)="Unused"
	mylen=0
	for j=1 to 3 : mylen=max(mylen,len(srvest$(j))) : next j
	fnFra(1,1,8,mylen+50,"Select and Configure Services to Estimate")
	fnLbl(2,mylen+12,"% of Average",15,0,0,1)
	for j=1 to 3
		resp$(j*2-1)="False" : resp$(j*2)=""
		if srvest$(j)="" or srvest$(j)="Unused" then disable=1 else disable=0
		fnChk(j+2,mylen,srvest$(j),0,1) ! add disable here
		! fnTxt(J+2,MYLEN+4,2,0,0,"30",DISABLE,EMPTY$,1)
		fnTxt(j+2,mylen+14,3,0,0,"30",disable,empty$,1)
	next j
	fnLbl(7,1,"% of average would normally be from 75 to 125 %",52,2,0,1)
	fnFra(11,1,3,50,"Account Selection Method")
	fnOpt(1,1,"Individual Accounts",0,2)
	resp$(7)="False"
	fnOpt(2,1,"Route",0,2)
	resp$(8)="True"
	fnCmdSet(2)
	fnAcs(mat resp$,ckey)
	if ckey=cancel then goto MENU1
	for j=1 to 3
		if uprc$(resp$(j*2-1))=uprc$("True") then est1(j,1)=1 else est1(j,1)=0
		est1(j,2)=val(resp$(j*2)) conv EST1
	next j
	if est1(1,1)=0 and est1(2,1)=0 and est1(3,1)=0 then goto L6520 else goto L6530
	L6520: !
	mat message$(1)
	message$(1)="You must select at least one service to estimate"
	fnmsgbox(mat message$,resp$,'',0)
	goto ASK_EST
	L6530: for j=1 to 3
		if est1(j,1)=0 then goto L6570
		if est1(j,2)<50 or est1(j,2)>150 then goto L6560 else goto L6570
	L6560: !
		mat message$(1)
		message$(1)="You percent must be between 50% and 150%"
		fnmsgbox(mat message$)
		goto ASK_EST
	L6570: !
	next j
	if ckey=cancel then goto MENU1
	if uprc$(resp$(7))=uprc$("True") then est1=1
	if uprc$(resp$(8))=uprc$("True") then est1=2 ! select route #
	fn_est_dates
	if est1=1 then goto EST3 ! selecting individual accounts to estimate
	if est1=2 then goto ASK_ROUTE
goto ASK_EST ! /r
EST3: ! r:
	fnTos
	mylen=27 : mypos=mylen+2
	fnLbl(1,1,"Account to Estimate:",mylen,1)
	fncmbact(1,mypos)
	resp$(1)=""
	if ex$<>"" then
		fnLbl(3,1,"Last Account entered:",mylen,1)
		fnTxt(3,mypos,10,0,0,empty$,1)
		resp$(2)=ex$
	end if
	fnCmdSet(11)
	fnAcs(mat resp$,ckey)
	x$=lpad$(trim$(resp$(1)(1:10)),10)
	if ckey=cancel or trim$(x$)="" then goto MENU1
	x$=lpad$(trim$(x$),10) conv EST3
	read #hCustomer1,using F_CUSTOMER_A,key=x$,release: x$,e2$,mat a,f,final,mat d,mat extra,extra$(3) nokey EST3
	fnapply_default_rates(mat extra, mat a)
	F_CUSTOMER_A: form pos 1,c 10,pos 41,c 30,pos 143,7*pd 2,pos 296,pd 4,pos 1821,n 1,pos 217,15*pd 5,pos 1741,n 2,n 7,2*n 6,n 9,pd 5.2,n 3,3*n 9,3*n 2,3*n 3,n 1,3*n 9,3*pd 5.2,c 30,7*c 12,3*c 30
	gosub EST2
	ex$=x$
	goto EST3 ! /r
ASK_ROUTE: ! r:
	fnTos
	mylen=21 : mypos=mylen+2 : respc=0
	fnLbl(1,1,"Route to Estimate:",mylen,1)
	fncmbrt2(1,mypos,0)
	resp$(respc+=1)="1"
	if eb2>0 then
		fnLbl(3,1,"Last Route estimated:")
		fnTxt(3,mypos,4)
		resp$(respc+=1)=str$(eb2)
	end if
	fnCmdSet(11)
	fnAcs(mat resp$,ckey)
	if resp$(1)="[All]" then eb1=0 : goto L6890
	eb1=val(resp$(1))
	L6890: !
	if ckey=cancel then goto MENU1 ! finish
	restore #hCustomer1:
	READ_CUSTOMER: !
	read #hCustomer1,using F_CUSTOMER_A,release: x$,e2$,mat a,f,final,mat d,mat extra,extra$(3) eof ASK_ROUTE
	if final=1 or final=2 or final=3 then goto READ_CUSTOMER
	fnapply_default_rates(mat extra, mat a)
	! fn_us1(x$,d1)
	if eb1>0 and extra(1)><eb1 then goto READ_CUSTOMER ! if route selected and does not match route
	if final=1 or final=3 then goto READ_CUSTOMER ! SKIP FINAL BILLED
	gosub EST2
	eb2=eb1
goto READ_CUSTOMER ! /r
EST2: ! r:
	mat x=(0) ! actually calculate the estimated usage
	EST2B: !
	a1=est4=0
	read #hWork,using Fwork,key=x$: x$,mat x nokey L7060
	a1=1
	t(1)-=1 ! Reverse Proof Totals
	for j=1 to 15 : t(j+1)=t(j+1)-x(j) : next j
	L7060: !
	for j=1 to 3
		if j=1 and a(1)=0 then
			goto L7140
		else if j=2 and a(3)=0 then
			goto L7140
		else if j=3 and a(4)=0 then
			goto L7140 ! took this off front 71509  If EST1(J,1)=0 Then Goto 6790 Else
		end if
		fn_est5
		if f=d1 then oldwatread=d(2) else oldwatread=d(1) ! old water reading equals the prior reading if recalculation else current reading if new calculation
		if f=d1 then oldelecread=d(6) else oldelecread=d(5) ! old electric reading equals the prior reading if recalculation else current reading if new calculation
		if f=d1 then oldgasread=d(10) else oldgasread=d(9) ! old gas reading equals the prior reading if recalculation else current reading if new calculation
		if j=1 then
			x(1)=oldwatread+watavg
		else if j=2 then
			x(3)=oldelecread+elecavg
		else if j=3 then
			x(2)=oldgasread+gasavg
		end if
		est4=1
		L7140: !
	next j
	! If A(2)>0 AND EST1(1,1)=1 Then eST4=1 ! Sewer
	if est4=0 then goto L7220
	if addmethod=am_loadHoldingFile then goto L7220 ! FROM Hand Held
	if a1=1 then
		rewrite #hWork,using Fwork: trim$(x$),mat x
		goto L7210
	end if
	fn_writeWork(hWork,x$,mat x)
	rewrite #hCustomer1,using "Form pos 1831,n 9",key=x$: d1 ! write billing date into bill estimated field  extra(19) any time bill estimated
	L7210: !
	fn_accumulateProofTotals
	L7220: !
return  ! /r
Xit: fnXit
def fn_us1(x$,d1)
	! rc1=0 ! SET USAGE FIELDS
	wr1=1 : er1=5 : gr1=9
	read #hCustomer1,using "Form POS 296,PD 4",key=x$,release: f nokey US1_XIT
	if f><d1 then goto US1_XIT
	wr1=2 : er1=6 : gr1=10 ! rc1=1 ! Re-Calculation
	US1_XIT: !
fnend
def fn_rmk1
	! rk$=x$(1:10)
	if ft$="" then goto RMK1_XIT
	ft$="*"&ft$
	! Read #note1,Using "Form POS 1,C 10,2*PD 3",Key=RK$: RK$,MAT RA Nokey 6580
	r32=ra(1)
	RMK1_L8110: !
	if r32=0 then goto RMK1_L8190
	! Read #note2,Using "Form POS 1,C 10,C 60,PD 3",Rec=R32: K32$,RM$,N32
	if rm$(1:1)><"*" then goto RMK1_L8160
	! Rewrite #note2,Using "Form POS 1,C 10,C 60,PD 3",Rec=R32: K32$,FT$
	goto RMK1_XIT
	RMK1_L8160: !
	r32=n32
	goto RMK1_L8110
	mat ra=(0)
	! Write #note1,Using "Form POS 1,C 10,2*PD 3": RK$,MAT RA
	RMK1_L8190: !
	r32=lrec(32)+1
	! Write #note2,Using "Form POS 1,C 10,C 60,PD 3",Rec=R32: RK$,FT$,0
	rn=rn+1
	! If RA(2)>0 Then
	! Rewrite #note2,Using "Form POS 68,PD 3",Rec=RA(2): R32
	! end if
	if ra(1)=0 then ra(1)=r32
	ra(2)=r32
	! Rewrite #note1,Using "Form POS 11,2*PD 3",Key=RK$: MAT RA
	RMK1_XIT: !
fnend
def fn_est_dates
	EST_DATES: !
	fnTos
	mylen=51 : mypos=mylen+2
	fnLbl(2,70,"",0,1)
	fnLbl(1,1,"Billing Dates of Months to be Averaged:",mylen,1)
	for j=1 to 8
		fnTxt(j,mypos,10,0,0,"3")
		resp$(j)=""
	next j
	fnCmdSet(2)
	fnAcs(mat resp$,ckey)
	if ckey=cancel then goto Xit
	for j=1 to 8
		cd1(j)=val(resp$(j)) conv EST_DATES
	next j
	if cd1(1)=0 then
		mat message$(1)
		message$(1)="You must enter at least one date!"
		fnmsgbox(mat message$)
		goto EST_DATES
	end if
fnend
def fn_est5(;___,j) ! calculate averages
	watermonths=elecmonths=gasmonths=watused=elecused=gasused=0
	restore #hTrans,key>=x$&"         ": nokey EST5_XIT ! no average but active customer (use 0 usage)
	EST5_READ_TRANS: !
	read #hTrans,using F_TRANS: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof EST5_FINIS
	F_TRANS: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
	if p$<>x$ then goto EST5_FINIS
	if tcode<>1 then goto EST5_READ_TRANS ! only charge transactions
	for j=1 to 8
		if est1(1,1)=1 and cd1(j)=tdate then watermonths+=1: watused+=wu
		if est1(2,1)=1 and cd1(j)=tdate then elecmonths+=1: elecused+=eu
		if est1(3,1)=1 and cd1(j)=tdate then gasmonths+=1 : gasused+=gu
	next j
	! probably a mat x proof total problem right here
	goto EST5_READ_TRANS
	EST5_FINIS: !
	watavg=elecavg=gasavg=0
	if watermonths>0 then watavg=int(watused/watermonths)
	if elecmonths>0 then elecavg=int(elecused/elecmonths)
	if gasmonths>0 then gasavg=int(gasused/gasmonths)
	EST5_XIT: ! write enter readings entry
fnend
def fn_rewrite_usage
	if servicetype$="WA" then x(12)=usage
	if servicetype$="GA" then x(14)=usage
	if servicetype$="EL" then x(13)=usage
	rewrite #hWork,using Fwork: trim$(x$),mat x
fnend
def fn_write_tamper(custno$*10,tval)
	read #hCustomer1,using TMPFORM,key=lpad$(trim$(custno$),10): tmp$ nokey WT_XIT
	rewrite #hCustomer1,using TMPFORM: lpad$(str$(tval),2)
	TMPFORM: form pos 438,c 2
	WT_XIT: !
fnend
ImportTabDelimited: ! r:
	! r: phase 1 - import from text file into readings.tmp file
	open #h_tmp:=fnH: "Name=OPEN:Tab Delimited Text (*.txt) |*.txt,RecL=129,Shr",display,input ioerr IT_XIT
	!   open #h_tmp:=2: "Name=L:\readings.txt,RecL=129",display,input
	open #h_readings_tmp:=fnH: "Name=[Temp]\readings.tmp,RecL=30,replace",display,output
	dim a$*256
	do
		IT_TEXT_READ: !
		linput #h_tmp: a$ eof IT_TEXT_EOF
		x=val(a$(1:3)) conv IT_TEXT_READ
		z$=""
		for j=1 to 8
			x=val(a$(j:j)) conv IT_L1060
			z$=z$&a$(j:j)
		next j
		IT_L1060: !
		z=val(z$)
		z$=cnvrt$("pic(zzzzzzz.##",z)
		reading$=""
		for j1=1 to 20
			x=val(a$(j1+j:j1+j)) conv IT_L1120
			reading$=reading$&a$(j1+j:j1+j)
			IT_L1120: !
		next j1
		pr #h_readings_tmp,using "form pos 1,c 10,c 9": z$,trim$(reading$)
	loop
	IT_TEXT_EOF: !
	close #h_tmp: ioerr ignore
	close #h_readings_tmp: ioerr ignore
	! /r
	! r: phase 2 - from readings.tmp file
	close #h_readings: ioerr ignore
	open #h_readings:=13: "Name=[Temp]\Readings.tmp,RecL=30",display,input
	do
		linput #h_readings: ln$ eof IT_FINIS
		mat x=(0)
		x$=lpad$(rtrm$(ln$(1:10)),10) conv IT_W_NEXT ! Account Key
		ti1=1 ! water
		x(ti1)=0
		! if env$('client')="Lovington" then
		! 	x(ti1)=val(ln$(11:19)) conv ignore
		! else
			x(ti1)=val(ln$(11:20)) conv ignore
		! end if
		read #hCustomer1,using F_CUSTOMER_C,key=x$,release: x$,aname$,mat a nokey ignore
		!       goto HH_CONTINUE

		!   HH_CONTINUE: ! Continue with standard Hand Held routine
		read #hCustomer1,using F_CUSTOMER_C,key=x$,release: x$,aname$,mat a,final,mat d,alp$,mat extra,extra$(3) nokey IT_W_NEXT
		fnapply_default_rates(mat extra, mat a)
		fn_us1(x$,d1)
		mat est1=(0)
		if x(1)=999999 then est1(1,1)=1 : est1(1,2)=100
		if x(2)=999999 then est1(3,1)=1 : est1(3,2)=100
		if x(3)=999999 then est1(2,1)=1 : est1(2,2)=100
		if sum(est1)<>0 then
			read #hCustomer1,using F_CUSTOMER_A,key=x$,release: x$,e2$,mat a,f,final,mat d,mat extra,extra$(3) nokey IT_W_NEXT
			fnapply_default_rates(mat extra, mat a)
			gosub EST2B
		end if
		gosub CheckUnusual
		fn_writeWork(hWork,x$,mat x)
		fn_accumulateProofTotals
		fn_rmk1
		IT_W_NEXT: !
	loop
	! /r
	IT_FINIS: !
	fncloseprn
	addmethod=am_askAndEnterIndviduals ! set back to regular readings
	close #h_readings,free:
	fnFree(workFileIndex$)
	IT_XIT: !
goto MENU1 ! /r
MENU1: ! r:
	editmode=0
	addmethod=0
	fnTos
	mylen=28 : mypos=mylen+3
	frame_bd_witdh=42
	fnFra(1,1,5,frame_bd_witdh,"Batch Data")
	disable=0 ! If LREC(hWork)>1 Then dISABLE=1 Else dISABLE=0
	fnLbl(2,2,"Billing Date:",mylen,1,0,1)
	fnTxt(2,mypos,8,0,0,"1001",disable,empty$,1)
	resp$(1)=str$(d1)
	fnLbl(4,2,"Meter Reading Date:",mylen,1,0,1)
	fnTxt(4,mypos,8,0,0,"1",disable,empty$,1)
	resp$(2)=str$(d2)

	moe_button_width=frame_bd_witdh-1
	fnFra(8,1,20,moe_button_width+1,"Add Readings") : frame_current=2 : frame_line=0
	fnLbl(frame_line+=2,2,"Individuals:",0,0,0,frame_current)
	fnButton(frame_line+=1,1,"Display customers in route sequence",fky_askCustomersInSequence:=2001,'Display each customer in route sequence',0,moe_button_width,frame_current)
	fnButton(frame_line+=1,1,"Ask Account, then enter Reading",fky_askAndEnterIndviduals:=2002,'Ask Account, then enter Reading',0,moe_button_width,frame_current)

	fnLbl(frame_line+=2,2,"Bulk:",0,0,0,frame_current)
	fnButton(frame_line+=1,1,"Load Holding File",fky_loadHoldingFile:=2003 ,'Retrieve readings previously saved to a Holding File',0,moe_button_width,frame_current)
	fnButton(frame_line+=1,1,"Estimate Readings",fky_estimateReadings:=2004,'',0,moe_button_width,frame_current)
	fnButton(frame_line+=1,1,"Import from Tab Delimited Text File",fky_importTabDelimited:=2006,'',0,moe_button_width,frame_current)

	if fnregistered_for_hh then
		! fnLbl(frame_line+=2,2,"Hand Held:",0,0,0,frame_current)
		! fnButton(frame_line+=1,1,'Import from Hand Held to Book'     ,fky_importHHtoBook:=2007,'Retrieve Hand Held File'                                                               ,0,moe_button_width,frame_current)
		! fnButton(frame_line+=1,1,'Load Hand Held Book'               ,fky_loadBook:=2005      ,'Generally for use after "Retreive (Import) from Hand Held to Book"'                    ,0,moe_button_width,frame_current)
		fnButton(frame_line+=2,1,'Hand Held Books'               ,fky_loadBook:=2005      ,'Generally for use after "Retreive (Import) from Hand Held to Book"'                    ,0,moe_button_width,frame_current)

		! fnButton(frame_line+=1,1,'Import and Load Hand Held (Book 1)',fky_importAndLoad:=2009 ,'Completes "Import to Hand Held to Book" (using book 1) and then "Load Hand Held Book".',0,moe_button_width,frame_current)
	end if  ! fnregistered_for_hh
! r: add the grid

	chc=0
	mat colhdr$(30)
	mat cm$(30)
	mat item$(30)
	colhdr$(chc+=1)="Account"
	reporth$="   Account  Customer Name              Customer Address           "
	form$="Form pos 1,c 10,x 2,c 25,x 2,c 25"
! r: Service 1 - Water
	if service_enabled(1) then
		colhdr$(chc+=1)=srvnam$(1)&" Reading"
		cm$(chc)="20"
		colhdr$(chc+=1)=srvnam$(1)&" Charge"
		cm$(chc)="10"
		colhdr$(chc+=1)=srvnam$(1)&" Usage"
		cm$(chc)="20"

		reporth$=reporth$&srvnam$(1)(1:2)&" Read   "&srvnam$(1)(1:2)&" Chg  "&srvnam$(1)(1:2)&" Usage "
		form$=form$&",n 9,n 9.2,n 9"
	end if
! /r
! r: Service 2 - Sewer
	if service_enabled(2) then
		colhdr$(chc+=1)=srvnam$(2)&" Charge"
		cm$(chc)="10"

		reporth$=reporth$&" "&srvnam$(2)(1:2)&" Chg  "
		form$=form$&",n 9.2"
	end if
! /r
! r: Service 3 - Electric
	if service_type(3)=3 then
		colhdr$(chc+=1)=srvnam$(3)&" Reading"
		cm$(chc)="20"
		colhdr$(chc+=1)=srvnam$(3)&" Charge"
		cm$(chc)="10"
		colhdr$(chc+=1)=srvnam$(3)&" Usage"
		cm$(chc)="20"
		colhdr$(chc+=1)="Demand"
		cm$(chc)="20"

		reporth$=reporth$&srvnam$(3)(1:2)&" Read   "&srvnam$(3)(1:2)
		reporth$=reporth$&" Chg  "&srvnam$(3)(1:2)&" Usage   Demand "
		form$=form$&",n 9,n 9.2,n 9,n 9"
	else if service_type(3)=3.1 then
		colhdr$(chc+=1)=srvnam$(3)&" Reading"
		cm$(chc)="20"
		colhdr$(chc+=1)=srvnam$(3)&" Charge"
		cm$(chc)="10"
		colhdr$(chc+=1)=srvnam$(3)&" Usage"
		cm$(chc)="20"

		reporth$=reporth$&srvnam$(3)(1:2)&" Read   "&srvnam$(3)(1:2)
		reporth$=reporth$&" Chg  "&srvnam$(3)(1:2)&" Usage "
		form$=form$&",n 9,n 9.2,n 9"
	else if service_type(3)=3.2 then
		colhdr$(chc+=1)=srvnam$(3)&" Usage"
		cm$(chc)="20"

		! LOOKS LIKE SOMETHING IS MiSSING HERE
	end if
! /r
! r: Service 4 - Gas
	if service_enabled(4) then ! if service_type(4)=4 then
		colhdr$(chc+=1)=srvnam$(4)&" Reading"
		cm$(chc)="20"
		colhdr$(chc+=1)=srvnam$(4)&" Charge"
		cm$(chc)="10"
		colhdr$(chc+=1)=srvnam$(4)&" Usage"
		cm$(chc)="20"

		reporth$=reporth$&srvnam$(4)(1:2)&" Read   "&srvnam$(4)(1:2)
		reporth$=reporth$&" Chg  "&srvnam$(4)(1:2)&" Usage "
		form$=form$&",n 9,n 9.2,n 9"
	end if
! /r
! r: Service 5 - Oother
	if service_enabled(5) then ! always show "Other Charge"
		colhdr$(chc+=1)=srvnam$(5)&" Charge"
		cm$(chc)="10"

		reporth$=reporth$&" "&srvnam$(5)(1:2)&" Chg  "
		form$=form$&",n 9.2"
	end if
! /r
! r: Service 6
	if service_enabled(6) then ! always show "Other Charge"
		colhdr$(chc+=1)=srvnam$(6)&" Charge"
		cm$(chc)="10"

		reporth$=reporth$&" "&srvnam$(6)(1:2)&" Chg  "
		form$=form$&",n 9.2"
	end if
! /r
! r: Service 7
	if service_enabled(7) then ! always show "Other Charge"
		colhdr$(chc+=1)=srvnam$(7)&" Charge"
		cm$(chc)="10"

		reporth$=reporth$&" "&srvnam$(7)(1:2)&" Chg  "
		form$=form$&",n 9.2"
	end if
! /r
! r: Service 8
	if service_enabled(8) then ! always show "Other Charge"
		colhdr$(chc+=1)=srvnam$(8)&" Charge"
		cm$(chc)="10"

		reporth$=reporth$&" "& srvnam$(8)(1:2)&" Chg  "
		form$=form$&",n 9.2"
	end if
! /r
! r: final billing code
	colhdr$(chc+=1)=" F/B"
	cm$(chc)="30"

	reporth$=reporth$&" Final "

	form$=form$&",n 9"
! /r
	mat colhdr$(chc) : mat cm$(chc)

	fnflexinit1("Work",2,frame_bd_witdh+3,28,74,mat colhdr$,mat cm$,1)
	 entryCount=0
	ic=0
	restore #hWork:
	batchtot=0
	do
		read #hWork,using Fwork: x$,mat x eof MENU1READWORKEOF
		ic=0
		item$(ic+=1)=x$
		batchtot+=val(x$) conv L1100
		L1100: !
		if service_enabled(1) then
			item$(ic+=1)=str$(x(01))
			item$(ic+=1)=str$(x(09))
			item$(ic+=1)=str$(x(12)) ! water
		end if
		if service_enabled(2) then
			item$(ic+=1)=str$(x(05)) ! sewer
		end if
		if service_type(3)=3.2 then
			item$(ic+=1)=str$(x(13)) ! eletric
		else if service_type(3)=3 then
			item$(ic+=1)=str$(x(03))
			item$(ic+=1)=str$(x(10))
			item$(ic+=1)=str$(x(13))
			item$(ic+=1)=str$(x(04)) ! eletric
		end if
		if service_type(3)=3.1 then
			item$(ic+=1)=str$(x(03))
			item$(ic+=1)=str$(x(10))
			item$(ic+=1)=str$(x(13))
		end if
		if service_enabled(4) then
			item$(ic+=1)=str$(x(02))
			item$(ic+=1)=str$(x(11))
			item$(ic+=1)=str$(x(14)) ! gas
		end if
		if service_enabled(5) then ! service 5
			item$(ic+=1)=str$(x(06))
		end if
		if service_enabled(6) then ! service 6
			item$(ic+=1)=str$(x(07))
		end if
		if service_enabled(7) then ! service 7
			item$(ic+=1)='' ! str$(x(07))  ! x(??)   07 is used in another place for service 7 but it is also used for service 6
		end if
		if service_enabled(8) then ! service 8
			item$(ic+=1)=str$(x(08))
		end if
		item$(ic+=1)=str$(x(15)) ! final billing code
		entryCount+=1
		fnflexadd1(mat item$) ! pr mat item$ : pause
	loop
MENU1READWORKEOF: ! /r
	fnLbl(1,frame_bd_witdh+4,'Entry Count: '&str$(entryCount))
	fnButton(1,frame_bd_witdh+21,'Clear All',fky_clearAll:=2008)
	if lrec(hWork)>0 then
!   fnCmdKey("&Add",1)
		fnCmdKey("E&dit",2,1,0,'Edit highlighted record by clicking this button, pressing enter or double clicking the record.')
		fnCmdKey("&Print",4,0,0,'Print a proof listing of the entered records.')
		fnCmdKey("Save to &Holding File",fkey_saveToHoldingFile:=6,0,0,'Save entered readings to a Holding File for later calculation.')
		fnCmdKey("&Delete",8)
		fnCmdKey("&Close",5,0,1)
		fnCmdKey("&Meter Change",9,0,0,"Calculates usage on meter change out.")
		fnCmdKey("&Finish and Calculate",10,0,0,'Calculate entered readings')
	else
		fnCmdKey("&Close",5,0,1)
		! fnCmdSet(1)
	end if
	fnAcs(mat resp$,ckey)
	if ckey=cancel then
		goto Xit
	end if
	d1=val(resp$(1))
	d2=val(resp$(2))
	fnLastBillingDate(d1,1)
	fncreg_write('Meter Reading Date Current',str$(d2))
	x$=lpad$(trim$(resp$(3)(1:10)),10) ! formerly resp$(9)
	if lrec(hWork)>0 and ckey=2 then
		goto MAKE_CORRECTIONS
	end if
	if ckey=4 then
		fn_print_readings(hWork)
	else if fkey_saveToHoldingFile and ckey=fkey_saveToHoldingFile then ! add to holding file
		if fn_holdingFileSave(hWork) then goto Xit
	else if ckey=8 then
		delete #hWork,key=x$:
	else if ckey=9 then
		if fn_meter_change_out=3 then goto EnterReadings3
	else if ckey=10 then
		if days(d1,"mmddyy")<days(date$)-25 then
			ok_click=msgbox('The billing date entered is over three weeks old. Please enter the correct date or contact ACS support.','Old Billing Date',"OK","EXCL")
			goto menu1
			end if
		fnchain("S:\Utility Billing\Calculate Bills") ! goto CALCULATE
	else if ckey=fky_askCustomersInSequence then
		addmethod=am_customersInSequence
		goto AUTO_REC
	else if ckey=1 or (fky_askAndEnterIndviduals and ckey=fky_askAndEnterIndviduals) then
		addmethod=am_askAndEnterIndviduals
		goto SEL_ACC
	else if fky_loadHoldingFile and ckey=fky_loadHoldingFile then
		addmethod=am_loadHoldingFile
		fn_loadBookOrHoldingFile(addmethod)
	else if fky_estimateReadings and ckey=fky_estimateReadings then
		addmethod=am_estimateReadings
		goto EST1
	else if ckey=fky_loadBook then
		addmethod=am_fromHhFile
		fn_loadBookOrHoldingFile(addmethod)
	else if fky_importTabDelimited and ckey=fky_importTabDelimited then
		addmethod=am_importTabDelimited
		goto ImportTabDelimited
	else if fky_importHHtoBook and ckey=fky_importHHtoBook then
		fnRetrieveHandHeldFile
		fnTop(program$)
	else if fky_clearAll and ckey=fky_clearAll then
		close #hWork:
		fnFree(workFile$)
		fnFree(workFileIndex$)
		open #hWork:=fnH: "Name="&workFile$&",KFName="&workFileIndex$&",Shr,Use,RecL=74,KPs=1,KLn=10",internal,outIn,keyed
	else if fky_importAndLoad and ckey=fky_importAndLoad then
		fnRetrieveHandHeldFile
		fnTop(program$)
		addmethod=am_fromHhFile
		fn_loadBookOrHoldingFile(addmethod)

	end if
goto MENU1 ! /r

def fn_holdingFileLoad(; ___,hld9)
	holdingFile$="[Q]\UBmstr\IpHold"&ip1$&".h[cno]"
	open #hld9=fnH: "Name="&holdingFile$,internal,input ioerr L7460 ! was =9
	do
		read #hld9,using Fwork: x$,mat x eof IPHOLD_EO_HLD9
		fn_writeWork(hWork,x$,mat x, 1)
		fn_accumulateProofTotals
	loop
	IPHOLD_EO_HLD9: !
	close #hld9:
	L7460: !
	addmethod=am_customersInSequence ! set addmethod back to 1 once holding file read in
fnend
def fn_holdingFileSave(hWork) ! probably requires more than just hWork
	holdingFileSaveReturn=0
	HoldingFileSave: !
	fnTos
	mylen=19 : mypos=mylen+2
	fnLbl(1,1,"Holding File Number:",mylen)
	fnTxt(1,mypos,3,0,0,"30")
	resp$(1)=""
	fnFra(4,1,3,94,"Create new file or append to existing file","If you have a different file for each route, you will always take the option to create a new file.  If you only use one file, clean it on the first batch of readings and append the rest.")
	fnOpt(1,1,"Create new file (deletes all previous readings in holding file)",0,1)
	resp$(respc_CreateNew:=2)="False"
	fnOpt(2,1,"Append to existing file (retain previous readings, merge new ones in, overwrites duplicates)",0,1)
	resp$(3)="True"
	fnCmdKey("&Save",1,1)
	fnCmdKey("&Cancel",5,0,1)
	fnAcs(mat resp$,ckey)
	if ckey<>cancel then
		holdingFileSaveReturn=1
		bk1=val(resp$(1)) conv HoldingFileSave
		if bk1<=0 then goto HoldingFileSave
		if uprc$(resp$(respc_CreateNew))=uprc$("True") and exists('[Q]\UBmstr\IpHold'&str$(bk1)&'.h[cno]') then ! Create New Holding File
			fnFree('[Q]\UBmstr\IpHold'&str$(bk1)&'.h[cno]')
		end if
		! Append to Existing Holding File
		dim holdingFile$*256
		dim holdingFileIndex$*256
		holdingFile$="[Q]\UBmstr\IpHold"&str$(bk1)&".h[cno]"
		holdingFileIndex$=env$('temp')&"\acs\IpHold"&str$(bk1)&"-Index.h[cno]"
		fnIndex(holdingFile$,holdingFileIndex$,'1 10')
		open #hld8:=fnH: "Name="&holdingFile$&",KFName="&holdingFileIndex$&',Shr,Use,RecL=74,KPs=1,KLn=10',internal,outIn,keyed
		restore #hWork: ! ,search>="": nokey AppendFinis
		do
			read #hWork,using Fwork: x$,mat x eof AppendFinis
			fn_writeWork(hld8,x$,mat x, 1)
		loop
		AppendFinis: !
		close #hld8:
		fnStatusClose
		close #hWork:
		fnFree(workFile$)
		fnFree(workFileIndex$)
	end if
	fn_holdingFileSave=holdingFileSaveReturn
fnend
def fn_loadBookOrHoldingFile(&addmethod; ___,book_or_holding_file$,ihDirFileMask$*64)
	if addmethod=am_fromHhFile then
		book_or_holding_file$='Book'
		ihDirFileMask$='Readings.*'
	else if addmethod=am_loadHoldingFile then
		book_or_holding_file$='Holding File'
		ihDirFileMask$='IPHold*.h[cno]'
	else
		pr bell;'addmethod not recognized by INPUT_HAND routine.' : fnpause : goto IH_XIT
	end if
	INPUT_HAND: !
	fnTos
	txt$="Select "&book_or_holding_file$&" for Input:"
	mylen=len(txt$)+1 : mypos=mylen+2
	fnLbl(1,1,txt$,mylen,1)
	! r: book or holding file grid
	colhdr$(1)=book_or_holding_file$ ! "Book"
	colhdr$(2)="Size"
	colhdr$(3)="Date Time"
	colhdr$(4)="Comment"
	
	dim bookItem$(0)*128
	mat bookItem$(4)
	mat colhdr$(4)
	ihFileCount=fngetdir2('[Q]\UBmstr\',mat ihFilename$, '',ihDirFileMask$,mat ihFileDate$,mat ihFileTime$,0,mat ihFileSize)
	fnflexinit1("book_"&book_or_holding_file$(1:1),1,mypos,10,32,mat colhdr$,mat cm2$,1)

	for ihFileItem=1 to ihFileCount
		if book_or_holding_file$='Book' then

			tmpBookNumber=val(ihFilename$(ihFileItem)(10:len(ihFilename$(ihFileItem)))) conv ihInvalidFile
			bookItem$(1)=str$(tmpBookNumber)
			bookItem$(2)=cnvrt$("pic(zzz,zzz,zzz,zzz)",ihFileSize(ihFileItem))
			bookItem$(3)=ihFileDate$(ihFileItem)&' '&ihFileTime$(ihFileItem)
			dim bookComment$*128
			fncreg_read('book comment '&str$(tmpBookNumber),bookComment$)
			bookItem$(4)=bookComment$
			fnflexadd1(mat bookItem$)
		else ! if book_or_holding_file$='Holding File' then
			ihTmpHoldingFileNumber$=ihFilename$(ihFileItem)(7:pos(ihFilename$(ihFileItem),'.',-1)-1) conv ihInvalidFile
			if ihTmpHoldingFileNumber$='-index' then goto ihInvalidFile
			bookItem$(1)=ihTmpHoldingFileNumber$
			bookItem$(2)=cnvrt$('pic(zzz,zzz,zzz,zzz)',ihFileSize(ihFileItem))
			bookItem$(3)=ihFileDate$(ihFileItem)
			bookItem$(4)=ihFileTime$(ihFileItem)
			fnflexadd1(mat bookItem$)
		end if
		ihInvalidFile: !
	next ihFileItem
	fnLbl(11,1,' ',15,1)
	! /r
	fnCmdKey('&Load'                 ,1                   ,1)
	fnCmdKey('I&mport from Hand Held',ck_importHHtoBook=3)
	fnCmdKey('&Delete'               ,ck_delete=4)
	fnCmdKey('&Print'                ,ck_print=6)
	fnCmdKey('&Cancel'               ,cancel              ,0,1)
	fnAcs(mat resp$,ckey)
	holdingFile$=''
	ip1$=resp$(1)
	if ckey=ck_importHHtoBook then
		fnRetrieveHandHeldFile
		fnTop(program$)
		goto INPUT_HAND
	else if ckey=cancel or ip1$='' then
		goto IH_XIT
	else if ckey=ck_print then
		if book_or_holding_file$='Holding File' then
			open #hpHoldingFile:=fnH: "Name=[Q]\UBmstr\IpHold"&ip1$&".h[cno]",internal,outIn,relative
			fn_print_readings(hpHoldingFile, 'Holding File '&ip1$)
			close #hpHoldingFile:
		else
			fn_hh_readings(ip1$, 1) ! pr for Books
		end if
		goto INPUT_HAND

	else if ckey=ck_delete then
		if fnConfirmDelete(book_or_holding_file$&' '&ip1$,'confirmDeleteBook') then ! resp$="Yes" then
			if addmethod=am_fromHhFile then
				fnFree("[Q]\UBmstr\Readings."&ip1$)
			else if addmethod=am_loadHoldingFile then
				fnFree("[Q]\UBmstr\IPHold"&ip1$&".h[cno]")
			end if
		end if
		goto INPUT_HAND
	else if addmethod=am_loadHoldingFile then
		fn_holdingFileLoad
	else
		fn_hh_readings(ip1$)
	end if
	IH_XIT: !
fnend
EnterReadings: ! r:
	if alp$="*" then goto READ_ROUTE_SEQUENCE
	EnterReadings2: !
	fn_us1(x$,d1)
	EnterReadings3: !
	fnTos
	rc=0 : frac=0

	fnFra(1,1,3,39,"Account Data")
	mylen=15 : mypos=mylen+2 : fraad=frac+=1
	fnLbl(1,1,"Account:",mylen,1,0,fraad)
	fnTxt(1,mypos,10,0,0,empty$,1,empty$,1)
	resp$(rc+=1)=x$
	fnLbl(2,1,"Name:",mylen,1,0,fraad)
	fnTxt(2,mypos,30,30,0,empty$,1,empty$,fraad)
	resp$(rc+=1)=aname$
	fnLbl(3,1,"Meter Address:",mylen,1,0,fraad)
	fnTxt(3,mypos,10,0,0,empty$,1,empty$,fraad)
	resp$(rc+=1)=e2$

	fnFra(7,1,12,60,"Readings & Overrides")
	mylen=0 : for j=1 to 8 : mylen=max(mylen,len(srvnam$(j))) : next j
	mypos1=mylen+2 : mypos2=mypos1+12
	mypos3=mypos2+12 : mypos4=mypos3+12 : mypos5=mypos4+12+4
	lc=0 : fraro=frac+=1
	fnLbl(lc+=1,mypos1,"Reading",10,2,0,fraro)
	fnLbl(lc,mypos2,"Charge",10,2,0,fraro)
	fnLbl(lc,mypos3,"Usage",10,2,0,fraro)
	if srvnam$(3)="Electric" then
		fnLbl(lc,mypos4,"Demand",10,2,0,fraro)
	end if
	! r: Service 1 - Water
	tmpService=1
	first_read_rc=rc
	if (a(1)=0) and (a(2)=0) then disa=1 else disa=0 ! water and sewer rate codes
	! rate 9 should still process ! if (a(1)=9 or a(1)=0) and (a(2)=9 or a(2)=0) then disa=1 else disa=0 ! water and sewer rate codes
	if onlyMonth(tmpService)>0 and onlyMonth(tmpService)<>date(days(d1,'mmddyy'),'mm') then disa=1
	! water
	if service_enabled(tmpService) then
		lc+=1
		fnLbl(lc,1,srvnamc$(1),mylen,1,0,2)
		fnTxt(lc,mypos1,10,11,1,"20",disa,empty$,fraro) ! reading
		fnTxt(lc,mypos2,10,10,1,"10",disa,empty$,fraro) ! charge
		fnTxt(lc,mypos3,10,11,1,"20",disa,empty$,fraro) ! usage
		if editmode=1 then
			resp$(rc+=1)=str$(x(tmpService)) ! water reading
			resp$(rc+=1)=str$(x(09)) ! water charge
			resp$(rc+=1)=str$(x(12)) ! water used
		else
			resp$(rc+=1)=''
			resp$(rc+=1)=''
			resp$(rc+=1)=''
		end if
	end if ! /r
	! r: Service 2 - Sewer
	tmpService=2
	if a(tmpService)=0 then disa=1 else disa=0 ! sewer rate code
	if onlyMonth(tmpService)>0 and onlyMonth(tmpService)<>date(days(d1,'mmddyy'),'mm') then disa=1
	if service_enabled(tmpService) then
		fnLbl(lc+=1,1,srvnamc$(tmpService),mylen,1,0,2)
		fnTxt(lc,mypos2,10,0,1,"10",disa,empty$,fraro) ! charge
		if editmode=1 then
			resp$(rc+=1)=str$(x(05)) ! sewer charge
		else
			resp$(rc+=1)=''
		end if
	end if ! /r
	! r: Service 3 - Electric
	tmpService=3
	if a(tmpService)=0 then disa=1 else disa=0 ! electric rate code
	if onlyMonth(tmpService)>0 and onlyMonth(tmpService)<>date(days(d1,'mmddyy'),'mm') then disa=1

	if service_type(tmpService)=3 then
		fnLbl(lc+=1,1,srvnamc$(tmpService),mylen,1,0,2)
		fnTxt(lc,mypos1,10,11,1,"20",disa,empty$,fraro) ! reading
		fnTxt(lc,mypos2,10,10,1,"10",disa,empty$,fraro) ! charge
		fnTxt(lc,mypos3,10,11,1,"20",disa,empty$,fraro) ! usage
		fnTxt(lc,mypos4,10,11,1,"20",disa,empty$,fraro) ! demand
		if editmode=1 then
			resp$(rc+=1)=str$(x(tmpService)) ! electric reading
			resp$(rc+=1)=str$(x(10)) ! electric charge
			resp$(rc+=1)=str$(x(13)) ! electric usage
			resp$(rc+=1)=str$(x(04)) ! electric demand
		else
			resp$(rc+=1)=""
			resp$(rc+=1)=""
			resp$(rc+=1)=""
			resp$(rc+=1)=""
		end if
	else if service_type(tmpService)=3.1 then
		fnLbl(lc+=1,1,srvnamc$(tmpService),mylen,1,0,2)
		fnTxt(lc,mypos1,10,11,1,"20",disa,empty$,fraro) ! reading
		fnTxt(lc,mypos2,10,10,1,"10",disa,empty$,fraro) ! charge
		fnTxt(lc,mypos3,10,11,1,"20",disa,empty$,fraro) ! usage
		if editmode=1 then
			resp$(rc+=1)=str$(x(03)) ! electric reading
			resp$(rc+=1)=str$(x(10)) ! electric charge
			resp$(rc+=1)=str$(x(13)) ! electric usage
		else
			resp$(rc+=1)=""
			resp$(rc+=1)=""
			resp$(rc+=1)=""
		end if
	else if service_type(tmpService)=3.2 then
		if (a(2)=0) then disa=1 else disa=0 ! water rate code
		! rate 9 should still process ! if (a(1)=9 or a(1)=0) and (a(2)=9 or a(2)=0) then disa=1 else disa=0 ! water rate code
		if onlyMonth(tmpService)>0 and onlyMonth(tmpService)<>date(days(d1,'mmddyy'),'mm') then disa=1
		fnLbl(lc+=1,1,srvnamc$(tmpService),mylen,1,0,2)
		fnTxt(lc,mypos3,10,11,1,"20",disa,empty$,fraro) ! usage
		if editmode=1 then
			resp$(rc+=1)=str$(x(13)) ! Reduction Usage
		else
			resp$(rc+=1)=""
		end if
	end if
	! /r
	! r: Service 4 - Gas
	tmpService=4
	if service_enabled(tmpService) then
		if a(tmpService)=0 then disa=1 else disa=0 ! gas rate code
		if onlyMonth(tmpService)>0 and onlyMonth(tmpService)<>date(days(d1,'mmddyy'),'mm') then disa=1
		lc+=1
		fnLbl(lc,1,srvnamc$(tmpService),mylen,1,0,2)
		fnTxt(lc,mypos1,10,11,1,"20",disa,empty$,fraro) ! reading
		fnTxt(lc,mypos2,10,10,1,"10",disa,empty$,fraro) ! charge
		fnTxt(lc,mypos3,10,11,1,"20",disa,empty$,fraro) ! usage
		if editmode=1 then
			resp$(rc+=1)=str$(x(02))
			resp$(rc+=1)=str$(x(11))
			resp$(rc+=1)=str$(x(14))
		else
			resp$(rc+=1)='' ! gas reading
			resp$(rc+=1)='' ! gas charge
			resp$(rc+=1)='' ! gas usage
		end if
	end if
	! /r
	! r: service 5
	tmpService=5
	if service_enabled(tmpService) then
		if a(tmpService)=0 then disa=1 else disa=0 ! service 5 rate code
		if onlyMonth(tmpService)>0 and onlyMonth(tmpService)<>date(days(d1,'mmddyy'),'mm') then disa=1
		if trim$(srvnam$(tmpService))="Reconnect Fee" then disa=0
		fnLbl(lc+=1,1,srvnamc$(tmpService),mylen,1,0,fraro)
		fnTxt(lc,mypos2,10,0,1,"10",disa,empty$,fraro)
		if editmode=1 then
			resp$(rc+=1)=str$(x(tmpService)) ! Service 5 charge
		else
			resp$(rc+=1)=''
		end if
	end if
	! /r
	! r: service 6
	tmpService=6
	if service_enabled(6) then
		if service_type(6)=5 or service_type(6)=7 or service_type(6)=8 then ! service 6 rate code
			disa=0
		else if extra(11)=0 then
			disa=1
		else
			disa=0
		end if
		fnLbl(lc+=1,1,srvnamc$(6),mylen,1,0,fraro)
		fnTxt(lc,mypos2,10,0,1,"10",disa,empty$,fraro) ! charge
		if editmode=1 then ! Service 6 charge
			resp$(rc+=1)=str$(x(tmpService))
		else
			resp$(rc+=1)=''
		end if
	end if
	! /r
	! r: Service 7
	tmpService=7
	if service_enabled(tmpService) then
		if service_type(tmpService)=5 then
			disa=0 ! don't disable other charge
		else if extra(12)=0 then
			disa=1
		else
			disa=0 ! service 7 rate code
		end if
		if onlyMonth(tmpService)>0 and onlyMonth(tmpService)<>date(days(d1,'mmddyy'),'mm') then disa=1
		fnLbl(lc+=1,1,srvnamc$(tmpService),mylen,1,0,2)
		fnTxt(lc,mypos2,10,0,1,"10",disa,empty$,2) ! charge
		if editmode=1 then ! Service 7 charge
			resp$(rc+=1)=str$(x(tmpService))
		else
			resp$(rc+=1)=''
		end if
	end if
	! /r
	! r: service 8
	tmpService=8
	if service_enabled(tmpService) then
		if service_type(tmpService)=5 or service_type(tmpService)=6 then ! don't disable other charge nor gas connect
			disa=0
		else if extra(13)=0 then
			disa=1
		else
			disa=0 ! service 8 rate code
		end if
		if onlyMonth(tmpService)>0 and onlyMonth(tmpService)<>date(days(d1,'mmddyy'),'mm') then disa=1
		lc+=1
		fnLbl(lc,1,srvnamc$(tmpService),mylen,1,0,2)
		fnTxt(lc,mypos2,10,0,1,"10",disa,empty$,fraro) ! charge
		if editmode=1 then
			resp$(rc+=1)=str$(x(tmpService)) ! Service 8 charge
		else
			resp$(rc+=1)=''
		end if
	end if
	! /r

	lc=lc+2
	fnLbl(lc,1,"Final Billing Code:",mylen+8,1,0,2)
	fncomboa("finalbill",lc,24,mat opt_final_billing$,"Used to record final billing code in customer record",28,fraro) ! final billing code
	resp_fianl_billing_code=(rc+=1)
	if editmode=1 then
		resp$(resp_fianl_billing_code)=str$(x(15)) ! Final Billing Code
	else
		resp$(resp_fianl_billing_code)=opt_final_billing$(final+1)
	end if
	if editmode=1 and x(15)=1 then resp$(resp_fianl_billing_code)=opt_final_billing$(2)
	if editmode=1 and x(15)=2 then resp$(resp_fianl_billing_code)=opt_final_billing$(3)
	begdate=fndate_mmddyy_to_ccyymmdd(d1)-20000
	fn_flexRead(1,mypos5+2,hTrans,x$,begdate,0,fraro) ! beginning date=billing date less one year
	fnCmdKey("&Meter Change",9,0,0,"Calculates usage on meter change out.")
	fnCmdKey("&Review Customer Record",8,0,0,"Allow you to review any customer while entering readings.")
	if addmethod=am_customersInSequence or addmethod=am_fromHhFile then fnCmdSet(17) else fnCmdSet(11) ! kj   3/24/06
	fnAcs(mat resp$,ckey)
	if ckey=8 then
		fncustomer(x): read #hCustomer1,using F_CUSTOMER_C,key=x$,release: x$,aname$,mat a,final,mat d,alp$,mat extra,extra$(3)
		fnapply_default_rates(mat extra, mat a)
		goto EnterReadings3
	end if
	rc=first_read_rc
	! If PASSCHECK=CKFAIL Then eDITMODE=0 ! xxx Ken
	if ckey=3 then done_with_readings=1 ! code as done with entering readings is select finish
	if service_enabled(1) then ! Service 1 - Water
		x(01)=val(resp$(rc+=1))
		x(09)=val(resp$(rc+=1))
		x(12)=val(resp$(rc+=1))
	end if
	if service_enabled(2) then ! Service 2 - Sewer
		x(05)=val(resp$(rc+=1))
	end if
	if service_type(3)=3 then ! electric
		x(03)=val(resp$(rc+=1))
		x(10)=val(resp$(rc+=1))
		x(13)=val(resp$(rc+=1))
		x(04)=val(resp$(rc+=1)) ! electric/lawn meter
	else if service_type(3)=3.1 then ! lawn meter
		x(03)=val(resp$(rc+=1))
		x(10)=val(resp$(rc+=1))
		x(13)=val(resp$(rc+=1))
	else if service_type(3)=3.2 then
		x(13)=val(resp$(rc+=1))
	end if  ! if srvnam$(3)=...
	if service_type(3)=3.1 and x(03)=0 and d(5)>0 and a(3)>0 then
		x(03)=d(5) ! if they skip reading the lawn meters, just write the previous reading into the current reading
	end if
	if service_enabled(4) then
		x(02)=val(resp$(rc+=1))
		x(11)=val(resp$(rc+=1))
		x(14)=val(resp$(rc+=1)) ! gas
	end if

	if service_enabled(5)=1 then ! service 5
		x(06)=val(resp$(rc+=1))
	end if

	if service_enabled(6)=1 then ! service 6
		x(07)=val(resp$(rc+=1))
	end if

	if service_enabled(7) then ! service 7
		x(07)=val(resp$(rc+=1))
	end if

	if service_enabled(8) then ! service 8
		x(08)=val(resp$(rc+=1))
	end if
	! pause
	rc+=1
	x(15)=val(resp$(resp_fianl_billing_code)(1:1)) ! final billing code
	if ckey=2 and addmethod=am_fromHhFile then
		skiprec=1
		goto L2910 ! if choose skip on pulling from hh file, then skip writing the record   ! kj 3/24/06
	else if addmethod=am_loadHoldingFile then
		goto CheckUnusual
	else if ckey=2 and editmode=0 then
		goto SEL_ACC
	else if ckey=2 and editmode=1 then
		goto MENU1
	else if ckey=3 or ckey=cancel then
		addmethod=0
		goto MENU1
	else if ckey=9 then
		if fn_meter_change_out=3 then goto EnterReadings3
		goto MENU1
	end if
	CheckUnusual: !
	if addmethod<>am_loadHoldingFile then mat mroll=(0)
	passcheck=ckpass=0 : ckfail=1 : ckcancel=2

	fn_checkWater
	if passcheck=ckfail then
		editmode=1
		goto EnterReadings3
	else if passcheck=ckcancel then
		goto EnterReadings_Finis
	end if

	fn_checkGas
	if passcheck=ckfail then
		editmode=1
		goto EnterReadings3
	else if passcheck=ckcancel then
		editmode=1
		goto EnterReadings3 ! Then Goto EnterReadings_Finis
	end if

	fn_checkElec
	if passcheck=ckfail then
		editmode=1
		goto EnterReadings3
	else if passcheck=ckcancel then
		goto EnterReadings_Finis
	end if

	L2910: !
	if addmethod=am_fromHhFile or addmethod=am_importTabDelimited then
		return  ! Hand Held or ImportTabDelimited
	end if

	if unusual<>2 then
		if editmode=0 then
			fn_writeWork(hWork,x$,mat x)
		else if editmode=1 then
			gosub REWRITE_WORK
		end if
	end if
	EnterReadings_Finis: !
	if addmethod=0 then goto MENU1
	if addmethod=am_customersInSequence and done_with_readings=0 then editmode=0 ! set editmode back after any corrections during the addmethod (am_customersInSequence)
	if addmethod=am_customersInSequence and editmode=0 then goto READ_ROUTE_SEQUENCE
	if addmethod=am_customersInSequence and editmode=1 then goto MENU1
	! If ADDMETHOD=am_customersInSequence AND EDITMODE=1 Then Goto READ_ROUTE_SEQUENCE ! MENU1
	if addmethod=am_askAndEnterIndviduals and (editmode=0 or editme=1) then mat x=(0): goto SEL_ACC ! kj 92407
	! If ADDMETHOD=am_askAndEnterIndviduals AND EDITMODE=1 Then Goto MENU1 ! kj 92407
goto MENU1 ! /r
def fn_setupFlexRead
	if ~setupFlexRead then
		setupFlexRead=1
		dim colmask$(30),frColHdr$(30)*20,serviceName$(10)*20,item$(25)*70
		dim tg(11),a(7)
		fnGetServices(mat serviceName$)
		tcode$(1)="Charge"
		tcode$(2)="Penalty"
		tcode$(3)="Collect"
		tcode$(4)="C/M"
		tcode$(5)="D/M"
	end if
fnend
def fn_flexRead(myline,mypos,filnum,z$,begdate,enddate,selcode) ! library ready
	if ~setupFlexRead then fn_setupFlexRead
	z$=trim$(z$)
	if z$<>'' then
		open #tmp=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed
		z$=lpad$(trim$(z$),10)
		read #tmp,using "Form Pos 143,7*pd 2",key=z$: mat a
		close #tmp:
	end if
	mat frColHdr$(30) : mat colmask$(30)
	frColHdr$(headers=1)="Date" : colmask$(headers)="3"
	if trim$(serviceName$(1))<>"" and (z$<>'' and a(1)>0) then
		frColHdr$(headers+=1)="Water Reading" : colmask$(headers)="20"
		frColHdr$(headers+=1)="Water Usage" : colmask$(headers)="20"
	end if
	if trim$(serviceName$(3))="Electric" and (z$<>'' and a(3)>0) then
		frColHdr$(headers+=1)="Electric Reading" : colmask$(headers)="20"
		frColHdr$(headers+=1)="Electric Usage" : colmask$(headers)="20"
	end if
	if trim$(serviceName$(3))="Lawn Meter" and (z$<>'' and a(3)>0) then
		frColHdr$(headers+=1)="Lawn Meter Reading" : colmask$(headers)="20"
		frColHdr$(headers+=1)="Lawn Meter Usage" : colmask$(headers)="20"
	end if
	if trim$(serviceName$(4))="Gas" and (z$<>'' and a(4)>0) then
		frColHdr$(headers+=1)="Gas Reading" : colmask$(headers)="20"
		frColHdr$(headers+=1)="Gas Usage" : colmask$(headers)="20"
	end if
	mat frColHdr$(headers)
	mat colmask$(headers)
	fnflexinit1("ubread",myline,mypos,13,30,mat frColHdr$,mat colmask$,1)
	items=0
	mat item$=('')
	restore #filnum,key>=rpad$(z$,kln(filnum)): nokey NO_RECORDS_FOUND
	do
		FlexReadCustomerRead: !
		read #filnum,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1',release: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof FlexReadXit
		if p$<>z$ then goto FlexReadXit ! not same account
		if (selcode>1 and tcode<>selcode-1) or (begdate>0 and tdate<begdate) or (enddate>0 and tdate>enddate) then goto FlexReadCustomerRead
		if tcode=0 then tcode=1 ! temporary to prevent bad transaction codes
		item$(1)=str$(tdate)
		items=1
		if trim$(serviceName$(1))<>"" and (z$<>'' and a(1)>0) then
			item$(items+=1)=str$(wr)
			item$(items+=1)=str$(wu)
		end if
		if trim$(serviceName$(3))="Electric" and (z$<>'' and a(3)>0) then
			item$(items+=1)=str$(er)
			item$(items+=1)=str$(eu)
		end if
		if trim$(serviceName$(3))="Lawn Meter" and (z$<>'' and a(3)>0) then
			item$(items+=1)=str$(er)
			item$(items+=1)=str$(eu)
		end if
		if trim$(serviceName$(4))<>"" and (z$<>'' and a(4)>0) then
			item$(items+=1)=str$(gr)
			item$(items+=1)=str$(gu)
		end if
		fnflexadd1(mat item$)
	loop

	NO_RECORDS_FOUND: !
		if items=0 then mat item$=("")
		fnflexadd1(mat item$)
	FlexReadXit: !
fnend
def fn_writeWork(hWork,x$,mat x; overwriteDupeAccount) ! write to hWork file

	if overwriteDupeAccount then
		rewrite #hWork,using Fwork,key=lpad$(trim$(x$),kln(hWork)): trim$(x$),mat x nokey ww_overwriteAdd
		goto ww_overwriteFinis
		ww_overwriteAdd: !
		write #hWork,using Fwork: trim$(x$),mat x
		ww_overwriteFinis: !
	else
		ww_writeWorkIncriment: ! write to hWork file
		rctr=lrec(hWork)+1
		write #hWork,using Fwork,rec=rctr: trim$(x$),mat x duprec ww_writeWorkIncriment
	end if
fnend
Fwork: form pos 1,cr 10,4*pd 5,7*pd 4.2,3*pd 5,n 1
def fn_meter_change_out
	mco_return=0
	do
		fnTos
		rc=0 : lc=0
		fnFra(1,1,2,49,"Method of Change Out")
		fnOpt(1,1,"Current customer only",0,1)
		resp$(1)="True"
		fnOpt(2,1,"All Customers",0,1)
		resp$(2)="False"
		fnLbl(5,1,"Service Type:",18,1)
		fncomboa("ServiceType",5,20,mat serviceoption$)
		resp$(3)=serviceoption$(1)
		fnLbl(6,1,"Beginning Customer:",18,1)
		fncmbact(6,20,1)
		resp$(4)="[All]"
		fnCmdKey("&Next",1,1,0): fnCmdKey("&Cancel",5,0,1)
		fnAcs(mat resp$,ckey)
		if ckey=5 then goto Mco_Xit
		servicetype$=resp$(3)(1:2)
		begx$=resp$(4)(1:10)
		if resp$(2)="True" then method$="File" : goto MCO_UPDATE_FULL_FILE ! working from a file
		if resp$(1)="True" then method$="Customer" : goto MCO_RECORD_READINGS
	loop
	MCO_RECORD_READINGS: !
	fnTos
	rc=0 : lc=0: resprc=0
	fnLbl(lc+=1,1,x$&"  "&aname$,50,0)
	fnLbl(lc+=2,32,"Old Meter",10,2)
	fnLbl(lc,55,"New Meter",10,2)
	fnLbl(lc+=1,25,"Prior",10,2)
	fnLbl(lc,37,"Current",10,2)
	fnLbl(lc,49,"Prior",10,2)
	fnLbl(lc,61,"Current",10,2)
	if trim$(servicetype$)="WA" then
		fnLbl(lc+=1,1,srvnamc$(1),20,1)
		fnTxt(lc,25,10,11,1,"30",0,"Enter the prior reading on the old meter")
		resp$(resprc+=1)=str$(d(1))
		fnTxt(lc,37,10,10,1,"30",0,"Enter the current reading on the old meter.")
		resp$(resprc+=1)=""
		fnTxt(lc,49,10,11,1,"30",0,"Enter the beginning reading the new meter")
		resp$(resprc+=1)=""
		fnTxt(lc,61,10,11,1,"30",0,"Enter the ending reading on new meter")
		resp$(resprc+=1)=str$(x(1))
	else if trim$(servicetype$)="EL" then
		fnLbl(lc+=1,1,srvnamc$(3),20,1)
		fnTxt(lc,25,10,11,1,"30",0,"Enter the prior reading on the old meter")
		resp$(resprc+=1)=str$(d(5))
		fnTxt(lc,37,10,10,1,"30",0,"Enter the current reading on the old meter.")
		resp$(resprc+=1)=""
		fnTxt(lc,49,10,11,1,"30",0,"Enter the beginning reading the new meter")
		resp$(resprc+=1)=""
		fnTxt(lc,61,10,11,1,"30",0,"Enter the ending reading on new meter")
		resp$(resprc+=1)=str$(x(3))
	else if trim$(servicetype$)="GA" then
		fnLbl(lc+=1,1,srvnamc$(4),20,1)
		fnTxt(lc,25,10,11,1,"30",0,"Enter the prior reading on the old meter")
		resp$(resprc+=1)=str$(d(9))
		fnTxt(lc,37,10,10,1,"30",0,"Enter the current reading on the old meter.")
		resp$(resprc+=1)=""
		fnTxt(lc,49,10,11,1,"30",0,"Enter the beginning reading the new meter")
		resp$(resprc+=1)=""
		fnTxt(lc,61,10,11,1,"30",0,"Enter the ending reading on new meter")
		resp$(resprc+=1)=str$(x(2))
	end if
	fnCmdKey("&Next",1,1,0): fnCmdKey("&Cancel",5,0,1)
	fnCmdKey("&Finish",10)
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto Mco_Xit
	if ckey=10 then goto Mco_Xit
	oldmeterprior=val(resp$(1))
	oldmetercurrent=val(resp$(2))
	newmeterprior=val(resp$(3))
	newmetercurrent=val(resp$(4))
	usage=oldmetercurrent-oldmeterprior+newmetercurrent-newmeterprior
	if usage<0 then
		mat txt$(3)
		txt$(1)="The readings you entered create a negative uuage."
		txt$(2)="Correct one of the readings or choose Cancel to"
		txt$(3)="skip this record!"
		fnmsgbox(mat txt$,resp$,'',1)
		if resp$="OK" then goto MCO_RECORD_READINGS
	end if
	if method$="File" then fn_rewrite_usage : goto MCO_WORK_READ ! read new record from readings file
	if method$="Customer" and servicetype$="WA" then x(1)=newmetercurrent: x(12)=usage
	if method$="Customer" and servicetype$="GA" then x(2)=newmetercurrent: x(14)=usage
	if method$="Customer" and servicetype$="EL" then x(3)=newmetercurrent: x(13)=usage
	if method$="Customer" then passcheck=ckfail: editmode=1 : goto mco_EnterReadings3
	! goto somewhere
	MCO_UPDATE_FULL_FILE: ! meter change over - update full file
	close #hWork: ioerr ignore
	open #hWork: "Name="&workFile$,internal,outIn,relative
	if lrec(hWork)=0 then goto MCO_L9290
	MCO_WORK_READ: !
	read #hWork,using Fwork: x$,mat x eof MCO_L9350
	MCO_L9290: !
	if trim$(begx$)="" or trim$(begx$)="[All]" then begx$="" : goto MCO_L9320
	if trim$(begx$)<>trim$(x$) then goto MCO_WORK_READ
	begx$=""
	MCO_L9320: !
	read #hCustomer1,using MCO_F_CUSTOMER,key=x$,release: aname$, mat d nokey MCO_WORK_READ
	MCO_F_CUSTOMER: form pos 41,c 20,pos 217,15*pd 5
	goto MCO_RECORD_READINGS
	MCO_L9350: !
	close #hWork: ioerr ignore
	open #hWork: "Name="&workFile$&",KFName="&workFileIndex$,internal,outIn,keyed
	goto Mco_Xit
	mco_EnterReadings3: !
	mco_return=3
	Mco_Xit: !
	fn_meter_change_out=mco_return
fnend

def fn_hh_other_type2(ip1$,listonly; ___,lineCount)
	dim hot_ver$*512,hot_line$*512
	dim hotImportDataField$(0)*256
	dim hotImportDataValue$(0)*256
	hotDataImportAsked=0
	open #h_readings=fnH: "Name=[Q]\UBmstr\Readings."&ip1$,display,input
	lineCount=0
	linput #h_readings: hot_ver$
	lineCount+=1
	hot_ver$=trim$(hot_ver$)
	hot_z_prior$=hot_z$=''
	if hot_ver$='[ACS Hand Held File Generic Version 2]' then
		if listonly=1 then fnopenprn
		do
			hotWaterMeterChangeBefore=hotWaterMeterChangeAfter=0
			mat hotImportDataField$(0)
			mat hotImportDataValue$(0)
			mat x=(0)
			do
				hot_z_prior$=hot_z$
				linput #h_readings: hot_line$ eof HOT_EOF
				lineCount+=1
				if trim$(srep$(hot_line$,'=',''))<>'' and trim$(hot_line$)(1:1)<>'!' then
					! pr 'before: hotline$='&hot_line$ ! pause
					fn_hot_parseLine(hot_line$,hot_z$,mat x,mat hotImportDataField$,mat hotImportDataValue$,hotWaterMeterChangeBefore,hotWaterMeterChangeAfter)
					! pr 'after ' : pause
			 end if
			loop until hot_z$<>hot_z_prior$ and hot_z_prior$<>''
			! r:	debug				if trim$(hot_z_prior$)='100020.03' then 
			! 						debug=1
			! 					! if debug then
			! 						pr 'after loop'
			! 						pr 'Customer.Number=';hot_z_prior$;'         hot_z_prior$=';hot_z_prior$
			! 						pr 'Reading.Water=';x(1);'  Usage=';x(12)
			! 						! pr 'MeterAddress.LocationID=';hotLocationID
			! 						! pr 'hotWaterMeterChangeBefore=';hotWaterMeterChangeBefore;'  hotWaterMeterChangeAfter=';hotWaterMeterChangeAfter
			! 						! for x=1 to udim(mat hotImportDataField$)
			! 						! 	pr hotImportDataField$(x)&'='&hotImportDataValue$(x)
			! 						! nex x
			! 						pr ''
			! 						pause
			! 					else
			! 						debug=0
			! /r					end if
			fn_hot_calcMeterChangeOut(hot_z_prior$,mat x,hotWaterMeterChangeBefore,hotWaterMeterChangeAfter)
			if listonly=1 then
				fn_lo_pr_rec(hot_z_prior$,mat x)
			else
				fn_hot_writeWork(hWork,hot_z_prior$,mat x,hotDataImportAsked,hotDataImportEnabled,mat hotImportDataField$,mat hotImportDataValue$)
			end if
			hot_z_prior$=hot_z$
		loop
		HOT_EOF: !
		fn_hot_calcMeterChangeOut(hot_z$,mat x,hotWaterMeterChangeBefore,hotWaterMeterChangeAfter)
		if listonly=1 then
			fn_lo_pr_rec(hot_z$,mat x)
		else
			fn_hot_writeWork(hWork,hot_z$,mat x,hotDataImportAsked,hotDataImportEnabled,mat hotImportDataField$,mat hotImportDataValue$)
		end if
		if listonly=1 then fncloseprn
	end if  ! hot_ver$='[ACS Hand Held File Generic Version 2]'
fnend
def fn_hot_parseLine(line$*512,&hot_z$,mat x,mat importDataField$,mat importDataValue$,&hotWaterMeterChangeBefore,&hotWaterMeterChangeAfter)
	! sets any one of the following local variables each call:
	! hot_z$, mat x
	pos_equal=pos(line$,'=')
	dim hpField$*256
	dim hpValue$*256
	hpField$=line$(1:pos_equal-1)
	hpValue$=line$(pos_equal+1:len(line$))
	hpValue$=trim$(hpValue$,'"')
	hpValueN=0
	hpValueN=val(hpValue$) conv ignore
	hpField$=lwrc$(trim$(hpField$))
	str2mat(hpField$,mat lfItem$,'.')
	if lfItem$(1)='source file' or lfItem$(1)(1:1)='!' then
		! do nothing     goto hpFinis
	else
		if lfItem$(2)='kwh' then lfItem$(2)="electric"
		if lfItem$(1)="customer" then
			if lfItem$(2)="number" then
				hot_z$=lpad$(trim$(hpValue$),10)
			end if
		else if lfItem$(1)="meterchangeout" then
			if lfItem$(2)='readingbefore' then
				if lfItem$(3)='water' then
					hotWaterMeterChangeBefore=hpValueN
				else
					pr 'encountered '&hpField$&' - add code to process it' : pause
				end if
			else if lfItem$(2)='readingafter' then
				if lfItem$(3)='water' then
					hotWaterMeterChangeAfter=hpValueN
				else
					pr 'encountered '&hpField$&' - add code to process it' : pause
				end if
			end if
		else if lfItem$(1)="reading" and udim(mat lfItem$)=2 then
			if lfItem$(2)="water" or lfItem$(2)="wa" then
				x(1)=hpValueN
			else if lfItem$(2)="gas" then
				x(2)=hpValueN
			else if lfItem$(2)="electric" then
				x(3)=hpValueN
			else if lfItem$(2)="demand" then
				x(4)=hpValueN
			end if
		else if lfItem$(1)="charge" then
			if lfItem$(2)="sewer" then
				x(5)=hpValueN
			else if lfItem$(2)="sanitation" then
				x(6)=hpValueN
			else if lfItem$(2)="fire protection" then
				x(7)=hpValueN
			else if lfItem$(2)="other" then
				x(8)=hpValueN
			else if lfItem$(2)="water" then
				x(9)=hpValueN
			else if lfItem$(2)="electric" then
				x(10)=hpValueN
			else if lfItem$(2)="gas" then
				x(11)=hpValueN
			end if
		else if lfItem$(1)="used" or lfItem$(1)="usage" then
			if lfItem$(2)="water" then
				x(12)=hpValueN
			else if lfItem$(2)="kwh" then
				x(13)=hpValueN
			else if lfItem$(2)="gas" then
				x(14)=hpValueN
			end if
		else if lfItem$(1)="final billing code" then
			x(15)=hpValueN
		else if lfItem$(1)="meter" then
			if lfItem$(2)="tamper" then
				fn_write_tamper(hot_z$,hpValueN)
			else if lfItem$(2)="meter number" or lfItem$(2)="transmitter" or lfItem$(2)="longitude" or lfItem$(2)="latitude" then
				if lfItem$(3)='water' then
					fnAddOneC(mat hotImportDataField$,hpField$)
					fnAddOneC(mat hotImportDataValue$,hpValue$)
				else
					goto HpDidNotCodeItYet
				end if
			end if
		else if lfItem$(1)='meteraddress' and lfItem$(2)='locationid' then
			hotLocationID=hpValueN ! really just setting this variable so we can check the value of it later during debugging
			hot_zFromLocationID$=fnAccountFromLocationId$(hotLocationID)
			if hot_z$<>hot_zFromLocationID$ then
				pr 'Customer.Number specified conflicts with Customer.Number derived from Location ID.'
				pr '  Customer.Number='&hot_z$
				pr '  MeterAddress.LocationID='&hpValue$
				pr '  Customer Number derived from Location ID='&hot_zFromLocationID$
				hot_z$=hot_zFromLocationID$
			end if
		else
			HpDidNotCodeItYet: !
			! pr 'code needed for lines like: "'&line$&'"'
			! if env$('acsDeveloper')<>'' then pause
		end if
	end if
fnend
def fn_hot_calcMeterChangeOut(hcmcoAccount$,mat x,hotWaterMeterChangeBefore,hotWaterMeterChangeAfter)
	if hotWaterMeterChangeBefore<>0 then
		if x(12)>0 then
			pr ' meter roll specified but usage was also specified.' : pause
		else
			read #hcustomer1,using 'form pos 217,pd 5',key=lpad$(trim$(hcmcoaccount$),10),release: hotwaterreadingprior
			! pr ' need to read customers water reading here' : pause
			! usage=oldmetercurrent-oldmeterprior+newmetercurrent-newmeterprior
			if debug then
				pr 'hcmcoaccount$='&hcmcoaccount$
				pr '  oldMeterCurrent=';hotWaterMeterChangeBefore
				pr '  oldMeterCurrent=';hotWaterMeterChangeBefore
				pr '  oldMeterPrior  =';hotWaterReadingPrior
				pr '  newMeterCurrent=';x(1)
				pr '  newMeterPrior  =';hotWaterMeterChangeAfter
				pr '  usage=oldmetercurrent-oldmeterprior+newmetercurrent-newmeterprior'
				pr '  usage=';hotWaterMeterChangeBefore-hotWaterReadingPrior+x(1)-hotWaterMeterChangeAfter
				pause
			end if
			x(12)=hotWaterMeterChangeBefore-hotWaterReadingPrior+x(1)-hotWaterMeterChangeAfter
			! if  x(12)<0 then
			!   pr 'negative usage ('&str$(x(12))&') calculated on '&hcmcoAccount$
			!   pr '  Water MeterChange Before= ';hotWaterMeterChangeBefore
			!   pr '  Water MeterChange  After= ';hotWaterMeterChangeAfter
			!   pr '       Water Reading Prior= ';hotWaterReadingPrior;' (from customer file)'
			!   pr '         Reading from Book= ';x(1)
			!   pause
			! end if
		end if
	end if
fnend
def fn_hot_writeWork(hWork,hwwAccount$,mat x,&hotDataImportAsked,&hotDataImportEnabled,mat hotImportDataField$,mat hotImportDataValue$)
	if udim(mat hotImportDataField$)>0 then
		if ~hotDataImportAsked then ! r: ask if they want to import data (non reading/usage)
			mat message$(0)
			fnAddOneC(mat message$,'This book contains (non reading/usage) data to import into the customer records.')
			fnAddOneC(mat message$,'Import the data now?')
			fnAddOneC(mat message$,'')
			fnAddOneC(mat message$,'Yes'&chr$(9)&'Updates customer and/or meter records with the new data')
			fnAddOneC(mat message$,'No'&chr$(9)&'Only load the readings from this file, ommits import data')
			fnmsgbox(mat message$, resp$,'',32+4)
			hotDataImportAsked=1
			if resp$='Yes' then
				hotDataImportEnabled=1
			else
				hotDataImportEnabled=0
			end if
		end if ! /r
		hwwAccount$=rpad$(trim$(hwwAccount$),10)
		if hotDataImportEnabled then ! r: import the data
			if ~hLocation then
				dim location$(0)*128,locationN(0)
				hLocation:=fn_open('U4 Meter Location',mat location$,mat locationN,mat form$, 0,4)
			end if
			mat location$=('')
			mat locationN=(0)
			location$(loc_activeCustomer)=hwwAccount$
			location$(loc_serviceId)='WA'
			locationKey$=fnbuildkey$('U4 Meter Location',mat location$,mat locationN,4)
			locationRecordDidChange=0
			for hotIdX=1 to udim(mat hotImportDataField$)
				if hotImportDataField$(hotIdX)='meter.transmitter.water' then
					! fn_HwwMeterMakeRecIfNone(hMeter,hwwAccount$,'WA')
					read #hLocation,using form$(hLocation),key=locationKey$: mat location$,mat locationN
					location$(loc_transmitter)=hotImportDataValue$(hotIdX)
					rewrite #hLocation,using form$(hLocation),key=locationKey$: mat location$,mat locationN
				else if hotImportDataField$(hotIdX)='meter.meter number.water' then
					read #hLocation,using form$(hLocation),key=locationKey$: mat location$,mat locationN
					location$(loc_meterNumber)=hotImportDataValue$(hotIdX) ! fixed 9/14/2020
					! location$(loc_transmitter)=hotImportDataValue$(hotIdX)
					rewrite #hLocation,using form$(hLocation),key=locationKey$: mat location$,mat locationN
				else if hotImportDataField$(hotIdX)='meter.longitude.water' then
					read #hLocation,using form$(hLocation),key=locationKey$: mat location$,mat locationN
					location$(loc_longitude)=hotImportDataValue$(hotIdX)
					rewrite #hLocation,using form$(hLocation),key=locationKey$: mat location$,mat locationN
				else if hotImportDataField$(hotIdX)='meter.latitude.water' then
					read #hLocation,using form$(hLocation),key=locationKey$: mat location$,mat locationN
					location$(loc_latitude)=hotImportDataValue$(hotIdX)
					rewrite #hLocation,using form$(hLocation),key=locationKey$: mat location$,mat locationN
				else if hotImportDataField$(hotIdX)='customer.sequence' then
					! *TODO: rewtite for customer.sequence "
					pr "rewrite #hCustomer1,using 'form',key=x$: "
					pause
				else
					pr ' add code to update '&hotImportDataField$(hotIdX)
					pause
				end if
			nex hotIdX
		end if ! /r
	end if
	finalBillingCode=val(fnCustomerData$(hwwAccount$,'Final Billing Code',1))
	if finalBillingCode and ~hotFinaledImportAsked then ! r: ask if they want to import data (non reading/usage)
		mat message$(0)
		fnAddOneC(mat message$,'This book contains accounts that are final billed.')
		fnAddOneC(mat message$,'Skip loading their readings?')
		fnmsgbox(mat message$, resp$,'',32+4)
		hotFinaledImportAsked=1
		if resp$='Yes' then
			hotFinaledImportEnabled=0
		else
			hotFinaledImportEnabled=1
		end if
	end if ! /r
	if ~finalBillingCode or hotFinaledImportEnabled then
		fn_writeWork(hWork,hot_z_prior$,mat x, 1)
	end if
	if hotDataImportEnabled then
		fnCloseFile(hLocation,'U4 Meter Location')
		hLocation=0
	end if
fnend
include: fn_open
include: ertn