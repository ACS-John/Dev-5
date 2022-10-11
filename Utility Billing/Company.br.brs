fn_setup
fnTop(program$)
gosub DoLoad
screen=screen_main
MAIN: ! r:
	fnTos
	lc=1
	respc=0
	fnButtonOrDisabled(screen<>screen_main   ,lc, 2,'Main'     ,1001, '',8)
	fnButtonOrDisabled(screen<>screen_Route  ,lc,12,'Routes'   ,1002, '',8)
	fnButtonOrDisabled(screen<>screen_Service,lc,24,'Services' ,1003, '',8)
	fnButtonOrDisabled(screen<>screen_EFT    ,lc,36,'EFT'      ,1004, '',8)
	lc=2
	if screen=screen_main then
		! r: company information portion of screen
		mylen=27
		mypos=mylen+2
		fnLbl(lc+=1,1,'Company Name:',mylen,1)	: fnTxt(lc,mypos,40) : resp$(respc+=1)=CompanyNameAndAddr$(1)
		fnLbl(lc+=1,1,'Address:',mylen,1)     	: fnTxt(lc,mypos,40) : resp$(respc+=1)=CompanyNameAndAddr$(2)
		fnLbl(lc+=1,1,'City, State and Zip:',mylen,1) 	: fnTxt(lc,mypos,40) : resp$(respc+=1)=CompanyNameAndAddr$(3)
		lc+=1
		fnLbl(lc+=1,1,'Last Billing Date:',mylen,1)   	: fnTxt(lc,mypos,8,0,1,'1') : resp$(resp_lastBillingDate=respc+=1)=str$(lastBillingDate)
		lc+=1
		fnChk(lc+=1,0,'Require Receipt Number on Collections') : 	resp$(resp_require_receipt=respc+=1)=rcpt$
		! /r
	else if screen=screen_Route then
		! r: screen Route
		mylen=44
		mypos=mylen+2
		fnLbl(lc+=1,1,'Starting Route Number:',mylen,1)	: fnTxt(lc,mypos,2,0,1,'30') : resp$(resp_route_low=respc+=1)=str$(bkno1)
		fnLbl(lc+=1,1,'Ending Route Number:',mylen,1)  	: fnTxt(lc,mypos,2,0,1,'30') : resp$(resp_route_high=respc+=1)=str$(bkno2)
		lc+=1
		fnLbl(lc+=1,1,'Unusual Usage Percent:',mylen,1)	: fnTxt(lc,mypos,4,0,1,'30',0,'Percent to use for determining unusual usage   (Example: 50% as 50)')
		resp$(resp_pcent=respc+=1)=str$(pcent)
		lc+=1
		fnLbl(lc+=1,1,'Water Unusual Usage Minimum:',mylen,1)
		fnTxt(lc,mypos,4,0,1,'30',0,'Do not report unusual usage if below this minimum.')
		resp$(resp_uum_water=respc+=1)=uum_water$
	
		fnLbl(lc+=1,1,'Electric Unusual Usage Minimum:',mylen,1)
		fnTxt(lc,mypos,4,0,1,'30',0,'Do not report unusual usage if below this minimum.')
		resp$(resp_uum_gas=respc+=1)=uum_gas$
	
		fnLbl(lc+=1,1,'Gas Unusual Usage Minimum:',mylen,1)
		fnTxt(lc,mypos,4,0,1,'30',0,'Do not report unusual usage if below this minimum.')
		resp$(resp_uum_electric=respc+=1)=uum_electric$
	
		! /r
	else if screen=screen_Service then

		! respc=11 ! criticial (for the next section) that this is the last resp$ used (above)
		! r: TYPEOSERVICE portion of screen
		disable_for_client=1
		if env$('ACSDeveloper')<>'' then disable_for_client=0
		srv_input_col_count=6
		fnFra(3,1,12,113, 'Type of Service') : fra=1
		fnLbl(2,13,'Full Name',20,2,0,fra)
		fnLbl(2,34,'Code',4,0,0,fra)
		fnLbl(2,39,'Taxable',7,0,0,fra)
		fnLbl(2,47,'Penalty',7,0,0,fra)
		fnLbl(1,55,'Subject',7,2,0,fra)
		fnLbl(2,55,'To',7,2,0,fra)
		fnLbl(1,64,'Collection',10,2,0,fra)
		fnLbl(2,64,'Order',10,2,0,fra)
		fnLbl(1,75,'Only',5,2,0,fra)
		fnLbl(2,75,'Month',5,2,0,fra)
		fnLbl(1,82,'Default',9,2,0,fra)
		fnLbl(2,82,'Rate Code',9,2,0,fra)
		respc_service_base=respc
		for service_item=1 to 10
			fnLbl(service_item+2,1,'Service '&str$(service_item)&':',11,1,0,fra)
			fnTxt(service_item+2,13,20,0,0,'',disable_for_client,'',fra)
			resp$(respc+=1)=serviceName$(service_item) ! resp$(respc_service_base+service_item*6-5)=serviceName$(service_item)
			fnTxt(service_item+2,34,3,0,0,'',disable_for_client,'',fra)
			resp$(respc+=1)=serviceCode$(service_item) ! resp$(respc_service_base+service_item*6-4)=serviceCode$(service_item)
			fnChk(service_item+2,41,'',align=0,fra,tabcon=0,disable_for_client)
			if tax_code$(service_item)='Y' then 
				resp$(respc+=1)='True' ! resp$(respc_service_base+service_item*6-3)='True'
			else 
				resp$(respc+=1)='False' ! resp$(respc_service_base+service_item*6-3)='False'
			end if 
			fnChk(service_item+2,49,'',align=0,fra,tabcon=0,disable_for_client)
			if penalty$(service_item)='Y' then 
				resp$(respc+=1)='True' ! resp$(respc_service_base+service_item*6-2)='True'
			else 
				resp$(respc+=1)='False' ! resp$(respc_service_base+service_item*6-2)='False'
			end if 
			fnTxt(service_item+2,58,2,0,0,'30',disable_for_client,'',fra)
			resp$(respc+=1)=str$(subjectto(service_item)) ! resp$(respc_service_base+service_item*6-1)=str$(subjectto(service_item))
			fnTxt(service_item+2,68,2,0,0,'30',disable_for_client,'',fra)
			resp$(respc+=1)=str$(ordertoapply(service_item)) ! resp$(respc_service_base+service_item*6)=str$(ordertoapply(service_item))
			fnTxt(service_item+2,76,2,0,0,'30',0,'',fra)
			resp$(respc+=1)=str$(onlyMonth(service_item))
			fn_cmb_rate(serviceCode$(service_item),service_item+2,82,'Select Default or 0 for None',fra)
			resp$(respc+=1)=default_rate$(service_item)
		next service_item
		fnChk(17,2,'Enable Cost of Gas Adjustment')
		resp$(resp_enableCostOfGas=respc+=1)=enableCostOfGas$
		! /r
	else if screen=screen_EFT then
		! r: EFT Screen
		dim imd$    *10  ! immediate destination
		dim imo$    *10  ! immediate origin
		dim imoName$*23  ! immediate origin name
		dim cid$    *10  ! company identification
		dim odi$    *17  ! originating dfi identification
		dim ecc$    * 3  ! standard entry class code

		fnLbl(lc+=1,1,'Immediate Destination:',mylen,1)
		fnTxt(lc,mypos,10,0,0,'',0,'Immediate Destination (party to which delivered - routing number of ach operator)')
		resp$(resp_imd=respc+=1)=imd$
		
		fnLbl(lc+=1,1,'Immediate Origin:',mylen,1)
		fnTxt(lc,mypos,10,0,0,'',0,'Immediate Origin (routing number of the bank  which the city uses)')
		resp$(resp_imo=respc+=1)=imo$

		fnLbl(lc+=1,1,'Immediate Origin Name:',mylen,1)
		fnTxt(lc,mypos,23,0,0,'',0,'Immediate Origin (routing number of the bank  which the city uses)')
		resp$(resp_imoName=respc+=1)=imoName$

		fnLbl(lc+=1,1,'Company Identification:',mylen,1)
		fnTxt(lc,mypos,23,0,0,'',0,'company identification')
		resp$(resp_cid=respc+=1)=cid$

		fnLbl(lc+=1,1,'Originating DFI Identification:',mylen,1)
		fnTxt(lc,mypos,17,0,0,'',0,'originating dfi identification')
		resp$(resp_odi=respc+=1)=odi$

		fnLbl(lc+=1,1,'Standard Entry Class Code:',mylen,1)
		fnTxt(lc,mypos,3,0,0,'',0,'standard entry class code')
		resp$(resp_ecc=respc+=1)=ecc$


		! /r
	end if
	fnCmdSet(4)
	ckey=fnAcs(mat resp$)
	
	if ckey=5 then 
		goto Xit
	else 
		if screen=screen_main then
			! r: RESP$ to main screen variables
			CompanyNameAndAddr$(1)=    resp$(1)
			CompanyNameAndAddr$(2)=    resp$(2)
			CompanyNameAndAddr$(3)=    resp$(3)
			lastBillingDate       =val(resp$(resp_lastBillingDate))
			rcpt$                 =    resp$(resp_require_receipt)
			! /r
		else if screen=screen_Route then
			! r: RESP$ to Company Information variables
			pcent                 =val(resp$(resp_pcent       ))
			uum_water$            =    resp$(resp_uum_water   )
			uum_gas$              =    resp$(resp_uum_gas     )
			uum_electric$         =    resp$(resp_uum_electric)
			bkno1                 =val(resp$(resp_route_low   ))
			bkno2                 =val(resp$(resp_route_high  ))
			! /r
		else if screen=screen_Service then
			! r: RESP$ to Service variables
			respc=respc_service_base
			for service_item=1 to 10
				serviceName$(service_item)=resp$(respc+=1) ! resp$(respc_service_base+service_item*6-5)
				serviceCode$(service_item)=uprc$(resp$(respc+=1)) ! uprc$(resp$(respc_service_base+service_item*6-4))
				if resp$(respc+=1)='True' then ! resp$(respc_service_base+service_item*6-3)='True' then
					tax_code$(service_item)='Y'
				else 
					tax_code$(service_item)='N'
				end if 
				if resp$(respc+=1)='True' then ! resp$(respc_service_base+service_item*6-2)='True' then
					penalty$(service_item)='Y'
				else 
					penalty$(service_item)='N'
				end if 
				subjectto(service_item)=val(resp$(respc+=1)) ! respc_service_base+service_item*6-1))
				ordertoapply(service_item)=val(resp$(respc+=1)) ! resp$(respc_service_base+service_item*6))
				onlyMonth(service_item)=val(resp$(respc+=1))
				default_rate$(service_item)=resp$(respc+=1)
			next service_item
			enableCostOfGas$=resp$(resp_enableCostOfGas)
			! /r
		else if screen=screen_EFT then
			! r: RESP$ to EFT variables
			imd$    =resp$(resp_imd     )
			imo$    =resp$(resp_imo     )
			imoName$=resp$(resp_imoName )
			cid$    =resp$(resp_cid     )
			odi$    =resp$(resp_odi     )
			ecc$    =resp$(resp_ecc     )
			! /r
		end if
		if ckey>1000 then screen=ckey : goto MAIN
	end if

	gosub DoSave
goto Xit ! /r

Xit: fnXit
DoLoad: ! r:
	! r: Company Load
	
	fncreg_read('Route Low' ,bkno1$, '1' ) : bkno1=val(bkno1$) ! Route Number Range Low
	fncreg_read('Route High',bkno2$, '99') : bkno2=val(bkno2$) ! Route Number Range High
	
	
	open #hCompany=fnH: 'Name=[Q]\UBmstr\Company.h[cno]',i,i 
	read #hCompany,using 'form pos 1,3*C 40,X 6,N 1,C 1,c 1,n 4': mat CompanyNameAndAddr$,maintac,rcpt$,escrow$,pcent ioerr DoLoadCompanyReadErr
	close #hCompany: 
	fnLastBillingDate(lastBillingDate)
	if pcent=0 then pcent=100
	if uprc$(rcpt$)=uprc$('Y') then rcpt$='True' else rcpt$='False'
	if uprc$(escrow$)=uprc$('Y') then escrow$='True' else escrow$='False'
	fncreg_read('unusual usage minimum water',uum_water$)
	fncreg_read('unusual usage minimum gas',uum_gas$)
	fncreg_read('unusual usage minimum electric',uum_electric$)
	if fnClientHas('UB-EFT') then
		imd$    =fnEftData$('Immediate Destination'           )
		imo$    =fnEftData$('Immediate Origin'                )
		imoName$=fnEftData$('Immediate Origin Name'           )
		cid$    =fnEftData$('Company Identification'          )
		odi$    =fnEftData$('Originating DFI Identification'  )
		ecc$    =fnEftData$('Standard Entry Class Code'       )
	end if 
	! /r
	! r: Service Load - Type Of Service Open
	open #hService=fnH: 'Name=[Q]\UBmstr\ubData\Service.h[cno],RecL=280,use',i,outi,r 
	F_SERVICE: form pos 1,10*c 20,10*c 2,10*c 1,10*c 1,10*n 2,10*n 2
	read #hService,using F_SERVICE,rec=1: mat serviceName$,mat serviceCode$,mat tax_code$,mat penalty$,mat subjectto,mat ordertoapply noRec TOS_WRITE
	goto ServiceLoad_Finis
	TOS_WRITE: ! 
	write #hService,using F_SERVICE,rec=1: mat serviceName$,mat serviceCode$,mat tax_code$,mat penalty$,mat subjectto,mat ordertoapply
	ServiceLoad_Finis: ! 
	close #hService: 

	for service_item=1 to 10
		fncreg_read('default rate '&str$(service_item),default_rate$(service_item))
		fncreg_read('Service '&str$(service_item)&' only month',tmp$): onlyMonth(service_item)=val(tmp$)
	next service_item
	fncreg_read('enable Cost of Gas',enableCostOfGas$, 'False')
	if fn_enableCostOfGas then enableCostOfGas$='True' else enableCostOfGas$='False'
	! /r
return  ! /r
DoLoadCompanyReadErr: ! r:
	if err<>714 then goto ERTN
	company_rln=rln(hCompany)
	if company_rln=133 then goto ERTN
	pr 'Converting UB Company Information'
	pr 'From Record Length '&str$(company_rln)&' to 133'
	close #hCompany: ioerr ignore
	fnCopy('[Q]\UBmstr\Company.h[cno]','[Q]\UBmstr\Company.h[cno]', 133)
goto DoLoad ! /r

DoSave: ! r:
	! r: Company Save
	if rcpt$='True' then rcpt$='Y' else rcpt$='N'
	if escrow$='True' then escrow$='Y' else escrow$='N'
	maintac=1 ! maintac was variable used for maintaining accumulated transaction file, no longer used but be want history to be retained no matter what (so set it to 1)
	close #1,free: ioerr ignore
	open #hCompany=fnH: 'Name=[Q]\UBmstr\Company.h[cno],Size=0,RecL=133,Replace',internal,outIn 
	write #hCompany,using 'form pos 1,3*C 40,x 6,N 1,C 1,c 1,n 4': mat CompanyNameAndAddr$,maintac,rcpt$,escrow$,pcent
	close #hCompany: 
	fnLastBillingDate(lastBillingDate,1)
	fncreg_write('unusual usage minimum water',uum_water$)
	fncreg_write('unusual usage minimum gas',uum_gas$)
	fncreg_write('unusual usage minimum electric',uum_electric$)
	fncreg_write('Route Low',str$(bkno1)) ! Route Number Range Low
	fncreg_write('Route High',str$(bkno2)) ! Route Number Range High
	if fnClientHas('UB-EFT') then
		fnEftData$('Immediate Destination'           ,imd$    )
		fnEftData$('Immediate Origin'                ,imo$    )
		fnEftData$('Immediate Origin Name'           ,imoName$)
		fnEftData$('Company Identification'          ,cid$    )
		fnEftData$('Originating DFI Identification'  ,odi$    )
		fnEftData$('Standard Entry Class Code'       ,ecc$    )
	end if
	! /r
	! r: Service Save
	open #hService=fnH: 'Name=[Q]\UBmstr\ubData\Service.h[cno],RecL=280,use',i,outi,r 
	rewrite #hService,using F_SERVICE,rec=1: mat serviceName$,mat serviceCode$,mat tax_code$,mat penalty$,mat subjectto,mat ordertoapply
	close #hService: 

	for service_item=1 to 10
		fncreg_write('default rate '&str$(service_item),default_rate$(service_item))
		fncreg_write('Service '&str$(service_item)&' only month',str$(onlyMonth(service_item)))
	next service_item
	fn_enableCostOfGas( enableCostOfGas$)
	! /r
return  ! /r


def fn_cmb_rate(searchcode$,cr_lyne,cr_pos,ttt$*300,fra)
	! GET_CODES: ! r: get applicable rate codes
	! search routine must be passed code for service (WA for water) in searchcode$
	dim rates$(50)*30,rt$*54
	open #hRate=fnH: 'Name=[Q]\UBmstr\ubData\RateMst.h[cno],KFName=[Q]\UBmstr\ubData\RateIdx1.h[cno],Shr',i,i,k 
	restore #hRate: 
	cr_rate_item=1: mat rates$=('')
	mat rates$(50)
	rates$(1)=' 0=Not applicable'
	do 
		CR_READ_RATE: ! 
		read #hRate,using 'form pos 1,C 54',release: rt$ eof CR_EO_RATE
		if trim$(rt$(1:2))<>searchcode$ then goto CR_READ_RATE
		cr_rate_item+=1
		rates$(cr_rate_item)=rt$(3:4)&'='&rt$(5:25)
		if ratecode=val(rt$(3:4)) then rateinfo$(3)=rt$(3:4)&'='&rt$(5:25)
	loop 
	CR_EO_RATE: ! 
	if cr_rate_item>0 then mat rates$(cr_rate_item) else mat rates$(1)
	if ratecode=0 then rateinfo$(3)=' 0=Not applicable'
	! 
	close #hRate: 
	! /r
	fnComboA('ubfm-rates',cr_lyne,cr_pos,mat rates$,ttt$,30,fra)
fnend
def library fnEnableCostOfGas(; setIt$)
	if ~setup then fn_setup
	fnEnableCostOfGas=fn_enableCostOfGas( setIt$)
fnend
def fn_enableCostOfGas(; setIt$, ___,returnN)
	setIt$=trim$(lwrc$(setIt$))
	if setIt$<>'' and setIt$<>'true' and setIt$<>'false' then
		pr 'invalid setIt$ ('&setIt$&') passed to fn_enableCostOfGas.'
		pause
	else if setIt$<>'' then
		fncreg_write('enable Cost of Gas',setIt$)
	else
		fncreg_read( 'enable Cost of Gas',setIt$)
		if setIt$='' and env$('client')='Edinburg' or env$('client')='French Settlement' or env$('client')='Allendale' then
			setIt$='True'
		else
			setIt$='False'
		end if
	end if
	setIt$=trim$(lwrc$(setIt$))
	if setIt$='true' then returnN=1
	fn_enableCostOfGas=returnN
fnend

def fn_setup
	if ~setup then 
		setup=1
		autoLibrary
		on error goto Ertn
	
		dim resp$(256)*40
		dim CompanyNameAndAddr$(3)*40
		dim serviceName$(10)*20
		dim serviceCode$(10)*2
		dim tax_code$(10)*1
		dim default_rate$(10)*80
		
		screen_main   =1001
		screen_Route  =1002
		screen_Service=1003
		screen_EFT    =1004
	end if
fnend
include: ertn
