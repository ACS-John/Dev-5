! formerly acsTM\moInvoice
fn_setup
fnTop(program$)
client_id_sageAx$='3811'
client_id_brc$='90'
enableMinimumMonthlyBill=1

invoice_number=fncreg_read('Last Invoice Number',tmp$)
invoice_number+=1
if invoice_number=1 then invoice_number=val(date$(days(date$)-20,'yymm')&'01')

if fn_askScreen1(invDateMmDdYy,invoice_number)=99 then goto Xit
invoiceDateCcyymmdd=date(days(invDateMmDdYy,'mmddyy'),'ccyymmdd')
! r: make a quick backup
fnStatus('Commit your work.  This data is saved in the repository.')
execute 'sy "C:\ACS\Util\Dev-5 Commit.cmd"'
! fnStatusPause

! fnStatus('Custom Saving to "D:\ACS\ACS LLC 420.zip"...')  ! these were nice ideas when the data wasnt in core.
! fnFileSaveAs('*.*', 'D:\ACS\ACS LLC 420.zip')             ! these were nice ideas when the data wasnt in core.
! fnAutomatedSavePoint('before')                            ! these were nice ideas when the data wasnt in core.

! /r
! r: main loop (produceInvoices)
	fnStatus('producing Invoice Archive...')
	open #hClient=fnH: 'Name=S:\Core\Data\acsllc\CLmstr.h[cno],KFName=S:\Core\Data\acsllc\CLIndex.h[cno],Shr',internal,input,keyed
	! pr 're-indexing support, just in case - probably not necessary to do so often, but one time there was this problem.'
	! fnIndex('S:\Core\Data\acsllc\support.h[cno]','S:\Core\Data\acsllc\support-idx.h[cno]','1/7,6/2')
	open #hSupport=fnH: 'Name=S:\Core\Data\acsllc\Support.h[cno],KFName=S:\Core\Data\acsllc\support-idx.h[cno],Shr',internal,input,keyed
	Fsupport: form pos 1,c 6,x 2,c 2,x 8,c 2,n 8,n 10.2,4*c 50

	! fnIndex('[Temp]\TmSht[session]','[Temp]\TmSht-idx[session]','1,5')
	fn_combineIntoTmSht('S:\Core\Data\acsllc\TimeSheet.h[cno]')
	! pr 'ok go do FileIo on TM Tmp TimeSheet now' : pause

	fnStatus('Printing Invoices...')
	fnInvoiceOpen

	open #h_tmwk2=fnH: 'Name=S:\Core\Data\acsllc\tmpInvoice.h[cno],Replace,RecL=4675,Shr',internal,outIn
	dim cde$(30)*6
	dim inv_item$(30)*128
	dim inv_amt(30)
	dim inv_category(30)
	dim inv_service_code$(30)
	dim inv_gl$(30)*12
	dim client_id$*5
	dim client_addr$(3)*30
	do
		read #hClient,using 'form pos 1,c 5,3*c 30,pos 283,pd 5.2': client_id$,mat client_addr$,pbal eof EoClient


		client_id$=trim$(client_id$)

		! if client_id$='911' then pr client_id$&' - '&fnClientNameShort$(client_id$) : pause
		! if client_id$='3379' then pr '3379 - Kathy Bacon' : pause
		! if client_id$='3385' then pr '3385 - Evelyn Pareya' : pause
		! if client_id$='3045' then pr '3045 - Moweaqua' : debug=1 : pause else debug=0
		! if client_id$='4132' then pr '4132 - Stern' : pause


		fn_billforMaint(client_id$,invTotal)
		! if client_id$='4132' then pr client_id$&' - before fn_billForNonMaint' : pause
		fn_billForNonMaint(client_id$,invTotal)
		! if invTotal then
		! 	pr client_id$&' - '&client_addr$(1)&'     ';invTotal
		! end if
		
		! r: debug point
		! pr '*'&client_id$&'*'
		! if client_id$='4132' or client_id$='911' then 
		! 	pr 'client_id$     ="'&client_id$&'"     invTotal  =';invTotal
		! 	! pr 'client_addr$(1)="'&client_addr$(1)&'"'
		! 	! pr 'client_addr$(2)="'&client_addr$(2)&'"'
		! 	! pr 'pbal           =';pbal
		! 	pause
		! end if
		! /r
		
		fn_print_inv ! ( client_id$,mat client_addr$,pbal,invTotal )

	loop
	EoClient: !
	! pause
	close #h_ivnum:
	close #hClient:
	close #h_tmwk2:
	fnInvoiceClose(invDateMmDdYy, 'ACS Invoices')
! /r


! r: finalize
	execute 'sy -c -w explorer "'&fnReportCacheFolderCurrent$&'\Ebilling"'
	execute 'sy -c -w explorer "'&fnReportCacheFolderCurrent$&'\Invoice\Archive"'
	execute 'sy -c -w explorer "'&fnReportCacheFolderCurrent$&'\Invoice\Print"'

	fnStatus('producing Summary...')
	fn_summaryRelease

	do
		fnToS
		fnOpt(1,41,'Merge Invoices and Email Queued Invoices',1)
		resp$(1)='True'
		fnOpt(2,1,'Merge Invoices only')
		resp$(2)='False'
		fnOpt(3,1,'Stop (neither merge, nor email)')
		resp$(3)='False'
		fnCmdSet(2)
		ckey=fnAcs(mat resp$)
		if ckey=5 or resp$(3)='True' then
			goto Xit
		else if resp$(1)='True' then
			fnEmailQueuedInvoices(str$(invoiceDateCcyymmdd))
			fnMergeInvoices
			goto Xit
		else if resp$(2)='True' then
			fnMergeInvoices
			goto Xit
		end if
	loop
! /r

def fn_billForMaint(client_id$,&invTotal)
	! loads of locals


	dim iv$*12
	iv$=rpad$(str$(invoice_number),12)
	
	client_id$=trim$(client_id$)
	
	restore #hSupport: ! ,key>=rpad$(client_id$,kln(hSupport)): ! nokey EoSupport
	do
		read #hSupport,using Fsupport: cln$,scode$,stm$,sup_exp_date,supCost eof EoSupport
		cln$=trim$(cln$)

		if cln$=client_id$ then
			needsRenewal=0 ! if it expires this month
			if int(invoiceDateCcyymmdd*.01)=int(sup_exp_date*.01) then needsRenewal=1

			if stm$='Mo' and needsRenewal then
				pr 'monthly bill encountered.  please test code before accepting.'
				pause
			end if


			if needsRenewal then

				if supCost=0 then supCost=fn_price(scode$,stm$)
				if supCost=0 then pr 'zero support price???' : pause

				if supCost>0 then
					returnN=supCost
					invLine+=1

					if stm$='An' then
						inv_item$(invLine)='Annual'
					else if stm$='Qt' then
						inv_item$(invLine)='Quarterly'
					else if stm$='Mo' then
						inv_item$(invLine)='Monthly'
					else
						inv_item$(invLine)='(unexpected timeframe)'
						pr inv_item$(invLine) : pause
					end if
					if scode$='U4' then
						inv_item$(invLine)=inv_item$(invLine)&' Maintenance for (UB) Hand Held Add-On'
					else
						inv_item$(invLine)=inv_item$(invLine)&' Maintenance for '&trim$(fnSystemNameFromId$(scode$))
						if trim$(fnSystemNameFromId$(scode$))='' then
							pr ' sending blank system name  scode$='&scode$
							pr '   client_id=';client_id$
							pause
						end if
					end if
					inv_amt(invLine)=supCost
					inv_category(invLine)=6
					inv_service_code$(invLine)=scode$
					inv_gl$(invLine)='  0  1160  0'
				end if

				invTotal+=supCost

				if debug then pr '   ';client_id$;' billForMaint Match encountered ';supCost
			end if
		end if
	loop ! while cln$=client_id$ ! commented out to work around a critical nokey problem above.  should severely slow things down though
	EoSupport: !
fnend

def fn_billForNonMaint(client_id$,&invTotal; ___,wo_desc$*30,hTimeSheet) ! add charges not under maintenance to maintenance invoices
	! dim timesheet$(0)*128
	! dim timesheetN(0)
	! hTimeSheet=fn_open('TM timeSheet',mat timesheet$, mat timesheetN, mat form$)
	open #hTimeSheet=fnH: 'Name=[Temp]\TmSht[session],KFName=[Temp]\TmSht-idx[session]',internal,outIn,keyed
	dim inp7
	read #hTimeSheet,using F_time,key=>rpad$(client_id$,kln(hTimeSheet)): inp1$,inp2,inp3,inp4,inp5  ,inp6	,inp7,b6  	,b7  ,	b8$,sc,       o_o 	,wo_desc$ nokey TM_XIT2
	F_time: form pos 1,                                                        v 5 ,n 9  ,2*pd 3.2 ,pd 4.2,n 6 	,n 2 ,pd 2	,pd 1,	c 2,n 4,x 12, pd 3	,c 30
	F_timeTc: form pos 1,x 14,pd 3.2,x 3,pd 4.2
	! inp1$=trim$(inp1$)
	if inp1$=client_id$ then
		do
			if b8$='0' or b8$='' then b8$='19'
			delete #hTimeSheet: ioerr ignore ! delete current record so it is not processed twice
			! fn_billForHours(client_id$)
			! def fn_billForHours(client_id$) ! ,inp1$,inp2,inp3,inp4,inp5,inp6,inp7,etc...
			if invLine=30 then fn_print_inv ! pr invoice if more than 20 entries
			if invLine>29 then pause
			spk$=' '&lpad$(client_id$,5)&lpad$(b8$,2)

			if inp7=2 then goto BfhGo ! always bill modifications

			if inp7=23 or inp7=11 then goto BfhXit ! always no charge

			if inp7<>2 then
				read #hSupport,using Fsupport,key=spk$: cln$,scode$,stm$,sup_exp_date,supCost nokey BfhGo
				trans_date=date(days(inp6,'mmddyy'),'ccyymmdd')
				if (trans_date<=sup_exp_date) then goto BfhXit !  it covered by maintenance
			end if

			BfhGo: !
			supCost=inp5

			invTotal+=supCost
			invLine+=1
			! if val(client_id$)=3828 then pr 'schachtner encountered invLine=';invLine : pause
			if client_id$=client_id_sageAx$ or client_id$=client_id_brc$ then
				!     pause  ! inv_item$(invLine)=str$(inp3)&' hours at a rate of '&&' on '&cnvrt$('pic(##/##/##)',inp6)
				inv_item$(invLine)=str$(inp3)&' hours at a rate of '&cnvrt$('pic($$#.##)',inp4)&' on '&cnvrt$('pic(##/##/##)',inp6)
			else if inp7=2 then
				inv_item$(invLine)=str$(inp3)&' hours of '&trim$(fnSystemNameFromId$(b8$))&' programming on '&cnvrt$('pic(##/##/##)',inp6)
			else
				inv_item$(invLine)=str$(inp3)&' hours of '&trim$(fnSystemNameFromId$(b8$))&' support on '&cnvrt$('pic(##/##/##)',inp6)
			end if

			inv_amt(invLine)=inp5
			inv_category(invLine)=6
			inv_service_code$(invLine)=b8$
			inv_gl$(invLine)='  0  1160  0'
			BfhXit: !
			! fnend

			read #hTimeSheet,using F_time: inp1$,inp2,inp3,inp4,inp5,inp6,inp7,b6,b7,b8$,sc,o_o,wo_desc$ eof TM_XIT2
		loop while inp1$=client_id$
	end if
	TM_XIT2: !
	close #hTimeSheet:
fnend
def fn_print_inv
	! if debug then pr 'debug print_inv' :  pause
	if enableMinimumMonthlyBill and invTotal>0 and invTotal<100 then
		invLine+=1
		if invLine<30 then
			inv_item$(invLine)='Minimum Monthly Billing of $100.00'
			inv_amt(invLine)=100-sum(mat inv_amt)
			invTotal+=inv_amt(invLine)
			inv_service_code$(invLine)='19'
			inv_gl$(invLine)='  0  1160  0'
		end if
	end if
	if invTotal=>1 then
		write #h_tmwk2,using F_TMWK2a: client_id$,2,invDateMmDdYy,iv$,mat cde$,mat inv_item$,mat inv_amt,mat inv_category,mat inv_service_code$,mat inv_gl$
		F_TMWK2a: form pos 1,c 5,n 1,n 6,c 12,30*c 6,30*c 128,30*pd 5.2,30*n 2,30*c 2,30*c 12
	end if
	if invTotal=>1 or pbal=>1 then
		fn_summaryAccumulate
		fnStatus('adding an $'&str$(sum(mat inv_amt))&' invoice for '&client_id$&' - '&client_addr$(1))
		! pr '  adding invoice '&iv$&' for '&client_id$
		fnInvoiceAdd(client_id$, mat client_addr$,iv$,invDateMmDdYy,mat inv_item$,mat inv_amt,pbal)
		invoice_number+=1
	end if
	mat inv_item$=(' ')
	mat inv_category=(0)
	mat inv_service_code$=('')
	mat inv_gl$=('')
	mat inv_amt=(0)
	invLine=invTotal=0
fnend

Xit: fnXit
dim resp$(30)*128
def fn_askScreen1(&invDateMmDdYy,&invoice_number; ___,returnN,invDay)
	lookbackDays=15

	if ~invDateMmDdYy then invDateMmDdYy=date(fnEndOfMonth(days(date$)-lookbackDays),'mmddyy')
	fntos : rc=lc=0

	fnlbl(lc+=1, 2,'Invoices for Month:'          ,24,1)
	fnComboF('month',lc,26,20,'S:\Core\Data\month.dat',1,2,3,18,'S:\Core\Data\month.idx',1,0,'Select Month Billing for - the month you are ending')
	resp$(resp_invMonth:=rc+=1)=str$(val(date$(days(invDateMmDdYy,'mmddyy'),'m')))

	fnlbl(lc+=1, 2,'Invoice Year (ccyy):'    ,24,1)
	fntxt(lc   ,26,4, 0,0,'number')
	resp$(resp_invYear:=rc+=1)=date$(days(invDateMmDdYy,'mmddyy'),'ccyy')

	fnlbl(lc+=1, 2,'Starting Invoice Number:',24,1)
	fntxt(lc   ,26,12, 0,0,'number')
	resp$(resp_invNo:=rc+=1)=str$(invoice_number)

	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=5 then
		returnN=99
	else
		returnN=0
		! invDateMmDdYy=val(resp$(resp_invDate)) ! date(days(resp$(1),'ccyymmdd'),'mmddyy')
		invoice_number=val(resp$(resp_invNo))
		invMonth=val(resp$(resp_invMonth)(1:2))
		invYear=val(resp$(resp_invYear))
		invDay=fnEndOfMonth( days(cnvrt$('pic(##)',invMonth)&'15'&cnvrt$('pic(####)',invYear),'mmddccyy') )
		if date(invDay,'mm')=2 and date(invDay,'dd')=29 then
			! it is a leap year - force the day to the 28th anyway.
			invDay-=1
		end if
		invDateMmDdYy=date(invDay,'mmddyy')
		invDateMmDdYy=date(invDay,'mmddyy')
	end if
	fn_askScreen1=returnN
fnend

def fn_combineIntoTmSht(file_from$*256; ___,tce_key$,wo_desc$*30,h_from,h_to,toInp3,toInp5)

	open #h_from=fnH: 'Name='&file_from$,internal,input
	open #h_to=fnH: 'Name=[Temp]\TmSht[session],KFName=[Temp]\TmSht-idx[session],Replace,RecL='&str$(rln(h_from))&',KPs=1/36/25,KLn=5/2/6',internal,outIn,keyed
	do
		read #h_from,using F_time: inp1$,inp2,inp3,inp4,inp5,inp6,inp7,b6,b7,b8$,sc,o_o,wo_desc$ eof TCE_EOF
		
		! pr '_______________________________'
		! pr 'inp1$   =';inp1$
		! pr 'inp2    =';inp2
		! pr 'inp3    =';inp3
		! pr 'inp4    =';inp4
		! pr 'inp5    =';inp5
		! pr 'inp6    =';inp6
		! pr 'inp7    =';inp7
		! pr 'b6      =';b6
		! pr 'b7      =';b7
		! pr 'b8$     =';b8$
		! pr 'sc      =';sc
		! pr 'o_o     =';o_o
		! pr 'wo_desc$=';wo_desc$
		! pause
		
		if b8$='20' then b8$='19' ! ALL PRINTING SUPPORT IS COVERED BY CORE
		
		tce_key$=rpad$(inp1$,5)&lpad$(b8$,2)&cnvrt$('N 6',inp6) ! ...=cnvrt$('N 5',inp1$)&...
		read #h_to,using F_timeTc,key=tce_key$: toInp3,toInp5 nokey CitAdd
		inp3+=toInp3 ! time
		inp5+=toInp5 ! charge
		rewrite #h_to,using F_time,key=tce_key$: inp1$,inp2,inp3,inp4,inp5,inp6,inp7,b6,b7,b8$,sc,o_o,wo_desc$
		goto CitNext
		CitAdd: !
		write #h_to,using F_time: inp1$,inp2,inp3,inp4,inp5,inp6,inp7,b6,b7,b8$,sc,o_o,wo_desc$
		CitNext: !
	loop
	TCE_EOF: !
	close #h_from:
	close #h_to:
fnend
def fn_summaryAccumulate
	! pr 'fn_summaryAccumulate   totalInvoicesPrinted=';totalInvoicesPrinted !
	if ~hSummary then
		open #hSummary=fnH: 'Name=PrnSummary[session],RecL=80,replace',display,output ! ioerr SI_ADD
		pr #hSummary: '{\fs16'  ! set the RTF Font Size to 8
		pr #hSummary: 'Clnt   Name           Date      Prev Bal    New Amt     Total Due   Inv No  '
		pr #hSummary: '_____ ______________  ________  __________  __________  __________  __________'
	end if
	! SI_ADD: !
	if pbal or invTotal then
		pr #hSummary,using Fsummary: client_id$,client_addr$(1)(1:14),invDateMmDdYy,pbal,invTotal,pbal+invTotal,iv$
		Fsummary: form pos 1,c 5,x 2,c 15,pic(zz/zz/zz),3*nz 12.2,x 2,c 12
		totalInvoicesPrinted+=invTotal
		totalPreviousBalances+=pbal
	end if
fnend
def fn_summaryRelease
	close  #hSummary:
	if exists('PrnSummary[session]') then
		open #hSummary=fnH: 'Name=PrnSummary[session]',display,input ! ioerr SpFinis
		fnsavetoasstart('[at]'&fnReportCacheFolderCurrent$&'\Invoice\Archive\ACS Invoice Summary '&date$(days(invDateMmDdYy,'mmddyy'),'ccyy-mm')&'.rtf')

		fnOpenPrn('Summary')
		pr #255: '\ql'
		dim line$*80
		do
			linput #hSummary: line$ eof SpEoI
			pr #255: line$
		loop
		SpEoI: !
		close #hSummary,free:
		hSummary=0
		pr #255:
		pr #255: 'Total of Invoices Printed:  '&cnvrt$('N 12.2',totalInvoicesPrinted)
		pr #255: 'Total of Previous Balances: '&cnvrt$('N 12.2',totalPreviousBalances)
		pr #255: '}' ! end the RTF font size setting of 8
		fnClosePrn
	end if
	SpFinis: !
	totalInvoicesPrinted=0
	totalPreviousBalances=0
	! pause
fnend


def fn_price(scode$,stm$; ___,h,returnN,which)
	! retains local setupPrice, mat priceKey$,mat price,priceCount,form$,mat price$,mat priceN
	if ~setupPrice then
		setupPrice=1
		h=fn_open('CO Prices',mat price$,mat priceN,mat form$, 1)
		mat priceKey$(0)
		mat price(0)
		do
			read #h,using form$(h): mat price$, mat priceN eof EoPrices
			fnAddOneC(mat priceKey$,price$(p_sysid)&'.'&price$(p_timeFrame))
			priceCount=fnAddOneN(mat price,priceN(p_price))
		loop
		EoPrices: !
		close #h:
	end if
	which=srch(mat priceKey$,scode$&'.'&stm$)
	if which<=0 then
		pr bell;'could not find price for "'&scode$&'.'&stm$&'"'
		pause
		returnN=0
	else
		returnN=price(which)
	end if
	fn_price=returnN
fnend

def library fnMergeInvoices
	fn_setup
	fnMergeInvoices=fn_mergeInvoices
fnend
def fn_mergeInvoices
	! fnTop(program$,cap$='Merge Invoices written to temp file S:\Core\Data\acsllc\tmpInvoice.h[cno]')
	dim ta(25,2),fb(25),e$*9,xb(8),sc$*4
	! clmstr dims
	dim ca(10),sCa(10)
	fnStatus('Merging Invoices...')
	open #h_tmwk2=fnH: 'Name=S:\Core\Data\acsllc\tmpInvoice.h[cno],NoShr',internal,input
	

	dim k$*5
	dim iv$*12
	dim cde$(30)*6
	dim id$(30)*128
	dim inv_amt(30)
	dim ct(30)
	dim tmwk2_sc$(30)
	open #h_artrans=fnH:  'Name=S:\Core\Data\acsllc\ARTrans.h[cno],Shr',internal,outIn,relative
	open #h_tmtrans=fnH:  'Name=S:\Core\Data\acsllc\TMTRANS.h[cno],Shr',internal,outIn,relative
	open #h_clmstr=fnH:   'Name=S:\Core\Data\acsllc\CLmstr.h[cno],KFName=S:\Core\Data\acsllc\CLIndex.h[cno],Shr',internal,outIn,keyed
	open #h_tmtraddr=fnH: 'Name=S:\Core\Data\acsllc\TMTRAddr.h[cno],Shr',internal,outIn,relative
	do  ! r: main loop
		READ_TMWK: !
		read #h_tmwk2,using F_TMWK2b: k$,xb(7),xb(4),iv$,mat cde$,mat id$,mat inv_amt,mat ct,mat tmwk2_sc$ eof MiFinis
		F_TMWK2b: form pos 1         ,c 5,n 1,n 6,c 12,30*c 6,30*c 128,30*pd 5.2,30*n 2,30*c 2
		! pr k$ : pause
		if rtrm$(k$)='' or rtrm$(k$)='0' then goto READ_TMWK
		read #h_clmstr,using F_CLMSTR,key=k$: e$,mat sCa,mat ca,ar1 nokey READ_TMWK
		F_CLMSTR: form pos 179,c 9,pos 220,10*n 1,10*pd 3,pos 283,pd 5.2
		if xb(7)=3 and rtrm$(iv$)='' then iv$='WRITE OFF'
		iv$=lpad$(rtrm$(iv$),12)
		xb(7)=-xb(7)
		for j=1 to udim(mat id$)
			if inv_amt(j)=0 then goto NEXT_ONE
			amt=amt+inv_amt(j)
			xb(3)=inv_amt(j)
			xb(5)=ct(j) ! inv_amt(j+10) ! Category i.e. 6,2
			xb(8)=fnval(tmwk2_sc$(j)) ! System Code

			if xb(8)=0 then b8N=25 else b8N=xb(8)
			L390: !
			lta=lrec(h_tmtrans)+1
			write #h_tmtrans,using 'form pos 1,c 5,c 9,2*pd 3.2,pd 4.2,n 6,n 2,pd 2,pd 1,n 2,c 4,c 12,pd 3,c 30',rec=lta,reserve: k$,' ',mat xb,sc$,iv$,0,id$(j)(1:30) duprec L390
			rewrite #h_tmtrans,using 'form pos 54,pd 3',rec=1,release: lta
			if xb(5)=0 or ca(xb(5))=0 then goto THAT_STEP ! added xb(5)=0 on 2/1/2012
				! if b8N>25 then b8N=25 ! goto NEXT_ONE
				p1=1+(b8N-1)*6
				p2=150+b8N

				read #h_tmtraddr,using F_TMTRADDR,rec=ca(xb(5)),reserve: ta1,ta2,fb1 noRec NEXT_ONE
				F_TMTRADDR: form pos p1,2*pd 3,pos p2,n 1
				if ta2><0 then rewrite #h_tmtrans,using 'form pos 54,pd 3',rec=ta2: lta else ta1=lta
				if fb1<2 then fb1=abs(xb(7))
				if ta1=0 then ta1=lta
				rewrite #h_tmtraddr,using F_TMTRADDR,rec=ca(xb(5)),release: ta1,lta,fb1
				goto NEXT_ONE
			THAT_STEP: !
			mat ta=(0)
			mat fb=(0)
			if xb(5)>0 then ca(xb(5))=lta4 ! added xb(5)>0 on 2/1/2012
			ta(b8N,1)=lta
			ta(b8N,2)=lta
			if xb(7)=-2 then fb(b8N)=2
			if fb(b8N)=2 then goto L630
			if xb(7)=-1 then fb(b8N)=1
			L630: !
			write #h_tmtraddr,using 'form pos 1,50*pd 3,25*n 1',reserve: mat ta,mat fb
			lta4=lrec(4)
			rewrite #h_tmtraddr,using 'form pos 1,pd 3',rec=1,release: lta4
			NEXT_ONE: !
		next j
		if abs(xb(7))<>3 then  ! SKIP AR IF WRITE OFF
			write #h_artrans,using 'form pos 1,c 5,c 12,n 6,2*pd 5.2,pd 2,2*n 1,c 20,pd 3',reserve: k$,iv$,xb(4),amt,amt,0,1,0,'CHARGE',0
			ar1+=amt
		end if
		if xb(7)=-2 and xb(5)>0 then sCa(xb(5))=2 ! added xb(5)>0 on 2/1/2012
		rewrite #h_clmstr,using 'form pos 220,10*n 1,10*pd 3,pos 283,pd 5.2',key=k$: mat sCa,mat ca,ar1
		amt=0
	loop  ! /r

	MiFinis: !
	close #h_clmstr:
	close #h_tmtrans:
	close #h_tmwk2:
	close #h_tmtraddr:
	fnStatusClose

fnend

include: fn_open
include: fn_setup
