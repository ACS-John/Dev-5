! formerly acsTM\moInvoice
fn_setup
fnTop(program$)
client_id_sageAx=3811
client_id_brc=90
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
fnStatus('producing Invoice Archive...')
fnInvoiceOpen
	! fn_produceInvoices
	! r: def fn_produceInvoices
	open #hClient=fnH: 'Name=S:\Core\Data\acsllc\CLmstr.h[cno],KFName=S:\Core\Data\acsllc\CLIndex.h[cno],Shr',internal,input,keyed
	! pr 're-indexing support, just in case - probably not necessary to do so often, but one time there was this problem.'
	! fnIndex('S:\Core\Data\acsllc\support.h[cno]','S:\Core\Data\acsllc\support-idx.h[cno]','1/7,6/2')
	open #h_support=fnH: 'Name=S:\Core\Data\acsllc\Support.h[cno],KFName=S:\Core\Data\acsllc\support-idx.h[cno],Shr',internal,input,keyed
	F_support: form pos 1,g 6,n 2,c 2,x 8,c 2,n 8,n 10.2,4*c 50

	! dim timesheet$(0)*128
	! dim timesheetN(0)
	! hTimeSheet=fn_open('TM timeSheet',mat timesheet$, mat timesheetN, mat form$)
	! fnIndex('TMSHT[wsid]','TMSHT-IDX[wsid]','1,5')
	fn_combineIntoTmSht('S:\Core\Data\acsllc\TimeSheet.h[cno]')
	open #hTimeSheet=fnH: 'Name=TmSht[session],KFName=TmSht-Idx[session]',internal,outIn,keyed

	fnStatus('Printing Invoices...')

	open #h_tmwk2=fnH: 'Name=S:\Core\Data\acsllc\tmpInvoice.h[cno],Replace,RecL=4675,Shr',internal,outIn
	dim cde$(30)*6
	dim inv_item$(30)*128
	dim inv_amt(30)
	dim inv_category(30)
	dim inv_service_code(30)
	dim inv_gl$(30)*12
	dim client_id$*5
	dim client_addr$(3)*30
	do
		read #hClient,using 'form pos 1,c 5,3*c 30,pos 283,pd 5.2': client_id$,mat client_addr$,pbal eof EoClient
		client_id=val(client_id$)
		dim iv$*12
		iv$=rpad$(str$(invoice_number),12)
		restore #h_support: ! ,key>=lpad$(trim$(client_id$),kln(h_support)): nokey EoSupport
		do
			read #h_support,using F_support: cln$,scode,scode$,stm$,sup_exp_date,supData_cost eof EoSupport
			cln=val(cln$)
			!     if client_id=918 then pr 'cln=';cln;'client_id=';client_id ! pause
			if cln=client_id then

				needsRenewal=0 ! if it expires this month
				if int(invoiceDateCcyymmdd*.01)=int(sup_exp_date*.01) then needsRenewal=1

				if stm$='Mo' and needsRenewal then pr 'monthly bill encountered.  please test code before accepting.' : pause

				if needsRenewal then
						invTotal+=fn_billForMaintenance(stm$,scode$,scode,client_id,supData_cost,invTotal,inv_line,mat inv_item$,mat inv_amt,mat inv_category,mat inv_service_code,mat inv_gl$)
				end if
			end if
		loop  !  while cln=client_id ! commented out to work around a critical nokey problem above.  should severely slow things down though
		EoSupport: !

		fn_print_inv
	loop
	EoClient: !
	close #hTimeSheet:
	close #h_ivnum:
	close #hClient:
	close #h_tmwk2:
	! /r fnend


fnInvoiceClose(invDateMmDdYy, 'ACS Invoices')

execute 'sy -c -w explorer "'&fnReportCacheFolderCurrent$&'\Ebilling"'
execute 'sy -c -w explorer "'&fnReportCacheFolderCurrent$&'\Invoice\Archive"'
execute 'sy -c -w explorer "'&fnReportCacheFolderCurrent$&'\Invoice\Print"'

fnStatus('producing Summary...')
fn_summaryRelease


! r: final screen
do
	fntos : lc=0
	fnOpt(lc+=1,41,'Merge Invoices and Email Queued Invoices',1)
	resp$(1)='True'
	fnOpt(lc+=1,1,'Merge Invoices only')
	resp$(2)='False'
	fnOpt(lc+=1,1,'Stop (neither merge, nor email)')
	resp$(3)='False'
	fnCmdSet(2)
	fnAcs(mat resp$,ckey)
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

def fn_billForMaintenance(stm$,scode$,scode,client_id,supData_cost,&invTotal,&inv_line,mat inv_item$,mat inv_amt,mat inv_category,mat inv_service_code,mat inv_gl$; ___,returnN)
	if supData_cost=0 then supData_cost=fn_price(scode$,stm$)
	if supData_cost=0 then pr 'zero price???' : pause

	if supData_cost>0 then
		returnN=supData_cost
		inv_line+=1

		if stm$='An' then
			inv_item$(inv_line)='Annual'
		else if stm$='Qt' then
			inv_item$(inv_line)='Quarterly'
		else if stm$='Mo' then
			inv_item$(inv_line)='Monthly'
		else
			inv_item$(inv_line)='(unexpected timeframe)'
			pr inv_item$(inv_line) : pause
		end if
		if scode$='U4' then
			inv_item$(inv_line)=inv_item$(inv_line)&' Maintenance for (UB) Hand Held Add-On'
		else
			inv_item$(inv_line)=inv_item$(inv_line)&' Maintenance for '&trim$(fnSystemNameFromId$(scode))
			if trim$(fnSystemNameFromId$(scode))='' then
				pr ' sending blank system name  scode='&str$(scode)
				pr '   client_id=';client_id
				pause
			end if
		end if
		inv_amt(inv_line)=supData_cost
		inv_category(inv_line)=6
		inv_service_code(inv_line)=scode
		inv_gl$(inv_line)='  0  1160  0'
	end if
	fn_billForMaintenance=returnN
fnend

Xit: fnXit
dim resp$(30)*128
def fn_askScreen1(&invDateMmDdYy,&invoice_number; ___,returnN,invDay)
	if ~invDateMmDdYy then invDateMmDdYy=date(fnEndOfMonth(days(date$)-15),'mmddyy')
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
	fnAcs(mat resp$,ckey)
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

def fn_combineIntoTmSht(file_from$*256; ___,wo_desc$*30)
	dim tce_to_inp(7)
	open #tce_h_from=fnH: 'Name='&file_from$,internal,input
	open #tce_h_to=fnH: 'Name=TmSht[session],KFName=TmSht-Idx[session],Replace,RecL='&str$(rln(tce_h_from))&',KPs=1/36/25,KLn=5/2/6',internal,outIn,keyed
	do
		read #tce_h_from,using F_TIME: mat inpX,b6,b7,b8,sc,o_o,wo_desc$ eof TCE_EOF
		if b8=20 then b8=19 ! ALL PRINTING SUPPORT IS COVERED BY CORE
		tce_key$=cnvrt$('N 5',inpX(1))&cnvrt$('N 2',b8)&cnvrt$('N 6',inpX(6))
		!   IF inpX(1)=2040 then pause
		read #tce_h_to,using F_TIME,key=tce_key$: mat tce_to_inp nokey TCE_TO_ADD
		inpX(3)+=tce_to_inp(3) ! time
		inpX(5)+=tce_to_inp(5) ! charge
		rewrite #tce_h_to,using F_TIME,key=tce_key$: mat inpX,b6,b7,b8,sc,o_o,wo_desc$
		!   if inpX(1)=4568 then rewr_count+=1
		goto TCE_NEXT
		TCE_TO_ADD: !
		write #tce_h_to,using F_TIME: mat inpX,b6,b7,b8,sc,o_o,wo_desc$
		!   if inpX(1)=4568 then wr_count+=1
		TCE_NEXT: !
		! if inpX(1)=4568 then pr '4568 (';b8;') had a time of ';inpX(3);' on ';inpX(6);' charge=';inpX(5)
	loop
	TCE_EOF: !
	close #tce_h_from:
	close #tce_h_to:
	! pr 'rewr_count=';rewr_count
	! pr '  wr_count=';wr_count
	! pause

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
	if (pbal+invTotal)>1 then 
		pr #hSummary,using Fsummary: client_id$,client_addr$(1)(1:14),invDateMmDdYy,pbal,invTotal,pbal+invTotal,piv$
		Fsummary: form pos 1,c 5,x 2,c 15,pic(zz/zz/zz),3*nz 12.2,x 2,c 12
		totalInvoicesPrinted+=invTotal
		totalPreviousBalances+=pbal
	end if
fnend
def fn_summaryRelease
	! pause
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

def fn_print_inv
	fn_billForNonMaint(hTimeSheet)
	if enableMinimumMonthlyBill and invTotal>0 and sum(mat inv_amt)<100 then
		inv_line+=1
		if inv_line<30 then
			inv_item$(inv_line)='Minimum Monthly Billing of $100.00'
			inv_amt(inv_line)=100-sum(mat inv_amt)
			invTotal+=inv_amt(inv_line)
			inv_service_code(inv_line)=19
			inv_gl$(inv_line)='  0  1160  0'
		end if
	end if
	if invTotal=>1 then
		write #h_tmwk2,using F_TMWK2a: client_id$,2,invDateMmDdYy,iv$,mat cde$,mat inv_item$,mat inv_amt,mat inv_category,mat inv_service_code,mat inv_gl$
		F_TMWK2a: form pos 1,c 5,n 1,n 6,c 12,30*c 6,30*c 128,30*pd 5.2,30*n 2,30*n 2,30*c 12
	end if
	if invTotal=>1 or pbal=>1 then
		fn_summaryAccumulate
		fnInvoiceAdd(client_id$, mat client_addr$,iv$,invDateMmDdYy,mat inv_item$,mat inv_amt,pbal)
		invoice_number+=1
	end if
	mat inv_item$=(' ')
	mat inv_category=(0)
	mat inv_service_code=(0)
	mat inv_gl$=('')
	mat inv_amt=(0)
	inv_line=invTotal=0
fnend
def fn_billForNonMaint(hTimeSheet; ___,wo_desc$*30) ! add charges not under maintenance to maintenance invoices
	dim inpX(7)
	read #hTimeSheet,using F_TIME,key=>rpad$(client_id$,kln(hTimeSheet)): mat inpX,b6,b7,b8,sc,o_o,wo_desc$ nokey TM_XIT2
	F_TIME: form pos 1,g 5,n 9,2*pd 3.2,pd 4.2,n 6,n 2,pd 2,pd 1,n 2,n 4,x 12,pd 3,c 30
	if inpX(1)=val(client_id$) then
		do
			if b8=0 then b8=19
			delete #hTimeSheet: ioerr ignore ! delete current record so it is not processed twice
			! fn_billForHours(client_id$)
			! def fn_billForHours(client_id$) ! ,mat inpX,etc...
			if inv_line=30 then fn_print_inv ! pr invoice if more than 20 entries
			if inv_line>29 then pause
			spk$=' '&client_id$&cnvrt$('n 2',b8)

			if inpX(7)=2 then goto BfhGo ! always bill modifications

			if inpX(7)=23 or inpX(7)=11 then goto BfhXit ! always no charge

			if inpX(7)<>2 then
				read #h_support,using F_support,key=spk$: cln$,scode,scode$,stm$,sup_exp_date,supData_cost nokey BfhGo
				trans_date=date(days(inpX(6),'mmddyy'),'ccyymmdd')
				if (trans_date<=sup_exp_date) then goto BfhXit !  it covered by maintenance
			end if

			BfhGo: !
			supData_cost=inpX(5)

			invTotal+=supData_cost
			inv_line+=1
			! if val(client_id$)=3828 then pr 'schachtner encountered inv_line=';inv_line : pause
			if val(client_id$)=client_id_sageAx or val(client_id$)=client_id_brc then
				!     pause  ! inv_item$(inv_line)=str$(inpX(3))&' hours at a rate of '&&' on '&cnvrt$('pic(##/##/##)',inpX(6))
				inv_item$(inv_line)=str$(inpX(3))&' hours at a rate of '&cnvrt$('pic($$#.##)',inpX(4))&' on '&cnvrt$('pic(##/##/##)',inpX(6))
			else if inpX(7)=2 then
				inv_item$(inv_line)=str$(inpX(3))&' hours of '&trim$(fnSystemNameFromId$(b8))&' programming on '&cnvrt$('pic(##/##/##)',inpX(6))
			else
				inv_item$(inv_line)=str$(inpX(3))&' hours of '&trim$(fnSystemNameFromId$(b8))&' support on '&cnvrt$('pic(##/##/##)',inpX(6))
			end if

			inv_amt(inv_line)=inpX(5)
			inv_category(inv_line)=6
			inv_service_code(inv_line)=b8
			inv_gl$(inv_line)='  0  1160  0'
			BfhXit: !
			! fnend

			read #hTimeSheet,using F_TIME: mat inpX,b6,b7,b8,sc,o_o,wo_desc$ eof TM_XIT2
		loop while inpX(1)=client_id
	end if
	TM_XIT2: !
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
	dim ta(25,2),fb(25),iv$*12,k$*5,e$*9,xb(8),sc$*4
	! clmstr dims
	dim ca(10),sc(10)
	fnStatus('Merging Invoices...')
	open #h_tmwk2=fnH: 'Name=S:\Core\Data\acsllc\tmpInvoice.h[cno],NoShr',internal,input
	F_TMWK2: form pos 1,c 5,n 1,n 6,c 12,30*c 6,30*c 128,30*pd 5.2,30*n 2,30*n 2

	dim cde$(30)*6
	dim id$(30)*128
	dim inv_amt(30)
	dim tmwk_sc(30)
	dim ct(30)
	dim tmwk2_sc(30)
	open #h_artrans=fnH:  'Name=S:\Core\Data\acsllc\ARTrans.h[cno],Shr',internal,outIn,relative
	open #h_tmtrans=fnH:  'Name=S:\Core\Data\acsllc\TMTRANS.H[cno],Shr',internal,outIn,relative
	open #h_clmstr=fnH:   'Name=S:\Core\Data\acsllc\CLmstr.h[cno],KFName=S:\Core\Data\acsllc\CLIndex.h[cno],Shr',internal,outIn,keyed
	open #h_tmtraddr=fnH: 'Name=S:\Core\Data\acsllc\TMTRAddr.h[cno],Shr',internal,outIn,relative
	do  ! r: main loop
		READ_TMWK: !
		read #h_tmwk2,using F_TMWK2: k$,xb(7),xb(4),iv$,mat cde$,mat id$,mat inv_amt,mat ct,mat tmwk2_sc eof MiFinis
		! pr k$ : pause
		if rtrm$(k$)='' or rtrm$(k$)='0' then goto READ_TMWK
		read #h_clmstr,using F_CLMSTR,key=k$: e$,mat sc,mat ca,ar1 nokey READ_TMWK
		F_CLMSTR: form pos 179,c 9,pos 220,10*n 1,10*pd 3,pos 283,pd 5.2
		if xb(7)=3 and rtrm$(iv$)='' then iv$='WRITE OFF'
		iv$=lpad$(rtrm$(iv$),12)
		xb(7)=-xb(7)
		for j=1 to udim(mat id$)
			if inv_amt(j)=0 then goto NEXT_ONE
			amt=amt+inv_amt(j)
			xb(3)=inv_amt(j)
			xb(5)=ct(j) ! inv_amt(j+10) ! Category i.e. 6,2
			xb(8)=tmwk2_sc(j) ! System Code

			if xb(8)=0 then b8=25 else b8=xb(8)
			L390: !
			lta=lrec(h_tmtrans)+1
			write #h_tmtrans,using 'form pos 1,c 5,c 9,2*pd 3.2,pd 4.2,n 6,n 2,pd 2,pd 1,n 2,c 4,c 12,pd 3,c 30',rec=lta,reserve: k$,' ',mat xb,sc$,iv$,0,id$(j)(1:30) duprec L390
			rewrite #h_tmtrans,using 'form pos 54,pd 3',rec=1,release: lta
			if xb(5)=0 or ca(xb(5))=0 then goto THAT_STEP ! added xb(5)=0 on 2/1/2012
				if b8>25 then b8=25 ! goto NEXT_ONE
				p1=1+(b8-1)*6
				p2=150+b8

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
			ta(b8,1)=lta
			ta(b8,2)=lta
			if xb(7)=-2 then fb(b8)=2
			if fb(b8)=2 then goto L630
			if xb(7)=-1 then fb(b8)=1
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
		if xb(7)=-2 and xb(5)>0 then sc(xb(5))=2 ! added xb(5)>0 on 2/1/2012
		rewrite #h_clmstr,using 'form pos 220,10*n 1,10*pd 3,pos 283,pd 5.2',key=k$: mat sc,mat ca,ar1
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
