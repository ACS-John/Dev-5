! formerly acsTM\moInvoice
fn_setup
	fntop(program$)
	client_id_sageAx=3811
	client_id_brc=90
	enableMinimumMonthlyBill=1
	
	dim b(8)
	dim iv$*12

	dim inpX(7)
	fncreg_read('Last Invoice Number',tmp$, '1704001') : invoice_number=val(tmp$)
	invoice_number+=1
	if invoice_number=1 then invoice_number=val(date$(days(date$)-20,'yymm')&'01')
	let Clientebilling=fnClient_has('EM')
	SCREEN1: !
	fn_askScreen1(invDateMmDdYy,invoice_number)
	invoiceDateCcyymmdd=date(days(invDateMmDdYy,"mmddyy"),"ccyymmdd")
	fnAutomatedSavePoint('before')
	open #hClient:=fngethandle: "Name=S:\Core\Data\acsllc\CLmstr.h[cno],KFName=S:\Core\Data\acsllc\CLIndex.h[cno],Shr",internal,input,keyed 
! pr 're-indexing support, just in case - probably not necessary to do so often, but one time there was this problem.'
! execute "Index S:\Core\Data\acsllc\support.h[cno]  S:\Core\Data\acsllc\support-idx.h[cno] 1/7,6/2,replace,DupKeys"
	open #h_support:=fngethandle: "Name=S:\Core\Data\acsllc\Support.h[cno],KFName=S:\Core\Data\acsllc\support-idx.h[cno],Shr",internal,input,keyed 
	F_SUPPORT: form pos 1,g 6,n 2,c 2,n 8,c 2,n 8,n 10.2,4*c 50
	fn_thsht_combine_entries("S:\Core\Data\acsllc\TimeSheet.h[cno]","TMSHT"&wsid$,"TMSHT-IDX"&wsid$)
	! restore #hClient,key>=lpad$(str$(starting_acct_no),5): nokey SCREEN1
	pr newpage
	pr f "10,10,Cc 60": "Printing Invoices..."
! r: process "S:\Core\Data\acsllc\TMWK2"&wsid$&".h[cno]"
	open #h_tmwk2:=fngethandle: 'Name=S:\Core\Data\acsllc\tmpInvoice.h[cno],Replace,RecL=4675,Shr',internal,outIn 
	F_TMWK2: form pos 1,c 5,n 1,n 6,c 12,30*c 6,30*c 128,30*pd 5.2,30*n 2,30*n 2,30*c 12
	dim cde$(30)*6
	dim inv_item$(30)*128
	dim inv_amt(30)
	dim inv_category(30)
	dim inv_category(30)
	dim inv_service_code(30)
	dim inv_gl$(30)*12
	! restore #h_tmwk2:
	dim client_id$*5
	dim client_addr$(3)*30
	do 
		read #hClient,using 'form pos 1,c 5,3*c 30,pos 283,pd 5.2': client_id$,mat client_addr$,pbal eof EOJ
		client_id=val(client_id$)
		iv$=rpad$(str$(invoice_number),12)
		restore #h_support: ! ,key>=lpad$(trim$(client_id$),kln(h_support)): nokey BMM_SUPPORT_EOF
		do 
			read #h_support,using F_SUPPORT: cln$,scode,scode$,sdt1,stm$,sup_exp_date,supData_cost eof BMM_SUPPORT_EOF
			cln=val(cln$)
			!     if client_id=918 then pr 'cln=';cln;'client_id=';client_id ! pause
			if cln=client_id then 
				!       if stm$="Mo" then goto BillMaint ! always bill monthly
				if stm$="Mo" then goto NXTJ ! never bill monthly
				!        pr "An";int(invoiceDateCcyymmdd*.01);'=';int(sup_exp_date*.01);' !  bill annual if it expires this month'
				if stm$="An" and int(invoiceDateCcyymmdd*.01)=int(sup_exp_date*.01) then goto BillMaint ! bill annual if it expires this month
				if stm$="An" then goto NXTJ ! skip annual people
				pr 'ZZZAAABBB i did not think it could reach here 7/2/2019' : pause
				if invoiceDateCcyymmdd<=sup_exp_date then goto NXTJ ! on maintenance
				BillMaint: ! r:
				if supData_cost>0 then 
					b(3)=supData_cost
					b(8)=scode
					b3=b3+b(3)
					inv_line=inv_line+1
					if stm$="An" then 
						inv_item$(inv_line)='Annual'
					else 
						inv_item$(inv_line)='Monthly'
					end if 
					if scode$='U4' then 
						inv_item$(inv_line)=inv_item$(inv_line)&" Maintenance for (UB) Hand Held Add-On"
					else 
						inv_item$(inv_line)=inv_item$(inv_line)&" Maintenance for "&trim$(fnSystemNameFromId$(scode))
						if trim$(fnSystemNameFromId$(scode))='' then 
							pr ' sending blank system name  scode='&str$(scode)
							pr '   client_id=';client_id
							pause 
						end if
					end if 
					inv_amt(inv_line)=supData_cost
					inv_category(inv_line)=6
					inv_service_code(inv_line)=scode
					inv_gl$(inv_line)="  0  1160  0"
				end if
				! /r
			end if 
			NXTJ: ! 
		loop  !  while cln=client_id ! commented out to work around a critical nokey problem above.  should severely slow things down though
		BMM_SUPPORT_EOF: ! if client_id=4568 then pr 'A (maintenance renewal stuff) 4568 encountered' : pause
		! if client_id=918 then pr 'processing client 918 COMPLETE' : pause
		fn_print_inv
	loop ! /r
EOJ: ! r:
	fn_summary_print
	fnClosePrn
	open #h_ivnum:=fngethandle: "Name=S:\Core\Data\acsllc\IVNUM.h[cno],Use,RecL=8,Shr",internal,outIn,relative 
	rewrite #h_ivnum,using "Form POS 1,N 8",rec=1: invoice_number-1
	close #h_ivnum: 
	close #hClient: 
	close #h_tmwk2: 
	do
		fntos : lc=0
		fnOpt(lc+=1,41,'Merge Invoices and Email Queued Invoices',1)
		resp$(1)='True'
		fnOpt(lc+=1,1,'Merge Invoices only')
		resp$(2)='False'
		fnOpt(lc+=1,1,'Stop (neither merge, nor email)')
		resp$(3)='False'
		fnCmdSet(2)
		fnAcs2(mat resp$,ckey)
		if ckey=5 or resp$(3)='True' then
			goto Xit
		else if resp$(1)='True' then
			fnEmailQueuedInvoices(str$(invoiceDateCcyymmdd))
			fnChain("S:\acsTM\TMMRGINV")
		else if resp$(2)='True' then
			fnChain("S:\acsTM\TMMRGINV")
		end if
	loop
	! /r
XIT: fnxit
! _____________________________________________________________________
def fn_setup
	if ~setup then
		setup=1
		library 'S:\Core\Library': fnComboF
		library 'S:\Core\Library': fnSystemNameFromId$
		library 'S:\Core\Library': fnPrintInvoice
		library 'S:\Core\Library': fnTop,fnXit
		library 'S:\Core\Library': fnOpenPrn,fnClosePrn
		library 'S:\Core\Library': fnGethandle
		library 'S:\Core\Library': fnAutomatedSavePoint
		library 'S:\Core\Library': fnCreg_read
		library 'S:\Core\Library': fnEndOfMonth
		library 'S:\Core\Library': fnCustomerHasEbilling
		library 'S:\Core\Library': fnMakesurepathexists
		library 'S:\Core\Library': fnCopy
		library 'S:\Core\Library': fnPrint_file_name$
		library 'S:\Core\Library': fnClient_has
		library 'S:\Core\Library': fnReport_cache_folder_current$
		library 'S:\Core\Library': fnChain
		library 'S:\Core\Library': fnTos,fnlbl,fncmdset,fnAcs2,fntxt
		library 'S:\Core\Library': fnOpt
		library 'S:\Core\Library': fnEmailQueuedInvoices
		library 'S:\Core\Library': fnSaveToAsStart
		dim resp$(30)*128
	end if
fnend
def fn_askScreen1(&invDateMmDdYy,&invoice_number; ___,returnN)
	if ~invDateMmDdYy then invDateMmDdYy=date(fnEndOfMonth(days(date$)-15),'mmddyy')
	fntos : rc=lc=0

	fnlbl(lc+=1, 2,'Invoice Month:'          ,24,1)
	fnComboF('month',lc,26,20,'S:\Core\Data\month.dat',1,2,3,18,'S:\Core\Data\month.idx',1,0,'Select Month Billing for - the month you are ending')
	resp$(resp_invMonth:=rc+=1)=str$(val(date$(days(invDateMmDdYy,'mmddyy'),'m')))
	
	fnlbl(lc+=1, 2,'Invoice Year (ccyy):'    ,24,1)
	fntxt(lc   ,26,4, 0,0,'number')
	resp$(resp_invYear:=rc+=1)=date$(days(invDateMmDdYy,'mmddyy'),'ccyy')

	fnlbl(lc+=1, 2,'Starting Invoice Number:',24,1)
	fntxt(lc   ,26,12, 0,0,'number')
	resp$(resp_invNo:=rc+=1)=str$(invoice_number)

	fnCmdSet(2)
	fnAcs2(mat resp$,ckey)
	if ckey=5 then
		returnN=99
	else 
		returnN=0
		! invDateMmDdYy=val(resp$(resp_invDate)) ! date(days(resp$(1),'ccyymmdd'),'mmddyy')
		invoice_number=val(resp$(resp_invNo))
		invMonth=val(resp$(resp_invMonth)(1:2))
		invYear=val(resp$(resp_invYear))
		invDateMmDdYy=fnEndOfMonth( days(cnvrt$('pic(##)',invMonth)&'15'&cnvrt$('pic(####)',invYear),'mmddyy') )
		pr "Invoice Date MUST match expiration date of Annual Support contracts!"
		pr 'invDateMmDdYy (mmddyy) =';invDateMmDdYy
		Pause
	end if
	fn_askScreen1=returnN
fnend

def fn_timesheet2(hTimeSheet; ___,wo_desc$*30) ! add charges not under maintenance to maintenance invoices
	read #hTimeSheet,using F_TIME,key=client_id$: mat inpX,b6,b7,b8,sc,o_o,wo_desc$ nokey TM_XIT2
	F_TIME: form pos 1,g 5,n 9,2*pd 3.2,pd 4.2,n 6,n 2,pd 2,pd 1,n 2,n 4,x 12,pd 3,c 30
	do 
		if b8=0 then b8=19
		delete #hTimeSheet: ioerr ignore ! delete current record so it is not processed twice
		fn_bld_rec(client_id$)
		read #hTimeSheet,using F_TIME: mat inpX,b6,b7,b8,sc,o_o,wo_desc$ eof TM_XIT2
	loop while inpX(1)=client_id
	TM_XIT2: ! 
fnend 
def fn_bld_rec(client_id$)
	if inv_line=30 then let fn_print_inv ! pr invoice if more than 20 entries
	if inv_line>29 then pause 
	spk$=" "&client_id$&cnvrt$("n 2",b8)
	if inpX(7)=2 then goto BLD_REC_L1780 ! always bill modifications
	if inpX(7)=23 or inpX(7)=11 then goto BLD_XIT ! always no charge
	read #h_support,using F_SUPPORT,key=spk$: cln$,scode,scode$,sdt1,stm$,sup_exp_date,supData_cost nokey BLD_REC_L1780
	trans_date=date(days(inpX(6),'mmddyy'),'ccyymmdd')
	!   if stm$="Mo" or (stm$='An' and trans_date<=sup_exp_date) then goto BLD_XIT ! TM_RD2 ! on maintenance
	if (stm$='An' and trans_date<=sup_exp_date) then goto BLD_XIT ! TM_RD2 ! on maintenance
	! if client_id=4625 then pause
	BLD_REC_L1780: ! 
	b(3)=inpX(5)
	b(8)=b8
	b3=b3+b(3)
	inv_line+=1
	! if val(client_id$)=3828 then pr 'schachtner encountered inv_line=';inv_line : pause
	if val(client_id$)=client_id_sageAx or val(client_id$)=client_id_brc then 
		!     pause  ! inv_item$(inv_line)=str$(inpX(3))&' hours at a rate of '&&' on '&cnvrt$("pic(##/##/##)",inpX(6))
		inv_item$(inv_line)=str$(inpX(3))&' hours at a rate of '&cnvrt$('pic($$#.##)',inpX(4))&' on '&cnvrt$("pic(##/##/##)",inpX(6))
	else if inpX(7)=2 then 
		inv_item$(inv_line)=str$(inpX(3))&' hours of '&trim$(fnSystemNameFromId$(b8))&" programming on "&cnvrt$("pic(##/##/##)",inpX(6))
	else 
		inv_item$(inv_line)=str$(inpX(3))&' hours of '&trim$(fnSystemNameFromId$(b8))&" support on "&cnvrt$("pic(##/##/##)",inpX(6))
	end if 
	
	inv_amt(inv_line)=inpX(5)
	inv_category(inv_line)=6
	inv_service_code(inv_line)=b8
	inv_gl$(inv_line)="  0  1160  0"
	BLD_XIT: ! 
fnend 
def fn_get_system_list(mat sys_name$)
	mat sys_name$(40)
	sys_name$(01)="General Ledger"
	sys_name$(02)="Accounts Receivable"
	sys_name$(03)="Accounts Payable"
	sys_name$(04)="Utility Billing"
	sys_name$(05)="Patient Billing"
	sys_name$(06)="Property Tax"
	sys_name$(07)="Accountants G.L."
	sys_name$(08)="Fixed Asset"
	sys_name$(09)="Time Management"
	sys_name$(10)="Cash Register"
	sys_name$(11)="Point of Sale"
	sys_name$(12)="Invoicing"
	sys_name$(13)="Inventory"
	sys_name$(14)="Payroll"
	sys_name$(15)="Purchase Order"
	sys_name$(16)="Municipal Court"
	sys_name$(17)="Electronic UB 82"
	sys_name$(18)="Checkbook"
	sys_name$(19)="Core"
	sys_name$(20)="Printing"
	sys_name$(21)="Job Cost"
	sys_name$(22)=""
	sys_name$(23)=""
	sys_name$(24)="Sales Tax"
	sys_name$(25)=""
	sys_name$(26)="ITbrain Anti-Malware"
	sys_name$(27)=""
	sys_name$(28)=""
	sys_name$(29)=""
	sys_name$(30)=""
	sys_name$(31)="Collection-Master Add-On"
	sys_name$(32)=""
	sys_name$(33)=""
	sys_name$(34)=""
	sys_name$(35)=""
	sys_name$(36)=""
	sys_name$(37)=""
	sys_name$(38)=""
	sys_name$(39)=""
	sys_name$(40)=""
fnend  ! fn_get_system_list
def fn_thsht_combine_entries(file_from$*256,file_to$*256,file_to_index$*256; ___,wo_desc$*30)
	dim tce_to_inp(7)
	open #tce_h_from:=fngethandle: 'Name='&file_from$,internal,input 
	open #tce_h_to:=fngethandle: 'Name='&file_to$&',KFName='&env$('Temp')&'\tmwksh.idx,Replace,RecL='&str$(rln(tce_h_from))&',KPs=1/36/25,KLn=5/2/6',internal,outIn,keyed 
	do 
		read #tce_h_from,using F_TIME: mat inpX,b6,b7,b8,sc,o_o,wo_desc$ eof TCE_EOF
		if b8=20 then b8=19 ! ALL PRINTING SUPPORT IS COVERED BY CORE
		tce_key$=cnvrt$("N 5",inpX(1))&cnvrt$("N 2",b8)&cnvrt$("N 6",inpX(6))
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
	execute 'index '&file_to$&' '&file_to_index$&' 1,5,replace,DupKeys,Shr'
	! dim timesheet$(0)*128
	! dim timesheetN(0)
	! hTimeSheet=fn_open('TM timeSheet',mat timesheet$, mat timesheetN, mat form$)
	open #hTimeSheet:=fngethandle: "Name="&file_to$&",KFName="&file_to_index$,internal,outIn,keyed 
fnend  ! fn_thsht_combine_entries
def fn_summary_add
	open #22: "Name=skip_prn,RecL=80,replace",display,output ioerr SI_ADD
	pr #22: "{\fs16"  ! set the RTF Font Size to 8
	pr #22: "Clnt   Name           Date      Prev Bal    New Amt     Total Due   Inv No  "
	pr #22: "_____ ______________  ________  __________  __________  __________  __________"
	SI_ADD: !
	if (pbal+b3)<1 then piv$="" else piv$=iv$ ! if less than a dollar than don't charge it
	if piv$<>'' then 
		pr #22,using Fsa22: client_id$,client_addr$(1)(1:14),invDateMmDdYy,pbal,b3,pbal+b3,piv$
		Fsa22: form pos 1,c 5,x 2,c 15,pic(zz/zz/zz),3*nz 12.2,x 2,c 12
		totalInvoicesPrinted+=b3
		totalPreviousBalances+=pbal
	end if  ! piv$<>''
fnend 
def fn_summary_print
	close #22: ioerr SP_XIT
	open #22: "Name=skip_prn",display,input 
	pr #255: "\ql"
	dim skln$*80
	do 
		linput #22: skln$ eof SP_FINIS
		pr #255: skln$
	loop 
	SP_FINIS: ! 
	pr #255: 
	pr #255: "Total of Invoices Printed:  "&cnvrt$("N 12.2",totalInvoicesPrinted)
	pr #255: "Total of Previous Balances: "&cnvrt$("N 12.2",totalPreviousBalances)
	pr #255: '}' ! end the RTF font size setting of 8
	close #22,free: 
	SP_XIT: ! 
fnend 
def fn_print_inv
	dim pdf_filename_final$*255
	fn_timesheet2(hTimeSheet)
	if enableMinimumMonthlyBill then
		if b3>0 and sum(mat inv_amt)<100 then 
			inv_line+=1
			if inv_line<30 and client_id<>client_id_sageAx and client_id<>client_id_brc and client_id<>4625 then 
				inv_item$(inv_line)='Minimum Monthly Billing of $100.00'
				inv_amt(inv_line)=100-sum(mat inv_amt) : b3+=inv_amt(inv_line)
				inv_service_code(inv_line)=19
				inv_gl$(inv_line)='  0  1160  0'
			end if  ! inv_line<30
		end if
	end if
	fn_summary_add ! pr all clients
	if b3=>1 then 
		write #h_tmwk2,using F_TMWK2: client_id$,2,invDateMmDdYy,iv$,mat cde$,mat inv_item$,mat inv_amt,mat inv_category,mat inv_service_code,mat inv_gl$
	end if
	if b3=>1 or pbal=>1 then
		! if sum(mat inv_amt)+pbal>0 then
		dim saveToAsStartFile$*1024
		saveToAsStartFile$='D:\ACS\Doc\Invoices\ACS Invoices - '
		saveToAsStartFile$&=str$(invoiceDateCcyymmdd)(1:4)&'-'&str$(invoiceDateCcyymmdd)(5:6)
		saveToAsStartFile$&='.rtf'
		fnSaveToAsStart(saveToAsStartFile$)
		fnOpenPrn
		if ClientEbilling=1 then 
			! see if customer that we're sending the invoice for right now has ebilling selected 
			ebilling=fnCustomerHasEbilling(client_id$)
		end if 
		! if trim$(client_id$)='4132' then pause
		! if trim$(client_id$)='1478' then pr ebilling : pause 
		if ebilling=0 then 
		   fnPrintInvoice(255,align,client_id$, mat client_addr$,iv$,invDateMmDdYy,mat inv_item$,mat inv_amt,pbal)
			pr #255: newpage ! mat inv_item$=("")
		else if ebilling then 
			! open pdf 
			pdf_filename_final$=fnprint_file_name$(client_id$,'pdf')
			pr 'creating:  '&pdf_filename_final$ 
			
			! print pdf
			fnPrintInvoice(pdfout,align,client_id$, mat client_addr$,iv$,invDateMmDdYy,mat inv_item$,mat inv_amt,pbal,pdf_filename_final$)
			! close pdf 
			! close #pdfout: 
			! move to Send folder 
			fnmakesurepathexists(fnreport_cache_folder_current$&"\Ebilling\")
			! pause
			fnCopy(env$('at')&os_filename$(pdf_filename_final$),env$('at')&fnreport_cache_folder_current$&"\Ebilling\ACS Invoice."&trim$(client_id$)&'.'&date$("mmddyy")&'.pdf')
			! execute 'copy "'&os_filename$(env$('at')&pdf_filename_final$)&'" "'&os_filename$("s:\Time Management\Ebilling\ACS Invoice."&trim$(client_id$)&'.'&date$("mmddyy")&'.pdf')&'"'
			exec 'sy -c "'&fnreport_cache_folder_current$&'\Ebilling\ACS Invoice.'&trim$(client_id$)&'.'&date$("mmddyy")&'.pdf"'
				menu_option$=srep$(menu_option$,'%report_cache_folder_current%',fnreport_cache_folder_current$)
				! open the folder it is in
				execute 'sy -c -w explorer "'&fnreport_cache_folder_current$&'\Ebilling"'

		end if 
		invoice_number+=1 ! moved here 10/4/11 (from below) in an attempt to stop skipping invoice numbers
		! end if
	end if
	mat inv_item$=(" ")
	mat inv_category=(0)
	mat inv_service_code=(0)
	mat inv_gl$=("")
	! invoice_number=invoice_number+1
	mat inv_amt=(0)
	inv_line=b3=0
fnend  ! fn_print_inv
include: fn_open
include: ertn
