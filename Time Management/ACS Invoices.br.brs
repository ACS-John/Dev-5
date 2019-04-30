! formerly acsTM\moInvoice
	library "S:\acsTM\print_invoice": fnprint_invoice
	library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fngethandle,fnAutomatedSavePoint,fncreg_read
	fntop(program$)
	client_id_sage_ax=3811
	client_id_brc=90
	dim sys_name$(40)*55,client_id$*5,b(8),iv$*12,skln$*80 ! co$(4)*40,
	dim client_addr$(3)*30,cap$*128
	dim in1$(3)
	dim inpX(7),wo_desc$*30
	fn_get_system_list(mat sys_name$)
	fncreg_read('Last Invoice Number',tmp$, '1704001') : invoice_number=val(tmp$)
	invoice_number+=1
	if invoice_number=1 then invoice_number=val(date$(days(date$)-20,'yymm')&'01')
	pr newpage
	pr f "5,2,cr 43": "Invoice Date (mmddyy):"
	pr f "4,30,c": "Invoice Date MUST match expiration date of Annual Support contracts!"
	pr f "6,2,cr 43": "Starting Invoice Number:"
	pr f "7,2,Cr 43": "Starting Account Number:"
	in1$(1)="5,46,Nz 6,U"
	in1$(2)="6,46,N 12,U"
	in1$(3)="7,46,N 5,U"
! inv_date=date('mmddyy')
	pr f "10,30,c 20,,B99": "Cancel (Esc)"
SCREEN1_ASK: ! 
	rinput fields mat in1$: inv_date,invoice_number,starting_acct_no conv SCREEN1_ASK
	if cmdkey=5 or fkey=99 then goto XIT
	b4=date(days(inv_date,"mmddyy"),"ccyymmdd")
	fnAutomatedSavePoint('before')
	open #h_clmstr:=fngethandle: "Name=S:\Core\Data\acsllc\CLmstr.h[cno],KFName=S:\Core\Data\acsllc\CLIndex.h[cno],Shr",internal,input,keyed 
! pr 're-indexing support, just in case - probably not necessary to do so often, but one time there was this problem.'
! execute "Index S:\Core\Data\acsllc\support.h[cno]  S:\Core\Data\acsllc\support-idx.h[cno] 1/7,6/2,replace,DupKeys"
	open #h_support:=fngethandle: "Name=S:\Core\Data\acsllc\Support.h[cno],KFName=S:\Core\Data\acsllc\support-idx.h[cno],Shr",internal,input,keyed 
F_SUPPORT: form pos 1,g 6,n 2,c 2,n 8,c 2,n 8,n 10.2,4*c 50
	fn_thsht_combine_entries("S:\Core\Data\acsllc\TIMESHEET.h[cno]","TMSHT"&wsid$,"TMSHT-IDX"&wsid$)
	restore #h_clmstr,key>=lpad$(str$(starting_acct_no),5): nokey SCREEN1_ASK
	pr newpage
	pr f "10,10,Cc 60": "Printing Invoices..."
! r: process "S:\Core\Data\acsllc\TMWK1"&wsid$&".h[cno]"
	open #h_tmwk1:=fngethandle: 'Name=S:\Core\Data\acsllc\TMWK1.h[cno],Replace,RecL=2484,Shr',internal,outIn 
	F_TMWK1: form pos 1,c 5,n 1,n 6,c 12,30*c 6,30*c 55,30*pd 5.2,30*n 2,30*n 2,30*c 12
	dim cde$(30)*6
	dim inv_item$(30)*55
	dim inv_amt(30)
	dim inv_category(30)
	dim inv_category(30)
	dim inv_service_code(30)
	dim inv_gl$(30)*12
! restore #h_tmwk1:
	do 
		read #h_clmstr,using 'form pos 1,c 5,3*c 30,pos 283,pd 5.2': client_id$,mat client_addr$,pbal eof EOJ
		client_id=val(client_id$)
		iv$=rpad$(str$(invoice_number),12)
		restore #h_support: ! ,key>=lpad$(trim$(client_id$),kln(h_support)): nokey BMM_SUPPORT_EOF
		do 
			read #h_support,using F_SUPPORT: cln$,scode,scode$,sdt1,stm$,sup_exp_date,scst eof BMM_SUPPORT_EOF
			cln=val(cln$)
			!     if client_id=918 then pr 'cln=';cln;'client_id=';client_id ! pause
			if cln=client_id then 
				!       if stm$="Mo" then goto BILL_MONTHLY_MAINT ! always bill monthly
				if stm$="Mo" then goto NXTJ ! never bill monthly
				!        pr "An";int(b4*.01);'=';int(sup_exp_date*.01);' !  bill annual if it expires this month'
				if stm$="An" and int(b4*.01)=int(sup_exp_date*.01) then goto BILL_MONTHLY_MAINT ! bill annual if it expires this month
				if stm$="An" then goto NXTJ ! skip annual people
				if b4<=sup_exp_date then goto NXTJ ! on maintenance
				BILL_MONTHLY_MAINT: ! 
				if scst=0 then goto NXTJ
				b(3)=scst
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
					inv_item$(inv_line)=inv_item$(inv_line)&" Maintenance for "&trim$(sys_name$(scode))
					if trim$(sys_name$(scode))='' then pr ' sending blank system name  scode='&str$(scode) : pause 
				end if 
				inv_amt(inv_line)=scst
				inv_category(inv_line)=6
				inv_service_code(inv_line)=scode
				inv_gl$(inv_line)="  0  1160  0"
			end if 
			NXTJ: ! 
		loop  !  while cln=client_id ! commented out to work around a critical nokey problem above.  should severely slow things down though
		BMM_SUPPORT_EOF: ! if client_id=4568 then pr 'A (maintenance renewal stuff) 4568 encountered' : pause
		! if client_id=918 then pr 'processing client 918 COMPLETE' : pause
		fn_print_inv
	loop ! /r
EOJ: ! r:
	fn_summary_print
	fncloseprn
	open #h_ivnum:=fngethandle: "Name=S:\Core\Data\acsllc\IVNUM.h[cno],Use,RecL=8,Shr",internal,outIn,relative 
	rewrite #h_ivnum,using "Form POS 1,N 8",rec=1: invoice_number-1
	close #h_ivnum: 
	close #h_clmstr: 
	close #h_tmwk1: 
	goto ASK_MERGE  ! /r
	ASK_MERGE: ! r:
	pr newpage
	pr f "10,2,c 70": "All invoices have been printed.  Enter 1 to merge, or 2 to Stop"
		L1260: input fields "10,72,n 1,eu,n": num conv L1260
	if num=1 then 
		chain "S:\acsTM\TMMRGINV"
	else if num=2 then 
		goto XIT
	else 
		goto ASK_MERGE
	end if 
	! /r
	XIT: fnxit
! _____________________________________________________________________
def fn_timesheet2(h_tmsht) ! add charges not under maintenance to maintenance invoices
	read #h_tmsht,using F_TIME,key=client_id$: mat inpX,b6,b7,b8,sc,o_o,wo_desc$ nokey TM_XIT2
	F_TIME: form pos 1,g 5,n 9,2*pd 3.2,pd 4.2,n 6,n 2,pd 2,pd 1,n 2,n 4,x 12,pd 3,c 30
	do 
		if b8=0 then b8=19
		delete #h_tmsht: ioerr ignore ! delete current record so it is not processed twice
		fn_bld_rec(client_id$)
		read #h_tmsht,using F_TIME: mat inpX,b6,b7,b8,sc,o_o,wo_desc$ eof TM_XIT2
	loop while inpX(1)=client_id
	TM_XIT2: ! 
fnend 
def fn_bld_rec(client_id$)
	if inv_line=30 then let fn_print_inv ! pr invoice if more than 20 entries
	if inv_line>29 then pause 
	spk$=" "&client_id$&cnvrt$("n 2",b8)
	if inpX(7)=2 then goto BLD_REC_L1780 ! always bill modifications
	if inpX(7)=23 or inpX(7)=11 then goto BLD_XIT ! always no charge
	read #h_support,using F_SUPPORT,key=spk$: cln$,scode,scode$,sdt1,stm$,sup_exp_date,scst nokey BLD_REC_L1780
	trans_date=date(days(inpX(6),'mmddyy'),'ccyymmdd')
	!   if stm$="Mo" or (stm$='An' and trans_date<=sup_exp_date) then goto BLD_XIT ! TM_RD2 ! on maintenance
	if (stm$='An' and trans_date<=sup_exp_date) then goto BLD_XIT ! TM_RD2 ! on maintenance
	! if client_id=4625 then pause
	BLD_REC_L1780: ! 
	b(3)=inpX(5)
	b(8)=b8
	b3=b3+b(3)
	inv_line+=1
	if val(client_id$)=client_id_sage_ax or val(client_id$)=client_id_brc then 
		if val(client_id$)=client_id_sage_ax or val(client_id$)=client_id_brc then 
		!     pause  ! inv_item$(inv_line)=str$(inpX(3))&' hours at a rate of '&&' on '&cnvrt$("pic(##/##/##)",inpX(6))
			inv_item$(inv_line)=str$(inpX(3))&' hours at a rate of '&cnvrt$('pic($$#.##)',inpX(4))&' on '&cnvrt$("pic(##/##/##)",inpX(6))
		else if inpX(7)=2 then 
			inv_item$(inv_line)=str$(inpX(3))&' hours of '&trim$(sys_name$(b8))&" programming on "&cnvrt$("pic(##/##/##)",inpX(6))
		else 
			inv_item$(inv_line)=str$(inpX(3))&' hours of '&trim$(sys_name$(b8))&" support on "&cnvrt$("pic(##/##/##)",inpX(6))
		end if 
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
	sys_name$(31)=""
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
def fn_thsht_combine_entries(file_from$*256,file_to$*256,file_to_index$*256)
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
	open #h_tmsht:=6: "Name="&file_to$&",KFName="&file_to_index$,internal,outIn,keyed 
fnend  ! fn_thsht_combine_entries
def fn_summary_add
	open #22: "Name=skip_prn,RecL=80,replace",display,output ioerr SI_ADD
	pr #22: "Clnt   Name           Date      Prev Bal    New Amt     Total Due   Inv No  "
	pr #22: "_____ ______________  ________  __________  __________  __________  __________"
	SI_ADD: if (pbal+b3)<1 then piv$="" else piv$=iv$ ! if less than a dollar than don't charge it
	if piv$<>'' then 
		pr #22,using Fsa22: client_id$,client_addr$(1)(1:14),inv_date,pbal,b3,pbal+b3,piv$
		Fsa22: form pos 1,c 5,x 2,c 15,pic(zz/zz/zz),3*nz 12.2,x 2,c 12
		totalInvoicesPrinted+=b3
		totalPreviousBalances+=pbal
	end if  ! piv$<>''
fnend 
def fn_summary_print
	close #22: ioerr SP_XIT
	open #22: "Name=skip_prn",display,input 
	pr #255: "\ql"
	do 
		linput #22: skln$ eof SP_FINIS
		pr #255: skln$
	loop 
	SP_FINIS: ! 
	pr #255: 
	pr #255: "Total of Invoices Printed:  "&cnvrt$("N 12.2",totalInvoicesPrinted)
	pr #255: "Total of Previous Balances: "&cnvrt$("N 12.2",totalPreviousBalances)
	close #22,free: 
	SP_XIT: ! 
fnend 
def fn_print_inv ! pr INVOICE
	fn_timesheet2(h_tmsht)
	if b3>0 and sum(mat inv_amt)<250 then 
		inv_line+=1
		if inv_line<30 and client_id<>client_id_sage_ax and client_id<>client_id_brc and client_id<>4625 then 
			inv_item$(inv_line)='Minimum Monthly Billing of $250.00'
			inv_amt(inv_line)=250-sum(mat inv_amt) : b3+=inv_amt(inv_line)
			inv_service_code(inv_line)=19
			inv_gl$(inv_line)='  0  1160  0'
		end if  ! inv_line<30
	end if  ! pbal<250
	fn_summary_add ! pr all clients
	if b3=>1 then 
		write #h_tmwk1,using F_TMWK1: client_id$,2,inv_date,iv$,mat cde$,mat inv_item$,mat inv_amt,mat inv_category,mat inv_service_code,mat inv_gl$
	end if  ! b3=>1
	if b3<1 and pbal<1 then goto PI_SKIP_PRINT
	! if sum(mat inv_amt)+pbal>0 then
	fnopenprn(cp,42,220,process)
	!   if trim$(client_id$)='3811' then pause 
	fnprint_invoice(align,client_id$, mat client_addr$,iv$,inv_date,mat inv_item$,mat inv_amt,pbal)
	pr #255: newpage ! mat inv_item$=("")
	invoice_number=invoice_number+1 ! moved here 10/4/11 (from below) in an attempt to stop skipping invoice numbers
	! end if
	PI_SKIP_PRINT: ! 
	mat inv_item$=(" ")
	mat inv_category=(0)
	mat inv_service_code=(0)
	mat inv_gl$=("")
	! invoice_number=invoice_number+1
	mat inv_amt=(0)
	inv_line=b3=0
fnend  ! fn_print_inv
