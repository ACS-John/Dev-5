! S:\Core\company_import
! r: setup library, dims, fnTop, and read defaults
autoLibrary
! pr
! ub
! on error goto Ertn
!
dim cap$*128
dim message$(1)*256
!
fnTop(program$)
dim company_file$*256,company_import_path$*256
dim resp$(5)*256
! defaults
cursys$=fncursys$ : cursys_origional$=cursys$
fncreg_read(cap$&'.company_file',company_file$)
fncreg_read(cap$&'.destination_company_number',destination_company_number$) : destination_company_number=val(destination_company_number$) conv ignore
! /r
SCREEN1: ! r:
fnTos
col1_width=27 : col2_pos=col1_width+2
fnLbl(1,1,"Source Company File:",col1_width,1)
fnTxt(1,col2_pos,30,256,0,'70',0,'select any data file from the data set to be imported.  i.e. Z:\vol002\CLmstr\BankIdx.h2')
resp$(1)=company_file$
fnLbl(2,1,"Destination Company Number:",col1_width,1)
fnTxt(2,col2_pos,5,5,0,'1030',0,'')
resp$(2)=str$(destination_company_number)
fnCmdSet(2)
fnAcs(mat resp$,ckey)
if ckey=5 then goto Xit
company_file$=resp$(1)
destination_company_number=val(resp$(2)) : destination_company_number$=str$(destination_company_number)
cursys$=env$('cursys') ! resp$(3)
import_only=0 ! if resp$(4)='True' then import_only=1 else import_only=0
fncreg_write(cap$&'.company_file',company_file$)
fncreg_write(cap$&'.destination_company_number',destination_company_number$)
! /r
cursys$=fncursys$(cursys$)
if cursys_origional$<>cursys$ then
	fnStatus('Set current system to: [CurSys] from '&cursys_origional$)
end if
fnputcno(destination_company_number) : cno=destination_company_number
fnStatus('Set active Company Number to: [cno]') ! ')
!
pos_point_h=pos(lwrc$(company_file$),'.h',-1)
if ~exists(env$('at')&company_file$) then
	mat message$(2)
	message$(1)='the company_file$ ('&company_file$&') does not exist'
	message$(2)="Please correct the selected path"
	fnmsgbox(mat message$, response$, cap$)
	goto SCREEN1
else if pos_point_h<=0 then
	mat message$(2)
	message$(1)="The source company_file ("&company_file$&") does not contain a .h"
	message$(2)="It must be a zip or rar file type archive, please enhance code to handle such things."
	fnmsgbox(mat message$, response$, cap$)
	goto SCREEN1
else
	if ~import_only then fn_is_converison_detection
	! pause
	fn_copy_files_in
	! exe 'dir [Q]\ubmstr\ubmaster.h700' : pause
	if ~import_only then fn_rename_xxcoinfo_to_company
	! r: conversion point A
	if cnv_ub then
		if cnv_ub_french_settlement then
			fn_ubmaster2customer_frenchset('[Q]\UBmstr\UBMaster.h[cno]','[Q]\UBmstr\Customer.h[cno]')
		else if cnv_ub_merriam_woods then
			fn_ubmaster2customer_merriam('[Q]\UBmstr\Customer.h[cno]','[Q]\UBmstr\Customer.h[cno]')
		else if cnv_exeter then
			fnCopy('[Q]\UBmstr\UBMaster.h[cno]','[Q]\UBmstr\Customer.h[cno]',2067)
			fnFree('[Q]\UBmstr\UBMaster.h[cno]')
		else if ~exists('[Q]\UBmstr\Customer.h[cno]') and exists('[Q]\UBmstr\UBMaster.h[cno]') then
			fnCopy('[Q]\UBmstr\UBMaster.h[cno]','[Q]\UBmstr\Customer.h[cno]')
			fnFree('[Q]\UBmstr\UBMaster.h[cno]')
		end if
		fnub_cnv_build_transactions
		fnub_cnv_ubmstr_vb
		if ~exists(company_import_path$&'UBmstr\notes'&company_import_extension$) then
			fnub_cnv_note
		end if
	end if
	! /r
	setenv('force_reindex','yes') ! for fnCheckFileVersion
	if ~import_only then
		if ~cnv_pr then fnCheckFileVersion
		if cnv_ub_french_settlement then
			fnfix_trans_breakdowns(1,0)
			fn_translateRateAbbreviations('GS','GA')
		!       fn_ub_combine_services(5,4)
		else if cnv_ub_merriam_woods then
			fn_ub_cust_route_from_acctno(destination_company_number)
		end if
		if cnv_pr then
			if ~exists("[Q]\PRmstr\dednames.h"&str$(cno)) then
				if env$('client')="Merriam Woods" then pr_cnv_medicare_is_seperated=0
				fnpr_conversion_department(cno, pr_cnv_medicare_is_seperated)
				fnpr_conversion_add_missing(cno)
			end if
			fnCheckFileVersion
		end if
	end if
end if
fnindex_sys(cno,cursys$)
Xit: !
fnXit
def fn_deleteExistingFiles
	company_import_extension$=company_file$(pos_point_h:len(company_file$))
	company_import_path$=company_file$(1:pos(company_file$,'\',-1))
	fnFree('[Q]\[CurSys]mstr\*.h[cno]')
	if cursys$='UB' then
		fnFree('[Q]\[CurSys]mstr\Notes.h[cno]'&'\*.*')
		! fnFree('[Q]\[CurSys]mstr\ubData\*.h[cno]')  <-- this is done in fn_ub_copy_extras
	end if
	fnStatus('Existing files ('&os_filename$('[Q]\'&cursys$&'mstr\*.h[cno]')&') have been removed.')
fnend
def fn_copy_files_in
	fn_deleteExistingFiles
	! company_import_extension$=company_file$(pos_point_h:len(company_file$))
	! company_import_path$=company_file$(1:pos(company_file$,'\',-1))
	! fnFree('[Q]\'&cursys$&'mstr\*.h[cno]')
	! fnStatus('Existing files ('&os_filename$('[Q]\'&cursys$&'mstr\*.h[cno]')&') have been removed.')
	fnCopy(env$('at')&company_import_path$&'*'&company_import_extension$,'[Q]\'&cursys$&'mstr\*.h[cno]')
	if cursys$='UB' then
		fn_ub_copy_extras
	end if
	fnStatus('Import data copied in.')
	!
fnend
def fn_is_converison_detection
	! r: set cnv_[cursys]
	cnv_ub=cnv_pr=0
	if cursys$='UB' and ~exists(company_import_path$&'Customer'&company_import_extension$) then
		cnv_ub=1
	else if cursys$='PR' and ~exists(company_import_path$&'DEDNames'&company_import_extension$) then
		cnv_pr=1
	end if
	! /r
	! r: set is_[client]
	is_french_settlement=is_merriam_woods=is_payrollDoneRight=is_exeter=0
	if ((env$('ACSDeveloper')<>'' and destination_company_number=1880) or (env$('client')="French Settlement" and serial=33380)) then
		is_french_settlement=1
	else if ((env$('ACSDeveloper')<>'' and destination_company_number=1615) or (env$('client')="Exeter" and serial=31210)) then
		is_exeter=1
	else if ((env$('ACSDeveloper')<>'' and destination_company_number=2900) or (env$('client')="Merriam Woods" and serial=31702)) then
		is_merriam_woods=1
	else if ((env$('ACSDeveloper')<>'' and destination_company_number=4560) or (env$('client')="Payroll Done Right" and serial=30176)) then  ! env$('client')="West Accounting" or
		is_payrollDoneRight=1
	else if ((env$('ACSDeveloper')<>'' and destination_company_number=700) or (env$('client')="Campbell")) then
		is_campbell=1
	end if
	! /r
	! r: set cnv_[cursys]_[client]
	cnv_ub_french_settlement=cnv_ub_capbell=cnv_ub_merriam_woods=cnv_exeter=0
	if cnv_ub then
		if is_french_settlement then
			cnv_ub_french_settlement=1
		else if is_exeter then
			cnv_exeter=1
		else if is_merriam_woods then
			cnv_ub_merriam_woods=1
		else if is_campbell then
			cnv_ub_capbell=1
		end if
		fnStatus('Processing as '&env$('client')&' '&cursys$&' Conversion')
	else if cnv_pr then
		!     if is_merriam_woods or is_payrollDoneRight then
		!       cnv_pr=1
		!     end if
		fnStatus('Processing as '&env$('client')&' '&cursys$&' Conversion')
	end if
	! /r
fnend
def fn_rename_xxcoinfo_to_company
	if ~exists('[Q]\ubmstr\company.h[cno]') then
		if cursys$='UB' then
			open #h_tmp:=fngethandle: 'Name=[Q]\ubmstr\ubcoinfo.h[cno]',internal,input
			read #h_tmp,using 'form pos 121,N 6': d1
			close #h_tmp:
		! else if cursys$='PR' then
		!
		!   open #h_tmp:=fngethandle: "Name=[Q]\PRmstr\PRCOINFO.h[cno]",internal,input IOERR PRCoInfoOldVersionFinis
		!   dim prcoinfo_a$(3)*40
		!   dim prcoinfo_fid$*12
		!   dim prcoinfo_mcr
		!   dim prcoinfo_mcm
		!   dim prcoinfo_feducrat
		!   dim prcoinfo_d$(10)*8
		!   dim prcoinfo_loccode
		!   dim prcoinfo_feducmax
		!   dim prcoinfo_ficarate
		!   dim prcoinfo_ficamaxw
		!   dim prcoinfo_ficawh
		!   dim prcoinfo_m(10)
		!   dim prcoinfo_r(10)
		!   dim prcoinfo_e$(10)*12
		!   dim prcoinfo_gln$(15)*12
		!   dim prcoinfo_gli
		!   dim prcoinfo_dedcode(10)
		!   dim prcoinfo_calcode(10)
		!   dim prcoinfo_dedfed(10)
		!   dim prcoinfo_rpnames2$(10)*6
		!   dim prcoinfo_sck(4)
		!   dim prcoinfo_vacm
		!   dim prcoinfo_mhw
		!   dim prcoinfo_wcm(4)
		!   dim prcoinfo_tc
		!   dim prcoinfo_jn$(2)*6
		!   dim prcoinfo_dc
		!   read #h_tmp,using fPrCoInfoOldVersion: mat prcoinfo_a$,prcoinfo_fid$,prcoinfo_mcr,prcoinfo_mcm,prcoinfo_feducrat,mat prcoinfo_d$,prcoinfo_loccode,prcoinfo_feducmax,prcoinfo_ficarate,prcoinfo_ficamaxw,prcoinfo_ficawh,mat prcoinfo_m,mat prcoinfo_r,mat prcoinfo_e$,mat prcoinfo_gln$,prcoinfo_gli,mat prcoinfo_dedcode,mat prcoinfo_calcode,mat prcoinfo_dedfed,mat prcoinfo_rpnames2$,mat prcoinfo_sck,vacm,prcoinfo_mhw,mat prcoinfo_wcm,prcoinfo_tc,mat prcoinfo_jn$,prcoinfo_dc ioerr PRCoInfoOldVersionFinis
		!   fprcoInfoOldVersion: form pos 1,3*C 40,C 12,PD 6.3,PD 6.2,PD 5.2,10*C 8,N 2,PD 4.2,PD 3.3,12*PD 4.2,10*PD 3.3,25*C 12,31*N 1,10*C 6,3*PD 4.3,3*PD 3.2,4*PD 4.2,N 1,2*C 6,N 2
		!   close #h_tmp:
		!   pause
		end if
		! PRCoInfoOldVersionFinis: !
		if ~exists('[Q]\'&cursys$&'mstr\Company.h[cno]') then
			if exists('[Q]\'&cursys$&'mstr\'&cursys$&'coinfo.h[cno]') then
				fnCopy('[Q]\'&cursys$&'mstr\'&cursys$&'coinfo.h[cno]','[Q]\'&cursys$&'mstr\Company.h[cno]')
				fnFree('[Q]\'&cursys$&'mstr\'&cursys$&'coinfo.h[cno]')
			end if
			if exists('[Q]\'&cursys$&'mstr\prcoinfo.h[cno]') then
				fnCopy('[Q]\'&cursys$&'mstr\prcoinfo.h[cno]','[Q]\'&cursys$&'mstr\Company.h[cno]')
				fnFree('[Q]\'&cursys$&'mstr\prcoinfo.h[cno]')
			end if
			if exists('[Q]\'&cursys$&'mstr\coinfo.h[cno]') then
				fnCopy('[Q]\'&cursys$&'mstr\coinfo.h[cno]','[Q]\'&cursys$&'mstr\Company.h[cno]')
				fnFree('[Q]\'&cursys$&'mstr\coinfo.h[cno]')
			end if
		end if
		if cursys$='UB' then
			fnLastBillingDate(d1,1)
		end if
	end if
fnend
def fn_ub_copy_extras
	! r: import rates
	if exists(company_import_path$&'ubdata') then
		if exists('[Q]\UBmstr\ubdata\*.h[cno]') then
			fnFree('[Q]\UBmstr\ubdata\*.h[cno]')
		end if  ! exists('[Q]\UBmstr\ubdata\*.h[cno]')
		fnCopy(env$('at')&company_import_path$&'ubdata\*'&company_import_extension$,'[Q]\UBmstr\ubdata\*.h[cno]')
		fnStatus('UBmstr\ubData found in source and replaced destination.')
	else if exists(company_import_path$&'..\ubmstr\service'&company_import_extension$) then
	else if exists(company_import_path$&'..\acsub\ratemst'&company_import_extension$) then
		fnStatus('RateMst found in source acsUB folder.  Copying to UBmstr\ubData from there.')
		fnCopy(env$('at')&company_import_path$&'..\acsub\ratemst'&company_import_extension$,'[Q]\UBmstr\ubdata\RateMst.h[cno]')
		fnCopy(env$('at')&company_import_path$&'..\acsub\RateIdx1'&company_import_extension$,'[Q]\UBmstr\ubdata\RateIdx1.h[cno]')
		fnCopy(env$('at')&company_import_path$&'..\acsub\RateIdx2'&company_import_extension$,'[Q]\UBmstr\ubdata\RateIdx2.h[cno]')
	else
		fnStatus('UBmstr\ubData did not exist in source. Destination ubData remains unchanged.')
	!   it is a UB system but no ubdata directory exists - so get the rate files from ACSUB
	!   copy in rates from acsUB
		dim import_install_path$*256
		import_install_path$=company_import_path$(1:len(company_import_path$))
		import_install_path$=rtrm$(import_install_path$,'\')
		import_install_path$=import_install_path$(1:pos(import_install_path$,'\',-1))
	!  pause
		if exists(env$('at')&import_install_path$&'S:\acsUB\ratemst'&company_import_extension$) then
			fnCopy(env$('at')&import_install_path$&'S:\acsUB\ratemst'&company_import_extension$,'[Q]\'&cursys$&'mstr\ubdata\*.h[cno]')
		end if
	end if
	! /r
	! r: import notes folder
	if exists(company_import_path$&'UBmstr\notes'&company_import_extension$) then
		fnFree('[Q]\'&cursys$&'mstr\notes.h[cno]')
		! this would not get subdirs ! fnCopy(env$('at')&company_import_path$&'UBmstr\notes'&company_import_extension$&'\*.*','[Q]\UBmstr\notes.h[cno]'&'\*.*')
		execute 'sy xcopy "'&company_import_path$&'UBmstr\notes'&company_import_extension$&'\*.*" "'&os_filename$('[Q]\UBmstr\notes.h[cno]')&'\*.*" /t /y'
		fnStatus('UB Notes imported.')
	end if  ! exists [import path][Q]\UBmstr\notes.h[company_import_extension]
	! /r
	! r: rename ubmaster to customer
	if ~cnv_ub and ~exists('[Q]\UBmstr\Customer.h[cno]') and exists('[Q]\UBmstr\UBMaster.h[cno]') then ! and ~cnv_ub_capbell and ~cnv_ub_french_settlement then
		fnCopy('[Q]\UBmstr\UBMaster.h[cno]','[Q]\UBmstr\Customer.h[cno]')
		fnFree('[Q]\'&cursys$&'mstr\UBMaster.h[cno]')
	end if  ! exists [import path][Q]\UBmstr\notes.h[company_import_extension]
	! /r
fnend
def fn_translateRateAbbreviations(from$*2,to$*2)
	fnStatus('Translating Rate abbreviations from '&from$&' to '&to$)
	from$=uprc$(from$)
	to$=uprc$(to$)
	open #h1:=fngethandle: "Name=[Q]\UBmstr\ubData\RateMst.h[cno],KFName=[Q]\UBmstr\ubData\RateIdx1.h[cno],Use,RecL=374,KPs=1,KLn=4,Shr",internal,outIn,keyed
	open #h2:=fngethandle: "Name=[Q]\UBmstr\ubData\RateMst.h[cno],KFName=[Q]\UBmstr\ubData\RateIdx2.h[cno],Use,RecL=374,KPs=5,KLn=25,Shr",internal,outIn,keyed
	do
		read #h1,using 'Form POS 1,C 2': rate_type$ eof EO_RATE
		if rate_type$=from$ then
			rate_type$=to$
			rewrite #h1,using 'Form POS 1,C 2': rate_type$
		end if
	loop
	EO_RATE: !
	close #h1:
	close #h2:
fnend
def fn_ubmaster2customer_frenchset(file_source$*256,file_destination$*256)
	fnStatus('UBMaster Conversion for French Settlement processing...')
	open #h_old:=fngethandle: "Name="&file_source$,internal,input
	F_UBMASTER_OLD: form pos 1,c 10,4*c 30,c 12,7*pd 2,11*pd 4.2,4*pd 4,15*pd 5,pd 4.2,pd 4,12*pd 4.2,2*pd 3,c 7,2*c 12,pd 3,10*pd 5.2,78*pd 5,13*pd 4.2,13*n 6,156*pd 4.2,13*n 6,13*pd 4.2,c 1,c 9,c 2,c 17
	open #h_new:=fngethandle: "Name="&file_destination$&',RecL=2067,Replace',internal,outIn,relative
	dim rw4(22,13),gb(10),adr(2),f$(3)*12,g(12),a(7),b(11),c(4),d(15),e$(4)*30,extra(23),extra$(11)*30
	F_CUSTOMER_NEW: form pos 1,c 10,4*c 30,c 12,7*pd 2,11*pd 4.2,4*pd 4,15*pd 5,pd 4.2,pd 4,12*pd 4.2,2*pd 3,c 7,2*c 12,pd 3,10*pd 5.2,78*pd 5,13*pd 4.2,13*n 6,156*pd 4.2,13*n 6,13*pd 4.2,c 1,c 9,c 2,c 17,n 2,n 7,2*n 6,n 9,pd 5.2,n 3,3*n 9,3*n 2,3*n 3,n 1,3*n 9,3*pd 5.2,c 30,7*c 12,3*c 30
	do
		read #h_old,using F_UBMASTER_OLD: z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,f,mat g,mat adr,alp$,f$(2),f$(3),bra,mat gb,mat rw4,df$,dr$,dc$,da$ eof EO_UBMASTER
		!     if trim$(z$)(1:6)='100875' then pause
		extra(17)=c(3) ! in new system   extra(17) = final billing code
		extra(3)=d(5) ! meter reading date - current
		extra(4)=d(6) ! meter reading date - prior
		c(3)=0 ! in new system   c(3) service 3 deposit date
		!
		extra(1)=val(z$(1:2))
		extra(2)=val(f$(2))*10 ! in new system    extra(2) is the sequence number
		f$(2)='' ! in new system    f$(2) = service 3 meter number
		extra$(2)=f$(1) !   extra$(2)  is the phone number
		f$(1)='' ! f$(1)  is the service 1 meter number
		!
		write #h_new,using F_CUSTOMER_NEW: z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,f,mat g,mat adr,alp$,f$(2),f$(3),bra,mat gb,mat rw4,df$,dr$,dc$,da$,mat extra,mat extra$
	loop
	EO_UBMASTER: !
	close #h_old:
	close #h_new:
	fnub_index_customer
	fnStatus('UBMaster Conversion for French Settlement complete.')
fnend
def fn_ubmaster2customer_merriam(file_source$*256,file_destination$*256)
	fnStatus('UBMaster Conversion for Merriam Woods complete.')
fnend
def fn_ub_cust_route_from_acctno(cno)
	open #h_customer:=fngethandle: "Name=[Q]\UBmstr\Customer.h"&str$(cno)&",Shr",internal,outIn,relative
	do
		read #h_customer,using 'Form Pos 1,C 10,pos 1741,n 2,n 7': z$,route_number,sequence_number eof UCRFA_CUSTOMER_EOF
		if route_number=0 then
			route_number=sequence_number=0
			route_number=val(z$(1:2)) conv UCRFA_SKIP_IT
			sequence_number=val(z$(3:7)) conv ignore
			rewrite #h_customer,using 'Form Pos 1,C 10,pos 1741,n 2,n 7': z$,route_number,sequence_number
		end if
		UCRFA_SKIP_IT: !
	loop
	UCRFA_CUSTOMER_EOF: !
	close #h_customer:
fnend
def fn_ub_combine_services(service_from,service_to)
	! r: customer
	dim g(12), gb(10)
	open #h_customer:=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno],Shr",internal,outIn,relative
	do
		read #h_customer,using 'Form Pos 1,C 10,pos 300,12*pd 4.2,pos 388,10*pd 5.2': z$,mat g,mat gb eof UCCS_CUSTOMER_EOF
		if g(service_from)<>0 or gb(service_from)<>0 then
			g(service_to)+=g(service_from)
			gb(service_to)+=gb(service_from)
			g(service_from)=0
			gb(service_from)=0
			rewrite #h_customer,using 'Form Pos 1,C 10,pos 300,12*pd 4.2,pos 388,10*pd 5.2': z$,mat g,mat gb
		end if
	loop
	UCCS_CUSTOMER_EOF: !
	close #h_customer:
	! /r
	! r: transactions
	dim tg(11)
	open #h_customer:=fngethandle: "Name=[Q]\UBmstr\UBTransVB.h[cno],Shr",internal,outIn,relative
	do
		read #h_ubtrans,using 'form pos 1,c 10,x 8,x 1,x 4,12*pd 4.2': x$,mat tg eof UCCS_TRANS_EOF
		if tg(service_from)<>0 then
			tg(service_to)+=tg(service_from)
			tg(service_from)=0
			rewrite #h_ubtrans,using 'Form Pos 1,C 10,pos 300,12*pd 4.2,pos 388,10*pd 5.2': x$,mat tg
		end if
	loop
	UCCS_TRANS_EOF: !
	close #h_ubtrans:
	! /r
fnend
