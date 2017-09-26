00010 ! S:\Core\company_import
00020 ! r: setup library, dims, fntop, and read defaults
00030   library 'S:\Core\Library': fntop,fnxit, fnerror,fntos,fnlbl,fntxt,fncmdset,fnacs,fnreg_read,fnreg_write,fnmsgbox,fnputcno,fncursys$,fncheckfileversion,fngethandle,fnstatus,fnstatus_pause,fnchain,fncopy,fnchk,fnindex_sys
00031 ! pr
00032   library 'S:\Core\Library': fnpr_conversion_add_missing,fnpr_conversion_department
00033 ! ub
00035   library 'S:\Core\Library': fnd1
00036   library 'S:\Core\Library': fnub_cnv_build_transactions
00037   library 'S:\Core\Library': fnub_cnv_note
00038   library 'S:\Core\Library': fnub_cnv_ubmstr_vb
00039   library 'S:\Core\Library': fnub_index_customer
00040   library 'S:\Core\Library': fnfix_trans_breakdowns
00048 ! on error goto ERTN
00050 ! ______________________________________________________________________
00060   dim cap$*128
00062   dim message$(1)*256
00070 ! ______________________________________________________________________
00080   let fntop(program$)
00090   dim company_file$*256,company_import_path$*256
00100   dim resp$(5)*256
00110 ! defaults
00112   let cursys$=fncursys$ : let cursys_origional$=cursys$
00120   let fnreg_read(cap$&'.'&cursys$&'.'&env$('cno')&'.company_file',company_file$)
00130   let fnreg_read(cap$&'.'&cursys$&'.'&env$('cno')&'.destination_company_number',destination_company_number$) : let destination_company_number=val(destination_company_number$) conv ignore
00140 ! /r
00170 SCREEN1: ! r:
00180   let fntos(sn$="Company_Import_1")
00190   let col1_width=27 : let col2_pos=col1_width+2
00200   let fnlbl(1,1,"Source Company File:",col1_width,1)
00210   let fntxt(1,col2_pos,30,256,0,'70',0,'select any data file from the data set to be imported.  i.e. Z:\vol002\CLmstr\BankIdx.h2')
00220   let resp$(1)=company_file$
00230   let fnlbl(2,1,"Destination Company Number:",col1_width,1)
00240   let fntxt(2,col2_pos,5,5,0,'1030',0,'')
00250   let resp$(2)=str$(destination_company_number)
00260 ! let fnlbl(3,1,"System Abbreviation:",col1_width,1)
00270 ! let fntxt(3,col2_pos,2,2,0)
00280 ! let resp$(3)=env$('cursys') ! cursys$
00282 ! let fnchk(1,col2_pos+30+5,'Import Only (do not convert)',0)
00284 ! let resp$(4)='False'
00290   let fncmdset(2)
00300   let fnacs(sn$,0,mat resp$,ckey)
00310   if ckey=5 then goto XIT
00320   let company_file$=resp$(1)
00330   let destination_company_number=val(resp$(2)) : let destination_company_number$=str$(destination_company_number)
00340   let cursys$=env$('cursys') ! resp$(3)
00360   let import_only=0 ! if resp$(4)='True' then let import_only=1 else let import_only=0
00500   let fnreg_write(cap$&'.'&cursys$&'.'&env$('cno')&'.company_file',company_file$)
00520   let fnreg_write(cap$&'.'&cursys$&'.'&env$('cno')&'.destination_company_number',destination_company_number$)
00522 ! /r
00540   let cursys$=fncursys$(cursys$)
00542   if cursys_origional$<>cursys$ then 
00560     let fnstatus('Set current system to: '&cursys$&' from '&cursys_origional$)
00570   end if 
00580   let fnputcno(destination_company_number) : let cno=destination_company_number
00600   let fnstatus('Set active Company Number to: '&str$(destination_company_number))
00620 ! 
10000   let pos_point_h=pos(lwrc$(company_file$),'.h',-1)
10020   if ~exists(company_file$) then 
10040     mat message$(2)
10060     let message$(1)='the company_file$ ('&company_file$&') does not exist'
10080     let message$(2)="Please correct the selected path"
10100     let fnmsgbox(mat message$, response$, cap$)
10120     goto SCREEN1
10140   else if pos_point_h<=0 then 
10160     mat message$(2)
10180     let message$(1)="the source company_file$ ('&company_file$&') does not contain a .h"
10200     let message$(2)="It must be a zip or rar file type archive, please enhance code to handle such things."
10220     let fnmsgbox(mat message$, response$, cap$)
10240     goto SCREEN1
10260   else 
11000     if ~import_only then let fn_is_converison_detection
11020 ! pause
12000     let fn_copy_files_in
12020 ! exe 'dir '&env$('Q')&'\ubmstr\ubmaster.h700' : pause
13000     if ~import_only then let fn_rename_xxcoinfo_to_company
16000 ! r: conversion point A
16020     if cnv_ub then 
16040       if cnv_ub_french_settlement then 
16060         let fn_ubmaster2customer_frenchset(env$('Q')&'\UBmstr\UBMaster.h'&str$(destination_company_number),env$('Q')&'\UBmstr\Customer.h'&str$(destination_company_number))
16080       else if cnv_ub_merriam_woods then 
16090         let fn_ubmaster2customer_merriam(env$('Q')&'\UBmstr\Customer.h'&str$(destination_company_number),env$('Q')&'\UBmstr\Customer.h'&str$(destination_company_number))
16092       else if cnv_exeter then 
16094         let fncopy(env$('Q')&'\UBmstr\UBMaster.h'&str$(destination_company_number),env$('Q')&'\UBmstr\Customer.h'&str$(destination_company_number),2067)
16096         execute 'free '&env$('Q')&'\UBmstr\UBMaster.h'&str$(destination_company_number)
16100       else if ~exists(env$('Q')&'\UBmstr\Customer.h'&str$(destination_company_number)) and exists(env$('Q')&'\UBmstr\UBMaster.h'&str$(destination_company_number)) then 
16110         let fncopy(env$('Q')&'\UBmstr\UBMaster.h'&str$(destination_company_number),env$('Q')&'\UBmstr\Customer.h'&str$(destination_company_number))
16120         execute 'free '&env$('Q')&'\UBmstr\UBMaster.h'&str$(destination_company_number)
16140       end if 
16160       let fnub_cnv_build_transactions
16170       let fnub_cnv_ubmstr_vb
16172       let fnub_cnv_note
16180     end if 
16900 ! /r
17000     let setenv('force_reindex','yes') ! for fncheckfileversion
17020     if ~import_only then 
18000       if ~cnv_pr then let fncheckfileversion
18020       if cnv_ub_french_settlement then 
18040         let fnfix_trans_breakdowns(1,0)
18060         let fntranslate_rate_abreviations('GS','GA')
18080 !       let fn_ub_combine_services(5,4)
18100       else if cnv_ub_merriam_woods then 
18120         let fn_ub_cust_route_from_acctno(destination_company_number)
18140       end if 
18160 !     if env$('client')="West Accounting" then pause ! dir '&env$('Q')&'\PRmstr\dednames.h4560
19000       if cnv_pr then 
19020         if ~exists(env$('Q')&"\PRmstr\dednames.h"&str$(cno)) then 
19040           if env$('client')="Merriam Woods" then let pr_cnv_medicare_is_seperated=0
19060           let fnpr_conversion_department(cno, pr_cnv_medicare_is_seperated)
19080           let fnpr_conversion_add_missing(cno)
19100         end if 
19110         let fncheckfileversion
19120       end if 
19140     end if 
19160   end if 
19180   let fnindex_sys(cno,cursys$)
19980 XIT: ! 
19990   let fnxit
20000   def fn_copy_files_in
20020     let company_import_extension$=company_file$(pos_point_h:len(company_file$))
20040     let company_import_path$=company_file$(1:pos(company_file$,'\',-1))
