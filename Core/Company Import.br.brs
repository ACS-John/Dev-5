00010 ! S:\Core\company_import
00020 ! r: setup library, dims, fntop, and read defaults
00022 library 'S:\Core\Library': fntop,fnxit, fnerror,fnTos,fnLbl,fnTxt,fnCmdSet,fnAcs,fnreg_read,fnreg_write,fnmsgbox
00024 library 'S:\Core\Library': fnputcno,fncursys$,fncheckfileversion,fngethandle,fnStatus,fnStatusPause,fnchain,fnChk,fnindex_sys
00026 library 'S:\Core\Library': fnFree,fnCopy
00031 ! pr
00032 library 'S:\Core\Library': fnpr_conversion_add_missing,fnpr_conversion_department
00033 ! ub
00035 library 'S:\Core\Library': fnLastBillingDate
00036 library 'S:\Core\Library': fnub_cnv_build_transactions
00037 library 'S:\Core\Library': fnub_cnv_note
00038 library 'S:\Core\Library': fnub_cnv_ubmstr_vb
00039 library 'S:\Core\Library': fnub_index_customer
00040 library 'S:\Core\Library': fnfix_trans_breakdowns
00048 ! on error goto ERTN
00050 ! ______________________________________________________________________
00060 dim cap$*128
00062 dim message$(1)*256
00070 ! ______________________________________________________________________
00080 fntop(program$)
00090 dim company_file$*256,company_import_path$*256
00100 dim resp$(5)*256
00110 ! defaults
00112 cursys$=fncursys$ : cursys_origional$=cursys$
00120 fnreg_read(cap$&'.'&cursys$&'.'&env$('cno')&'.company_file',company_file$)
00130 fnreg_read(cap$&'.'&cursys$&'.'&env$('cno')&'.destination_company_number',destination_company_number$) : destination_company_number=val(destination_company_number$) conv ignore
00140 ! /r
00170 SCREEN1: ! r:
00180 fnTos(sn$="Company_Import_1")
00190 col1_width=27 : col2_pos=col1_width+2
00200 fnLbl(1,1,"Source Company File:",col1_width,1)
00210 fnTxt(1,col2_pos,30,256,0,'70',0,'select any data file from the data set to be imported.  i.e. Z:\vol002\CLmstr\BankIdx.h2')
00220 resp$(1)=company_file$
00230 fnLbl(2,1,"Destination Company Number:",col1_width,1)
00240 fnTxt(2,col2_pos,5,5,0,'1030',0,'')
00250 resp$(2)=str$(destination_company_number)
00290 fnCmdSet(2)
00300 fnAcs(sn$,0,mat resp$,ckey)
00310 if ckey=5 then goto XIT
00320 company_file$=resp$(1)
00330 destination_company_number=val(resp$(2)) : destination_company_number$=str$(destination_company_number)
00340 cursys$=env$('cursys') ! resp$(3)
00360 import_only=0 ! if resp$(4)='True' then import_only=1 else import_only=0
00500 fnreg_write(cap$&'.'&cursys$&'.'&env$('cno')&'.company_file',company_file$)
00520 fnreg_write(cap$&'.'&cursys$&'.'&env$('cno')&'.destination_company_number',destination_company_number$)
00522 ! /r
00540 cursys$=fncursys$(cursys$)
00542 if cursys_origional$<>cursys$ then 
00560   fnStatus('Set current system to: '&cursys$&' from '&cursys_origional$)
00570 end if 
00580 fnputcno(destination_company_number) : cno=destination_company_number
00600 fnStatus('Set active Company Number to: '&str$(destination_company_number))
00620 ! 
10000 pos_point_h=pos(lwrc$(company_file$),'.h',-1)
10020 if ~exists(env$('at')&company_file$) then 
10040   mat message$(2)
10060   message$(1)='the company_file$ ('&company_file$&') does not exist'
10080   message$(2)="Please correct the selected path"
10100   fnmsgbox(mat message$, response$, cap$)
10120   goto SCREEN1
10140 else if pos_point_h<=0 then 
10160   mat message$(2)
10180   message$(1)="the source company_file$ ("&company_file$&") does not contain a .h"
10200   message$(2)="It must be a zip or rar file type archive, please enhance code to handle such things."
10220   fnmsgbox(mat message$, response$, cap$)
10240   goto SCREEN1
10260 else 
11000   if ~import_only then let fn_is_converison_detection
11020   ! pause
12000   fn_copy_files_in
12020   ! exe 'dir '&env$('Q')&'\ubmstr\ubmaster.h700' : pause
13000   if ~import_only then let fn_rename_xxcoinfo_to_company
16000   ! r: conversion point A
16020   if cnv_ub then 
16040     if cnv_ub_french_settlement then 
16060       fn_ubmaster2customer_frenchset(env$('Q')&'\UBmstr\UBMaster.h'&str$(destination_company_number),env$('Q')&'\UBmstr\Customer.h'&str$(destination_company_number))
16080     else if cnv_ub_merriam_woods then 
16090       fn_ubmaster2customer_merriam(env$('Q')&'\UBmstr\Customer.h'&str$(destination_company_number),env$('Q')&'\UBmstr\Customer.h'&str$(destination_company_number))
16092     else if cnv_exeter then 
16094       fnCopy(env$('Q')&'\UBmstr\UBMaster.h'&str$(destination_company_number),env$('Q')&'\UBmstr\Customer.h'&str$(destination_company_number),2067)
16096       fnFree(env$('Q')&'\UBmstr\UBMaster.h'&str$(destination_company_number))
16100     else if ~exists(env$('Q')&'\UBmstr\Customer.h'&str$(destination_company_number)) and exists(env$('Q')&'\UBmstr\UBMaster.h'&str$(destination_company_number)) then 
16110       fnCopy(env$('Q')&'\UBmstr\UBMaster.h'&str$(destination_company_number),env$('Q')&'\UBmstr\Customer.h'&str$(destination_company_number))
16120       fnFree(env$('Q')&'\UBmstr\UBMaster.h'&str$(destination_company_number))
16140     end if 
16160     fnub_cnv_build_transactions
16170     fnub_cnv_ubmstr_vb
16172     fnub_cnv_note
16180   end if 
16900   ! /r
17000   setenv('force_reindex','yes') ! for fncheckfileversion
17020   if ~import_only then 
18000     if ~cnv_pr then let fncheckfileversion
18020     if cnv_ub_french_settlement then 
18040       fnfix_trans_breakdowns(1,0)
18060       fntranslate_rate_abreviations('GS','GA')
18080     !       fn_ub_combine_services(5,4)
18100     else if cnv_ub_merriam_woods then 
18120       fn_ub_cust_route_from_acctno(destination_company_number)
18140     end if 
19000     if cnv_pr then 
19020       if ~exists(env$('Q')&"\PRmstr\dednames.h"&str$(cno)) then 
19040         if env$('client')="Merriam Woods" then pr_cnv_medicare_is_seperated=0
19060         fnpr_conversion_department(cno, pr_cnv_medicare_is_seperated)
19080         fnpr_conversion_add_missing(cno)
19100       end if 
19110       fncheckfileversion
19120     end if 
19140   end if 
19160 end if 
19180 fnindex_sys(cno,cursys$)
19980 XIT: ! 
19990 fnxit
20000 def fn_copy_files_in
20020   company_import_extension$=company_file$(pos_point_h:len(company_file$))
20040   company_import_path$=company_file$(1:pos(company_file$,'\',-1))
20060   fnFree(env$('Q')&'\'&cursys$&'mstr\*.h'&str$(destination_company_number))
20062   fnStatus('Existing files ('&os_filename$(env$('Q')&'\'&cursys$&'mstr\*.h'&str$(destination_company_number))&') have been removed.')
20080   fnCopy(env$('at')&company_import_path$&'*'&company_import_extension$,env$('Q')&'\'&cursys$&'mstr\*.h'&str$(destination_company_number))
20100   if cursys$='UB' then let fn_ub_copy_extras
20120   fnStatus('Import data copied in.')
20140   ! 
20990 fnend 
21000 def fn_is_converison_detection
21020   ! r: set cnv_[cursys]
21040   cnv_ub=cnv_pr=0
21060   if cursys$='UB' and ~exists(company_import_path$&'Customer'&company_import_extension$) then 
21080     cnv_ub=1
21100   else if cursys$='PR' and ~exists(company_import_path$&'DEDNames'&company_import_extension$) then 
21120     cnv_pr=1
21140   end if 
21160   ! /r
21180   ! r: set is_[client]
21200   is_french_settlement=is_merriam_woods=is_west_accounting=is_exeter=0
21220   if ((env$('ACSDeveloper')<>'' and destination_company_number=1880) or (env$('client')="French Settlement" and serial=33380)) then 
21240     is_french_settlement=1
21242   else if ((env$('ACSDeveloper')<>'' and destination_company_number=1615) or (env$('client')="Exeter" and serial=31210)) then 
21244     is_exeter=1
21260   else if ((env$('ACSDeveloper')<>'' and destination_company_number=2900) or (env$('client')="Merriam Woods" and serial=31702)) then 
21280     is_merriam_woods=1
21282   else if ((env$('ACSDeveloper')<>'' and destination_company_number=4560) or ((env$('client')="West Accounting" or env$('client')="Payroll Done Right") and serial=30176)) then 
21284     is_west_accounting=1
21286   else if ((env$('ACSDeveloper')<>'' and destination_company_number=700) or (env$('client')="Campbell")) then 
21288     is_campbell=1
21300   end if 
21320   ! /r
21340   ! r: set cnv_[cursys]_[client]
21360   cnv_ub_french_settlement=cnv_ub_capbell=cnv_ub_merriam_woods=cnv_exeter=0
21380   if cnv_ub then 
21400     if is_french_settlement then 
21420       cnv_ub_french_settlement=1
21422     else if is_exeter then 
21424       cnv_exeter=1
21440     else if is_merriam_woods then 
21460       cnv_ub_merriam_woods=1
21462     else if is_campbell then 
21464       cnv_ub_capbell=1
21480     end if 
21500     fnStatus('Processing as '&env$('client')&' '&cursys$&' Conversion')
21520   else if cnv_pr then 
21540     !     if is_merriam_woods or is_west_accounting then
21560     !       cnv_pr=1
21580     !     end if
21600     fnStatus('Processing as '&env$('client')&' '&cursys$&' Conversion')
21620   end if 
21640   ! /r
22990 fnend 
25000 def fn_rename_xxcoinfo_to_company
25010   if cursys$='UB' then 
25020     open #h_tmp:=fngethandle: 'Name='&env$('Q')&'\ubmstr\ubcoinfo.h'&str$(destination_company_number),internal,input 
25030     read #h_tmp,using 'form pos 121,N 6': d1
25040     close #h_tmp: 
25050   ! else if cursys$='PR' then 
25060   ! 
25070   !   open #h_tmp:=fngethandle: "Name="&env$('Q')&"\PRmstr\PRCOINFO.h"&str$(destination_company_number),internal,input IOERR PRCoInfoOldVersionFinis
25080   !   dim prcoinfo_a$(3)*40
25090   !   dim prcoinfo_fid$*12
25100   !   dim prcoinfo_mcr
25110   !   dim prcoinfo_mcm
25120   !   dim prcoinfo_feducrat
25130   !   dim prcoinfo_d$(10)*8
25140   !   dim prcoinfo_loccode
25150   !   dim prcoinfo_feducmax
25160   !   dim prcoinfo_ficarate
25170   !   dim prcoinfo_ficamaxw
25180   !   dim prcoinfo_ficawh
25190   !   dim prcoinfo_m(10)
25200   !   dim prcoinfo_r(10)
25210   !   dim prcoinfo_e$(10)*12
25220   !   dim prcoinfo_gln$(15)*12
25230   !   dim prcoinfo_gli
25240   !   dim prcoinfo_dedcode(10)
25250   !   dim prcoinfo_calcode(10)
25260   !   dim prcoinfo_dedfed(10)
25270   !   dim prcoinfo_rpnames2$(10)*6
25280   !   dim prcoinfo_sck(4)
25290   !   dim prcoinfo_vacm
25300   !   dim prcoinfo_mhw
25310   !   dim prcoinfo_wcm(4)
25320   !   dim prcoinfo_tc
25330   !   dim prcoinfo_jn$(2)*6
25340   !   dim prcoinfo_dc
25350   !   read #h_tmp,using fPrCoInfoOldVersion: mat prcoinfo_a$,prcoinfo_fid$,prcoinfo_mcr,prcoinfo_mcm,prcoinfo_feducrat,mat prcoinfo_d$,prcoinfo_loccode,prcoinfo_feducmax,prcoinfo_ficarate,prcoinfo_ficamaxw,prcoinfo_ficawh,mat prcoinfo_m,mat prcoinfo_r,mat prcoinfo_e$,mat prcoinfo_gln$,prcoinfo_gli,mat prcoinfo_dedcode,mat prcoinfo_calcode,mat prcoinfo_dedfed,mat prcoinfo_rpnames2$,mat prcoinfo_sck,vacm,prcoinfo_mhw,mat prcoinfo_wcm,prcoinfo_tc,mat prcoinfo_jn$,prcoinfo_dc ioerr PRCoInfoOldVersionFinis
25360   !   fprcoInfoOldVersion: form pos 1,3*C 40,C 12,PD 6.3,PD 6.2,PD 5.2,10*C 8,N 2,PD 4.2,PD 3.3,12*PD 4.2,10*PD 3.3,25*C 12,31*N 1,10*C 6,3*PD 4.3,3*PD 3.2,4*PD 4.2,N 1,2*C 6,N 2
25370   !   close #h_tmp: 
25380   !   pause
25390   end if 
25400   ! PRCoInfoOldVersionFinis: !
25410   if ~exists(env$('Q')&'\'&cursys$&'mstr\Company.h'&str$(destination_company_number)) then 
25420     if exists(env$('Q')&'\'&cursys$&'mstr\'&cursys$&'coinfo.h'&str$(destination_company_number)) then 
25440       fnCopy(env$('Q')&'\'&cursys$&'mstr\'&cursys$&'coinfo.h'&str$(destination_company_number),env$('Q')&'\'&cursys$&'mstr\Company.h'&str$(destination_company_number))
25460       fnFree(env$('Q')&'\'&cursys$&'mstr\'&cursys$&'coinfo.h'&str$(destination_company_number))
26000     end if 
26020     if exists(env$('Q')&'\'&cursys$&'mstr\prcoinfo.h'&str$(destination_company_number)) then 
26040       fnCopy(env$('Q')&'\'&cursys$&'mstr\prcoinfo.h'&str$(destination_company_number),env$('Q')&'\'&cursys$&'mstr\Company.h'&str$(destination_company_number))
26060       fnFree(env$('Q')&'\'&cursys$&'mstr\prcoinfo.h'&str$(destination_company_number))
26080     end if 
26100     if exists(env$('Q')&'\'&cursys$&'mstr\coinfo.h'&str$(destination_company_number)) then 
26120       fnCopy(env$('Q')&'\'&cursys$&'mstr\coinfo.h'&str$(destination_company_number),env$('Q')&'\'&cursys$&'mstr\Company.h'&str$(destination_company_number))
26140       fnFree(env$('Q')&'\'&cursys$&'mstr\coinfo.h'&str$(destination_company_number))
26160     end if 
26180   end if 
26181   if cursys$='UB' then 
26182     fnLastBillingDate(d1,1)
26183   end if 
26200 fnend 
30000 def fn_ub_copy_extras
30010   ! r: import rates
30020   if exists(company_import_path$&'ubdata') then 
30030     if exists(env$('Q')&'\UBmstr\ubdata\*.h'&str$(destination_company_number)) then 
30040       fnFree(env$('Q')&'\UBmstr\ubdata\*.h'&str$(destination_company_number))
30050     end if  ! exists(env$('Q')&'\UBmstr\ubdata\*.h'&str$(destination_company_number))
30060     fnCopy(env$('at')&company_import_path$&'ubdata\*'&company_import_extension$,env$('Q')&'\UBmstr\ubdata\*.h'&str$(destination_company_number))
30070     fnStatus('UBmstr\ubData found in source and replaced destination.')
30080   else if exists(company_import_path$&'..\acsub\ratemst'&company_import_extension$) then 
30090     fnStatus('RateMst found in source acsUB folder.  Copying to UBmstr\ubData from there.')
30100     fnCopy(env$('at')&company_import_path$&'..\acsub\ratemst'&company_import_extension$,env$('Q')&'\UBmstr\ubdata\RateMst.h'&str$(destination_company_number))
30110     fnCopy(env$('at')&company_import_path$&'..\acsub\RateIdx1'&company_import_extension$,env$('Q')&'\UBmstr\ubdata\RateIdx1.h'&str$(destination_company_number))
30120     fnCopy(env$('at')&company_import_path$&'..\acsub\RateIdx2'&company_import_extension$,env$('Q')&'\UBmstr\ubdata\RateIdx2.h'&str$(destination_company_number))
30130   else 
30300     fnStatus('UBmstr\ubData did not exist in source. Destination ubData remains unchanged.')
30310   !   it is a UB system but no ubdata directory exists - so get the rate files from ACSUB
30320   !   copy in rates from acsUB
32000     dim import_install_path$*256
32020     import_install_path$=company_import_path$(1:len(company_import_path$))
32040     import_install_path$=rtrm$(import_install_path$,'\')
32060     import_install_path$=import_install_path$(1:pos(import_install_path$,'\',-1))
32080   !  pause
32100     if exists(env$('at')&import_install_path$&'S:\acsUB\ratemst'&company_import_extension$) then 
32120       fnCopy(env$('at')&import_install_path$&'S:\acsUB\ratemst'&company_import_extension$,env$('Q')&'\'&cursys$&'mstr\ubdata\*.h'&str$(destination_company_number))
32140     end if 
32160   end if 
32180   ! /r
32200   ! r: import notes folder
32220   if exists(company_import_path$&'UBmstr\notes'&company_import_extension$) then 
32240     fnFree(env$('Q')&'\'&cursys$&'mstr\notes.h'&str$(destination_company_number))
32258     ! this would not get subdirs ! fnCopy(env$('at')&company_import_path$&'UBmstr\notes'&company_import_extension$&'\*.*',env$('Q')&'\UBmstr\notes.h'&str$(destination_company_number)&'\*.*')
32260     execute 'sy xcopy "'&company_import_path$&'UBmstr\notes'&company_import_extension$&'\*.*" "'&os_filename$(env$('Q')&'\UBmstr\notes.h'&str$(destination_company_number))&'\*.*" /t /y'
32262     fnStatus('UB Notes imported.')
32280   end if  ! exists [import path]'&env$('Q')&'\UBmstr\notes.h[company_import_extension]
32300   ! /r
32320   ! r: rename ubmaster to customer
32340   if ~cnv_ub and ~exists(env$('Q')&'\UBmstr\Customer.h'&str$(destination_company_number)) and exists(env$('Q')&'\UBmstr\UBMaster.h'&str$(destination_company_number)) then ! and ~cnv_ub_capbell and ~cnv_ub_french_settlement then
32360     fnCopy(env$('Q')&'\UBmstr\UBMaster.h'&str$(destination_company_number),env$('Q')&'\UBmstr\Customer.h'&str$(destination_company_number))
32380     fnFree(env$('Q')&'\'&cursys$&'mstr\UBMaster.h'&str$(destination_company_number))
32400   end if  ! exists [import path]'&env$('Q')&'\UBmstr\notes.h[company_import_extension]
32420   ! /r
32450 fnend 
40000 IGNORE: continue 
41000 def fntranslate_rate_abreviations(from$*2,to$*2)
41020   fnStatus('Translating Rate abbreviations from '&from$&' to '&to$)
41040   from$=uprc$(from$)
41060   to$=uprc$(to$)
41064   open #1: "Name="&env$('Q')&"\UBmstr\ubData\RateMst.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubData\RateIdx1.h"&env$('cno')&",Use,RecL=374,KPs=1,KLn=4,Shr",internal,outIn,keyed 
41065   open #2: "Name="&env$('Q')&"\UBmstr\ubData\RateMst.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubData\RateIdx2.h"&env$('cno')&",Use,RecL=374,KPs=5,KLn=25,Shr",internal,outIn,keyed 
41100   do 
41120     read #1,using 'Form POS 1,C 2': rate_type$ eof EO_RATE
41140     if rate_type$=from$ then 
41160       rate_type$=to$
41180       rewrite #1,using 'Form POS 1,C 2': rate_type$
41200     end if 
41220   loop 
41240   EO_RATE: ! 
41260   close #1: 
41280   close #2: 
41300 fnend 
50000 def fn_ubmaster2customer_frenchset(file_source$*256,file_destination$*256)
50002   fnStatus('UBMaster Conversion for French Settlement processing...')
50020   open #h_old:=fngethandle: "Name="&file_source$,internal,input 
50040   F_UBMASTER_OLD: form pos 1,c 10,4*c 30,c 12,7*pd 2,11*pd 4.2,4*pd 4,15*pd 5,pd 4.2,pd 4,12*pd 4.2,2*pd 3,c 7,2*c 12,pd 3,10*pd 5.2,78*pd 5,13*pd 4.2,13*n 6,156*pd 4.2,13*n 6,13*pd 4.2,c 1,c 9,c 2,c 17
50060   open #h_new:=fngethandle: "Name="&file_destination$&',RecL=2067,Replace',internal,outIn,relative 
50120   dim rw4(22,13),gb(10),adr(2),f$(3)*12,g(12),a(7),b(11),c(4),d(15),e$(4)*30,extra(23),extra$(11)*30
50140   F_CUSTOMER_NEW: form pos 1,c 10,4*c 30,c 12,7*pd 2,11*pd 4.2,4*pd 4,15*pd 5,pd 4.2,pd 4,12*pd 4.2,2*pd 3,c 7,2*c 12,pd 3,10*pd 5.2,78*pd 5,13*pd 4.2,13*n 6,156*pd 4.2,13*n 6,13*pd 4.2,c 1,c 9,c 2,c 17,n 2,n 7,2*n 6,n 9,pd 5.2,n 3,3*n 9,3*n 2,3*n 3,n 1,3*n 9,3*pd 5.2,c 30,7*c 12,3*c 30
50160   do 
50180     read #h_old,using F_UBMASTER_OLD: z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,f,mat g,mat adr,alp$,f$(2),f$(3),bra,mat gb,mat rw4,df$,dr$,dc$,da$ eof EO_UBMASTER
50200     !     if trim$(z$)(1:6)='100875' then pause
50201     extra(17)=c(3) ! in new system   extra(17) = final billing code
50202     extra(3)=d(5) ! meter reading date - current
50203     extra(4)=d(6) ! meter reading date - prior
50204     c(3)=0 ! in new system   c(3) service 3 deposit date
50206     ! 
50207     extra(1)=val(z$(1:2))
50208     extra(2)=val(f$(2))*10 ! in new system    extra(2) is the sequence number
50210     f$(2)='' ! in new system    f$(2) = service 3 meter number
50212     extra$(2)=f$(1) !   extra$(2)  is the phone number
50214     f$(1)='' ! f$(1)  is the service 1 meter number
50216     ! 
50220     write #h_new,using F_CUSTOMER_NEW: z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,f,mat g,mat adr,alp$,f$(2),f$(3),bra,mat gb,mat rw4,df$,dr$,dc$,da$,mat extra,mat extra$
50240   loop 
50260   EO_UBMASTER: ! 
50280   close #h_old: 
50300   close #h_new: 
50320   fnub_index_customer
50322   fnStatus('UBMaster Conversion for French Settlement complete.')
50340 fnend 
52000 def fn_ubmaster2customer_merriam(file_source$*256,file_destination$*256)
52322   fnStatus('UBMaster Conversion for Merriam Woods complete.')
52340 fnend 
54000 def fn_ub_cust_route_from_acctno(cno)
54020   open #h_customer:=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",Shr",internal,outIn,relative 
54040   do 
54060     read #h_customer,using 'Form Pos 1,C 10,pos 1741,n 2,n 7': z$,route_number,sequence_number eof UCRFA_CUSTOMER_EOF
54080     if route_number=0 then 
54090       route_number=sequence_number=0
54100       route_number=val(z$(1:2)) conv UCRFA_SKIP_IT
54110       sequence_number=val(z$(3:7)) conv ignore
54120       rewrite #h_customer,using 'Form Pos 1,C 10,pos 1741,n 2,n 7': z$,route_number,sequence_number
54140     end if 
54160     UCRFA_SKIP_IT: ! 
54180   loop 
54200   UCRFA_CUSTOMER_EOF: ! 
54220   close #h_customer: 
54240 fnend 
56000 def fn_ub_combine_services(service_from,service_to)
56020   ! r: customer
56040   dim g(12), gb(10)
56060   open #h_customer:=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(destination_company_number)&",Shr",internal,outIn,relative 
56080   do 
56100     read #h_customer,using 'Form Pos 1,C 10,pos 300,12*pd 4.2,pos 388,10*pd 5.2': z$,mat g,mat gb eof UCCS_CUSTOMER_EOF
56120     if g(service_from)<>0 or gb(service_from)<>0 then 
56140       g(service_to)+=g(service_from)
56160       gb(service_to)+=gb(service_from)
56180       g(service_from)=0
56200       gb(service_from)=0
56220       rewrite #h_customer,using 'Form Pos 1,C 10,pos 300,12*pd 4.2,pos 388,10*pd 5.2': z$,mat g,mat gb
56240     end if 
56260   loop 
56280   UCCS_CUSTOMER_EOF: ! 
56300   close #h_customer: 
56320   ! /r
56340   ! r: transactions
56360   dim tg(11)
56380   open #h_customer:=fngethandle: "Name="&env$('Q')&"\UBmstr\UBTransVB.h"&str$(destination_company_number)&",Shr",internal,outIn,relative 
56400   do 
56420     read #h_ubtrans,using 'form pos 1,c 10,x 8,x 1,x 4,12*pd 4.2': x$,mat tg eof UCCS_TRANS_EOF
56440     if tg(service_from)<>0 then 
56460       tg(service_to)+=tg(service_from)
56480       tg(service_from)=0
56500       rewrite #h_ubtrans,using 'Form Pos 1,C 10,pos 300,12*pd 4.2,pos 388,10*pd 5.2': x$,mat tg
56520     end if 
56540   loop 
56560   UCCS_TRANS_EOF: ! 
56580   close #h_ubtrans: 
56600   ! /r
56620 fnend 
