00010 ! formerly acsTM\moInvoice
20020   library "S:\acsTM\print_invoice": fnprint_invoice
20040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fngethandle,fnAutomatedSavePoint,fncreg_read
20060   fntop(program$)
20082   client_id_sage_ax=3811
20084   client_id_brc=90
20120   dim sys_name$(40)*55,client_id$*5,b(8),iv$*12,skln$*80 ! co$(4)*40,
20150   dim client_addr$(3)*30,cap$*128
20160   dim in1$(3)
20180   dim inp(7),wo_desc$*30
20200   fn_get_system_list(mat sys_name$)
20220   fncreg_read('Last Invoice Number',tmp$, '1704001') : invoice_number=val(tmp$)
20280   let invoice_number=invoice_number+1
20420   pr newpage
20440   pr fields "5,2,cr 43": "Invoice Date (mmddyy):"
20460   pr fields "4,30,c": "Invoice Date MUST match expiration date of Annual Support contracts!"
20480   pr fields "6,2,cr 43": "Starting Invoice Number:"
20500   pr fields "7,2,Cr 43": "Starting Account Number:"
20520   let in1$(1)="5,46,Nz 6,U"
20540   let in1$(2)="6,46,N 12,U"
20560   let in1$(3)="7,46,N 5,U"
20600 ! let inv_date=date('mmddyy')
20620   pr fields "10,30,c 20,,B99": "Cancel (Esc)"
20640 SCREEN1_ASK: ! 
20650   rinput fields mat in1$: inv_date,invoice_number,starting_acct_no conv SCREEN1_ASK
20660   if cmdkey=5 or fkey=99 then goto XIT
20680   b4=date(days(inv_date,"mmddyy"),"ccyymmdd")
20690   fnAutomatedSavePoint('before')
20300   open #h_clmstr:=fngethandle: "Name="&env$('Q')&"\TMmstr\CLmstr.h"&env$('cno')&",KFName="&env$('Q')&"\TMmstr\CLIndex.h"&env$('cno')&",Shr",internal,input,keyed 
20320 ! pr 're-indexing support, just in case - probably not necessary to do so often, but one time there was this problem.'
20340 ! execute "Index "&env$('Q')&"\TMmstr\support.h"&env$('cno')&"  "&env$('Q')&"\TMmstr\support-idx.h"&env$('cno')&" 1/7,6/2,replace,DupKeys"
20360   open #h_support:=fngethandle: "Name="&env$('Q')&"\TMmstr\Support.h"&env$('cno')&",KFName="&env$('Q')&"\TMmstr\support-idx.h"&env$('cno')&",Shr",internal,input,keyed 
20380 F_SUPPORT: form pos 1,g 6,n 2,c 2,n 8,c 2,n 8,n 10.2,4*c 50
20400   fn_thsht_combine_entries(env$('Q')&"\TMmstr\TIMESHEET.h"&env$('cno'),"TMSHT"&wsid$,"TMSHT-IDX"&wsid$)
20700   restore #h_clmstr,key>=lpad$(str$(starting_acct_no),5): nokey SCREEN1_ASK
20710   pr newpage
20720   pr fields "10,10,Cc 60": "Printing Invoices..."
22000 ! r: process env$('Q')&"\TMmstr\TMWK1"&wsid$&".h"&env$('cno')
22020   open #h_tmwk1:=fngethandle: 'Name='&env$('Q')&'\TMmstr\TMWK1.h'&env$('cno')&',Replace,RecL=2484,Shr',internal,outin 
22040   F_TMWK1: form pos 1,c 5,n 1,n 6,c 12,30*c 6,30*c 55,30*pd 5.2,30*n 2,30*n 2,30*c 12
22060   dim cde$(30)*6
22080   dim inv_item$(30)*55
22100   dim inv_amt(30)
22120   dim inv_category(30)
22140   dim inv_category(30)
22160   dim inv_service_code(30)
22180   dim inv_gl$(30)*12
22200 ! restore #h_tmwk1:
22220   do 
22240     read #h_clmstr,using 'form pos 1,c 5,3*c 30,pos 283,pd 5.2': client_id$,mat client_addr$,pbal eof EOJ
22260     client_id=val(client_id$)
22280     let iv$=rpad$(str$(invoice_number),12)
22300     restore #h_support: ! ,key>=lpad$(trim$(client_id$),kln(h_support)): nokey BMM_SUPPORT_EOF
22320     do 
22340       read #h_support,using F_SUPPORT: cln$,scode,scode$,sdt1,stm$,sup_exp_date,scst eof BMM_SUPPORT_EOF
22360       cln=val(cln$)
22380       !     if client_id=918 then pr 'cln=';cln;'client_id=';client_id ! pause
22400       if cln=client_id then 
22420         !       if stm$="Mo" then goto BILL_MONTHLY_MAINT ! always bill monthly
22440         if stm$="Mo" then goto NXTJ ! never bill monthly
22460         !        pr "An";int(b4*.01);'=';int(sup_exp_date*.01);' !  bill annual if it expires this month'
22480         if stm$="An" and int(b4*.01)=int(sup_exp_date*.01) then goto BILL_MONTHLY_MAINT ! bill annual if it expires this month
22500         if stm$="An" then goto NXTJ ! skip annual people
22520         if b4<=sup_exp_date then goto NXTJ ! on maintenance
22540         BILL_MONTHLY_MAINT: ! 
22560         if scst=0 then goto NXTJ
22580         b(3)=scst
22600         b(8)=scode
22620         b3=b3+b(3)
22640         let inv_line=inv_line+1
22660         if stm$="An" then 
22680           let inv_item$(inv_line)='Annual'
22700         else 
22720           let inv_item$(inv_line)='Monthly'
22740         end if 
22760         if scode$='U4' then 
22780           let inv_item$(inv_line)=inv_item$(inv_line)&" Maintenance for (UB) Hand Held Add-On"
22800         else 
22820           let inv_item$(inv_line)=inv_item$(inv_line)&" Maintenance for "&trim$(sys_name$(scode))
22840           if trim$(sys_name$(scode))='' then pr ' sending blank system name  scode='&str$(scode) : pause 
22860         end if 
22880         let inv_amt(inv_line)=scst
22900         let inv_category(inv_line)=6
22920         let inv_service_code(inv_line)=scode
22940         let inv_gl$(inv_line)="  0  1160  0"
22960       end if 
22980       NXTJ: ! 
23000     loop  !  while cln=client_id ! commented out to work around a critical nokey problem above.  should severely slow things down though
23020     BMM_SUPPORT_EOF: ! if client_id=4568 then pr 'A (maintenance renewal stuff) 4568 encountered' : pause
23040     ! if client_id=918 then pr 'processing client 918 COMPLETE' : pause
23060     fn_print_inv
23080   loop ! /r
24000 EOJ: ! r:
24020   fn_summary_print
24040   fncloseprn
24060   open #h_ivnum:=fngethandle: "Name="&env$('Q')&"\TMmstr\IVNUM.h"&env$('cno')&",Use,RecL=8,Shr",internal,outin,relative 
24080   rewrite #h_ivnum,using "Form POS 1,N 8",rec=1: invoice_number-1
24100   close #h_ivnum: 
24120   close #h_clmstr: 
24140   close #h_tmwk1: 
24160   goto ASK_MERGE  ! /r
26000   ASK_MERGE: ! r:
26010   pr newpage
26020   pr fields "10,2,c 70": "All invoices have been printed.  Enter 1 to merge, or 2 to Stop"
26040     L1260: input fields "10,72,n 1,eu,n": num conv L1260
26060   if num=1 then 
26080     chain "S:\acsTM\TMMRGINV"
26100   else if num=2 then 
26120     goto XIT
26140   else 
26160     goto ASK_MERGE
26180   end if 
26190   ! /r
26220   XIT: fnxit
26900 ! _____________________________________________________________________
28000 def fn_timesheet2(h_tmsht) ! add charges not under maintenance to maintenance invoices
28020   read #h_tmsht,using F_TIME,key=client_id$: mat inp,b6,b7,b8,sc,o_o,wo_desc$ nokey TM_XIT2
28040   F_TIME: form pos 1,g 5,n 9,2*pd 3.2,pd 4.2,n 6,n 2,pd 2,pd 1,n 2,n 4,x 12,pd 3,c 30
28060   do 
28080     if b8=0 then b8=19
28100     delete #h_tmsht: ioerr ignore ! delete current record so it is not processed twice
28120     fn_bld_rec(client_id$)
28140     read #h_tmsht,using F_TIME: mat inp,b6,b7,b8,sc,o_o,wo_desc$ eof TM_XIT2
28160   loop while inp(1)=client_id
28180   TM_XIT2: ! 
28200 fnend 
32000 def fn_bld_rec(client_id$)
32020   if inv_line=30 then let fn_print_inv ! pr invoice if more than 20 entries
32040   if inv_line>29 then pause 
32060   let spk$=" "&client_id$&cnvrt$("n 2",b8)
32080   if inp(7)=2 then goto BLD_REC_L1780 ! always bill modifications
32100   if inp(7)=23 or inp(7)=11 then goto BLD_XIT ! always no charge
32120   read #h_support,using F_SUPPORT,key=spk$: cln$,scode,scode$,sdt1,stm$,sup_exp_date,scst nokey BLD_REC_L1780
32140   let trans_date=date(days(inp(6),'mmddyy'),'ccyymmdd')
32160   !   if stm$="Mo" or (stm$='An' and trans_date<=sup_exp_date) then goto BLD_XIT ! TM_RD2 ! on maintenance
32180   if (stm$='An' and trans_date<=sup_exp_date) then goto BLD_XIT ! TM_RD2 ! on maintenance
32200   ! if client_id=4625 then pause
33000   BLD_REC_L1780: ! 
33020   b(3)=inp(5)
33040   b(8)=b8
33060   b3=b3+b(3)
33080   ! let inv_line=inv_line+1
33100   if val(client_id$)=client_id_sage_ax or val(client_id$)=client_id_brc then 
33120   !     pause  ! let inv_item$(inv_line)=str$(inp(3))&' hours at a rate of '&&' on '&cnvrt$("pic(##/##/##)",inp(6))
33140     let inv_item$(inv_line+=1)=str$(inp(3))&' hours at a rate of '&cnvrt$('pic($$#.##)',inp(4))&' on '&cnvrt$("pic(##/##/##)",inp(6))
33160   else if inp(7)=2 then 
33180     let inv_item$(inv_line+=1)=str$(inp(3))&' hours of '&trim$(sys_name$(b8))&" programming on "&cnvrt$("pic(##/##/##)",inp(6))
33200   else 
33220     let inv_item$(inv_line+=1)=str$(inp(3))&' hours of '&trim$(sys_name$(b8))&" support on "&cnvrt$("pic(##/##/##)",inp(6))
33240   end if 
33260   let inv_amt(inv_line)=inp(5)
33280   let inv_category(inv_line)=6
33300   let inv_service_code(inv_line)=b8
33320   let inv_gl$(inv_line)="  0  1160  0"
33340   BLD_XIT: ! 
33360 fnend 
34000 def fn_get_system_list(mat sys_name$)
34020   mat sys_name$(40)
34040   let sys_name$(01)="General Ledger"
34060   let sys_name$(02)="Accounts Receivable"
34080   let sys_name$(03)="Accounts Payable"
34100   let sys_name$(04)="Utility Billing"
34120   let sys_name$(05)="Patient Billing"
34140   let sys_name$(06)="Property Tax"
34160   let sys_name$(07)="Accountants G.L."
34180   let sys_name$(08)="Fixed Asset"
34200   let sys_name$(09)="Time Management"
34220   let sys_name$(10)="Cash Register"
34240   let sys_name$(11)="Point of Sale"
34260   let sys_name$(12)="Invoicing"
34280   let sys_name$(13)="Inventory"
34300   let sys_name$(14)="Payroll"
34320   let sys_name$(15)="Purchase Order"
34340   let sys_name$(16)="Municipal Court"
34360   let sys_name$(17)="Electronic UB 82"
34380   let sys_name$(18)="Checkbook"
34400   let sys_name$(19)="Core"
34420   let sys_name$(20)="Printing"
34440   let sys_name$(21)="Job Cost"
34460   let sys_name$(22)=""
34480   let sys_name$(23)=""
34500   let sys_name$(24)="Sales Tax"
34520   let sys_name$(25)=""
34540   let sys_name$(26)="ITbrain Anti-Malware"
34560   let sys_name$(27)=""
34580   let sys_name$(28)=""
34600   let sys_name$(29)=""
34620   let sys_name$(30)=""
34640   let sys_name$(31)=""
34660   let sys_name$(32)=""
34680   let sys_name$(33)=""
34700   let sys_name$(34)=""
34720   let sys_name$(35)=""
34740   let sys_name$(36)=""
34760   let sys_name$(37)=""
34780   let sys_name$(38)=""
34800   let sys_name$(39)=""
34820   let sys_name$(40)=""
34860 fnend  ! fn_get_system_list
36000 def fn_thsht_combine_entries(file_from$*256,file_to$*256,file_to_index$*256)
36020   dim tce_to_inp(7)
36040   open #tce_h_from:=fngethandle: 'Name='&file_from$,internal,input 
36060   open #tce_h_to:=fngethandle: 'Name='&file_to$&',KFName='&env$('Temp')&'\tmwksh.idx,Replace,RecL='&str$(rln(tce_h_from))&',KPs=1/36/25,KLn=5/2/6',internal,outin,keyed 
36080   do 
36100     read #tce_h_from,using F_TIME: mat inp,b6,b7,b8,sc,o_o,wo_desc$ eof TCE_EOF
36120     if b8=20 then b8=19 ! ALL PRINTING SUPPORT IS COVERED BY CORE
36140     let tce_key$=cnvrt$("N 5",inp(1))&cnvrt$("N 2",b8)&cnvrt$("N 6",inp(6))
36160   !   IF INP(1)=2040 then pause
36180     read #tce_h_to,using F_TIME,key=tce_key$: mat tce_to_inp nokey TCE_TO_ADD
36200     let inp(3)+=tce_to_inp(3) ! time
36220     let inp(5)+=tce_to_inp(5) ! charge
36240     rewrite #tce_h_to,using F_TIME,key=tce_key$: mat inp,b6,b7,b8,sc,o_o,wo_desc$
36260   !   if inp(1)=4568 then rewr_count+=1
36280     goto TCE_NEXT
36300   TCE_TO_ADD: ! 
36320     write #tce_h_to,using F_TIME: mat inp,b6,b7,b8,sc,o_o,wo_desc$
36340   !   if inp(1)=4568 then wr_count+=1
36360   TCE_NEXT: ! 
36380   ! if inp(1)=4568 then pr '4568 (';b8;') had a time of ';inp(3);' on ';inp(6);' charge=';inp(5)
36400   loop 
36420   TCE_EOF: ! 
36440   close #tce_h_from: 
36460   close #tce_h_to: 
36480   ! pr 'rewr_count=';rewr_count
36500   ! pr '  wr_count=';wr_count
36520   ! pause
36540   execute 'index '&file_to$&' '&file_to_index$&' 1,5,replace,DupKeys,Shr'
36560   open #h_tmsht:=6: "Name="&file_to$&",KFName="&file_to_index$,internal,outin,keyed 
36580 fnend  ! fn_thsht_combine_entries
38000 def fn_summary_add
38020   open #22: "Name=skip_prn,RecL=80,replace",display,output ioerr SI_ADD
38040   pr #22: "Clnt   Name           Date      Prev Bal    New Amt     Total Due   Inv No  "
38060   pr #22: "_____ ______________  ________  __________  __________  __________  __________"
38080   SI_ADD: if (pbal+b3)<1 then let piv$="" else let piv$=iv$ ! if less than a dollar than don't charge it
38100   if piv$<>'' then 
38120     pr #22,using L1420: client_id$,client_addr$(1)(1:14),inv_date,pbal,b3,pbal+b3,piv$
38140   end if  ! piv$<>''
38160   L1420: form pos 1,c 5,x 2,c 15,pic(zz/zz/zz),3*nz 12.2,x 2,c 12
38180   let tb3=tb3+b3
38200   let tb4=tb4+pbal
38220 fnend 
40000   def fn_summary_print
40020     close #22: ioerr SP_XIT
40040     open #22: "Name=skip_prn",display,input 
40060     pr #255: "\ql"
40080     do 
40100       linput #22: skln$ eof SP_FINIS
40120       pr #255: skln$
40140     loop 
40160     SP_FINIS: ! 
40180     pr #255: 
40200     pr #255: "Total of Invoices Printed:  ";cnvrt$("N 12.2",tb3)
40220     pr #255: "Total of Previous Balances: ";cnvrt$("N 12.2",tb4)
40240     close #22,free: 
40260 SP_XIT: ! 
40280   fnend 
42000   def fn_print_inv ! pr INVOICE
42020     fn_timesheet2(h_tmsht)
42040     if b3>0 and sum(mat inv_amt)<250 then 
42060       let inv_line+=1
42080       if inv_line<30 and client_id<>client_id_sage_ax and client_id<>client_id_brc then 

42100         let inv_item$(inv_line)='Minimum Monthly Billing of $250.00'
42120         let inv_amt(inv_line)=250-sum(mat inv_amt) : b3+=inv_amt(inv_line)
42140         let inv_service_code(inv_line)=19
42160         let inv_gl$(inv_line)='  0  1160  0'
42180       end if  ! inv_line<30
42200     end if  ! pbal<250
42220     fn_summary_add ! pr all clients
42240     if b3=>1 then 
42260       write #h_tmwk1,using F_TMWK1: client_id$,2,inv_date,iv$,mat cde$,mat inv_item$,mat inv_amt,mat inv_category,mat inv_service_code,mat inv_gl$
42300     end if  ! b3=>1
42320     if b3<1 and pbal<1 then goto PI_SKIP_PRINT
42340 ! if sum(mat inv_amt)+pbal>0 then
42360     fnopenprn(cp,42,220,process)
42372 !   if trim$(client_id$)='3811' then pause 
42380     fnprint_invoice(align,client_id$, mat client_addr$,iv$,inv_date,mat inv_item$,mat inv_amt,pbal)
42400     pr #255: newpage ! mat inv_item$=("")
42420     let invoice_number=invoice_number+1 ! moved here 10/4/11 (from below) in an attempt to stop skipping invoice numbers
42440 ! end if
42460 PI_SKIP_PRINT: ! 
42480     mat inv_item$=(" ")
42500     mat inv_category=(0)
42520     mat inv_service_code=(0)
42540     mat inv_gl$=("")
42560 ! let invoice_number=invoice_number+1
42580     mat inv_amt=(0)
42600     let inv_line=b3=0
42620   fnend  ! fn_print_inv
44000 IGNORE: continue 
