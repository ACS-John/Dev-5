00010 ! formerly S:\acsUB\ubPDTnOf
00020 ! r: initial stuff
00030   library 'S:\Core\Library': fnacs,fnlbl,fntxt,fnerror,fntos,fnopenprn,fncloseprn,fnxit,fncomboa,fnfra,fnd1,fncmdset,fntop,fnchk,fndat,fncreg_read,fncreg_write,fnget_services,fngethandle
00040   on error goto ERTN
00050 ! 
00060   dim resp$(20)*80
00070   dim z$*10,e$*30,g(12),metradr$*30
00080   dim ba(13),badr(2),bt1(14,2),bd1(5),bd2(5),cap$*128,month(4),dat$*20
00090 ! 
26000   let fntop(program$,cap$="Past Due and Turn Off List")
26040   let fnd1(lbill)
26060   let fndat(dat$)
26080   dim opt_aai$(3)
26100   let opt_aai$(1)="[All]"
26120   let opt_aai$(2)="Active"
26140   let opt_aai$(3)="Inactive"
26150   let fncreg_read('ubpdtnof.aai',aai$,opt_aai$(1))
26160   let fncreg_read('ubpdtnof.printadr',printadr$,'False')
26180   let fncreg_read('ubpdtnof.excludecurrent',excludecurrent$,'False')
26200   let fncreg_read('ubpdtnof.excludelast',excludelast$,'False')
26220   let fncreg_read('ubpdtnof.pastduebalance',pastduebalance$,'False')
26230   let fncreg_read('ubpdtnof.pr_s4_meter_number',pr_s4_meter_number$,'False')
26240   let fncreg_read('ubpdtnof.pr_blank_lines_for_notes',pr_blank_lines_for_notes$,'False')
26260   let fncreg_read('ubpdtnof.accountSequence',accountSequence$,'True')
27000 ! 
27020   dim srvnam$(10)*20,srv$(10)*2
27040   fnget_services(mat srvnam$,mat srv$)
27100 ! /r
32000 ! r: SCREEN1
32020   let fntos(sn$="UBPdTnOf")
32040   let mylen=21 : let mypos=mylen+2
32060   let fnfra(1,1,3,40,"Aging Dates","Use the last day of each month for your aging dates (Use ccyymmdd format).")
32080 ! 
32100   let fnlbl(1,1,"Current Month:",mylen,1,0,1)
32120   let fntxt(1,mypos,10,10,1,"3",0,"Use the last day of your current mongh for the best aging results.",1)
32140   let resp$(1)=""
32160   let fnlbl(2,1,"Last Month:",mylen,1,0,1)
32180   let fntxt(2,mypos,10,10,1,"3",0,"Use the last day of last month.",1)
32200   let resp$(2)=""
32220   let fnlbl(3,1,"Month Before That:",mylen,1,0,1)
32240   let fntxt(3,mypos,10,10,1,"3",0,"Equivalent date from two months ago.",1)
32260   let resp$(3)=""
32280   let fnlbl(6,1,"Billing Date:",mylen,1)
32300   let fntxt(6,mypos,8,8,1,"1")
32320   let resp$(4)=str$(lbill)
32340   let fnlbl(7,1,"Final Billing Code:" ,mylen,1)
32360   let fncomboa("aai",7,mypos,mat opt_aai$)
32380   let resp$(5)=aai$
32400   let fnchk(9,40,"Print Meter Address:",1)
32420   let resp$(6)=printadr$
32440   let fnchk(10,40,"Exclude Current Month:",1)
32460   let resp$(7)=excludecurrent$
32480   let fnchk(11,40,"Exclude Last Month:",1)
32500   let resp$(8)=excludelast$
32520   let fnchk(12,40,"Print Past Due Balance:",1)
32540   let resp$(9)=pastduebalance$
32542   let fnchk(13,40,"Print "&trim$(srvnam$(4))&" Meter Number:",1)
32544   let resp$(10)=pr_s4_meter_number$
32546   let fnchk(15,40,"Print blank lines for notes:",1)
32548   let resp$(11)=pr_blank_lines_for_notes$
32550   let fnchk(17,40,"Account Sequence",1)
32552   let resp$(rc_accountSequence:=12)=accountSequence$
32560   let fncmdset(3)
32580   let fnacs(sn$,0,mat resp$,ck)
32600   if ck=5 then goto XIT
32620   for j=1 to 3
32640 L400: let x=pos(resp$(j),"/",1)
32660     if x>0 then let resp$(j)(x:x)="" : goto L400
32680   next j
32700   let lastday(1)=val(resp$(1))
32720   let firstday(1)=(val(resp$(1)(1:6))*100)+1
32740   let lastday(2)= val(resp$(2))
32760   let firstday(2)=(val(resp$(2)(1:6))*100)+1
32780   let lastday(3)=val(resp$(3))
32800   let firstday(3)=(val(resp$(3)(1:6))*100)+1
32820   let lbill=val(resp$(4))
32840   let aai$=printal$=resp$(5)
32860   let printadr$=resp$(6) : if printadr$="True" then let printadr=1 ! wants meter address printed
32880   let excludecurrent$=resp$(7) : if excludecurrent$="True" then let excludecurrent=1 ! do not list those owing just the current month
32900   let excludelast$=resp$(8) : if excludelast$="True" then let excludelast=1 ! do not list those owing just the current month and last month
32920   let pastduebalance$=resp$(9) : if pastduebalance$="True" then let pastduebalance=1 ! only show past due amount in balance column
32925   let pr_s4_meter_number$=resp$(10) : if pr_s4_meter_number$="True" then let pr_s4_meter_number=1 ! only show past due amount in balance column
32926   let pr_blank_lines_for_notes$=resp$(11)
32928   let accountSequence$=resp$(rc_accountSequence)
32930   let fncreg_write('ubpdtnof.aai',aai$)
32940   let fncreg_write('ubpdtnof.printadr',printadr$)
32960   let fncreg_write('ubpdtnof.excludecurrent',excludecurrent$)
32980   let fncreg_write('ubpdtnof.excludelast',excludelast$)
33000   let fncreg_write('ubpdtnof.pastduebalance',pastduebalance$)
33010   let fncreg_write('ubpdtnof.pr_s4_meter_number',pr_s4_meter_number$)
33020   let fncreg_write('ubpdtnof.pr_blank_lines_for_notes',pr_blank_lines_for_notes$)
33040   let fncreg_write('ubpdtnof.accountSequence',accountSequence$)
33880 ! /r
42000   on fkey 5 goto DONE
42020 ! r: the report
42040 ! r: report setup
42060   let fnopenprn
42080   gosub HDR1
42100   if accountSequence$='True' then
42120     open #hCustomer:=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,input,keyed 
42140   else
42160     open #hCustomer:=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndx5.h"&env$('cno')&",Shr",internal,input,keyed 
42180   end if
42200   open #hTrans:=fngethandle: "Name="&env$('Q')&"\UBmstr\UBTransVB.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\UBTrIndx.h"&env$('cno')&",Shr",internal,input,keyed 
42220   ! open #ratemst:=8: "Name="&env$('Q')&"\UBmstr\ubData\RateMst.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubData\RateIdx1.h"&env$('cno')&",Shr",internal,input,keyed 
42240   gosub BUD1
42260 ! /r
44000 MAIN_LOOP_TOP: ! 
44020   read #hCustomer,using F_CUSTOMER: z$,metradr$,e$,a7,final,bal,f,mat g,s4_meter_number$,route eof TOTAL_FINAL
44040 F_CUSTOMER: form pos 1,c 10,c 30,pos 41,c 30,pos 155,pd 2,pos 1821,n 1,pos 292,pd 4.2,pos 296,pd 4,pos 300,12*pd 4.2,pos 373,c 12,pos 1741,n 2
44060   if bud1=1 then gosub BUD2
44080   if bd1>0 then goto L650 ! IF BUDGET BILLING AND HAVE NOT PAID LAST BILL, LIST ANYWAY   ( BD1=# OF BUDGET BILLS NOT PAID)
44100   if totba>0 and bd1=0 then goto MAIN_LOOP_TOP ! DON'T LIST IF BUDGET BILL AND HAVE PAID LAST BILL (NO MATTER WHAT BALANCE)
44120   if bal<=1 then goto MAIN_LOOP_TOP
44140   if final=3 then let final=0 ! consider active customer who are not be billed the same as regular active customers.
44160   if printal$=opt_aai$(2) and final>0 then goto MAIN_LOOP_TOP
44180   if printal$=opt_aai$(3) and final=0 then goto MAIN_LOOP_TOP
44200 L650: ! 
44220   if holdrt=0 or route=0 or holdrt=route then goto L690
44240   gosub TOTAL_BOOK
44260   print #255: newpage
44280   gosub HDR1
44300 L690: ! 
44320   gosub TRANS_ACCUMULATE
44340 ! 
45000 ! if trim$(z$)='100780.00' then pause
45020   let az$=""
45040   if month(4)>0 or bd1=3 then 
45060     let az$="****"
45080   else if month(3)>0 or bd1=2 then 
45100     let az$="***"
45120   else if month(2)>0 or bd1=1 then 
45140     let az$="**"
45160   end if 
45180 ! 
45200   if totba>0 then let lev$="L" else let lev$=""
45220   if excludecurrent=1 and len(az$)<2 then goto L880 ! exclude those oweing only current month
45240   if excludelast=1 and len(az$)<3 then goto L880 ! exclude those oweing only current month  or previous month
45260   if pastduebalance=1 and excludecurrent=1 then let bal=bal-month(1) ! don't show current in past due balance column
45280   if pastduebalance=1 and excludelast=1 then let bal=bal-month(2) ! don't show last month in past due balance column
45300 ! 
45320   if printadr and pr_s4_meter_number then 
45340     print #255,using F_REPORT_LINE: z$,e$(1:25),bal,f,az$,lev$,metradr$(1:25),fn_s4_meter_number$ pageoflow PGOF
45360   else if pr_s4_meter_number then 
45380     print #255,using F_REPORT_LINE: z$,e$(1:25),bal,f,az$,lev$,fn_s4_meter_number$ pageoflow PGOF
45400   else if printadr then 
45420     print #255,using F_REPORT_LINE: z$,e$(1:25),bal,f,az$,lev$,metradr$(1:25) pageoflow PGOF
45440   else 
45460     print #255,using F_REPORT_LINE: z$,e$(1:25),bal,f,az$,lev$ pageoflow PGOF
45480   end if 
45500   if trans_accumulate_execption$<>'' then 
45520     print #255,using 'form pos 64,C 24': trans_accumulate_execption$ pageoflow PGOF
45540   end if 
45542   if pr_blank_lines_for_notes$='True' then 
45544     print #255: '' pageoflow PGOF
45548     print #255: rpt$('_',71) pageoflow PGOF
45550     print #255: '' pageoflow PGOF
45558   end if 
45560 F_REPORT_LINE: form pos 1,c 12,c 25,n 12.2,pic(bbzz/zz/zzbb),x 3,c 4,x 1,c 1,x 2,c 25,x 2,c 25
45580 ! 
45600   let s2=s2+bal
45620   let t2=t2+bal
45640   let t1=t1+g(10)
45660   let s1=s1+g(10)
45680   let holdrt=route
45700 L880: ! 
45720   goto MAIN_LOOP_TOP
45740 ! /r
50000 HDR1: ! r:
50020   let p2=p2+1
50040   print #255: "\qc "&env$('cnam')
50060   print #255: "\qc  {\f181 \fs28 \b "&env$('program_caption')&"}"
50080 ! Print #255: "As of "&CNVRT$("pic(zzzz/zz/zz)",LASTDAY(1))
50100   print #255: "As of "&dat$
50120   print #255,using L970: "\ql "&date$,"Page "&str$(p2)
50140 L970: form pos 1,c 70,cr 14
50160   if pastduebalance=1 then 
50180     print #255: "                                         Past Due  Last Bill   Turn"
50200   else 
50220     print #255: "                                         Current   Last Bill   Turn"
50240   end if 
50260   if printadr and pr_s4_meter_number then 
50280     print #255: "{\ul Account No}  {\ul Customer Name            }  {\ul   Balance } {\ul    Date   }  {\ul  Off}      {\ul Meter Address            }  {\ul Meter Number}"
50300   else if pr_s4_meter_number then 
50320     print #255: "{\ul Account No}  {\ul Customer Name            }  {\ul   Balance } {\ul    Date   }  {\ul  Off}      {\ul Meter Number}"
50340   else if printadr then 
50360     print #255: "{\ul Account No}  {\ul Customer Name            }  {\ul   Balance } {\ul    Date   }  {\ul  Off}      {\ul Meter Address            }"
50380   else 
50400     print #255: "{\ul Account No}  {\ul Customer Name            }  {\ul   Balance }  {\ul    Date   }  {\ul  Off}"
50420   end if 
50440   return  ! /r
52000 TOTAL_BOOK: ! r:
52020   print #255: "" pageoflow PGOF
52040   print #255: "" pageoflow PGOF
52060   print #255: "Totals For Route Number ";holdrt;
52080   print #255,using F_PR_TOTAL: s2 pageoflow PGOF
52100 F_PR_TOTAL: form pos 38,pic(-,---,---.##)
52120   print #255: "" pageoflow PGOF
52140   let s1=0
52160   let s2=0
52180   let holdrt=route
52200   return  ! /r
54000 TOTAL_FINAL: ! r:
54020   gosub TOTAL_BOOK
54040 ! TOTAL_GRAND: !
54060   let s1=t1 : let s2=t2
54080   print #255: ""
54100   print #255: ""
54120   print #255: "               Grand Totals";
54140   print #255,using F_PR_TOTAL: s2
54160   goto DONE ! /r
56000 DONE: ! r:
56020   close #hCustomer: ioerr ignore
56040   close #hTrans: ioerr ignore
56060   let fncloseprn
56080   goto XIT ! /r
56100 PGOF: ! r:
56120   print #255: newpage
56140   gosub HDR1
56160   continue  ! /r
58000 XIT: let fnxit
60000 IGNORE: continue 
62000 BUD1: ! r:
62020   let bud1=0
62040   open #81: "Name="&env$('Q')&"\UBmstr\BudMstr.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\BudIdx1.h"&env$('cno')&",Shr",internal,outin,keyed ioerr L1390
62060   open #82: "Name="&env$('Q')&"\UBmstr\BudTrans.h"&env$('cno')&",Shr",internal,outin,relative 
62080   let bud1=1
62100 L1390: return  ! /r
64000 BUD2: ! r:
64020   let bd1=totba=0
64040   mat bd1(5) : mat bd1=(0) : mat bd2=(0)
64060   if bud1=0 then goto L1580
64080   read #81,using L1470,key=z$: z$,mat ba,mat badr nokey L1580
64100   for j=2 to 12: let totba=totba+ba(j): next j
64120 L1470: form pos 1,c 10,pd 4,12*pd 5.2,2*pd 3
64140   let ta1=badr(1)
64160 L1490: if ta1=0 then goto L1580
64180   read #82,using L1510,rec=ta1: z$,mat bt1,nba norec L1580
64200 L1510: form pos 1,c 10,2*pd 4,24*pd 5.2,2*pd 4,pd 3
64220   if bt1(14,1)>0 then goto L1570
64240   let bd1=bd1+1
64260   if bd1>5 then goto L1580
64280   let bd1(bd1)=bt1(1,2)
64300   let bd2(bd1)=ta1
64320 L1570: let ta1=nba : goto L1490
64340 L1580: ! 
64360   return  ! /r
66000 ! <Updateable Region: ERTN>
66020 ERTN: let fnerror(program$,err,line,act$,"xit")
66040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
66060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
66080   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
66100 ERTN_EXEC_ACT: execute act$ : goto ERTN
66120 ! /region
67000 PR_TRAN_DEBUG_DATA: ! r:
67020   if debug_this_tran then print '                            tdate=';tdate;' tcode=';tcode;' tamount=';tamount : let debug_this_tran=1 else let debug_this_tran=0 ! pause
67040   return  ! /r
68000 TRANS_ACCUMULATE: ! r:
68040   mat month=(0)
68042   dim trans_accumulate_execption$*24
68044   let trans_accumulate_execption$=''
68046 ! if trim$(z$)='100093.57' then pause
68060   restore #hTrans,key>=z$&"         ": nokey TA_NO_TRANS
68070   let ta_p1_read_count=0
68080 TA_TRANS_READ: ! 
68100   read #hTrans,using 'form pos 1,c 10,n 8,n 1,pd 4.2': p$,tdate,tcode,tamount eof TA_PHASE_2
68101   let ta_p1_read_count+=1
68102 ! if env$('ACSDeveloper')<>'' and trim$(z$)='100780.00' then let debug_this_account=1 else debug_this_account=0 ! pause
68104 ! if debug_this_account and str$(tdate)(1:4)='2015' then debug_this_tran=1 else debug_this_tran=0 ! pause
68106   if p$<>z$ and ta_p1_read_count=1 then let trans_accumulate_execption$='(no transaction history)'
68120   if p$<>z$ then goto TA_PHASE_2
68140   for j=1 to 3
68160     if tdate<firstday(3) then ! older than we want to analyze
68180 !     if debug_this_tran then pr '  A  older than what we want to analyze'
68182       goto TA_TRANS_READ
68200     else if tdate>=firstday(j) and tdate<=lastday(j) and (tcode = 1 or tcode=2 or tcode=5) then 
68220       let month(j)=month(j)+tamount
68240 !      if debug_this_tran then gosub PR_TRAN_DEBUG_DATA : pr '  B  month(';j;')=';month(j);'    because ';tdate;' =/between >';firstday(j);' - ';lastday(j);' and tcode=';tcode
68242       goto TA_TRANS_READ
68260     else if tdate>lastday(j) and (tcode = 1 or tcode=2 or tcode=5) then ! accumulate all collections in month 4
68280       let month(j)=month(j)+tamount
68300 !      if debug_this_tran then gosub PR_TRAN_DEBUG_DATA : pr '  C  month(';j;')=';month(j);'    because ';tdate;'>';lastday(j);' and tcode=';tcode
68302       goto TA_TRANS_READ
68320     end if 
68340   next j
68360   goto TA_TRANS_READ
70000 TA_PHASE_2: ! 
70020   if debug_this_account then print ' the month accumulators before pahse 2' : print mat month ! pause
70040   let holdbal=bal
70060   for j=1 to 4 ! find oldest month still owed
70080     if holdbal<=0 then 
70100 !     if debug_this_account then pr '  AA  changing month(';j;') from ';month(j);' to 0 because holdbal<=0'
70120       let month(j)=0
70140 !   else if env$('client')="Albany" and holdbal<10.50 then ! don't let any balance less than a minimum bill cause a month to show delinquent
70180 !     let month(j)=0
70200     else if holdbal>0 then 
70220 !     if debug_this_account then pr '  CC  changing holdbal from (';holdbal;') to ';holdbal-month(j)
70240       let holdbal=holdbal-month(j)
70260     end if 
70280 !   if debug_this_account then pause
70300   next j
70320   goto TA_FINIS
72000 TA_NO_TRANS: ! 
72020   let trans_accumulate_execption$='(no transaction history)'
72040   goto TA_PHASE_2 ! TA_FINIS
74000 TA_FINIS: ! 
74020   if debug_this_account then 
74040     print ' the month accumulators AFTER pahse 2' : print mat month
74060     print ' trans_accumulate_execption$='&trans_accumulate_execption$
74080     pause 
74100   end if 
74120   return  ! /r
76000   def fn_s4_meter_number$*12
76020     let fn_s4_meter_number$=s4_meter_number$
76040   fnend 
