10000 ! replace S:\Checkbook\Transaction
10100 ! Checkbook transaction file editor
10200 ! ______________________________________________________________________
10300   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fncno,fnerror,fndat,fndate_mmddyy_to_ccyymmdd,fncmdset,fntos,fnlbl,fnacs,fncombof,fnmsgbox,fnfra,fntxt,fnbutton,fnflexinit1,fnflexadd1,fncmdkey,fnchk,fnaddpayee,fnagl$,fnqgl,fnrgl$,fnupdatebankbal,fnbankbal,fngethandle,fnaddreceipt,fnchain,fnRemoveDeletedRecords,fnIndex
10400   on error goto ERTN
10500 ! r: dim
10600   dim cap$*128,tr$(5)*35,de$*30,bn$*40,aa(2)
10700   dim ml$(10)*80 ! message box message lines
10800   dim resp$(100)*100,gldesc$*30
10900   dim vn$*8,ph$*12 ! payee file
11000   dim tcde$*25 ! description of working transaction type
11100   dim chdr$(11),cmask$(11),item$(11) ! temp variables for flexgrids
11200   dim item$(10)*50 ! flex grid items
11300   dim tradesc$*30 ! for tralloc file
11400 ! /r
11500   fntop(program$, cap$="Transaction") ! r:
11600 ! constants
11700   cancel=99 : right=1 : limit_to_list=1 : center=2
11800   ccyymmdd$='3' : mmddyy$='1' : number$='30'
11900   pointtwo$='32' : disable=1 : add_all=2 : false=0
12000   true=1
12100   true$='True' : false$='False'
12200 ! defaults
12300   addloop$='True'
12400   fncno(cno)
12500   d1=val(date$(4:5))*10000+val(date$(7:8))*100+val(date$(1:2))
12600   if d1<19999 then d1=d1+110000 : rollback=1 else d1=d1-10000 : rollback=0
12700   begd=(int(d1/10000)*10000)+100+val(date$('yy'))-rollback
12800   endd=begd+3000
12900   transstartdate=date('mm')*10000+100+date('yy') ! changed default to beginning of month as per billing's suggestion on 2/9/12    ! begd
13000   transenddate=date('mmddyy') ! val(date$(4:5))*10000+val(date$(7:8))*100+val(date$(1:2))
13100   open #20: "Name="&env$('Q')&"\CLmstr\Company.h"&env$('cno')&",Shr",internal,input,relative ioerr CHAIN_SELCNO: read #20,using 'Form Pos 152,N 2',rec=1,release: wbc : close #20: 
13200   gosub OPEN_TRANSACTION_FILES
13300   open #h_tralloc=fngethandle: "Name="&env$('Q')&"\CLmstr\TrAlloc.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\TrAlloc-Idx.h"&env$('cno')&",Shr",internal,outin,keyed 
13400 ! /r
13500 SCREEN1: ! r:
13600 ! select limitations for the menu1's record selection grid
13700   fntos(sn$='Trans-Screen1')
13800   lc=0 : mylen=25 : mypos=mylen+2 : width=100
13900   fnlbl(lc+=1,1,'Transaction Grid Selection Criteria',width,center)
14000   lc+=1
14100   fnlbl(lc+=1,1,"Working Bank:",mylen,right)
14200   fncombof('BankAll',lc,mypos,0,env$('Q')&"\CLmstr\BankMstr.h"&env$('cno'),1,2,3,30,env$('Q')&"\CLmstr\BankIdx1.h"&env$('cno'),add_all)
14300   if wbc=0 then resp$(1)='[All]' else resp$(1)=str$(wbc)
14400   fnlbl(lc+=1,1,"Working Transaction Type:",mylen,right)
14500   fncombof('TransactionTypeall',lc,mypos,0,env$('Q')&"\CLmstr\TransactionType.dat",1,1,2,25,env$('Q')&"\CLmstr\TransactionType.idx",add_all)
14600   if wtt=0 then resp$(2)='[All]' else resp$(2)=str$(wtt)
14700   fnlbl(lc+=1,1,"Payee:",mylen,right)
14800   fncombof('Payeeall',lc,mypos,0,env$('Q')&"\CLmstr\PayMstr.h"&env$('cno'),1,8,9,30,env$('Q')&"\CLmstr\PayIdx1.h"&env$('cno'),add_all)
14900   if wpayee$='' then resp$(3)='[All]' else resp$(3)=wpayee$
15000   lc+=1
15100   fnlbl(lc+=1,1,"Transaction Starting Date:",mylen,right)
15200   fntxt(lc,mypos,8,0,left,ccyymmdd$,0,'Blank for All')
15300   resp$(4)=str$(transstartdate)
15400   fnlbl(lc+=1,1,"Transaction Ending Date:",mylen,right)
15500   fntxt(lc,mypos,8,0,left,ccyymmdd$,0,'Blank for All')
15600   resp$(5)=str$(transenddate)
15700   fnlbl(lc+=1,1,"Statement Date Cleared:",mylen,right)
15800   fntxt(lc,mypos,8,0,left,ccyymmdd$,0,'Blank for All')
15900   resp$(6)=''
16000   lc+=1
16100   fnlbl(lc+=1,1,"Posting Status:",mylen,right)
16200   fncombof('PostCodeall',lc,mypos,0,"S:\acsCL\PostingCode.dat",1,1,2,25,"S:\acsCL\PostingCode.idx",add_all)
16300   resp$(7)='[All]'
16400   fnlbl(lc+=1,1,"Source:",mylen,right)
16500   fncombof('SourceAll',lc,mypos,0,"S:\acsCL\SourceCode.dat",1,1,2,25,"S:\acsCL\SourceCode.idx",add_all)
16600   resp$(8)='[All]'
16700   fnlbl(lc+=1,1,"Check/Reference #:",mylen,right)
16800   fntxt(lc,mypos,8,0,left,"",0,'Enter the check or reference # to access a specific transactin, else blank for all')
16900   resp$(9)=''
17000   fncmdset(2)
17100   fnacs(sn$,0,mat resp$,ckey)
17200   if ckey=5 or ckey=cancel then goto XIT
17300   if resp$(1)='[All]' then 
17400     wbc=0 : bn$='[All]'
17500   else 
17600     wbc=val(resp$(1)(1:2))
17700     bn$=resp$(1)(4:len(resp$(1)))
17800   end if 
17900   if resp$(2)='[All]' then wtt=0 else wtt=tcdekey=val(resp$(2)(1:1))
18000   if wtt=0 then tcde$='[All]' else tcde$=resp$(2)(3:len(resp$(2)))
18100   if resp$(3)='[All]' then wpayee$=resp$(3) else wpayee$=resp$(3)(1:8)
18200   transstartdate=val(resp$(4))
18300   transenddate=val(resp$(5))
18400   statementdatecleared=val(resp$(6))
18500   postingcode$=resp$(7)
18600   sourcecode$=resp$(8)
18700   if trim$(resp$(9))<>"" then selectedck=1 else selectedck=0
18800   if trim$(resp$(9))="" then goto MENU1
18900   if wbc=0 then wbc=1
19000   if trim$(tcde$)="" or trim$(tcde$)="[All]" then tcde$="1" : tcdekey=1 ! try defaulting to check
19100   check_ref$=cnvrt$("pic(ZZ)",wbc)&str$(tcdekey)&lpad$(rtrm$(resp$(9)),8)
19200   read #h_trmstr(1),using 'Form Pos 1,C 3,C 8,G 6,PD 10.2,C 8,C 35,N 1,N 6,N 1',key=check_ref$: newkey$,tr$(1),tr$(2),tr3,tr$(4),tr$(5),posting_code,clr,scd nokey TRY_RECEIPT
19300   editrec=rec(h_trmstr(1)): goto DO_EDIT
19400 TRY_RECEIPT: tcde$="2" ! try as receipt
19500   check_ref$=cnvrt$("pic(ZZ)",wbc)&str$(tcdekey)&lpad$(rtrm$(resp$(9)),8)
19600   read #h_trmstr(1),using 'Form Pos 1,C 3,C 8,G 6,PD 10.2,C 8,C 35,N 1,N 6,N 1',key=check_ref$: newkey$,tr$(1),tr$(2),tr3,tr$(4),tr$(5),posting_code,clr,scd nokey MENU1
19700   editrec=rec(h_trmstr(1)): goto DO_EDIT
19800   goto MENU1 ! /r
20000 OPEN_TRANSACTION_FILES: ! r:
20100   open #h_trmstr(1)=fngethandle: "Name="&env$('Q')&"\CLmstr\TrMstr.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\TrIdx1.h"&env$('cno')&",Shr",internal,outin,keyed 
20200   open #h_trmstr(2)=fngethandle: "Name="&env$('Q')&"\CLmstr\TrMstr.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\TrIdx2.h"&env$('cno')&",Shr",internal,outin,keyed 
20300   return  ! /r
20500 MENU1: ! r:
20600   fntos(sn$='Transaction-Menu1')
20700   lc=0 : mylen=30 : mypos=mylen+2
20800   fc=0 ! frame count
20900   fnfra(1,1,10,100,'Transaction Grid Selection Criteria')
21000   frame=fc+=1
21100   lc=0
21200   fnlbl(lc+=1,1,'Bank:',mylen,right,0,frame)
21300   fntxt(lc,mypos,3,0,center,'',disable,'',frame)
21400   resp$(1)=str$(wbc)
21500   fntxt(lc,mypos+5,30,0,center,'',disable,'',frame)
21600   resp$(2)=bn$
21700   fntxt(lc,mypos+38,15,0,right,pointtwo$,disable,'',frame)
21800   resp$(3)=str$(fnbankbal(wbc))
21900   fnlbl(lc+=1,1,'Transaction Type:',mylen,right,0,frame)
22000   fntxt(lc,mypos,1,0,left,'',disable,'',frame)
22100   resp$(4)=str$(wtt)
22200   fntxt(lc,mypos+4,25,0,left,'',disable,'',frame)
22300   resp$(5)=tcde$
22400   lc+=1
22500   fnlbl(lc+=1,1,'Transaction Starting Date:',mylen,right,0,frame)
22600   fntxt(lc,mypos,0,0,left,ccyymmdd$,disable,'',frame)
22700   resp$(6)=str$(transstartdate)
22720 ! 
22800   fnlbl(lc+=1,1,'Transaction Ending Date:',mylen,right,0,frame)
22900   fntxt(lc,mypos,0,0,left,ccyymmdd$,disable,'',frame)
23000   resp$(7)=str$(transenddate)
23020 ! 
23100   fnlbl(lc+=1,1,'Statement Cleared Date:',mylen,right,0,frame)
23200   fntxt(lc,mypos,0,0,left,ccyymmdd$,disable,'',frame)
23300   resp$(8)=str$(statementcleareddate)
23320 ! 
23400   lc+=1
23420 ! 
23500   fnlbl(lc+=1,1,'Posting Status:',mylen,right,0,frame)
23600   fntxt(lc,mypos,30,0,left,'',disable,'',frame)
23700   resp$(9)=postingcode$
23720 ! 
23800   fnlbl(lc+=1,1,'Source:',mylen,right,0,frame)
23900   fntxt(lc,mypos,30,0,left,'',disable,'',frame)
24000   resp$(10)=sourcecode$
24100   fnbutton(1,90,'Change',1,'',1,10,frame)
24200 ! r: Transaction Allocation Grid
24210   fnlbl(lc=13,1,'Transaction Grid',20)
24220   mat chdr$(11) : mat cmask$(11) : mat item$(11)
24230   chdr$(1)='Rec'
24240   chdr$(2)='Ck/Rf'
24250   chdr$(3)='Date'
24260   chdr$(4)='Amount'
24270   chdr$(5)='Payee'
24280   chdr$(6)='Name/Description'
24290   chdr$(7)='PC'
24300   chdr$(8)='Stmt Clr Date'
24310   chdr$(9)='SC'
24320   chdr$(10)='Bank'
24330   chdr$(11)='Type'
24340   cmask$(1)='20'
24350   cmask$(2)=''
24360   cmask$(3)='1'
24370   cmask$(4)='10'
24380   cmask$(8)='1'
25120 ! 
25500   fnflexinit1('ApTrans',14,1,10,100,mat chdr$,mat cmask$,1,pas)
25600 ! if pas=1 then goto 1140  ! for pas to work properly, totals need to be adjusted for adds,corrections, and deletions
25700   restore #h_trmstr(1): 
25800   transactionstotal=0
25900 READ_TRMSTR1_1: ! 
26000   read #h_trmstr(1),using 'Form Pos 1,C 3,C 8,G 6,PD 10.2,C 8,C 35,N 1,N 6,N 1': newkey$,tr$(1),tr$(2),tr3,tr$(4),tr$(5),posting_code,clr,scd eof EO_FLEX1
26100   if wbc<>0 and val(newkey$(1:2))<>wbc then goto READ_TRMSTR1_1
26200   if wtt<>0 and val(newkey$(3:3))<>wtt then goto READ_TRMSTR1_1
26300   if wpayee$<>'[All]' and trim$(wpayee$)<>trim$(tr$(4)) then goto READ_TRMSTR1_1
26400   if transstartdate<>0 and transstartdate>fndate_mmddyy_to_ccyymmdd(val(tr$(2))) then goto READ_TRMSTR1_1
26500   if transenddate<>0 and transenddate<fndate_mmddyy_to_ccyymmdd(val(tr$(2))) then goto READ_TRMSTR1_1
26600   if statementdatecleared<>0 and statementdatecleared<>fndate_mmddyy_to_ccyymmdd(clr) then goto READ_TRMSTR1_1
26700   if postingcode$<>'[All]' and str$(posting_code)<>postingcode$(1:1) then goto READ_TRMSTR1_1
26800   if sourcecode$<>'[All]' and str$(scd)<>sourcecode$(1:1) then goto READ_TRMSTR1_1
26900   item$(1)=str$(rec(h_trmstr(1)))
27000   item$(2)=tr$(1) : item$(3)=tr$(2) : item$(4)=str$(tr3)
27100   item$(5)=tr$(4) : item$(6)=tr$(5) : item$(7)=str$(posting_code)
27200   item$(8)=str$(clr) : item$(9)=str$(scd)
27300   item$(10)=newkey$(1:2) : item$(11)=newkey$(3:3)
27400   fnflexadd1(mat item$)
27500   transactionstotal+=tr3
27600   goto READ_TRMSTR1_1
27700 EO_FLEX1: ! /r
27710   resp$(11)=''
27720 !  r:
27740 !  this uses the transactionstotal which is calculated when the flex grid is made
27750   fnlbl(13,31,'Transactions Total:',mylen,right)
27760   fntxt(13,mypos+30,15,0,right,pointtwo$,disable,'This is the total of only the transactions shown in the Transaction Grid above.  To update this total click the change button at the top and reselect your Transaction Grid Selection Criteria')
27780   resp$(12)=str$(transactionstotal)
27790 ! /r
28100   fncmdkey('E&dit',3,1,0,"Highlight any entry and click edit to change or review the complete entry.")
28200   fncmdkey('&Add Deposit (Receipt)',2,0,0,"Allows you to enter deposits into the files.")
28300   fncmdkey('Add &Check (Disbursment)',8,0,0,"Allows you to add hand written checks to the checkbook files.")
28400   fncmdkey('&ReIndex',7,0,0,"Allows you to reindex the check history files. Should only be necessary if power failures have corrupted the files.")
28500   fncmdkey('&Change Selection Criteria',6,0,0,"Allows you to return to first screen and change date ranges, etc.")
28600   fncmdkey('E&xit',5,0,1,"Exits the checkbook system.")
28700   fnacs(sn$,0,mat resp$,ckey)
28800   if ckey=5 or ckey=cancel then goto XIT
28900   if ckey=2 or ckey=8 then addloopcode=1 else addloopcode=0
29000 ! pas=1
29100   if ckey=6 then pas=0: goto SCREEN1
29200   wbc=val(resp$(1)(1:2)) ! working bank(s) code
29300   bn$=resp$(2) ! bank name
29400   bankbalance=val(resp$(3)) ! bank balance
29500   tcde =val(resp$(4)) ! transaction type
29600   tcde$=resp$(5) ! transaction description
29700   transstartdate=val(resp$(6))
29800   transenddate=val(resp$(7))
29900   statementcleareddate =val(resp$(8)) ! cleared date
30000   postingcode$=resp$(9) ! posting code
30100   sourcecode$=resp$(10) ! source code code
30200   transactionstotal=val(resp$(12)) conv TEST_CKEY ! transaction total
30400 TEST_CKEY: ! 
30500   if ckey=3 then typeofentry=tcde
30600   if ckey=2 then 
30700     ti=typeofentry=2
30800     editrec=0
30900     goto ADD
31000   else if ckey=8 then 
31100     ti=typeofentry=1
31200     editrec=0
31300     goto ADD
31400   else if ckey=3 then 
31500     ti=typeofentry=2
31600     allocations_messed_with=false
31700     editrec=val(resp$(11))
31800     goto DO_EDIT
31900   else if ckey=1 then 
32000     goto SCREEN1
32100   else if ckey=6 then 
32200     gosub REINDEX
32300     goto MENU1
32400   end if 
32500   goto MENU1 ! /r only if somehow got through without a valid ckey
32700 ADD: ! r:
32800   editrec=hamt=tr3=posting_code=clr=0
32900   scd=8 : tcde=typeofentry
33000   bank_code=wbc
33100 ! tcde set by add button
33200   tr$(1)=tr$(3)=tr$(4)=tr$(5)='' : tr$(2)=date$("mmddyy")
33300   goto FM_SCREEN ! /r
33500 STANDARD_BREAKDOWN: ! r:
33600 ! pull standard gl breakdowns from payee file
33700 ! uses - tr$(4), tralloc (file), bank_code, tcde, tr$(1)
33800 ! returns - nothing - it's all in the TrAlloc file additions and removal
33900 ! what if allocate 2nd time
34000 ! ** first remove all old allocations for this check
34100   key$=lpad$(str$(bank_code),2)&str$(tcde)&rpad$(tr$(1),8)
34200   restore #h_tralloc,key>=key$: nokey REMOVE_TRALLOC_FOR_KEY_EOF
34300 REMOVE_TRALLOC_FOR_KEY_READ: ! 
34400   read #h_tralloc,using 'Form Pos 1,C 11': newkey$ eof REMOVE_TRALLOC_FOR_KEY_EOF
34500   if newkey$=key$ then 
34600     delete #h_tralloc: 
34700     goto REMOVE_TRALLOC_FOR_KEY_READ
34800   else 
34900     goto REMOVE_TRALLOC_FOR_KEY_EOF
35000   end if 
35100   goto REMOVE_TRALLOC_FOR_KEY_READ
35200 REMOVE_TRALLOC_FOR_KEY_EOF: ! eo first...
35300 ! 
35400 ! 
35500 ! ** next add new allocations that match what they have in their payee file or receipt file (typeofentry=2=reading from receipt file
35600   if typeofentry=2 then 
35700     open #payee:=fngethandle: "Name="&env$('Q')&"\CLmstr\RecMstr.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\recIdx1.h"&env$('cno')&",Shr",internal,input,keyed 
35800     open #payeegl:=fngethandle: "Name="&env$('Q')&"\CLmstr\ReceiptGLBreakdown.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\ReceiptGLBkdIdx.h"&env$('cno')&",Shr",internal,outin,keyed 
35900   else 
36000     open #payee:=fngethandle: "Name="&env$('Q')&"\CLmstr\PayMstr.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\PayIdx1.h"&env$('cno')&",Shr",internal,input,keyed 
36100     open #payeegl:=fngethandle: "Name="&env$('Q')&"\CLmstr\PayeeGLBreakdown.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\PayeeGLBkdIdx.h"&env$('cno')&",Shr",internal,outin,keyed 
36200   end if 
36300 ! 
36400   read #payee,using "form pos 1,c 8",key=lpad$(rtrm$(tr$(4)),8): vn$ nokey XIT_READSTGL
36500   restore #payeegl,key>=vn$: nokey EO_READSTGL
36600   totalalloc=0 : totalamt=0
36700 READ_PAYEEGL: ! 
36800   if val(tr$(3))<>0 then goto GET_TOTAL
36900   mat ml$(3)
37000   ml$(1)='You must enter the transaction amount before'
37100   ml$(2)="you can pull the standard general ledger breakdowns."
37200   fnmsgbox(mat ml$,ok$,cap$,48)
37300   goto EO_READSTGL
37400 GET_TOTAL: ! 
37500   do until totalamt>=val(tr$(3))
37600     read #payeegl,using 'Form Pos 1,C 8,c 12,n 6.2,c 30': payeekey$,payeegl$,percent,gldesc$ eof EO_READSTGL
37700     if vn$<>payeekey$ then goto EO_READSTGL
37800     allocamt=round(val(tr$(3))*percent*.01,2)
37900     totalalloc+=percent
38000     totalamt+=allocamt
38100 ! 
38200     write #h_tralloc,using 'form pos 1,C 11,C 12,PD 5.2,C 30,G 6,X 3,C 12,N 1': key$,payeegl$,allocamt,gldesc$,0,"",0
38300     lastrec=rec(h_tralloc)
38400   loop 
38500 ! 
38600 EO_READSTGL: ! 
38700   if totalamt<>val(tr$(3)) then 
38800     allocamt-=totalamt-val(tr$(3))
38900     rewrite #h_tralloc,using 'Form Pos 24,Pd 5.2',rec=lastrec: allocamt norec ASSIGN_IF_EMPTY
39000 ! plug any rounding differences into last allocation
39100   end if 
39200 ASSIGN_IF_EMPTY: if trim$(tr$(5))="" then tr$(5)=resp$(6)(9:30) ! assign a name if none entered
39300 XIT_READSTGL: ! 
39400   close #payee: 
39500   close #payeegl: 
39600 XIT_STGL: ! 
39700   return  ! /r
39900 DO_EDIT: ! r:
40000   if editrec=0 then goto MENU1
40100   read #h_trmstr(1),using 'Form POS 1,N 2,N 1,C 8,G 6,pd 10.2,C 8,C 35,N 1,N 6,N 1',rec=editrec,reserve: bank_code,tcde,tr$(1),tr$(2),tx3,tr$(4),tr$(5),posting_code,clr,scd ! norec MENU1
40200   tr$(3)=str$(tx3)
40300   ti=3 : ad1=0
40400 ! if posting_code>0 then gosub crgl1
40500   hamt=val(tr$(3)) : hkey$=key$ : tr3=val(tr$(3))
40600   goto FM_SCREEN ! /r
40800 SAVE: ! r:
40900   save_good=false
41000   if editrec>0 then goto EMPTY_BANK_MSG
41100   check_key$=cnvrt$("pic(ZZ)",wbc)&str$(tcde)&lpad$(rtrm$(tr$(1)),8)
41200   read #h_trmstr(1),using 'Form Pos 1,C 11',key=check_key$: newkey$ nokey EMPTY_BANK_MSG
41300   mat ml$(1)
41400   ml$(1)="You already have a transaction with reference # "&trim$(tr$(1))&"."
41500   fnmsgbox(mat ml$,resp$,cap$,0)
41600   tr$(1)=""
41700   goto FM_SCREEN
41800 EMPTY_BANK_MSG: ! 
41900   if bank_code=0 then 
42000     mat ml$(1)
42100     ml$(1)="You must first select a Bank."
42200     fnmsgbox(mat ml$,resp$,cap$,0)
42300     goto EO_SAVE
42400   end if 
42500 ! if trim$(tr$(4))='' and tcde=1 and trim$(uprc$(tr$(5)))<>"VOID" then mat ml$(1)
42600 ! ml$(1)="You must first select a Payee."
42700 ! fnmsgbox(mat ml$,resp$,cap$,0)
42800 ! goto eo_save
42900 ! end if
43000   if allocationstotal=val(tr$(3)) then goto RELEASE_TRMSTR1 ! allow zero checks to go thru as long as the allocations = 0 also
43100   if trim$(tr$(3))='0.00' and trim$(uprc$(tr$(5)))<>"VOID" then 
43200     mat ml$(1)
43300     ml$(1)="You must first enter an amount."
43400     fnmsgbox(mat ml$,resp$,cap$,0)
43500     goto EO_SAVE
43600   end if 
43700 RELEASE_TRMSTR1: release #h_trmstr(1): 
43800   if editrec<>0 then 
43900     read #h_trmstr(1),using 'Form POS 1,N 2,N 1,C 8,Pos 18,PD 10.2,pos 71,n 1',rec=editrec,reserve: oldbank_code,oldtcde,oldtr1$,oldtr3,oldposting_code
44000   end if 
44100 ! 
44200 ! if allocations changed on a posted transaction tell them to update their general ledger
44300   if allocations_messed_with=true and oldposting_code>0 then 
44400     mat ml$(2)
44500     ml$(1)='Your allocations have changed on a posted transaction.'
44600     ml$(2)='You will need to update your General Ledger!'
44700     fnmsgbox(mat ml$,resp$,cap$,0)
44800     allocations_messed_with=false
44900   end if 
45000 ! 
45100 ! save - update bank balance for changed amounts
45200   if editrec=0 then goto L2040 ! adding new record
45300   if oldtr3<>val(tr$(3)) or oldbank_code<>bank_code or tcde<>oldtcde then 
45400     if oldtcde=1 or oldtcde=4 then 
45500       fnupdatebankbal(oldbank_code,+oldtr3)
45600     else 
45700       fnupdatebankbal(oldbank_code,-oldtr3) ! take out the old
45800     end if 
45900 L2040: ! 
46000     if tcde=1 or tcde=4 then 
46100       fnupdatebankbal(bank_code,-val(tr$(3)))
46200     else 
46300       fnupdatebankbal(bank_code,+val(tr$(3))) ! put in the new
46400     end if 
46500   end if 
46600 ! save - update key fields in tralloc
46700   if (oldbank_code<>bank_code or oldtcde<>tcde or oldtr1$<>tr$(1)) and editrec<>0 then 
46800     newkey$=cnvrt$('pic(zz)',bank_code)&str$(tcde)&tr$(1)
46900     oldkey$=cnvrt$('pic(zz)',oldbank_code)&str$(oldtcde)&oldtr1$
47000     restore #h_tralloc,key=oldkey$: nokey EO_UPDATE_TRALLOC_KEYS
47100 READ_TRALLOC_UPDATE: ! 
47200     read #h_tralloc,using 'Form Pos 1,C 11',reserve: readkey$ eof L2130
47300     if oldkey$=readkey$ then 
47400       rewrite #h_tralloc,using 'Form Pos 1,C 11',release: newkey$
47500       goto READ_TRALLOC_UPDATE
47600     else 
47700       release #h_tralloc: 
47800     end if 
47900 L2130: ! 
48000   end if 
48100 EO_UPDATE_TRALLOC_KEYS: ! 
48200 ! 
48300 ! 
48400 ! 
48500 ! 
48600 ! 
48700 ! and finaly, actually save the transaction
48800   tr$(1)=lpad$(trim$(tr$(1)),8)
48900   tx3=val(tr$(3))
49000   if editrec<>0 then 
49100     tr2=val(tr$(2))
49200     rewrite #h_trmstr(1),using 'Form POS 1,N 2,N 1,C 8,G 6,pd 10.2,C 8,C 35,N 1,N 6,N 1',same,reserve: bank_code,tcde,tr$(1),tr2,tx3,tr$(4),tr$(5),posting_code,clr,scd
49300     goto L2260
49400   end if 
49500   check_key$=cnvrt$("pic(ZZ)",wbc)&str$(tcde)&lpad$(rtrm$(tr$(1)),8)
49600   read #h_trmstr(1),using 'Form Pos 1,C 11',key=check_key$: newkey$ nokey L2250
49700   mat ml$(1)
49800   ml$(1)="You already have a transaction with reference # "&trim$(tr$(1))&"."
49900   fnmsgbox(mat ml$,resp$,cap$,0)
50000   tr$(1)=""
50100   goto FM_SCREEN
50200 L2250: ! 
50300   tr2=val(tr$(2)): write #h_trmstr(1),using 'Form POS 1,N 2,N 1,C 8,G 6,pd 10.2,C 8,C 35,N 1,N 6,N 1',reserve: bank_code,tcde,tr$(1),tr2,tx3,tr$(4),tr$(5),posting_code,clr,scd
50400   editrec=rec(h_trmstr(1))
50500 L2260: ! 
50600   if ckey=1 and allocationstotal<>val(tr$(3)) then 
50700     mat ml$(4)
50800     ml$(1)='Allocations do not equal the Check Amount'
50900     ml$(2)='Please correct the Check Amount or '
51000     ml$(3)='the Allocations'
51100     ml$(4)='You are off by '&str$(val(tr$(3))-allocationstotal)
51200     fnmsgbox(mat ml$,yn$,cap$,48)
51300     goto EO_SAVE
51400   end if 
51500   save_good=true
51600 EO_SAVE: ! 
51700   return  ! /r
51900 VOID_TRANSACTION: ! r:
52000 ! uses:    bank_code, tcde, tr$(1), h_tralloc
52100 ! returns: tr$(3) and tr$(5)
52200   tr$(3)='0'
52300   tr$(5)='Void'
52400   key$=cnvrt$('pic(ZZ)',bank_code)&str$(tcde)&lpad$(trim$(tr$(1)),8)
52500   restore #h_tralloc,key>=key$: nokey VOID_EO_TRALLOC
52600 VOID_READ_TRALLOC: ! 
52700   read #h_tralloc,using 'Form Pos 1,C 11,C 12,pd 5.2,C 30,G 6,X 3,C 12,G 1': newkey$,item$(1),tmp,item$(3),item$(4),item$(5),item$(6) eof VOID_EO_TRALLOC
52800   if key$=newkey$ then 
52900     rewrite #h_tralloc,using 'Form Pos 24,PD 5.2,C 30',release: 0,'Void'
53000     goto VOID_READ_TRALLOC
53100   else 
53200     release #h_tralloc: 
53300     goto VOID_EO_TRALLOC
53400   end if 
53500 VOID_EO_TRALLOC: ! 
53600   return  ! /r
53800 DELETE_TRANSACTION: ! r:
53900   fntos(sn$='Trans-Delete')
54000   lc=0 : width=50
54100   fnlbl(lc+=1,1,'Delete Transaction Options',width,center)
54200   ln+=1
54300   fnchk(lc+=1,1,'Update Bank Balance')
54400   resp$(1)="True"
54500   fnchk(lc+=1,1,'Delete Transaction Allocations too')
54600   resp$(2)='True'
54700   fncmdset(2)
54800   fnacs(sn$,0,mat resp$,ckey)
54900   if ckey=5 or ckey=cancel then goto DELETE_TRANSACTION_DONE
55000   updatebankbalance$=resp$(1)
55100   deletetransactionallocation$=resp$(2)
55200   if updatebankbalance$='True' then 
55300     if tcde=1 or tcde=4 then 
55400       fnupdatebankbal(bank_code,+val(tr$(3)))
55500     else 
55600       fnupdatebankbal(bank_code,-val(tr$(3)))
55700 ! take out the old
55800     end if 
55900   end if 
56000   if deletetransactionallocation$='True' then 
56100     key$=cnvrt$('pic(ZZ)',bank_code)&str$(tcde)&lpad$(trim$(tr$(1)),8)
56200     restore #h_tralloc,key>=key$: nokey DONE_DELETETRALLOC
56300 READ_DELETETRALLOC: ! 
56400     read #h_tralloc,using 'Form Pos 1,C 11',reserve: oldkey$ eof DONE_DELETETRALLOC
56500     if oldkey$=key$ then 
56600       delete #h_tralloc,release: 
56700       goto READ_DELETETRALLOC
56800     else 
56900       goto DONE_DELETETRALLOC
57000     end if 
57100 DONE_DELETETRALLOC: ! 
57200     release #h_tralloc: 
57300   end if 
57400   delete #h_trmstr(1),release: 
57500 DELETE_TRANSACTION_DONE: ! 
57600   return  ! /r
57800 ADD_ALLOCATION: ! r:
57900   adding_allocation=1
58000   trabank_code=bank_code : tratcde=tcde
58100   track$=lpad$(trim$(tr$(1)),8) : tradesc$=tr$(5)(1:12)
58200   tragl$=traivd$=trapo$='' : traamt=0 : tragde=posting_code
58300 ! write #h_tralloc,using 'Form Pos 1,N 2,N 1,C 8,C 12,PD 5.2,C 30,G 6,X 3,C 12,N 1',reserve: trabank_code,tratcde,track$,tragl$,traamt,tradesc$,traivd$,trapo$,tragde
58400 ! read #h_tralloc,using 'Form Pos 1,N 2,N 1,C 8,C 12,PD 5.2,C 30,G 6,X 3,C 12,N 1',same,reserve: trabank_code,tratcde,track$,tragl$,traamt,tradesc$,traivd$,trapo$,tragde
58500   return  ! /r
58700 READ_ALLOCATION: ! r: uses allocrec and returns ???
58800   if allocrec=0 then 
58900     track$=lpad$(trim$(tr$(1)),8)
59000   else 
59100     read #h_tralloc,using 'Form Pos 1,N 2,N 1,C 8,C 12,pd 5.2,C 30,G 6,X 3,C 12,N 1',rec=allocrec,reserve: trabank_code,tratcde,track$,tragl$,traamt,tradesc$,traivd$,trapo$,tragde
59200   end if 
59300 L2670: return  ! /r
59500 FM_ALLOCATION: ! r:
59600   allocations_messed_with=true
59700   fntos(sn$='Trans-TrAlloc')
59800   lc=0 : mylen=22 : mypos=mylen+2
59900   fnlbl(lc+=1,1,'Bank:',mylen,right)
60000   fntxt(lc,mypos,2,0,left,number$,disable)
60100   resp$(1)=str$(trabank_code)
60200   fnlbl(lc+=1,1,'Transaction Type:',mylen,right)
60300   fntxt(lc,mypos,1,0,left,number$,disable)
60400   resp$(2)=str$(tratcde)
60500   fnlbl(lc+=1,1,'Check/Reference:',mylen,right)
60600   fntxt(lc,mypos,8,0,right,'',disable)
60700   resp$(3)=track$
60800   fnlbl(lc+=1,1,'General Ledger Number:',mylen,right)
60900   fnqgl(lc,mypos)
61000   resp$(4)=fnrgl$(tragl$)
61100   fnlbl(lc+=1,1,'Amount:',mylen,right)
61200   fntxt(lc,mypos,9,0,right,pointtwo$)
61300   resp$(5)=str$(traamt)
61400   fnlbl(lc+=1,1,'Description:',mylen,right)
61500   fntxt(lc,mypos,30,0,left)
61600   resp$(6)=tradesc$
61700   fnlbl(lc+=1,1,'Reference:',mylen,right)
61800   fntxt(lc,mypos,6,0,left,"",0)
61900   resp$(7)=traivd$ ! the last zero above was disabled, why kj
62000   fnlbl(lc+=1,1,'Purchase Order:',mylen,right)
62100   fntxt(lc,mypos,12,0,left)
62200   resp$(8)=trapo$
62300   fnlbl(lc+=1,1,'Posting Status:',mylen,right)
62400   fntxt(lc,mypos,1,0,left,number$,disable)
62500   resp$(9)=str$(tragde)
62600   fncmdset(4)
62700   fnacs(sn$,0,mat resp$,ckey)
62800   if ckey=5 then goto CANCEL_ALLOC
62900   trabank_code=val(resp$(1))
63000   tratcde=val(resp$(2))
63100   track$=resp$(3)
63200   tragl$=fnagl$(resp$(4))
63300   traamt=val(resp$(5))
63400   tradesc$=resp$(6)
63500   traivd$=resp$(7)
63600   trapo$=resp$(8)
63700   tragde=val(resp$(9))
63800   if ckey=1 then gosub SAVE_ALLOC
63900   if ckey=1 and adding_allocation=1 then 
64000     tragl$=""
64100     traamt=0
64200     goto FM_ALLOCATION
64300 ! add loop
64400   end if 
64500 CANCEL_ALLOC: ! r:
64600   if adding_allocation=1 then 
64700 ! delete #h_tralloc,same:
64800   else 
64900     release #h_tralloc: 
65000   end if 
65100   goto EO_ALLOC ! /r
65200 EO_ALLOC: ! 
65300   adding_allocation=0
65400   return  ! /r
65600 SAVE_ALLOC: ! r:
65700   track$=lpad$(trim$(track$),8)
65800   if adding_allocation=0 then 
65900     rewrite #h_tralloc,using 'form pos 1,N 2,N 1,C 8,C 12,PD 5.2,C 30,G 6,X 3,C 12,N 1',release: trabank_code,tratcde,track$,tragl$,traamt,tradesc$,traivd$,trapo$,tragde
66000   else 
66100     write #h_tralloc,using 'form pos 1,N 2,N 1,C 8,C 12,PD 5.2,C 30,G 6,X 3,C 12,N 1': trabank_code,tratcde,track$,tragl$,traamt,tradesc$,traivd$,trapo$,tragde
66200   end if 
66300   return  ! /r
66500 DEL_ALLOCATION: ! r: uses allocrec
66600   allocations_messed_with=true
66700   delete #h_tralloc,rec=allocrec: norec DEL_ALLOCATION_NOREC
66800   return  ! /r
67000 DEL_ALLOCATION_NOREC: ! r:
67100   mat ml$(3)
67200   ml$(1)='Delete Allocation Error'
67300   ml$(2)="You must select an Allocation Record to delete"
67400   ml$(3)="before clicking the Delete Allocation button."
67500   fnmsgbox(mat ml$,ok$,cap$,48)
67600   continue  ! /r
67800 FM_SCREEN: ! r:
67900   fntos(sn$='transfm2b'&str$(typeofentry))
68000   lc=0 ! line count
68100   fc=0 ! frame count
68200   width=120 ! screen width
68300   fnfra(1,1,10,width,'Transaction Data')
68400   frame=fc+=1
68500   lc=0 : mylen=23 : mypos=mylen+2
68600   fnlbl(lc+=1,1,'Bank:',mylen,right,0,frame)
68700   fncombof('Bank',lc,mypos,0,env$('Q')&"\CLmstr\BankMstr.h"&env$('cno'),1,2,3,30,env$('Q')&"\CLmstr\BankIdx1.h"&env$('cno'),limit_to_list,0,'',frame)
68800   resp$(1)=str$(bank_code)
68900   fnlbl(lc+=1,1,'Transaction Type:',mylen,right,0,frame)
69000 ! fncombof('TransactionType',lc,mypos,0,env$('Q')&'\CLmstr\TransactionType.dat',1,1,2,25,env$('Q')&'\CLmstr\TransactionType.idx',limit_to_list,0,'',frame)
69100 ! resp$(2)=str$(tcde)
69200   fntxt(lc,mypos,28,0,left,'',disable,'',frame)
69300   resp$(2)=str$(tcde)
69400   if wit=1 then 
69500     fnlbl(lc+=1,1,'Check Number:',mylen,right,0,frame)
69600   else 
69700     fnlbl(lc+=1,1,'Reference Number:',mylen,right,0,frame)
69800   end if 
69900   fntxt(lc,mypos,8,0,right,'',0,'',frame)
70000   resp$(3)=tr$(1)
70100   fnlbl(lc+=1,1,'Transaction Date:',mylen,right,0,frame)
70200   fntxt(lc,mypos,8,0,left,mmddyy$,0,'',frame)
70300   resp$(4)=tr$(2)
70400   fnlbl(lc+=1,1,'Transaction Amount:',mylen,right,0,frame)
70500   fntxt(lc,mypos,12,0,right,pointtwo$,0,'',frame)
70600   resp$(5)=cnvrt$("N 10.2",val(tr$(3)))
70700   if typeofentry=2 then 
70800     fnlbl(lc+=1,1,'Receipt Type:',mylen,right,0,frame)
70900     fncombof('ReceiptType',lc,mypos,0,env$('Q')&"\CLmstr\RecMstr.h"&env$('cno'),1,8,9,30,env$('Q')&"\CLmstr\RecIdx1.h"&env$('cno'),limit_to_list,0,'',frame)
71000     resp$(6)=tr$(4)
71100   else 
71200     fnlbl(lc+=1,1,'Payee:',mylen,right,0,frame)
71300     if scd=4 then 
71400       fntxt(lc,mypos,8,0,left,"",0,'Employee # for payroll checksl',frame)
71500       resp$(6)=tr$(4)
71600     else 
71700       fncombof('Payee',lc,mypos,0,env$('Q')&"\CLmstr\PayMstr.h"&env$('cno'),1,8,9,30,env$('Q')&"\CLmstr\PayIdx1.h"&env$('cno'),limit_to_list,0,'',frame)
71800       resp$(6)=tr$(4)
71900     end if  ! scd=4   /   else 
72000   end if  ! typeofentry=2   /   else 
72100   fnlbl(lc+=1,1,'Name/Description:',mylen,right,0,frame)
72200   fntxt(lc,mypos,35,0,left,'',0,'',frame)
72300   resp$(7)=tr$(5)
72400   fnlbl(lc+=1,1,'Posting Status:',mylen,right,0,frame)
72500   fncombof('PostCode',lc,mypos,0,"S:\acsCL\PostingCode.dat",1,1,2,25,"S:\acsCL\PostingCode.idx",limit_to_list,0,'',frame)
72600   resp$(8)=str$(posting_code)
72700   fnlbl(lc+=1,1,'Statement Date Cleared:',mylen,right,0,frame)
72800   fntxt(lc,mypos,8,0,left,mmddyy$,0,'',frame)
72900   resp$(9)=str$(clr)
73000 !  r: the transaction allocation grid
73300   mat chdr$(7) : mat cmask$(7) : mat item$(7)
73310   chdr$(1)='Rec'
73320   chdr$(2)='GL Account'
73330   chdr$(3)='Amount'
73340   chdr$(4)='Description'
73350   chdr$(5)='Invoice'
73360   chdr$(6)='PO Number'
73370   chdr$(7)='PC'
73372   mat cmask$=("")
73380   cmask$(1)='30'
73390   cmask$(2)=''
73400   cmask$(3)='10'
73410   cmask$(5)=''
73900   fnflexinit1('TrAlloc-tran2',16,1,4,90,mat chdr$,mat cmask$,1)
74000   allocationstotal=0
74100 ! fnflexinit1('TrAlloc-'&str$(bank_code)&'-'&str$(tcde)&'-'&trim$(tr$(1)),13,1,7,80,mat chdr$,mat cmask$,1)
74200 ! allocationstotal=0
74300   key$=cnvrt$('pic(ZZ)',bank_code)&str$(tcde)&lpad$(trim$(tr$(1)),8)
74400   restore #h_tralloc,key>=key$: nokey EO_FLEX2
74500 READ_TRALLOC_1: ! 
74600   read #h_tralloc,using 'Form Pos 1,C 11,C 12,pd 5.2,C 30,G 6,X 3,C 12,G 1': newkey$,item$(2),tmp,item$(4),item$(5),item$(6),item$(7) eof EO_FLEX2
74700   if key$<>newkey$ then goto EO_FLEX2
74800   allocationstotal+=tmp
74900   item$(1)=str$(rec(h_tralloc))
75000   item$(3)=str$(tmp)
75100   fnflexadd1(mat item$)
75200   goto READ_TRALLOC_1
75300 EO_FLEX2: ! /r
75400   fnlbl(lc=15,1,'Allocation Total: $'&trim$(cnvrt$("N 15.2",allocationstotal)),40,right)
76100   fnbutton(lc=15,(61),'&Add',8,'Add Allocation')
76200   fnbutton(lc,(61+4+2),'&Edit',7,'Edit Allocation')
76300   fnbutton(lc,(61+4+2+5+2),'&Delete',6,'Delete Allocation')
76400   fnbutton(lc,(61+4+2+5+2+7+2),'&Get Standard G/L Breakdowns',9,'Reset Allocations to those associated with the Payee.')
76500   if typeofentry=2 then 
76600     fnbutton(6,72,'&Receipt Type File',11,'Add or Edit different types or classifications of receipts ',0,0,1)
76700   else 
76800     fnbutton(6,72,'&Payee File',10,'Add or Edit Payees',0,0,1)
76900   end if 
77000   lc+=1
77100   fncmdkey('&Save',1,1,0,"Saves this record and any changes back to the files.")
77200   fncmdkey('&Delete',3,0,0,"Will delete this entry from your files.  You will have an option as to how to effect the bank balance.")
77300   fncmdkey('&Void',4,0,0,"Voids the transaction that is on the screen. It will adjust the bank balance. It leaves a voided transaction on file.")
77400   fncmdkey('&Cancel',5,0,1,"Returns to previous screen without saving any changes.")
77500   fnacs(sn$,0,mat resp$,ckey)
78000   holdtr1$=tr$(1)
78020   if ckey=3 then 
78040     gosub DELETE_TRANSACTION
78060     goto MENU1
78080   else if ckey=5 or ckey=cancel then 
78100     goto MENU1
78120   else if ckey=1 or ckey=4 or ckey=6 or ckey=7 or ckey=8 or ckey=9 then 
78140     bank_code=val(resp$(1)(1:2))
78160     tcde=val(resp$(2)(1:1))
78180     tr$(1)=lpad$(resp$(3),8)
78200     tr$(2)=resp$(4)
78220     tr$(3)=resp$(5)
78240     tr$(4)=lpad$(trim$(resp$(6)(1:8)),8)
78260     tr$(5)=resp$(7)
78280     posting_code=val(resp$(8)(1:1))
78300     clr=val(resp$(9))
78320   end if 
78340   if ckey=1 and allocationstotal<>val(resp$(5)) then 
78360     mat ml$(3)
78380     ml$(1)='Allocations ('&cnvrt$('pic(---,---,--#.##)',allocationstotal)&') do not equal the Check Amount ('&cnvrt$('pic(---,---,--#.##)',val(tr$(3)))&')'
78400     ml$(2)='Please correct the Check Amount or the Allocations'
78420     ml$(3)='You are off by '&str$(val(tr$(3))-allocationstotal)
78440     fnmsgbox(mat ml$,yn$,cap$,48)
78460     goto FM_SCREEN
78480   end if 
78500   if (ckey=1 or ckey=8) and trim$(tr$(1))="" then 
78520     mat ml$(1)
78540     ml$(1)="You must first enter a Reference Number."
78560     fnmsgbox(mat ml$,resp$,cap$,0)
78580     goto FM_SCREEN
78600   end if 
78620   if ckey=6 then 
78640     allocrec=val(resp$(10))
78660     gosub DEL_ALLOCATION
78680     goto FM_SCREEN
78700   end if 
80500   if holdtr1$=tr$(1) and editrec>0 then goto L3780 ! r: duplicate transaction?
80600   check_key$=cnvrt$("pic(ZZ)",wbc)&str$(tcde)&lpad$(rtrm$(tr$(1)),8)
80700   read #h_trmstr(1),using 'Form Pos 1,C 11',key=check_key$: newkey$ nokey L3780
80800   mat ml$(1)
80900   ml$(1)="You already have a transaction with reference # "&trim$(tr$(1))&"."
81000   fnmsgbox(mat ml$,resp$,cap$,0)
81020   tr$(1)=""
81040   goto FM_SCREEN
81060 L3780: ! /r
81080   if ckey=8 then 
81100     gosub ADD_ALLOCATION
81120     gosub FM_ALLOCATION
81140     goto FM_SCREEN
81160   else if ckey=7 then 
81180     allocrec=val(resp$(10))
81200     gosub READ_ALLOCATION
81220     gosub FM_ALLOCATION
81240     goto FM_SCREEN
81260   else if ckey=4 then 
81280     gosub VOID_TRANSACTION
81300     gosub SAVE
81320     goto MENU1
81340   else if ckey=10 then 
81360     fnaddpayee
81380     goto FM_SCREEN
81400   else if ckey=11 then 
81420     fnaddreceipt
81440     goto FM_SCREEN
81460   end if 
81480   gosub SAVE
81500   if save_good=false then goto FM_SCREEN
81520   if ckey=9 then gosub STANDARD_BREAKDOWN : goto FM_SCREEN ! the record must be good save first!!!
81540   if trim$(check_ref$)<>"" then 
81560     check_ref$=""
81580     goto SCREEN1
81600   else if editrec<>0 then ! return to main screen after edit
81620     goto MENU1
81640   end if 
81660   goto ADD ! if resp$(12)=true$ then goto add
81680   goto MENU1 ! /r
84200 REINDEX: ! r: drops deleted records and reindexes trmstr
84300   close #h_trmstr(1): 
84400   close #h_trmstr(2): 
84500   close #h_tralloc: 
84600   fnRemoveDeletedRecords(env$('Q')&"\CLmstr\TrMstr.h"&env$('cno'))
84900   fnIndex(env$('Q')&"\CLmstr\TrMstr.h"&env$('cno'),env$('Q')&"\CLmstr\TrIdx1.h"&env$('cno'),"1 11")
85000   fnIndex(env$('Q')&"\CLmstr\TrMstr.h"&env$('cno'),env$('Q')&"\CLmstr\TrIdx2.h"&env$('cno'),"28/1 8/11")
85100   fnIndex(env$('Q')&"\CLmstr\Tralloc.h"&env$('cno'),env$('Q')&"\CLmstr\Tralloc-idx.h"&env$('cno'),"1 11")
85200   gosub OPEN_TRANSACTION_FILES
85300   return  ! /r
85500 ! <updateable region: ertn>
85600 ERTN: fnerror(program$,err,line,act$,"xit")
85700   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
85800   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
85900   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
86000 ERTN_EXEC_ACT: execute act$ : goto ERTN
86100 ! </updateable region: ertn>
86300 XIT: fnxit
86500 CHAIN_SELCNO: fnchain("S:\Core\programs\Select Company")
