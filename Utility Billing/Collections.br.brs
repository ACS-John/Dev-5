06000 ! (formerly) S:\acsUB\Collections  and before that acsUB\ubIpColl
06020   fn_setup ! r:
06040   fntop(program$) ! for now use the settings from Enter Collections for page formatting of reports
06060   open #h_customer:=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,outin,keyed 
06080   open #11: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\UBIndx2.h"&env$('cno')&",Shr",internal,outin,keyed 
06100   open #4: "Name="&env$('Q')&"\UBmstr\UBTransVB.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\UBTrIndx.h"&env$('cno')&",Shr",internal,outin,keyed 
06120   bud1=0
06140   open #h_budmstr:=fngethandle: "Name="&env$('Q')&"\UBmstr\BudMstr.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\BudIdx1.h"&env$('cno')&",Shr",internal,outin,keyed ioerr L630
06160   open #h_budTrans:=fngethandle: "Name="&env$('Q')&"\UBmstr\BudTrans.h"&env$('cno')&",Shr",internal,outin,relative 
06180   bud1=1
06200 L630: ! 
06220   open #h_ubcolinp:=6: "Name="&collections_filename$,internal,outin,relative ioerr L700
06240   if lrec(h_ubcolinp)<1 then 
06260     mat x=(0)
06280     transType=postingCodeUnused=0
06300   else 
06320     ti1=2 ! edit previous unposted input and make new entries
06340     ! gosub READD
06360   end if 
06380   goto MENU1B
06400 L700: ! 
06420   open #h_ubcolinp:=fngethandle: "Name="&collections_filename$&",RecL=91,Replace", internal,outin,relative 
06440   mat x=(0) : transType=postingCodeUnused=0
06460   goto MENU1B
06480 ! /r
10000 MENU1B: ! r:
10010   do 
10020     fntos(sn$="Collections-menu1") : rc=0
10220     fnflexinit1('Collections',6,1,10,80,mat chdr$,mat cm$,1) ! r: add the grid to the screen
10240     restore #h_ubcolinp: 
10260     totalacct=0 : totalCollections=totalDebitMemos=totalCreditMemos=0
10280     do  ! for j=1 to lrec(h_ubcolinp)
10300       read #h_ubcolinp,using F_ubColInp: x$,transAmount,transDate, transType,postingCodeUnused,rcpt$,mat alloc,mat bd3,escrow eof L1080 norec L1070
10320       totalacct+=val(x$) conv ignore
10360       m1_item$(1)=str$(rec(h_ubcolinp)) ! record
10380       m1_item$(2)=x$                     ! account
10390       m1_item$(3)=str$(transAmount)               ! amount
10392       m1_item$(4)=str$(transDate)               ! date
10400       m1_item$(5)=fn_collType$(transType)
10401       if env$('acsDeveloper')<>'' then m1_item$(5)=m1_item$(5)&' ('&str$(transType)&')'
10402       m1_item$(6)=str$(postingCodeUnused)            ! 
10406       fn_increaseMatR(transType,transAmount,totalCollections,totalDebitMemos,totalCreditMemos)
10420       ! m1_item$(7)=rcpt$
10440       cHdrItem=cHdrItemFirstService=6
10460       for j2=1 to 10
10480         if trim$(srvname$(j2))="" or trim$(srvname$(j2))(1:5)="Reduc" then 
10500           goto L1030
10520         else 
10540           m1_item$(cHdrItem+=1)=str$(alloc(cHdrItem-cHdrItemFirstService))
10560         end if 
10580         L1030: ! 
10600       next j2
10620       if uprc$(escrow$)="Y" then m1_item$(cHdrItem+=1)=str$(escrow)
10640       mat m1_item$(cHdrItem)
10660       fnflexadd1(mat m1_item$)
10680       L1070: ! 
10700     loop  ! next j
10720     L1080: ! /r
10721     resp_selectedRecordNumber=rc+=1 ! resp$(1) returns the record number of the selected entry
10722     fnlbl(1,1,"Total Collections:",22,1)
10724     fnlbl(1,24,cnvrt$('pic($$$$,$$$,$$#.##)',totalCollections),15,1) 
10725     ! fntxt(1,24,15,15,1,"10",1) : resp$(rc+=1)=str$(totalCollections)
10726     fnlbl(2,1,"Total Credit Memos:",22,1)
10727     fnlbl(2,24,cnvrt$('pic($$$$,$$$,$$#.##)',totalCreditMemos),15,1) 
10728     ! fntxt(2,24,15,15,1,"10",1) : resp$(rc+=1)=str$(totalCreditMemos)
10730     fnlbl(3,1,"Total Debit Memos:",22,1)
10731     fnlbl(3,24,cnvrt$('pic($$$$,$$$,$$#.##)',totalDebitMemos),15,1) 
10732     ! fntxt(3,24,15,15,1,"10",1) : resp$(rc+=1)=str$(totalDebitMemos)
10740     fnlbl(4,1,"Total Account Numbers:",22,1)
10750     fnlbl(4,24,str$(totalacct),15,1) 
10760     ! fntxt(4,24,15,15,1,"10",1) : resp$(rc+=1)=str$(totalacct)
10780     if lrec(h_ubcolinp)<1 then 
10800       fncmdkey("Add &Transaction",2,1,0,"Allows you to enter transactions.")
10820       fncmdkey("&Close",5,0,1,"Returns to menu.")
10840     else 
10860       fncmdkey("&Add",2)
10880       fncmdkey("E&dit",1,1)
10890       fncmdkey("Delete",4)
10900       fncmdkey("&Print",3)
10940       fncmdkey("&Close",5,0,1,'Return to main menu.\nEntries will not be lost nor will they be posted to accounts.')
10960       fncmdkey("&Post",10,0,0,'Apply collections to customer records.')
10980       fncmdkey("Open Drawer",12,0,0,'Open an attached cash drawer')
11000     end if 
11002 !   if fnclient_has('U5') then
11004     fncmdkey("Import CSV",14,0,0,'Import Collections CSV')
11008 !   end if
11020     fnacs(sn$,0,mat resp$,ck1)
11030     if ck1=5 then 
11040       goto XIT
11060     else 
11080       edrec=val(resp$(resp_selectedRecordNumber))
11100       if ck1=1 then 
11110         editmode=1
11120         goto EDIT_REC
11130       else if ck1=2 then 
11140         editmode=0
11150         goto ADD_REC
11160       else if ck1=4 then 
11180 !       read #h_ubcolinp,using F_ubColInp,rec=edrec: x$,amount,transDate,transType,postingCodeUnused,rcpt$,mat alloc,mat bd3,escrow
11190         delete #h_ubcolinp,rec=edrec: ioerr ignore
11220       else if ck1=3 then 
11240         fn_print_listings
11260       else if ck1=10 then 
11280         goto SCREEN_LAST_CHANCE
11300       else if ck1=12 then 
11320         fn_open_cash_drawer
11340       else if ck1=14 then 
11360         fn_csv_import
11440       end if 
11460     end if
11480   loop 
11500 ! /r
12000 ADD_REC: ! r:
12020   b7=transType=ti1
12040   p$=" "
12060   rcpt$=""
12080   transType=b7
12100   goto SCREEN_SELECT_ACCOUNT ! /r
14000 SCREEN_SELECT_ACCOUNT: ! r:
14020   if fnask_account('Collections',z$,h_customer)=5 then 
14040     goto MENU1B
14060   end if 
14080   coramt=0
14100   let x1$=z$
14120 ! r: read selected account and prepare data for SCREEN_ADD
14140   read #h_customer,using 'Form Pos 41,C 28,Pos 292,PD 4.2,PD 4,Pos 388,10*PD 5.2,pos 1859,pd 5.2',key=x1$,release: nam$,bal,db1,mat gb,escrowbal nokey SCREEN_SELECT_ACCOUNT
14160   havebudget=0 : mat tgb=(0)
14180   j2=0: escrow=0
14200   for j=1 to 10
14220     if trim$(srvname$(j))<>"" and trim$(srvname$(j)(1:5))<>"Reduc" then tgb(j2+=1)=gb(j)
14240   next j
14260   if uprc$(escrow$)="Y" and transType=3 then oldescrowbal=escrowbal ! add escrow balance into last allocation if have escrow and processing a collection transaction
14280 ! /r
14300   goto SCREEN_ADD ! /r
16000 SCREEN_ADD: ! r:
16020 ! 
16040   if x(3)=0 then let x(3)=date('mmddyy') ! date should default to today
16060   let x(2)=0 ! amount collected should default to zero
16080 ! 
16100   fntos(sn$="ipcollAddv2")
16120   respc=0
16140 ! 
16160   fnlbl(3,1,"Amount:",25,1)
16180   fntxt(3,27,8,0,0,"32")
16200   if ~do_not_blank_rcpt then resp$(respc:=1)=cnvrt$("N 10.2",max(0,bal)) ! str$(x(2))
16220 ! 
16240   fnlbl(4,1,"Date (mmddyy):",25,1)
16260   fntxt(4,27,8,0,0,"1001")
16280   if ~do_not_blank_rcpt then resp$(respc:=2)=str$(x(3))
16300 ! 
16320   fnlbl(5,1,"Receipt Number (CA=Cash):",25,1)
16340   fnbutton(5,40,"Cash",7,"(F7) Set Receipt Number to CA (for Cash)")
16360   fntxt(5,27,9)
16380 ! 
16400   fnlbl(1,1,"Entry Type:",25,1)
16420   if ~do_not_blank_rcpt then resp$(respc:=3)=""
16440   fncomboa("coll_type_rdc",1,27,mat coll_type_option$)
16460   if ~do_not_blank_rcpt then 
16480     for a=1 to 3
16500       if hresp1$=coll_type_option$(a) then resp$(respc:=4)=coll_type_option$(a): verify=1
16520     next a
16540     if verify=0 then resp$(respc:=4)=coll_type_option$(1)
16560   end if 
16580 ! 
16600   fnlbl(2,1,"Account:",25,1)
16620   fntxt(2,27,10,10,1,"",1,"Account (Press Cancel to Re-Select)")
16640   if ~do_not_blank_rcpt then resp$(respc:=5)=z$
16660 ! 
16680   col3_pos=50 : col4_pos=76
16700 ! 
16720   fnlbl(1,col3_pos,"Name:",25,1)
16740   fntxt(1,col4_pos,30,30,0,"",1,"Account Name (Press Cancel to Re-Select)")
16760   if ~do_not_blank_rcpt then resp$(respc:=6)=trim$(nam$)
16780 ! 
16800   fnlbl(2,col3_pos,"Balance:",25,1)
16820   fntxt(2,col4_pos,10,10,1,"",1,"Account Balance (Press Cancel to Re-Select)")
16840   if ~do_not_blank_rcpt then resp$(respc:=7)=cnvrt$("N 10.2",bal)
16860 ! 
16880   fnlbl(3,col3_pos,"Billed:",25,1)
16900   fntxt(3,col4_pos,8,8,1,"1",1)
16920   resp$(respc:=8)=str$(db1)
16940 ! 
16960   if uprc$(escrow$)="Y" then 
16980     fnlbl(4,col3_pos,"Escrow Balance:",25,1)
17000     fntxt(4,col4_pos,10,10,1,"10",1)
17020     if ~do_not_blank_rcpt then resp$(respc:=9)=str$(escrowbal)
17040   end if 
17060   fncmdkey("&Next",1,1,0,"Complete with this entry.  Move to next record.")
17080   fncmdkey("&Review Customer Record",8,0,0,"Allows you to review any customer record.")
17100   fncmdkey("&Notes",3,0,0,"Customer Notes")
17120   fncmdkey("&Back",2,0,0,"Back up one screen. Select a different customer.")
17140   fncmdkey("&Cancel",5,0,1,"Return to proof total screen.")
17160   fnacs(sn$,0,mat resp$,ckey,1)
17180 ! 
17200   do_not_blank_rcpt=0
17220 ! 
17240 ! 
17260   if ckey=2 then ! 2=back
17280     goto SCREEN_SELECT_ACCOUNT
17300   else if ckey=5 then 
17320     goto MENU1B ! 5=cancel
17340   end if 
17360   let x(2)=val(resp$(1))
17380   let x(3)=val(resp$(2))
17400   rcpt$=trim$(resp$(3))(1:9)
17420   if ckey=8 then 
17440     fncustomer(x)
17460     goto SCREEN_ADD
17480   else if ckey=3 then
17500     fnCustomerNotes(z$)
17520     goto SCREEN_ADD
17540   end if 
17542   transType=fn_oSub1(resp$(4))
17544   hresp1$=fn_collType$(transType)
17560   ! if resp$(4)=coll_type_option$(1) then 
17580   !   transType=3 : hresp1$=coll_type_option$(1)
17600   ! else if resp$(4)=coll_type_option$(2) then 
17620   !   transType=4 : hresp1$=coll_type_option$(2)
17640   ! else if resp$(4)=coll_type_option$(3) then 
17660   !   transType=5 : hresp1$=coll_type_option$(3)
17680   ! end if 
17700   let x1$=lpad$(trim$(z$),10)
17720 ! 
17740   if ckey=7 then 
17760     resp$(3)=rcpt$='CA'
17780     do_not_blank_rcpt=1
17800     goto SCREEN_ADD
17820   end if 
17840   if x(3)=0 then 
17860     mat ml$(2)
17880     ml$(1)="Blank Date Detected."
17900     ml$(2)="Please correct the date."
17920     fnmsgbox(mat ml$,resp$)
17940     goto SCREEN_ADD
17960   end if 
17980 ! 
18000   if days(x(3),'mmddyy')>days(date) or days(x(3),'mmddyy')<days(date)-7 then ! warning if collection date greater than to today's date of less that one week ago
18020     if holdbaddate<>x(3) then ! had warning on same date, don't ask again
18040       mat mesg$(3)
18060       mesg$(1)="The collection date of "&resp$(2)&" appears "
18080       mesg$(2)="to be wrong!"
18100       mesg$(3)="Enter Yes to correct, else No to proceed."
18120       fnmsgbox(mat mesg$,resp$,'',52)
18140       holdbaddate=x(3)
18160       if resp$="Yes" then goto SCREEN_ADD
18180     end if 
18200   end if 
18220 ! 
18240   if x(2)<=0 then 
18260     mat mesg$(1)
18280     mesg$(1)="Negative amounts are not allowed."
18320     fnmsgbox(mat mesg$)
18340     goto SCREEN_ADD
18360   end if 
18380   if uprc$(receipt$)="Y" and trim$(rcpt$)="" then 
18390     mat mesg$(6)
18400     mesg$(1)="<<<<<   NO RECEIPT # ENTERED   >>>>>!"
18420     mesg$(2)="You have indicated in the company information"
18440     mesg$(3)="file that you require receipt numbers. You must"
18460     mesg$(4)="either enter a receipt # or change the option to"
18480     mesg$(5)="prevent getting this message."
18500     mesg$(6)="Take OK to continue."
18520     fnmsgbox(mat mesg$)
18540     goto SCREEN_ADD
18560   end if 
18580   if ckey=1 then 
18600     fn_print_receipt(z$,nam$,rcpt$,bal,x(2),x(3),hresp1$)
18620   end if 
18640 ! /r
20000 ! r: after SCREEN_ADD - actually do the adding stuff
20020   if ti1=3 then goto L1980
20040   if sum(tgb)=x(2) then goto L2020
20060   if uprc$(escrow$)="Y" then gosub CHECK_ESCROW ! check escrow balance
20080   mat hgb=tgb
20100 ! Check for previous months
20120   mat tgb=(0)
20140   restore #4,key>=z$&"         ": nokey L1960
20160 L1800: read #4,using L1810: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof L1960
20180 L1810: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
20200   if p$<>z$ then goto L1960
20220   if tcode<1 or tcode>2 then goto L1800 ! only allow charge and penalty trans to flow thru
20240   mat tgb=(0)
20260   j2=0
20280   for j1=1 to 10
20300     if tcode=1 and penalty$(j1)="Y" then ! add penalties up seperate
20320       pgb(j2+=1)=tg(j1)
20340     else if trim$(srvname$(j1))<>"" and trim$(srvname$(j1)(1:5))<>"Reduc" then 
20360       tgb(j2+=1)=tg(j1)
20380     end if 
20400   next j1
20420   if sum(tgb)=x(2) then goto L2020
20440   if sum(tgb)+sum(pgb)=x(2) then ! test with penalties added in
20460     for x=1 to udim(tgb)
20480       tgb(x)+=pgb(x)
20500     next x
20520     goto L2020
20540   end if 
20560 ! L1950: !
20580   goto L1800
20600 L1960: ! 
20620   mat tgb=hgb
20640   gosub BUD2
20660   gosub BUD3
20680 L1980: ! 
20700   if ~fn_breakdown(h_customer,h_budmstr,x1$,havebudget, mat tgb, mat alloc,mat baorder,coramt,ckey) then goto SCREEN_SELECT_ACCOUNT
20720   if ckey=2 then 
20740     goto SCREEN_ADD
20760   else if coramt=1 then 
20780     goto SCREEN_SELECT_ACCOUNT
20800   else 
20820     goto L2040
20840   end if 
20860 ! 
20880 L2020: ! 
20900   items=sz1 ! If UPRC$(ESCROW$)="Y" Then iTEMS=SZ1-1 Else iTEMS = SZ1
20920   for j=1 to items : alloc(j)=tgb(j) : next j
20940   L2040: ! 
20980   transAmount=x(2) : transDate=x(3) : b7=transType
21000   postingCodeUnused=0
21020 L2060: if sum(tgb)=x(2) then gosub BUD2 ! kj 10/14/09
21040   if sum(tgb)=x(2) and bud1=1 then gosub BUD3 ! was commented out; changed to if sum= on 101409 to keep from skipping ubdget update if exact amount paid.
21060   r6=lrec(h_ubcolinp)+1
21080   if escrow>90000 then escrow=0 ! PREVENT 726 ERROR
21100   write #h_ubcolinp,using F_ubColInp,rec=r6: z$,transAmount,transDate,transType,postingCodeUnused,rcpt$,mat alloc,mat bd2,escrow duprec L2060
21120 ! oldn=transDate
21140   transType=b7
21160   goto SCREEN_SELECT_ACCOUNT ! /r
22000 SCREEN_LAST_CHANCE: ! r:
22010   mat mesg$(6)
22020   mesg$(1)="This is your last chance!"
22040   mesg$(2)=""
22060   mesg$(3)="Do you want to pr a"
22080   mesg$(4)="Cash Receipts Journal"
22100   mesg$(5)="or a "
22120   mesg$(6)="Deposit List?"
22140   fnmsgbox(mat mesg$,resp$,'',52)
22160   if uprc$(trim$(resp$))=uprc$("YES") then let fn_print_listings
22180   goto MERGE ! /r
24000 EDIT_REC: ! r:
24040   read #h_ubcolinp,using F_ubColInp,rec=edrec: x$,transAmount,transDate,transType,postingCodeUnused,rcpt$,mat alloc,mat bd3,escrow norec MENU1B
24080   nam$=""
24100   read #h_customer,using 'Form Pos 41,C 28,Pos 292,PD 4.2,PD 4,Pos 388,10*PD 5.2,pos 1859,pd 5.2',key=x$,release: nam$,bal,db1,mat gb,escrowbal nokey ignore
24120   fntos("Collections-edit")
24140   respc=0
24160 ! 
24180   fnlbl(1,1,"Entry Type:",25,1)
24200   fncomboa("rdc",1,27,mat coll_type_option$)
24210   resp$(resp_CollType:=respc+=1)=fn_collType$(transType)
24220   ! if transType=3 then 
24240   !   resp$(resp_CollType:=respc+=1)=coll_type_option$(1)
24260   ! else if transType=4 then 
24280   !   resp$(resp_CollType:=respc+=1)=coll_type_option$(2)
24300   ! else if transType=5 then 
24320   !   resp$(resp_CollType:=respc+=1)=coll_type_option$(3)
24340   ! end if 
24360 ! 
24380   fnlbl(2,1,"Account:",25,1)
24400 ! read #h_customer,using "Form POS 41,C 28",key=x$,release: nam$ nokey IGNORE ! <--  it's already read at the top of the screen
24420   fncmbact(2,27)
24440   resp$(resp_account:=respc+=1)=x$&"  "&nam$
24460 ! 
24480   if uprc$(escrow$)="Y" then transAmount=transAmount+escrow: escrow=0 ! .   ! .    ! add escrow amount back into payment amount before edit
24500 ! 
24520   fnlbl(3,1,"Amount:",25,1)
24540   fntxt(3,27,8,0,0,"10")
24560   resp$(resp_amount:=respc+=1)=str$(transAmount)
24580 ! 
24600   fnlbl(4,1,"Date (mmddyy):",25,1)
24620   fntxt(4,27,8,0,0,"1")
24640   resp$(resp_transDate:=respc+=1)=str$(transDate)
24660 ! 
24680   fnlbl(5,1,"Receipt # (CA=Cash):",25,1)
24700   fntxt(5,27,9)
24720   resp$(resp_receiptNumber:=respc+=1)=rcpt$
24740 ! 
24760   fnlbl(3,40,"Balance:",25,1)
24780   fntxt(3,66,12,12,1,"10",1,"Account Balance (Press Cancel to Re-Select)")
24800   resp$(respc+=1)=cnvrt$("N 12.2",bal)
24820 ! 
24840   fnlbl(4,40,"Billed:",25,1)
24860   fntxt(4,66,8,8,1,"1",1)
24880   resp$(respc+=1)=str$(db1) 
24900 ! 
24920   fncmdkey("&Save",1,1,0,"Saves any changes")
24940   fncmdkey("&Edit",2,0,0,"Allows you to change the breakdown")
24960   fncmdkey("&Delete",4,0,0,"Deletes this collection record")
24980   fncmdkey("&Cancel",5,0,1,"Returns to main collection screen")
25000   fnacs(sn$,0,mat resp$,ckey)
25020 ! 
25040   if ckey=5 then goto L2590
25060 ! If CKEY=2 Then Goto X
25080   if ckey=4 then 
25100     delete #h_ubcolinp,rec=edrec: 
25120     goto MENU1B
25140   end if 
25160   ! 
25162   transType=fn_oSub1(resp$(resp_CollType)) : hresp1$
25164   hresp1$=resp$(resp_CollType)
25166   ! 
25180   !   if resp$(resp_CollType)=coll_type_option$(1) then 
25200   !     transType=3 : hresp1$=coll_type_option$(1)
25220   !   else if resp$(resp_CollType)=coll_type_option$(2) then 
25240   !     transType=4 : hresp1$=coll_type_option$(2)
25260   !   else if resp$(resp_CollType)=coll_type_option$(3) then 
25280   !     transType=5 : hresp1$=coll_type_option$(3)
25300   !      ! 1="Regular Collection",transType=3
25320   !      ! 2="Credit Memo",transType=4
25340   !      ! 3="Debit Memo",transType=5
25360   !   end if 
25380   let x$=x1$=lpad$(trim$(resp$(resp_account)(1:10)),10)
25400   transAmount=x(2)=val(resp$(resp_amount))
25420   transDate=val(resp$(resp_transDate))
25440   rcpt$=trim$(resp$(resp_receiptNumber))
25460   if uprc$(escrow$)="Y" then gosub CHECK_ESCROW ! check escrow balance
25480   if ~fn_breakdown(h_customer,h_budmstr,x1$,havebudget, mat tgb, mat alloc,mat baorder,coramt,ckey) then goto SCREEN_SELECT_ACCOUNT
25500   if uprc$(escrow$)="Y" then transAmount=transAmount-escrow ! .   ! .    ! subtract escrow amount from  payment amount before rewriting
25520   rewrite #h_ubcolinp,using F_ubColInp,rec=edrec: x$,transAmount,transDate,transType,postingCodeUnused,rcpt$,mat alloc,mat bd3,escrow
25540 ! If BUD1=1 Then Gosub BUD2 : Gosub BUD3
25560 L2590: ! 
25580   ! fn_increaseMatR(transType,transAmount,totalCollections,totalDebitMemos,totalCreditMemos)
25600   fn_print_receipt(x$,nam$,rcpt$,bal,x(2),x(3),hresp1$)
25620 goto MENU1B ! /r
26000 MERGE: ! r:
26020   r6=0
26040 MERGE_LOOP_TOP: r6+=1
26060   if r6>lrec(h_ubcolinp) then goto MERGE_FINIS ! prevent stopping to deleted record and quit when finished
26080   read #h_ubcolinp,using F_ubColInp,rec=r6: p$,transAmount,transDate,transType,postingCodeUnused,rcpt$,mat alloc,mat bd3,escrow norec MERGE_LOOP_TOP
26100   if p$(1:2)="  " and transAmount=0 and escrow=0 then goto MERGE_LOOP_TOP
26120   read #h_customer,using 'Form POS 292,PD 4.2,POS 388,10*PD 5.2,pos 1859,pd 5.2',key=p$: bal,mat gb,escrowbal nokey MERGE_LOOP_TOP
26140 ! eSCROW=0   ken 52505
26160   if transType=3 then tcode=3 ! collection
26180   if transType=4 then tcode=4 ! credit memo
26200   if transType=5 then tcode=5 ! debit memo
26220   if transType=5 then bal+=transAmount else bal-=transAmount
26240   tmp=fndate_mmddyy_to_ccyymmdd(transDate)
26260   mat tg=(0): let x=0
26280   for j=1 to 10
26300     if trim$(srvname$(j))<>"" and trim$(srvname$(j)(1:5))<>"Reduc" then tg(j)=alloc(x+=1)
26320   next j
26340   write #4,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1': p$,tmp,tcode,transAmount,mat tg,0,0,0,0,0,0,bal,pcode
26360   if uprc$(escrow$)="Y" and escrow<>0 then 
26380     transAmount=escrow
26400     mat tg=(0)
26420     write #4,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1': p$,tmp,tcode,transAmount,mat tg,0,0,0,0,0,0,bal,pcode ! write a history record for escrow amount
26440   end if 
26460   j2=0
26480   for j=1 to 10
26500     if trim$(srvname$(j))="" or trim$(srvname$(j)(1:5))="Reduc" then goto L4470
26520     j2=j2+1
26540     if transType=5 then let gb(j)=gb(j)+alloc(j2) else let gb(j)=gb(j)-alloc(j2)
26560 L4470: next j
26580   rewrite #h_customer,using 'Form POS 292,PD 4.2,POS 388,10*PD 5.2,pos 1859,pd 5.2',key=p$: bal,mat gb,escrowbal+escrow
26600 ! postingCodeUnused=9
26620   delete #h_ubcolinp,rec=r6: ! rewrite #h_ubcolinp,using "Form POS 19,2*N 1",rec=r6: transType,postingCodeUnused
26640   goto MERGE_LOOP_TOP
26660 MERGE_FINIS: ! 
26680   close #h_ubcolinp,free: 
26700   close #h_customer: 
26720   close #4: 
26740   close #h_ubcolinp: ioerr ignore
26760   goto XIT ! /r
27000 XIT: fnxit
27020 IGNORE: continue 
28000 BUD2: ! r: requires x1$
28020   havebudget=0
28040   bd1=0 : mat bd1(5) : mat bd1=(0) : mat bd2=(0) : mat bd3=(0)
28060   if bud1=0 then goto L5080
28080   read #h_budmstr,using 'Form POS 1,C 10,PD 4,12*PD 5.2,2*PD 3',key=x1$: z$,mat ba,mat badr nokey L5080
28100   ta1=badr(1)
28120 L4820: if ta1=0 then goto L4900
28140   read #h_budTrans,using 'Form POS 1,C 10,2*PD 4,24*PD 5.2,2*PD 4,PD 3',rec=ta1: z$,mat bt1,nba norec L4900
28160   if bt1(14,1)>0 and bt1(14,1)<>transDate then goto L4890
28180   if bt1(14,1)=transDate then bt1(14,1)=bt1(14,2)=0 : rewrite #h_budTrans,using "Form POS 11,2*PD 4,24*PD 5.2,2*PD 4",rec=ta1: mat bt1
28200   bd1+=1 ! 7/06/05  KJ
28220   if bd1=>5 then goto L4900 ! 7/06/05 kj
28240   bd1(bd1)=bt1(1,2) : bd2(bd1)=ta1
28260 L4890: ta1=nba : goto L4820
28280 L4900: if bd1=0 then goto L5080
28300   if bd1(1)>0 and bd1(2)=0 then bd3(1)=bd1(1): goto L5030
28320   mat bd1(bd1)
28340 ! MATCH_BUDGET_BILLING: !
28360   fntos(sn$="Collections-budget")
28380   fnlbl(2,1,"Check the bills to be paid:",30,0)
28400   for j=1 to udim(bd1)
28420     if bd1(j)=0 then goto L4990
28440     fnchk(j+2,10,cnvrt$("pic(zz/zz/zz)",bd1(j)),0)
28460 L4990: next j
28480   fncmdset(2)
28500   fnacs(sn$,0,mat resp$,ck1)
28520   if ck1=5 then goto L5080 ! 7/06/05  KJ
28540 L5030: for j=1 to 5
28560     if uprc$(resp$(j))=uprc$("True") then bd3(j)=1
28580     if bd3(j)=0 then bd2(j)=0
28600   next j
28620   if sum(bd2)=0 then goto BUD2
28640 L5080: return  ! /r
30000 BUD3: ! r:
30020   mat tgb=(0): mat pgb=(0): mat bt1=(0)
30040   for j=1 to 5
30060     if bd3(j)<>0 then rewrite #h_budTrans,using "Form POS 139,2*PD 4",rec=bd2(j): x(3),x(3)
30080     if bd3(j)<>0 then read #h_budTrans,using 'Form POS 1,C 10,2*PD 4,24*PD 5.2,2*PD 4,PD 3',rec=bd2(j): z$,mat bt1,nba
30100     let x2=0
30120     for j3=1 to 10
30140       if srvname$(j3)<>"" then 
30160         if penalty$(j3)="Y" then  ! add penalties up seperat
30180           pgb(x2+=1)=bt1(j3+1,1)
30200         else
30220           if bt1(j3+1,1)>0 then tgb(x2+=1)=bt1(j3+1,1)
30240         end if
30260       end if
30280     next j3
30300     if sum(tgb)=x(2) then 
30320       goto L5280
30340     else if sum(tgb)+sum(pgb)=x(2) then 
30360       goto L5230 
30380     else 
30400       goto L5260
30420     end if
30440     L5230: !
30460     for x=1 to udim(tgb)
30480       tgb(x)+=pgb(x)
30500     next x
30520     L5260: !
30540   next j
30560   mat tgb=(0) ! no matches found
30580   L5280: !
30600   if sum(tgb)>0 then havebudget=1 else havebudget=0
30620 return  ! /r
32000 ! <Updateable Region: ERTN>
32020 ERTN: fnerror(program$,err,line,act$,"xit")
32040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
32060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
32080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
32100 ERTN_EXEC_ACT: execute act$ : goto ERTN
32120 ! /region
34000 CHECK_ESCROW: ! r:
34020   escrow=0
34040   a2=a1=0
34060   tg=0
34080   for j=1 to 10
34100     tg=tg+gb(j)
34120   next j
34140   a2=0
34160   if transType><3 then goto L5710
34180   if x(2)=tg then goto L5710
34200   a1=tg-x(2)
34220   if escrowbal>=a1 then a2=a1 else a2=escrowbal
34240   let x(2)=x(2)+a2
34260   escrow=-a2
34280   L5710: !
34300 return  ! /r
36000 def fn_print_listings
36020   let x$=cnvrt$("pic(######)",transDate)
36060   ! fntos(sn$="Collections-print2")
36080   ! respc=0
36100   ! fnfra(1,2,3,37,"Report Type")
36120   ! fnopt(1,1,"Receipt Listing",0,1)
36140   ! resp$(1)="False"
36160   ! fnopt(2,1,"Deposit Listing",0,1)
36180   ! resp$(2)="False"
36182   ! fnopt(3,1,"Both Receipt and Deposit Listing",0,1)
36184   ! resp$(3)="True"
36186    ! fnlbl(6,1,"Sort Order:",20,1)
36190   ! opt2$(1)="Entry Order"
36300   ! opt2$(2)="Account"
36320   ! mat opt2$(2)
36340   ! fncomboa("Collections_report_so",6,22,mat opt2$)
36360   ! resp$(4)=opt2$(1)
36380   ! fncmdset(3)
36400   ! fnacs(sn$,0,mat resp$,ck1)
36420   ! if ck1=5 then goto MENU1B
36440   if ub_collDisableDepositList$='True' then  ! if uprc$(resp$(1))=uprc$("True") then
36460    ti1=1
36480    ! else if uprc$(resp$(2))=uprc$("True") then
36500    !   ti1=2
36502   else ! else if uprc$(resp$(3))=uprc$("True") then
36504     ti1=3
36520   end if
36540   reportdate$=date$('month, d, ccyy')
36560   if ub_collPrintInAccountOrder$='False' then ! if resp$(4)=opt2$(1) then
36580     srt=1
36600   else ! else if resp$(4)=opt2$(2) then
36620    srt=2
36640   end if
38020   if ti1=1 then 
38040     fnopenprn(0,0,0,0,"Receipt Listing") : ti1_start=1 : ti1_end=1
38060   else if ti1=2 then 
38080     fnopenprn(0,0,0,0,"Deposit Listing") : ti1_start=2 : ti1_end=2
38100   else if ti1=3 then 
38120     fnopenprn(0,0,0,0,"Receipt and Deposit Listing") : ti1_start=1 : ti1_end=2
38140   end if 
38160   mat ct=(0): mat brk=(0)
40000   if srt=2 then 
40020     ! r: Create Sort for UBIPCOLINP
40040     close #h_ubcolinp: ioerr ignore
40060     execute 'Free "'&env$('temp')&'\acs\collections_sort_address_s'&session$&'.int" -n' ioerr ignore
40080     open #h_control:=fngethandle: "Name="&env$('temp')&"\acs\collections_sort_control_s"&session$&".int,RecL=128,Replace", internal,output 
40100     write #h_control,using 'Form POS 1,C 128': "File "&collections_filename$&",,,"&env$('temp')&'\acs\collections_sort_address_s'&session$&'.int,,,,,A,N'
40120     write #h_control,using 'Form POS 1,C 128': "Mask 1,11,C,A"
40140     close #h_control: 
40160     execute "SORT "&env$('temp')&"\acs\collections_sort_control_s"&session$&".int -n"
40180     open #h_ubcolinp:=fngethandle: "Name="&collections_filename$,internal,outin,relative 
40200     open #h_addr:=fngethandle: "Name="&env$('temp')&'\acs\collections_sort_address_s'&session$&'.int',internal,outin 
40220     ! /r
40240   end if 
41000   for ti1=ti1_start to ti1_end
41020     p2=0
41040     r5=0 : totcheck=totcash=totescrow=0 : mat totalByService=(0) : totalCollections=totalDebitMemos=totalCreditMemos=0
41060     if srt=2 then
41080       restore #h_addr:
41100     end if
41120     gosub HEADER
42070     do 
42080       PS_LOOP_TOP: ! r:
42100       if srt=2 then 
42120         read #h_addr,using "Form POS 1,PD 3": r5 eof PS_TOTALS
42140       else 
42160         r5+=1
42180       end if 
42200       if r5>lrec(h_ubcolinp) then goto PS_TOTALS
42220       read #h_ubcolinp,using F_ubColInp,rec=r5: x$,transAmount,transDate,transType,postingCodeUnused,rcpt$,mat alloc,mat bd3,escrow conv PS_TOTALS norec PS_LOOP_TOP
42240       nam$=""
42260       read #h_customer,using "Form POS 41,C 28,pos 143,pd 2",key=x$,release: nam$,wcode nokey ignore
42280       if trim$(x$)="" or (transAmount=0 and escrow=0) then goto PS_LOOP_TOP
44000       if ti1=2 and uprc$(ltrm$(rtrm$(rcpt$)))(1:2)="CA" then 
44020         totcash+=transAmount
44040         goto PS_LOOP_TOP ! Skip CASH RECEIPTS ON DEPOSIT LIST
44060       end if 
44080       if ti1=2 and transType<>3 then goto PS_LOOP_TOP ! Skip all entries except receipts.
44100       totcheck+=transAmount
44140       fn_increaseMatR(transType,transAmount,totalCollections,totalDebitMemos,totalCreditMemos)
44280       if env$('client')="Ash Grove" and (wcode<1 or wcode>10) then let wcode=1
44300       if env$('client')="Ash Grove" and transType=3 then let water(wcode)=water(wcode)+alloc(1) ! TOTAL WATER BY WATER CODE
44320       for j=1 to sz1
44340         totalByService(j)+=alloc(j)
44360         brk(transType,j)=brk(transType,j)+alloc(j)
44380       next j
44400       if transType=3 or transAmount=0 then c$=" "
44420       if transType=5 then c$="Dm"
44440       if transType=4 then c$="Cm"
44460       ! 
44480       if ti1=1 then 
44500         if uprc$(escrow$)<>"Y" then 
44520           pr #255,using L3130: r5,x$,transAmount,c$,transDate,rcpt$,mat alloc,nam$ pageoflow PGOF
44540           L3130:    form pos 1,n 4,x 2,c 10,n 10.2,c 4,pic(zz/zz/zz),x 2,c 9,sz1*n 8.2,x 3,c 30
44560         else 
44580           pr #255,using L3090: r5,x$,transAmount,c$,transDate,rcpt$,mat alloc,escrow,nam$ pageoflow PGOF
44600           L3090:    form pos 1,n 4,x 2,c 10,n 10.2,c 4,pic(zz/zz/zz),x 2,c 9,sz1*n 8.2,n 8.2,x 3,c 30
44620           totescrow=totescrow+escrow
44640         end if 
44660       else if ti1=2 then 
44680         pr #255,using 'form pos 1,c 15,n 10.2,x 1,c 15': x$,transAmount,nam$(1:15) pageoflow PGOF
44700       end if
44720     loop ! goto PS_LOOP_TOP ! /r
52000   PS_TOTALS: ! r:
52020   if ti1=2 then 
52040     ! r: DEPOSIT_LIST_TOTAL
52060     pr #255,using F_PR_TOTALS: "__________","Total Checks",totcheck
52080     pr #255,using F_PR_TOTALS: "          ","Total Cash",totcash
52100     pr #255,using F_PR_TOTALS: "__________","Total Deposit",totcheck+totcash
52120     pr #255,using "form pos 16,c 10": "=========="
52140     F_PR_TOTALS: form pos 16,c 10,skip 1,pos 1,c 15,n 10.2
52160     ! /r
54000   else ! ti1=1
54020     pr #255: ""
54040     pr #255,using " Form POS 24,C 32": "************ Totals ************"
54060     pr #255: ""
54080     pr #255: ""
54100     pr #255,using "Form POS 34,4*C 12": " Collections","         C/M","         D/M","       Total"
54120     pr #255,using F_PR_TOTAL_STUFF: "Totals",totalCollections,totalCreditMemos,totalDebitMemos,totalCollections+totalCreditMemos+totalDebitMemos
54140     pr #255: ""
54160     for j=1 to sz1
54180       pr #255,using F_PR_TOTAL_STUFF: serviceLabel$(j),brk(3,j),brk(4,j),brk(5,j),totalByService(j) pageoflow PGOF
54200     next j
54220     if uprc$(escrow$)="Y" then pr #255,using F_PR_TOTAL_STUFF: "Escrow",totescrow
54240     pr #255: ""
54260     F_PR_TOTAL_STUFF:  form pos 4,c 30,4*n 12.2
54280     if env$('client')="Ash Grove" then 
54300       pr #255: ''
54320       pr #255: ''
54340       pr #255,using 'form pos 7,c 50': "Water Collection Breakdown by Rate Code"
54360       for j=1 to 10
54380         if water(j)<>0 then 
54400           pr #255,using 'form pos 21,c 13,n 11.2,skip 1': "WATER CODE "&str$(j),water(j)
54420         end if 
54440       next j
54460       mat water=(0)
54480     end if 
54500    end if ! /r
54540    if ti1<>ti1_end and env$('acsDeveloper')='' then pr #255: newpage
54560   next ti1
55000   fncloseprn
55020   close #h_addr: ioerr ignore
55040   h_addr=0
55600 fnend  ! if l3=1 then goto MERGE else goto MENU1B
56000 PGOF: ! r:
56020   pr #255: newpage
56040   gosub HEADER
56060 continue  ! /r
58000 HEADER: ! r:
58020   if ti1=1 then 
58040     pr #255: "\qc  {\f181 \fs18 \b "&env$('cnam')&"}"
58060     pr #255: "\qc  {\f181 \fs22 \b Receipt Listing}"
58080     pr #255: "\qc  {\f181 \fs16 \b "&trim$(reportdate$)&"}"
58100     pr #255,using 'form pos 1,c 82,c 5,n 4': "\ql "&date$,"Page ",p2+=1
58120     pr #255: hd1$
58140   else if ti1=2 then 
58160     pr #255: "\ql  {\f181 \fs18 \b "&env$('cnam')&"}"
58180     pr #255: "\ql  {\f181 \fs22 \b Deposit Listing}"
58200     pr #255: "\ql  {\f181 \fs16 \b "&trim$(reportdate$)&"}"
58220     pr #255,using 'form pos 1,c 30,c 5,n 4': "\ql "&date$,"Page ",p2+=1
58240     pr #255: ''
58260     pr #255: "{\ul Account   }         {\ul Amount} {\ul Name          }"
58280   end if 
58300 return  ! /r
60000 def fn_setup
60020   if ~setup then 
60040     setup=1
60060  ! ______________________________________________________________________
60080     library 'S:\Core\Library': fnopenprn,fncloseprn,fnmsgbox,fnflexinit1,fnflexadd1,fndat,fncomboa
60100     library 'S:\Core\Library': fnremove,fncmbact,fndate_mmddyy_to_ccyymmdd,fnchk,fnCustomerNotes
60120     library 'S:\Core\Library': fncmdkey,fntop,fncustomer,fngethandle,fnbutton,fnget_services
60140     library 'S:\Core\Library': fnopen_receipt_printer,fnclose_receipt_printer,fnask_account
60160     library 'S:\Core\Library': fnxit, fnureg_read,fnureg_write,fnsafe_filename$,fnreport_cache_folder_current$
60180     library 'S:\Core\Library': fnerror,fntos,fnlbl,fnacs,fntxt,fncmdset,fnclient_has,fnCopy,fnreg_read
60200     on error goto ERTN
60220   ! ______________________________________________________________________
60240     dim alloc(10),serviceLabel$(11)*30,order(10),m1_item$(20)*80,srvname$(10)*20
60260     dim srv$(10)*2
60280     dim original(10),a(7)
60300     dim apply(10),penalty$(10)*1,water(10),reportdate$*20
60320     dim gb(10),tgb(10),pgb(10),hd1$*260,totalByService(10),tg(11)
60360     dim x(3),x$*10,dat$*20,nam$*28,x1$*10,z$*10,brk(5,10)
60380     dim resp$(30)*128,collections_filename$*128,hgb(10) ! ,fin$*20
60400     dim nam$*30,mesg$(10)*128
60420     dim ba(13),badr(2),bt1(14,2),bd1(5),bd2(5),bd3(5) ! bd$(5)*30,n$*30,txt$*80,notuse(10),
60440     dim ml$(1)*256
60460   ! 
60480     dim coll_type_option$(3)
60500     coll_type_option$(1)="Regular Collection"
60520     coll_type_option$(2)="Credit Memo"
60540     coll_type_option$(3)="Debit Memo"
60560   ! 
60580     tab$=chr$(9)
60600   ! ______________________________________________________________________
60640   ! 
60660     collections_filename$=env$('Q')&"\UBmstr\Collections-"&env$('acsUserId')&".h"&env$('cno')
60680     fndat(dat$,1)
60700   ! ______________________________________________________________________
60720     open #20: "Name="&env$('Q')&"\UBmstr\Company.h"&env$('cno')&",NoShr",internal,input 
60740     read #20,using "Form pos 128,C 1,c 1": receipt$,escrow$
60760     close #20: 
60780   ! ______________________________________________________________________
60800     fnget_services(mat srvname$, mat srv$, mat unused_tax_code$,mat penalty$,mat unused_subjectto,mat apply)
60820     if trim$(srvname$(1))="Water" then havewater=1
60840     if trim$(srvname$(3))<>"Electric" and srv$(3)="EL" then srvname$(3)=""
60860     if trim$(srvname$(4))<>"Gas" and srv$(4)="GA" then srvname$(4)=""
60880     if trim$(srvname$(2))="Sewer" then havesewer=1
60900     ! if trim$(srvname$(3))="Electric" then haveelectric=1
60920     ! if trim$(srvname$(4))="Gas" then havegas=1
60940     for j=1 to 10
60960       original(j)=apply(j)
60980       if apply(j)>0 then apply=apply+1: order(apply)=apply(j) ! set order of applying     collections
61000       if apply(j)=0 then noapply=noapply+1 ! notuse(noapply)=apply(j) ! set non used services
61020     next j
61040     for j=1 to 10
61060       for j1=1 to 10
61080         if j=original(j1) then baorder(j)=j1
61100       next j1
61120     next j
61140     if env$('client')="Divernon" then mat baorder=original ! may need to be made standard for everyone   !!!
61160     hd1$="{\ul Rec }  {\ul Account   }  {\ul    Total}    {\ul   Date  }  {\ul ReceiptNo}"
61180     for j=1 to 10
61200       if trim$(srvname$(j))<>"" and trim$(srvname$(j)(1:5))<>"Reduc" then 
61220         sz1+=1
61240         hd1$=hd1$&"  {\ul "&srvname$(j)(1:6)&"}"
61260         serviceLabel$(sz1)=trim$(srvname$(j)(1:28))&":"
61280       end if 
61300     next j
61320     if uprc$(escrow$)="Y" then hd1$=rtrm$(hd1$)&"  {\ul Escrow}"
61340     mat totalByService(sz1)
61360     mat tgb(sz1) : mat hgb(sz1) : mat alloc(sz1) : mat serviceLabel$(sz1)
61380     F_ubColInp: form pos 1,c 10,pd 4.2,pd 4,2*n 1,pos 24,c 9,sz1*pd 4.2,5*pd 3,pd 4.2
61400   end if 
61420   ! r: setup column headers (mat chdr$) and column masks (mat cm$) for flex grid on MENU1B
61440     dim chdr$(20)*30,cm$(20)
61460     mat chdr$(20)
61480     chdr$(1)="Rec" : chdr$(2)="Account" : chdr$(3)="Amount"
61500     chdr$(4)="Date" : chdr$(5)="Type" ! chdr$(6)="PC"
61520     chdr$(6)="Receipt Number" : cHdrItem=6
61540     for j2=1 to 10
61560       if trim$(srvname$(j2))<>"" then 
61580         chdr$(cHdrItem+=1)=srvname$(j2)(1:10)
61600         fnremove(":",chdr$(cHdrItem))
61620       end if 
61640     next j2
61660     if uprc$(escrow$)="Y" then chdr$(cHdrItem+=1)="Escrow"
61680     mat chdr$(cHdrItem)
61700   ! 
61720     mat cm$=("")
61740     cm$(2)="32" : cm$(3)="10" : cm$(4)="1"
61760     cm$(5)="" ! "30" 
61770     cm$(6)="30" ! cm$(7)=""
61780     for j=7 to max(9,udim(chdr$)) : cm$(j)="10" : next j
61800     mat cm$(udim(chdr$))
61820   ! /r
61840   ei_item_account=1
61860   ei_item_amount=2
61880   ei_item_date_time=3
61900   ei_item_collection_type=4
61920   !
61940   fnreg_read('Collections pr in Account Order',ub_collPrintInAccountOrder$,'False')
61960   fnreg_read('Collections Disable Deposit List',ub_collDisableDepositList$,'False')
61990 fnend 
62000 def fn_haveMainBudget(h_budmstr,x1$)
62020   havemainbudget=0
62040   read #h_budmstr,using 'Form POS 1,C 10,PD 4,12*PD 5.2,2*PD 3',key=x1$: z$,mat ba,mat badr nokey L3820 ! get mat ba again
62060   if ba(2)+ba(3)+ba(4)+ba(5)+ba(6)+ba(7)+ba(8)+ba(9)+ba(10)+ba(11)>0 then havemainbudget=1
62080   L3820: ! 
62100   fn_haveMainBudget=havemainbudget
62120 fnend 
64000 def library fnBreakdown(h_customer,h_budmstr,x1$,havebudget, mat tgb, mat alloc,mat baorder,&coramt,&ckey)
64020   fn_setup
64040   fnBreakdown=fn_breakdown(h_customer,h_budmstr,x1$,havebudget, mat tgb, mat alloc,mat baorder,coramt,ckey)
64060 fnend 
68000 def fn_breakdown(h_customer,h_budmstr,x1$,havebudget, mat tgb, mat alloc,mat baorder,&coramt,&ckey)
68020   ! returns 0 if canceled, else 1  (back returns 1, but also coramt=1)
68040   ! h_customer - customer file handle with account number key
68060   ! x1$ - customer account number key
68080   ! havebudget - 1=yes customer is on budget billing, 0=no
68100   ! mat tgb -
68120   ! mat alloc -
68140   ! mat baorder -
68160   ! x(2) is transaction amount
68180   ! coramt - used to determine if BACK was selected (coramt=1)
68200   dim bd_real(11)
68220   read #h_customer,using 'Form Pos 41,C 28,Pos 292,PD 4.2,PD 4,Pos 388,10*PD 5.2,pos 1859,pd 5.2,pos 143,7*pd 2',key=x1$,release: nam$,bal,db1,mat gb,escrowbal,mat a nokey BD_TOS
68240 BD_TOS: ! 
68260   fntos(sn$="breakdown")
68280   reco=0
68300   fnlbl(1,1,"Account:",30,1)
68320   fntxt(1,32,10,0,1,"",1)
68340   resp$(reco+=1)=x1$
68360   fnlbl(2,1,"Customer Name:",30,1)
68380   fntxt(2,32,30,0,0,"",1)
68400   resp$(reco+=1)=trim$(nam$)
68420   fnlbl(3,1,"Transaction Amount:",30,1)
68440   fntxt(3,32,10,12,1,"10",1)
68460   resp$(reco+=1)=str$(x(2))
68480   fnlbl(5,1,"Enter Allocation Breakdown amounts.",54,2)
68500   fnlbl(6,31,"Allocation",10,2)
68520   fnlbl(6,44,"Balance",10,2)
68540   if bd_re_editmode=1 then 
68560     bd_re_editmode=0
68580   else 
68600     gosub BD_ALLOC
68620   end if 
68640   bd_line_add=0 : mat bd_real=(0)
68660   for j=1 to 10
68680     if trim$(srvname$(j))<>"" and trim$(srvname$(j)(1:5))<>"Reduc" then 
68700       bd_line_add+=1
68720       fnlbl(bd_line_add+6,1,serviceLabel$(bd_line_add),29,1)
68740       resp$(reco+=1)=str$(tgb(bd_line_add))
68760       fntxt(bd_line_add+6,44,12,0,1,"10",1)
68780       resp$(reco+=1)=str$(alloc(bd_line_add))
68800       fntxt(bd_line_add+6,32,12,0,1,"10")
68820       bd_real(bd_line_add)=reco
68840     end if 
68860   next j
68880   if uprc$(escrow$)="Y" then 
68900     bd_line_add=bd_line_add+1
68920     fnlbl(bd_line_add+6,1,"Escrow:",29,1)
68940     resp$(reco+=1)=str$(oldescrowbal)
68960     fntxt(bd_line_add+6,44,12,0,1,"10",1)
68980     resp$(reco+=1)=str$(escrow)
69000     fntxt(bd_line_add+6,32,12,0,1,"10")
69020     bd_real(bd_line_add)=reco
69040   end if 
69060   if csv_import_in_process then 
69080     fncmdkey("&Save",1,1) : fncmdkey("&Skip",5,0,1)
69100   else 
69120     fncmdset(6) ! fncmdkey("&Next",1,1) : fncmdkey("&Back",2) : fncmdkey("&Cancel",5,0,1)
69140   end if 
69160   fnacs(sn$,0,mat resp$,ckey)
69180 ! 
69200   for j=1 to udim(alloc)
69220     if bd_real(j)<>0 then alloc(j)=val(resp$(bd_real(j))) else alloc(j)=0
69240   next j
69260   if uprc$(escrow$)="Y" then escrow=val(resp$(bd_real(j)))
69280   if ckey=1 then 
69300     goto NEXT_AFTER_BREAKDOWN
69320   else if ckey=2 then 
69340     coramt=1 : goto BD_FINIS
69360   else if ckey=5 then 
69380     bd_return=0 : goto BD_FINIS
69400   else 
69420     goto NEXT_AFTER_BREAKDOWN
69440   end if 
69460 ! 
69480 NEXT_AFTER_BREAKDOWN: ! 
69500   tal=0 : for j=1 to udim(alloc) : tal+=alloc(j) : next j
69520   if tal<>x(2) then 
69530     mat mesg$(6)
69540     mesg$(1)="Total Allocations must equal Transaction Amount!"
69560     mesg$(2)=""
69580     mesg$(3)=cnvrt$("pic(-----------#.##)",x(2))&" (Transaction Amount)"
69600     mesg$(4)=cnvrt$("pic(-----------#.##)",tal)&" (Total Allocations) "
69620     mesg$(5)="___________"
69640     mesg$(6)=cnvrt$("pic(-----------#.##)",x(2)-tal)&" (Difference)"
69660     fnmsgbox(mat mesg$, resp$,'',48)
69680     bd_re_editmode=1 : goto BD_TOS ! (skip re-reading of record) ! goto BREAKDOWN
69700   end if 
69720   bd_return=1
69740 BD_FINIS: ! 
69760   fn_breakdown=bd_return
69780 fnend 
72000 BD_ALLOC: ! r:
72020   if editmode=1 then 
72040     mat tgb=alloc
72060   else 
72080     j2=0
72100     if ~havebudget=1 then 
72120       for j=1 to 10
72140         if trim$(srvname$(j))<>"" and trim$(srvname$(j)(1:5))<>"Reduc" then 
72160           tgb(j2+=1)=gb(j)
72180         end if 
72200       next j
72220     end if 
72240   end if 
72260   if uprc$(escrow$)="Y" and transType=3 then ! add escrow balance into last allocation if have escrow and processing a collection transaction
72280     oldescrowbal=escrowbal
72300   end if 
72320   bd_tgbj=tn=0
72340   mat ba=(0) : mat badr=(0)
72360   for j=1 to udim(mat alloc)
72380     if tgb(j)<0 then tn-=tgb(j) ! Total Negative Breakdowns
72400   next j
72420   items=udim(mat alloc) ! If UPRC$(ESCROW$)="Y" Then iTEMS=UDIM(ALLOC)-1 Else iTEMS=UDIM(ALLOC) ! subtract one from order if escrow
72440   havemainbudget=fn_haveMainBudget(h_budmstr,x1$)
72460   for j=1 to items
72480     if ~(havemainbudget=1 and penalty$(baorder(j))="Y") then ! ELSE don't allow penalty budgets amount to go thru routine
72500       if havemainbudget=1 then 
72520         alloc(order(j))=max(0,min(x(2)-bd_tgbj,ba(baorder(j)+1)))
72540       else 
72560         alloc(order(j))=max(0,min(x(2)-bd_tgbj+tn,tgb(order(j))))
72580       end if 
72600       bd_tgbj=bd_tgbj+alloc(order(j))
72620       if tgb(order(j))<0 then tn=tn+tgb(order(j))
72640     end if 
72660   next j
72680   if havemainbudget=1 and sum(alloc)<x(2) then 
72700     for j=1 to items ! if have budget and pay more than budget, how to allocate remainder
72720       if alloc(order(j))=0 then 
72740         alloc(order(j))=max(0,min(x(2)-bd_tgbj+tn,tgb(order(j))))
72760         bd_tgbj=bd_tgbj+alloc(order(j))
72780         if tgb(order(j))<0 then tn=tn+tgb(order(j))
72800       end if 
72820     next j
72840   end if 
72860   if env$('client')="Findlay" and a(4)>0 then ! excess in gas
72880     alloc(3)=alloc(3)+x(2)-bd_tgbj
72900     goto BD_ALLOC_FINIS
72920   end if 
72940   if env$('client')="Findlay" and a(1)>0 then ! if no gas, put excess in water
72960     alloc(1)=alloc(1)+x(2)-bd_tgbj
72980     goto BD_ALLOC_FINIS
73000   end if 
73020   if alloc(1)>0 or (havewater=1 and a(1)>0) then ! excess in water if it is an active service for this customer
73040     alloc(1)=alloc(1)+x(2)-bd_tgbj
73060     goto BD_ALLOC_FINIS
73080   else if alloc(2)>0 or (havesewer=1 and a(2)>0) then ! excess in sewer if it is an active service for this customer
73100     alloc(2)=alloc(2)+x(2)-bd_tgbj
73120     goto BD_ALLOC_FINIS
73140   end if 
73160   if udim(alloc)>=3 then 
73180     if alloc(3)>0 then ! excess in electric if it is an active service for this customer
73200       alloc(3)=alloc(3)+x(2)-bd_tgbj
73220       goto BD_ALLOC_FINIS
73240     end if 
73260     if udim(alloc)>=4 then 
73280       if alloc(4)>0 then ! excess in gas if it is an active service for this customer
73300         alloc(4)=alloc(4)+x(2)-bd_tgbj
73320         goto BD_ALLOC_FINIS
73340       end if 
73360       alloc(1)=alloc(1)+x(2)-bd_tgbj ! if excess not allocated to any other service, allocate it to water
73380     end if 
73400   end if 
73420 BD_ALLOC_FINIS: ! 
73440   return  ! /r
73460 def fn_open_cash_drawer
73500   fnopen_receipt_printer
73520   pr #255,using 'form pos 1,c 9,skip 0': hex$("1B70302828") ioerr ignore ! apg cash drawer hooked to epson t 88 thermal receipt printer
73540   fnclose_receipt_printer
73580 fnend 
74000 def fn_print_receipt(pr_acct_key$,pr_acct_name$*30,rcpt$,bal,pr_trans_amt,pr_trans_date,coll_type$*30)
74020   if fnopen_receipt_printer(1) then 
74040     receipt_width=32
74060     pr #255,using 'form pos 1,C 2,Cc '&str$(receipt_width-4)&',C 2': '**',env$('cnam')(1:28),'**'
74080     pr #255,using 'form pos 1,Cc '&str$(receipt_width): date$('mm/dd/ccyy')
74100     pr #255,using 'form pos 1,Cc '&str$(receipt_width): time$
74120     if rcpt$<>'' then 
74140       pr #255: "Receipt Number: "&rcpt$
74160       pr #255: ''
74180     end if 
74200     pr #255: ''
74220     pr #255: 'Account: '&pr_acct_key$
74240     pr #255: '   Name: '&pr_acct_name$
74260     pr #255: ''
74280     pr #255,using 'form pos 1,Cc '&str$(receipt_width): coll_type$
74300     pr #255: ' Amount: '&cnvrt$('G 10.2',pr_trans_amt)
74320   !   pr #255: 'Bal Before Payment: '&cnvrt$('G 10.2',bal)
74340   !   pr #255: '              Date: '&cnvrt$('pic(##/##/##)',pr_trans_date)
74360     pr #255: ''
74380     pr #255: '________________________________' ! 32 characters - perfect max width fit for my POs-58 usb receipt printer
74400     pr #255: ''
74420     fnclose_receipt_printer
74440   end if 
74460 fnend 
76000 def fn_csv_import
76020   fnureg_read('Collections CSV Import Filename',ecp_filename$)
76040   ! if ecp_filename$='' then ecp_filename$=os_filename$(env$('userprofile')&'\Desktop')&"\ACS_ECP_Export.txt"
76060   EI_SCREEN1: ! 
76080   fntos(sn$="coll_csv_imp")
76100   fnlbl(1,1,"Import CSV Path and File Name:",33,1)
76120   fntxt(1,35,40,256,0,"71")
76140   resp$(1)=ecp_filename$
76160   ! fnlbl(5,1,"NOTE: If Destination exists it will be overwritten.",76,2)
76180   fncmdset(2)
76200   fnacs(sn$,0,mat resp$,ckey)
76220   if ckey=5 then goto EI_XIT
76240   ecp_filename$=resp$(1)
76260   ! 
76280   fn_ei_backup(ecp_filename$)
76300   open #h_csv:=fngethandle: "Name="&env$('at')&br_filename$(ecp_filename$),display,input ioerr EI_SCREEN1
76320   ecp_filename$=os_filename$(file$(h_csv))
76340   fnureg_write('Collections CSV Import Filename',ecp_filename$)
76360   type=fn_csv_type(h_csv)
76380   csv_import_in_process=1
76400   if type=1 then 
76420     fn_ecp_import(h_csv)
76440   else if type=2 then 
76460   ! r: import stardardized CSV (column headers enums already identified in fn_csv_type)
76480     dim csv_line$*512
76500     dim csv_item$(0)*256
76520     do 
76540       linput #h_csv: csv_line$ eof CSV_FINIS
76560       if trim$(srep$(csv_line$,csv_delim$,''))<>'' then ! if not a blank line then
76580         csv_line$=fn_remove_quote_encap_commas$(csv_line$)
76600         csv_line$=fn_remove_quote_encap_commas$(csv_line$)
76620         str2mat(csv_line$,mat csv_item$,csv_delim$)
76640         trans_date_mmddyy=date(days(csv_item$(csv_date),'m/d/ccyy'),'mmddyy')
76642         csv_item$(csv_amount)=srep$(csv_item$(csv_amount),'$','')
76644         csv_item$(csv_amount)=srep$(csv_item$(csv_amount),',','')
76660         trans_amount=val(csv_item$(csv_amount))
76680         if trans_amount<0 then !  debit memos maybe used for bounced checks
76700           trans_type=5 ! debit memo
76720           trans_amount=abs(trans_amount)
76740         else 
76760           trans_type=3 ! regular collection
76780         end if 
76800         fn_add_trans(csv_item$(csv_account),trans_date_mmddyy,trans_type,trans_amount)
76820       end if 
76840     loop 
76860   CSV_FINIS: ! 
76880   ! /r
76900   else 
76920     mat ml$(9)
76940     ml$(1)="Unrecognized CSV or TXT Type."
76960     ml$(2)="Make sure your CSV (comma seperated values) file has each of the following column headings:"
76980     ml$(3)=tab$&"Date"
77000     ml$(4)=tab$&"Account"&tab$&'("Acct" and "Acct #" are acceptable also.)'
77020     ml$(5)=tab$&"Amount"&tab$&'("Amt" and "Payment" are acceptable also.)'
77040     ml$(6)=tab$&"Type"&tab$&'"Cash" for cash.' ! '  "CK [checknumber]" for checks.'
77060     ml$(7)=tab$&tab$&'("Pmt Type" is acceptable also.)'
77080     ml$(8)="Negative amounts will be entered as Debit Memos."
77100     ml$(9)="Tab or Comma delimiters are allowed."
77120     fnmsgbox(mat ml$, response$, '',64)
77140   end if 
77160   close #h_csv: ioerr ignore
77180   csv_import_in_process=0
77200 fnend 
78000 def fn_csv_type(h_csv)
78020   ! returns 1 for ECP_IMPORT type CSV File
78040   ! returns 2 for standardized CSV type file
78060   ! returns 0 for unrecognized
78080   dim ct_line$*512
78100   dim ct_item$(0)*256
78120   linput #h_csv: ct_line$
78140   if pos(ct_line$,tab$)>0 then csv_delim$=tab$ else csv_delim$=','
78160   str2mat(ct_line$,mat ct_item$,csv_delim$)
78180   if udim(mat ct_item$)<4 then ct_return=0 : goto CT_FINIS
78200   ct_item_2_val=val(ct_item$(2)) conv CT_ITEM_2_CONV
78220   ct_return=1 ! second item on first line has a numeric value - it's ECP_IMPORT type
78240   goto CT_FINIS
78260   CT_ITEM_2_CONV: ! 
78280   for ct_item=1 to udim(mat ct_item$)
78300     ct_item$(ct_item)=trim$(lwrc$(ct_item$(ct_item)))
78320   next ct_item
78340   csv_date=max(0,srch(mat ct_item$,'date'))
78360   csv_account=max(0,srch(mat ct_item$,'acct #'))
78380   if csv_account<=0 then 
78400     csv_account=max(0,srch(mat ct_item$,'acct'))
78420     if csv_account<=0 then 
78440       csv_account=max(0,srch(mat ct_item$,'account'))
78460     end if 
78480   end if 
78500   csv_type=max(0,srch(mat ct_item$,'pmt type'))
78520   if csv_type<=0 then 
78540     csv_type=max(0,srch(mat ct_item$,'type'))
78560   end if 
78580   csv_amount=max(0,srch(mat ct_item$,'payment'))
78600   if csv_amount<=0 then 
78620     csv_amount=max(0,srch(mat ct_item$,'amt'))
78640     if csv_amount<=0 then 
78660       csv_amount=max(0,srch(mat ct_item$,'amount'))
78680     end if 
78700   end if 
78720   if csv_account and csv_amount and csv_date and csv_type then 
78740     ct_return=2
78760   end if 
78780   CT_FINIS: ! 
78800   fn_csv_type=ct_return
78820 fnend 
80000 def fn_remove_quote_encap_commas$*512(rqec_line$*512)
80020   for rqec_char=1 to len(rqec_line$)
80040     tmp_chr$=rqec_line$(rqec_char:rqec_char)
80060     if tmp_chr$='"' then 
80080       rqec_inside_quote+=1
80100       if rqec_inside_quote=2 then rqec_inside_quote=0
80120     else if tmp_chr$=',' then 
80140       if rqec_inside_quote then rqec_line$(rqec_char:rqec_char)=';' ! replace the quote with a semi-colon.
80160     end if 
80180   next rqec_char
80200   fn_remove_quote_encap_commas$=rqec_line$
80220 fnend 
82000 def fn_ecp_import(h_ecp)
82010   dim ecp_filename$*256
82020   dim ei_line$*512
82030   dim ei_item$(0)*256
82040   if ~fnclient_has('U5') then 
82050     mat ml$(2)
82060     ml$(1)="You must purchase the ACS Utility Billing External Collections Processing"
82070     ml$(2)="module to import this type of CSV."
82080     fnmsgbox(mat ml$, response$, '',64)
82090     goto EI_XIT
82100   end if 
82120   restore #h_ecp: 
82280   ! r: main loop
82290   do 
82300     ! 
82310     linput #h_ecp: ei_line$ eof EI_FINIS
82320     str2mat(ei_line$,mat ei_item$,csv_delim$)
82330     ei_item_account=1
82340     ei_item_amount=2
82350     ei_item_date_time=3
82360     ei_item_collection_type=4 ! Check or Credit
82370     trans_date_mmddyy=date(days(ei_item$(ei_item_date_time)(1:pos(ei_item$(ei_item_date_time),' ')-1),'m/d/ccyy'),'mmddyy')
82380     trans_amount=val(ei_item$(ei_item_amount))
82390     if trans_amount<0 then !  debit memos maybe used for bounced checks
82400       trans_type=5 ! debit memo
82410       trans_amount=abs(trans_amount)
82420     else 
82430       trans_type=3 ! regular collection
82440     end if 
82460     fn_add_trans(ei_item$(ei_item_account),trans_date_mmddyy,trans_type,trans_amount)
82470   loop 
82480   ! /r
82490   EI_FINIS: ! 
82500   goto EI_XIT
82510   EI_XIT: ! 
82520 fnend 
83000 def fn_ei_backup(ecp_filename$*256)
83020   if exists(ecp_filename$) then 
83040     fnCopy(ecp_filename$,fnreport_cache_folder_current$&'\Electronic Collections Imported - '&date$('ccyy-mm-dd')&' '&fnsafe_filename$(time$)&'.csv')
83060   end if  ! exists UBmstr\readings.[bk$]
83080 fnend 
84000 def fn_add_trans(at_customer$*10,at_date_mmddyy,at_trans_type,at_amount)
84040   b7=transType=ti1
84060   p$=" "
84080   rcpt$=""
84100   transType=b7
84120   AT_READ_CUSTOMER: ! 
84130   at_customer$=lpad$(at_customer$,10)
84160   coramt=0
84180   ! r: read selected account and prepare data for SCREEN_ADD
84200   read #h_customer,using 'Form Pos 41,C 28,Pos 292,PD 4.2,PD 4,Pos 388,10*PD 5.2,pos 1859,pd 5.2',key=at_customer$,release: nam$,bal,db1,mat gb,escrowbal nokey AT_NO_CUSTOMER
84210   let x1$=at_customer$
84220   havebudget=0 : mat tgb=(0)
84240   j2=0 : escrow=0
84260   for j=1 to 10
84280     if trim$(srvname$(j))<>"" and trim$(srvname$(j)(1:5))<>"Reduc" then tgb(j2+=1)=gb(j)
84300   next j
84320   if uprc$(escrow$)="Y" and transType=3 then oldescrowbal=escrowbal ! add escrow balance into last allocation if have escrow and processing a collection transaction
84340   ! /r
84360   ! SCREEN_ADD: !
84380   let x(3)=at_date_mmddyy
84400   let x(2)=at_amount
84420   transType=at_trans_type
84440   ! 
84460   do_not_blank_rcpt=0
84480   ! rcpt$=trim$(resp$(3))(1:9)
84500   at_customer$=lpad$(trim$(at_customer$),10)
84520   ! 
84540   ! pause !
84560   ! r: after SCREEN_ADD - actually do the adding stuff
84580   if ti1=3 then goto AT_L1980
84600   if sum(tgb)=x(2) then goto AT_L2020
84620   if uprc$(escrow$)="Y" then gosub CHECK_ESCROW ! check escrow balance
84640   mat hgb=tgb
84660   ! Check for previous months
84680   mat tgb=(0)
84700   restore #4,key>=at_customer$&"         ": nokey AT_L1960
84720   AT_L1800: read #4,using L1810: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof L1960
84740   if p$<>at_customer$ then goto AT_L1960
84760   if tcode<1 or tcode>2 then goto AT_L1800 ! only allow charge and penalty trans to flow thru
84780   mat tgb=(0)
84800   j2=0
84820   for j1=1 to 10
84840     if tcode=1 and penalty$(j1)="Y" then ! add penalties up seperate
84860       pgb(j2+=1)=tg(j1)
84880     else if trim$(srvname$(j1))<>"" and trim$(srvname$(j1)(1:5))<>"Reduc" then 
84900       tgb(j2+=1)=tg(j1)
84920     end if 
84940   next j1
84960   if sum(tgb)=x(2) then goto AT_L2020
84980   if sum(tgb)+sum(pgb)=x(2) then ! test with penalties added in
85000     for x=1 to udim(tgb)
85020       tgb(x)+=pgb(x)
85040     next x
85060     goto AT_L2020
85080   end if 
85100   ! L1950: !
85120   goto AT_L1800
85140   AT_L1960: ! 
85160   mat tgb=hgb
85180   gosub BUD2
85200   gosub BUD3
85210   AT_L1980: ! 
85220   if ~fn_breakdown(h_customer,h_budmstr,at_customer$,havebudget, mat tgb, mat alloc,mat baorder,coramt,ckey) then 
85230     if csv_import_in_process then 
85240       goto AT_FINIS
85250     else 
85260       goto SCREEN_SELECT_ACCOUNT
85270     end if 
85280   end if 
85290   if coramt=1 then 
85320     pause  ! goto SCREEN_SELECT_ACCOUNT
85340   else 
85360     goto AT_L2040
85380   end if 
85400   ! 
85420   AT_L2020: ! 
85440   items=sz1 ! If UPRC$(ESCROW$)="Y" Then iTEMS=SZ1-1 Else iTEMS = SZ1
85460   for j=1 to items : alloc(j)=tgb(j) : next j
85480   AT_L2040: ! 
85520   transAmount=x(2) : transDate=x(3) : b7=transType
85540   postingCodeUnused=0
85560   if sum(tgb)=x(2) then gosub BUD2 ! kj 10/14/09
85580   if sum(tgb)=x(2) and bud1=1 then gosub BUD3 ! was commented out; changed to if sum= on 101409 to keep from skipping ubdget update if exact amount paid.
85600   r6=lrec(h_ubcolinp)+1
85620   if escrow>90000 then escrow=0 ! PREVENT 726 ERROR
85640   write #h_ubcolinp,using F_ubColInp,rec=r6: at_customer$,transAmount,transDate,transType,postingCodeUnused,rcpt$,mat alloc,mat bd2,escrow duprec L2060
85660   ! oldn=transDate
85680   transType=b7
85700   goto AT_FINIS
85720   ! pr 'completed add' : pause ! goto SCREEN_SELECT_ACCOUNT ! /r
85740   AT_NO_CUSTOMER: ! 
85750   at_customer$=trim$(at_customer$)
85760   let x1_len=len(at_customer$)
85780   if x1_len<=9 and at_customer$(len(at_customer$)-1:len(at_customer$)-1)='.' then ! maybe excel messed it up, try adding a 0, because it has something like .1
85800     at_customer$=at_customer$&'0'
85820     goto AT_READ_CUSTOMER
85840   else if x1_len<=7 and pos(at_customer$,'.')<=0 then ! maybe excel messed it up, try adding a .00
85860     at_customer$=at_customer$&'.00'
85880     goto AT_READ_CUSTOMER
85900   end if 
85920   mat ml$(6)
85940   ml$(1)='Customer '&at_customer$&' could not be found.'
85960   ml$(2)='The following transaction will NOT be added.'
85980   ml$(3)=tab$&'Customer:'&tab$&at_customer$
86000   ml$(4)=tab$&'Date:'&tab$&date$(days(at_date_mmddyy,'mmddyy'),'mm/dd/ccyy') ! this date is not formatting on the screen properlly
86020   ml$(5)=tab$&'Type:'&tab$&str$(at_trans_type)
86040   ml$(6)=tab$&'Amount:'&tab$&str$(at_amount)
86060   fnmsgbox(mat ml$,resp$,'',0)
86080   AT_FINIS: ! 
86490 fnend
87000 def fn_oSub1(coll_type_option$) ! returns appropriate transType based on coll_type_option$ 
87020   oSub1Return=0
87040   ! 1="Regular Collection",transType=3
87060   ! 2="Credit Memo",transType=4
87080   ! 3="Debit Memo",transType=5
87100   if coll_type_option$=coll_type_option$(1) then 
87120     oSub1Return=3
87140   else if coll_type_option$=coll_type_option$(2) then 
87160     oSub1Return=4
87180   else if coll_type_option$=coll_type_option$(3) then 
87200     oSub1Return=5
87220   end if 
87240   fn_oSub1=oSub1Return
87260 fnend
88000 def fn_collType$(transType) ! returns appropriate coll_type_option$ based on 0(1)
88020 ! 1="Regular Collection",transType=3
88040 ! 2="Credit Memo",transType=4
88060 ! 3="Debit Memo",transType=5
88080   collTypeReturn$=''
88100   if transType=3 then 
88120     collTypeReturn$=coll_type_option$(1)
88140   else if transType=4 then 
88160     collTypeReturn$=coll_type_option$(2)
88180   else if transType=5 then 
88200     collTypeReturn$=coll_type_option$(3)
88220   end if 
88240   fn_collType$=collTypeReturn$ ! &' ('&str$(transType)&')' ! pr collTypeReturn$ : pause
88260 fnend
89000 ! READD: ! r: RE-ADD PROOF TOTALS
89020 !   for j=1 to lrec(h_ubcolinp)
89040 !     r5=j
89060 !     read #h_ubcolinp,using F_ubColInp,rec=r5: x$,transAmount,transDate,transType,postingCodeUnused,rcpt$,mat alloc,mat bd3,escrow norec L4690 eof L4700
89080 !     fn_increaseMatR(transType,transAmount,totalCollections,totalDebitMemos,totalCreditMemos)
89100 ! L4690: next j
89120 ! L4700: return  ! /r
90000 def fn_increaseMatR(transType,amt,&totalCollections,&totalDebitMemos,&totalCreditMemos)
90010   if transType=1 then 
90020     totalCollections+=amt
90030   else if transType=3 then 
90040     totalCollections+=amt 
90060   else if transType=4 then 
90080     totalCreditMemos+=amt 
90100   else if transType=5 then 
90120     totalDebitMemos+=amt 
90140   end if 
90160 fnend
