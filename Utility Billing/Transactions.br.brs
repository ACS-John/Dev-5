00010 ! formerly S:\acsUB\FlexTran
00030 fn_setup
00070 fntop(program$)
00080 fn_transfile
00090 XIT: fnxit
22000 ! <Updateable Region: ERTN>
22020 ERTN: let fnerror(program$,err,line,act$,"xit")
22040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
22060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
22080   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
22100 ERTN_EXEC_ACT: execute act$ : goto ERTN
22120 ! /region
24000 def fn_setup
24020   if ~setup then
24040     setup=1
24060     library 'S:\Core\Library': fntop,fntos,fnacs,fncmdkey,fnerror,fnfra,fnbutton,fnchk,fncmdset,fnopt,fnlbl,fntxt,fncmbact
24080     library 'S:\Core\Library': fngethandle,fncreg_read,fncreg_write
24100     library 'S:\Core\Library': fnopenprn,fncloseprn,fnmsgbox,fnxit
24120     library 'S:\Core\Library': fnget_services
24140     library 'S:\Core\Library': fnflexinit1,fnflexadd1
24160     on error goto ERTN
24180     let transtype$(1)="Charge"
24200     let transtype$(2)="Penalty"
24220     let transtype$(3)="Collection"
24240     let transtype$(4)="Credit Memo"
24260     let transtype$(5)="Debit Memo"
24380     fnget_services(mat servicename$, mat srv$)
24400   end if
24420 fnend
32000 def library fntrans_total_as_of(; customer_key$,date_ccyymmdd,trans_type)
32020   if ~setup then let fn_setup
32040   fntrans_total_as_of=fn_trans_total_as_of(customer_key$,date_ccyymmdd,trans_type)
32060 fnend
34000 def fn_trans_total_as_of(;customer_key$,date_ccyymmdd, trans_type)
34020   ! transaction_type or blank for all
34040   if ~ttao_setup then 
34060     ttao_setup=1
34080     open #ttao_h_trans:=fngethandle: "Name="&env$('Q')&"\UBmstr\ubtransvb.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\UBTrIndx.h"&env$('cno')&",Shr",internal,input,keyed 
34100     dim ttao_key$*19
34120   end if
34140   ttao_return=0
34160   customer_key$=lpad$(customer_key$,10)
34180 ! ttao_key$=rpad$(customer_key$,kln(ttao_h_trans)) 
34200   ttao_key$=customer_key$&lpad$(str$(date_ccyymmdd),8)&cnvrt$('pic(z)',trans_type)
34220   restore #ttao_h_trans,key>=ttao_key$: nokey TTAO_FINIS
34240   do
34260     read #ttao_h_trans,using "Form pos 1,c 10,N 8,N 1,pd 4.2": ttao_customer_read$,ttao_date,ttao_code,ttao_amount eof TTAO_FINIS
34280     if lpad$(ttao_customer_read$,10)=lpad$(customer_key$,10) or trim$(ttao_customer_read$)<>'' then 
34300       if ~date_ccyymmdd or ttao_date=>date_ccyymmdd then 
34320         if trans_type=0 or trans_type=ttao_code then
34340           ttao_return+=ttao_amount
34360         end if
34380       end if
34400     end if
34420   loop until customer_key$<>'' and ttao_customer_read$<>customer_key$
34440   TTAO_FINIS: !
34460   fn_trans_total_as_of=ttao_return
34480 fnend 
36000 def library fntransfile(; hact$*81)
36020   if ~setup then let fn_setup
36040   fntransfile=fn_transfile( hact$)
36060 fnend
40000 def fn_transfile(; hact$*81)
40020   dim resp$(10)*80,printlineform$*1024
40040   dim totalalloc(10),totalusage(3),usage(3)
40060   ! ___________________________________________________________________
40080   SCREEN1: ! 
40100   gosub ASKTRANSET ! Let FNASKTRANSET(CKEY,SEL_CODE,BEG_DATE,END_DATE,Z$,HACT$)
40120   if ckey=2 and trim$(z$)="" then goto SCREEN1 ! don't allow print to work if no customer selected
40140   ! If CKEY=2 AND TRIM$(Z$)="[All]" Then Goto SCREEN1
40160   if ckey=2 then let fn_PRINTTRANS : goto SCREEN1 ! print report of charges
40180   if ckey=5 then goto Tf_XIT else goto SCREEN_TRANS_GRID
40200   ! ___________________________________________________________________
41000   SCREEN_TRANS_GRID: ! r:
41020   let fntos(sn$="Transaction-2")
41040   stgFlexLine=0
41080   fnbutton(stgFlexLine+=1,1,'Columns',opt_columns:=6)
41120   if z$<>'[All]' then
41140     fnlbl(stgFlexLine+=1,1,'Account:',8,1)
41160     fntxt(stgFlexLine,10,10,0,0,'',1)
41180     resp$(1)=z$
41200   end if
41220   let fn_flextran (stgFlexLine+=1,1,0,z$,beg_date,end_date,sel_code)
41240   let fncmdkey('Print',opt_print:=4,0,0)
41260   let fncmdkey('Edit',opt_edit:=1,1,0)
41280   let fncmdkey('Back',opt_back:=2,0,0,'Return to filter selection')
41300   let fncmdkey('Close',5,0,1)
41320   let fnacs(sn$,0,mat resp$,ckey)
41340   if ckey=opt_back then 
41360     goto SCREEN1
41380   else if ckey=5 then 
41400     goto Tf_XIT
41420   else if opt_columns and ckey=opt_columns then 
41440     let fn_columnSelect
41460   else if ckey=opt_print then 
41480     let fn_PRINTTRANS
41500   else if ckey=opt_edit then 
41520     if z$='[All]' then let editrec=val(resp$(1)) else editrec=val(resp$(2))
41540     fn_TransactionEdit(editrec)
41560   end if 
41580   goto SCREEN_TRANS_GRID ! /r
42000   ASKTRANSET: ! r: Def FNASKTRANSET(&CKEY,&SEL_CODE,&BEG_DATE,&END_DATE,&Z$,HACT$*80)
42020     let fntos(sn$="Transaction-1")
42040     let rc=cf=0
42060     let fnfra(1,1,6,23,"Transaction Type","You can review all transactions or any specific type of transaction",0)
42080     let cf+=1 : let fratype=cf
42100     let fnopt(1,3,"[All]",0,fratype)
42120     if sel_code=1 or sel_code=0 then let resp$(rc+=1)="True" else let resp$(rc+=1)="False"
42140     let fnopt(2,3,"Charges",0,fratype)
42160     if sel_code=2 then let resp$(rc+=1)="True" else let resp$(rc+=1)="False"
42180     let fnopt(3,3,"Penalties",0,fratype)
42200     if sel_code=3 then let resp$(rc+=1)="True" else let resp$(rc+=1)="False"
42220     let fnopt(4,3,"Collections",0,fratype)
42240     if sel_code=4 then let resp$(rc+=1)="True" else let resp$(rc+=1)="False"
42260     let fnopt(5,3,"Credit Memos",0,fratype)
42280     if sel_code=5 then let resp$(rc+=1)="True" else let resp$(rc+=1)="False"
42300     let fnopt(6,3,"Debit Memos",0,fratype)
42320     if sel_code=6 then let resp$(rc+=1)="True" else let resp$(rc+=1)="False"
42340     let fnfra(1,30,3,42,"Date Range","You can transactions for any date range or leave these blank to see all transactions.")
42360     let cf+=1 : let fradate=cf : let mylen=26 : let mypos=mylen+2
42380     let fnlbl(1,1,"Starting Date:",mylen,1,0,fradate)
42400     let fntxt(1,mypos,10,0,1,"3",0,empty$,fradate)
42420     if beg_date=0 then let beg_date=date('mm')*10000+100+date('yy')-1
42440     let resp$(rc+=1)=str$(beg_date)
42460     let fnlbl(2,1,"Ending Date:",mylen,1,0,fradate)
42480     let fntxt(2,mypos,10,0,1,"3",0,empty$,fradate)
42500     let resp$(rc+=1)=str$(end_date)
42520     let fnfra(6,30,2,60,"Account","You review transactions for all accounts or for an individual.")
42540     let cf+=1 : let fraaccount=cf
42560     let fnlbl(1,1,"Account:",8,1,0,fraaccount)
42580     if trim$(hact$)='' then 
42600       let fncmbact(1,10,1,fraaccount)
42620       let rc+=1
42640       if resp$(rc)="" then let resp$(rc)="[All]"
42660     else 
42680       let fntxt(1,10,10,0,1,'',1,'',fraaccount) ! fntxt(lyne,ps,width;maxlen,ali,mask$,disable,tooltip$*300,contain,tabcon,addtomask$*40)
42700       let resp$(rc+=1)=hact$
42720     end if 
42740     if trim$(hact$)<>"" then let resp$(rc)=hact$ else if resp$(rc)="" then let resp$(rc)="[All]"
42760     let fncmdkey("Next",1,1,0,"Displays a list of transactions on the screen")
42780     let fncmdkey("Print",2,0,0,"Prints a transaction listing. (To get totals, you can only select one type of transaction at a time.")
42800     let fncmdkey("Cancel",5,0,1,"Returns to customer record")
42820     let fnacs(sn$,0,mat resp$,ckey)
43000     if ckey=5 then goto L810
43020     if resp$(1)="True" then 
43040       let sel_code=1
43060     else if resp$(2)="True" then 
43080       let sel_code=2
43100     else if resp$(3)="True" then 
43120       let sel_code=3
43140     else if resp$(4)="True" then 
43160       let sel_code=4
43180     else if resp$(5)="True" then 
43200       let sel_code=5
43220     else if resp$(6)="True" then 
43240       let sel_code=6
43260     end if 
43280     let beg_date=val(resp$(7))
43300     let end_date=val(resp$(8))
43320     let z$=resp$(9)(1:10)
43340     L810: !
43360   return  ! /r
44000   Tf_XIT: ! 
45000 fnend 
46000 def fn_TransactionEdit(editrec)
46020   open #trans=fngethandle: "Name="&env$('Q')&"\UBmstr\ubtransvb.h"&env$('cno')&",Shr",internal,outin,relative 
46040   read #trans,using "Form pos 1,c 10,N 8,N 1,pd 4.2",rec=editrec: p$,tdate,tcode,tamount
46060   let fntos(sn$="Transaction-3")
46080   let lc=rc=0 : let mylen=20 : let mypos=mylen+2
46100   let fnlbl(lc+=1,1,"Record:",mylen)
46120   let fntxt(lc,mypos,10,0,0,empty$,1)
46140   let resp$(rc+=1)=str$(editrec)
46160   let fnlbl(lc+=1,1,"Customer:",mylen)
46180   let fntxt(lc,mypos,10,0,0,empty$,1)
46200   let resp$(rc+=1)=p$
46220   let fnlbl(lc+=1,1,"Date:",mylen)
46240   let fntxt(lc,mypos,10,0,0,"3")
46260   let resp$(respc_tDate:=rc+=1)=str$(tdate)
46280   let fnlbl(lc+=1,1,"Type:",mylen)
46300   let fntxt(lc,mypos,10,0,0,empty$,1)
46320   let resp$(rc+=1)=transtype$(tcode)
46340   let fnlbl(lc+=1,1,"Amount:",mylen)
46360   let fntxt(lc,mypos,10,0,0,"10",1)
46380   let resp$(rc+=1)=str$(tamount)
46400   let fncmdkey('Save',1,1,0)
46420   let fncmdkey('Cancel',5,0,1)
46440   let fnacs(sn$,0,mat resp$,ckey)
46460   if ckey=1 then 
46480     let tdate=val(resp$(respc_tDate))
46500     rewrite #trans,using "Form pos 11,N 8",rec=editrec: tdate
46520   end if 
46540   close #trans: 
46560 fnend
48000 def fn_PRINTTRANS ! very local function - lots of inherritance
48020   dim scr1$(10)*30,alloc(10),nam$*30
48040   dim r(20,4),hd1$*255,servicename$(10)*20,tg(11),metraddr$*30
48060   dim srv$(10)*2
48080   dim name$(10)*20
48100   ! r: ask print_balance
48120   dim msgbox$(3)*128
48140   if env$('client')="White Hall" then 
48160     let msgbox_default=0
48180   else 
48200     let msgbox_default=256
48220   end if 
48240   let msgbox$(1)="Include balance column?"
48260   let msgbox$(2)="The balances listed were the account balance at the time the transaction completed"
48280   let msgbox$(3)="and will be misleading if transactions were processed out of date sequence."
48300   let fnmsgbox(mat msgbox$,resp$,cap$,32+3+msgbox_default)
48320   if resp$='Cancel' then goto PT_XIT
48340   if resp$='Yes' then 
48360     let print_balance=1
48380   else 
48400     let print_balance=0
48420   end if 
48440   ! /r
48460   let fnopenprn
48480   if trim$(servicename$(3))<>"Electric" and srv$(3)="EL" then let ptShowElecUsed=1 ! electric readings are being used for a reduction meter
48500   if trim$(servicename$(4))<>"Gas" and srv$(4)="GA" then let ptShowGasUsed=1 ! gas readings are being used for a reduction meter
48520   if trim$(z$)="[All]" then let hd1$="    {\ul Account        Date   }" else let hd1$="    {\ul    Date   }"
48540   let sz1=0
48560   let x=0
48580   for j=1 to 10
48600     if j=3 and ptShowElecUsed=1 then goto L1010 ! skp heading is electric field is used to hold other readings w/o matching changes (eg Kimberling City as reduction meters)
48620     if j=4 and ptShowGasUsed=1 then goto L1010 ! skp heading is gas field is used to hold other readings w/o matching changes (eg Kimberling City as reduction meters)
48640     let x2=pos(trim$(servicename$(j))," ",1)
48660     if x2>0 then let servicename$(j)=servicename$(j)(1:2)&"-"&servicename$(j)(x2+1:len(servicename$(j))) ! if service name two words long, use part of both
48680     if trim$(servicename$(j))<>"" then 
48700       let scr1$(sz1+=1)=servicename$(j)
48720       let hd1$=hd1$&"  {\ul "&lpad$(rtrm$(servicename$(j)(1:6)),6)&"}" : let name$(x+=1)=servicename$(j)
48740     end if  ! trim$(servicename$(j))<>"" then
48760     L1010: ! 
48780   next j
48800   let hd1$=hd1$&"{\ul     Total}"
48820   if print_balance then 
48840     let hd1$=hd1$&"  {\ul   Balance }" 
48880   else
48900     if trim$(servicename$(1))="Water" then 
48920       let hd1$=hd1$&"  {\ul   Wa Used }" 
48940       let water=1
48960     end if 
48980     if trim$(servicename$(3))="Electric" then 
49000       let hd1$=hd1$&"  {\ul   El Used }" 
49020       let electric=1 
49040     else if ptShowElecUsed=1 then 
49060       let hd1$=hd1$&"  {\ul      Used }" 
49080       let electric=1
49100     end if 
49120     if trim$(servicename$(4))="Gas" then 
49140       let hd1$=hd1$&"  {\ul   Ga Used }" 
49160       let gas=1 
49180     else if ptShowGasUsed=1 then 
49200       let hd1$=hd1$&"  {\ul      Used }" 
49220       let gas=1
49240     end if
49260   end if
49280   mat scr1$(sz1)
49300   mat alloc(sz1) : mat totalalloc(sz1)
49320   mat totalalloc=(0) : mat totalusage=(0) : let totaltamount=0
49340   close #trans: ioerr ignore
49360   open #trans=2: "Name="&env$('Q')&"\UBmstr\ubtransvb.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubTrIndx.h"&env$('cno')&",Shr",internal,outin,keyed 
49380   open #h_customer:=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,input,keyed 
49400   if trim$(z$)="[All]" then restore #trans: : goto L1160
49420   read #h_customer,using 'Form POS 11,c 30,C 28,pos 292,PD 4.2',key=lpad$(rtrm$(z$),10),release: metraddr$,nam$,account_balance nokey PT_NO_CUSTOMER
49440   restore #trans,key>=lpad$(rtrm$(z$),10)&"         ": nokey PT_FINIS
49460   L1160: !
49480   gosub HDR
49500   do 
49520     PT_TRANS_READ: ! 
49540     read #trans,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1': p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof PT_FINIS
49560     if trim$(z$)="[All]" then goto L1200 ! skip verifying customer if all selected
49580     if p$<>lpad$(rtrm$(z$),10) then goto PT_FINIS
49600     L1200: !
49620     if beg_date<>0 and tdate<beg_date then goto PT_TRANS_READ
49640     if end_date<>0 and tdate>end_date then goto PT_TRANS_READ
49660     if tamount=0 then goto PT_TRANS_READ
49680     if sel_code>1 and tcode<>sel_code-1 then goto PT_TRANS_READ
49700     ! 
49720     if tcode=3 then let ti2=1 ! REG.COLLECTION
49740     if tcode=4 then let ti2=2 ! CREDIT MEMO
49760     if tcode=5 then let ti2=3 ! DEBIT MEMO
49780     if ti2=3 then let r(1,1)-=tamount else let r(1,1)+=tamount
49800     let r(1,ti2+1)+=tamount
49820     let x=0
49840     for j=1 to 10
49860       if trim$(servicename$(j))="" then goto L1370
49880       if j=3 and (trim$(servicename$(j))<>"Electric" or trim$(servicename$(j))<>"Lawn Meter") and srv$(j)="EL" then goto L1370 ! electic being used for reduction meter
49900       if j=4 and trim$(servicename$(j))<>"Gas" and srv$(j)="GA" then goto L1370 ! gas being used for reduction meter
49920       let alloc(x+=1)=tg(j)
49940       if ti2=3 then let r(x+3,1)-=tg(j) else let r(x+3,1)+=tg(j)
49960       let r(x+3,ti2+1)+=tg(j)
49980       L1370: ! 
50000     next j
50020     let c$=" "
50040     if tcode=1 then let c$="CHG"
50060     if tcode=2 then let c$="PN"
50080     if tcode=3 then let c$="COL"
50100     if tcode=4 then let c$="CM"
50120     if tcode=5 then let c$="DM"
50140     let service=0
50160     if water=1 then let service+=1: let usage(service)=wu ! water
50180     if electric=1 then let service+=1: let usage(service)=eu ! Electric
50200     if gas=1 then let service+=1: let usage(service)=gu ! Gas
50220     if print_balance then 
50240       let printlineform$="c 4,PIC(ZZZZ/ZZ/ZZ),SZ1*N 8.2,n 10.2,3*pic(--------.--),x 1"
50260       let usage(1)=tbal
50280     else 
50300       let printlineform$="c 4,PIC(ZZZZ/ZZ/ZZ),SZ1*N 8.2,n 10.2,3*pic(zzzzzzzzzzz),x 1"
50320     end if 
50340     if trim$(z$)="[All]" then 
50360       print #255,using 'Form POS 1,c 10,x 1,'&printlineform$: p$,c$,tdate,mat alloc,tamount,usage(1),usage(2),usage(3) pageoflow PGOF
50380     else 
50400       print #255,using 'Form POS 1,'&printlineform$: c$,tdate,mat alloc,tamount,usage(1),usage(2),usage(3) pageoflow PGOF
50420     end if  ! trim$(z$)="[All]"   /   else 
50440     if tcode=1 then mat totalalloc=totalalloc+alloc: let totaltamount+=tamount: mat totalusage=totalusage+usage ! charges
50460     if tcode=2 then mat totalalloc=totalalloc+alloc: let totaltamount+=tamount ! penalties
50480     if tcode=3 then mat totalalloc=totalalloc+alloc: let totaltamount+=tamount ! collections
50500     if tcode=4 then mat totalalloc=totalalloc-alloc: let totaltamount-=tamount ! credit memos
50520     if tcode=5 then mat totalalloc=totalalloc+alloc: let totaltamount+=tamount ! debit memos
50540   loop 
50560   PGOF: ! r:
50580     print #255: newpage
50600     gosub HDR
50620   continue  ! /r
50640   HDR: ! r:
50660   ! need date$,time$
50670     print #255: "\qc  {\f181 \fs20 \b "&env$('cnam')&" }"
50680     print #255: "\qc  {\f181 \fs20 \b "&trim$(nam$)&" }"
50700     print #255: "\qc  {\f181 \fs20 \b "&trim$(metraddr$)&" }"
50720     print #255: "\qc  {\f181 \fs20 \b "&trim$(z$)&" }"
50740     print #255: "\qc  {\f181 \fs28 \b Transaction List }"
50760     if beg_date<>0 and end_date<>0 then 
50780       print #255: "\qc  {\f181 \fs18 \b From "&cnvrt$("pic(zzzz/zz/zz)",beg_date)& "  To "&cnvrt$("pic(zzzz/zz/zz)",end_date)&"}"
50800     end if  ! beg_date<>0 and end_date<>0
50820     print #255: ""
50840     print #255: "\ql "
50860     print #255: hd1$
50880   return  ! /r
50900   PT_FINIS: ! 
50920   print #255,using "form skip 1,pos 10,c 20": "Totals"
50940   for j=1 to udim(alloc)
50960     print #255,using "form pos 1,c 20,pic(---,---,---.##)": name$(j),totalalloc(j)
50980   next j
51000   print #255,using "form pos 1,c 20,pic(---,---,---.##)": "Total Amount",totaltamount
51020   if water=1 then print #255,using "form pos 1,c 20,pic(---,---,---)": "Water Usage",totalusage(1)
51040   if electric=1 and water=1 then print #255,using "form pos 1,c 20,pic(---,---,---)": "Electric Usage",totalusage(2) ! electric 2nd metered service
51060   if electric=1 and water=0 then print #255,using "form pos 1,c 20,pic(---,---,---)": "Electric Usage",totalusage(1) ! electric is 1st metered service
51080   if gas=1 and electric=1 and water=1 then print #255,using "form pos 1,c 20,pic(---,---,---)": "Gas Usage",totalusage(3) ! gas is third service
51100   if gas=1 and electric=0 and water=1 then print #255,using "form pos 1,c 20,pic(---,---,---)": "Gas Usage",totalusage(2) ! gas is second metered service
51120   if gas=1 and electric=0 and water=0 then print #255,using "form pos 1,c 20,pic(---,---,---)": "Gas Usage",totalusage(1) ! gas is first  metered service
51140   print #255,using "form skip 1,pos 1,cr 18,pic(-,---,---,--#.##)": "Current Balance:",account_balance
51160   PT_NO_CUSTOMER: ! 
51180   close #h_customer: ioerr ignore
51200   close #trans: ioerr ignore
51220   let fncloseprn
51240   PT_XIT: ! 
51260 fnend
62000 def fn_flextran(myline,mypos; hTrans,z$,begdate,enddate,selcode)
62060   ! ___________________________________________
62080   dim colmask$(30),colhdr$(30)*20,item$(25)*70,tg(11)
62100   dim srv$(10)*2,servicename$(10)*20
62120   ! ______________________________________________________________________
62200   if hTrans=0 then 
62220     let close_hTrans=1
62260     open #hTrans:=fngethandle: "Name="&env$('Q')&"\UBmstr\ubTransVB.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubTrIndx.h"&env$('cno')&",Shr",internal,input,keyed 
62280   end if 
62290   let hTrans_lrec_len=len(str$(lrec(hTrans)))
63000   fn_columnGet(mat colhdr$,mat colmask$,ftShowElecUsed,ftShowGasUsed)
63020   fn_columnEnabledGet(mat colEnabled) 
63030   forceAllColumnsOn=0
63040   if forceAllColumnsOn then mat colEnabled(25) : mat colEnabled=(1)
63060   if trim$(z$)="[All]" then 
63080     let z$=""
63100     colEnabled(2)=1
63120   else if trim$(z$)<>"" then 
63140     let z$=lpad$(trim$(z$),10)
63160     colEnabled(2)=0
63180   end if
64000   dim colHdr_enabled$(0)*20
64060   dim colMask_enabled$(0)
64070   colHeaderEnabledCount=0
64080   for hdrItem=1 to headerCount
64100     if colEnabled(hdrItem) then
64120       colHeaderEnabledCount+=1
64140       mat colHdr_enabled$(colHeaderEnabledCount)
64160       mat colMask_enabled$(colHeaderEnabledCount)
64180       colHdr_enabled$(colHeaderEnabledCount)=colhdr$(hdrItem)
64200       colMask_enabled$(colHeaderEnabledCount)=colmask$(hdrItem)
64220     end if
64240   nex hdrItem
64500   if trim$(z$)='' then 
64510     restore #hTrans: 
64520   else 
64530     restore #hTrans,key>=lpad$(z$,10)&"         ": nokey FlexTranFinis
64540   end if 
64560   fnflexinit1("ubtrans_b",myline,mypos,25,100,mat colHdr_enabled$,mat colMask_enabled$,1)
64670   do
66000     READ_UBTRANSVB: ! 
66020     read #hTrans,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1': p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof FlexTranFinis
66040     if lpad$(p$,10)<>lpad$(z$,10) and trim$(z$)<>'' then goto FlexTranFinis ! .     ! not same account
66060     if selcode>1 and tcode<>selcode-1 then goto READ_UBTRANSVB
66080     if begdate>20000000 and tdate<begdate then goto READ_UBTRANSVB
66100     if enddate>20000000 and tdate>enddate then goto READ_UBTRANSVB
66120     ! if tcode=0 then let tcode=1 ! temporary to prevent bad transaction codes
66140     items=0
66160     item$(items+=1)=lpad$(str$(rec(hTrans)),hTrans_lrec_len,'0')
66180     if colEnabled(2) then
66200        item$(items+=1)=p$
66220     end if
66240     item$(items+=1)=str$(tdate)
66260     if tcode<1 or tcode>udim(mat transtype$) then 
66280       item$(items+=1)='(invalid)'
66300     else
66320       item$(items+=1)=transtype$(tcode)
66340     end if
66360     item$(items+=1)=str$(tamount)
66380     colEnabledItem=items ! +1
66400     for j=1 to 10
66420       if j=3 and ftShowElecUsed=1 then goto L440
66440       if j=4 and ftShowGasUsed=1 then goto L440
66460       if trim$(servicename$(j))<>"" then 
66480         if colEnabled(colEnabledItem+=1) then
66500           ! pr colhdr$(colEnabledItem) : pause
66520           let item$(items+=1)=cnvrt$("pic(-------.zz)",tg(j))
66540         end if 
66560       end if 
66580       L440: ! 
66600     next j
66620     if colEnabled(colEnabledItem+=1) then
66640       ! pr colhdr$(colEnabledItem) : pause
66660       let item$(items+=1)=cnvrt$("pic(-------.zz)",tg(11)) ! net
66680     end if
66700     if trim$(servicename$(1))<>"" then 
66720       if colEnabled(colEnabledItem+=1) then
66740         ! pr colhdr$(colEnabledItem) : pause
66760         let item$(items+=1)=str$(wr)
66780       end if
66800       if colEnabled(colEnabledItem+=1) then
66820         let item$(items+=1)=str$(wu)
66840       end if
66860     end if 
66880     if trim$(servicename$(3))="Electric" or trim$(srv$(3))="EL" then 
66900       if colEnabled(colEnabledItem+=1) then
66920         let item$(items+=1)=str$(er)
66940       end if
66960       if colEnabled(colEnabledItem+=1) then
66980         let item$(items+=1)=str$(eu)
67000       end if
67020     end if 
67040     if trim$(servicename$(3))="Lawn Meter" then 
67060       if colEnabled(colEnabledItem+=1) then
67080         let item$(items+=1)=str$(er)
67100       end if
67120       if colEnabled(colEnabledItem+=1) then
67140         let item$(items+=1)=str$(eu)
67160       end if
68000     end if 
68020     if trim$(servicename$(4))="Gas" or trim$(srv$(4))="GA" then 
68040       if colEnabled(colEnabledItem+=1) then
68060         let item$(items+=1)=str$(gr) 
68080       end if
68100       if colEnabled(colEnabledItem+=1) then
68120         let item$(items+=1)=str$(gu)
68140       end if
68160     end if 
68180     if colEnabled(colEnabledItem+=1) then
68200       let item$(items+=1)=str$(tbal)
68220     end if
68240     let fnflexadd1(mat item$) 
68260   loop
68280   FlexTranFinis: ! 
68300   if close_hTrans=1 then close #hTrans: : let close_hTrans=0
68320 fnend 
72000 def fn_columnGet(mat colhdr$,mat colmask$,&ftShowElecUsed,&ftShowGasUsed)
72020   mat colhdr$(30)
72040   mat colmask$(30)
72060   let colhdr$(1)="Rec"
72080   let colhdr$(2)="Account"
72100   let colhdr$(3)="Date"
72120   let colhdr$(4)="Type"
72140   let colhdr$(5)="Amount"
72160   let colmask$(1)=""
72180   let colmask$(2)=""
72200   let colmask$(3)="3"
72220   let colmask$(4)=""
72240   let colmask$(5)="10"
72260   let headerCount=5
72280   if trim$(servicename$(3))<>"Electric" and srv$(3)="EL" then let ftShowElecUsed=1
72300   if trim$(servicename$(4))<>"Gas" and srv$(4)="GA" then let ftShowGasUsed=1
72320   for j=1 to 10
72340     if j=3 and ftShowElecUsed=1 then goto L220
72360     if j=4 and ftShowGasUsed=1 then goto L220
72380     if trim$(servicename$(j))<>"" then 
72400       let colhdr$(headerCount+=1)=trim$(servicename$(j))(1:min(8,len(trim$(servicename$(j)))))
72420       let colmask$(headerCount)="10"
72440     end if 
72460     L220: ! 
72480   next j
72500   let colhdr$(headerCount+=1)="Net" : let colmask$(headerCount)="10"
72520   for j=1 to 4
72540     if trim$(servicename$(j))<>"" and j=1 then 
72560       let colhdr$(headerCount+=1)="Water Reading"
72580       let colmask$(headerCount)="20"
72600       let colhdr$(headerCount+=1)="Water Used"
72620       let colmask$(headerCount)="20"
72640     end if 
72660     if trim$(servicename$(j))="Electric" and j=3 then 
72680       let colhdr$(headerCount+=1)="Elec Reading"
72700       let colmask$(headerCount)="20"
72720       let colhdr$(headerCount+=1)="Elec Used"
72740       let colmask$(headerCount)="20"
72760     else if trim$(srv$(j))="EL" and j=3 then 
72780       let colhdr$(headerCount+=1)=" 2nd Reading"
72800       let colmask$(headerCount)="20"
72820       let colhdr$(headerCount+=1)=" 2nd Used"
72840       let colmask$(headerCount)="20"
72860     end if 
72880     if trim$(servicename$(j))="Lawn Meter" and j=3 then 
72900       let colhdr$(headerCount+=1)="Lawn Reading"
72920       let colmask$(headerCount)="20"
72940       let colhdr$(headerCount+=1)="Lawn Used"
72960       let colmask$(headerCount)="20"
72980     end if 
73000     if uprc$(trim$(servicename$(j)))="GAS" and j=4 then 
73020       let colhdr$(headerCount+=1)="Gas Reading"
73040       let colmask$(headerCount)="20"
73060       let colhdr$(headerCount+=1)="Gas Used"
73080       let colmask$(headerCount)="20"
73100     else if uprc$(trim$(srv$(j)))="GA" and j=4 then 
73120       let colhdr$(headerCount+=1)="3nd Reading"
73140       let colmask$(headerCount)="20"
73160       let colhdr$(headerCount+=1)="3rd Used"
73180       let colmask$(headerCount)="20"
73200     end if 
73220   next j
73240   let colhdr$(headerCount+=1)="Balance"
73260   let colmask$(headerCount)="10"
73280   mat colhdr$(headerCount)
73300   mat colmask$(headerCount)
73320 fnend
76000 def fn_columnEnabledGet(mat colenabled) ! requires local: headerCount
76010   mat colenabled(headerCount)
76012   mat colenabled=(0)
76020   for hdrItem=1 to 5
76040     colenabled(hdrItem)=1
76060   nex hdrItem
76080   for hdrItem=6 to headerCount
76100     fncreg_read('Transaction Grid Column '&str$(hdrItem)&' Visible',tmp$,'True')
76110     ! pr 'read: Transaction Grid Column '&str$(hdrItem)&' Visible:'&tmp$
76120     if tmp$='True' then 
76140       colenabled(hdrItem)=1
76200     end if
76220   nex hdrItem
76230 ! pause
76240 fnend
78000 def fn_columnSelect
78020   dim csHeader$(30)*20
78040   fntos(sn$='ubTrColSel') : respc=0 : csLine=0
78060   fn_columnGet(mat csHeader$,mat unusedColMask$,unusedShowElecUsed,unusedShowGasUsed)
78080   for hdrItem=6 to udim(mat csHeader$)
78100     fnchk(csLine+=1,25,csHeader$(hdrItem), 1)
78120     fncreg_read('Transaction Grid Column '&str$(hdrItem)&' Visible',resp$(respc+=1),'True')
78130     ! pr 'read: Transaction Grid Column '&str$(hdrItem)&' Visible:'&resp$(respc)
78140   nex hdrItem
78150 ! pause
78160   fncmdset(4)
78180   fnacs(sn$,0,mat resp$,ckey)
78200   if ckey<>5 then
78210     respc=0
78220     for hdrItem=6 to udim(mat csHeader$)
78240       fncreg_write('Transaction Grid Column '&str$(hdrItem)&' Visible',resp$(respc+=1))
78250       ! pr 'wrote: Transaction Grid Column '&str$(hdrItem)&' Visible:'&resp$(hdrItem)
78260     nex hdrItem
78280   end if
78292 ! pause
78300 fnend
