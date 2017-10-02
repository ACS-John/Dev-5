00015 ! Customer File Editor
00020 library program$: fncustomer
00025 library 'S:\Core\Library': fnxit,fntop
00030 let fntop(program$)
00035 let fncustomer(x)
00040 let fnxit
00300 def library fncustomer(x)
00310   let fn_setup
00320   ! r: open files
00330   open #h_ubadrbil:=fngethandle: "Name="&env$('Q')&"\UBmstr\ubAdrBil.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\AdrIndex.h"&env$('cno')&",Shr,Use,RecL=130,KPs=1,KLn=10",internal,outin,keyed  ! was :=3
00332   F_ADRBIL: form pos 1,c 10,4*c 30
00340   gosub OPEN_CASS1
00360   fn_setup_depositChange ! INITIALIZE DEPOSIT TRACKING FILES
00401   ! r: BUD1: ! INITILIZE BUDGET FILE
00402   bud1=0
00404   open #h_budmstr:=fngethandle: "Name="&env$('Q')&"\UBmstr\BudMstr.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\BudIdx1.h"&env$('cno')&",Shr,Use,RecL=80,KPs=1,KLn=10",internal,outin,keyed  ! was 81
00405   F_BUDMSTR: form pos 1,c 10,pd 4,12*pd 5.2,2*pd 3
00406   open #h_budtrans:=fngethandle: "Name="&env$('Q')&"\UBmstr\BudTrans.h"&env$('cno')&",Shr,Use,RecL=149",internal,outin,relative  ! was 82
00407   F_BUDTRANS: form pos 1,c 10,2*pd 4,24*pd 5.2,2*pd 4,pd 3
00409   bud1=1
00410   ! /r
00470   open #h_customer_1:=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,outin,keyed ! 1
00480   open #h_customer_2:=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndx2.h"&env$('cno')&",Shr",internal,outin,keyed  ! 11
00490   open #h_customer_3:=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndx3.h"&env$('cno')&",Shr",internal,outin,keyed 
00500   open #h_customer_4:=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndx4.h"&env$('cno')&",Shr",internal,outin,keyed 
00510   open #h_customer_5:=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndx5.h"&env$('cno')&",Shr",internal,outin,keyed 
00512   F_CUSTOMER_1: form pos 1,c 10,4*c 30,c 12,7*pd 2,11*pd 4.2,4*pd 4,15*pd 5,pd 4.2,pd 4,12*pd 4.2,2*pd 3,c 7,2*c 12,pd 3,10*pd 5.2,pos 1712,c 1,c 9,c 2,c 17,n 2,n 7,2*n 6,n 9,pd 5.2,n 3,3*n 9,3*n 2,3*n 3,n 1,3*n 9,3*pd 5.2,c 30,7*c 12,3*c 30
00520   open #h_ubtransvb:=fngethandle: "Name="&env$('Q')&"\UBmstr\ubTransVB.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubTrIndx.h"&env$('cno')&",Shr,Use,RecL=102,KPs=1,KLn=19",internal,outin,keyed 
00530   open #h_citystzip:=fngethandle: "Name="&env$('Q')&"\Data\CityStZip.dat,KFName="&env$('Q')&"\Data\CityStZip.Idx,Use,RecL=30,KPs=1,KLn=30,Shr",internal,outin,keyed 
00542   ! /r
00554   ! 
00560   goto ASKACCT
00600   ! ______________________________________________________________________
00925   ACCOUNT_X_NOKEY: ! r:
00930     mat ml$(2)
00935     let ml$(1)="Account "&x$&' could not be found.'
00940     let ml$(2)="Select a different account."
00945     let fnmsgbox(mat ml$,resp$,cap$,48)
00950   goto ASKACCT ! /r
01000   EDIT_CUSTOMER: ! r:
01010   let jbact$=x$ ! ken 80905
01020   if len(x$)<>10 then goto ASKACCT
01030   read #h_customer_1,using F_CUSTOMER_1,key=x$: z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,f,mat g,mat adr,alp$,f$(2),f$(3),bra,mat gb,df$,dr$,dc$,da$,mat extra,mat extra$ nokey ACCOUNT_X_NOKEY
01032   gosub REMOVE_INCORRECT_ALLOCATIONS
01040   let holdz$=z$
01050   let olde3$=e$(3)
01054   EDIT_LOADED_CUSTOMER: ! 
01056     dim meterAddressBeforeEdit$*30
01058     if u4_meterAddress$='True' then meterAddressBeforeEdit$=e$(1)
01070     oldService1DepositAmount=b(8)
01072     oldService2DepositAmount=b(9)
01074     oldService3DepositAmount=b(10)
01075     oldService4DepositAmount=b(11)
01076     !   let old_gas_deposit=b(11)
01078     mat ab$=('')
01080     read #h_ubadrbil,using "Form POS 11,4*C 30",key=z$: mat ab$ nokey ignore
01090     ! let pb=bal
01100     let odp=b(8)+b(9)+b(11)
01110   goto NAMESCREEN ! /r
01120   ! ______________________________________________________________________
01130   CHECK_BALANCE_BREAKDOWN: ! r:
01140     gosub TGB_SET
01160     ! Gosub DRAFT1
01170     if tgb=bal then goto REWRITE_RECORD
01180   goto BREAKDOWN_NOT_EQUAL ! /r
01190   ! ______________________________________________________________________
01200   REWRITE_RECORD: ! r:
01210     gosub ALT_ADDRESS_SAVE ! rewrite alternate billing address
01220     if holdz$<>z$ then goto ASK_CONFIRM_KEY_CHANGE
01221     if u4_meterAddress$='True' then
01222       if meterAddressBeforeEdit$<>e$(1) then
01224         fnMeterAddressUpdate(meterAddressBeforeEdit$,e$(1))
01226       end if
01228     end if 
01230     release #h_customer_2: ioerr ignore
01240     rewrite #h_customer_1,using F_CUSTOMER_1,key=z$: z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,f,mat g,mat adr,alp$,f$(2),f$(3),bra,mat gb,df$,dr$,dc$,da$,mat extra,mat extra$
01242     if ad1 then let fn_record_previous_update(z$) ! &' '&e$(2))
01260     if oldService1DepositAmount<>b(8) then 
01263       fn_depositChangeLog(z$,oldService1DepositAmount,b(8),date('ccyymmdd'),trim$(srvnam$(1))(1:15)&' Deposit Changed')
01264     end if 
01265     if oldService2DepositAmount<>b(9) then 
01268       fn_depositChangeLog(z$,oldService2DepositAmount,b(9),date('ccyymmdd'),trim$(srvnam$(2))(1:15)&' Deposit Changed')
01269     end if 
01270     if oldService3DepositAmount<>b(10) then 
01273       fn_depositChangeLog(z$,oldService3DepositAmount,b(10),date('ccyymmdd'),trim$(srvnam$(3))(1:15)&' Deposit Changed')
01274     end if 
01275     if oldService4DepositAmount<>b(11) then 
01288       fn_depositChangeLog(z$,oldService4DepositAmount,b(11),date('ccyymmdd'),trim$(srvnam$(4))(1:15)&' Deposit Changed')
01290     end if 
01300     if olde3$=e$(3) then goto PAST_CASS_DELETE ! delete bar code if address changes
01310     if cassopen=0 then goto PAST_CASS_DELETE
01320     read #h_cass1,using 'Form POS 1,C 10,POS 96,C 12',key=z$: z2$,bc$ nokey PAST_CASS_DELETE
01330     delete #h_cass1,key=z$: ioerr ignore
01332   PAST_CASS_DELETE: ! 
01350   ! probably change customer in ubtrans-vb here !Gosub 5130
01380   if ad1=1 then goto ADD_RECORD else goto ASKACCT ! /r
01390   ! ______________________________________________________________________
01392   ASK_CONFIRM_KEY_CHANGE: ! r:
01394     mat ml$(2)
01396     let ml$(1)="Do you wish to change account"
01398     let ml$(2)='From "'&holdz$&'" to "'&z$&'"'
01400     let fnmsgbox(mat ml$,resp$,cap$,36)
01402     if uprc$(resp$(1:1))="Y" then 
01404       goto ACC_KEY_CHANGE_TEST_EXIST
01406     else if uprc$(resp$(1:1))="N" then 
01408       let z$=x$
01410       goto NAMESCREEN
01412     else 
01414       goto ASK_CONFIRM_KEY_CHANGE
01416     end if  ! /r
01424   ACC_KEY_CHANGE_TEST_EXIST: ! r:
01426     read #h_customer_1,using 'form pos 1,c 10',key=z$: p$ nokey ACC_KC_VALID_ROUTE_TEST
01428     mat ml$(2)
01430     let ml$(1)="Account "&trim$(z$)&" already exists."
01432     let ml$(2)="You must use a different account."
01434     let fnmsgbox(mat ml$,resp$,cap$,16)
01436     goto NAMESCREEN ! /r
01438   ACC_KC_VALID_ROUTE_TEST: ! r:
01440     if extra(1)<bkno1 or extra(1)>bkno2 then 
01442       mat ml$(2)
01444       let ml$(1)="You must have a valid route number!"
01446       let ml$(2)="(from "&bkno1$&" to "&bkno2$&")"
01448       let fnmsgbox(mat ml$,resp$,cap$,48)
01450       goto NAMESCREEN
01452     end if 
01480     ! 
01492     rewrite #h_customer_1,using F_CUSTOMER_1: z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,f,mat g,mat adr,alp$,f$(2),f$(3),bra,mat gb,df$,dr$,dc$,da$,mat extra,mat extra$ : if z$<>holdz$ or extra(1)<>holdroute or extra(2)>< holdseq then let fixgrid=1
01494     let fnkey_change(h_ubtransvb,'form pos 1,c 10',holdz$,z$) ! gosub REW_HIST ! change # in history transactions
01496     open #h_workorder:=fngethandle: "Name="&env$('Q')&"\UBmstr\WorkOrder.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\wkIndex.h"&env$('cno')&",Shr",internal,outin,keyed
01498     let fnkey_change(h_workorder,'form pos 1,c 10',holdz$,z$)
01500     close #h_workorder: 
01516     let fn_account_key_change_meter(holdz$,z$) ! gosub REW_HIST ! change # in history transactions
01522     ! 
01528     fnkey_change(h_deposit2,'form pos 1,c 10',x$,z$)
01530     gosub BUD3
01540     let notefile$=fn_notedir$&"\"&trim$(holdz$)&".txt" ! old notes
01550     let notefilenew$=fn_notedir$&"\"&trim$(z$)&".txt" ! new notes
01560     if exists(notefile$)<>0 then execute "rename "&notefile$&" "&notefilenew$&" -d -n"
01580   goto ASKACCT ! /r
01590   ! ______________________________________________________________________
01600   DONE: ! r:
01605     ! close #2: ioerr ignore
01620     close #h_customer_3: ioerr ignore
01630     close #h_customer_2: ioerr ignore
01640   goto XIT ! /r
01650   ! ______________________________________________________________________
01660   CONFIRM_DELETE: ! r:
01662     mat ml$(1)
01664     let ml$(1)="Do you wish to delete Account "&trim$(x$)&"?"
01666     let fnmsgbox(mat ml$,resp$,cap$,36)
01668     if uprc$(resp$)="YES" then let delconf$="Y" else let delconf$="N"
01670     if delconf$="N" then goto ASKACCT
01674     if bal<>0 then 
01676       mat ml$(3)
01678       let ml$(1)="You can not delete a customer with a balance."
01680       let ml$(2)="You must issue a debit memo or credit memo"
01682       let ml$(3)="to bring the balance to zero."
01684       let fnmsgbox(mat ml$,resp$,cap$,16)
01686       goto NAMESCREEN
01688     end if 
01690     gosub BUD4 ! delete budget info
01692     delete #h_customer_1,key=x$: 
01694     gosub DEL_CASS
01696     delete #h_ubadrbil,key=x$: nokey ignore
01698     gosub DEL_HIST
01700     let hact$=""
01702   goto ASKACCT ! /r
01840   ! ______________________________________________________________________
01850   ALT_ADDRESS_SAVE: ! r: write or rewrite alternate billing address
01855     rewrite #h_ubadrbil,using F_ADRBIL,key=z$: z$,mat ab$ nokey AAS_WRITE
01865     if trim$(ab$(1)&ab$(2)&ab$(3)&ab$(4))="" then 
01870       do 
01875         delete #h_ubadrbil,key=z$: ioerr AAS_DEL_ALL_END ! some how on conversion there can be several alternate addresses wtih same customer key (if delete any, delete them all)
01880       loop 
01885       AAS_DEL_ALL_END: ! 
01890     end if 
01895     goto AAS_FINIS
01900     AAS_WRITE: ! r:
01905     if trim$(ab$(1)&ab$(2)&ab$(3)&ab$(4))<>"" then 
01910       write #h_ubadrbil,using F_ADRBIL: z$,mat ab$
01915     end if 
01917     goto AAS_FINIS ! /r
01920     AAS_FINIS: ! 
01925     release #h_customer_2: ioerr ignore
01930   return  ! /r
01970   BREAKDOWN_NOT_EQUAL: ! r:
01972     mat ml$(4)
01974     let ml$(1)="Balance Breakdown does not equal Current Balance!"
01976     let ml$(2)="Balance = "&ltrm$(cnvrt$("pic($$$,$$$.## cr)",bal))
01978     let ml$(3)="Breakdown Balance = "&ltrm$(cnvrt$("pic($$$,$$$.## cr)",tgb))
01980     let ml$(4)="Difference = "&ltrm$(cnvrt$("pic($$$,$$$.## cr)",bal-tgb))
01982     let fnmsgbox(mat ml$,resp$,cap$,48)
01990   goto BILLING_INFO ! /r
02920   BUD2: ! r:
02922     if bud1=0 then goto NAMESCREEN
02930     let framelen=0
02940     for j=1 to 10
02950       if trim$(srvnam$(j))<>"" then let framelen=framelen+1
02960     next j
02970     let framelen=framelen+5
02980     mat ba=(0)
02990     mat badr=(0)
03000     br1=0 ! NO BUDGET RECORD
03010     read #h_budmstr,using F_BUDMSTR,key=z$: z$,mat ba,mat badr nokey L3040
03030     br1=1
03040     L3040: !
03041     let sn$="budget1"
03042     let fntos(sn$)
03050     let fnfra(1,1,framelen-1,45,"Budget Billing Information","Enter budget amounts to activate budget billing",0)
03060     let fnlbl(2,16,"Budget Amounts",20,2,3,1)
03070     let fnlbl(3,1,"Date:",20,1,0,1)
03080     let fntxt(3,22,8,8,1,"1",0,"Date budget billing approved (mmddyy format)",1)
03082     budgetinfo$(1)=str$(ba(1))
03090     let x=1 : let lyne =3
03100     for j=1 to 10
03110       if trim$(srvnam$(j))<>"" then  ! have this service
03120         let x+=1 : let lyne+=1
03122         let fnlbl(lyne,1,trim$(srvnam$(j))&":",20,1,0,1)
03130         let fntxt(lyne,22,10,0,1,"10",0,"Enter budget amounts where applicable.  All other services will be calculated as normal.",1)
03132         budgetinfo$(x)=str$(ba(j+1))
03140       end if
03142     next j
03150     let lyne+=1 : let fnlbl(lyne,1,"Net Bill:",20,1,0,1)
03160     let fntxt(lyne,22,10,10,1,"10",0,"Net would never have a value unless all items above were budgeted",1)
03162     budgetinfo$(x+=1)=str$(ba(12))
03170     let fncmdkey("&Next",1,1)
03172     let fncmdkey("Access &Transactions",8,1)
03174     let fncmdkey("&Delete",3,0)
03176     let fncmdkey("&Cancel",5,0,1)
03180     let fnacs(sn$,0,mat budgetinfo$,ckey) ! budget billing master record
03190     if ckey=5 then goto NAMESCREEN
03200     if ckey=8 then goto TRANS_ROUTINE
03210     let x=1: ba(1)=val(budgetinfo$(1)) conv ignore
03220     for j=2 to 11
03230       if trim$(srvnam$(j-1))<>"" then 
03232         let x+=1: ba(j)=val(budgetinfo$(x))
03234       end if 
03240     next j
03250     let x+=1: ba(12)=val(budgetinfo$(x))
03260     if br1=1 and ckey=3 then goto DEL_BUDGET_HISTORY ! if they choose to delete exiting record
03270     if br1=0 and ckey=3 then goto NAMESCREEN ! if press delete without writing a record
03280     if br1=0 then goto L3330 ! create record if none exists
03290     rewrite #h_budmstr,using F_BUDMSTR: z$,mat ba,mat badr
03300     if ckey=2 then goto L3350
03310   goto NAMESCREEN 
03320   ! 
03330   L3330: !
03333   if sum(ba)=0 then goto TRANS_ROUTINE
03340   write #h_budmstr,using F_BUDMSTR: z$,mat ba,mat badr
03350   L3350: !
03352   goto TRANS_ROUTINE
03360   ! /r
03370   DEL_BUDGET_HISTORY: ! r:
03371     mat ml$(3)
03372     let ml$(1)="You have chosen to delete all budget history"
03374     let ml$(2)="for this customer."
03376     let ml$(3)="Do you wish to continue?"
03378     let fnmsgbox(mat ml$,resp$,cap$,35)
03380     if uprc$(resp$)="YES" then goto DBH_DEL else goto DBH_XIT
03382     DBH_DEL: ! 
03384     if br1=1 then delete #h_budmstr,key=z$: nokey ignore
03386     DBH_XIT: ! 
03388   goto NAMESCREEN ! /r
14000   TRANS_ROUTINE: ! r:
14010     let ta1=badr(1)
14020     if ta1=0 and sum(ba)>0 then 
14030       let ta1=lrec(h_budtrans)+1
14040       mat bt1=(0)
14050       let nba=0
14060       write #h_budtrans,using F_BUDTRANS,rec=ta1: x$,mat bt1,nba
14070       badr(1)=ta1
14080       badr(2)=ta1
14090       rewrite #h_budmstr,using F_BUDMSTR,key=z$: z$,mat ba,mat badr
14100     end if 
14110     do while ta1<>0
14120       read #h_budtrans,using F_BUDTRANS,rec=ta1: x$,mat bt1,nba norec BUDTR_XIT
14130       ! BUDTRANS: ! budget transactions
14140       mat budgetinfo$(28)
14150       sn$="BUDGET"
14160       fntos(sn$)
14170       fnfra(1,1,framelen+1,50,"Budget Billing Transactions","Actual billing compared to budget billing for any month billed",0)
14180       fnlbl(2,22,"Budget     Actual",20,2,2,1)
14190       fnlbl(3,1,"Date:",20,1,0,1)
14200       budgetinfo$(1)=str$(bt1(1,1))
14210       fntxt(3,22,8,8,1,"1",0,'',1)
14220       budgetinfo$(2)=str$(bt1(1,2))
14230       fntxt(3,34,8,8,1,"1",0,empty$,1)
14240       x=2: let lyne=3
14250       for j=1 to 10
14260         if trim$(srvnam$(j))<>"" then ! they have this service
14270           x=x+2
14280           fnlbl(lyne+=1,1,trim$(srvnam$(j))&":",20,1,0,1)
14290           fntxt(lyne,22,10,10,1,"10",0,empty$,1)
14300           budgetinfo$(x-1)=str$(bt1(j+1,1))
14310           budgetinfo$(x)=str$(bt1(j+1,2))
14320           fntxt(lyne,34,10,10,1,"10",0,empty$,1)
14330         end if
14340       next j
14350       lyne+=1 : let fnlbl(lyne,1,"Net Bill:",20,1,0,1)
14360       budgetinfo$(x+=1)=str$(bt1(12,1))
14370       fntxt(lyne,22,10,10,1,"10",0,empty$,1)
14380       x+=1: : budgetinfo$(x)=str$(bt1(12,2))
14390       fntxt(lyne,34,10,10,1,"10",0,empty$,1)
14400       lyne+=1 : let fnlbl(lyne,1,"Gross Bill:",20,1,0,1)
14410       budgetinfo$(x+=1)=str$(bt1(13,1))
14420       fntxt(lyne,22,10,10,1,"10",0,empty$,1)
14430       budgetinfo$(x+=1)=str$(bt1(13,2))
14440       fntxt(lyne,34,10,10,1,"10",0,empty$,1)
14450       lyne+=1 : let fnlbl(lyne,1,"Date paid:",20,1,0,1)
14460       budgetinfo$(x+=1)=str$(bt1(14,1))
14470       fntxt(lyne,22,8,8,1,"1",0,'',1)
14480       budgetinfo$(x+=1)=str$(bt1(14,2))
14490       fntxt(lyne,34,8,8,1,"1",0,'',1)
14500       fncmdset(2)
14510       fnacs(sn$,0,mat budgetinfo$,ckey) ! budget billing transactions
14520       if ckey=5 then goto NAMESCREEN
14530       let x=0
14540       for j=1 to 14
14550         if j<2 or j>11 or trim$(srvnam$(j-1))<>"" then 
14560           let x=x+1
14570           bt1(j,1)=val(budgetinfo$(x*2-1))
14580           bt1(j,2)=val(budgetinfo$(x*2))
14590         end if
14600       next j
14610       rewrite #h_budtrans,using F_BUDTRANS,rec=ta1: x$,mat bt1,nba
14620       let ta1=nba 
14630     loop ! goto L3450
14640     BUDTR_XIT: ! 
14650   goto BUD2 ! /r
15000   BUD3: ! r:
15020     if bud1=0 then goto L3980 ! Account CHANGED
15040     read #h_budmstr,using 'form pos 1,c 10,pos 75,2*pd 3',key=x$: x$,mat badr nokey L3980
15060     rewrite #h_budmstr,using 'form pos 1,c 10,pos 75,2*pd 3': z$
15080     let tadr=badr(1)
15100     do
15120       if tadr=0 then goto L3980
15140       read #h_budtrans,using 'form pos 1,c 10,pos 147,pd 3',rec=tadr: x$,nba
15160       rewrite #h_budtrans,using 'form pos 1,c 10,pos 147,pd 3',rec=tadr: z$ ! 11/15/00  ADDED USING
15180       let tadr=nba
15200     loop
15220     L3980: ! 
15240   return ! /r
16000   BUD4: ! r:
16020   if bud1=0 then goto L4100 ! Account DELETED
16040    read #h_budmstr,using 'form pos 1,c 10,pos 75,2*pd 3',key=x$: x$,mat badr nokey L4100
16060    delete #h_budmstr: 
16080    let tadr=badr(1)
16100    do
16120      if tadr=0 then goto L4100
16140      read #h_budtrans,using 'form pos 1,c 10,pos 147,pd 3',rec=tadr: x$,nba
16160      delete #h_budtrans,rec=tadr: 
16180      let tadr=nba
16200    loop
16220    L4100: !
16240   return ! /r
17000   OPEN_CASS1: ! r:
17020     open #h_cass1:=fngethandle: "Name="&env$('Q')&"\UBmstr\Cass1.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\CASS1IDX.h"&env$('cno')&",Shr",internal,outin,keyed ioerr L4150 
17040     cassopen=1
17060     L4150: ! 
17080   return  ! /r
18000   DEL_CASS: ! r:
18020     if cassopen then 
18040       delete #h_cass1,key=x$: nokey ignore
18060     end if 
18080   return  ! /r
20000   DEL_HIST: ! r: Delete History with old account
20020     restore #h_ubtransvb,key>=x$&"         ": nokey DEL_HIST_FINIS
20040     do 
20060       read #h_ubtransvb,using 'form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1': p$,tdate eof DEL_HIST_FINIS
20080       if p$<>x$ then goto DEL_HIST_FINIS ! not same account
20100       if p$=x$ then delete #h_ubtransvb: 
20120     loop 
20140     DEL_HIST_FINIS: ! 
20160   return  ! /r
25000   NAMESCREEN: ! r: the main customer screen
25020     let fntos(sn$="custinfo")
25040     let respc=0 : let frac=0
25060     let mylen=25 : let mylen+2
25080     let fnlbl(1,1,"Account:",15,1)
25100     let fntxt(1,17,10,10,1)
25120     custinfo$(respc+=1)=trim$(z$)
25140     let fnlbl(1,26,"Route:",8,1)
25160     let fncmbrt2(1,36,1)
25180     custinfo$(respc+=1)=str$(extra(1))
25200     let fnlbl(1,45,"Sequence:",11,1)
25220     let fntxt(1,58,7,7,1,"30")
25240     custinfo$(respc+=1)=str$(extra(2))
25260     let fnfra(3,1,4,48,"Customer Information")
25280     let fracustinfo=frac+=1
25300     let fnlbl(1,1,"Name:",13,1,0,fracustinfo)
25320     let fntxt(1,15,25,30,0,"",0,"",fracustinfo)
25340     custinfo$(respc+=1)=e$(2)
25360     let fnlbl(2,1,"Address:",13,1,0,fracustinfo)
25380     let fntxt(2,15,25,30,0,"",0,"",fracustinfo)
25400     custinfo$(respc+=1)=e$(3)
25420     let fnlbl(3,1,"Address:",13,1,0,fracustinfo)
25440     let fntxt(3,15,25,30,0,"",0,"",fracustinfo)
25460     custinfo$(respc+=1)=extra$(1)
25480     let fnlbl(4,1,"City, St Zip:",13,1,0,fracustinfo)
25540     let fncombof("CityStZip",4,15,30,env$('Q')&"\Data\CityStZip.dat",1,30,0,0,env$('Q')&"\Data\CityStZip.idx",0,0, " ",fracustinfo,0)
25560     custinfo$(respc+=1)=e$(4)
25580     let fnlbl(9,1,"Meter Address:",mylen,1)
25600     let fntxt(9,27,20,30)
25620     custinfo$(respc+=1)=e$(1)
25622     if u4_meterAddress$='True' then
25626       fnbutton(9,48,'…',fkey_meterAddress:=54,"Select an existing Meter Address/Location ID",0,1)
25628       fnlbl(9,50,str$(fnMeterAddressLocationID(e$(1))),9,0,0,0,0,'Location ID')
25632     end if
25640     let fnlbl(10,1,"Alpha Sort Name:",mylen,1)
25660     let fntxt(10,27,7)
25680     custinfo$(respc+=1)=alp$
25700     let fnlbl(11,1,"Phone Number:",mylen,1)
25720     let fntxt(11,27,12)
25740     custinfo$(respc+=1)=extra$(2)
25760     let fnlbl(12,1,"Cell Phone Number:",mylen,1)
25780     let fntxt(12,27,12,12)
25800     custinfo$(respc+=1)=extra$(8)
25820     let fnlbl(13,1,"E-mail Address:",mylen,1)
25840     let fntxt(13,27,20,30,0,"",0)
25860     custinfo$(respc+=1)=extra$(9)
25880     let fnlbl(14,1,"Current Balance:",mylen,1)
25900     let fntxt(14,27,12,12,1,"10",1)
25920     custinfo$(respc+=1)=str$(bal)
25940     let fnlbl(15,1,"Last Billing Date:",mylen,1)
25960     let fntxt(15,27,8,8,1,"1")
25980     custinfo$(respc+=1)=str$(f)
26000     let fnlbl(16,1,"Current Reading Date:",mylen,1)
26010     let fntxt(16,27,8,0,1,"1")
26020     custinfo$(respc+=1)=str$(extra(3))
26030     let fnlbl(17,1,"Prior Reading Date:",mylen,1)
26040     let fntxt(17,27,8,0,1,"1")
26050     custinfo$(respc+=1)=str$(extra(4))
26060     let fnlbl(18,1,"Final Billing Code:",mylen,1)
26070     code$(1)="0 - Active"
26080     code$(2)="1 - Inactive / Final Billed"
26090     code$(3)="2 - Inactive / Deposit Refunded"
26100     code$(4)="3 - Active / but Do Not Bill"
26110     code$(5)="4 - Finaled / but Not Billed"
26120     let respc+=1 ! update counter for at least one final billing code
26130     for j=1 to udim(code$)
26140       if extra(17)=val(code$(j)(1:1)) then custinfo$(respc)=code$(j)
26150     next j
26160     let fncomboa("final_bill",18,27,mat code$,"",25)
26170     let fnlbl(19,1,"Bulk Sort Code:",mylen,1)
26180     let fntxt(19,27,12)
26190     custinfo$(respc+=1)=extra$(6)
26200     let fnlbl(20,1,"Last Estimation Date:",mylen,1)
26210     let fntxt(20,27,8,8,1,"1")
26220     custinfo$(respc+=1)=str$(extra(19))
26230     if env$('client')="Kincaid" then 
26240       fnlbl(21,1,"1=Wand 2=Manual:",mylen,1)
26250     else if env$('client')="Findlay" then 
26260       fnlbl(21,1,"Energy Assistance:",mylen,1)
26270     else
26280       fnlbl(21,1,"Test Circle Code:",mylen,1)
26290     end if
26300     if extra(22)<0 then let extra(22)=0
26310     if extra(22)=0 or extra(22)=2 then let do_not_use_alt_addr=1 else let do_not_use_alt_addr=0
26320     ! 
26330     let fntxt(21,27,12)
26340     custinfo$(respc+=1)=extra$(7)
26350     let fnfra(3,52,4,48,"Mailing Information","Mailing information is only necessary if different than the customer information",0)
26360     let fnlbl(1,1,"Name:",13,1,0,2)
26370     let fntxt(1,15,20,30,0,"",do_not_use_alt_addr,"Mailing information is only necessary if different than the customer information",2)
26380     custinfo$(respc+=1)=ab$(1)
26390     let fnlbl(2,1,"Address:",13,1,0,2)
26400     let fntxt(2,15,20,30,0,"",do_not_use_alt_addr,empty$,2)
26410     custinfo$(respc+=1)=ab$(2)
26420     let fnlbl(3,1,"Address:",13,1,0,2)
26430     let fntxt(3,15,20,30,0,"",do_not_use_alt_addr,empty$,2)
26440     custinfo$(respc+=1)=ab$(3)
26450     let fnlbl(4,1,"City, St Zip:",13,1,0,2)
26460     if do_not_use_alt_addr then 
26470       let fntxt(4,15,20,30,0,"",do_not_use_alt_addr,empty$,2)
26480     else 
26490       let fncombof("CityStZip",4,15,30,env$('Q')&"\Data\CityStZip.dat",1,30,0,0,env$('Q')&"\Data\CityStZip.idx",0,0, " ",2,0)
26500     end if 
26510     custinfo$(respc+=1)=ab$(4)
26520     !
26530     fnlbl(23,1,"Social Security Number:",24,1)
26540     fntxt(23,27,9,0,0,'30',0)
26550     custinfo$(resp_ssn=respc+=1)=str$(extra(20))
26560     fnlbl(24,1,"Work Phone Number:",24,1)
26570     fntxt(24,27,12,0,0,'',0,"Enter the customers work number.")
26580     custinfo$(resp_phone_work=respc+=1)=extra$(10)
26590     fnlbl(25,1,"Business Phone Number:",24,1)
26600     fntxt(25,27,12,0,0,'',0,"Enter the customers work number.")
26610     custinfo$(resp_phone_business=respc+=1)=extra$(11)
26620     fnbutton(27,6,fn_warn_text$(z$,"The last note line that begins with 'warn:' will be displayed here."),23,"The last note line that begins with 'warn:' will be displayed here. Click to edit notes.",0,95)
26630     if do_not_use_alt_addr then 
26640       fnbutton(1,37,"Don't Use",51,"Will use regular address on bills, etc. ",2,10,2)
26650     else 
26660       fnbutton(1,37,"Use",50,"Will use alternate address on bills, etc.",2,8,2)
26670     end if 
26950     nav_button_pos=76 : let nav_button_width=25
26960     fnlbl(9,nav_button_pos,"Additional Information",nav_button_width,2) !,3)
26980     fnbutton(10,nav_button_pos,"Servi&ces",20,"Service Code Information: Including rates codes, meter numbers, deposits, readings, usages, etc",0,nav_button_width)
27000     fnbutton(11,nav_button_pos,"Current &Bill and Breakdown",21,"Charges, balance, balance breakdown, net and gross bill",0,nav_button_width)
27020     fnbutton(12,nav_button_pos,"Bank &Draft Information",22,"Bank draft codes, routing numbers, bank numbers, etc",0,nav_button_width)
27040     fnbutton(13,nav_button_pos,"&Notes",23,"Add notes or footnotes pertaining to this customer",0,nav_button_width)
27060     fnbutton(14,nav_button_pos,"Deposit &History",25,"A record of all changes to customer deposit amounts",0,nav_button_width)
27080     fnbutton(15,nav_button_pos,"&Transaction History",26,"Transactions for all charges, collections, penalties, memos, etc. that have been processed on the customer.",0,nav_button_width)
27100     fnbutton(16,nav_button_pos,"Budget Billing In&formation",27,"Budget amounts and variances between budget and actual",0,nav_button_width)
27120     fnbutton(17,nav_button_pos,"Work &Orders",28,"Print a work order on this customer",0,nav_button_width/2)
27122     fnbutton(17,nav_button_pos+nav_button_width/2+1,"Print History",29,"Review descriptions of past work orders.",0,nav_button_width/2-1)
27160     fncmdkey("&Save",1,1,0,"Saves all changes or new information")
27180     if ad1=0 then 
27200       fncmdkey("Delete",4,0,0,"Deletes this record")
27220     end if 
27240     fncmdkey("&Cancel",5,0,1,"Stops without recording any changes")
27250     fnacs(sn$,0,mat custinfo$,ckey) ! CALL main screen
27260     if ckey=5 then 
27270       release #h_customer_1: ioerr ignore
27280       release #h_ubadrbil: ioerr ignore
27290       if ad1=1 then 
27300         goto ADD_CANCEL
27310       else 
27320         goto ASKACCT
27330       end if 
27340     end if 
27350     z$=lpad$(trim$(custinfo$(1)),10) : if ckey<>5 then let rp_prev$(1)=z$ ! important in case of an account number change
27360     extra(1)=val(custinfo$(2))
27370     extra(2)=val(custinfo$(3))
27380     e$(2)=custinfo$(4)
27400     e$(3)=custinfo$(5)
27420     extra$(1)=custinfo$(6)
27440     e$(4)=custinfo$(7)
27460     e$(1)=custinfo$(8)
27480     if trim$(e$(1))="" then let e$(1)=e$(3) ! set meter address same as customer address if left blank
27500     citykey$=rpad$(e$(4),30)
27520     read #h_citystzip,using 'form pos 1,c 30',key=citykey$,release: citystzip$ nokey L5430
27540     goto L5440
27560     L5430: ! 
27570     write #h_citystzip,using 'form pos 1,c 30': e$(4)
27580     L5440: ! 
27600     alp$=custinfo$(9)
27620     extra$(2)=custinfo$(10)
27640     extra$(8)=custinfo$(11)
27660     extra$(9)=custinfo$(12)
27680     bal=val(custinfo$(13))
27700     f=val(custinfo$(14))
27720     extra(3)=val(custinfo$(15))
27740     extra(4)=val(custinfo$(16))
27760     extra(17)=val(custinfo$(17)(1:1))
27780     extra$(6)=custinfo$(18)
27800     extra(19)=val(custinfo$(19))
27820     extra$(7)=custinfo$(20)
27840     ab$(1)=custinfo$(21)
27860     ab$(2)=custinfo$(22)
27880     ab$(3)=custinfo$(23)
27900     ab$(4)=custinfo$(24)
27902     extra(20)=val(custinfo$(resp_ssn))
27904     extra$(10)=custinfo$(resp_phone_work)
27906     extra$(11)=custinfo$(resp_phone_business)
27920     citykey$=rpad$(ab$(4),30)
27940     ! r: add city state zip to h_citystzip file (if it does not exist)
27960     read #h_citystzip,using 'form pos 1,c 30',key=citykey$,release: citystzip$ nokey L5520
27980     goto L5530
28000     L5520: ! 
28020     write #h_citystzip,using 'form pos 1,c 30': ab$(4)
28040     L5530: ! 
28060     ! /r
28080     if ckey=4 and ad1=0 then goto CONFIRM_DELETE ! delete account
28100     if extra(2)=0 then 
28120       mat ml$(1)
28140       ml$(1)="Sequence number is required!"
28160       fnmsgbox(mat ml$,resp$,cap$,48)
28180       goto NAMESCREEN
28200     else if extra(1)<bkno1 or extra(1)>bkno2  then 
28220       mat ml$(2)
28240       ml$(1)="You must have a valid route number within the range of "&bkno1$&" and "&bkno2$&"!"
28260       ml$(2)="You can use Company > Configure to set the route number range.."
28280       fnmsgbox(mat ml$,resp$,cap$,48)
28300       goto NAMESCREEN
28320     end if 
28340     if sum(gb)<>bal then goto CHECK_BALANCE_BREAKDOWN
28360     if ckey=1 then 
28380       goto REWRITE_RECORD
28400     else if ckey=2 then 
28420       goto REWRITE_RECORD
28440     else if ckey=20 then 
28460       if trim$(srvnam$(1))<>'' then 
28480         goto SERVICE1
28500       else if trim$(srvnam$(2))<>'' then 
28520         goto SERVICE2
28540       else if trim$(srvnam$(4))<>'' then 
28560         goto SERVICE4
28580       else if trim$(srvnam$(5))<>'' then 
28600         goto SERVICE5
28620       else if trim$(srvnam$(6))<>'' then 
28640         goto SERVICE6
28660       else if trim$(srvnam$(7))<>'' then 
28680         goto SERVICE7
28700       else if trim$(srvnam$(8))<>'' then 
28720         goto SERVICE8
28740       else if trim$(srvnam$(9))<>'' then 
28760         goto SERVICE9
28780       else if trim$(srvnam$(10))<>'' then 
28800         goto SERVICE10
28820       else 
28840         pr 'no services';bell : goto NAMESCREEN
28860       end if 
28880     else if ckey=21 then 
28900       goto BILLING_INFO
28920     else if ckey=22 then 
28940       goto BANK_DRAFT
28960     else if ckey=23 then 
28980       fn_customerNotes(z$)
28990       goto NAMESCREEN 
29000     else if ckey=25 then 
29020       goto DEPOSIT_HIST
29040     else if ckey=26 then 
29060       goto TRANS_HIST
29080     else if ckey=27 then 
29100       goto BUD2
29120     else if ckey=28 then 
29140       let fnWorkOrderAdd(holdz$)
29160       goto NAMESCREEN 
29180     else if ckey=29 then
29200       fnWorkOrderList(z$)
29220       goto NAMESCREEN
29240     else if ckey=50 then 
29260       let extra(22)=2 : goto NAMESCREEN
29280     else if ckey=51 then 
29300       let extra(22)=1 : goto NAMESCREEN
29400     else if ckey=fkey_meterAddress then ! if ckey=54
29420       dim meterAddressBeforeSelect$*30
29440       meterAddressBeforeSelect$=e$(1)
29460       e$(1)=fnfm$('MeterAddressSelect',cnvrt$('N 11',fnMeterAddressLocationID(e$(1))),9,27)
29480       if e$(1)='' then 
29500         e$(1)=meterAddressBeforeSelect$
29520       else
29540         e$(1)=fnMeterAddressName$(val(e$(1)))
29560       end if
29580       goto NAMESCREEN
29600     end if 
29620   ! /r (namescreen)
32000   GET_CODES: ! fnget_codes(service_code,&ratecode,mat rates$ ) requires h_rate1 r: get applicable rate codes
32020     ! search routine must be passed code for service (WA for water) in searchcode$
32040     if ~gcode_setup then 
32060       let gcode_setup=1
32080       open #h_rate1:=51: "Name="&env$('Q')&"\UBmstr\ubData\RateMst.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubData\RateIdx1.h"&env$('cno')&",Use,RecL=374,KPs=1,KLn=4,Shr",internal,outin,keyed 
32082       open #h_rate2:=52: "Name="&env$('Q')&"\UBmstr\ubData\RateMst.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubData\RateIdx2.h"&env$('cno')&",Use,RecL=374,KPs=5,KLn=25,Shr",internal,outin,keyed 
32100     end if 
32120     restore #h_rate1: 
32140     mat rates$(99)
32160     mat rates$=("")
32180     let fncreg_read('default rate '&str$(service_code),tmp_rate$)
32200     if tmp_rate$<>'' then 
32240       let x=0
32260       if ratecode=0 then 
32280         let ratecode=val(tmp_rate$(1:pos(tmp_rate$,'=')-1))
32300       end if 
32320     else 
32340       let x=1
32360       let rates$(1)=" 0=Not applicable"
32380     end if 
32400     do 
32420       read #h_rate1,using "Form POS 1,C 54",release: rt$ eof GCODE_FINIS
32440       if trim$(rt$(1:2))=searchcode$ then 
32460         let x=x+1
32480         let rates$(x)=rt$(3:4)&"="&rt$(5:25)
32500         if ratecode=val(rt$(3:4)) then let rateinfo$(3)=rt$(3:4)&"="&rt$(5:25)
32520       end if 
32540     loop 
32560     GCODE_FINIS: ! 
32580     if x>0 then mat rates$(x) else mat rates$(1)
32600     if ratecode=0 then let rateinfo$(3)=" 0=Not applicable"
32620   return  ! /r
34000   BILLING_INFO: ! r:
34020     let sn$="billing_info"
34040     let fntos(sn$)
34060     let fnlbl(1,14,"Billing Information",30,2,4)
34080     let fnlbl(2,1,"Account:",10,1)
34100     let fntxt(2,12,10,0,1,'',1)
34120     bxnf$(1)=z$
34140     let fnlbl(2,24,"Name:",5,1)
34160     let fntxt(2,31,25,30,0,'',1)
34180     bxnf$(2)=e$(2)
34200     let fnfra(3,1,16,49,'')
34220     let fnlbl(1,1,"Date of Charge:",14,1,0,1)
34240     let fntxt(1,16,8,0,0,'1',0,'',1)
34260     bxnf$(3)=str$(f)
34280     let fnlbl(2,1,"Balance:",8,0,0,1)
34300     let fntxt(2,10,10,0,1,'10',1,'',1)
34320     bxnf$(4)=str$(bal)
34340     if uprc$(escrow$)="Y" then 
34360       let fnlbl(2,21,"Escrow Balance:",15,1,0,1)
34380       let fntxt(2,38,9,0,1,'10',0,'',1)
34400       bxnf$(5)=str$(extra(23)) ! escrow balance
34420     end if 
34440     let fnlbl(3,19,"Current      Balance",25,2,2,1)
34460     let fnlbl(4,20,"    Bill      Breakdown",25,2,2,1)
34480     let lyne=4
34500     if uprc$(escrow$)="Y" then billinfo=5 else billinfo=4
34520     for j=1 to 10
34540       if rtrm$(srvnam$(j))<>"" then 
34560         let lyne+=1
34580         let fnlbl(lyne,1,trim$(srvnam$(j))&":",16,1,0,1)
34600         let fntxt(lyne,19,10,0,1,'10',0,'',1)
34620         bxnf$(billinfo+=1)=str$(g(j))
34640         let fntxt(lyne,33,10,0,1,'10',0,'',1)
34660         bxnf$(billinfo+=1)=str$(gb(j))
34680       end if 
34700     next j
34720     let lyne+=1 : let fnlbl(lyne,1,"Net Bill:",16,1,0,1)
34740     let fntxt(lyne,19,10,0,1,'10',0,'',1)
34760     bxnf$(billinfo+=1)=str$(g(11))
34780     let lyne+=1 : let fnlbl(lyne,1,"Gross Bill:",16,1,0,1)
34800     let fntxt(lyne,19,10,0,1,'10',0,'',1)
34820     bxnf$(billinfo+=1)=str$(g(12))
34840     let fncmdset(2)
34860     let fnacs(sn$,0,mat bxnf$,ckey) ! billing information
34880     if ckey=5 then goto NAMESCREEN
34900     let f=val(bxnf$(3))
34920     bal=val(bxnf$(4))
34940     if uprc$(escrow$)="Y" then let extra(23)=val(bxnf$(5))
34960     if uprc$(escrow$)="Y" then billinfo=5 else billinfo=4
34980     for j=1 to 10
35000       if rtrm$(srvnam$(j))<>"" then 
35020         billinfo=billinfo+1 : let g(j)=val(bxnf$(billinfo))
35040         billinfo=billinfo+1 : let gb(j)=val(bxnf$(billinfo))
35060       end if 
35080     next j
35100     billinfo=billinfo+1 : let g(11)=val(bxnf$(billinfo))
35120     billinfo=billinfo+1 : let g(12)=val(bxnf$(billinfo))
35140   goto NAMESCREEN ! /r
36000   BANK_DRAFT: ! r:
36020     let sn$="bank_draft"
36040     let fntos(sn$)
36060     let fnlbl(1,9,"Bank Draft Information",40,2,4)
36080     let fnlbl(2,1,"Account:",10,1)
36100     let fntxt(2,12,10,10,1,'',1)
36120     let dri$(1)=z$
36140     let fnlbl(2,24,"Name:",5,1)
36160     let fntxt(2,31,25,30,0,'',1)
36180     let dri$(2)=e$(2)
36200     ! 
36220     let fnlbl(4,3,"Bank Draft (Y/N):",18)
36240     let fntxt(4,20,1,0,0,"",0,"Use Y to specify the customer has requested a bank draft")
36260     if uprc$(df$)="1" or uprc$(df$)="Y" then 
36280       let dri$(3)="Y"
36300     else 
36320       let dri$(3)="N"
36340     end if 
36360     let fnlbl(5,1,"Routing Number:",18,1)
36380     let fntxt(5,20,9,0,0,'',0,"Routing number for customer's bank")
36400     let dri$(4)=dr$
36420     let fnlbl(6,1,"Account Code:",18,1,0)
36440     let opt$(1)="27 = Checking"
36460     let opt$(2)= "37 = Savings"
36480     let fncomboa("bankdraft",6,20,mat opt$,empty$,13)
36500     if dc$="37" then 
36520       let dri$(5)="37 = Savings"
36540     else 
36560       let dri$(5)="27 = Checking"
36580     end if 
36600     let fnlbl(7,1,"Bank Account:",18,1,0,0)
36620     let fntxt(7,20,17,0,0,'',0,"Customer's bank account from which payments should be drafted.")
36640     let dri$(6)=da$
36660     let fncmdset(2)
36680     let fnacs(sn$,0,mat dri$,ckey) ! bank draft information
36700     if ckey=5 then goto NAMESCREEN ! dont update information
36720     let df$=dri$(3)
36740     let dr$=dri$(4)
36760     let dc$=dri$(5)(1:2)
36780     let da$=dri$(6)
36800   goto NAMESCREEN ! /r
40000   DEPOSIT_HIST: ! r:
40010     read #h_deposit2,using 'form pos 1,c 10,g 8,c 32,2*n 10.2',key=z$: k32$,dt1,dp$,dp1,dp2 nokey DEPOSIT_HIST_NONE
40020     let fntos(sn$="billing_info")
40040     let fnlbl(1,16,"Deposit Change Information",40,2,4)
40060     let fnlbl(2,1,"Account:",16,1)
40080     let fntxt(2,18,10,0,1,'',1)
40100     let resp$(1)=lpad$(trim$(z$),10)
40120     let fnlbl(3,1,"Name:",16,1)
40140     let fntxt(3,18,25,30,0,'',1)
40160     let resp$(2)=e$(2)
40180     let fnlbl(4,16,"Meter Address:",16,1)
40200     let fntxt(4,18,25,30,0,'',1)
40220     let resp$(3)=e$(1)
40240     dim dh_ch$(4)*20
40260     mat dh_ch$(4)
40280     let dh_ch$(1)="Date"
40300     let dh_ch$(2)="Description"
40320     let dh_ch$(3)="Before"
40340     let dh_ch$(4)="After"
40360     dim dh_cm$(4)
40380     mat dh_cm$(4)
40400     let dh_cm$(1)=("3") : let dh_cm$(2)="" : let dh_cm$(3)="10" : let dh_cm$(4)="10"
40420     let fnflexinit1("deposit",5,1,10,70,mat dh_ch$,mat dh_cm$,1)
40460     do while k32$=z$
40480       let item$(1)=str$(dt1)
40500       let item$(2)=dp$
40520       let item$(3)=str$(dp1)
40540       let item$(4)=str$(dp2)
40560       let fnflexadd1(mat item$)
40580       read #h_deposit2,using 'Form POS 1,C 10,G 8,C 32,2*N 10.2,PD 3': k32$,dt1,dp$,dp1,dp2 eof L7160
40600     loop
40620     L7160: ! 
40640     let fncmdset(2)
40660     let fnacs(sn$,0,mat resp$,ckey) ! CALL deposit change grd
40680     DEPOSIT_HIST_XIT: ! 
40700   goto NAMESCREEN ! /r
40720   DEPOSIT_HIST_NONE: ! r:
40740     mat ml$(1)
40760     let ml$(1)="There is no deposit history to display!"
40780     let fnmsgbox(mat ml$,resp$,cap$,48)
40800   goto DEPOSIT_HIST_XIT ! /r
41000   TRANS_HIST: ! r:
41020     let fntransfile(jbact$)
41060   goto NAMESCREEN ! /r
42000   ASKACCT: ! r:
42010     release #h_customer_1: ioerr ignore
42012     ad1=0 ! add code - used to tell other parts of the program, that I am currently adding a customer record.
42020     ckey=fn_ask_account('ubfm',x$,h_customer_1, 'Edit',1)
43260     let jbact$=hact$=x$
43280     if ckey=2 then ! add
43300       ad1=1
43320       goto ADD_RECORD
43340     else if ckey=1 then ! edit
43380       goto EDIT_CUSTOMER
43600     else if ckey=5 then ! Cancel
43620       goto DONE
43880     end if 
43900     goto ASKACCT
43920   ! /r
44000   ADD_RECORD: ! r:
44020     let fntos(sn$="customer8")
44040     let fnlbl(1,5,"Adding Accounts",20,0,2)
44060     let fnlbl(3,1,"Account:",15,1)
44080     let fntxt(3,17,10,0,1)
44100     let resp$(1)=""
44120     let fncmdset(11)
44140     let fnacs(sn$,0,mat resp$,ckey)
44160     if ckey=5 then goto ASKACCT
44180     let x$=lpad$(trim$(resp$(1)),10)
44200     if trim$(x$)="" then goto ADD_RECORD
44220     read #h_customer_1,using F_CUSTOMER_1,key=x$: z$ nokey ADD_CONTINUE
44240     mat ml$(2)
44260     let ml$(1)="A record with this number already exists!"
44280     let ml$(2)="Select a different account."
44300     let fnmsgbox(mat ml$,resp$,cap$,48)
44320   goto ADD_RECORD
44340     ! 
44360   ADD_CANCEL: ! 
44380     delete #h_customer_1,key=x$: ioerr ignore
44400   goto ADD_RECORD
44420   ! /r
44440   ADD_CONTINUE: ! r:
44460     let z$=x$ : mat e$=("") : let e$(4)=newe4$
44480     mat f$=("") : mat a=(0) : mat b=(0) : mat c=(0) : mat d=(0)
44500     mat g=(0) : mat adr=(0) : mat gb=(0) : bal=f=0
44520     alp$="" : let df$=dr$=dc$=da$="" : mat extra=(0) : mat extra$=("")
44540     ad1=1 : let holdz$=z$
44550     let fn_apply_default_rates(mat extra, mat a)
44960     write #h_customer_1,using F_CUSTOMER_1: z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,f,mat g,mat adr,alp$,f$(2),f$(3),bra,mat gb,df$,dr$,dc$,da$
44962     let fixgrid=1
44980     read #h_customer_1,using 'Form POS 1,C 10',key=z$: z$ ! this line should lock the record and set the SAME paramater for use in add_cancel
45000   goto EDIT_LOADED_CUSTOMER ! /r
45010   IGNORE: continue 
45020   XIT: ! r: close files and leave
45080   let fn_close_file(h_rate1)
45100   let fn_close_file(h_rate2)
45120   let fn_close_file(h_ubadrbil)
45140   let fn_close_file(h_citystzip)
45160   let fn_close_file(h_ubtransvb)
45180   ! fn_close_file(h_deposit1)
45200   let fn_close_file(h_deposit2)
45220   let fn_close_file(h_cass1)
45240   let fn_close_file(h_budtrans)
45260   let fn_close_file(h_budmstr)
45280   let fn_close_file(h_budtrans)
45300   let fn_close_file(h_customer_1)
45320   let fn_close_file(h_customer_2)
45340   let fn_close_file(h_customer_3)
45360   let fn_close_file(h_customer_4)
45380   let fn_close_file(h_customer_5) ! /r
45400 fnend 
46000 def library fnDepositChangeLog(z$*10,odp,ndp,chgDateMmDdYy,comment$*32)
46010   if ~setup then let fn_setup
46020   if ~setup_depositChange then let fn_setup_depositChange
46040   fnDepositChangeLog=fn_depositChangeLog(z$,odp,ndp,chgDateMmDdYy,comment$)
46060 fnend
46080 def fn_setup_depositChange
46140   open #h_deposit2:=fngethandle: 'Name='&env$('Q')&'\UBmstr\Deposit2.h'&env$('cno')&',KFName='&env$('Q')&'\UBmstr\Deposit2Index.h'&env$('cno')&',Shr,Use,RecL=73,KPs=1,KLn=10',internal,outin,keyed ! "Name="&env$('Q')&"\UBmstr\Deposit2.h"&env$('cno')&",Shr,Use,RecL=73",internal,outin,relative  ! was 42
46180 fnend
46200 def fn_depositChangeLog(z$,odp,ndp,chgDateMmDdYy,comment$*32)
46220   ! requires local:  #h_deposit2
46240   let rk$=z$
46260   if rk$<>"" then 
46450     chgDateCcyyMmDd=date(days(chgDateMmDdYy,'mmddyy'),'ccyymmdd')
46480     write #h_deposit2,using 'form pos 1,c 10,g 8,c 32,2*n 10.2,pd 3',rec=r32: z$,chgDateCcyyMmDd,comment$,odp,ndp,0
46600   end if
46620 fnend
47000 def fn_close_file(cf_handle)
47020   close #cf_handle: ioerr ignore
47040 fnend 
48000 SERVICE_BUTTONS: ! r:
48020   let lyne=5
48040   for j=1 to 10
48060     if trim$(srv$(j))<>"" and trim$(srvnam$(j))<>"" then 
48070       let lyne+=1 : let funkeyval=j+20
48086       let fnbutton_or_disabled(trim$(srv$(j))<>searchcode$,lyne,45,srvnam$(j),funkeyval,"Allows you to assign "&lwrc$(trim$(srvnam$(j)))&" codes for this customer (service "&str$(j)&')',20)
48200     end if
48220   next j
48240 return  ! /r
50000 SERVICE_SCREEN: ! r:
50020   if ckey=5 then goto NAMESCREEN
50040   if env$('client')="Gilbertown" then 
50060     if a(7)=0 then a(7)=1
50080     if a(6)=0 then a(6)=1
50100   end if 
50120   mat rateinfo$=("")
50140   if ckey>20 and ckey<=30 then 
50160     on ckey-20 goto SERVICE1,SERVICE2,SERVICE3,SERVICE4,SERVICE5,SERVICE6,SERVICE7,SERVICE8,SERVICE9,SERVICE10
50180   end if 
50220 goto NAMESCREEN ! /r
51000 SERVICE1: ! r: 1ST SERVICE  -  Water
51010   let fntos(sn$="Service1") : let service_code=1
51020   let respc=0
51030   let fnlbl(1,19,srvnam$(service_code),20,2,4)
51040   let fnlbl(2,1,"Account:",10,1)
51050   let fntxt(2,12,10,0,1,'',1)
51060   let rateinfo$(respc+=1)=z$ ! 1
51070   let fnlbl(2,24,"Name:",5,1)
51080   let fntxt(2,31,25,30,0,'',1)
51090   let rateinfo$(respc+=1)=e$(2) ! 2
51100   let fnlbl(4,1,"Rate Code:",17,1)
51110   let searchcode$=srv$(service_code)
51120   let ratecode=a(1)
51130   gosub GET_CODES
51140   let fncomboa("ubfm-rates",4,19,mat rates$,"",30)
51150   let respc+=1 ! 3
51160   if env$('client')="Sangamon" then 
51170     let fnlbl(5,1,"Device Code:",17,1)
51180   else 
51190     let fnlbl(5,1,"MeterNo:",17,1)
51200   end if 
51210   let fntxt(5,19,12)
51220   let rateinfo$(respc+=1)=f$(1) ! 4
51230   let fnlbl(6,1,'Serial No:',17,1)
51240   let fntxt(6,19,12)
51250   let rateinfo$(respc+=1)=extra$(3) ! 5
51260   let fnlbl(7,1,"Deposit:",17,1)
51270   let fntxt(7,19,8,0,1,'10')
51280   let rateinfo$(respc+=1)=str$(b(8)) ! 6
51290   let fnlbl(8,1,"Deposit Date:",17,1)
51300   let fntxt(8,19,8,0,1,'1')
51310   let rateinfo$(respc+=1)=str$(c(1)) ! 7
51320   let fnlbl(9,1,"Standard Charge:",17,1)
51330   let fntxt(9,19,10,0,1,'10')
51340   let rateinfo$(respc+=1)=str$(b(1)) ! 8
51350   let fnlbl(10,1,"Current Reading:",17,1)
51360   let fntxt(10,19,11,0,1,'20')
51370   let rateinfo$(respc+=1)=str$(d(1)) ! 9
51380   let fnlbl(11,1,"Prior Reading:",17,1)
51390   let fntxt(11,19,11,0,1,'20',0)
51400   let rateinfo$(respc+=1)=str$(d(2)) ! 10
51410   let fnlbl(12,1,"Usage - Current:",17,1)
51420   let fntxt(12,19,11,0,1,'20')
51430   let rateinfo$(respc+=1)=str$(d(3)) ! 11
51440   let fnlbl(13,1,"Usage - YTD:",17,1)
51450   let fntxt(13,19,11,0,1,'20')
51460   let rateinfo$(respc+=1)=str$(d(4)) ! 12
51470   let fnlbl(14,1,"Unit Count:",17,1)
51480   let fntxt(14,19,5,0,1,'20')
51490   let rateinfo$(respc+=1)=str$(d(13)) ! 13
51500   gosub SERVICE_BUTTONS
51510 ! Let PICTURE$="water.jpg"
51520 ! .    ! Let FNPIC(3,45,3,12,PICTURE$)
51522   let fnlbl(21,45,'') ! force all service windows to be same size
51530   let fncmdset(2)
51540   let fnacs(sn$,0,mat rateinfo$,ckey) ! rate screen 1
51550   if ckey=5 then goto NAMESCREEN
51560   let x=pos(rateinfo$(3),"=",1)
51570   if x=3 then a(1)=val(rateinfo$(3)(1:2)) else a(1)=val(rateinfo$(3)(1:1))
51580   let f$(1)=rateinfo$(4)(1:12)
51590   let extra$(3)=rateinfo$(5)(1:12)
51600   b(8)=val(rateinfo$(6))
51610   c(1)=val(rateinfo$(7))
51620   b(1)=val(rateinfo$(8))
51630   let d(1)=val(rateinfo$(9))
51640   let d(2)=val(rateinfo$(10))
51650   let d(3)=val(rateinfo$(11))
51660   let d(4)=val(rateinfo$(12))
51670   let d(13)=val(rateinfo$(13))
51680   if ckey=1 then goto NAMESCREEN
51690   goto SERVICE_SCREEN
51700   return  ! /r
52000 SERVICE2: ! r: 2nd SERVICE  -  Sewer
52020   let fntos(sn$="service2") : let service_code=2
52040   let respc=0
52060   let fnlbl(1,19,uprc$(srvnam$(service_code)),20,2,4)
52080   let fnlbl(2,1,"Account:",10,1)
52100   let fntxt(2,12,10,0,1,'',1)
52120   let rateinfo$(respc+=1)=z$
52140   let fnlbl(2,24,"Name:",5,1)
52160   let fntxt(2,31,25,30,0,'',1)
52180   let rateinfo$(respc+=1)=e$(2)
52200   let fnlbl(4,1,"Rate Code:",17,1)
52220   let searchcode$=srv$(service_code) : let ratecode=a(2) : gosub GET_CODES
52240   let fncomboa("ubfm-rates",4,19,mat rates$,"",30)
52260   let respc+=1
52280   let fnlbl(5,1,"Deposit:",17,1)
52300   let fntxt(5,19,8,0,1,'10')
52320   let rateinfo$(respc+=1)=str$(b(9))
52340   let fnlbl(6,1,"Deposit Date:",17,1)
52360   let fntxt(6,19,8,0,1,'1')
52380   let rateinfo$(respc+=1)=str$(c(2))
52400   let fnlbl(7,1,"Standard Charge:",17,1)
52420   let fntxt(7,19,10,0,1,'10')
52440   let rateinfo$(respc+=1)=str$(b(2))
52460   let fnlbl(8,1,"Sewer Reduction:",17,1)
52480   let fntxt(8,19,9,0,1,'20')
52500   let rateinfo$(respc+=1)=str$(extra(5))
52520   let fnlbl(9,1,"Sewer Average:",17,1)
52540   let fntxt(9,19,9,0,1,'20',0)
52560   let rateinfo$(respc+=1)=str$(extra(18))
52580   let fnlbl(10,1,"Units Per Meter:",17,1)
52600   let fntxt(10,19,3,0,1,'20',0)
52620   let rateinfo$(respc+=1)=str$(extra(14))
52640   gosub SERVICE_BUTTONS
52660 ! Let PICTURE$="data\sewer.jpg"
52680 ! .   ! let fnpic(3,45,3,12,PICTURE$)
52690   let fnlbl(21,45,'') ! force all service windows to be same size
52700   let fncmdset(2)
52720   let fnacs(sn$,0,mat rateinfo$,ckey) ! rate screen 2
52740   if ckey=5 then goto NAMESCREEN
52760   let x=pos(rateinfo$(3),"=",1)
52780   if x=3 then 
52800     a(2)=val(rateinfo$(3)(1:2))
52820   else 
52840     a(2)=val(rateinfo$(3)(1:1))
52860   end if 
52880   b(9)=val(rateinfo$(4))
52900   c(2)=val(rateinfo$(5))
52920   b(2)=val(rateinfo$(6))
52940   let extra(5)=val(rateinfo$(7))
52960   let extra(18)=val(rateinfo$(8))
52980   let extra(14)=val(rateinfo$(9))
53000   goto SERVICE_SCREEN ! /r
54000 SERVICE3: ! r: 3RD SERVICE  -  Electric
54020   let fntos(sn$="service3") : let service_code=3
54040   let respc=0
54060   let fnlbl(1,19,uprc$(srvnam$(service_code)),20,2,4)
54080   let fnlbl(2,1,"Account:",10,1)
54100   let fntxt(2,12,10,0,1,'',1)
54120   let rateinfo$(respc+=1)=z$
54140   let fnlbl(2,24,"Name:",5,1)
54160   let fntxt(2,31,25,30,0,'',1)
54180   let rateinfo$(respc+=1)=e$(2)
54200   let fnlbl(4,1,"Rate Code:",14,1)
54220   let searchcode$=srv$(service_code) : let ratecode=a(3) : gosub GET_CODES
54240   let fncomboa("ubfm-rates",4,18,mat rates$,"",30)
54260   let respc+=1
54280   if srv$(3)="EL" or srv$(3)="LM" then 
54300     goto L7870
54320   else ! .    ! skip all of electric information if field is used for something  other than electric
54340     goto L8200
54360   end if 
54380 L7870: ! 
54400   let fnlbl(5,1,"Meter Number:",18,1)
54420   let fntxt(5,20,12)
54440   let rateinfo$(respc+=1)=f$(2)
54460   let fnlbl(6,1,'Serial Number:',18,1)
54480   let fntxt(6,20,12,0,0,'',0)
54500   let rateinfo$(respc+=1)=extra$(4)
54520   let fnlbl(7,1,"Deposit:",18,1)
54540   let fntxt(7,20,8,0,1,'10',0)
54560   let rateinfo$(respc+=1)=str$(b(10))
54580   let fnlbl(8,1,"Deposit Date:",18,1)
54600   let fntxt(8,20,8,0,1,'1',0)
54620   let rateinfo$(respc+=1)=str$(c(3))
54640   let fnlbl(9,1,"Standard Charge:",18,1)
54660   let fntxt(9,20,10,0,1,'10',0)
54680   let rateinfo$(respc+=1)=str$(b(3))
54700   let fnlbl(10,1,"Current Reading:",18,1)
54720   let fntxt(10,20,9,0,1,'20',0)
54740   let rateinfo$(respc+=1)=str$(d(5))
54760   let fnlbl(11,1,"Prior Reading:",18,1)
54780   let fntxt(11,20,9,0,1,'20',0)
54800   let rateinfo$(respc+=1)=str$(d(6))
54820   let fnlbl(12,1,"Usage - Current:",18,1)
54840   let fntxt(12,20,9,0,1,'20',0)
54860   let rateinfo$(respc+=1)=str$(d(7))
54880   let fnlbl(13,1,"Usage - YTD:",18,1)
54900   let fntxt(13,20,9,0,1,"20")
54920   let rateinfo$(respc+=1)=str$(d(8))
54940   if srv$(3)="LM" then goto L8200 ! .    ! skip rest of information on lawn meters
54960   let fnlbl(14,1,"Demand Reading:",18,1)
54980   let fntxt(14,20,9,0,1,"20")
55000   let rateinfo$(respc+=1)=str$(d(15))
55020   let fnlbl(15,1,"Demand Multiplier:",18,1)
55040   let fntxt(15,20,9,0,1,"33")
55060   let rateinfo$(respc+=1)=str$(d(14)*.001)
55080   let fnlbl(16,1,"Average Usage:",18,1)
55100   let fntxt(16,20,9,0,1,"20")
55120   let rateinfo$(respc+=1)=str$(extra(9))
55140   let fnlbl(17,1,"Usage Multiplier:",18,1)
55160   let fntxt(17,20,9,0,1,"33")
55180   let rateinfo$(respc+=1)=str$(extra(8)*.001)
55200   let fnlbl(18,1,"Security Light $:",18,1)
55220   let fntxt(18,20,9,0,1,"10")
55240   let rateinfo$(respc+=1)=str$(extra(6))
55260   let fnlbl(19,1,"Num of Lights:",18,1)
55280   let fntxt(19,20,3,0,1,"20")
55300   let rateinfo$(respc+=1)=str$(extra(7))
55320   let fnlbl(20,1,"Units per Meter:",18,1)
55340   let fntxt(20,20,3,0,1,"20")
55360   let rateinfo$(respc+=1)=str$(extra(15))
55380 L8200: gosub SERVICE_BUTTONS
55400 ! Let PICTURE$="data\electric.jpg"
55420 ! .   ! Let fnpic(3,45,3,12,PICTURE$)
55430   let fnlbl(21,45,'') ! force all service windows to be same size
55440   let fncmdset(2)
55460   let fnacs(sn$,0,mat rateinfo$,ckey) ! .   ! ELECTRIC RATE SCREEN
55480   if ckey=5 then goto NAMESCREEN
55500   let x=pos(rateinfo$(3),"=",1)
55520   if x=3 then 
55540     a(3)=val(rateinfo$(3)(1:2))
55560   else 
55580     a(3)=val(rateinfo$(3)(1:1))
55600   end if 
55620   let f$(2)=rateinfo$(4)(1:12)
55640   let extra$(4)=rateinfo$(5)(1:12)
55660   b(10)=val(rateinfo$(6))
55680   c(3)=val(rateinfo$(7))
55700   b(3)=val(rateinfo$(8))
55720   let d(5)=val(rateinfo$(9))
55740   let d(6)=val(rateinfo$(10))
55760   let d(7)=val(rateinfo$(11))
55780   let d(8)=val(rateinfo$(12))
55800   let d(15)=val(rateinfo$(13))
55820   let d(14)=val(rateinfo$(14))*1000
55840   let extra(9)=val(rateinfo$(15))
55860   let extra(8)=val(rateinfo$(16))*1000
55880   let extra(6)=val(rateinfo$(17))
55900   let extra(7)=val(rateinfo$(18))
55920   let extra(15)=val(rateinfo$(19))
55940   if ckey=1 then goto NAMESCREEN
55960   goto SERVICE_SCREEN ! /r
56000 SERVICE4: ! r: 4th SERVICE  -  Gas
56020   let fntos(sn$="service4") : let service_code=4
56040   let respc=0
56060   let fnlbl(1,19,uprc$(srvnam$(service_code)),20,2,4)
56080   let fnlbl(2,1,"Account:",10,1)
56100   let fntxt(2,12,10,0,1,"",1)
56120   let rateinfo$(respc+=1)=z$
56140   let fnlbl(2,24,"Name:",5,1)
56160   let fntxt(2,31,25,30,0,"",1)
56180   let rateinfo$(respc+=1)=e$(2)
56200   let fnlbl(4,1,"Rate Code:",14,1)
56220   let searchcode$=srv$(service_code) : let ratecode=a(4) : gosub GET_CODES
56240   let fncomboa("ubfm-rates",4,16,mat rates$,"",30)
56260   let respc+=1
56280   if srv$(4)="GA" then ! show all of gas information (unless field is used for something other than gas)
56300     let fnlbl(5,1,"Meter Number:",18,1)
56320     let fntxt(5,20,12)
56340     let rateinfo$(respc+=1)=f$(3)
56360     let fnlbl(6,1,"Serial Number:",18,1)
56380     let fntxt(6,20,12)
56400     let rateinfo$(respc+=1)=extra$(5)
56420     let fnlbl(7,1,"Deposit:",18,1)
56440     let fntxt(7,20,8,0,1,"10")
56460     let rateinfo$(respc+=1)=str$(b(11))
56480     let fnlbl(8,1,"Deposit Date:",18,1)
56500     let fntxt(8,20,8,0,1,"1")
56520     let rateinfo$(respc+=1)=str$(c(4))
56540     let fnlbl(9,1,"Standard Charge:",18,1)
56560     let fntxt(9,20,10,0,1,"10")
56580     let rateinfo$(respc+=1)=str$(b(4))
56600     let fnlbl(10,1,"Current Reading:",18,1)
56620     let fntxt(10,20,12,0,1,"20")
56640     let rateinfo$(respc+=1)=str$(d(9))
56660     let fnlbl(11,1,"Prior Reading:",18,1)
56680     let fntxt(11,20,12,0,1,"20")
56700     let rateinfo$(respc+=1)=str$(d(10))
56720     let fnlbl(12,1,"Usage - Current:",18,1)
56740     let fntxt(12,20,12,0,1,"20")
56760     let rateinfo$(respc+=1)=str$(d(11))
56780     let fnlbl(13,1,"Usage - YTD:",18,1)
56800     let fntxt(13,20,12,0,1,"20")
56820     let rateinfo$(respc+=1)=str$(d(12))
56840     let fnlbl(14,1,"Multiplier:",18,1)
56860     let fntxt(14,20,9,0,1,"33")
56880     let rateinfo$(respc+=1)=str$(extra(10)*.001)
56900     let fnlbl(15,1,"Number of Units:",18,1)
56920     let fntxt(15,20,3,0,1,"20")
56940     let rateinfo$(respc+=1)=str$(extra(16))
56960   end if 
56980   gosub SERVICE_BUTTONS
57000 ! Let PICTURE$="data\gas.jpg" : Let fnpic(3,45,3,12,PICTURE$)
57010   let fnlbl(21,45,'') ! force all service windows to be same size
57020   let fncmdset(2)
57030   let fnacs(sn$,0,mat rateinfo$,ckey) ! gas rate screen
57040   if ckey=5 then goto NAMESCREEN
57060   let x=pos(rateinfo$(3),"=",1)
57080   if x=3 then a(4)=val(rateinfo$(3)(1:2)) else a(4)=val(rateinfo$(3)(1:1))
57100   let f$(3)=rateinfo$(4)(1:12)
57120   let extra$(5)=rateinfo$(5)(1:12)
57140   b(11)=val(rateinfo$(6))
57160   c(4)=val(rateinfo$(7))
57180   b(4)=val(rateinfo$(8))
57200   let d(9)=val(rateinfo$(9))
57220   let d(10)=val(rateinfo$(10))
57240   let d(11)=val(rateinfo$(11))
57260   let d(12)=val(rateinfo$(12))
57280   let extra(10)=val(rateinfo$(13))*1000
57300   let extra(16)=val(rateinfo$(14))
57320   if ckey=1 then goto NAMESCREEN
57340   goto SERVICE_SCREEN ! /r
58000 SERVICE5: ! r: 5th SERVICE
58020   let fntos(sn$="service5") : let service_code=5
58040   let respc=0
58060   let fnlbl(1,19,uprc$(srvnam$(service_code)),20,2,4)
58080   let fnlbl(2,1,"Account:",10,1)
58100   let fntxt(2,12,10,0,1,"",1)
58120   let rateinfo$(respc+=1)=z$
58140   let fnlbl(2,24,"Name:",5,1)
58160   let fntxt(2,31,25,30,0,"",1)
58180   let rateinfo$(respc+=1)=e$(2)
58200   let fnlbl(4,1,"Rate Code:",14,1)
58220   let searchcode$=srv$(service_code) : let ratecode=a(5) : gosub GET_CODES
58240   let fncomboa("ubfm-rates",4,20,mat rates$,"",30)
58260   let respc+=1
58280   let fnlbl(5,1,"Standard Charge:",18,1)
58300   let fntxt(5,20,10,0,1,"10",0)
58320   let rateinfo$(respc+=1)=str$(b(5))
58340   gosub SERVICE_BUTTONS
58360 ! Let PICTURE$="data\misc5.jpg"
58380 ! .   ! let fnpic(3,45,3,12,PICTURE$)
58390   let fnlbl(21,45,'') ! force all service windows to be same size
58400   let fncmdset(2)
58410   let fnacs(sn$,0,mat rateinfo$,ckey) ! .   ! rate screen 5
58420   if ckey=5 then goto NAMESCREEN
58440   let x=pos(rateinfo$(3),"=",1)
58460   if x=3 then a(5)=val(rateinfo$(3)(1:2)) else a(5)=val(rateinfo$(3)(1:1))
58480   b(5)=val(rateinfo$(4))
58500   goto SERVICE_SCREEN ! /r
60000 SERVICE6: ! r: 6th SERVICE
60020   let fntos(sn$="service6") : let service_code=6
60040   let respc=0
60060   let fnlbl(1,19,uprc$(srvnam$(service_code)),20,2,4)
60080   let fnlbl(2,1,"Account:",10,1)
60100   let fntxt(2,12,10,0,1,"",1)
60120   let rateinfo$(respc+=1)=z$
60140   let fnlbl(2,24,"Name:",5,1)
60160   let fntxt(2,31,25,30,0,"",1)
60180   let rateinfo$(respc+=1)=e$(2)
60200   let fnlbl(4,1,"Rate Code:",14,1)
60220   let searchcode$=srv$(service_code) : let ratecode=extra(11) : gosub GET_CODES
60240   let fncomboa("ubfm-rates",4,20,mat rates$,"",30)
60260   let respc+=1
60280   let fnlbl(5,1,"Standard Charge:",18,1)
60300   let fntxt(5,20,10,0,1,"10")
60320   let rateinfo$(respc+=1)=str$(b(6))
60340   gosub SERVICE_BUTTONS
60360 ! Let PICTURE$="data\misc5.jpg"
60380 ! .   ! let fnpic(3,45,3,12,PICTURE$)
60390   let fnlbl(21,45,'') ! force all service windows to be same size
60400   let fncmdset(2)
60410   let fnacs(sn$,0,mat rateinfo$,ckey) ! .   ! rate screen 6
60420   if ckey=5 then goto NAMESCREEN
60440   let x=pos(rateinfo$(3),"=",1)
60460   if x=3 then let extra(11)=val(rateinfo$(3)(1:2)) else let extra(11)=val(rateinfo$(3)(1:1))
60480   b(6)=val(rateinfo$(4))
60500   goto SERVICE_SCREEN ! /r
62000 SERVICE7: ! r:  7th SERVICE
62020   let fntos(sn$="service7") : let service_code=7
62040   let respc=0
62060   let fnlbl(1,19,uprc$(srvnam$(service_code)),20,2,4)
62080   let fnlbl(2,1,"Account:",10,1)
62100   let fntxt(2,12,10,0,1,"",1)
62120   let rateinfo$(respc+=1)=z$
62140   let fnlbl(2,24,"Name:",5,1)
62160   let fntxt(2,31,25,30,0,"",1)
62180   let rateinfo$(respc+=1)=e$(2)
62200   let fnlbl(4,1,"Rate Code:",14,1)
62220   let searchcode$=srv$(service_code) : let ratecode=extra(12) : gosub GET_CODES
62240   let fncomboa("ubfm-rates",4,16,mat rates$,"",30)
62260   let respc+=1
62280 ! Let fnlbl(5,1,"Standard Charge:",16,1)
62300 ! Let fntxt(5,16,10,10,1,"10")
62320 ! .   ! Let RATEINFO$(4)=STR$(B(7))
62340   gosub SERVICE_BUTTONS
62360 ! Let PICTURE$="data\misc7.jpg"
62380 ! .   ! let fnpic(3,45,3,12,PICTURE$)
62390   let fnlbl(21,45,'') ! force all service windows to be same size
62400   let fncmdset(2)
62410   let fnacs(sn$,0,mat rateinfo$,ckey) ! rate screen 7
62420   if ckey=5 then goto NAMESCREEN
62440   let x=pos(rateinfo$(3),"=",1)
62460   if x=3 then let extra(12)=val(rateinfo$(3)(1:2)) else let extra(12)=val(rateinfo$(3)(1:1))
62480 ! b(7)=VAL(RATEINFO$(4))
62500   goto SERVICE_SCREEN ! /r
64000 SERVICE8: ! r:  8th SERVICE
64020   let fntos(sn$="service8") : let service_code=8
64040   let respc=0
64060   let fnlbl(1,19,uprc$(srvnam$(service_code)),20,2,4)
64080   let fnlbl(2,1,"Account:",10,1)
64100   let fntxt(2,12,10,10,1,"",1)
64120   let rateinfo$(respc+=1)=z$
64140   let fnlbl(2,24,"Name:",5,1)
64160   let fntxt(2,31,25,30,0,"",1)
64180   let rateinfo$(respc+=1)=e$(2)
64200   let fnlbl(4,1,"Rate Code:",14,1)
64220   let searchcode$=srv$(service_code) : let ratecode=extra(13) : gosub GET_CODES
64240   let fncomboa("ubfm-rates",4,16,mat rates$,"",30)
64260   let respc+=1
64280   let fnlbl(5,1,"Standard Charge:",16,1)
64300   let fntxt(5,16,10,0,1,"10")
64320   let rateinfo$(respc+=1)=str$(b(7))
64340   gosub SERVICE_BUTTONS
64360 ! Let PICTURE$="data\misc8.jpg" : let fnpic(3,45,3,12,PICTURE$)
64370   let fnlbl(21,45,'') ! force all service windows to be same size
64380   let fncmdset(2)
64400   let fnacs(sn$,0,mat rateinfo$,ckey) ! rate screen 8
64420   if ckey=5 then goto NAMESCREEN
64440   let x=pos(rateinfo$(3),"=",1)
64460   if x=3 then let extra(13)=val(rateinfo$(3)(1:2)) else let extra(13)=val(rateinfo$(3)(1:1))
64480   b(7)=val(rateinfo$(4))
64500   goto SERVICE_SCREEN ! /r
66000 SERVICE9: ! r:  9th service (sales tax)
66020   let fntos(sn$="service9") : let service_code=9
66040   let respc=0
66060   let fnlbl(1,19,uprc$(srvnam$(service_code)),20,2,4)
66080   let fnlbl(2,1,"Account:",10,1)
66100   let fntxt(2,12,10,10,1,"",1)
66120   let rateinfo$(respc+=1)=z$
66140   let fnlbl(2,24,"Name:",5,1)
66160   let fntxt(2,31,25,30,0,"",1)
66180   let rateinfo$(respc+=1)=e$(2)
66200   let fnlbl(4,1,"Rate Code:",14,1)
66220   let searchcode$=srv$(service_code) : let ratecode=a(6) : gosub GET_CODES
66240   let fncomboa("ubfm-rates",4,16,mat rates$,"",30)
66260   let respc+=1
66280   gosub SERVICE_BUTTONS
66300 ! Let PICTURE$="data\misc5.jpg" : let fnpic(3,45,3,12,PICTURE$)
66310   let fnlbl(21,45,'') ! force all service windows to be same size
66320   let fncmdset(2)
66340   let fnacs(sn$,0,mat rateinfo$,ckey) ! rate screen 9
66360   if ckey=5 then goto NAMESCREEN
66380   let x=pos(rateinfo$(3),"=",1)
66400   if x=3 then a(6)=val(rateinfo$(3)(1:2)) else a(6)=val(rateinfo$(3)(1:1))
66420   goto SERVICE_SCREEN ! /r
68000 SERVICE10: ! r:  10th SERVICE   (penalty)
68020   let fntos(sn$="service10") : let service_code=10
68040   let respc=0
68060   let fnlbl(1,19,uprc$(srvnam$(service_code)),20,2,4)
68080   let fnlbl(2,1,"Account:",10,1)
68100   let fntxt(2,12,10,10,1,"",1)
68120   let rateinfo$(respc+=1)=z$
68140   let fnlbl(2,24,"Name:",5,1)
68160   let fntxt(2,31,25,30,0,"",1)
68180   let rateinfo$(respc+=1)=e$(2)
68200   let fnlbl(4,1,"Rate Code:",14,1)
68220   let searchcode$=srv$(service_code) : let ratecode=a(7) : gosub GET_CODES
68240   let fncomboa("ubfm-rates",4,16,mat rates$,"",30)
68260   let respc+=1
68280   gosub SERVICE_BUTTONS
68300 ! Let PICTURE$="data\misc10.jpg" : let fnpic(3,45,3,12,PICTURE$)
68310   let fnlbl(21,45,'') ! force all service windows to be same size
68320   let fncmdset(2)
68330   let fnacs(sn$,0,mat rateinfo$,ckey) ! rate screen 5
68340   if ckey=5 then goto NAMESCREEN
68360   let x=pos(rateinfo$(3),"=",1)
68380   if x=3 then a(7)=val(rateinfo$(3)(1:2)) else a(7)=val(rateinfo$(3)(1:1))
68400 goto SERVICE_SCREEN ! /r
73000 ERTN: ! r:
73020   let fnerror(program$,err,line,act$,"xit")
73040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
73060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
73080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
73100 ERTN_EXEC_ACT: execute act$ : goto ERTN
73120 ! /r
74000 TGB_SET: ! r:
74020   let tgb=0
74040   for j=1 to 10 : let tgb=tgb+gb(j) : next j
74060 return  ! /r TGB_SET
75000 REMOVE_INCORRECT_ALLOCATIONS: ! r:
75020   gosub TGB_SET
75140   for j=1 to 10
75160     if trim$(srvnam$(j))='' and gb(j)<>0 then 
75180       mat ml$(2)
75200       let ml$(1)="A lost allocation of "&str$(gb(j))&" was found!"
75220       let ml$(2)="This amount has been moved into "&srvnam$(first_service)
75240       let fnmsgbox(mat ml$,resp$,cap$,48)
75260       let gb(first_service)+=gb(j)
75280       let gb(j)=0
75300     end if  ! trim$(srvnam$(j))='' and gb(j)<>0
75320   next j
75340 return  ! /r
76000 def fn_record_previous_update(rp_account$*10)
76020   if ~record_previous_load then let fn_record_previous_load(prev_list_id$)
76040   !   pr 'before' : for x=1 to 10 : pr x;'.';rp_prev$(x) : next x
76060   let rp_which=srch(mat rp_prev$,rp_account$)
76080   if rp_which>0 then ! remove the old entry
76100     for rp_item=rp_which to 2 step -1
76120       let rp_item_from=rp_item
76140       let rp_item_to=rp_item-1
76160       !     pr 'rp_prev$(';rp_item_from;') inherits from ';rp_item_to;'(';rp_prev$(rp_item_to);')'
76180       let rp_prev$(rp_item_from)=rp_prev$(rp_item_to)
76200     next rp_item
76220   else 
76240     ! 
76260     for rp_item=udim(mat rp_prev$) to 2 step -1
76280       let rp_item_from=rp_item
76300       let rp_item_to=rp_item-1
76320       !     pr 'rp_prev$(';rp_item_from;') inherits from ';rp_item_to;'(';rp_prev$(rp_item_to);')'
76340       let rp_prev$(rp_item_from)=rp_prev$(rp_item_to)
76360     next rp_item
76380   end if 
76400   let rp_prev$(1)=rp_account$ ! &' '&fn_customer_name$(rp_account$)
76420   !   pr 'after' : for x=1 to 10 : pr x;'.';rp_prev$(x) : next x
76440 fnend 
77000 def library fnCustomerNotes(z$)
77020   if ~setup then let fn_setup
77040   fnCustomerNotes=fn_customerNotes(z$)
77060 fnend
77080 def fn_customerNotes(z$)
77100     let notefile$=fn_notedir$&"\"&trim$(z$)&".txt"
77120     if exists(notefile$)=0 then 
77140       open #hTmp:=fngethandle: "Name="&notefile$&",Replace",display,output 
77160       close #hTmp: 
77180     end if  ! exists(notefile$)=0
77200     let fntext_editor(os_filename$(notefile$)) ! execute 'SY -w '&atlantis$&' "'&os_filename$(notefile$)&'" -n' ! ioerr [itself]
77220 fnend
78000 def fn_record_previous_save
78020   for rp_item=1 to udim(mat rp_prev$)
78040     let fncreg_write(prev_list_id$&'.previous.'&str$(rp_item),rp_prev$(rp_item))
78060   next rp_item
78080 fnend 
78100 def fn_record_previous_load(prev_list_id$)
78120   if ~record_previous_load then 
78140     let record_previous_load=1
78160     dim rp_prev$(10)*10 ! 1 is the most recent, 10 is the oldest
78180     dim rp_tmp$*256
78200     for rp_item=1 to udim(mat rp_prev$)
78220       let fncreg_read(prev_list_id$&'.previous.'&str$(rp_item),rp_tmp$) : let rp_prev$(rp_item)=rp_tmp$(1:10)
78240     next rp_item
78260   end if 
78280 fnend 
78300 def fn_record_previous_clear
78320   mat rp_prev$=('')
78340 fnend 
79000 def library fnNoteDir$*256
79020   fnNoteDir$=fn_notedir$
79040 fnend 
79060 def fn_notedir$*256
79080   if notedir_setup<>val(env$('cno')) then
79100     dim notedir$*256
79120     let notedir$=env$('Q')&"\UBmstr\notes.h"&env$('cno')
79140     if ~exists(notedir$) then execute 'mkdir "'&notedir$&'" -n'
79160   end if
79180   let fn_notedir$=notedir$
79200 fnend 
80000 def fn_customer_name$*30(cn_account$*10)
80020   dim customer_name_return$*30
80040   customer_name_return$=''
80060   read #h_customer_1,using 'form pos 41,C 30',key=cn_account$,release: customer_name_return$ ioerr ignore
80080   let fn_customer_name$=rtrm$(customer_name_return$)
80100 fnend 
81000 def fn_account_key_change_meter(key_from$*10,key_to$*10)
81020   let h_meterinfo=fnopen_meter
81030   let fnkey_change(h_meterinfo,'form pos 1,c 10',key_from$,key_to$)
81220   close #h_meterinfo: 
81240 fnend 
82000 def fn_customer_grid(cg_line,cg_pos)
82020   dim cg_item$(12)*30,cg_ch$(12),cg_cm$(12)
82040   open #cg_file_num:=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,input,keyed ioerr ERTN
82060   restore #file_num: 
82080   mat cg_ch$(12) : mat cg_cm$(12) : mat cg_cm$(12)
82100   cg_ch$(1)="Account"
82120   cg_ch$(2)="Status"
82140   cg_ch$(3)="Name"
82160   cg_ch$(4)="Address"
82180   cg_ch$(5)="Address"
82200   cg_ch$(6)="City, ST Zip"
82220   cg_ch$(7)="Meter Address"
82240   cg_ch$(8)="Route"
82260   cg_ch$(9)="Sequence"
82280   cg_ch$(10)="Phone"
82300   cg_ch$(11)="Meter"
82320   cg_ch$(12)="Alpha"
82340   mat cg_cm$=("80") : cg_cm$(2)="61" : cg_cm$(8)="61": cg_cm$(9)="61"
82360   let fnflexinit1('Cust2',cg_line,cg_pos,10,72,mat cg_ch$,mat cg_cm$,1)
82380   do 
82400     CG_READ_FILE: ! 
82420     read #cg_file_num,using 'Form POS 1,C 10,pos 1821,c 1,POS 41,C 30,C 30,POS 1864,C 30,POS 101,C 30,POS 11,C 30,POS 1741,C 2,C 7,POS 1894,C 12,POS 131,C 12,pos 354, c 7': mat cg_item$ eof CG_EO_CUSTOMER ioerr CG_ERR_READ
82440     let fnflexadd1(mat cg_item$)
82460   loop 
82480   ! ______________________________________________________________________
82500   CG_ERR_READ: ! 
82520   if err<>61 then goto ERTN
82540   ! pr 'Record locked during Customer_Search flexgrid creation - skipped'
82560   read #file_num,release: 
82580   goto CG_READ_FILE
82600   ! ______________________________________________________________________
82620   CG_EO_CUSTOMER: ! 
82640   close #cg_file_num: 
82660 fnend 
83000 def fn_key_tweak(&kt_key$,h_customer_1)
83020   ! function tweaks the customer account key to make it valid if it is not.  if it succeeds it returns a 1, otherwise it returns a 0
83040   kt_return=kt_key_addition=0
83060   kt_read_account$=''
83080   kt_key_origional$=kt_key$
83100   do 
83120     let kt_key$=rpad$(trim$(kt_key$),10)
83140     !   pr 'trying Rpadded account key: "'&kt_key$&'"'
83160     read #h_customer_1,using 'Form POS 1,C 10',key>=kt_key$,release: kt_read_account$ nokey KT_TRY_LPADDED
83180     if kt_key$=kt_read_account$ then let kt_return=1 : goto KT_FINIS
83200     KT_TRY_LPADDED: ! 
83220     let kt_key$=lpad$(trim$(kt_key$),10)
83240     !   pr 'trying Lpadded account key: "'&kt_key$&'"'
83260     read #h_customer_1,using 'Form POS 1,C 10',key>=kt_key$,release: kt_read_account$ nokey KT_TWEAK
83280     if kt_key$=kt_read_account$ then let kt_return=1 : goto KT_FINIS
83300     ! 
83320     KT_TWEAK: ! 
83340     let kt_key$=trim$(kt_key$)
83360     if pos(kt_key$,'.')<=0 and len(kt_key$)<=7 then 
83380       let kt_key$=kt_key$&'.00'
83400     else if kt_key$(len(kt_key$):len(kt_key$))='.' and len(kt_key$)<=6 then 
83420       let kt_key$=kt_key$&'00'
83440     else if kt_key$(len(kt_key$)-1:len(kt_key$)-1)='.' and len(kt_key$)<=9 then ! it ends with like a .? so try a .?0
83460       let kt_key$=kt_key$&'0'
83480     else if kt_key$(len(kt_key$)-2:len(kt_key$)-2)='.' then 
83500       let kt_key_addition=val(kt_key$(len(kt_key$)-1:len(kt_key$))) conv KT_NOT_A_NUMBER_AFTER_DOT
83520       let kt_key_addition+=1
83540       if kt_key_addition<=98 then 
83560         let kt_key$(len(kt_key$)-2:len(kt_key$))='.'&cnvrt$('pic(##)',kt_key_addition)
83580       else if kt_key_addition>=99 then 
83600          !       pr 'could not find the account - went all the way to .'&cnvrt$('pic(##)',kt_key_addition)
83620          !       pause
83640          goto KT_FINIS
83660       end if 
83680     else 
83700       !       pr 'found nothing to change.' : pause
83720       goto KT_FINIS
83740     end if 
83760     !   pr 'about to try: '&kt_key$ : pause
83780   loop 
83800   KT_NOT_A_NUMBER_AFTER_DOT: ! 
83820   !   pr 'could not find the account - there was a dot but it was not a number after it:  '&kt_key$(len(kt_key$)-2:len(kt_key$)) : pause
83840   goto KT_FINIS
83860   KT_FINIS: ! 
83880   ! pr 'fn_key_tweak complete.  returning ';kt_return : pause
83900   if ~kt_return then let kt_key$=kt_key_origional$
83920   let fn_key_tweak=kt_return
83940 fnend 
84000 def library fnask_account(prev_list_id$,&x$,h_customer_1; select_button_text$,aas_button_enable_add)
84010   if ~setup then let fn_setup
84020   fnask_account=fn_ask_account(prev_list_id$,x$,h_customer_1, select_button_text$,aas_button_enable_add)
84030 fnend 
84040 def fn_ask_account(prev_list_id$,&x$,h_customer_1; select_button_text$,aas_button_enable_add)
84050 !  function returns:
84060 !    2 = (if AAS_button_enable_add) for Add
84070 !    1 = Edit or Select
84080 !    5 = Exit/Cancel Selected
84090 !  X$ = account number selected.
84100   if ~aas_setup then 
84110     aas_setup=1
84120     asm_combo=1
84130     asm_text=2
84140     asm_grid=3
84150     fnureg_read('ubfm.account_selection_method',account_selection_method$)
84160     account_selection_method=val(account_selection_method$) conv ignore
84170     if account_selection_method=0 then account_selection_method=asm_combo
84180     fn_record_previous_load(prev_list_id$)
84190   end if 
84200   if select_button_text$='' then let select_button_text$='Next'
84210   col1_width=17
84220   col2_pos=col1_width+2
84230   do 
84240   AAS_TOP: ! 
84250     let fntos(sn$="Customer-AskAcct2")
84260     let respc=0
84270     askacct_line=0
84280     if rp_prev$(1)<>'' then 
84290       if rp_prev$(1)<>'' then 
84300         let fnbutton(askacct_line+=1,col2_pos,'Clear Recent',1000,'Clear Recent Accounts list')
84310       end if 
84320       let fnlbl(askacct_line+=1,1,"Recent:",col1_width,1) : askacct_line=askacct_line-1
84330       for rp_item=udim(mat rp_prev$) to 1, step -1
84340         if rp_prev$(rp_item)<>'' then 
84350           let fnbutton(askacct_line+=1,col2_pos,rp_prev$(rp_item)&' '&fn_customer_name$(rp_prev$(rp_item)),1000+rp_item, 'click to select this previously accessed account',1,43)
84360         end if 
84370       next rp_item
84380       askacct_line+=2
84390     end if 
84400     let fnlbl(askacct_line+=1,1,"Selection Method:",col1_width,1)
84410     btn_width=14
84420     if account_selection_method=asm_combo then 
84430       let fnlbl(askacct_line,col2_pos,'Combo',btn_width,2)
84440     else 
84450       let fnbutton(askacct_line,col2_pos,'Combo',2001,'tooltip',1,btn_width)
84460     end if 
84470     if account_selection_method=asm_grid then 
84480       let fnlbl(askacct_line,col2_pos+((btn_width+1)*1),'Grid',btn_width,2)
84490     else 
84500       let fnbutton(askacct_line,col2_pos+((btn_width+1)*1),'Grid',2002,'tooltip',1,btn_width)
84510     end if 
84520     if account_selection_method=asm_text then 
84530       let fnlbl(askacct_line,col2_pos+((btn_width+2)*2),'Text',btn_width,2)
84540     else 
84550       let fnbutton(askacct_line,col2_pos+((btn_width+2)*2),'Text',2003,'tooltip',1,btn_width)
84560     end if 
84570     askacct_line+=1
84580     let fnlbl(askacct_line+=1,1,"Account:",col1_width,1)
84590     if account_selection_method=asm_combo then 
84600       let fncmbact(askacct_line,col2_pos)
84610     else if account_selection_method=asm_grid then 
84620       let fn_customer_grid(askacct_line,col2_pos)
84630     else if account_selection_method=asm_text then 
84640       let fntxt(askacct_line,col2_pos,10)
84650     end if 
84660     let resp$(respc+=1)=hact$
84670     if aas_button_enable_add then let fncmdkey("&Add",2,0,0,"Add a new customer" )
84680     let fncmdkey(select_button_text$,1,1,0,select_button_text$&" the selected/highlighted record.")
84690     let fncmdkey("Search",6,0,0,"Search for customer record")
84700     let fncmdkey('Back',5,0,1,"Returns to previous screen")
84710     let fnacs(sn$,0,mat resp$,ckey)
84720     let x$=trim$(resp$(1)(1:10))
84730     if account_selection_method=asm_text and ckey=1 then 
84740       if ~fn_key_tweak(x$,h_customer_1) then 
84750         mat ml$(2)
84760         let ml$(1)="Account "&x$&' could not be found.'
84770         let ml$(2)="Select a different account."
84780         let fnmsgbox(mat ml$,resp$,cap$,48)
84790         goto AAS_TOP
84800       end if 
84810     else 
84820       let x$=lpad$(x$,10)
84830     end if 
84840     if ckey=1 then 
84850       let fn_record_previous_update(x$)
84860       goto AA_FINIS
84870     else if ckey=2 and aas_button_enable_add then 
84880       goto AA_FINIS
84890     else if ckey=5 then 
84900       goto AA_FINIS
84910     else if ckey=6 then 
84920       let fncustomer_search(x$,fixgrid)
84930       if trim$(x$)<>'' then ! in case the search was canceled
84940         let hact$=x$
84950         let fn_record_previous_update(x$)
84960         ckey=1
84970         goto AA_FINIS
84980       end if 
84990     else if ckey=1000 then 
85000       let fn_record_previous_clear
85010     else if ckey>1000 and ckey<=1000+udim(mat rp_prev$) then 
85020       let x$=lpad$(trim$(rp_prev$(ckey-1000)(1:10)),10)
85030       let fn_record_previous_update(rp_prev$(ckey-1000)(1:10))
85040       ckey=1
85050       goto AA_FINIS
85060     else if ckey=2001 then 
85070       account_selection_method=asm_combo
85080     else if ckey=2002 then 
85090       account_selection_method=asm_grid
85100     else if ckey=2003 then 
85110       account_selection_method=asm_text
85120     end if 
85130   loop 
85140   AA_FINIS: ! 
85150   let fn_record_previous_save
85160   let fnureg_write('ubfm.account_selection_method',str$(account_selection_method))
85170   let fn_ask_account=ckey
85180 fnend 
86200 def library fnapply_default_rates(mat extra, mat a)
86210   if ~setup then let fn_setup
86220   let fnapply_default_rates=fn_apply_default_rates(mat extra, mat a)
86230 fnend 
86240 def fn_apply_default_rates(mat extra, mat a)
86250   if ~adr_setup then 
86260     adr_setup=1
86270     for service_item=1 to 10
86280       dim tmp_rate$*256
86290       let fncreg_read('default rate '&str$(service_item),tmp_rate$)
86300       let tmp_rate_val(service_item)=val(tmp_rate$(1:pos(tmp_rate$,'=')-1))
86310     next service_item
86320   end if 
86330   for service_item=1 to 10
86340     if tmp_rate_val(service_item)>0 then 
86350       if service_item=>1 and service_item<=5 then 
86360         if a(service_item)=0 then a(service_item)=tmp_rate_val(service_item)
86370       else if service_item=6 then 
86380         if extra(11)=0 then let extra(11)=tmp_rate_val(service_item)
86390       else if service_item=7 then 
86400         if extra(12)=0 then let extra(12)=tmp_rate_val(service_item)
86410       else if service_item=8 then 
86420         if extra(13)=0 then let extra(13)=tmp_rate_val(service_item)
86430       else if service_item=9 then 
86440         if a(6)=0 then a(6)=tmp_rate_val(service_item)
86450       else if service_item=10 then 
86460         if a(7)=0 then a(7)=tmp_rate_val(service_item)
86470       end if 
86480     end if 
86490   next service_item
86500 fnend 
88000 def fn_warn_text$*200(z$*10; wtDefaultText$*256)
88020   dim wtReturn$*200
88040   dim wtLine$*2048
88060   wtMaxLen=200 
88080   wtReturn$=wtDefaultText$
88100   open #h_notefile:=fngethandle:'name='&fn_notedir$&"\"&trim$(z$)&".txt",d,input ioerr WT_FINIS
88120   do
88140     linput #h_notefile: wtLine$ eof WT_FINIS
88160     if lwrc$(wtLine$(1:5))='warn:' then
88180       wtLine$(1:5)=''
88200       wtLine$=trim$(wtLine$)
88220       if len(wtLine$)>(wtMaxLen) then let wtLine$(wtMaxLen-2:wtMaxLen)='...' 
88240       wtReturn$=wtLine$(1:wtMaxLen)
88260     end if
88280   loop
88300   WT_FINIS: !
88320   close #h_notefile: ioerr ignore
88340   fn_warn_text$=wtReturn$
88360 fnend
90000 def fn_setup
90002   if ~setup then 
90004     setup=1
90010     library 'S:\Core\Library': fnerror,fnacs,fnflexadd1,fnflexinit1,fntos
90020     library 'S:\Core\Library': fncustomer_search,fnlbl,fntxt,fnmsgbox,fncomboa,fnbutton,fnfra
90030     library 'S:\Core\Library': fncmbact,fncombof,fncmbrt2
90032     library 'S:\Core\Library': fnMeterAddressUpdate,fnMeterAddressLocationID
90040     library 'S:\Core\Library': fncmdset,fncmdkey,fngethandle,fnreg_read
90050     library 'S:\Core\Library': fntransfile,fncreg_read,fncreg_write,fnopen_meter,fntext_editor
90060     library 'S:\Core\Library': fnureg_write,fnureg_read,fnbutton_or_disabled,fnget_services,fnkey_change
90070     library 'S:\Core\Library': fnWorkOrderList,fnWorkOrderAdd
90072     library 'S:\Core\Library': fnfm$,fnMeterAddressName$
90080     on error goto ERTN
90090     ! r: dims
90100     dim z$*10
90110     dim e$(4)*30
90120     dim f$(3)*12
90130     dim a(7)
90140     dim b(11)
90150     dim c(4)
90160     dim d(15)
90170     dim g(12)
90180     dim adr(2)
90190     dim alp$*7
90200     dim resp$(50)*320
90210     dim hact$*80
90220     dim jbact$*81
90230     dim custinfo$(35)*40
90240     dim x$*10
90250     dim p$*10
90260     dim srvnam$(10)*20
90270     dim srv$(10)*2
90280     dim notefile$*256
90290     dim notefilenew$*256
90300     dim gb(10)
90310     dim ab$(4)*30
90330     dim rt$*54
90340     dim newe4$*30
90350     dim olde3$*30
90360     dim code$(5)*32
90370     dim rates$(50)*30
90380     dim rateinfo$(20)*30
90400     ! dim dat$*20
90410     dim cap$*128
90420     dim citykey$*30
90430     dim citystzip$*30
90440     dim extra(23)
90450     dim extra$(11)*30
90490     dim item$(25)*70
90500     dim opt$(2)*20
90510     dim ba(13)
90520     dim ml$(6)*200
90530     dim dp$*70
90540     dim da(2)
90550     dim bt1(14,2)
90560     dim badr(2)
90570     dim budgetinfo$(28)
90580     dim ra(2)
90590     dim bxnf$(30)*30 ! Billing Information Responses
90600     dim dri$(8)*30 ! Draft Information Responses
90610     dim df$*200
90620     ! /r
90640     ! r: CONSTANTS
90670   end if
90680   fncreg_read('Route Low',bkno1$) : bkno1=val(bkno1$)
90690   fncreg_read('Route High',bkno2$) : bkno2=val(bkno2$)
90700   let fnget_services(mat srvnam$,mat srv$)
90710   open #20: "Name="&env$('Q')&"\UBmstr\Company.h"&env$('cno')&",Shr",internal,input,relative 
90720   read #20,using "Form POS 81,C 30,pos 129,c 1",rec=1: newe4$,escrow$
90730   close #20: 
90740   let j=first_service=0
90750   do until first_service<>0
90770     if trim$(srvnam$(j+=1))<>'' then let first_service=j
90780   loop
90782 !
90784 fnreg_read('Meter Address Enable',u4_meterAddress$,'False')
90790 ! /r
90800 fnend 
