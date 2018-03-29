00015 ! Customer File Editor
00020 library program$: fnCustomer
00025 library 'S:\Core\Library': fnxit,fntop
00030 fntop(program$)
00035 fnCustomer(x)
00040 fnxit
00300 def library fnCustomer(x)
00310   fn_setup
00320   ! r: open files
00330   open #h_ubadrbil:=fngethandle: "Name=[Q]\UBmstr\ubAdrBil.h[cno],KFName=[Q]\UBmstr\AdrIndex.h[cno],Shr,Use,RecL=130,KPs=1,KLn=10",internal,outIn,keyed  ! was :=3
00332   F_ADRBIL: form pos 1,c 10,4*c 30
00340   gosub OPEN_CASS1
00360   fn_setup_depositChange ! INITIALIZE DEPOSIT TRACKING FILES
00401   ! r: BUD1: ! INITILIZE BUDGET FILE
00402   bud1=0
00404   open #h_budmstr:=fngethandle: "Name=[Q]\UBmstr\BudMstr.h[cno],KFName=[Q]\UBmstr\BudIdx1.h[cno],Shr,Use,RecL=80,KPs=1,KLn=10",internal,outIn,keyed  ! was 81
00405   F_BUDMSTR: form pos 1,c 10,pd 4,12*pd 5.2,2*pd 3
00406   open #h_budtrans:=fngethandle: "Name=[Q]\UBmstr\BudTrans.h[cno],Shr,Use,RecL=149",internal,outIn,relative  ! was 82
00407   F_BUDTRANS: form pos 1,c 10,2*pd 4,24*pd 5.2,2*pd 4,pd 3
00409   bud1=1
00410   ! /r
00470   open #h_customer_1:=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,outIn,keyed ! 1
00480   open #h_customer_2:=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx2.h[cno],Shr",internal,outIn,keyed  ! 11
00490   open #h_customer_3:=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx3.h[cno],Shr",internal,outIn,keyed ! Meter address
00500   open #h_customer_4:=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx4.h[cno],Shr",internal,outIn,keyed 
00510   open #h_customer_5:=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx5.h[cno],Shr",internal,outIn,keyed 
00512   F_CUSTOMER_1: form pos 1,c 10,4*c 30,c 12,7*pd 2,11*pd 4.2,4*pd 4,15*pd 5,pd 4.2,pd 4,12*pd 4.2,2*pd 3,c 7,2*c 12,pd 3,10*pd 5.2,pos 1712,c 1,c 9,c 2,c 17,n 2,n 7,2*n 6,n 9,pd 5.2,n 3,3*n 9,3*n 2,3*n 3,n 1,3*n 9,3*pd 5.2,c 30,7*c 12,3*c 30
00530   open #h_citystzip:=fngethandle: "Name=[Q]\Data\CityStZip.dat,KFName=[Q]\Data\CityStZip.Idx,Use,RecL=30,KPs=1,KLn=30,Shr",internal,outIn,keyed 
00542   ! /r
00554   ! 
00560   goto ASKACCT
00600   ! ______________________________________________________________________
00925   ACCOUNT_X_NOKEY: ! r:
00930     mat ml$(2)
00935     ml$(1)="Account "&x$&' could not be found.'
00940     ml$(2)="Select a different account."
00945     fnmsgbox(mat ml$,resp$,'',48)
00950   goto ASKACCT ! /r
01000   EDIT_CUSTOMER: ! r:
01010   jbact$=x$ ! ken 80905
01020   if len(x$)<>10 then goto ASKACCT
01030   read #h_customer_1,using F_CUSTOMER_1,key=x$: z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,f,mat g,mat adr,alp$,f$(2),f$(3),bra,mat gb,df$,dr$,dc$,da$,mat extra,mat extra$ nokey ACCOUNT_X_NOKEY
01031   fnFixPd(mat extra)
01032   gosub REMOVE_INCORRECT_ALLOCATIONS
01040   holdz$=z$
01050   olde3$=e$(3)
01054   EDIT_LOADED_CUSTOMER: ! 
01056     dim meterAddressBeforeEdit$*30
01070     oldService1DepositAmount=b(8)
01072     oldService2DepositAmount=b(9)
01074     oldService3DepositAmount=b(10)
01075     oldService4DepositAmount=b(11)
01076     !   old_gas_deposit=b(11)
01078     mat ab$=('')
01080     read #h_ubadrbil,using "Form POS 11,4*C 30",key=z$: mat ab$ nokey ignore
01090     ! pb=bal
01100     odp=b(8)+b(9)+b(11)
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
01230     release #h_customer_2: ioerr ignore
01240     rewrite #h_customer_1,using F_CUSTOMER_1,key=z$: z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,f,mat g,mat adr,alp$,f$(2),f$(3),bra,mat gb,df$,dr$,dc$,da$,mat extra,mat extra$
01242     if ad1 then let fn_record_previous_update(z$) ! &' '&e$(2))
01260     if oldService1DepositAmount<>b(8) then 
01263       fn_depositChangeLog(z$,oldService1DepositAmount,b(8),date('mmddyy'),trim$(srvnam$(1))(1:15)&' Deposit Changed')
01264     end if 
01265     if oldService2DepositAmount<>b(9) then 
01268       fn_depositChangeLog(z$,oldService2DepositAmount,b(9),date('mmddyy'),trim$(srvnam$(2))(1:15)&' Deposit Changed')
01269     end if 
01270     if oldService3DepositAmount<>b(10) then 
01273       fn_depositChangeLog(z$,oldService3DepositAmount,b(10),date('mmddyy'),trim$(srvnam$(3))(1:15)&' Deposit Changed')
01274     end if 
01275     if oldService4DepositAmount<>b(11) then 
01288       fn_depositChangeLog(z$,oldService4DepositAmount,b(11),date('mmddyy'),trim$(srvnam$(4))(1:15)&' Deposit Changed')
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
01396     ml$(1)="Do you wish to change account"
01398     ml$(2)='From "'&holdz$&'" to "'&z$&'"'
01400     fnmsgbox(mat ml$,resp$,'',36)
01402     if uprc$(resp$(1:1))="Y" then 
01404       goto ACC_KEY_CHANGE_TEST_EXIST
01406     else if uprc$(resp$(1:1))="N" then 
01408       z$=x$
01410       goto NAMESCREEN
01412     else 
01414       goto ASK_CONFIRM_KEY_CHANGE
01416     end if  ! /r
01424   ACC_KEY_CHANGE_TEST_EXIST: ! r:
01426     read #h_customer_1,using 'form pos 1,c 10',key=z$: p$ nokey ACC_KC_VALID_ROUTE_TEST
01428     mat ml$(2)
01430     ml$(1)="Account "&trim$(z$)&" already exists."
01432     ml$(2)="You must use a different account."
01434     fnmsgbox(mat ml$,resp$,'',16)
01436     goto NAMESCREEN ! /r
01438   ACC_KC_VALID_ROUTE_TEST: ! r:
01440     if extra(1)<bkno1 or extra(1)>bkno2 then 
01442       mat ml$(2)
01444       ml$(1)="You must have a valid route number!"
01446       ml$(2)="(from "&bkno1$&" to "&bkno2$&")"
01448       fnmsgbox(mat ml$,resp$,'',48)
01450       goto NAMESCREEN
01452     end if 
01480     ! 
01492     rewrite #h_customer_1,using F_CUSTOMER_1: z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,f,mat g,mat adr,alp$,f$(2),f$(3),bra,mat gb,df$,dr$,dc$,da$,mat extra,mat extra$ : if z$<>holdz$ or extra(1)<>holdroute or extra(2)>< holdseq then fixgrid=1
01493     open #h_ubtransvb:=fngethandle: "Name=[Q]\UBmstr\ubTransVB.h[cno],KFName=[Q]\UBmstr\ubTrIndx.h[cno],Shr,Use,RecL=102,KPs=1,KLn=19",internal,outIn,keyed 
01494     fnkey_change(h_ubtransvb,'form pos 1,c 10',holdz$,z$) ! change # in history transactions
01495     close #h_ubtransvb:
01496     open #h_workorder:=fngethandle: "Name=[Q]\UBmstr\WorkOrder.h[cno],KFName=[Q]\UBmstr\wkIndex.h[cno],Shr",internal,outIn,keyed
01498     fnkey_change(h_workorder,'form pos 1,c 10',holdz$,z$)
01500     close #h_workorder: 
01516     fn_accountKeyChange_meter(holdz$,z$) 
01522     ! 
01528     fnkey_change(h_deposit2,'form pos 1,c 10',x$,z$)
01530     gosub BUD3
01540     noteFile$=fn_notedir$&"\"&trim$(holdz$)&".txt" ! old notes
01550     noteFileNew$=fn_notedir$&"\"&trim$(z$)&".txt" ! new notes
01560     if exists(noteFile$)<>0 then execute "rename "&noteFile$&" "&noteFileNew$&" -d -n"
01580   goto ASKACCT ! /r
01590   ! ______________________________________________________________________
01660   CONFIRM_DELETE: ! r:
01662     mat ml$(1)
01664     ml$(1)="Do you wish to delete Account "&trim$(x$)&"?"
01666     fnmsgbox(mat ml$,resp$,'',36)
01668     if uprc$(resp$)="YES" then delconf$="Y" else delconf$="N"
01670     if delconf$="N" then goto ASKACCT
01674     if bal<>0 then 
01676       mat ml$(3)
01678       ml$(1)="You can not delete a customer with a balance."
01680       ml$(2)="You must issue a debit memo or credit memo"
01682       ml$(3)="to bring the balance to zero."
01684       fnmsgbox(mat ml$,resp$,'',16)
01686       goto NAMESCREEN
01688     end if 
01690     gosub BUD4 ! delete budget info
01692     delete #h_customer_1,key=x$: 
01694     gosub DEL_CASS
01696     delete #h_ubadrbil,key=x$: nokey ignore
01698     gosub DEL_HIST
01700     hact$=""
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
01974     ml$(1)="Balance Breakdown does not equal Current Balance!"
01976     ml$(2)="Balance = "&ltrm$(cnvrt$("pic($$$,$$$.## cr)",bal))
01978     ml$(3)="Breakdown Balance = "&ltrm$(cnvrt$("pic($$$,$$$.## cr)",tgb))
01980     ml$(4)="Difference = "&ltrm$(cnvrt$("pic($$$,$$$.## cr)",bal-tgb))
01982     fnmsgbox(mat ml$,resp$,'',48)
01990   goto BILLING_INFO ! /r
02920   BUD2: ! r:
02922     if bud1=0 then goto NAMESCREEN
02930     framelen=0
02940     for j=1 to 10
02950       if trim$(srvnam$(j))<>"" then framelen=framelen+1
02960     next j
02970     framelen=framelen+5
02980     mat ba=(0)
02990     mat badr=(0)
03000     br1=0 ! NO BUDGET RECORD
03010     read #h_budmstr,using F_BUDMSTR,key=z$: z$,mat ba,mat badr nokey L3040
03030     br1=1
03040     L3040: !
03041     sn$="budget1"
03042     fnTos(sn$)
03050     fnFra(1,1,framelen-1,45,"Budget Billing Information","Enter budget amounts to activate budget billing",0)
03060     fnLbl(2,16,"Budget Amounts",20,2,3,1)
03070     fnLbl(3,1,"Date:",20,1,0,1)
03080     fnTxt(3,22,8,8,1,"1",0,"Date budget billing approved (mmddyy format)",1)
03082     budgetinfo$(1)=str$(ba(1))
03090     x=1 : lyne =3
03100     for j=1 to 10
03110       if trim$(srvnam$(j))<>"" then  ! have this service
03120         x+=1 : lyne+=1
03122         fnLbl(lyne,1,trim$(srvnam$(j))&":",20,1,0,1)
03130         fnTxt(lyne,22,10,0,1,"10",0,"Enter budget amounts where applicable.  All other services will be calculated as normal.",1)
03132         budgetinfo$(x)=str$(ba(j+1))
03140       end if
03142     next j
03150     lyne+=1 : fnLbl(lyne,1,"Net Bill:",20,1,0,1)
03160     fnTxt(lyne,22,10,10,1,"10",0,"Net would never have a value unless all items above were budgeted",1)
03162     budgetinfo$(x+=1)=str$(ba(12))
03170     fnCmdKey("&Next",1,1)
03172     fnCmdKey("Access &Transactions",8,1)
03174     fnCmdKey("&Delete",3,0)
03176     fnCmdKey("&Cancel",5,0,1)
03180     fnAcs(sn$,0,mat budgetinfo$,ckey) ! budget billing master record
03190     if ckey=5 then goto NAMESCREEN
03200     if ckey=8 then goto TRANS_ROUTINE
03210     x=1: ba(1)=val(budgetinfo$(1)) conv ignore
03220     for j=2 to 11
03230       if trim$(srvnam$(j-1))<>"" then 
03232         x+=1: ba(j)=val(budgetinfo$(x))
03234       end if 
03240     next j
03250     x+=1: ba(12)=val(budgetinfo$(x))
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
03372     ml$(1)="You have chosen to delete all budget history"
03374     ml$(2)="for this customer."
03376     ml$(3)="Do you wish to continue?"
03378     fnmsgbox(mat ml$,resp$,'',35)
03380     if uprc$(resp$)="YES" then goto DBH_DEL else goto DBH_XIT
03382     DBH_DEL: ! 
03384     if br1=1 then delete #h_budmstr,key=z$: nokey ignore
03386     DBH_XIT: ! 
03388   goto NAMESCREEN ! /r
14000   TRANS_ROUTINE: ! r:
14010     ta1=badr(1)
14020     if ta1=0 and sum(ba)>0 then 
14030       ta1=lrec(h_budtrans)+1
14040       mat bt1=(0)
14050       nba=0
14060       write #h_budtrans,using F_BUDTRANS,rec=ta1: x$,mat bt1,nba
14070       badr(1)=ta1
14080       badr(2)=ta1
14090       rewrite #h_budmstr,using F_BUDMSTR,key=z$: z$,mat ba,mat badr
14100     end if 
14110     do while ta1<>0
14120       read #h_budtrans,using F_BUDTRANS,rec=ta1: x$,mat bt1,nba noRec BUDTR_XIT
14130       ! BUDTRANS: ! budget transactions
14140       mat budgetinfo$(28)
14150       sn$="BUDGET"
14160       fnTos(sn$)
14170       fnFra(1,1,framelen+1,50,"Budget Billing Transactions","Actual billing compared to budget billing for any month billed",0)
14180       fnLbl(2,22,"Budget     Actual",20,2,2,1)
14190       fnLbl(3,1,"Date:",20,1,0,1)
14200       budgetinfo$(1)=str$(bt1(1,1))
14210       fnTxt(3,22,8,8,1,"1",0,'',1)
14220       budgetinfo$(2)=str$(bt1(1,2))
14230       fnTxt(3,34,8,8,1,"1",0,empty$,1)
14240       x=2: lyne=3
14250       for j=1 to 10
14260         if trim$(srvnam$(j))<>"" then ! they have this service
14270           x=x+2
14280           fnLbl(lyne+=1,1,trim$(srvnam$(j))&":",20,1,0,1)
14290           fnTxt(lyne,22,10,10,1,"10",0,empty$,1)
14300           budgetinfo$(x-1)=str$(bt1(j+1,1))
14310           budgetinfo$(x)=str$(bt1(j+1,2))
14320           fnTxt(lyne,34,10,10,1,"10",0,empty$,1)
14330         end if
14340       next j
14350       lyne+=1 : fnLbl(lyne,1,"Net Bill:",20,1,0,1)
14360       budgetinfo$(x+=1)=str$(bt1(12,1))
14370       fnTxt(lyne,22,10,10,1,"10",0,empty$,1)
14380       x+=1: : budgetinfo$(x)=str$(bt1(12,2))
14390       fnTxt(lyne,34,10,10,1,"10",0,empty$,1)
14400       lyne+=1 : fnLbl(lyne,1,"Gross Bill:",20,1,0,1)
14410       budgetinfo$(x+=1)=str$(bt1(13,1))
14420       fnTxt(lyne,22,10,10,1,"10",0,empty$,1)
14430       budgetinfo$(x+=1)=str$(bt1(13,2))
14440       fnTxt(lyne,34,10,10,1,"10",0,empty$,1)
14450       lyne+=1 : fnLbl(lyne,1,"Date paid:",20,1,0,1)
14460       budgetinfo$(x+=1)=str$(bt1(14,1))
14470       fnTxt(lyne,22,8,8,1,"1",0,'',1)
14480       budgetinfo$(x+=1)=str$(bt1(14,2))
14490       fnTxt(lyne,34,8,8,1,"1",0,'',1)
14500       fnCmdSet(2)
14510       fnAcs(sn$,0,mat budgetinfo$,ckey) ! budget billing transactions
14520       if ckey=5 then goto NAMESCREEN
14530       x=0
14540       for j=1 to 14
14550         if j<2 or j>11 or trim$(srvnam$(j-1))<>"" then 
14560           x=x+1
14570           bt1(j,1)=val(budgetinfo$(x*2-1))
14580           bt1(j,2)=val(budgetinfo$(x*2))
14590         end if
14600       next j
14610       rewrite #h_budtrans,using F_BUDTRANS,rec=ta1: x$,mat bt1,nba
14620       ta1=nba 
14630     loop ! goto L3450
14640     BUDTR_XIT: ! 
14650   goto BUD2 ! /r
15000   BUD3: ! r:
15020     if bud1=0 then goto L3980 ! Account CHANGED
15040     read #h_budmstr,using 'form pos 1,c 10,pos 75,2*pd 3',key=x$: x$,mat badr nokey L3980
15060     rewrite #h_budmstr,using 'form pos 1,c 10,pos 75,2*pd 3': z$
15080     tadr=badr(1)
15100     do
15120       if tadr=0 then goto L3980
15140       read #h_budtrans,using 'form pos 1,c 10,pos 147,pd 3',rec=tadr: x$,nba
15160       rewrite #h_budtrans,using 'form pos 1,c 10,pos 147,pd 3',rec=tadr: z$ ! 11/15/00  ADDED USING
15180       tadr=nba
15200     loop
15220     L3980: ! 
15240   return ! /r
16000   BUD4: ! r:
16020   if bud1=0 then goto L4100 ! Account DELETED
16040    read #h_budmstr,using 'form pos 1,c 10,pos 75,2*pd 3',key=x$: x$,mat badr nokey L4100
16060    delete #h_budmstr: 
16080    tadr=badr(1)
16100    do
16120      if tadr=0 then goto L4100
16140      read #h_budtrans,using 'form pos 1,c 10,pos 147,pd 3',rec=tadr: x$,nba
16160      delete #h_budtrans,rec=tadr: 
16180      tadr=nba
16200    loop
16220    L4100: !
16240   return ! /r
17000   OPEN_CASS1: ! r:
17020     open #h_cass1:=fngethandle: "Name=[Q]\UBmstr\Cass1.h[cno],KFName=[Q]\UBmstr\CASS1IDX.h[cno],Shr",internal,outIn,keyed ioerr L4150 
17040     cassopen=1
17060     L4150: ! 
17080   return  ! /r
18000   DEL_CASS: ! r:
18020     if cassopen then 
18040       delete #h_cass1,key=x$: nokey ignore
18060     end if 
18080   return  ! /r
20000   DEL_HIST: ! r: Delete History with old account
20010     open #h_ubtransvb:=fngethandle: "Name=[Q]\UBmstr\ubTransVB.h[cno],KFName=[Q]\UBmstr\ubTrIndx.h[cno],Shr,Use,RecL=102,KPs=1,KLn=19",internal,outIn,keyed
20020     restore #h_ubtransvb,key>=x$&"         ": nokey DEL_HIST_FINIS
20040     do 
20060       read #h_ubtransvb,using 'form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1': p$,tdate eof DEL_HIST_FINIS
20080       if p$<>x$ then goto DEL_HIST_FINIS ! not same account
20100       if p$=x$ then delete #h_ubtransvb: 
20120     loop 
20140     DEL_HIST_FINIS: ! 
20150     close #h_ubtransvb:
20160   return  ! /r
25000   NAMESCREEN: ! r: the main customer screen
25020     fnTos(sn$="custinfo")
25040     respc=0 : frac=0
25060     mylen=25 : mylen+2
25080     fnLbl(1,1,"Account:",15,1)               :    fnTxt(1,17,10,10,1)                   : custInfo$(respc+=1)=trim$(z$)
25100     fnLbl(1,26,"Route:",8,1)                 :    fncmbrt2(1,36,1)                      : custInfo$(respc+=1)=str$(extra(1))
25120     fnLbl(1,45,"Sequence:",11,1)             :    fnTxt(1,58,7,7,1,"30")                : custInfo$(respc+=1)=str$(extra(2))
25140     fnFra(3,1,4,48,"Customer Information")   :  fraCustInfo=frac+=1
25160     fnLbl(1,1,"Name:",13,1,0,fraCustInfo)    : fnTxt(1,15,25,30,0,"",0,"",fraCustInfo) : custInfo$(respc+=1)=e$(2)
25180     fnLbl(2,1,"Address:",13,1,0,fraCustInfo) : fnTxt(2,15,25,30,0,"",0,"",fraCustInfo) : custInfo$(respc+=1)=e$(3)
25200     fnLbl(3,1,"Address:",13,1,0,fraCustInfo) : fnTxt(3,15,25,30,0,"",0,"",fraCustInfo) : custInfo$(respc+=1)=extra$(1)
25220     fnLbl(4,1,"City, St Zip:",13,1,0,fraCustInfo)
25240     fnComboF("CityStZip",4,15,30,"[Q]\Data\CityStZip.dat",1,30,0,0,"[Q]\Data\CityStZip.idx",0,0, " ",fraCustInfo,0)
25260     custInfo$(respc+=1)=e$(4)
25440     fnLbl(9,1,"Meter Address:",mylen,1)
25460     fnTxt(9,27,20,30) ! ,0,'',1)
25480     custInfo$(respc+=1)=e$(1)
25520     fnLbl(10,1,"Alpha Sort Name:",mylen,1)
25540     fnTxt(10,27,7)
25560     custInfo$(respc+=1)=alp$
25580     fnLbl(11,1,"Phone Number:",mylen,1)
25600     fnTxt(11,27,12)
25620     custInfo$(respc+=1)=extra$(2)
25640     fnLbl(12,1,"Cell Phone Number:",mylen,1)
25660     fnTxt(12,27,12,12)
25680     custInfo$(respc+=1)=extra$(8)
25700     fnLbl(13,1,"E-mail Address:",mylen,1)
25720     fnTxt(13,27,20,30,0,"",0)
25740     custInfo$(respc+=1)=extra$(9)
25760     fnLbl(14,1,"Current Balance:",mylen,1)
25780     fnTxt(14,27,12,12,1,"10",1)
25800     custInfo$(respc+=1)=str$(bal)
25820     fnLbl(15,1,"Last Billing Date:",mylen,1)
25840     fnTxt(15,27,8,8,1,"1")
25860     custInfo$(respc+=1)=str$(f)
26000     fnLbl(16,1,"Current Reading Date:",mylen,1)
26010     fnTxt(16,27,8,0,1,"1")
26020     custInfo$(respc+=1)=str$(extra(3))
26030     fnLbl(17,1,"Prior Reading Date:",mylen,1)
26040     fnTxt(17,27,8,0,1,"1")
26050     custInfo$(respc+=1)=str$(extra(4))
26060     fnLbl(18,1,"Final Billing Code:",mylen,1)
26070     code$(1)="0 - Active"
26080     code$(2)="1 - Inactive / Final Billed"
26090     code$(3)="2 - Inactive / Deposit Refunded"
26100     code$(4)="3 - Active / but Do Not Bill"
26110     code$(5)="4 - Finaled / but Not Billed"
26120     respc+=1 ! update counter for at least one final billing code
26130     for j=1 to udim(code$)
26140       if extra(17)=val(code$(j)(1:1)) then custInfo$(respc)=code$(j)
26150     next j
26160     fncomboa("final_bill",18,27,mat code$,"",25)
26170     fnLbl(19,1,"Bulk Sort Code:",mylen,1)
26180     fnTxt(19,27,12)
26190     custInfo$(respc+=1)=extra$(6)
26200     fnLbl(20,1,"Last Estimation Date:",mylen,1)
26210     fnTxt(20,27,8,8,1,"1")
26220     custInfo$(respc+=1)=str$(extra(19))
26230     if env$('client')="Kincaid" then 
26240       fnLbl(21,1,"1=Wand 2=Manual:",mylen,1)
26250     else if env$('client')="Findlay" then 
26260       fnLbl(21,1,"Energy Assistance:",mylen,1)
26270     else
26280       fnLbl(21,1,"Test Circle Code:",mylen,1)
26290     end if
26300     if extra(22)<0 then extra(22)=0
26310     if extra(22)=0 or extra(22)=2 then do_not_use_alt_addr=1 else do_not_use_alt_addr=0
26320     ! 
26330     fnTxt(21,27,12)
26340     custInfo$(respc+=1)=extra$(7)
26350     fnFra(3,52,4,48,"Mailing Information","Mailing information is only necessary if different than the customer information",0)
26360     fnLbl(1,1,"Name:",13,1,0,2)
26370     fnTxt(1,15,20,30,0,"",do_not_use_alt_addr,"Mailing information is only necessary if different than the customer information",2)
26380     custInfo$(respc+=1)=ab$(1)
26390     fnLbl(2,1,"Address:",13,1,0,2)
26400     fnTxt(2,15,20,30,0,"",do_not_use_alt_addr,empty$,2)
26410     custInfo$(respc+=1)=ab$(2)
26420     fnLbl(3,1,"Address:",13,1,0,2)
26430     fnTxt(3,15,20,30,0,"",do_not_use_alt_addr,empty$,2)
26440     custInfo$(respc+=1)=ab$(3)
26450     fnLbl(4,1,"City, St Zip:",13,1,0,2)
26460     if do_not_use_alt_addr then 
26470       fnTxt(4,15,20,30,0,"",do_not_use_alt_addr,empty$,2)
26480     else 
26490       fnComboF("CityStZip",4,15,30,"[Q]\Data\CityStZip.dat",1,30,0,0,"[Q]\Data\CityStZip.idx",0,0, " ",2,0)
26500     end if 
26510     custInfo$(respc+=1)=ab$(4)
26520     !
26530     fnLbl(23,1,"Social Security Number:",24,1)
26540     fnTxt(23,27,9,0,0,'30',0)
26550     custInfo$(resp_ssn=respc+=1)=str$(extra(20))
26560     fnLbl(24,1,"Work Phone Number:",24,1)
26570     fnTxt(24,27,12,0,0,'',0,"Enter the customers work number.")
26580     custInfo$(resp_phone_work=respc+=1)=extra$(10)
26590     fnLbl(25,1,"Business Phone Number:",24,1)
26600     fnTxt(25,27,12,0,0,'',0,"Enter the customers work number.")
26610     custInfo$(resp_phone_business=respc+=1)=extra$(11)
26620     fnButton(27,6,fn_warn_text$(z$,"The last note line that begins with 'warn:' will be displayed here."),23,"The last note line that begins with 'warn:' will be displayed here. Click to edit notes.",0,95)
26630     if do_not_use_alt_addr then 
26640       fnButton(1,37,"Don't Use",51,"Will use regular address on bills, etc. ",2,10,2)
26650     else 
26660       fnButton(1,37,"Use",50,"Will use alternate address on bills, etc.",2,8,2)
26670     end if 
26950     nav_button_pos=76 : nav_button_width=25
26960     fnLbl(9,nav_button_pos,"Additional Information",nav_button_width,2) !,3)
26980     fnButton(10,nav_button_pos,"Servi&ces",20,"Service Code Information: Including rates codes, meter numbers, deposits, readings, usages, etc",0,nav_button_width)
27000     fnButton(11,nav_button_pos,"Current &Bill and Breakdown",21,"Charges, balance, balance breakdown, net and gross bill",0,nav_button_width)
27020     fnButton(12,nav_button_pos,"Bank &Draft Information",22,"Bank draft codes, routing numbers, bank numbers, etc",0,nav_button_width)
27040     fnButton(13,nav_button_pos,"&Notes",23,"Add notes or footnotes pertaining to this customer",0,nav_button_width)
27060     fnButton(14,nav_button_pos,"Deposit &History",25,"A record of all changes to customer deposit amounts",0,nav_button_width)
27080     fnButton(15,nav_button_pos,"&Transaction History",26,"Transactions for all charges, collections, penalties, memos, etc. that have been processed on the customer.",0,nav_button_width)
27100     fnButton(16,nav_button_pos,"Budget Billing In&formation",27,"Budget amounts and variances between budget and actual",0,nav_button_width)
27120     fnButton(17,nav_button_pos,"Work &Orders",28,"Print a work order on this customer",0,nav_button_width/2)
27122     fnButton(17,nav_button_pos+nav_button_width/2+1,"Print History",29,"Review descriptions of past work orders.",0,nav_button_width/2-1)
27160     fnCmdKey("&Save",1,1,0,"Saves all changes or new information")
27180     if ad1=0 then 
27200       fnCmdKey("Delete",4,0,0,"Deletes this record")
27220     end if 
27240     fnCmdKey("&Cancel",5,0,1,"Stops without recording any changes")
27250     fnAcs(sn$,0,mat custInfo$,ckey) ! CALL main screen
27260     if ckey=5 then 
27270       release #h_customer_1: ioerr ignore
27280       release #h_ubadrbil: ioerr ignore
27290       if ad1=1 then 
27300         goto ADD_CANCEL
27310       else 
27320         goto ASKACCT
27330       end if 
27340     end if 
27350     z$=lpad$(trim$(custInfo$(1)),10) : if ckey<>5 then rp_prev$(1)=z$ ! important in case of an account number change
27360     extra(1)=val(custInfo$(2))
27370     extra(2)=val(custInfo$(3))
27380     e$(2)=custInfo$(4)
27400     e$(3)=custInfo$(5)
27420     extra$(1)=custInfo$(6)
27440     e$(4)=custInfo$(7)
27460     e$(1)=custInfo$(8)
27480     if trim$(e$(1))="" then e$(1)=e$(3) ! set meter address same as customer address if left blank
27500     citykey$=rpad$(e$(4),30)
27520     read #h_citystzip,using 'form pos 1,c 30',key=citykey$,release: citystzip$ nokey L5430
27540     goto L5440
27560     L5430: ! 
27570     write #h_citystzip,using 'form pos 1,c 30': e$(4)
27580     L5440: ! 
27600     alp$=custInfo$(9)
27620     extra$(2)=custInfo$(10)
27640     extra$(8)=custInfo$(11)
27660     extra$(9)=custInfo$(12)
27680     bal=val(custInfo$(13))
27700     f=val(custInfo$(14))
27720     extra(3)=val(custInfo$(15))
27740     extra(4)=val(custInfo$(16))
27760     extra(17)=val(custInfo$(17)(1:1))
27780     extra$(6)=custInfo$(18)
27800     extra(19)=val(custInfo$(19))
27820     extra$(7)=custInfo$(20)
27840     ab$(1)=custInfo$(21)
27860     ab$(2)=custInfo$(22)
27880     ab$(3)=custInfo$(23)
27900     ab$(4)=custInfo$(24)
27902     extra(20)=val(custInfo$(resp_ssn))
27904     extra$(10)=custInfo$(resp_phone_work)
27906     extra$(11)=custInfo$(resp_phone_business)
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
28160       fnmsgbox(mat ml$,resp$,'',48)
28180       goto NAMESCREEN
28200     else if extra(1)<bkno1 or extra(1)>bkno2  then 
28220       mat ml$(2)
28240       ml$(1)="You must have a valid route number within the range of "&bkno1$&" and "&bkno2$&"!"
28260       ml$(2)="You can use Company > Configure to set the route number range.."
28280       fnmsgbox(mat ml$,resp$,'',48)
28300       goto NAMESCREEN
28320     end if 
28340     if sum(gb)<>bal then goto CHECK_BALANCE_BREAKDOWN
28360     if ckey=1 then 
28380       goto REWRITE_RECORD
28400     else if ckey=2 then 
28420       goto REWRITE_RECORD
28440     else if ckey=20 then 
28460       if trim$(srvnam$(1))<>'' then 
28480         ckey=21 : goto SERVICE_SCREEN
28500       else if trim$(srvnam$(2))<>'' then 
28520         ckey=22 : goto SERVICE_SCREEN
28540       else if trim$(srvnam$(4))<>'' then 
28560         ckey=24 : goto SERVICE_SCREEN
28580       else if trim$(srvnam$(5))<>'' then 
28600         ckey=25 : goto SERVICE_SCREEN
28620       else if trim$(srvnam$(6))<>'' then 
28640         ckey=26 : goto SERVICE_SCREEN
28660       else if trim$(srvnam$(7))<>'' then 
28680         ckey=27 : goto SERVICE_SCREEN
28700       else if trim$(srvnam$(8))<>'' then 
28720         ckey=28 : goto SERVICE_SCREEN
28740       else if trim$(srvnam$(9))<>'' then 
28760         ckey=29 : goto SERVICE_SCREEN
28780       else if trim$(srvnam$(10))<>'' then 
28800         ckey=30 : goto SERVICE_SCREEN
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
29140       fnWorkOrderAdd(holdz$)
29160       goto NAMESCREEN 
29180     else if ckey=29 then
29200       fnWorkOrderList(z$)
29220       goto NAMESCREEN
29240     else if ckey=50 then 
29260       extra(22)=2 : goto NAMESCREEN
29280     else if ckey=51 then 
29300       extra(22)=1 : goto NAMESCREEN
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
34000   BILLING_INFO: ! r:
34020     sn$="billing_info"
34040     fnTos(sn$)
34060     fnLbl(1,14,"Billing Information",30,2,4)
34080     fnLbl(2,1,"Account:",10,1)
34100     fnTxt(2,12,10,0,1,'',1)
34120     bxnf$(1)=z$
34140     fnLbl(2,24,"Name:",5,1)
34160     fnTxt(2,31,25,30,0,'',1)
34180     bxnf$(2)=e$(2)
34200     fnFra(3,1,16,49,'')
34220     fnLbl(1,1,"Date of Charge:",14,1,0,1)
34240     fnTxt(1,16,8,0,0,'1',0,'',1)
34260     bxnf$(3)=str$(f)
34280     fnLbl(2,1,"Balance:",8,0,0,1)
34300     fnTxt(2,10,10,0,1,'10',1,'',1)
34320     bxnf$(4)=str$(bal)
34340     if uprc$(escrow$)="Y" then 
34360       fnLbl(2,21,"Escrow Balance:",15,1,0,1)
34380       fnTxt(2,38,9,0,1,'10',0,'',1)
34400       bxnf$(5)=str$(extra(23)) ! escrow balance
34420     end if 
34440     fnLbl(3,19,"Current      Balance",25,2,2,1)
34460     fnLbl(4,20,"    Bill      Breakdown",25,2,2,1)
34480     lyne=4
34500     if uprc$(escrow$)="Y" then billinfo=5 else billinfo=4
34520     for j=1 to 10
34540       if rtrm$(srvnam$(j))<>"" then 
34560         lyne+=1
34580         fnLbl(lyne,1,trim$(srvnam$(j))&":",16,1,0,1)
34600         fnTxt(lyne,19,10,0,1,'10',0,'',1)
34620         bxnf$(billinfo+=1)=str$(g(j))
34640         fnTxt(lyne,33,10,0,1,'10',0,'',1)
34660         bxnf$(billinfo+=1)=str$(gb(j))
34680       end if 
34700     next j
34720     lyne+=1 : fnLbl(lyne,1,"Net Bill:",16,1,0,1)
34740     fnTxt(lyne,19,10,0,1,'10',0,'',1)
34760     bxnf$(billinfo+=1)=str$(g(11))
34780     lyne+=1 : fnLbl(lyne,1,"Gross Bill:",16,1,0,1)
34800     fnTxt(lyne,19,10,0,1,'10',0,'',1)
34820     bxnf$(billinfo+=1)=str$(g(12))
34840     fnCmdSet(2)
34860     fnAcs(sn$,0,mat bxnf$,ckey) ! billing information
34880     if ckey=5 then goto NAMESCREEN
34900     f=val(bxnf$(3))
34910     bal=val(bxnf$(4))
34920     if uprc$(escrow$)="Y" then 
34930       extra(23)=val(bxnf$(5))
34940       billinfo=5 
34950     else 
34960       billinfo=4
34970     end if
34980     for j=1 to 10
35000       if rtrm$(srvnam$(j))<>"" then 
35020         billinfo=billinfo+1 : g(j)=val(bxnf$(billinfo))
35040         billinfo=billinfo+1 : gb(j)=val(bxnf$(billinfo))
35060       end if 
35080     next j
35100     billinfo=billinfo+1 : g(11)=val(bxnf$(billinfo))
35120     billinfo=billinfo+1 : g(12)=val(bxnf$(billinfo))
35140   goto NAMESCREEN ! /r
36000   BANK_DRAFT: ! r:
36020     sn$="bank_draft"
36040     fnTos(sn$)
36060     fnLbl(1,9,"Bank Draft Information",40,2,4)
36080     fnLbl(2,1,"Account:",10,1)
36100     fnTxt(2,12,10,10,1,'',1)
36120     dri$(1)=z$
36140     fnLbl(2,24,"Name:",5,1)
36160     fnTxt(2,31,25,30,0,'',1)
36180     dri$(2)=e$(2)
36200     ! 
36220     fnLbl(4,3,"Bank Draft (Y/N):",18)
36240     fnTxt(4,20,1,0,0,"",0,"Use Y to specify the customer has requested a bank draft")
36260     if uprc$(df$)="1" or uprc$(df$)="Y" then 
36280       dri$(3)="Y"
36300     else 
36320       dri$(3)="N"
36340     end if 
36360     fnLbl(5,1,"Routing Number:",18,1)
36380     fnTxt(5,20,9,0,0,'',0,"Routing number for customer's bank")
36400     dri$(4)=dr$
36420     fnLbl(6,1,"Account Code:",18,1,0)
36440     opt$(1)="27 = Checking"
36460     opt$(2)= "37 = Savings"
36480     fncomboa("bankdraft",6,20,mat opt$,empty$,13)
36500     if dc$="37" then 
36520       dri$(5)="37 = Savings"
36540     else 
36560       dri$(5)="27 = Checking"
36580     end if 
36600     fnLbl(7,1,"Bank Account:",18,1,0,0)
36620     fnTxt(7,20,17,0,0,'',0,"Customer's bank account from which payments should be drafted.")
36640     dri$(6)=da$
36660     fnCmdSet(2)
36680     fnAcs(sn$,0,mat dri$,ckey) ! bank draft information
36700     if ckey=5 then goto NAMESCREEN ! dont update information
36720     df$=dri$(3)
36740     dr$=dri$(4)
36760     dc$=dri$(5)(1:2)
36780     da$=dri$(6)
36800   goto NAMESCREEN ! /r
40000   DEPOSIT_HIST: ! r:
40010     read #h_deposit2,using 'form pos 1,c 10,g 8,c 32,2*n 10.2',key=z$: k32$,dt1,dp$,dp1,dp2 nokey DEPOSIT_HIST_NONE
40020     fnTos(sn$="billing_info")
40040     fnLbl(1,16,"Deposit Change Information",40,2,4)
40060     fnLbl(2,1,"Account:",16,1)
40080     fnTxt(2,18,10,0,1,'',1)
40100     resp$(1)=lpad$(trim$(z$),10)
40120     fnLbl(3,1,"Name:",16,1)
40140     fnTxt(3,18,25,30,0,'',1)
40160     resp$(2)=e$(2)
40180     fnLbl(4,16,"Meter Address:",16,1)
40200     fnTxt(4,18,25,30,0,'',1)
40220     resp$(3)=e$(1)
40240     dim dh_ch$(4)*20
40260     mat dh_ch$(4)
40280     dh_ch$(1)="Date"
40300     dh_ch$(2)="Description"
40320     dh_ch$(3)="Before"
40340     dh_ch$(4)="After"
40360     dim dh_cm$(4)
40380     mat dh_cm$(4)
40400     dh_cm$(1)=("3") : dh_cm$(2)="" : dh_cm$(3)="10" : dh_cm$(4)="10"
40420     fnflexinit1("deposit",5,1,10,70,mat dh_ch$,mat dh_cm$,1)
40460     do while k32$=z$
40480       item$(1)=str$(dt1)
40500       item$(2)=dp$
40520       item$(3)=str$(dp1)
40540       item$(4)=str$(dp2)
40560       fnflexadd1(mat item$)
40580       read #h_deposit2,using 'Form POS 1,C 10,G 8,C 32,2*N 10.2,PD 3': k32$,dt1,dp$,dp1,dp2 eof L7160
40600     loop
40620     L7160: ! 
40640     fnCmdSet(2)
40660     fnAcs(sn$,0,mat resp$,ckey) ! CALL deposit change grd
40680     DEPOSIT_HIST_XIT: ! 
40700   goto NAMESCREEN ! /r
40720   DEPOSIT_HIST_NONE: ! r:
40740     mat ml$(1)
40760     ml$(1)="There is no deposit history to display!"
40780     fnmsgbox(mat ml$,resp$,'',48)
40800   goto DEPOSIT_HIST_XIT ! /r
41000   TRANS_HIST: ! r:
41020     fntransfile(jbact$)
41060   goto NAMESCREEN ! /r
42000   ASKACCT: ! r:
42010     release #h_customer_1: ioerr ignore
42012     ad1=0 ! add code - used to tell other parts of the program, that I am currently adding a customer record.
42020     ckey=fn_ask_account('ubfm',x$,h_customer_1, 'Edit',1)
43260     jbact$=hact$=x$
43280     if ckey=2 then ! add
43300       ad1=1
43320       goto ADD_RECORD
43340     else if ckey=1 then ! edit
43380       goto EDIT_CUSTOMER
43600     else if ckey=5 then ! Cancel
43620       goto XIT
43880     end if 
43900     goto ASKACCT
43920   ! /r
44000   ADD_RECORD: ! r:
44020     fnTos(sn$="customer8")
44040     fnLbl(1,5,"Adding Accounts",20,0,2)
44060     fnLbl(3,1,"Account:",15,1)
44080     fnTxt(3,17,10,0,1)
44100     resp$(1)=""
44120     fnCmdSet(11)
44140     fnAcs(sn$,0,mat resp$,ckey)
44160     if ckey=5 then goto ASKACCT
44180     x$=lpad$(trim$(resp$(1)),10)
44200     if trim$(x$)="" then goto ADD_RECORD
44220     read #h_customer_1,using F_CUSTOMER_1,key=x$: z$ nokey ADD_CONTINUE
44240     mat ml$(2)
44260     ml$(1)="A record with this number already exists!"
44280     ml$(2)="Select a different account."
44300     fnmsgbox(mat ml$,resp$,'',48)
44320   goto ADD_RECORD
44340     ! 
44360   ADD_CANCEL: ! 
44380     delete #h_customer_1,key=x$: ioerr ignore
44400   goto ADD_RECORD
44420   ! /r
44440   ADD_CONTINUE: ! r:
44460     z$=x$ : mat e$=("") : e$(4)=newe4$
44480     mat f$=("") : mat a=(0) : mat b=(0) : mat c=(0) : mat d=(0)
44500     mat g=(0) : mat adr=(0) : mat gb=(0) : bal=f=0
44520     alp$="" : df$=dr$=dc$=da$="" : mat extra=(0) : mat extra$=("")
44540     ad1=1 : holdz$=z$
44550     fn_apply_default_rates(mat extra, mat a)
44960     write #h_customer_1,using F_CUSTOMER_1: z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,f,mat g,mat adr,alp$,f$(2),f$(3),bra,mat gb,df$,dr$,dc$,da$
44962     fixgrid=1
44980     read #h_customer_1,using 'Form POS 1,C 10',key=z$: z$ ! this line should lock the record and set the SAME paramater for use in add_cancel
45000   goto EDIT_LOADED_CUSTOMER ! /r
45020   IGNORE: continue 
45040   XIT: ! r: close files and leave
45060     ! close #2: ioerr ignore
45080     fn_close_file(h_customer_3)
45100     fn_close_file(h_customer_2)
45120     fnCloseFile(hLocation,'U4 Meter Location')
45140     setup_MeterLocation=0
45200   fn_close_file(h_ubadrbil)
45220   fn_close_file(h_citystzip)
45260   ! fn_close_file(h_deposit1)
45280   fn_close_file(h_deposit2)
45300   fn_close_file(h_cass1)
45320   fn_close_file(h_budtrans)
45340   fn_close_file(h_budmstr)
45360   fn_close_file(h_budtrans)
45380   fn_close_file(h_customer_1)
45400   fn_close_file(h_customer_2)
45420   fn_close_file(h_customer_3)
45440   fn_close_file(h_customer_4)
45460   fn_close_file(h_customer_5) ! /r
45480 fnend 
46000 def library fnDepositChangeLog(z$*10,odp,ndp,chgDateMmDdYy,comment$*32)
46010   if ~setup then let fn_setup
46020   if ~setup_depositChange then let fn_setup_depositChange
46040   fnDepositChangeLog=fn_depositChangeLog(z$,odp,ndp,chgDateMmDdYy,comment$)
46060 fnend
46080 def fn_setup_depositChange
46140   open #h_deposit2:=fngethandle: 'Name=[Q]\UBmstr\Deposit2.h[cno],KFName=[Q]\UBmstr\Deposit2Index.h[cno],Shr,Use,RecL=73,KPs=1,KLn=10',internal,outIn,keyed ! "Name=[Q]\UBmstr\Deposit2.h[cno],Shr,Use,RecL=73",internal,outIn,relative  ! was 42
46180 fnend
46200 def fn_depositChangeLog(z$,odp,ndp,chgDateMmDdYy,comment$*32)
46220   ! requires local:  #h_deposit2
46240   rk$=z$
46260   if rk$<>"" then 
46450     chgDateCcyyMmDd=date(days(chgDateMmDdYy,'mmddyy'),'ccyymmdd')
46480     write #h_deposit2,using 'form pos 1,c 10,g 8,c 32,2*n 10.2,pd 3',rec=r32: z$,chgDateCcyyMmDd,comment$,odp,ndp,0
46600   end if
46620 fnend
47000 def fn_close_file(cf_handle)
47020   close #cf_handle: ioerr ignore
47040 fnend 
50000 SERVICE_SCREEN: ! r:
50010   ! if ckey=5 then goto NAMESCREEN
50020   mat rateInfo$=("")
50030   if ckey>20 and ckey<=30 then 
50040     ! on ckey-20 goto SERVICE1,SERVICE2,SERVICE3,SERVICE4,SERVICE5,SERVICE6,SERVICE7,SERVICE8,SERVICE9,SERVICE10
50050     ! r: set service_code and ratecode
50060     if ckey=21 then 
50070       service_code=1 : ratecode=a(1)
50080     else if ckey=22 then
50090       service_code=2 : ratecode=a(2)
50100     else if ckey=23 then
50110       service_code=3 : ratecode=a(3)
50120     else if ckey=24 then
50130       service_code=4 : ratecode=a(4)
50140     else if ckey=25 then
50150       service_code=5 : ratecode=a(5)
50160     else if ckey=26 then
50170       service_code=6 : ratecode=extra(11)
50180     else if ckey=27 then
50190       service_code=7 : ratecode=extra(12)
50200     else if ckey=28 then
50210       service_code=8 : ratecode=extra(13)
50220     else if ckey=29 then
50230       service_code=9 : ratecode=a(6)
50240     else if ckey=30 then
50250       service_code=10 : ratecode=a(7)
50260     end if
50270     ! /r
50280     ! r: ServicePart1
50290     respc=gFkeyMeterLocationSelect=srvLine=0 
50292     srvCol1len=20 : srvCol2pos=22 
50300     gLocationFirstRespc=0 : gLocationKey$=''
50310     fnTos(sn$='service'&str$(service_code))
50320     fnLbl(srvLine+=1,19,srvnam$(service_code),20,2,4)
50330     fnLbl(srvLine+=1, 1,"Account:"  ,10,1) : fnTxt(srvLine,12,10, 0,1,'',1) : rateInfo$(respc+=1)=z$    ! 1
50340     fnLbl(srvLine    ,24,"Name:"    , 5,1) : fnTxt(srvLine,31,25,30,0,'',1) : rateInfo$(respc+=1)=e$(2) ! 2
50350     srvLine+=1
50360     fnLbl(srvLine+=1,1,"Rate Code:",srvCol1len,1) 
50370     fn_getRateCodeOptions(service_code,ratecode,mat rates$)
50380     fncomboa("ubfm-rates",srvLine,srvCol2pos,mat rates$,"",30)
50390     respc+=1                                                                                 ! 3
50400     if service_code=1 then
50410       fnLbl(srvLine+=1,1,"MeterNo:"        ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos,12          ) : rateInfo$(respc+=1)=f$(1)       ! 4
50420       fnLbl(srvLine+=1,1,'Serial No:'      ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos,12          ) : rateInfo$(respc+=1)=extra$(3)   ! 5
50430       fnLbl(srvLine+=1,1,"Deposit:"        ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos, 8,0,1,'10') : rateInfo$(respc+=1)=str$(b(8))  ! 6
50440       fnLbl(srvLine+=1,1,"Deposit Date:"   ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos, 8,0,1,'1' ) : rateInfo$(respc+=1)=str$(c(1))  ! 7
50450       fnLbl(srvLine+=1,1,"Standard Charge:",srvCol1len,1) : fnTxt(srvLine,srvCol2pos,10,0,1,'10') : rateInfo$(respc+=1)=str$(b(1))  ! 8
50460       fnLbl(srvLine+=1,1,"Current Reading:",srvCol1len,1) : fnTxt(srvLine,srvCol2pos,11,0,1,'20') : rateInfo$(respc+=1)=str$(d(1))  ! 9
50470       fnLbl(srvLine+=1,1,"Prior Reading:"  ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos,11,0,1,'20') : rateInfo$(respc+=1)=str$(d(2))  ! 10
50480       fnLbl(srvLine+=1,1,"Usage - Current:",srvCol1len,1) : fnTxt(srvLine,srvCol2pos,11,0,1,'20') : rateInfo$(respc+=1)=str$(d(3))  ! 11
50490       fnLbl(srvLine+=1,1,"Usage - YTD:"    ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos,11,0,1,'20') : rateInfo$(respc+=1)=str$(d(4))  ! 12
50500       fnLbl(srvLine+=1,1,"Unit Count:"     ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos, 5,0,1,'20') : rateInfo$(respc+=1)=str$(d(13)) ! 13
50510     else if service_code=2 then
50520       fnLbl(srvLine+=1,1,"Deposit:"         ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos, 8,0,1,'10') : rateInfo$(respc+=1)=str$(b(9))
50530       fnLbl(srvLine+=1,1,"Deposit Date:"    ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos, 8,0,1,'1' ) : rateInfo$(respc+=1)=str$(c(2))
50540       fnLbl(srvLine+=1,1,"Standard Charge:" ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos,10,0,1,'10') : rateInfo$(respc+=1)=str$(b(2))
50550       fnLbl(srvLine+=1,1,"Sewer Reduction:" ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos, 9,0,1,'20') : rateInfo$(respc+=1)=str$(extra(5))
50560       fnLbl(srvLine+=1,1,"Sewer Average:"   ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos, 9,0,1,'20') : rateInfo$(respc+=1)=str$(extra(18))
50570       fnLbl(srvLine+=1,1,"Units Per Meter:" ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos, 3,0,1,'20') : rateInfo$(respc+=1)=str$(extra(14))
50580     else if service_code=3 then
50590       if srv$(service_code)="EL" or srv$(service_code)="LM" then 
50600         fnLbl(srvLine+=1,1,"Meter Number:"   ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos,12          ) : rateInfo$(respc+=1)=f$(2)
50610         fnLbl(srvLine+=1,1,'Serial Number:'  ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos,12          ) : rateInfo$(respc+=1)=extra$(4)
50620         fnLbl(srvLine+=1,1,"Deposit:"        ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos, 8,0,1,'10') : rateInfo$(respc+=1)=str$(b(10))
50630         fnLbl(srvLine+=1,1,"Deposit Date:"   ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos, 8,0,1,'1' ) : rateInfo$(respc+=1)=str$(c(3))
50640         fnLbl(srvLine+=1,1,"Standard Charge:",srvCol1len,1) : fnTxt(srvLine,srvCol2pos,10,0,1,'10') : rateInfo$(respc+=1)=str$(b(3))
50650         fnLbl(srvLine+=1,1,"Current Reading:",srvCol1len,1) : fnTxt(srvLine,srvCol2pos, 9,0,1,'20') : rateInfo$(respc+=1)=str$(d(5))
50660         fnLbl(srvLine+=1,1,"Prior Reading:"  ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos, 9,0,1,'20') : rateInfo$(respc+=1)=str$(d(6))
50670         fnLbl(srvLine+=1,1,"Usage - Current:",srvCol1len,1) : fnTxt(srvLine,srvCol2pos, 9,0,1,'20') : rateInfo$(respc+=1)=str$(d(7))
50680         fnLbl(srvLine+=1,1,"Usage - YTD:"    ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos, 9,0,1,"20") : rateInfo$(respc+=1)=str$(d(8))
50690         if srv$(service_code)="EL" then  ! .    ! skip rest of information on lawn meters
50700           fnLbl(srvLine+=1,1,"Demand Reading:"   ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos,9,0,1,"20") : rateInfo$(respc+=1)=str$(d(15))
50710           fnLbl(srvLine+=1,1,"Demand Multiplier:",srvCol1len,1) : fnTxt(srvLine,srvCol2pos,9,0,1,"33") : rateInfo$(respc+=1)=str$(d(14)*.001)
50720           fnLbl(srvLine+=1,1,"Average Usage:"    ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos,9,0,1,"20") : rateInfo$(respc+=1)=str$(extra(9))
50730           fnLbl(srvLine+=1,1,"Usage Multiplier:" ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos,9,0,1,"33") : rateInfo$(respc+=1)=str$(extra(8)*.001)
50740           fnLbl(srvLine+=1,1,"Security Light $:" ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos,9,0,1,"10") : rateInfo$(respc+=1)=str$(extra(6))
50750           fnLbl(srvLine+=1,1,"Num of Lights:"    ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos,3,0,1,"20") : rateInfo$(respc+=1)=str$(extra(7))
50760           fnLbl(srvLine+=1,1,"Units per Meter:"  ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos,3,0,1,"20") : rateInfo$(respc+=1)=str$(extra(15))
50770         end if
50780       end if 
50790     else if service_code=4 then
50800       if srv$(service_code)="GA" then ! show all of gas information (unless field is used for something other than gas)
50810         fnLbl(srvLine+=1,1,"Meter Number:"   ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos,12          ) : rateInfo$(respc+=1)=f$(3)
50820         fnLbl(srvLine+=1,1,"Serial Number:"  ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos,12          ) : rateInfo$(respc+=1)=extra$(5)
50830         fnLbl(srvLine+=1,1,"Deposit:"        ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos, 8,0,1,"10") : rateInfo$(respc+=1)=str$(b(11))
50840         fnLbl(srvLine+=1,1,"Deposit Date:"   ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos, 8,0,1,"1" ) : rateInfo$(respc+=1)=str$(c(4))
50850         fnLbl(srvLine+=1,1,"Standard Charge:",srvCol1len,1) : fnTxt(srvLine,srvCol2pos,10,0,1,"10") : rateInfo$(respc+=1)=str$(b(4))
50860         fnLbl(srvLine+=1,1,"Current Reading:",srvCol1len,1) : fnTxt(srvLine,srvCol2pos,12,0,1,"20") : rateInfo$(respc+=1)=str$(d(9))
50870         fnLbl(srvLine+=1,1,"Prior Reading:"  ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos,12,0,1,"20") : rateInfo$(respc+=1)=str$(d(10))
50880         fnLbl(srvLine+=1,1,"Usage - Current:",srvCol1len,1) : fnTxt(srvLine,srvCol2pos,12,0,1,"20") : rateInfo$(respc+=1)=str$(d(11))
50890         fnLbl(srvLine+=1,1,"Usage - YTD:"    ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos,12,0,1,"20") : rateInfo$(respc+=1)=str$(d(12))
50900         fnLbl(srvLine+=1,1,"Multiplier:"     ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos, 9,0,1,"33") : rateInfo$(respc+=1)=str$(extra(10)*.001)
50910         fnLbl(srvLine+=1,1,"Number of Units:",srvCol1len,1) : fnTxt(srvLine,srvCol2pos, 3,0,1,"20") : rateInfo$(respc+=1)=str$(extra(16))
50920       end if 
50930     else if service_code=5 then
50940       fnLbl(srvLine+=1,1,"Standard Charge:",srvCol1len,1) : fnTxt(srvLine,srvCol2pos,10,0,1,"10",0) : rateInfo$(respc+=1)=str$(b(5))
50950     else if service_code=6 then
50960       fnLbl(srvLine+=1,1,"Standard Charge:",srvCol1len,1) : fnTxt(srvLine,srvCol2pos,10,0,1,"10") : rateInfo$(respc+=1)=str$(b(6))
50970     else if service_code=7 then
50980         ! (place holder for future developemnt)
50990     else if service_code=8 then
51000       fnLbl(srvLine+=1,1,"Standard Charge:",srvCol1len,1) : fnTxt(srvLine,srvCol2pos,10,0,1,"10") : rateInfo$(respc+=1)=str$(b(7))
51010     else if service_code=9 then
51020         ! (place holder for future developemnt)
51030     else if service_code=10 then
51040         ! (place holder for future developemnt)
51050     end if 
51052     ! /r
51060     ! r: SERVICE_BUTTONS
51070     lyne=5
51080     for j=1 to 10
51090       if trim$(srv$(j))<>"" and trim$(srvnam$(j))<>"" then 
51100         lyne+=1 : funkeyval=j+20
51110         fnbutton_or_disabled(trim$(srv$(j))<>searchcode$,lyne,45,srvnam$(j),funkeyval,"Allows you to assign "&lwrc$(trim$(srvnam$(j)))&" codes for this customer (service "&str$(j)&')',20)
51120       end if
51130     next j
51150     fn_ScrAddServiceMeterInfo(srvLine,respc+=1,mat rateInfo$,srv$(service_code),service_code)
51160     fnCmdKey("&Save",1,1,1) ! fnCmdSet(2)  <---  remove the cancel button
51170     fnAcs(sn$,0,mat rateInfo$,ckey) ! rate screen 1
51180     ! /r
51190     if ckey<>5 then ! r: get local values out of mat rateInfo$ and Save the record
51200       ! r: receive ratecode back
51210       ratecode=val(rateInfo$(3)(1:(pos(rateInfo$(3),"=",1)-1)))
51220       if service_code=1 then !  r: 1ST SERVICE  -  Water
51230         a(1)     =ratecode
51240         f$(1)    =rateInfo$(4)(1:12)
51250         extra$(3)=rateInfo$(5)(1:12)
51260         b(8)     =val(rateInfo$(6))
51270         c(1)     =val(rateInfo$(7))
51280         b(1)     =val(rateInfo$(8))
51290         d(1)     =val(rateInfo$(9))
51300         d(2)     =val(rateInfo$(10))
51310         d(3)     =val(rateInfo$(11))
51320         d(4)     =val(rateInfo$(12))
51330         d(13)    =val(rateInfo$(13))
51340          ! /r
51350       else if service_code=2 then ! r: 2nd SERVICE  -  Sewer
51360         a(2)     =ratecode
51370         b(9)     =val(rateInfo$(4))
51380         c(2)     =val(rateInfo$(5))
51390         b(2)     =val(rateInfo$(6))
51400         extra(5) =val(rateInfo$(7))
51410         extra(18)=val(rateInfo$(8))
51420         extra(14)=val(rateInfo$(9))
51430          ! /r
51440       else if service_code=3 then ! r: 3RD SERVICE  -  Electric
51450         a(3)     =ratecode
51460         f$(2)    =rateInfo$(4)(1:12)
51470         extra$(4)=rateInfo$(5)(1:12)
51480         b(10)    =val(rateInfo$(6))
51490         c(3)     =val(rateInfo$(7))
51500         b(3)     =val(rateInfo$(8))
51510         d(5)     =val(rateInfo$(9))
51520         d(6)     =val(rateInfo$(10))
51530         d(7)     =val(rateInfo$(11))
51540         d(8)     =val(rateInfo$(12))
51550         d(15)    =val(rateInfo$(13))
51560         d(14)    =val(rateInfo$(14))*1000
51570         extra(9) =val(rateInfo$(15))
51580         extra(8) =val(rateInfo$(16))*1000
51590         extra(6) =val(rateInfo$(17))
51600         extra(7) =val(rateInfo$(18))
51610         extra(15)=val(rateInfo$(19))
51620         ! /r
51630       else if service_code=4 then ! r: 4th SERVICE  -  Gas
51640         a(4)     =ratecode
51650         f$(3)    =rateInfo$(4)(1:12)
51660         extra$(5)=rateInfo$(5)(1:12)
51670         b(11)    =val(rateInfo$(6))
51680         c(4)     =val(rateInfo$(7))
51690         b(4)     =val(rateInfo$(8))
51700         d(9)     =val(rateInfo$(9))
51710         d(10)    =val(rateInfo$(10))
51720         d(11)    =val(rateInfo$(11))
51730         d(12)    =val(rateInfo$(12))
51740         extra(10)=val(rateInfo$(13))*1000
51750         extra(16)=val(rateInfo$(14))
51760          ! /r
51770       else if service_code=5 then ! r: 5th SERVICE
51780         a(5)     =ratecode
51790         b(5)     =val(rateInfo$(4))
51800       ! /r
51810       else if service_code=6 then ! r: 6th SERVICE
51820         extra(11)=ratecode
51830         b(6)     =val(rateInfo$(4))
51840         ! /r
51850       else if service_code=7 then ! r:  7th SERVICE
51860         extra(12)=ratecode
51870         ! b(7)     =VAL(rateInfo$(4))
51880         ! /r
51890       else if service_code=8 then ! r:  8th SERVICE
51900         extra(13)=ratecode
51910         b(7)     =val(rateInfo$(4))
51920         ! /r
51930       else if service_code=9 then ! r: 9th service (sales tax)
51940         a(6)     =ratecode
51950         ! /r
51960       else if service_code=10 then ! r: 10th SERVICE   (penalty)
51970         a(7)     =ratecode
51980         ! /r
51990       end if
52000       fn_ScrAddSrvMeterLocSave
52010     end if ! /r
52020     if ckey=5 then 
52030       goto SERVICE_SCREEN
52040     else if gFkeyMeterLocationSelect<>0 and ckey=gFkeyMeterLocationSelect then
52050       fnCustomerMeterLocationSelect(x$,srv$(service_code))
52052       ckey=service_code+20
52054       goto SERVICE_SCREEN
52056     else if ckey=>21 and ckey<=30 then
52058       goto SERVICE_SCREEN
52060     end if
52070     ! /r
52080   end if 
52090 goto NAMESCREEN ! /r
69790  dim gLocationKey$*128
69800  dim gLocationFirstRespc
70000 def fn_ScrAddServiceMeterInfo(&srvLine,&respc1,mat rateInfo$,serviceCode$*2,service_code)
70020   if fn_serviceIsMetered(service_code) then
70040     if ~setup_MeterLocation then
70060       setup_MeterLocation=1
70080       hLocation=fn_open('U4 Meter Location',mat location$,mat locationN,mat form$, 0,4)
70100     end if
70120     srvLine+=1
70140     fnLbl(srvLine+=1,19,trim$(srvnam$(service_code))&' Meter Location',20,2,4)
70150     fncmdkey('Select Location',gFkeyMeterLocationSelect:=2101)
70152     srvLine+=1
70160     mat location$=('')
70180     mat locationN=(0)
70200     location$(loc_activeCustomer)=trim$(x$)
70220     location$(loc_serviceId)=serviceCode$
70260     gLocationKey$=fnbuildkey$('U4 Meter Location',mat location$,mat locationN,4)
70280     gLocationFirstRespc=respc1
70300     read #hLocation,using form$(hLocation),key=gLocationKey$,release: mat location$,mat locationN nokey SasNoLocation
70320     fnlbl(srvLine+=1,1,'Location ID:'       ,srvCol1len,1) : fntxt(srvLine,srvCol2pos,11, 0,0,'',1) : rateInfo$(respc1   )=str$(locationN(loc_locationID     ))
70340     fnlbl(srvLine+=1,1,'Meter Address:'     ,srvCol1len,1) : fntxt(srvLine,srvCol2pos,30, 0,0,''  ) : rateInfo$(respc1+=1)=location$(loc_name           )
70400     fnlbl(srvLine+=1,1,'Longitude:'         ,srvCol1len,1) : fntxt(srvLine,srvCol2pos,17, 0,0,''  ) : rateInfo$(respc1+=1)=location$(loc_longitude     )
70420     fnlbl(srvLine+=1,1,'Latitude:'          ,srvCol1len,1) : fntxt(srvLine,srvCol2pos,17, 0,0,''  ) : rateInfo$(respc1+=1)=location$(loc_latitude       )
70440     fnlbl(srvLine+=1,1,'Meter Number:'      ,srvCol1len,1) : fntxt(srvLine,srvCol2pos,12, 0,0,''  ) : rateInfo$(respc1+=1)=location$(loc_meterNumber   )
70460     fnlbl(srvLine+=1,1,'Transmitter Number:',srvCol1len,1) : fntxt(srvLine,srvCol2pos,20, 0,0,''  ) : rateInfo$(respc1+=1)=location$(loc_transmitter    )
70480     fnlbl(srvLine+=1,1,'Meter Type:'        ,srvCol1len,1) : fncombof('',srvLine,srvCol2pos,0,'[Q]\UBmstr\MeterType.h[cno]',1,5,6,40,'[Q]\UBmstr\MeterTypeIdx.h[cno]',1) : rateInfo$(respc1+=1)=location$(loc_meterType     )
70500     goto SasFinis
70520   end if
70540   SasNoLocation: !
70542     gLocationKey$=''
70550     fnLbl(srvLine+=1,1,'No Meter Location attached.')
70620   goto SasFinis
70640   SasFinis: !
70660   fnLbl(29,1,'') ! force all service windows to be same size
70680 fnend
70700 def fn_ScrAddSrvMeterLocSave
70720   if gLocationKey$<>'' then
70740     sasrRespc=gLocationFirstRespc
70760     locationN(loc_locationID     )=val(rateInfo$(sasrRespc))
70780     location$(loc_name           )=rateInfo$(sasrRespc+=1)
70800     location$(loc_longitude      )=rateInfo$(sasrRespc+=1)
70820     location$(loc_latitude       )=rateInfo$(sasrRespc+=1)
70840     location$(loc_meterNumber    )=rateInfo$(sasrRespc+=1)
70860     location$(loc_transmitter    )=rateInfo$(sasrRespc+=1)
70880     location$(loc_meterType      )=rateInfo$(sasrRespc+=1)(1:5)
70890     rewrite #hLocation,using form$(hLocation),key=gLocationKey$: mat location$,mat locationN
70892     gLocationKey$=''
70900   end if
70920 fnend
72000 def fn_serviceIsMetered(serviceNumber)=max(0,srch(mat serviceCodeMetered$,srv$(serviceNumber))) ! /r
73000 ! <updateable region: ertn>
73020 ERTN: fnerror(program$,err,line,act$,"xit")
73040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
73060   if uprc$(act$)="PAUSE" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT ! if env$("ACSDeveloper")<>"" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
73080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
73100 ERTN_EXEC_ACT: execute act$ : goto ERTN
73120 ! </updateable region: ertn>
73500 ! <updateable region: fn_open (supressprompt:=2)>  
73510 def fn_open(filename$*255, mat f$, mat fn, mat form$; inputonly, keynum, dont_sort_subs, path$*255, mat descr$, mat field_widths,dontupdate,___,index)
73520   dim _fileiosubs$(1)*800, loadedsubs$(1)*32
73530   fn_open=fnOpenFile(filename$, mat f$, mat fn, mat form$, inputonly, keynum, dont_sort_subs, path$, mat descr$, mat field_widths, mat _fileiosubs$,supressprompt:=2)
73540   if ~max(srch(loadedsubs$,uprc$(filename$)),0) then 
73550     mat loadedsubs$(udim(loadedsubs$)+1) 
73560     loadedsubs$(udim(loadedsubs$))=uprc$(filename$)
73570     for index=1 to udim(mat _fileiosubs$) 
73580       execute (_fileiosubs$(index)) 
73590     next index
73600   end if
73610 fnend
73620 ! </updateable region: fnopen>
74000 TGB_SET: ! r:
74020   tgb=0
74040   for j=1 to 10 : tgb=tgb+gb(j) : next j
74060 return ! /r
75000 REMOVE_INCORRECT_ALLOCATIONS: ! r:
75020   gosub TGB_SET
75140   for j=1 to 10
75160     if trim$(srvnam$(j))='' and gb(j)<>0 then 
75180       mat ml$(2)
75200       ml$(1)="A lost allocation of "&str$(gb(j))&" was found!"
75220       ml$(2)="This amount has been moved into "&srvnam$(first_service)
75240       fnmsgbox(mat ml$,resp$,'',48)
75260       gb(first_service)+=gb(j)
75280       gb(j)=0
75300     end if  ! trim$(srvnam$(j))='' and gb(j)<>0
75320   next j
75340 return  ! /r
76000 def fn_record_previous_update(rp_account$*10)
76020   if ~record_previous_load then let fn_record_previous_load(prev_list_id$)
76040   !   pr 'before' : for x=1 to 10 : pr x;'.';rp_prev$(x) : next x
76060   rp_which=srch(mat rp_prev$,rp_account$)
76080   if rp_which>0 then ! remove the old entry
76100     for rp_item=rp_which to 2 step -1
76120       rp_item_from=rp_item
76140       rp_item_to=rp_item-1
76160       !     pr 'rp_prev$(';rp_item_from;') inherits from ';rp_item_to;'(';rp_prev$(rp_item_to);')'
76180       rp_prev$(rp_item_from)=rp_prev$(rp_item_to)
76200     next rp_item
76220   else 
76240     ! 
76260     for rp_item=udim(mat rp_prev$) to 2 step -1
76280       rp_item_from=rp_item
76300       rp_item_to=rp_item-1
76320       !     pr 'rp_prev$(';rp_item_from;') inherits from ';rp_item_to;'(';rp_prev$(rp_item_to);')'
76340       rp_prev$(rp_item_from)=rp_prev$(rp_item_to)
76360     next rp_item
76380   end if 
76400   rp_prev$(1)=rp_account$ ! &' '&fn_customer_name$(rp_account$)
76420   !   pr 'after' : for x=1 to 10 : pr x;'.';rp_prev$(x) : next x
76440 fnend 
77000 def library fnCustomerNotes(z$)
77020   if ~setup then let fn_setup
77040   fnCustomerNotes=fn_customerNotes(z$)
77060 fnend
77080 def fn_customerNotes(z$)
77100     noteFile$=fn_notedir$&"\"&trim$(z$)&".txt"
77120     if exists(noteFile$)=0 then 
77140       open #hTmp:=fngethandle: "Name="&noteFile$&",Replace",display,output 
77160       close #hTmp: 
77180     end if  ! exists(noteFile$)=0
77200     fnEditFile('text',noteFile$) ! execute 'SY -w '&atlantis$&' "'&os_filename$(noteFile$)&'" -n' ! ioerr [itself]
77220 fnend
78000 def fn_record_previous_save
78020   for rp_item=1 to udim(mat rp_prev$)
78040     fncreg_write(prev_list_id$&'.previous.'&str$(rp_item),rp_prev$(rp_item))
78060   next rp_item
78080 fnend 
78100 def fn_record_previous_load(prev_list_id$)
78120   if ~record_previous_load then 
78140     record_previous_load=1
78160     dim rp_prev$(10)*10 ! 1 is the most recent, 10 is the oldest
78180     dim rp_tmp$*256
78200     for rp_item=1 to udim(mat rp_prev$)
78220       fncreg_read(prev_list_id$&'.previous.'&str$(rp_item),rp_tmp$) : rp_prev$(rp_item)=rp_tmp$(1:10)
78240     next rp_item
78260   end if 
78280 fnend 
78300 def fn_record_previous_clear
78320   mat rp_prev$=('')
78340 fnend 
79000 def library fnNoteDir$*256
79010   if ~setup then let fn_setup
79020   fnNoteDir$=fn_notedir$
79040 fnend 
79060 def fn_notedir$*256
79080   if notedir_setup<>val(env$('cno')) then
79100     dim notedir$*256
79120     notedir$="[Q]\UBmstr\notes.h[cno]"
79140     fnmakesurepathexists(notedir$&'\')
79160   end if
79180   fn_notedir$=notedir$
79200 fnend 
80000 def fn_customer_name$*30(cn_account$*10)
80020   dim customer_name_return$*30
80040   customer_name_return$=''
80060   read #h_customer_1,using 'form pos 41,C 30',key=cn_account$,release: customer_name_return$ ioerr ignore
80080   fn_customer_name$=rtrm$(customer_name_return$)
80100 fnend 
81000 def fn_accountKeyChange_meter(key_from$*10,key_to$*10)
81020   hLocation=fn_open('U4 Meter Location',mat location$,mat locationN,mat form$, 0,3)
81030   fnkey_change(hLocation,'form pos 42,c 10',key_from$,key_to$)
81220   fnclosefile(hLocation,'U4 Meter Location')
81240 fnend 
82000 def fn_customer_grid(cg_line,cg_pos)
82020   dim cg_item$(12)*30,cg_ch$(12),cg_cm$(12)
82040   open #cg_file_num:=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed ioerr ERTN
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
82360   fnflexinit1('Cust2',cg_line,cg_pos,10,72,mat cg_ch$,mat cg_cm$,1)
82380   do 
82400     CG_READ_FILE: ! 
82420     read #cg_file_num,using 'Form POS 1,C 10,pos 1821,c 1,POS 41,C 30,C 30,POS 1864,C 30,POS 101,C 30,POS 11,C 30,POS 1741,C 2,C 7,POS 1894,C 12,POS 131,C 12,pos 354, c 7': mat cg_item$ eof CG_EO_CUSTOMER ioerr CG_ERR_READ
82440     fnflexadd1(mat cg_item$)
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
83120     kt_key$=rpad$(trim$(kt_key$),10)
83140     !   pr 'trying Rpadded account key: "'&kt_key$&'"'
83160     read #h_customer_1,using 'Form POS 1,C 10',key>=kt_key$,release: kt_read_account$ nokey KT_TRY_LPADDED
83180     if kt_key$=kt_read_account$ then kt_return=1 : goto KT_FINIS
83200     KT_TRY_LPADDED: ! 
83220     kt_key$=lpad$(trim$(kt_key$),10)
83240     !   pr 'trying Lpadded account key: "'&kt_key$&'"'
83260     read #h_customer_1,using 'Form POS 1,C 10',key>=kt_key$,release: kt_read_account$ nokey KT_TWEAK
83280     if kt_key$=kt_read_account$ then kt_return=1 : goto KT_FINIS
83300     ! 
83320     KT_TWEAK: ! 
83340     kt_key$=trim$(kt_key$)
83360     if pos(kt_key$,'.')<=0 and len(kt_key$)<=7 then 
83380       kt_key$=kt_key$&'.00'
83400     else if kt_key$(len(kt_key$):len(kt_key$))='.' and len(kt_key$)<=6 then 
83420       kt_key$=kt_key$&'00'
83440     else if kt_key$(len(kt_key$)-1:len(kt_key$)-1)='.' and len(kt_key$)<=9 then ! it ends with like a .? so try a .?0
83460       kt_key$=kt_key$&'0'
83480     else if kt_key$(len(kt_key$)-2:len(kt_key$)-2)='.' then 
83500       kt_key_addition=val(kt_key$(len(kt_key$)-1:len(kt_key$))) conv KT_NOT_A_NUMBER_AFTER_DOT
83520       kt_key_addition+=1
83540       if kt_key_addition<=98 then 
83560         kt_key$(len(kt_key$)-2:len(kt_key$))='.'&cnvrt$('pic(##)',kt_key_addition)
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
83900   if ~kt_return then kt_key$=kt_key_origional$
83920   fn_key_tweak=kt_return
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
84150     asm_locationId=4
84160     fnureg_read('ubfm.account_selection_method',account_selection_method$)
84170     account_selection_method=val(account_selection_method$) conv ignore
84180     if account_selection_method=0 then account_selection_method=asm_combo
84190     fn_record_previous_load(prev_list_id$)
84200   end if 
84210   if select_button_text$='' then select_button_text$='Next'
84220   col1_width=17
84230   col2_pos=col1_width+2
84240   do 
84250   AAS_TOP: ! 
84260     fnTos(sn$="Customer-AskAcct2")
84270     respc=0
84280     askacct_line=0
84290     if rp_prev$(1)<>'' then 
84300       if rp_prev$(1)<>'' then 
84310         fnButton(askacct_line+=1,col2_pos,'Clear Recent',1000,'Clear Recent Accounts list')
84320       end if 
84330       fnLbl(askacct_line+=1,1,"Recent:",col1_width,1) : askacct_line=askacct_line-1
84340       for rp_item=udim(mat rp_prev$) to 1, step -1
84350         if rp_prev$(rp_item)<>'' then 
84360           fnButton(askacct_line+=1,col2_pos,rp_prev$(rp_item)&' '&fn_customer_name$(rp_prev$(rp_item)),1000+rp_item, 'click to select this previously accessed account',1,43)
84370         end if 
84380       next rp_item
84390       askacct_line+=2
84400     end if 
84410     fnLbl(askacct_line+=1,1,"Selection Method:",col1_width,1)
84420     btn_width=10
84430     fnbutton_or_disabled(account_selection_method<>asm_combo,askacct_line,col2_pos,'Combo',2001,'',btn_width)
84440     fnbutton_or_disabled(account_selection_method<>asm_grid,askacct_line,col2_pos+((btn_width+1)*1),'Grid',2002,'',btn_width)
84450     fnbutton_or_disabled(account_selection_method<>asm_text,askacct_line,col2_pos+((btn_width+1)*2),'Text',2003,'',btn_width)
84470     fnbutton_or_disabled(account_selection_method<>asm_locationId,askacct_line,col2_pos+((btn_width+1)*3),'Location ID',2004,'',btn_width)
84482     askacct_line+=1
84484     if account_selection_method=asm_locationId then
84486       fnLbl(askacct_line+=1,1,"Location ID:",col1_width,1)
84488     else
84500       fnLbl(askacct_line+=1,1,"Account:",col1_width,1)
84502     end if
84510     if account_selection_method=asm_combo then 
84520       fncmbact(askacct_line,col2_pos)
84530     else if account_selection_method=asm_grid then 
84540       fn_customer_grid(askacct_line,col2_pos)
84550     else if account_selection_method=asm_text then 
84560       fnTxt(askacct_line,col2_pos,10)
84570     else if account_selection_method=asm_locationId then 
84580       fnTxt(askacct_line,col2_pos,11, 0,0,'30')
84590     end if 
84592     if account_selection_method=asm_locationId then
84594       resp$(respc+=1)=str$(tmpLocationId)
84596     else
84598       resp$(respc+=1)=hact$
84600     end if
84610     if aas_button_enable_add then let fnCmdKey("&Add",2,0,0,"Add a new customer" )
84620     fnCmdKey(select_button_text$,1,1,0,select_button_text$&" the selected/highlighted record.")
84630     fnCmdKey("Search",6,0,0,"Search for customer record")
84640     fnCmdKey('Back',5,0,1,"Returns to previous screen")
84650     fnAcs(sn$,0,mat resp$,ckey)
84660     x$=trim$(resp$(1)(1:10))
84670     if account_selection_method=asm_text and ckey=1 then 
84680       if ~fn_key_tweak(x$,h_customer_1) then 
84690         mat ml$(2)
84700         ml$(1)="Account "&x$&' could not be found.'
84710         ml$(2)="Select a different account."
84720         fnmsgbox(mat ml$,resp$,'',48)
84730         goto AAS_TOP
84740       end if 
84750     else if account_selection_method=asm_locationId and ckey=1 then
84760       tmpLocationId=val(resp$(1)) conv AAS_TOP
84770       x$=fnAccountFromLocationId$(tmpLocationId)
84780       if x$='' then goto AAS_TOP
84840     else 
84850       x$=lpad$(x$,10)
84860     end if 
84870     if ckey=1 then 
84880       fn_record_previous_update(x$)
84890       goto AA_FINIS
84900     else if ckey=2 and aas_button_enable_add then 
84910       goto AA_FINIS
84920     else if ckey=5 then 
84930       goto AA_FINIS
84940     else if ckey=6 then 
84950       fncustomer_search(x$,fixgrid)
84960       if trim$(x$)<>'' then ! in case the search was canceled
84970         hact$=x$
84980         fn_record_previous_update(x$)
84990         ckey=1
85000         goto AA_FINIS
85010       end if 
85020     else if ckey=1000 then 
85030       fn_record_previous_clear
85040     else if ckey>1000 and ckey<=1000+udim(mat rp_prev$) then 
85050       x$=lpad$(trim$(rp_prev$(ckey-1000)(1:10)),10)
85060       fn_record_previous_update(rp_prev$(ckey-1000)(1:10))
85070       ckey=1
85080       goto AA_FINIS
85090     else if ckey=2001 then 
85100       account_selection_method=asm_combo
85110     else if ckey=2002 then 
85120       account_selection_method=asm_grid
85130     else if ckey=2003 then 
85140       account_selection_method=asm_text
85150     else if ckey=2004 then 
85160       account_selection_method=asm_locationId
85170     end if 
85180   loop 
85190   AA_FINIS: ! 
85200   fn_record_previous_save
85210   fnureg_write('ubfm.account_selection_method',str$(account_selection_method))
85220   fn_ask_account=ckey
85230 fnend 
86200 def library fnapply_default_rates(mat extra, mat a)
86210   if ~setup then let fn_setup
86220   fnapply_default_rates=fn_apply_default_rates(mat extra, mat a)
86230 fnend 
86240 def fn_apply_default_rates(mat extra, mat a)
86250   if ~adr_setup then 
86260     adr_setup=1
86270     for service_item=1 to 10
86280       dim tmp_rate$*256
86290       fncreg_read('default rate '&str$(service_item),tmp_rate$)
86300       tmp_rate_val(service_item)=val(tmp_rate$(1:pos(tmp_rate$,'=')-1))
86310     next service_item
86320   end if 
86330   for service_item=1 to 10
86340     if tmp_rate_val(service_item)>0 then 
86350       if service_item=>1 and service_item<=5 then 
86360         if a(service_item)=0 then a(service_item)=tmp_rate_val(service_item)
86370       else if service_item=6 then 
86380         if extra(11)=0 then extra(11)=tmp_rate_val(service_item)
86390       else if service_item=7 then 
86400         if extra(12)=0 then extra(12)=tmp_rate_val(service_item)
86410       else if service_item=8 then 
86420         if extra(13)=0 then extra(13)=tmp_rate_val(service_item)
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
88220       if len(wtLine$)>(wtMaxLen) then wtLine$(wtMaxLen-2:wtMaxLen)='...' 
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
90010     library 'S:\Core\Library': fnerror,fnAcs,fnflexadd1,fnflexinit1,fnTos
90020     library 'S:\Core\Library': fncustomer_search,fnLbl,fnTxt,fnmsgbox,fncomboa,fnButton,fnFra
90030     library 'S:\Core\Library': fncmbact,fnComboF,fncmbrt2
90032     library 'S:\Core\Library': fnMeterAddressLocationID
90033     ! library 'S:\Core\Library': fnMeterAddressUpdate
90040     library 'S:\Core\Library': fnCmdSet,fnCmdKey,fngethandle
90046     library 'S:\Core\Library': fnreg_read
90050     library 'S:\Core\Library': fntransfile
90052     library 'S:\Core\Library': fncreg_read,fncreg_write
90056     library 'S:\Core\Library': fnEditFile
90060     library 'S:\Core\Library': fnureg_write,fnureg_read
90062     library 'S:\Core\Library': fnbutton_or_disabled
90063     library 'S:\Core\Library': fnget_services
90064     library 'S:\Core\Library': fnkey_change
90070     library 'S:\Core\Library': fnWorkOrderList,fnWorkOrderAdd
90071     library 'S:\Core\Library': fnGetServiceCodesMetered
90072     library 'S:\Core\Library': fnfm$
90073     library 'S:\Core\Library': fnMeterAddressName$
90074     library 'S:\Core\Library': fnAccountFromLocationId$
90075     library 'S:\Core\Library': fnOpenFile,fnCloseFile,fnbuildkey$
90076     library 'S:\Core\Library': fnCustomerMeterLocationSelect
90077     library 'S:\Core\Library': fnmakesurepathexists
90078     library 'S:\Core\Library': fnFixPd
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
90230     dim custInfo$(35)*40
90240     dim x$*10
90250     dim p$*10
90260     dim srvnam$(10)*20
90270     dim srv$(10)*2
90280     dim noteFile$*256
90290     dim noteFileNew$*256
90300     dim gb(10)
90310     dim ab$(4)*30
90330     dim rt$*54
90340     dim newe4$*30
90350     dim olde3$*30
90360     dim code$(5)*32
90370     dim rates$(50)*30
90380     dim rateInfo$(128)*128
90420     dim citykey$*30
90430     dim citystzip$*30
90440     dim extra(23)
90450     dim extra$(11)*30
90490     dim item$(25)*70
90500     dim opt$(2)*20
90510     dim ba(13)
90520     dim ml$(6)*200
90530     dim dp$*70
90550     dim bt1(14,2)
90560     dim badr(2)
90570     dim budgetinfo$(28)
90590     dim bxnf$(30)*30      ! Billing Information Responses
90600     dim dri$(8)*30        ! Draft Information Responses
90610     dim df$*1             ! Bank Draft (Y)
90612     dim form$(0)*512
90614     dim location$(0)*256,locationN(0)
90620     ! /r
90670   end if
90672   ! r: CONSTANTS
90680   fncreg_read('Route Low',bkno1$) : bkno1=val(bkno1$)
90690   fncreg_read('Route High',bkno2$) : bkno2=val(bkno2$)
90700   fnget_services(mat srvnam$,mat srv$)
90702   dim serviceCodeMetered$(0)*2
90704   fnGetServiceCodesMetered(mat serviceCodeMetered$)
90710   open #20: "Name=[Q]\UBmstr\Company.h[cno],Shr",internal,input,relative 
90720   read #20,using "Form POS 81,C 30,pos 129,c 1",rec=1: newe4$,escrow$
90730   close #20: 
90740   j=first_service=0
90750   do until first_service<>0
90770     if trim$(srvnam$(j+=1))<>'' then first_service=j
90780   loop
90782   !
90790   ! /r
90800 fnend 
92000 def fn_getRateCodeOptions(service_code,&ratecode,mat rates$ ) ! get applicable rate codes
92020   ! search routine must be passed code for service (WA for water) in searchcode$
92030   searchcode$=srv$(service_code)
92080   open #h_rate1:=fngethandle: "Name=[Q]\UBmstr\ubData\RateMst.h[cno],KFName=[Q]\UBmstr\ubData\RateIdx1.h[cno],Shr",internal,input,keyed
92120   restore #h_rate1: 
92140   mat rates$(99)
92160   mat rates$=("")
92180   fncreg_read('default rate '&str$(service_code),tmp_rate$)
92200   if tmp_rate$<>'' then 
92240     x=0
92260     if ratecode=0 then 
92280       ratecode=val(tmp_rate$(1:pos(tmp_rate$,'=')-1))
92300     end if 
92320   else 
92340     x=1
92360     rates$(1)=" 0=Not applicable"
92380   end if 
92400   do 
92420     read #h_rate1,using "Form POS 1,C 54",release: rt$ eof GCODE_FINIS
92440     if trim$(rt$(1:2))=searchcode$ then 
92460       x=x+1
92480       rates$(x)=rt$(3:4)&"="&rt$(5:25)
92500       if ratecode=val(rt$(3:4)) then rateInfo$(3)=rt$(3:4)&"="&rt$(5:25)
92520     end if 
92540   loop 
92560   GCODE_FINIS: ! 
92580   if x>0 then mat rates$(x) else mat rates$(1)
92600   if ratecode=0 then rateInfo$(3)=" 0=Not applicable"
92610   close #h_rate1: ioerr ignore
92620 fnend
