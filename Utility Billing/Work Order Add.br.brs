18000 library program$: fnworkOrderAdd
18010 library 'S:\Core\Library': fnxit,fntop,fnask_account,fngethandle
18020 let fntop(program$)
18030 open #h_customer:=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,input,keyed 
18040 do
18050   if fnask_account('Work Order',z$,h_customer)=5 then 
18060     goto XIT
18070   else
18080     fnworkOrderAdd(z$)
18090   end if 
18100 loop
18110 XIT: !
18120 close #h_customer:
18130 let fnxit
32000 def library fnworkOrderAdd(z$*10)
32020   if ~wo_setup then ! r:
32040     wo_setup=1
32060     library 'S:\Core\Library': fnWorkOrderPrint,fnacs,fntos,fnlbl,fntxt
32070     library 'S:\Core\Library': fngethandle
32080     library 'S:\Core\Library': fnNoteDir$,fncmdkey,fnWorkOrderList,fnerror
32100     on error goto ERTN
32120     dim ws$(13)*30
32140     dim workinfo$(15)*512
32160     dim i$(16)*320
32180     dim line$(5)*100
32200     let ws$(1)="Date Order Taken:"
32220     let ws$(2)="Taken by:"
32240     let ws$(3)="Date Last Reading:"
32260     let ws$(4)="Date to be Completed:"
32280     let ws$(5)="Request made by:"
32300     let ws$(6)="Phone:"
32320     let ws$(7)="Name In:"
32340     let ws$(8)="Turn On:"
32360     let ws$(9)="Turn Off:"
32380     let ws$(10)="Leave On:"
32400     let ws$(11)="Forwarding Address:"
32420     let ws$(12)="Forwarding City St Zip:"
32440     let ws$(13)="Comments:"
32460     dim customer_name$*30
32480     dim customer_phone_number$*12
32500     dim z$*10
32520     dim e$(4)*30
32540     dim f$(3)*12
32560     dim a(7)
32580     dim b(11)
32600     dim c(4)
32620     dim d(15)
32640     dim g(12)
32660     dim adr(2)
32680     dim alp$*7
32700     dim gb(10)
32720     dim rw4(22,13)
32740     dim extra(23)
32760     dim extra$(11)*30
32780   end if ! /r

33000   dat$=date$("Month DD, CCYY")
33010   open #wo_h_customer:=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,input,keyed 
33020   ! really only need these: (z$,mat e$,mat i$,mat line$,mat a,mat b,mat d,mat f$,mat extra$)
33030   read #wo_h_customer,using F_CUSTOMER_1,key=z$: z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,f,mat g,mat adr,alp$,f$(2),f$(3),bra,mat gb,mat rw4,df$,dr$,dc$,da$,mat extra,mat extra$
33040   F_CUSTOMER_1: form pos 1,c 10,4*c 30,c 12,7*pd 2,11*pd 4.2,4*pd 4,15*pd 5,pd 4.2,pd 4,12*pd 4.2,2*pd 3,c 7,2*c 12,pd 3,10*pd 5.2,78*pd 5,13*pd 4.2,13*n 6,156*pd 4.2,13*n 6,13*pd 4.2,c 1,c 9,c 2,c 17,n 2,n 7,2*n 6,n 9,pd 5.2,n 3,3*n 9,3*n 2,3*n 3,n 1,3*n 9,3*pd 5.2,c 30,7*c 12,3*c 30
33050   customer_name$=e$(2)
33060   customer_phone_number$=extra$(2)
33070   close #wo_h_customer:

34000   WO_TOS: !
34020   fntos(sn$="workorder")
34040   respc=0
34060   fnlbl(1,30,"WORK ORDER",20,0,4)
34080   fnlbl(2,1,"Account:",10,1)
34160   fntxt(2,12,10,0,1,"",1)
34200   let workinfo$(respc_accont=respc+=1)=z$
34220   fnlbl(2,24,"Name:",5,1)
34240   fntxt(2,31,25,30,0,"",1)
34260   let workinfo$(respc+=1)=customer_name$
34280   fnlbl(4,1,"Date Order Taken:",23,1)
34300   fntxt(4,25,25)
34320   let workinfo$(respc+=1)=dat$
34340   fnlbl(5,1,ws$(2),23,1)
34360   fntxt(5,25,25)
34380   let workinfo$(respc+=1)=""
34400   fnlbl(6,1,ws$(3),23,1)
34420   fntxt(6,25,25)
34440   let workinfo$(respc+=1)=""
34460   fnlbl(7,1,ws$(4),23,1)
34480   fntxt(7,25,25)
34500   let workinfo$(respc+=1)=""
34520   fnlbl(8,1,ws$(5),23,1)
34540   fntxt(8,25,25)
34560   let workinfo$(respc+=1)=""
34580   fnlbl(9,1,ws$(6),23,1)
34600   fntxt(9,25,14)
34620   let workinfo$(respc+=1)=customer_phone_number$
34640   fnlbl(10,1,ws$(7),23,1)
34660   fntxt(10,25,25)
34680   let workinfo$(respc+=1)=""
34700   fnlbl(11,1,ws$(8),23,1)
34720   fntxt(11,25,8,0,0,"1")
34740   let workinfo$(respc+=1)=""
34760   fnlbl(12,1,ws$(9),23,1)
34780   fntxt(12,25,8,0,0,"1")
34800   let workinfo$(respc+=1)=""
34820   fnlbl(13,1,ws$(10),23,1)
34840   fntxt(13,25,8)
34860   let workinfo$(respc+=1)=""
34880   fnlbl(14,1,ws$(11),23,1)
34900   fntxt(14,25,30)
34920   let workinfo$(respc+=1)=""
34940   fnlbl(15,1,ws$(12),23,1)
34960   fntxt(15,25,30)
34980   let workinfo$(respc+=1)=""
35000   fnlbl(16,1,ws$(13),23,1)
35020   fntxt(16,25,50,280)
35040   let workinfo$(respc+=1)=""
35060   fncmdkey("Print History",8,0,0,"This allows you to review the description of any work order issued in the past")
35080   fncmdkey("&Print",1,1,0,"Prints a workorder on this customer for the information entered above.")
35100   fncmdkey("&Cancel",5,0,1,"Returns to main customer record.")
35120   fnacs(sn$,0,mat workinfo$,ckey) ! work order screen

38000   if ckey=5 then goto woaXIT
38020   z$=workinfo$(respc_accont)(1:10) ! lpad$(trim$(workinfo$(respc_accont)(1:10)),10)
38160   if ckey=8 then let fnWorkOrderList(z$) : goto WO_TOS
38180   for j=3 to 15 : i$(j-2)=workinfo$(j) : next j
38200   for j=1 to 12
38220     if trim$(workinfo$(j+2))="" then i$(j)="________________"
38240   next j
38260   for j=2 to 12
38280     if i$(j)(1:5)<>"_____" then i$(j)="{\ul "&i$(j)&"}" ! underline the answer if there was one
38300   next j
38320   let y=55: let z=1
38340   for j=1 to 5
38360     let x=pos(i$(13)," ",y)
38380     if x>0 and x<=j*70 then 
38400       line$(j)=i$(13)(z:x)
38420       let z=x+1
38440       let y=x+55
38460     end if 
38480     if x=0 or x>j*70 then 
38500       line$(j)=i$(13)(z:j*70)
38520       let y=z+70
38540       let z=z+70
38560     end if 
38580   next j

40000   fnWorkOrderPrint(z$,mat e$,mat i$,mat line$,mat a,mat b,mat d,mat f$,mat extra$)
40020 ! fn_workorder_print_legacy

43000    ! r: write to WorkOrder History file (z$)
43020     open #h_workorder:=fngethandle: "Name="&env$('Q')&"\UBmstr\WorkOrder.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\wkIndex.h"&env$('cno')&",Shr",internal,outin,keyed
43040     write #h_workorder,using "form pos 1,Cr 10,n 8,c 30,5*c 100": z$,date('ccyymmdd'),customer_name$,mat line$
43060     close #h_workorder: 
43080   ! /r
44000   ! r: append to note file
44020     open #h_notefile:=fngethandle: "Name="&fnNoteDir$&"\"&trim$(z$)&".txt,Use",display,output
44040     pr #h_notefile: '** Work Order added '&date$('mm/dd/ccyy')&' at '&time$&' **'
44060     pr #h_notefile:   '              Account: '&z$&'  '&customer_name$
44080     if fn_not_blank(i$(5)) then
44100       pr #h_notefile: "      Request made by: "&fn_clean_ul$(i$(5))
44120     end if
44140     if fn_not_blank(i$(2)) then
44160       pr #h_notefile: "             Taken by: "&fn_clean_ul$(i$(2))
44180     end if
44200     if fn_not_blank(i$(7)) then
44220       pr #h_notefile: "              Name In: "&fn_clean_ul$(i$(7))
44240     end if
44260     if fn_not_blank(i$(3)) then
44280       pr #h_notefile: "    Date Last Reading: "&fn_clean_ul$(i$(3))
44300     end if
44320     if fn_not_blank(i$(4)) then
44340       pr #h_notefile: " Date to be Completed: "&fn_clean_ul$(i$(4))
44360     end if
44380     if fn_not_blank(i$(8)) then
44400       pr #h_notefile: "              Turn On: "&date$(days(fn_clean_ul$(i$(8),1008),'mmddyy'),'mm/dd/ccyy') ! fn_clean_ul$(i$(8)) : pause
44420     end if
44440     if fn_not_blank(i$(9)) then
44460       pr #h_notefile: "             Turn Off: "&date$(days(fn_clean_ul$(i$(9),1008),'mmddyy'),'mm/dd/ccyy')
44480     end if
44500     if fn_not_blank(i$(10)) then
44520       pr #h_notefile: "             Leave On: "&fn_clean_ul$(i$(10))
44540     end if
44560     if fn_not_blank(i$(11)&i$(12)) then
44580       pr #h_notefile: "   Forwarding Address: "&fn_clean_ul$(i$(11))&"  "&fn_clean_ul$(i$(12))
44600     end if
44620     for lineItem=1 to udim(mat line$)
44640       if trim$(line$(lineItem))<>'' then
44660         pr #h_notefile: '  '&line$(lineItem)
44680       end if
44700     nex lineItem
44720     pr #h_notefile: '**'
44740     close #h_notefile: 
44760   ! /r
48000   woaXIT: !
48020 fnend
50000 def fn_clean_ul$*256(cu_in$*256; cu_reformat)
50020   cu_len=len(rtrm$(cu_in$))
50040   if cu_in$(1:5)='{\ul ' and cu_in$(cu_len:cu_len)='}' then 
50060     cu_in$(cu_len:cu_len)=''
50080     cu_in$(1:5)=''
50100   end if
50120   if cu_reformat=1008 then ! means that it is a date and it may need a leading zero added
50140     if len(cu_in$)<8 then
50160       cu_in$='0'&cu_in$
50180     end if
50200   end if
50220   fn_clean_ul$=cu_in$
50240  fnend
73000 ERTN: ! r:
73020   fnerror(program$,err,line,act$,"xit")
73040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
73060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
73080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
73100 ERTN_EXEC_ACT: execute act$ : goto ERTN
73120 ! /r
77000 def fn_workorder_print_legacy
77010   library 'S:\Core\Library': fnopenprn,fncloseprn,fnsavetoasstart
77030 ! if exists(env$('Q')&"\WorkOrder")=0 then execute "mkdir "&env$('Q')&"\WorkOrder -n"
77032   fnsavetoasstart(env$('Q')&"\WorkOrder\"&trim$(z$)&date$("ccyymmdd")&".rtf")
77034   fnopenprn
77040   pr #255: "\qc {\f181 {\fs32 {\b Utility Work Order}"
77050   pr #255: "{\fs24 "&env$('cnam')&"}}}"
77060   pr #255: "\qc {\fs20 "&trim$(i$(1))&"}"
77070   if trim$(srvnam$(3))<>"Electric" or trim$(srvnam$(3))<>"Lawn Meter" or trim$(srvnam$(4))<>"Gas" then 
77080     pr #255,using "Form POS 1,C 1,SKIP 3": " " ! extra lines at top if either gas or electric not used
77090   end if 
77100   pr #255: "\ql "
77110   pr #255: "{\b "&ws$(3)&"}"&i$(3)&"       {\b "&ws$(2)&"}"&i$(2)
77120   pr #255: ""
77130   pr #255: "{\b "&ws$(4)&"}"&i$(4)
77140   pr #255: ""
77150   pr #255: "         {\b "&ws$(5)&"}"&i$(5)&"     {\b "&ws$(6)&"}"&i$(6)
77160   pr #255: ""
77170 L10730: form pos 10,c 132,skip 2
77180   pr #255,using L10730: "{\b Service Address: }{\ul "&e$(1)&"}"
77190   pr #255,using L10730: "{\b    Meter number: }"&i$(7)
77200   pr #255,using L10730: "{\b        Name Out: }{\ul "&customer_name$&"}{\b           Account :}{\ul "&z$&"}"
77210   pr #255,using "Form pos 10,C 132": "{\b         Name In: }"&i$(7)
77220   pr #255: ""
77230   pr #255,using "Form pos 10,C 132": "{\b         "&ws$(8)&"}"&i$(8)(1:12)&"    {\b "&ws$(9)&"}"&i$(9)(1:12)&"    {\b "&ws$(10)&"}"&i$(10)(1:12)
77240   pr #255: ""
77250   pr #255,using "Form pos 10,C 132": "{\b "&ws$(11)&"}"&i$(11)&"  "&i$(12)(1:23)
77260   pr #255: ""
77270   fn_pwo_service_data(srvnam$(1),d(1),f$(1),extra$(3)) ! Water
77280   fn_pwo_service_data(srvnam$(3),d(5),f$(2),extra$(4)) ! Electric or Lawn Meter
77290   fn_pwo_service_data(srvnam$(4),d(9),f$(3),extra$(5)) ! Gas
77300   pr #255: "\qc ";"{\b Comments:}"
77310   pr #255: ""
77320   pr #255: "\ql "
77330   for j=1 to 5
77340     if trim$(line$(j))<>"" then 
77350       pr #255,using "Form pos 10,C 132": line$(j)
77360     else 
77370       pr #255,using "Form pos 10,C 132": rpt$("_",80)
77380     end if 
77390     pr #255: ""
77400   next j
77410   for j=1 to 2
77420     pr #255,using "Form pos 10,C 132": rpt$("_",80)
77430     pr #255: ""
77440   next j
77450   if trim$(srvnam$(1))<>"" then 
77460     pr #255,using 'form pos 1,cr 25,n 7.2,x 2,c 45': "{\b Water Deposit:}",b(8),"{\b Water Code: }"&str$(a(1))
77470   end if 
77480   if trim$(srvnam$(2))<>"" then 
77490     pr #255,using 'form pos 1,cr 25,n 7.2,x 2,c 45': "{\b Sewer Deposit:}",b(9),"{\b Sewer Code: }"&str$(a(2))
77500   end if 
77510   if trim$(srvnam$(3))="Electric" then 
77520     pr #255,using 'form pos 1,cr 25,n 7.2,x 2,c 45': "{\b Electric Deposit:}",b(10),"{\b Electric Code:}"&str$(a(3))
77530   end if 
77540   if trim$(srvnam$(3))="Lawn Meter" then 
77550     pr #255,using 'form pos 1,cr 25,n 7.2,x 2,c 45': "{\b Lawn Meter Deposit:}",b(10),"{\b  }"&str$(a(3))
77560   end if 
77570   if trim$(srvnam$(3))="Lawn Meter" then 
77580     pr #255,using 'form pos 1,cr 25,n 7.2,x 2,c 45': "{\b Lawn Meter Deposit:}",b(10),"{\b }"&str$(a(3))
77590   end if 
77600   if trim$(srvnam$(4))="Gas" then 
77610     pr #255,using 'form pos 1,cr 25,n 7.2,x 2,c 45': "{\b Gas Deposit:}",b(11), "{\b Gas Code:}"&str$(a(4))
77620   end if 
77630   pr #255: ""
77640   pr #255,using 'form pos 47,c 51': "{\b Date Order Completed: _____________________}"
77650   pr #255: ""
77660   pr #255,using 'form pos 47,c 51': "{\b By: _______________________________________}"
77670   fncloseprn
77680 fnend 
77690 def fn_pwo_service_data(service_name$*80,reading_prior,meter_number$,serial_number$)
77700   if trim$(service_name$)<>"" and reading_prior>0 then 
77710     pr #255: "{\b <-------------"&trim$(service_name$)&"--------------->}"
77720     pr #255: ""
77730     pr #255,using 'form pos 1,c 25,g 20': "{\b Previous Reading:}",str$(reading_prior)
77740     pr #255: ""
77750     if trim$(meter_number$)<>'' then 
77760       pr #255,using 'form pos 1,c 25,g 20': "{\b Meter Number:}","{\ul "&meter_number$&"}"
77770       pr #255: ""
77780     end if 
77790     if trim$(serial_number$)<>'' then 
77800       pr #255,using 'form pos 1,c 25,g 20': "{\b Serial Number:}","{\ul "&serial_number$&"}"
77810       pr #255: ""
77820     end if 
77830     pr #255,using 'form pos 1,c 25,g 20': "{\b Current Reading:}","______________"
77840     pr #255: ""
77850   end if 
77860 fnend 
82000 def fn_not_blank(nbTestText$*256)
82010   nbReturn=1
82020   nbTestText$=srep$(nbTestText$,'0','')
82030   nbTestText$=srep$(nbTestText$,'{\ul ','')
82040   nbTestText$=srep$(nbTestText$,'}','')
82050   nbTestText$=srep$(nbTestText$,' ','')
82060   nbTestText$=srep$(nbTestText$,'_','')
82080   if nbTestText$='' then
82100     nbReturn=0
82120   end if
82140   fn_not_blank=nbReturn
82160 fnend