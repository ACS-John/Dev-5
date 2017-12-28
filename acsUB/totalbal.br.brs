00010 ! Replace S:\acsUB\totalBal
00030 fn_setup
00110 fntop(program$,"View Total Accounts Receivable")
11000 do 
12000   fnTos(sn$="totalBal")
12020   mylen=28 : mypos=mylen+2
12040   fnLbl(1,1,"Total Accounts Receivable:",28,1)
12060   fnTxt(1,mypos,18,0,1,"10",1)
12080   resp$(1)=str$(fn_total_ar)
12100   fnChk(3,mypos,'Exclude Final Billed', 1)
12120   resp$(2)=excludeFinalBilled$
12130   fnCmdKey('Close',5,0,1,'Save option(s) and exit')
12140   fnCmdKey('Refresh',1,1,0,'Recalculate')
12160   fnAcs(sn$,0,mat resp$,ck)
12180   excludeFinalBilled$=resp$(2)
12200   fncreg_write(env$('program_caption')&' - '&'Exclude Final Billed',excludeFinalBilled$)
13000 loop until ck=5
14000 XIT: fnxit
16000 def fn_setup
16010   if ~setup then 
16020     setup=1
16030     library 'S:\Core\Library': fntop,fnxit, fnerror,fngethandle,fnLastBillingDate
16031     library 'S:\Core\Library': fnAcs,fnLbl,fnTxt,fnChk,fnTos,fnCmdKey
16032     library 'S:\Core\Library': fncreg_read,fncreg_write
16040     on error goto ERTN
16090   end if 
16500   fnLastBillingDate(lastBillingDate)
16520   fncreg_read(env$('program_caption')&' - '&'Exclude Final Billed',excludeFinalBilled$, 'False')
16540   ! if env$('client')='French Settlement' then filterByBillingDate=1 else filterByBillingDate=0
16900 fnend 
18000 ! <Updateable Region: ERTN>
18010 ERTN: fnerror(program$,err,line,act$,"xit")
18020   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
18030   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
18040   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
18050 ERTN_EXEC_ACT: execute act$ : goto ERTN
18060 ! /region
22000 def library fntotal_ar
22010   fn_setup
22020   fntotal_ar=fn_total_ar
22030 fnend 
24000 def fn_total_ar
24020   totalBal=0
24060   open #h_customer:=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",Shr",internal,input
25000   do 
25020     read #h_customer,using "Form POS 292,PD 4.2,PD 4,pos 1821,N 1": bal,customerBillingDate,finalBillingCode eof TA_FINIS
25040     if excludeFinalBilled$='False' or finalBillingCode=0 then 
25050       ! pr lastBillingDate,customerBillingDate : pause
25060       if ~filterByBillingDate or lastBillingDate=customerBillingDate then 
25080         totalBal+=bal
25100       end if
25120     end if
25140   loop 
26000   TA_FINIS: ! 
26020   close #h_customer: 
26040   fn_total_ar=totalBal
26060 fnend 
