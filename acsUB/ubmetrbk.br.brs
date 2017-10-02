00010 ! Replace S:\acsUB\ubMetrBk
00020 ! pr Meter Reading Routes
02000 ! ______________________________________________________________________
02020   library "S:\Core\Library": fnacs,fnlbl,fnwait,fncmbrt2,fntos,fnopenprn,fncloseprn,fnerror,fnxit,fndat,fncmdset,fntop,fncomboa,fnpause,fnchk,fntxt,fnfra,fnopt,fnreg_write,fnreg_read,fnpa_finis,fnget_services,fngethandle
02040   on error goto ERTN
02060 ! ______________________________________________________________________
02080   dim z$*10,e$(4)*30,x$*10,f$(1)*12,f$(3)*12,cap$*128,dat$*20,service$(1)*2,bulksort$*12
02100   dim a(7),option$(4)*25,extra(17),x_service$(3)*62,rm$*132,ft$*60
02120   dim resp$(11)*40
02140   dim notefile$*100,notedir$*100,ul$*60,d(15),snm$(10)*20,srv$(10)*2
02160 ! ______________________________________________________________________
02180   let fntop(program$,cap$="Meter Reading Book")
02240   let fndat(dat$,1)
02260   if env$('client')="Gilbertown" then let seq=2
02280 ! r: this section+the comboA on the first screen is just what you need  for a fnCurrently availableServiceTypeComboBox
02300   let fnget_services(mat snm$,mat srv$)
02360   let x=0
02380   let option$(x+=1)="[All]"
02400   for j=1 to 4
02420     if j=1 and trim$(snm$(j))="Water" then 
02440       let option$(x+=1)=srv$(j)&"-"&snm$(j)(1:20)
02460       let x_service=x_service+1
02480       mat service$(x_service)=service$
02500       let service$(x_service)="WA"
02520     else if j=3 and (trim$(snm$(j))="Electric" or trim$(srv$(3)))="EL" then 
02540       let option$(x+=1)=srv$(j)&"-"&snm$(j)(1:20)
02560       let x_service=x_service+1
02580       mat service$(x_service)=service$
02600       let service$(x_service)="EL"
02620     else if j=4 and (trim$(snm$(j))="Gas" or trim$(srv$(4))="GA") then 
02640       let option$(x+=1)=srv$(j)&"-"&snm$(j)(1:20)
02660       let x_service=x_service+1
02680       mat service$(x_service)=service$
02700       let service$(x_service)="GA"
02720     end if 
02740   next j
02760   mat option$(x)
02780   mat x_service$(x_service)
02800   let x_service =udim(x_service$)
02820 ! /r
02900   if env$('client')='Campbell' then let fn_campbell_meter_book : goto XIT
08000 MENU1: ! r:
08020   let fntos(sn$="ubMetrRt")
08040   let mylen=22 : let mypos=mylen+2 : let respc=lc=0
08060   let fnlbl(lc+=1,1,"Route Number:",mylen,1)
08080   let fncmbrt2(lc,mypos)
08100   let resp$(1)=fn_reg_try$(sn$&'.route',"1")
08120   let fnlbl(lc+=1,1,"Service Type:",mylen,1)
08140   let fncomboa("ubrate3",lc,mypos,mat option$)
08160   let resp$(2)=fn_reg_try$(sn$&'.service type',option$(1))
08170   let lc+=1
08180   let fnchk(lc+=1,mylen+2,"Print Footnotes:",1)
08200   let resp$(3)=fn_reg_try$(sn$&'.print footnotes',"False")
08220   let fnchk(lc+=1,mylen+2,"Double Space:",1)
08240   let resp$(4)=fn_reg_try$(sn$&'.double space',"False")
08260   let fnchk(lc+=1,mylen+2,"Blank Used Column:",1)
08280   let resp$(5)=fn_reg_try$(sn$&'.blank used column',"False")
08300   let fnchk(lc+=1,mylen+2,"Show Disconnects:",1)
08320   let resp$(6)=fn_reg_try$(sn$&'.show disconnects',"False")
08340   let fnchk(lc+=1,mylen+2,"Print Prior Usage:",1)
08360   let resp$(7)=fn_reg_try$(sn$&'.print prior usage',"False")
08380   let fnchk(lc+=1,mylen+2,"Skip Meter Number:",1)
08400   let resp$(8)=fn_reg_try$(sn$&'.skip meter number',"False")
08410   let lc+=1
08420   let fnlbl(lc+=1,1,"Number of Blank Lines:",mylen-2,1)
08440   let fntxt(lc,mylen+2,2,0,0,"30",0,"If you want extra space between accounts, enter the number of lines.")
08460   let resp$(9)=fn_reg_try$(sn$&'.# of blank lines',str$(extralines))
08470   let lc+=1
08480   let fnfra(lc+=1,10,2,26,"Order for printing","The billing journal can be printed if route # sequence, Account sequence or name sequence.",0)
08500   let fnopt(1,2,"Route Number Sequence",0,1)
08520   let resp$(10)=fn_reg_try$(sn$&'.route number sequence',"True")
08540   let fnopt(2,2,"Account Sequence",0,1)
08560   let resp$(11)=fn_reg_try$(sn$&'.account sequence',"False")
08580   let fncmdset(3)
08600   let fnacs(sn$,0,mat resp$,ck)
10000   if ck=5 then goto XIT
10020   if uprc$(resp$(1))=uprc$("[All]") then let route=0 else let route=val(resp$(1))
10040   let svt$=resp$(2)
10060   if resp$(2)<>"[All]" then let x_service=1 ! only printing one service if not answered as all
10080   if resp$(3)(1:1)="T" then let remark=1 ! yes want remarks printed
10100   if resp$(4)(1:1)="T" then let double=1 ! yes want to double space
10120   if resp$(5)(1:1)="T" then let usedcolumn=1 ! yes want a gallons used column
10140   if resp$(6)(1:1)="T" then let skipdisconnects=1 ! yes want show disconnects
10160   if resp$(7)(1:1)="T" then let printprior=1 ! pr prior months usage
10180   if resp$(8)(1:1)="T" then let skipmeter=1 ! don't pr meter number
10200   let extralines=val(resp$(9))
10220   if resp$(10)="True" then let seq=1
10240   if resp$(11)="True" then let seq=2
10260   let fnreg_write(sn$&'.route',resp$(1))
10280   let fnreg_write(sn$&'.service type',resp$(2))
10300   let fnreg_write(sn$&'.print footnotes',resp$(3))
10320   let fnreg_write(sn$&'.double space',resp$(4))
10340   let fnreg_write(sn$&'.blank used column',resp$(5))
10360   let fnreg_write(sn$&'.show disconnects',resp$(6))
10380   let fnreg_write(sn$&'.print prior usage',resp$(7))
10400   let fnreg_write(sn$&'.skip meter number',resp$(8))
10420   let fnreg_write(sn$&'.# of blank lines',resp$(9))
10440   let fnreg_write(sn$&'.route number sequence',resp$(10))
10460   let fnreg_write(sn$&'.account sequence',resp$(11))
10480 ! /r
10500   if seq=2 then 
10520     open #h_customer:=1: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,input,keyed 
10540   else 
10560     open #h_customer:=1: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndx5.h"&env$('cno')&",Shr",internal,input,keyed 
10580   end if 
10600 ! ______________________________________________________________________
12000 ! on fkey 5 goto DONE
12020   let fnopenprn
12040   on pageoflow goto PGOF
12060   gosub HEADER
15000 LOOP_TOP: ! r: main loop
15020 L730: ! 
15040   read #h_customer,using 'Form POS 1,C 10,4*C 30,C 12,POS 217,15*PD 5,POS 373,C 12,POS 361,C 12,POS 1741,N 2,POS 1821,N 1,pos 143,7*pd 2,pos 1806,3*n 2,pos 1821,n 1,pos 1942,c 12': z$,mat e$,f$(1),mat d,f$(3),f$(2),extra(1),fbc,mat a,extra(11),extra(12),extra(13),extra(17),bulksort$ eof DONE
15060   if extra(17)>0 and skipdisconnects=0 then goto L730
15080   if route=0 then goto L790
15100   if extra(1)<>route then goto LOOP_TOP
15120   if env$('client')="Gilbertown" then goto L790
15140   if a(1)=0 and a(3)=0 and a(4)=0 then goto LOOP_TOP ! don't have any services
15160 L790: let x=0
15180   for j=1 to udim(x_service$)
15200     if usedcolumn=1 then 
15260       let ul$="  {\ul          }  {\ul          }"
15280     else 
15300       let ul$="  {\ul          }"
15320     end if 
15340     if printprior=1 then let ul$="  {\ul          }"
15360     if printprior=1 and resp$(2)="[All]" and option$(j+1)(1:2)="WA" then 
15380       let prior=d(3)
15400       let x_service$(x+=1)=cnvrt$("pic(zzzzzzzzz)",d(1))&ul$&cnvrt$("pic(zzzzzzzzz)",prior)
15420     end if 
15440     if printprior=0 and resp$(2)="[All]" and option$(j+1)(1:2)="WA" then 
15520       let x_service$(x+=1)=cnvrt$("pic(zzzzzzzzz)",d(1))&ul$
15560     end if 
15580     if printprior=1 and resp$(2)(1:2)="WA" then 
15600       let prior=d(3)
15620       let x_service$(x+=1)=cnvrt$("pic(zzzzzzzzz)",d(1))&ul$&cnvrt$("pic(zzzzzzzzz)",prior)
15640       mat x_service$(1)
15660       goto L940
15680     end if 
15700     if printprior=1 and resp$(2)="[All]" and option$(j+1)(1:2)="EL" then 
15720       let prior=d(7)
15740       let x_service$(x+=1)=cnvrt$("pic(zzzzzzzzz)",d(5))&ul$&cnvrt$("pic(zzzzzzzzz)",prior)
15760     end if 
15780     if printprior=0 and resp$(2)="[All]" and option$(j+1)(1:2)="EL" then 
15800       let prior=d(7)
15820       let x_service$(x+=1)=cnvrt$("pic(zzzzzzzzz)",d(5))&ul$
15840     end if 
15860     if printprior=1 and resp$(2)(1:2)="EL" then 
15880       let prior=d(7)
15900       let x_service$(x+=1)=cnvrt$("pic(zzzzzzzzz)",d(5))&ul$&cnvrt$("pic(zzzzzzzzz)",prior)
15920       mat x_service$(1)
15940       goto L940
15960     end if 
15980     if printprior=1 and resp$(2)="[All]" and option$(j+1)(1:2)="GA" then 
16000       let prior=d(11)
16020       let x_service$(x+=1)=cnvrt$("pic(zzzzzzzzz)",d(9))&ul$&cnvrt$("pic(zzzzzzzzz)",prior)
16040     end if 
16060     if printprior=0 and resp$(2)="[All]" and option$(j+1)(1:2)="GA" then 
16140       let prior = d(11)
16160       let x_service$(x+=1)=cnvrt$("pic(zzzzzzzzz)",d(9))&ul$
16200     end if 
16220     if printprior=1 and resp$(2)(1:2)="GA" and option$(j+1)(1:2)="GA" then 
16240       let prior=d(11)
16260       let x_service$(x+=1)=cnvrt$("pic(zzzzzzzzz)",d(9))&ul$&cnvrt$("pic(zzzzzzzzz)",prior)
16280       mat x_service$(1)
16300       goto L940
16320     end if 
16340     if printprior=0 and resp$(2)(1:2)="GA" and option$(j+1)(1:2)="GA" then 
16360       let prior=d(11)
16380       let x_service$(x+=1)=cnvrt$("pic(zzzzzzzzz)",d(9))&ul$: mat x_service$(1)
16400       goto L940
16420     end if 
16440   next j
18000 L940: ! If env$('client')="Thomasboro" AND FBC<>0 Then Goto 500
18020   if fbc><0 then let e$(2)="Disconnect"
18040   if env$('client')="Thomasboro" then 
18120     pr #255,using 'Form POS 1,C 10,X 1,C 18,X 1,C 20,X 1,X_service*c 43': z$,e$(2)(1:18),e$(1)(1:20),mat x_service$
18140     goto L1050
18160   end if 
18180   if skipmeter=0 then 
18200     if printprior=1 then 
18220       pr #255,using 'Form POS 1,C 10,X 1,C 18,X 1,C 12,X 1,C 20,X 1,X_service*c 35,n 9': z$,e$(2)(1:18),f$(1),e$(1)(1:20),mat x_service$
18240     else if usedcolumn=0 then 
18260       pr #255,using L1032: z$,e$(2)(1:18),f$(1),e$(1)(1:20),mat x_service$
18280 L1032: form pos 1,c 10,x 1,c 18,x 1,c 12,x 1,c 20,x 1,x_service*c 26
18300     else if usedcolumn=1 then 
18320       pr #255,using 'Form POS 1,C 10,X 1,C 18,X 1,C 12,X 1,C 20,X 1,X_service*c 43,x 3': z$,e$(2)(1:18),f$(1),e$(1)(1:20),mat x_service$
18340     end if 
18360   else if printprior=1 then 
18380     pr #255,using 'Form POS 1,C 10,X 1,C 18,X 1,C 20,X 1,X_service*c 35,n 9': z$,e$(2)(1:18),e$(1)(1:20),mat x_service$
18400   else if usedcolumn=0 then 
18420     pr #255,using 'Form POS 1,C 10,X 1,C 18,X 1,C 20,X 1,X_service*c 26': z$,e$(2)(1:18),e$(1)(1:20),mat x_service$
18440   else if usedcolumn=1 then 
18460     pr #255,using 'Form POS 1,C 10,X 1,C 18,X 1,C 20,X 1,X_service*c 43,x 3': z$,e$(2)(1:18),e$(1)(1:20),mat x_service$
18480   end if 
18500 L1050: ! 
18520   if remark=1 then 
18540     gosub REMARK
18560     pr #255,using "form pos 1,c 60": ft$
18580   end if 
18600   if double=1 then pr #255: 
18620   if extralines>0 then 
18640     for j=1 to extralines
18660       pr #255: 
18680     next j
18700   end if 
18720   goto LOOP_TOP
18740 ! /r
21000 PGOF: pr #255: newpage : gosub HEADER : continue 
22000 DONE: ! r:
22020   close #h_customer: ioerr ignore
22040   let fncloseprn ! /r
22060 XIT: let fnxit
22080 ! ______________________________________________________________________
24000   def fn_reg_try$*256(field_name$*128,default_value$*256)
24020     dim rt_return$*256
24040     let fnreg_read(field_name$,rt_return$)
24060     if rt_return$='' then let rt_return$=default_value$
24080     let fn_reg_try$=rt_return$
24100   fnend  ! fn_reg_try$
26000 HEADER: ! r:
26020   if env$('client')="Thomasboro" then 
26040     pr #255: "\qc  {\f181 \fs22 \b "&env$('cnam')&"}"
26060     pr #255: "\qc  {\f181 \fs18 \b "&trim$(dat$)&"}"
26080     pr #255: ""
26100     pr #255: "\ql {\ul Account No} {\ul Name              } ";
26120     pr #255: "{\ul Meter Address          } ";
26140     goto L1190
26160   end if 
26180   pr #255: "\qc  {\f181 \fs22 \b "&env$('cnam')&"}"
26200   pr #255: "\qc  {\f181 \fs18 \b "&trim$(dat$)&"}"
26210   pr #255: "\qc  {\f181 \fs18 \b Page "&str$(page_count+=1)&"}"
26220   pr #255: ""
26240   if route<>0 then 
26260     pr #255: " Route: "&str$(route)
26280     pr #255: ""
26300   end if 
26460   pr #255: "\ql {\ul Account No} {\ul Name              } ";
26480   if skipmeter=1 then 
26500     pr #255: "{\ul Meter Address          } ";
26520   else 
26540     pr #255: "{\ul Meter Number} {\ul Meter Address          } ";
26560   end if 
26580 L1190: ! 
26600   for j=1 to x_service
26700     if printprior=1 then 
26720       pr #255: " {\ul Prior}    {\ul Current}    {\ul Usage}   ";
26740       goto L1230
26760     end if 
26780     if usedcolumn=0 then 
26800       pr #255: " {\ul Prior}    {\ul Current}   ";
26820     end if 
26840     if usedcolumn=1 then 
26860       pr #255: " {\ul Prior}    {\ul Current}      {\ul Used}   ";
26880     end if 
26900 L1230: ! 
26920   next j
26940   pr #255: " "
26960   if double=1 then 
26980     pr #255: ""
27000   end if 
27020   return  ! /r
32000 REMARK: ! r: ! read the footnote from the note file  (any note with * as first character
32020   let ft$="                    "
32040   let notedir$=env$('Q')&"\UBmstr\notes.h"&env$('cno')
32060   let notefile$=notedir$&"\"&trim$(z$)&".txt"
32080   if exists(notedir$)=0 then goto L1510
32100   open #20: "Name="&notefile$,display,input ioerr L1520
32120   do 
32140     linput #20: rm$ eof L1510
32160   loop until rm$(1:1)="*"
32200   let ft$=rpad$(rm$(2:60),60)
32220 L1510: ! 
32240   close #20: ioerr ignore
32260 L1520: ! 
32280   return  ! /r
34000 ! <Updateable Region: ERTN>
34020 ERTN: let fnerror(program$,err,line,act$,"xit")
34040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
34060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
34080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
34100 ERTN_EXEC_ACT: execute act$ : goto ERTN
34120 ! /region
34140 IGNORE: continue 
36000 def fn_campbell_meter_book ! Campbell Special Routine (printed once a year, one customer per page)
36020 dim z$*10,e$(4)*30,x$*10,f$(1)*12,f$(3)*12,message$*40,cap$*40,g(12),d(15)
36040 cap$="Print Meter Reading Books"
36060 fntos(sn$='umbc')
36080 let mylen=22 : let mypos=mylen+2 : let respc=lc=0
36100 let fnlbl(lc+=1,1,"Route Number:",mylen,1)
36120 let fncmbrt2(lc,mypos)
36140   let resp$(1)=fn_reg_try$(sn$&'.route',"[All]")
36160 ! let fnlbl(lc+=1,1,"Service Type:",mylen,1)
36180 ! fntxt(lc,mypos,2, 0,0,'',1)
36200 ! resp$(2)='WA'
36220 ! let fncomboa("ubrate3",lc,mypos,mat option$)
36240 let fncmdset(3)
36260 let fnacs(sn$,0,mat resp$,ck)
36280 if ck=5 then goto CAMPBELL_XIT
36300 if uprc$(resp$(1))=uprc$("[All]") then let route=0 else let route=val(resp$(1))
36320 service$="W"
36340 let fnreg_write(sn$&'.route',resp$(1))
36360 open #h_customer:=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,input,keyed 
36380 let fnopenprn
38000 do
38010 CAMPBELL_LOOP_TOP: ! 
38012   dim g(12)
38020   ! read #h_customer,using F_CAMPBELL_CUSTOMER: z$,mat e$,f$(1),watcode,elecode,litecode,mat d,f$(3),c4,f$(2), mat g eof CAMPBELL_DONE
38030   ! F_CAMPBELL_CUSTOMER:  form pos 1,c 10,4*c 30,c 12,pos 143,pd 2,pos 147,pd 2,pos 177,pd 4.2,pos 217,15*pd 5,pos 373,c 12,pos 213,pd 4,pos 361,c 12,pos 300,12*pd 4.2
38040   read #h_customer,using 'Form POS 1,C 10,4*C 30,C 12,POS 217,15*PD 5,POS 373,C 12,POS 361,C 12,POS 1741,N 2,POS 1821,N 1,pos 143,7*pd 2,pos 1806,3*n 2,pos 1821,n 1,pos 1942,c 12,pos 300,12*pd 4.2': z$,mat e$,f$(1),mat d,f$(3),f$(2),extra(1),fbc,mat a,extra(11),extra(12),extra(13),extra(17),bulksort$,mat g eof DONE
38050   watcode=a(1)
38060   ! elecode=a(3) ! no longer used  
38070   ! litecode=    ! no longer used
38080   if route and extra(1)<>route then goto CAMPBELL_LOOP_TOP
38100   if c4><0 then let e$(2)="DISCONNECT"
38120   if uprc$(service$)="W" and watcode=0 then goto CAMPBELL_LOOP_TOP ! if water selected and water code=0 then skip
38140   if uprc$(service$)="W" and watcode>0 then goto CAMPBELL_EO_LOOP ! if water selected and water code>0 then print
38160 ! if uprc$(service$)="E" and elecode>0 then goto CAMPBELL_EO_LOOP ! if elec selected and elec code>0 then print
38180 loop
40000 CAMPBELL_EO_LOOP: ! 
40020 !  if uprc$(service$)="W" then  ! WATER
40040   for j=1 to 39
40060     pr #255:''
40080   nex j
40100   pr #255,using F_CAMPBELL_OUT_W_1: D(1),D(3),G(1),G(2),G(5)
40120   F_CAMPBELL_OUT_W_1: form pos 8,n 9,x 2,n 8,n 8.2,n 6.2,n 6.2
40140   pr #255:''
40160   pr #255,using 'form pos 6,c 30': E$(1)
40180   pr #255:''
40200   pr #255,using 'form pos 6,c 10': Z$
40220   pr #255,using 'form pos 6,c 30': E$(2)
40240   pr #255,using 'form pos 6,c 30': E$(3) 
40260   pr #255,using 'form pos 6,c 30': E$(4) 
40280   pr #255: newpage
40300   goto CAMPBELL_LOOP_TOP
40320   CAMPBELL_DONE: ! 
40340   close #h_customer: Ioerr ignore
40360   let fncloseprn
40380   CAMPBELL_XIT: !
40400   close #h_customer:
40420 fnend
