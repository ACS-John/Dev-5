10000 ! Replace S:\acsUB\reset_readings
10200 ! -- reset current or prior reading to what it was on a given transaction date.
10400 ! ______________________________________________________________________
10600   library 'S:\Core\Library': fndate_mmddyy_to_ccyymmdd,fncno,fnxit,fnerror,fntos,fnlbl,fnacs,fntxt,fnwait,fncmdset,fntop,fnpause,fnopt,fngethandle,fnchk
10800   on error goto ERTN
11000 ! ______________________________________________________________________
11200   dim x$*10,x(15),w(5),r(4),gb(10),rt(10,3),ba(13),da(2),txt$(3)*80,txt$*50
11400   dim a(7),b(11),c(4),d(15),g(12),rw(22,13),d$*6,dat$*20,bt1(14,2)
11600   dim p$*10,o(2),bt2(14,2),badr(2),dp$*60,tg(11),transkey$*19,meteradr$*30,custname$*30
11800   dim watuse(12),watdat(12),elecuse(12),elecdat(12),gasuse(12),gasdat(12)
12000   dim servicename$(10)*20,servicecode$(10)*2,tax_code$(10)*1,work$*80
12200   dim penatly$(10)*1,subjectto(10)
12400   dim extra(23),extra$(11)*30,client$*30
12600   dim cap$*128,work$*80,work_addr$*80
12800 ! ______________________________________________________________________
13000   let fncno(cno) : !  :  !
13200   let fntop("S:\acsUB\reset_readings",cap$="Reset Readings")
13400   dim srvnam$(10)*20,srv$(10)*2
13600   open #20: "Name="&env$('Q')&"\UBmstr\ubData\Service.h"&str$(cno)&",Shr",internal,input,relative 
13800   read #20,using "Form POS 1,10*C 20,10*C 2",rec=1: mat srvnam$,mat srv$
14000   close #20: 
14200 ! ______________________________________________________________________
14400 SCREEN1: ! 
14600   let fntos(sn$='resetreadings3')
14800   let mylen=22 : let mypos=mylen+2
15000   let fnlbl(1,1,"Transaction Date (mmddyy):",mylen,1)
15200   let fntxt(1,mypos,8,0,1,"1001") : let resp$(1)='' ! '070611'
15400   let fnlbl(3,1,"Reading to Reset:",mylen,1)
15600   let fnopt(3,mypos,"Current") : let resp$(2)='True'
15800   let fnopt(4,mypos,"Prior") : let resp$(3)='False'
16000   let fnchk(6,mypos,srvnam$(1),1) : let resp$(4)='True'
16200   let fnchk(7,mypos,srvnam$(4),1) : let resp$(5)='True'
16400   let fnchk(9,mypos,"Update Usages",1) : let resp$(6)='False'
16600   let fncmdset(2)
16800   let fnacs(sn$,0,mat resp$,ck)
17000   if ck=5 then goto XIT
17200   let d1=fndate_mmddyy_to_ccyymmdd(val(resp$(1)))
17400   if resp$(2)='True' then let do_current=1 else let do_current=0
17600   if resp$(4)='True' then let do_water=1 else let do_water=0
17800   if resp$(5)='True' then let do_gas=1 else let do_gas=0
18000   if resp$(6)='True' then let do_usages=1 else let do_usages=0
18200   execute "Index "&env$('Q')&"\UBmstr\UBTransVB.h"&str$(cno)&' '&env$('Q')&"\UBmstr\UTV_Date.h"&str$(cno)&" 11 8 Replace DupKeys -n"
18400   open #h_trans=fngethandle: "Name="&env$('Q')&"\UBmstr\ubtransvb.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\UTV_Date.h"&str$(cno)&",Shr",internal,input,keyed 
18600   open #h_customer:=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,outin,keyed 
18800 F_CUSTOMER: form pos 11,2*c 30,pos 143,7*pd 2,pos 157,11*pd 4.2,pos 201,4*pd 4,pos 217,15*pd 5,pos 292,pd 4.2,pos 296,pd 4,pos 300,12*pd 4.2,pos 388,10*pd 5.2,pos 1741,n 2,n 7,2*n 6,n 9,pd 5.2,n 3,3*n 9,3*n 2,3*n 3,n 1,3*n 9,3*pd 5.2,c 30,7*c 12,3*c 30
19000 ! ______________________________________________________________________
19200   restore #h_trans,key=str$(d1): nokey SCREEN1
19400   do 
19600     read #h_trans,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1': p$,tdate,tcode,tamount,mat tg,_wr,wu,er,eu,gr,gu,tbal,pcode eof XIT
19800     if tdate<>d1 then goto XIT
20000     read #h_customer,using F_CUSTOMER,key=p$: meteradr$,custname$,mat a,mat b,mat c,mat d, bal,f,mat g,mat gb,mat extra nokey NEXT_ONE
20200 ! if trim$(p$)='100002.00' then pause
20400     if do_water then let fn_reading_fix(d(1),d(2),d(3),d(4),_wr) ! water
20600     if do_gas then let fn_reading_fix(d(9),d(10),d(11),d(12),gr) ! gas
20800 ! <Updateable Region: ERTN>
21000     rewrite #h_customer,using F_CUSTOMER,key=p$: meteradr$,custname$,mat a,mat b,mat c,mat d,bal,f,mat g,mat gb,mat extra
21200 NEXT_ONE: ! 
21400   loop 
21600 XIT: ! 
21800   let fnxit
22000 ! ______________________________________________________________________
22200 ! <Updateable Region: ERTN>
22400 ERTN: let fnerror(program$,err,line,act$,"NO")
22600   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
22800   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
23000   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
23200 ERTN_EXEC_ACT: execute act$ : goto ERTN
23400 ! /region
23600 ! ______________________________________________________________________
23800   def fn_reading_fix(&reading_current,&reading_prior,&usage_current,&usage_ytd,reading_new)
24000     let rf_reading_prior=reading_prior
24200     let rf_reading_cur=reading_current
24400     if do_current then 
24600       let rf_reading_cur=reading_new
24800     else 
25000       let rf_reading_prior=reading_new
25200     end if 
25400     let reading_current=rf_reading_cur
25600     let reading_prior=rf_reading_prior
25800     if do_usages then 
26000       let usage_ytd-=usage_current
26200       let usage_current=max(0,reading_current-reading_prior)
26400       let usage_ytd+=usage_current
26600     end if  ! do_usages
26800   fnend  ! fn_reading_fix
