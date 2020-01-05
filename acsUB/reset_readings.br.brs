10000 ! Replace S:\acsUB\reset_readings
10200 ! -- reset current or prior reading to what it was on a given transaction date.
10400 ! ______________________________________________________________________
10600   library 'S:\Core\Library': fndate_mmddyy_to_ccyymmdd,fnget_services,fnxit,fnerror,fnTos,fnLbl,fnAcs,fnTxt,fnwait,fnCmdSet,fntop,fnpause,fnOpt,fngethandle,fnChk
10800   on error goto Ertn
11000 ! ______________________________________________________________________
11200   dim x$*10,x(15),w(5),r(4),gb(10),rt(10,3),ba(13),da(2),txt$(3)*80,txt$*50
11400   dim a(7),b(11),c(4),d(15),g(12),rw(22,13),d$*6,dat$*20,bt1(14,2)
11600   dim p$*10,o(2),bt2(14,2),badr(2),dp$*60,tg(11),transkey$*19,meteradr$*30,custname$*30
11800   dim watuse(12),watdat(12),elecuse(12),elecdat(12),gasuse(12),gasdat(12)
12000   dim serviceName$(10)*20,serviceCode$(10)*2,tax_code$(10)*1,work$*80
12200   dim penatly$(10)*1,subjectto(10)
12400   dim extra(23),extra$(11)*30,client$*30
12600   dim cap$*128,work$*80,work_addr$*80
12800 ! ______________________________________________________________________
13200   fntop("S:\acsUB\reset_readings",cap$="Reset Readings")
13400   dim srvnam$(10)*20,srv$(10)*2
13600   fnget_services(mat srvnam$,mat srv$)
14200 ! ______________________________________________________________________
14400 SCREEN1: ! 
14600   fnTos(sn$='resetreadings3')
14800   mylen=22 : mypos=mylen+2
15000   fnLbl(1,1,"Transaction Date (mmddyy):",mylen,1)
15200   fnTxt(1,mypos,8,0,1,"1001") : resp$(1)='' ! '070611'
15400   fnLbl(3,1,"Reading to Reset:",mylen,1)
15600   fnOpt(3,mypos,"Current") : resp$(2)='True'
15800   fnOpt(4,mypos,"Prior") : resp$(3)='False'
16000   fnChk(6,mypos,srvnam$(1),1) : resp$(4)='True'
16200   fnChk(7,mypos,srvnam$(4),1) : resp$(5)='True'
16400   fnChk(9,mypos,"Update Usages",1) : resp$(6)='False'
16600   fnCmdSet(2)
16800   fnAcs(sn$,0,mat resp$,ck)
17000   if ck=5 then goto XIT
17200   d1=fndate_mmddyy_to_ccyymmdd(val(resp$(1)))
17400   if resp$(2)='True' then do_current=1 else do_current=0
17600   if resp$(4)='True' then do_water=1 else do_water=0
17800   if resp$(5)='True' then do_gas=1 else do_gas=0
18000   if resp$(6)='True' then do_usages=1 else do_usages=0
18200   execute "Index [Q]\UBmstr\UBTransVB.h[cno]"&' '&"[Q]\UBmstr\UTV_Date.h[cno] 11 8 Replace DupKeys -n"
18400   open #h_trans=fngethandle: "Name=[Q]\UBmstr\ubtransvb.h[cno],KFName=[Q]\UBmstr\UTV_Date.h[cno],Shr",internal,input,keyed 
18600   open #h_customer:=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,outIn,keyed 
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
21800   fnxit
22000 ! ______________________________________________________________________
22200 ! <Updateable Region: ERTN>
22400 ERTN: fnerror(program$,err,line,act$,"NO")
22600   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
22800   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
23000   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
23200 ERTN_EXEC_ACT: execute act$ : goto ERTN
23400 ! /region
23600 ! ______________________________________________________________________
23800   def fn_reading_fix(&reading_current,&reading_prior,&usage_current,&usage_ytd,reading_new)
24000     rf_reading_prior=reading_prior
24200     rf_reading_cur=reading_current
24400     if do_current then 
24600       rf_reading_cur=reading_new
24800     else 
25000       rf_reading_prior=reading_new
25200     end if 
25400     reading_current=rf_reading_cur
25600     reading_prior=rf_reading_prior
25800     if do_usages then 
26000       usage_ytd-=usage_current
26200       usage_current=max(0,reading_current-reading_prior)
26400       usage_ytd+=usage_current
26600     end if  ! do_usages
26800   fnend  ! fn_reading_fix
