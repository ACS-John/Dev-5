10000 ! Replace S:\acsUB\swap_cur_prior
10200 ! -- for all billing dates that match - put the lower of prior/current water readings into prior and the higher into current
10400 ! ______________________________________________________________________
10600   library 'S:\Core\Library': fndate_mmddyy_to_ccyymmdd,fnget_services,fnLastBillingDate,fnxit,fnerror,fnTos,fnLbl,fnAcs,fnTxt,fnwait,fnmsgbox,fnCmdSet,fntop,fngethandle
10800   on error goto ERTN
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
13200   fntop(program$,cap$="Swap Current and Prior Readings for Water")
13400   fnLastBillingDate(d1)
13600   fnget_services(mat serviceName$,mat service$,mat tax_code$,mat penalty$,mat subjectto)
14200   for j=1 to udim(serviceName$)
14400     serviceName$(j)=trim$(serviceName$(j))
14600   next j
14800 ! ______________________________________________________________________
15000   fnTos(sn$='SwapCurPri1')
15200   mylen=22 : mypos=mylen+2
15400   fnLbl(1,1,"Billing Date (mmddyy):",mylen,1)
15600   fnTxt(1,mypos,8,0,1,"1001")
15800   resp$(1)=str$(d1)
16000 L440: ! 
16200   fnCmdSet(2)
16400   fnAcs(sn$,0,mat resp$,ck)
16600   if ck=5 then goto XIT
16800   d1=val(resp$(1))
17000   open #customer:=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,outIn,keyed 
17200 F_CUSTOMER: form pos 11,2*c 30,pos 143,7*pd 2,pos 157,11*pd 4.2,pos 201,4*pd 4,pos 217,15*pd 5,pos 292,pd 4.2,pos 296,pd 4,pos 300,12*pd 4.2,pos 388,10*pd 5.2,pos 1741,n 2,n 7,2*n 6,n 9,pd 5.2,n 3,3*n 9,3*n 2,3*n 3,n 1,3*n 9,3*pd 5.2,c 30,7*c 12,3*c 30
17400 ! ______________________________________________________________________
17600   do 
17800     read #customer,using F_CUSTOMER: meteradr$,custname$,mat a,mat b,mat c,mat d, bal,f,mat g,mat gb,mat extra eof XIT
18000     if f=d1 then ! else recalculation reduce balances
18200       water_reading_prior=min(d(1),d(2))
18400       water_reading_cur=max(d(1),d(2))
18600       d(1)=water_reading_cur
18800       d(2)=water_reading_prior
18900       rewrite #customer,using F_CUSTOMER: meteradr$,custname$,mat a,mat b,mat c,mat d,bal,f,mat g,mat gb,mat extra
19000     end if  ! f=d1
19400   loop 
19600 XIT: ! 
19800   fnxit
20000 ! ______________________________________________________________________
20200 ! <Updateable Region: ERTN>
20400 ERTN: fnerror(program$,err,line,act$,"NO")
20600   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
20800   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
21000   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
21200 ERTN_EXEC_ACT: execute act$ : goto ERTN
21400 ! /region
21600 ! ______________________________________________________________________
