00010 ! modifies customer records depending upon how the program is configured, routes services, etc
00030 ! r: setup library, dims, on err, fntop, etc
00040   library 'S:\Core\Library': fntop,fnxit, fnacs,fnlbl,fntxt,fnwait,fntos,fncno,fnxit,fnerror,fncmdset,fntop,fnopenprn,fncloseprn,fnub_index_customer,fngethandle
00050   on errror goto ERTN
00110   let fntop(program$)
00120  ! /r
00130   ! gosub OldWorkFromFixedWidthList
12000   ! r: primary loop setup
12020   dim z$*10,e$(4)*30,f$(3)*12,a(7),b(11),c(4),d(15),g(12),adr(2),alp$*7,gb(10),extra$(11)*30
12040   dim extra(23)
12042   dim df$*1
12060   open #h_customer:=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno'),internal,outin,relative 
12080     F_CUSTOMER: form pos 1,c 10,4*c 30,c 12,7*pd 2,11*pd 4.2,4*pd 4,15*pd 5,pd 4.2,pd 4,12*pd 4.2,2*pd 3,c 7,2*c 12,pd 3,10*pd 5.2,pos 1712,c 1,c 9,c 2,c 17,n 2,n 7,2*n 6,n 9,pd 5.2,n 3,3*n 9,3*n 2,3*n 3,n 1,3*n 9,3*pd 5.2,c 30,7*c 12,3*c 30
12100   ! /r
14000   do ! r: primary loop
14020     read_count+=1
14040     read #h_customer,using F_CUSTOMER: z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,f,mat g,mat adr,alp$,f$(2),f$(3),bra,mat gb,df$,dr$,dc$,da$,mat extra,mat extra$ eof PrimaryFinis
14060     didChange=0
14080     didChange+=fnDivideRoutesByTen(extra(1))
14100     didChange+=fnServicesRateCodeAdjust
14120     if didChange then
14140       rewrite #h_customer,using F_CUSTOMER: z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,f,mat g,mat adr,alp$,f$(2),f$(3),bra,mat gb,df$,dr$,dc$,da$,mat extra,mat extra$
14160       let write_count+=1
14180     end if
14200   loop ! /r
16000   PrimaryFinis: !
16020     pr 'read_count=';read_count
16040     pr 'write_count=';write_count : pause
18000   xit: fnxit
28000 ! <Updateable Region: ERTN>
28060 ERTN: let fnerror(program$,err,line,act$,"xit")
28080   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
28100   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
28120   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
28140 ERTN_EXEC_ACT: execute act$ : goto ERTN
28160 ! /region
32000 OldWorkFromFixedWidthList: ! r: change route and sequence numbers from a text file
32020   dim ln$*128
32040   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,outin,keyed 
32060   open #2: "Name=chg\newrouteseq.txt",display,input 
32080   fnopenprn
32100   READ_CUSTOMER: ! 
32120   linput #2: ln$ eof OWFFL_Finis
32140   ! Let Z$=LPAD$(RTRM$(LN$(17:26)),10)
32160   let z$=lpad$(rtrm$(ln$(1:9)),10)
32180   ! Let ROUTE=VAL(LN$(1:7))
32200   let route=val(ln$(81:87))
32220   ! Let SEQUENCE=VAL(LN$(9:15))
32240   let sequence=val(ln$(73:79))
32260   read #1,using "Form POS 1,c 10,pos 1741,n 2,pos 1743,n 7",key=z$: oldz$,oldroute,oldsequence nokey L250
32280   rewrite #1,using "Form pos 1741,n 2,pos 1743,n 7": route,sequence
32300   goto READ_CUSTOMER
32320   ! 
32340   L250: !
32360   pr #255,using "form pos 1,c 50": "Account "&z$&" not found"
32380   goto READ_CUSTOMER
32400   OWFFL_Finis: !
32420   let fncloseprn
32440   close #1: 
32460   fnub_index_customer ! execute "Index "&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&' '&env$('Q')&"\UBmstr\UBIndx5.h"&env$('cno')&" 1741/1743 2/7 Replace DupKeys -n"
32480 return ! /r
34000 def fnDivideRoutesByTen(&routeNumber)
34020   drbtReturn=0
34040   if len(str$(routeNumber))<>2 then
34060     pr bell;'route number ('&str$(routeNumber)&') wrong length to convert.'
34080     pause
34100   else if str$(routeNumber)(2:2)<>'0' then
34120     pr bell;'route number ('&str$(routeNumber)&') does not end with a zero.'
34140     pause
34160   else
34180     routeNumber=routeNumber/10
34200     drbtReturn=1
34220   end if
34240   fnDivideRoutesByTen=drbtReturn
34260 fnend
38000 def fnServicesRateCodeAdjust
38020   srcaReturn=0
38040   ! r: Sanitation Standard Charge - they should all be ZERO
38060   !   b(5)=0
38080   !   srcaReturn=1
38100   ! /r
38120   ! r: super dynamic - just change the code LOL
38140     ! if a(4)=2 then ! service 4 (gas) has a rate code of 2 then
38160     ! if a(2)>0 then ! if Serive 2 has a rate code
38180     if a(1)>0 then ! if water (service 1) rate code (any)
38200       ! a(3)=1     ! Service 3 (Electric or Lawn Meter) – Rate Code
38220       extra(11)=1 ! service 6 (fire protection) gets a rate code of 1
38240       ! a(6)=2     ! service 9 (sales tax) gets a rate code of 2
38260     else
38280       extra(11)=9
38300     end if
38320     srcaReturn=1 ! tells the calling routine to write the record
38340   ! /r
38360   fnServicesRateCodeAdjust=srcaReturn
38380 fnend
      