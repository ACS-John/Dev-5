00010 ! formerly S:\acsUB\Analyze
00020 ! ______________________________________________________________________
00030   library 'S:\Core\Library': fntop,fnxit, fnerror,fnopenprn,fncloseprn,fntos,fnget_services,fncombof,fnlbl,fntxt,fnfra,fnacs,fnLastBillingDate,fncmdset
00040 ! Goto XIT
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim rate(11),use_from(11),d(12),a(10),bla3$*55,rt$(32),use_to(11)
00080   dim usagtot(18),ratetot(18),customer(18),resp$(40)*80
00090   dim svce$*11,srvnam$(10)*20,srv$(10)
00100 ! ______________________________________________________________________
00110   fntop(program$)
00130   fnLastBillingDate(bdate)
00140   fnget_services(mat srvnam$,mat srv$)
09000 goto SCR1
20000 SCR1: ! r:
20010   fntos(sn$="Anlyze-1") 
20020   rc=0 : mylen=20 : mypos=mylen+2
20030   fnlbl(1,1,"Last Billing Date:",mylen,1)
20040   fntxt(1,mypos,8,0,0,"1") 
20050   resp$(rc+=1)=str$(bdate)
20060   fnlbl(2,1,"Rate for Analysis:",mylen,1)
20070   fncombof("nerd",2,mypos,55,env$('Q')&"\UBmstr\ubData\RateMst.h"&env$('cno'),1,4,5,50,env$('Q')&"\UBmstr\ubData\RateIdx1.h"&env$('cno'),1,usa) 
20080   usa+=1 
20090   resp$(rc+=1)="" ! just default to the first one
20100   fncmdset(2)
20110   fnacs(sn$,0,mat resp$,ckey)
20120   if ckey=5 then goto XIT 
20130   bdate=val(resp$(1)) 
20140   bla3$=trim$(resp$(2)) 
20150   svce$=resp$(2)(1:4) 
20160   cde=val(resp$(2)(3:4))
20170   for j=1 to udim(srv$)
20180     if uprc$(resp$(2)(1:2))=uprc$(srv$(j)) then svce=j
20190   next j
20200   open #20: "Name="&env$('Q')&"\UBmstr\ubData\RateMst.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubData\RateIdx1.h"&env$('cno')&",Shr",internal,input,keyed 
20210   read #20,using "Form POS 55,32*G 10",key=svce$: mat rt$ ioerr SCR1 
20220   close #20: 
20900 goto SCR2 ! /r
24000 SCR2: ! r:
24010   fntos(sn$:="Anlyze-2b") 
24020   rc=rtc=0 : mylen=20 : mypos=mylen+2
24030   fnlbl(1,1,"Analysis Based On:",mylen,1)
24040   fntxt(1,mypos,55,0,0,"",1) 
24050   resp$(rc+=1)=bla3$
24060   fnlbl(2,1,"Minimum Charge:",mylen,1)
24070   fntxt(2,mypos,9,0,1,"10") 
24080   resp$(rc+=1)=rt$(rtc+=1)
24090   fnlbl(3,1,"Minimum Usage:",mylen,1)
24100   fntxt(3,mypos,9,0,1,"30") 
24110   resp$(rc+=1)=rt$(rtc+=1)
24120   fnfra(4,1,12,45,"Rate Breakdown")
24130   fnlbl(1,5,"Usage",10,2,0,1) 
24140   fnlbl(1,17,"Usage",10,2,0,1) 
24150   fnlbl(1,32,"Charge",15,2,0,1)
24160   fnlbl(2,5,"From",10,2,0,1) 
24170   fnlbl(2,17,"To",10,2,0,1) 
24180   fnlbl(2,32,"Per Unit",15,2,0,1)
24190   for j=1 to 10
24200     txt$=str$(j)&"." : fnlbl(j+2,1,txt$,3,1,0,1)
24210     fntxt(j+2,05,10,0,1,"30",0,mt$,1) 
24220     resp$(rc+=1)=rt$(rtc+=1)
24230     fntxt(j+2,17,10,0,1,"30",0,mt$,1) 
24240     resp$(rc+=1)=rt$(rtc+=1)
24250     fntxt(j+2,34,10,0,1,"46",0,mt$,1) 
24260     resp$(rc+=1)=rt$(rtc+=1)
24270   next j
24280   fncmdset(8)
24290   fnacs(sn$,0,mat resp$,ckey)
24300   if ckey=2 then 
24310     goto SCR1 
24320   else if ckey=5 then 
24330     goto XIT
24340   end if
24350   rate(1)=val(resp$(2)) ! minimum charge
24360   use_to(1)=val(resp$(3)) ! minimum usage
24370   use_from(2)=val(resp$(4))  
24380   use_from(3)=val(resp$(7))  
24390   use_from(4)=val(resp$(10))  
24400   use_from(5)=val(resp$(13))  
24410   use_from(6)=val(resp$(16))  
24420   use_from(7)=val(resp$(19))  
24430   use_from(8)=val(resp$(22))  
24440   use_from(9)=val(resp$(25))  
24450   use_from(10)=val(resp$(28)) 
24460   use_from(11)=val(resp$(31))
24470   rate(2)=val(resp$(6))  
24480   rate(3)=val(resp$(9))  
24490   rate(4)=val(resp$(12)) 
24500   rate(5)=val(resp$(15)) 
24510   rate(6)=val(resp$(18)) 
24520   rate(7)=val(resp$(21)) 
24530   rate(8)=val(resp$(24)) 
24540   rate(9)=val(resp$(27)) 
24550   rate(10)=val(resp$(30))
24560   rate(11)=val(resp$(33))
24570   use_to(2)=val(resp$(5))  
24580   use_to(3)=val(resp$(8))  
24590   use_to(4)=val(resp$(11)) 
24600   use_to(5)=val(resp$(14))  
24610   use_to(6)=val(resp$(17))  
24620   use_to(7)=val(resp$(20))  
24630   use_to(8)=val(resp$(23))  
24640   use_to(9)=val(resp$(26))  
24650   use_to(10)=val(resp$(29)) 
24660   use_to(11)=val(resp$(32))
24900   goto initialize ! /r
26000 initialize: ! r: initialize
26010   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,input,keyed 
26020   fnopenprn
26030   mat customer=(0) 
26040   numbcust=0   
26050   totdol=0     
26060   mat ratetot=(0)  
26070   mat usagtot=(0)  
26080   ds9=0
26900 goto READ_CUSTOMER ! /r
32000 READ_CUSTOMER: ! r:
32010   read #1,using L590: mat a,mat d,f eof L840
32020   L590: form pos 143,7*pd 2,pos 1806,3*n 2,pos 217,12*pd 5,pos 296,pd 4
32030   if f<>bdate then goto READ_CUSTOMER
32040   if a(svce)=0 then goto READ_CUSTOMER
32050   s9=3
32060   if svce<3 then goto L670
32070   s9+=4
32080   if svce=3 then goto L670
32090   s9+=4
32100   L670: !
32110   if a(svce)=cde then goto L690
32120   goto READ_CUSTOMER
34000   L690: !
34010   if d(s9)<=0 then goto READ_CUSTOMER
34020   usagtot(1)=usagtot(1)+min(d(s9),use_to(1)) ! add usage for minimun usages
34030   ratetot(1)=ratetot(1)+rate(1) ! add minimum charges
34040   if d(s9)<=use_to(1) then  ! add mimimum customer user to first array item
34050     customer(1)=customer(1)+1 
34060     goto L810
34070   end if
34080   for k7=2 to 11
34090     if use_from(k7)=0 then goto L790 ! no table
34100     if d(s9)>=use_from(k7) and d(s9)<=use_to(k7) then  ! accumulate usage when usage stops between breaks
34110       usagtot(k7)=usagtot(k7)+max(0,(min(d(s9)-use_from(k7)+1,use_to(k7)-use_from(k7)))) 
34120       goto L770
34130     end if
34140     if d(s9)>use_to(k7) then  ! accumulate                 useage between breaks
34150       usagtot(k7)=usagtot(k7)+use_to(k7)-use_from(k7)+1
34160     end if
34170     L770: !
34180     ds9=ds9+usagtot(k7)
34190   next k7
34200   L790: ! 
34210   ds9+=usagtot(1)
34220   customer(k7-1)+=1 ! add customers by rate break
34230   L810: !
34240   numbcust+=1
34250 goto READ_CUSTOMER ! /r
44000 L840: ! r:
44010   for k5=2 to 11 
44020     ratetot(k5)=usagtot(k5)*rate(k5) 
44030     totdol=totdol+ratetot(k5) 
44040   next k5
44050   totdol=totdol+ratetot(1) ! add minimum bills to total dollars
44060   if cde=0 then goto L960
44070   pr #255: "\qc {\b Utility Billing  - Rate Analysis}" 
44080   pr #255: "Analysis for Rate Code: "&bla3$ 
44090   pr #255: "\ql "
44100   pr #255: "{\ul    Usage From} {\ul      Rate} {\ul Dollars Gnrtd.} {\ul Num.Customers}"
44110   for k1=1 to 11
44120     if use_from(k1)=0 and rate(k1)=0 then goto L930
44130     pr #255,using L920: use_from(k1),rate(k1),ratetot(k1),customer(k1)
44140     L920: form pos 1,pic(z,zzz,zzz,zz#),x 1,pic(zz#.#####),x 1,pic(zzz,zzz,zzz.##),x 1,pic(zzzzzzzzzzzzz)
44150     L930: !
44160   next k1
44170   pr #255: "\qc {\ul             Totals            }"
44180   pr #255: "\ql "
44190   L960: !
44200   pr #255: "              Total Customers: "&cnvrt$("PIC(ZZZ,ZZZ,ZZZ)",numbcust)
44210   pr #255: "                Total Dollars: "&cnvrt$("PIC(zzZZ,ZZZ.ZZ)",totdol)
44600 goto DONE ! /r
48000 DONE: ! r:
48010   close #1: ioerr ignore
48020   fncloseprn
48030 goto SCR1 ! /r
52000 XIT: fnxit
54000 ! <Updateable Region: ERTN>
54010 ERTN: fnerror(program$,err,line,act$,"xit")
54020   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
54030   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
54040   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
54050 ERTN_EXEC_ACT: execute act$ : goto ERTN
54060 ! /region
