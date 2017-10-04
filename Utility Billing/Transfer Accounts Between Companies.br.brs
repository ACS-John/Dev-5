00010 ! formerly S:\acsUB\ubCoTr
00020 ! -- Transfer Accounts Between Companies
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnacs,fnlbl,fntxt,fntos,fnopenprn,fncloseprn,fnerror,fncmbact,fnxit,fncmdset,fntop,fncmbcno,fnindex_sys
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim z$*10,e$(4)*30,f$(3)*12,a(7),b(11),c(4),d(15),g(12),ta(2),alp$*7
00080   dim ubstd$*200,ubextra$*100,mstrform$*300,extra(23),extra$(11)*30
00090   dim rw4(22,13),ab$(4)*30,cap$*128
00100   dim df$*256
00110   dim rm$*60,rm$(20)*60,ra(2),resp$(10)*50,tg(11)
00120 ! ______________________________________________________________________
20000   cno=val(env$('cno'))
20040   fntop(program$)
20060   let ubstd$="Form Pos 1,C 10,4*C 30,C 12,7*PD 2,11*PD 4.2,4*PD 4,15*PD 5,PD 4.2,PD 4,12*PD 4.2,2*PD 3,C 7,2*C 12,PD 3,10*PD 5.2,78*PD 5,13*PD 4.2,13*N 6,156*PD 4.2,13*N 6,13*PD 4.2,C 1,C 9,C 2,C 17"
20080   let ubextra$=",n 2,n 7,n 6,n 6,n 9,pd 5.2,n 3,3*n 9,3*n 2,3*n 3,n 1,3*n 9,3*pd 5.2,c 30,7*c 12,3*c 30"
20100   let mstrform$=rtrm$(ubstd$)&rtrm$(ubextra$)
30000 MENU1: ! 
30020   fntos(sn$="CoTr-1")
30040   let mylen=5 : let mypos=mylen+2
30060   fnlbl(1,1,"From:",mylen,1)
30080   fntxt(1,mypos+.4,50, 0,0,'',1) ! let fncmbcno(1,mypos)
30100   let resp$(1)=env$('cnam')&' ('&str$(cno)&')'
30120   fnlbl(2,1,"To:",mylen,1)
30140   fncmbcno(2,mypos)
30160   let resp$(2)=''
30180   fnlbl(4,10,"(Both companies must be set up in advance)",49,0)
30200   fncmdset(2)
30220   fnacs(sn$,0,mat resp$,ck)
32000   if ck=5 then goto XIT
32040   co1=cno ! val(resp$(1)(43:47))
32060   co2=val(resp$(2)(43:47))
32080   if co1=0 or co2=0 then goto MENU1
32100   close #1: ioerr ignore
32120   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(co1)&",Shr,KFName="&env$('Q')&"\UBmstr\UBIndex.h"&str$(co1)&",Shr",internal,outin,keyed  ! Ioerr MENU1
32140   close #2: ioerr ignore
32160   open #2: "Name="&env$('Q')&"\UBmstr\ubTransVB.h"&str$(co1)&",Shr,KFName="&env$('Q')&"\UBmstr\ubTrIndx.h"&str$(co1)&",Shr",internal,outin,keyed 
32180   close #3: ioerr ignore
32200   open #3: "Name="&env$('Q')&"\UBmstr\UBADRBIL.h"&str$(co1)&",Shr,KFName="&env$('Q')&"\UBmstr\AdrIndex.h"&str$(co1)&",Shr",internal,outin,keyed 
32300   close #41: ioerr ignore
32320   open #41: "Name="&env$('Q')&"\UBmstr\DEPOSIT1.h"&str$(co1)&",Shr,KFName="&env$('Q')&"\UBmstr\DEPIDX1.h"&str$(co1)&",Shr,USE,RecL=16,KPS=1,KLN=10",internal,outin,keyed 
32340   close #42: ioerr ignore
32360   open #42: 'Name='&env$('Q')&'\UBmstr\Deposit2.h'&env$('cno')&',KFName='&env$('Q')&'\UBmstr\Deposit2Index.h'&env$('cno')&',Shr,Use,RecL=73,KPs=1,KLn=10',internal,outin,keyed ! "Name="&env$('Q')&"\UBmstr\DEPOSIT2.h"&str$(co1)&",Shr,USE,RecL=73",internal,outin,relative 
32380   close #26: ioerr ignore
32400   open #26: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(co2)&",Shr,KFName="&env$('Q')&"\UBmstr\UBIndex.h"&str$(co2)&",Shr",internal,outin,keyed  ! Ioerr MENU1
32420   close #27: ioerr ignore
32440   close #28: ioerr ignore
32460   open #11: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(co2)&",Shr,KFName="&env$('Q')&"\UBmstr\UBIndx2.h"&str$(co2)&",Shr",internal,outin,keyed 
32480   open #12: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(co2)&",Shr,KFName="&env$('Q')&"\UBmstr\UBIndx3.h"&str$(co2)&",Shr",internal,outin,keyed 
32500   close #14: ioerr ignore
32520   open #14: "Name="&env$('Q')&"\UBmstr\ubTransVB.h"&str$(co2)&",Shr,KFName="&env$('Q')&"\UBmstr\ubTrIndx.h"&str$(co2)&",Shr",internal,outin,keyed 
32540   close #23: ioerr ignore
32560   open #23: "Name="&env$('Q')&"\UBmstr\UBADRBIL.h"&str$(co2)&",Shr,KFName="&env$('Q')&"\UBmstr\AdrIndex.h"&str$(co2)&",Shr",internal,outin,keyed  ! Ioerr MENU1
32660   close #51: ioerr ignore
32680   open #51: "Name="&env$('Q')&"\UBmstr\Deposit1.h"&str$(co2)&",Shr,KFName="&env$('Q')&"\UBmstr\DepIdx1.h"&str$(co2)&",Shr,Use,RecL=16,KPs=1,KLn=10",internal,outin,keyed ioerr MENU1
32700   close #52: ioerr ignore
32720   open #52: "Name="&env$('Q')&"\UBmstr\Deposit2.h"&str$(co2)&",Shr,USE,RecL=73",internal,outin,relative 
32740   fnopenprn
32760   gosub HDR
34000 MENU2: ! 
34020   let hcno=cno
34040 L700: let fntos(sn$="CoTr-2")
34060   fnlbl(1,1,"Customer to Transfer:",28,1)
34080   fncmbact(1,30)
34100   let resp$(1)=""
34120   fncmdset(2)
34140   fnacs(sn$,0,mat resp$,ck)
34200   if ck=5 then goto DONE
34220   let z$=lpad$(trim$(resp$(1)(1:10)),10)
34240   read #1,using mstrform$,key=z$: z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,f,mat g,mat ta,alp$,f$(2),f$(3),bra,mat gb,mat rw4,df$,dr$,dc$,da$,mat extra,mat extra$ nokey L700
34280   let z2$=z$
34300 L820: read #26,using "Form POS 1,C 10",key=z2$: z2$ nokey L960
36000 MENU3: ! 
36020   sn$="CoTr-3"
36040   fntos(sn$)
36060   let mylen=28
36080   let mypos=mylen+2
36100   fnlbl(3,1,"New Account:",mylen,1)
36120   fntxt(3,30,10)
36140   let resp$(1)=z2$
36180   fnlbl(1,1,"Account "&z2$&" already exists!",0,2)
36200   fncmdset(2)
36220   fnacs(sn$,0,mat resp$,ck)
36240   if ck=5 then goto MENU2
36260   let z2=val(resp$(1)) conv MENU3
36280   if z2=0 then goto MENU2
36300   let z2$=cnvrt$("N 10.2",z2)
36320   goto L820
36340 ! ______________________________________________________________________
38000 L960: restore #2,key>=z$&"         ": nokey L1040
38020 L970: read #2,using L980: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof L1040
38040 L980: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
38060   if p$<>z$ then goto L1040
38080   write #14,using L980: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode
38100   delete #2: 
38120   goto L970
38140 ! ______________________________________________________________________
40000 L1040: gosub ALTBILLADDR
40020   write #26,using mstrform$: z2$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,f,mat g,mat ta,alp$,f$(2),f$(3),bra,mat gb,mat rw4,df$,dr$,dc$,da$,mat extra,mat extra$
40040   gosub PRNT1
40060   delete #1,key=z$: 
40080   goto MENU2
40100 ! ______________________________________________________________________
42000   close #1: 
42020   close #2: 
42040   close #3: 
42060   close #26: 
42080   close #14: 
42100   close #23: 
46000 FINIS: ! r:
46020   ! close #31: 
46040   ! close #61: 
46060   ! close #32: 
46080   fncloseprn
46100   goto DONE
46120 ! /r
48000 ALTBILLADDR: ! r: alternate billing address
48020   read #3,using "Form POS 1,C 10,4*C 30",key=z$: z$,mat ab$ nokey L1440
48040   write #23,using "Form POS 1,C 10,4*C 30": z2$,mat ab$
48060 L1440: return  ! /r
50000 DONE: ! r:
50020   close #1: ioerr ignore
50040   close #2: ioerr ignore
50060   close #3: ioerr ignore
50080   ! close #31: ioerr ignore
50100   close #41: ioerr ignore
50120   close #42: ioerr ignore
50140   close #26: ioerr ignore
50160   close #11: ioerr ignore
50180   close #12: ioerr ignore
50200   close #14: ioerr ignore
50220   close #23: ioerr ignore
50280   close #51: ioerr ignore
50300   close #52: ioerr ignore
50320   fnindex_sys(co1)
50340   fnindex_sys(co2)
61770 XIT: let fnxit ! /r
61790 HDR: ! r:
61800   pr #255,using "Form POS 1,Cc 80": "Accounts Transferred from Company Number "&str$(co1)&" to Company Number "&str$(co2)
61810   pr #255,using "Form POS 5,CC 70": date$
61812   pr #255: ""
61820   pr #255,using "Form POS 2,C 9,POS 15,C 4,POS 53,C 7": "Act. Num.","Name","Balance"
61830 return  ! /r
61850 PRNT1: ! r: pr TRANSFERS
61860   pr #255,using 'form pos 1,c 10,pos 15,c 30,pos 50,n 10.2': z$,e$(2),bal pageoflow PGOF
61880 return  ! /r
61890 IGNORE: continue 
61900 PGOF: ! r:
61910   pr #255: newpage
61920   gosub HDR
61930 continue  ! /r
71950 ! <Updateable Region: ERTN>
71960 ERTN: let fnerror(program$,err,line,act$,"xit")
71970   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
71980   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
71990   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
72000 ERTN_EXEC_ACT: execute act$ : goto ERTN
72010 ! /region
