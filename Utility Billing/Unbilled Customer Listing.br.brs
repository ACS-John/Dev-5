00010 ! formerly S:\acsUB\ubUnBill
00020 ! r: setup 
00030   library 'S:\Core\Library': fnacs,fnlbl,fntxt,fncmbrt2,fntos,fnerror,fnopenprn,fncloseprn,fnxit,fnd1,fncmdset,fntop,fnchk
00040   on error goto ERTN
00050   ! 
00060   dim z$*10,e$(4)*30,resp$(10)*40
00070   ! 
00090   fnd1(d1)
00110   fntop(program$)
00120 goto MAIN ! /r
28000 MAIN: ! r: main screen
28020   fntos(sn$:="UBUnBill") 
28040   mylen=20 
28060   mypos=mylen+2
28080   fnlbl(2,1,"Billing Date:" ,mylen,1)
28100   fntxt(2,mypos,8,8,0,"1") 
28120   resp$(1)=str$(d1)
28140   fnlbl(3,1,"Route Number:" ,mylen,1)
28160   fncmbrt2(3,mypos) 
28180   resp$(2)="[All]"
28200   fnchk(4,23,"Print Meter Address:",1)
28220   resp$(3)="True"
28240   fncmdset(3)
28260   fnacs(sn$,0,mat resp$,ck)
28280   if ck=5 then goto XIT
28300 goto Initialize ! /r
32000 Initialize: ! r:
32020   d1=val(resp$(1))
32040   if resp$(2)="[All]" then prtbkno=0 else prtbkno = val(resp$(2))
32060   if resp$(3)="True" then printadr=1 ! wants meter address printed
32080   if d1<10100 or d1>123199 then goto MAIN
32100   fnopenprn
32120   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndx5.h"&env$('cno')&",Shr",internal,input,keyed 
32140   gosub HDR
32160   if prtbkno=0 then goto READ_CUSTOMER
32180   prtbkno$=lpad$(str$(prtbkno),2)&"       "
32200   startcd=1
32220   restore #1,key>=prtbkno$: nokey TOTALS
32240 goto READ_CUSTOMER ! /r
34000 READ_CUSTOMER: ! r: main loop
34020   read #1,using L420: z$,mat e$,final,bal,f,route eof TOTALS
34040   L420: form pos 1,c 10,4*c 30,pos 1821,n 1,pos 292,pd 4.2,pd 4,pos 1741,n 2
34060   if f=d1 then goto READ_CUSTOMER
34080   if final=1 or final=2 then goto READ_CUSTOMER           ! Skip if InActive
34100   if startcd=1 and prtbkno<>route then goto TOTALS
34120   if final=3 then final$="Final=3" else final$=""
34140   if final=4 then final$="Final=4"
34160   if printadr=1 then pr #255,using L490: z$,e$(2),f,bal,e$(1)(1:25),final$ pageoflow PGOF else pr #255,using L491: z$,e$(2),f,bal,final$ pageoflow PGOF
34180   L490: form pos 1,c 10,pos 13,c 30,pos 45,pic(zz/zz/zz),n 15.2,x 2,c 25,x 2,c 8
34200   L491: form pos 1,c 10,pos 13,c 30,pos 45,pic(zz/zz/zz),n 15.2,x 2,c 8
34220   tbal=tbal+bal
34240 goto READ_CUSTOMER ! /r
38000 TOTALS: ! r:
38020   pr #255: rpt$(" ",55)&"{\ul             }" 
38040   pr #255,using "Form POS 56,N 12.2": tbal 
38060   pr #255: rpt$(" ",55)&"{\ul \strike             }"
38080 goto DONE ! /r
44000 HDR: ! r:
44020   p2=p2+1
44040   pr #255: "\qc  {\f181 \fs18 \b "&env$('cnam')&"}"
44060   pr #255: "\qc  {\f181 \fs24 \b "&env$('program_caption')&"}"
44080   pr #255: "\qc  {\f181 \fs16 \b "&date$("Month DD, CCYY")&"}"
44100   pr #255,using L590: "\ql "&date$,"Page "&str$(p2)
44120   L590: form pos 1,c 82,c 10
44140   pr #255: ""
44160   ! pr #255: "{\ul Account    }                               {\ul Date of}         Current"
44180   if printadr<>1 then pr #255: " {\ul Number   }  {\ul Name             }             {\ul Last Billing}      {\ul Balance}"
44200   if printadr=1 then pr #255: " {\ul Number   }  {\ul Name             }             {\ul Last Billing}      {\ul Balance}  {\ul Meter Address}"
44220   pr #255: ""
44240 return ! /r
46000 PGOF: ! r:
46020   pr #255: newpage 
46040   gosub HDR 
46060 continue ! /r
54000 DONE: ! r:
54020   close #1: ioerr ignore
54040   fncloseprn
54060 goto XIT ! /r
58000 XIT: fnxit
62000 ! <Updateable Region: ERTN>
62020 ERTN: fnerror(program$,err,line,act$,"xit")
62040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
62060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
62080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
62100 ERTN_EXEC_ACT: execute act$ : goto ERTN
62120 ! /region
