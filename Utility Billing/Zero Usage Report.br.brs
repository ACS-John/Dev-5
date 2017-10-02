00010 ! formerly S:\acsUB\ubNoUsage
00020 ! ______________________________________________________________________
16000   library 'S:\Core\Library': fnacs,fnlbl,fntxt,fncmbrt2,fntos,fnerror,fnopenprn,fncloseprn,fnxit,fnd1,fncmdset,fntop,fnchk,fnget_services
16020   on error goto ERTN
16040 ! ______________________________________________________________________
16060   dim z$*10,e$(4)*30,resp$(10)*40,d(15)
16080   dim servicename$(10)*20,a(7)
16100 ! ______________________________________________________________________
16120   let fntop(program$)
16140   let fnd1(d1)
16160   fnget_services(mat servicename$)
24000 MAIN: ! 
24040   let fntos(sn$:="UBNOUsage")
24060   let mylen=20
24080   let mypos=mylen+2
24120   let fnlbl(2,1,"Billing Date:",mylen,1)
24140   let fntxt(2,mypos,8,8,0,"1")
24160   let resp$(1)=str$(d1)
24200   let fnlbl(3,1,"Route Number:",mylen,1)
24220   let fncmbrt2(3,mypos)
24240   let resp$(2)="[All]"
24260   let fnchk(4,23,"Print Meter Address:",1)
24280   let resp$(3)="True"
24300   let fncmdset(3)
24320   let fnacs(sn$,0,mat resp$,ck)
28000   if ck=5 then goto XIT
28020   let d1 = val(resp$(1))
28040   if resp$(2)="[All]" then 
28060     let prtbkno=0 
28080   else 
28100     let prtbkno=val(resp$(2))
28120   end if
28140   if resp$(3)="True" then let printadr=1 ! wants meter address printed
28160   if d1<10100 or d1>123199 then goto MAIN
36000   let fnopenprn
36020   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndx5.h"&env$('cno')&",Shr",internal,input,keyed 
36040   gosub HDR
36060   if prtbkno=0 then goto READ_CUSTOMER
36080   let prtbkno$=lpad$(str$(prtbkno),2)&"       "
36100   let startcd=1
36120   restore #1,key>=prtbkno$: nokey TOTALS
36140   goto READ_CUSTOMER
36160 ! ______________________________________________________________________
42000 READ_CUSTOMER: ! 
42020   read #1,using L450: z$,mat e$,final,bal,f,route,mat d,mat a eof TOTALS
42040   L450: form pos 1,c 10,4*c 30,pos 1821,n 1,pos 292,pd 4.2,pd 4,pos 1741,n 2,pos 217,15*pd 5,pos 143,7*pd 2
42060   if f<>d1 then goto READ_CUSTOMER
42080   if a(1)>0 and trim$(servicename$(1))="Water" and d(3)=0 then ! if have water and zero usage then list
42100     currentread=d(1)
42120     let priorread=d(2)
42140   else if a(3)>0 and trim$(servicename$(3))="Electric" and d(7)=0 then  ! if have electric and zero usage then list
42160     currentread=d(5)
42180     let priorread=d(6)
42200   else if a(4)>0 and trim$(servicename$(4))="Gas" and d(11)=0 then ! if have gas and zero usage then list
42220     currentread=d(9)
42240     let priorread=d(10)
42260   else
42280     goto READ_CUSTOMER
42300   end if
42320   if startcd=1 and prtbkno<>route then goto TOTALS
42380   if printadr=1 then 
42400     pr #255,using L550: z$,e$(2),currentread,priorread,e$(1)(1:25) pageoflow PGOF 
42420     L550: form pos 1,c 10,pos 13,c 30,pos 43,n 13,x 4,n 13,x 3,c 25
42440   else 
42460     pr #255,using L550: z$,e$(2),currentread,priorread pageoflow PGOF
42480   end if
42500   let tbal=tbal+bal
42520   goto READ_CUSTOMER
42540 ! ______________________________________________________________________
48000 HDR: ! r:
48020   let p2=p2+1
48040   pr #255: "\qc  {\f181 \fs18 \b "&env$('cnam')&"}"
48060   pr #255: "\qc  {\f181 \fs24 \b "&env$('program_caption')&"}"
48080   pr #255: "\qc  {\f181 \fs16 \b "&date$("Month DD, CCYY")&"}"
48100   pr #255,using L650: "\ql "&date$,"Page "&str$(p2)
48120   L650: form pos 1,c 82,c 10
48140   pr #255: ""
48160   if printadr<>1 then pr #255: " {\ul Number   }  {\ul Name             }          {\ul Current Reading}    {\ul Prior Reading}"
48180   if printadr=1 then pr #255: " {\ul Number   }  {\ul Name             }           {\ul Current Reading}    {\ul Prior Reading}  {\ul Meter Address}"
48200   pr #255: ""
48220 return ! /r
52000 PGOF: ! r:
52020   pr #255: newpage 
52040   gosub HDR 
52060 continue ! /r
56000 TOTALS: ! r:
56020 ! pr #255: RPT$(" ",55)&"{\ul             }" 
56040   ! pr #255,Using "Form POS 56,N 12.2": TBAL 
56060   ! pr #255: RPT$(" ",55)&"{\ul \strike             }"
56080 goto DONE ! /r
58000 DONE: ! r:
58020   close #1: ioerr ignore
58040   let fncloseprn
58060 goto XIT ! /r
61000 XIT: let fnxit
63000 ! <Updateable Region: ERTN>
63020 ERTN: let fnerror(program$,err,line,act$,"xit")
63040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
63060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
63080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
63100 ERTN_EXEC_ACT: execute act$ : goto ERTN
63120 ! /region
