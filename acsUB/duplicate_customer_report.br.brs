20000 ! Replace S:\acsUB\duplicate_customer_report
20200 ! ______________________________________________________________________
20400   library 'S:\Core\Library': fnacs,fnwait, fnopenprn,fncloseprn,fnerror,fncno,fndat,fnlbl,fntxt,fnchk,fntos,fnxit,fncmdset,fntop
20600   on error goto ERTN
20800 ! ______________________________________________________________________
21000   dim z$*10,e$(4)*30,cnam$*40,dat$*20,idx$(5)*40
21200   dim item1$(6)*22,cap$*128
21400   dim a2(10),a1(10),a(10),tt$*200,ab$(4)*30,extra$(11)*30
21600 ! ______________________________________________________________________
21800   fntop(program$,cap$="Duplicate Customer List")
22000   fncno(cno,cnam$)
22200   fndat(dat$,1)
23600   on fkey 5 goto DONE
23800   fnopenprn
24000   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",Shr",internal,input,relative 
24200   open #2: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,input,keyed 
24400   fn_header
24600   do 
24800     read #1,using FORM_CUSTOMER: z$,mat e$,mat a,final,bal,route,sequence,extra$(1) eof DONE
25000 FORM_CUSTOMER: form pos 1,c 10,pos 11,4*c 30,pos 143,5*pd 2,pos 1806,3*n 2,pos 153,2*pd 2,pos 1821,n 1,pos 292,pd 4.2,pos 1741,n 2,n 7,pos 1864,c 30
25200     if fn_has_dupe(z$) then 
25400       pr #255,using FORM_OUT: rec(1),z$,e$(2),e$(1),bal,mat a2 pageoflow PGOF
25600 FORM_OUT: form n 4,x 1,c 10,x 5,c 30,x 7,c 30,n 11.2,x 1,10*nz 3
25800     end if  ! fn_has_dupe(z$)
26000   loop 
26200 DONE: ! 
26400   fncloseprn
26600 XIT: ! 
26800   fnxit
27000 ! ______________________________________________________________________
27200 PGOF: ! 
27400   pr #255: newpage
27600   fn_header
27800   continue 
28000 ! ______________________________________________________________________
28200   def fn_has_dupe(z$)
28400     hd_return=0
28600     let z_one$=z_two$=''
28700     restore #2: 
28800     read #2,using FORM_CUSTOMER,key=z$: z_one$
29000     read #2,using FORM_CUSTOMER: z_two$ eof HD_EOF
29200 HD_EOF: ! 
29400     if z_one$=z_two$ then hd_return=1
29600     fn_has_dupe=hd_return
29800   fnend  ! fn_has_dupe
30000   def fn_header
30200     pr #255: "\qc {\b "&env$('cnam')&"}"
30400     pr #255: "\qc {\fs28 {\b Duplicate Customer List}}"
30600     pr #255: "\qc {\b "&trim$(item1$(1))&" Order}"
30800     pr #255,using 'form pos 21,cc 40,pos 71,c 5,pic(zzz)': dat$,"Page ",p2+=1
31000     pr #255: ""
31200     pr #255: "\ql {\ul     Customer No}     {\ul Name                             }    {\ul Meter Address                }   {\ul   Balance} {\ul}"
31400   fnend 
50970 ! ______________________________________________________________________
50980 ! <Updateable Region: ERTN>
50990 ERTN: fnerror(program$,err,line,act$,"xit")
51000   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
51010   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
51020   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
51030 ERTN_EXEC_ACT: execute act$ : goto ERTN
51040 ! /region
51050 ! ______________________________________________________________________
