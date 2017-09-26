00020 ! ______________________________________________________________________
00030   library 'S:\Core\Library': fncno,fnerror,fnwait,fnopenprn,fncloseprn,fnacs,fnflexadd1,fnflexinit1,fntos,fncustomer_search,fnlbl,fntxt,fnopt,fnmsgbox,fncomboa,fnbutton,fnpic,fnfra,fnchk,fndat,fncmbact,fncombof,fncmbrt2,fnxit,fnd1,fncmdset,fncmdkey,fntop,fndate_mmddyy_to_ccyymmdd,fnpause,fngethandle
00040   library 'S:\Core\Library': fnd1
00050   on error goto ERTN
00060 ! ______________________________________________________________________
24000   dim rate(18,20),usage(18,20),cde(20),d(12),t(18,2),a(4),message$*40
24020   dim usagtot(18,20),ratetot(18,20),customer(18,20),cap$*128
24040   dim fa$(5),sa$(4),fb$(1),fc$(1),sb$(1)*38,fd$(1),z$(4)*11,srvc$*11
24060   dim cnam$*40,a(7),d(15),g(10),e$(4)*30,f$(3)*12
24080   dim code$(4)
24100   let code$(1)="Water"
24120   let code$(2)="Sewer"
24140   let code$(3)="Electric"
24160   let code$(4)="Gas"
24180 ! 
38000   let fncno(cno,cnam$)
38020   let fnd1(bdate)
38040   let fntop("S:\acsUB\Consumptionlist",cap$="Consumption List")
44000 MAIN: ! 
44020   let fntos(sn$:="UBAnalyze")
44040   let mylen=20
44060   let mypos=mylen+2
44080   let fnlbl(1,1,"Billing Date:",mylen,1)
44100   let fntxt(1,mypos,8,8,0,"1")
44120   let resp$(1)=str$(bdate)
44140   let fnlbl(2,1,"Type of Service:",mylen,1)
44160   let fncomboa("Service",2,mylen+3,mat code$,"",16)
44180   let fnlbl(3,1,"Rate Code",mylen,1)
44200   let fntxt(3,mypos,3,3,0,"1030")
44220   let resp$(3)=""
44240   let fncmdset(3)
44260   let fnacs(sn$,0,mat resp$,ck)
48000   if ck=5 then goto XIT
48020   let bdate= val(resp$(1))
48040   if resp$(2)="Water" then 
48060     let srvc=1 : let srvc$=resp$(2)
48080   else if resp$(2)="Sewer" then 
48100     let srvc=2 : let srvc$=resp$(2)
48120   else if resp$(2)="Electric" then 
48140     let srvc=3 : let srvc$=resp$(2)
48160   else if resp$(2)="Gas" then 
48180     let srvc=4 : let srvc$=resp$(2)
48190   else 
48192     goto MAIN
48200   end if 
48220   let rcode=val(resp$(3))
54000   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,input,keyed 
54040   let fnopenprn
54060   gosub PRINTIT
58000 DONE: close #1: ioerr ignore
58020 L370: let fncloseprn
58040 XIT: let fnxit
58060 IGNORE: continue 
58080 ! ______________________________________________________________________
64000 PRINTIT: ! r:
64020   let p2=0
64040   let count=0
64060   gosub HDR
64080 READ_CUSTOMER: ! 
64100   read #1,using F_CUSTOMER: z$,mat e$,mat a,mat f$,mat d,mat g,last_billing_date eof PR_TOTALS ! READ MASTER RECORD
64120 F_CUSTOMER: form pos 1,c 10,pos 11,4*c 30,pos 143,7*pd 2,pos 131,c 12,pos 361,c 12,pos 373,c 12,pos 217,15*pd 5,pos 300,10*pd 4.2,pos 296,pd 4
64130   if bdate<>0 and bdate<>last_billing_date then goto READ_CUSTOMER
64140   if a(srvc)=0 then goto READ_CUSTOMER ! no service
64160   if a(srvc)<>rcode then goto READ_CUSTOMER
64180   let usage=0
64200   if srvc=1 then let usage=d(3): let amount=g(1): let meter$=f$(1) ! water
64220   if srvc=2 then let usage=d(3): let amount=g(2): let meter$="" ! sewer
64240   if srvc=3 then let usage=d(7): let amount=g(3): let meter$=f$(2) ! electric
64260   if srvc=4 then let usage=d(11): let amount=g(4): let meter$=f$(3) ! gas
64280   if a(srvc)=tc or tc=0 then 
64300     print #255,using F_PR_LINE: z$,e$(2),e$(1),meter$,usage,amount pageoflow PGOF
64320 F_PR_LINE: form x 5,c 10,x 5,c 30,x 7,c 30,x 2,c 12,x 2,pic(zzzzzzzzz),x 2,n 12.2
64340     let count+=1
64360     let totusage=totusage+usage
64380     let totamount=totamount+amount
64400   end if 
64420   goto READ_CUSTOMER
66000 PR_TOTALS: ! 
66020   print #255,using "Form POS 101,C 28": "____________  ____________"
66040   print #255,using "Form POS 101,N 12,X 2,N 12.2": totusage,totamount
66060   print #255,using "Form POS 101,C 28": "============  ============"
66080   print #255,using 'form pos 1,c 20,pic(zzzz,zzz,zzz)': "Total Customers:",count
66100   if count>0 then print #255,using 'form pos 1,c 20,pic(zzzz,zzz,zzz)': "Average Usage:",round(totusage/count,0)
66120   if count>0 then print #255,using 'form pos 1,c 20,pic(z,zzz,zzz.##)': "Average Amount:",round(totamount/count,2)
66140   close #1: 
66160   return  ! /r
68000 PGOF: ! r:
68020   print #255: newpage
68040   gosub HDR
68060   continue  ! /r
70000 HDR: ! r:
70020   let p2=p2+1
70040   print #255,using "Form POS 1,CC 80": cnam$
70060   print #255,using "Form POS 1,CC 80": "Consumption List - "&srvc$
70080   print #255,using " Form POS 1,CC 80": "Rate Code "&str$(rcode)
70100   print #255,using "Form POS 110,C 5,PIC(ZZZ)": "Page ",p2
70120   print #255: ""
70140   if tc<>0 then print #255,using L740: srvc$&" Code ",tc
70160 L740: form pos 41,c 9,n 2,skip 2
70180   print #255: tab(7);"Customer #";tab(21);"Name";tab(58);"Meter Address";tab(90);"   Meter #    Consumption  Dollar Amt"
70200   print #255: tab(7);"__________";tab(21);"________________________________";tab(58);"______________________________  ____________  ___________  __________"
70220   return  ! /r
76020 ! <updateable region: ertn>
76040 ERTN: let fnerror(program$,err,line,act$,"xit")
76060   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
76080   if uprc$(act$)="PAUSE" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT ! if env$("ACSDeveloper")<>"" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
76100   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
76120 ERTN_EXEC_ACT: execute act$ : goto ERTN
76140 ! </updateable region: ertn>
