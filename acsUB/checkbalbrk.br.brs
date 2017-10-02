10020   library 'S:\Core\Library': fntop,fnacs,fnopenprn,fncloseprn,fnerror,fnmsgbox,fntxt,fnlbl,fntos,fncno,fnxit,fncmdset,fntop,fndat,fnopt
10040   let fntop("S:\acsUB\checkbalbrk",cap$="Check Balance Breakdown 1")
10060   dim cnam$*40,dat$*20,cb(13),gb(10),a(7),resp$(1)*128
10080   let fncno(cno,cnam$)
10100   dim g(10)
10120   dim gb(10),u(61)
10140   dim servicename$(10)*20,service$(10)*2,tax_code$(10)*1,penalty$(10)*1,subjectto(10)
10160   dim cap$*128,txt$*80
10180   let fndat(dat$,1)
30000 ! r: screen 1
30020   let fntos(sn$="checkbalbrk-7")
30040   mat resp$(3)
30060   let fnlbl(1,1,"",40,0)
30080   let fnlbl(1,1,"Print Options:",38,0)
30100   let fnopt(2,3,'All',0)
30120   let resp$(1)="False"
30140   let fnopt(3,3,'Only accounts where Balance Breakdown does not equal Balance',0)
30160   let resp$(2)="True" !  if ckoption=1 or ckoption=3 then let resp$(respc+=1)="True" else let resp$(respc+=1)="False"
30180   let fnopt(4,3,'Only accounts without a credit balance but negative allocations exist',0)
30200   let resp$(3)="False"
30220   let fncmdset(2)
30240   let fnacs(sn$,0,mat resp$,ck)
30260   if ck=5 then goto XIT
30280   let filter_none=1
30300   let filter_not_equal=2
30320   let filter_negative=3
30340   if resp$(1)='True' then 
30360     let filter=filter_none
30380   else if resp$(2)='True' then 
30400     let filter=filter_not_equal
30420   else if resp$(3)='True' then 
30440     let filter=filter_negative
30460   else 
30480     let filter=filter_none
30500   end if 
30520 ! /r
31000 ! Gosub SCR_ASK_DATES
32100   let fnopenprn
32120   gosub HEADER
36000   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,outin,keyed 
36020   open #2: "Name="&env$('Q')&"\UBmstr\UBTransVB.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\UBTrIndx.h"&str$(cno)&",Shr",internal,input,keyed 
36040   open #20: "Name="&env$('Q')&"\UBmstr\ubData\Service.h"&str$(cno)&",Shr",internal,input,relative 
36060   read #20,using 'Form POS 1,10*C 20,10*C 2,10*C 1,10*C 1,10*N 2,pos 261,10*n 2',rec=1: mat servicename$,mat service$,mat tax_code$,mat penalty$,mat subjectto,mat apply
36080   close #20: 
36100   for j=1 to 10
36120     let servicename$(j)=lpad$(rtrm$(servicename$(j)(1:8)),8)
36140   next j
36160   gosub NWPGE
38000 READ_CUSTOMER: ! 
38020   read #1,using F_CUSTOMER: z$,mat a,bal,mat g,mat gb eof DONE
38040 F_CUSTOMER: form pos 1,c 10,pos 143,7*pd 2,pos 292,pd 4.2,pos 300,10*pd 4.2,pos 388,10*pd 5.2
38060 ! ------------------------------
38080   if filter=filter_not_equal and bal=sum(gb) then goto READ_CUSTOMER
38100   if filter=filter_negative then 
38120     if bal<0 then goto READ_CUSTOMER
38140     if ~fn_any_gb_negative then goto READ_CUSTOMER
38160   end if 
38180 ! gosub FIX_MAT_GB
38200 ! gosub REW_MAT_GB
38220   pr #255,using L340: z$,bal,sum(gb),mat gb pageoflow PGOF
38240 L340: form pos 1,c 10,x 1,12*n 9.2
38260   goto READ_CUSTOMER
40000 NWPGE: ! pr #255: NEWPAGE
40020   pr #255: "Account    Balance   TotBrk   "&servicename$(1)(1:8)&" "&servicename$(2)(1:8)&" "&servicename$(3)(1:8)&" "&servicename$(4)(1:8)&" "&servicename$(5)(1:8)&" "&servicename$(6)(1:8)&" "&servicename$(7)(1:8)&" "&servicename$(8)(1:8)&" "&servicename$(9)(1:8)&" "&servicename$(10)(1:8)&" "
40040   pr #255: "__________ _________ ________ ________ ________ ________ ________ ________ ________ ________ ________ ________ ________"
40060   return 
42000 DONE: let fncloseprn
44000 XIT: let fnxit
46000 REW_MAT_GB: ! 
46020   rewrite #1,using L440,key=z$: mat gb
46040 L440: form pos 388,10*pd 5.2
46060 ! pr #255: "Rewrote next record "&cause$
46080   return  ! 
48000 SCR_ASK_DATES: ! r:
48020   let sn$
48040   let fntos(sn$="balbrkfix")
48060   let mylen=62 : let mypos=50
48080   let txt$="Billing Dates for last three months:"
48100   let fnlbl(1,1,txt$,mylen,1)
48120   for j=1 to 3
48140     let fntxt(j+1,mypos,10,0,0,"3",0,"Put your most recent billing date first and then in order from there.")
48160     let resp$(j)=""
48180   next j
48200   let txt$="Penalty Dates for last three months:"
48220   let fnlbl(5,1,txt$,mylen,1)
48240   for j=1 to 3
48260     let fntxt(j+5,mypos,10,0,0,"3",0,"Put your most recent penalty date first and then in order from there.")
48280     let resp$(j+3)=""
48300   next j
48320   let fncmdset(2): let fnacs(sn$,0,mat resp$,ckey)
48340   if ckey=5 then goto XIT
48360   for j=1 to 6
48380 L560: let x=pos(resp$(j),"/",1)
48400     if x>0 then let resp$(j)(x:x)="": goto L560
48420   next j
48440   for j=1 to 6
48460     cd1(j)=val(resp$(j)) conv SCR_ASK_DATES
48480   next j
48500   if cd1(1)=0 then 
48520     mat message$(1): let mytype=0
48540     let message$(1)="You must enter at least one date!"
48560     let fnmsgbox(mat message$,resp$,cap$,mytype)
48580     goto SCR_ASK_DATES
48600   end if 
48620   return  ! /r
48640 ! ______________________________________________________________________
50000 HEADER: ! r:
50020   pr #255: "\qc  {\f181 \fs20 \b "&env$('cnam')&"}"
50040   pr #255: "\qc  {\f181 \fs22 \b "&env$('program_caption')&"}"
50060   pr #255: "\qc  {\f181 \fs16 \b "&trim$(dat$)&"}"
50080   if d1<>0 or oob$="Y" then 
50100     pr #255: "\qc "&trim$(temp$(1))&"   "&temp$(2)
50120   end if 
50140   pr #255,using L690: "\ql  ","Page "&str$(pg+=1)
50160 L690: form pos 1,c 82,c 10,skip 1
50200   return  ! /r
52000 PGOF: ! r:
52020   pr #255: newpage
52040   gosub HEADER
52060   continue  ! /r
54000   def fn_any_gb_negative
54020     agn_return=0
54040     for agn_item=1 to 10
54060       if gb(agn_item)<0 then agn_return=1
54080     next agn_item
54100     let fn_any_gb_negative=agn_return
54120   fnend  ! fn_any_gb_negative
