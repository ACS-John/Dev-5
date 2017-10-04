00010 ! Replace S:\acsUB\UBCustDp
00020 ! -- Customer Deposit List
18000 ! r: setup stuff
18020   library 'S:\Core\Library': fnacs,fntos,fntxt,fnlbl,fnopenprn,fncloseprn, fnerror,fndat,fnwait,fncno,fnwin3,fnxit,fncmdset,fntop,fnchk,fnopt,fnget_services,fncomboa,fncreg_read,fncreg_write
18040   on error goto ERTN
18060   dim z$*10,e$(2)*30,b(11),c(4),cnam$*40,dat$*20,t(4),r(4),deposit$(4)*18
18080   dim totalb(11),s(4)
18100   dim cap$*128,message$*40,services$(4)*16,date_amount$*60
18120   dim servicename$(10)*20,tt$*160,resp$(10)*80,sn$*30,date_amount$*80
18140   dim filter_option$(6)*31,filter_default$*31
18160   let filter_option$(1)="[All]"
18180   let filter_option$(2)="0 - Active"
18200   let filter_option$(3)="1 - Inactive / Final Billed"
18220   let filter_option$(4)="2 - Inactive / Deposit Refunded"
18240   let filter_option$(5)="3 - Active / but Do Not Bill"
18260   let filter_option$(6)="4 - Finaled / but Not Billed"
18280 ! ______________________________________________________________________
19000   fntop(program$,cap$="Deposit List")
19020   fncno(cno,cnam$)
19040   fnget_services(mat servicename$ )
19060   for j=1 to 4
19080     servicename$(j)=lpad$(rtrm$(servicename$(j)),16)
19100   next j
19120   fndat(dat$,1)
19140   fncreg_read('ubcustdp.sequence',seq$) : seq=val(seq$)
19160   fncreg_read('ubcustdp.subtotal by route',subtotal$)
19180   fncreg_read('ubcustdp.filter choice',filter_default$)
19200   if filter_default$='' then let filter_default$=filter_option$(1)
19220 ! /r
32000   fntos(sn$="ubcustdp_1" )
32020   let rc=0 : let mylen=30 : let mypos=mylen+3
32040   fnlbl(1,1,"Report Heading Date:",mylen,1)
32060   fntxt(1,mypos,20)
32080   let resp$(1)=trim$(dat$)
32082   fnlbl(3,1,"Sequence:",mylen,1)
32100   fnopt(3,mypos,"Name")
32120   if seq=1 then let resp$(2)="True" else let resp$(2)="False"
32140   fnopt(4,mypos,"Route and Sequence Number")
32160   if seq<>1 then let resp$(3)="True" else let resp$(3)="False"
32180   fnchk(6,mylen+4,"Subtotal By Route:",1)
32200   if subtotal$='True' then let resp$(resp_subtotal:=4)='True' else let resp$(resp_subtotal:=4)="False"
32220   fnlbl(8,1,"Final Billing Code:",mylen,1)
32380   fncomboa("final_bill",8,mypos,mat filter_option$,"",25)
32400   let resp$(resp_filter:=5)=filter_default$
32420   fncmdset(3)
32440   fnacs(sn$,0,mat resp$,ckey)
32460   if ckey=5 then goto XIT
32480   let dat$=resp$(1)
32520   if resp$(2)="True" then seq=1 else seq=2 ! 1=name sequence  2= route sequence
32540   subtotal$=resp$(resp_subtotal)
32560   let filter_choice=srch(mat filter_option$,resp$(resp_filter))
36000 ! r: save answers
36020   fndat(dat$,2)
36040   fncreg_write('ubcustdp.sequence',str$(seq))
36060   fncreg_write('ubcustdp.subtotal by route',subtotal$)
36080   fncreg_write('ubcustdp.filter choice',resp$(resp_filter))
36100 ! /r
48000   fnopenprn
48020   gosub HEADER
48040   if seq=1 then 
48100     open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\UBIndx2.h"&str$(cno)&",Shr",internal,input,keyed  ! name sequence
48120   else 
48140     open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndx5.h"&str$(cno)&",Shr",internal,input,keyed  ! route sequence
48160   end if 
50000 F_CUSTOMER: form pos 1,c 10,2*c 30,pos 157,11*pd 4.2,4*pd 4,pos 1741,n 2,n 7,pos 1821,n 1
52000   do 
52010 CUSTOMER_READ: ! 
52020     let holdroute=route
52040     read #1,using F_CUSTOMER: z$,mat e$,mat b,mat c,route,sequence,final_billing_code eof PRINT_GRAND_TOTALS
52060     if c(3)=1 or c(3)=2 then c(3)=0 ! if deposit date previously used for final billing code, set it to zero
52080     if c(4)=1 or c(4)=2 then c(4)=0
54000     if filter_choice>1 then ! didn't select [All]
54020       if filter_choice=2 then ! 0 - Active
54040         if final_billing_code<>0 then goto CUSTOMER_READ
54060       else if filter_choice=3 then ! 1 - Inactive / Final Billed
54080         if final_billing_code<>1 then goto CUSTOMER_READ
54100       else if filter_choice=4 then ! 2 - Inactive / Deposit Refunded
54120         if final_billing_code<>2 then goto CUSTOMER_READ
54140       else if filter_choice=5 then ! 3 - Active / but Do Not Bill
54160         if final_billing_code<>3 then goto CUSTOMER_READ
54180       else if filter_choice=6 then ! 4 - Finaled / but Not Billed
54200         if final_billing_code<>4 then goto CUSTOMER_READ
54220       end if 
54240     end if 
56020     mat totalb=totalb+b
56040     if seq=2 and subtotal$="True" then ! consider subtotals
56060       if holdroute>0 and holdroute<>route then gosub PRINT_SUB_TOTALS
56080     end if 
56100     gosub PRINT_DETAILS
56120   loop 
56140 ! ______________________________________________________________________
58000 HEADER: ! r:
58020   let p2=p2+1
58040   lnpg=0
58060   pr #255: "\qc  {\f181 \fs22 \b "&env$('cnam')&"}"
58080   pr #255: "\qc  {\f181 \fs28 \b "&env$('program_caption')&"}"
58100   pr #255: "\qc  {\f181 \fs18 \b "&trim$(dat$)&"}"
58120   pr #255,using L540: "\ql ","Page "&str$(p2)
58140 L540: form pos 1,c 5,pos 110,c 10
58160   pr #255: ""
58180   let jp=0
58200   let date_amount$=""
58220   mat services$=("")
58240   for j=1 to 3
58260     let x=pos(servicename$(j),":",1)
58280     if x>0 then servicename$(j)(x:x)=""
58300   next j
58320   for j=1 to 4
58340     if j=1 and trim$(servicename$(j))<>"Water" then goto L730
58360     if j=2 and trim$(servicename$(j))<>"Sewer" then goto L730
58380     if j=3 and trim$(servicename$(j))<>"Electric" then goto L730
58400     if j=4 and trim$(servicename$(j))<>"Gas" then goto L730
58420     let jp=jp+1
58440     let p1=jp*19+40
58460     let date_amount$=date_amount$&"  --Date--  Amount"
58480     let x=pos(trim$(servicename$(j))," ",1)
58500     if x=0 then let x=len(servicename$(j))
58520     services$(jp)=trim$(servicename$(j))(1:x)
58540 L730: ! 
58542   next j
58560   pr #255,using 'form pos 62,4*cc 18': mat services$
58600   pr #255,using 'form pos 60,c 80': date_amount$
58640   return  ! /r
62000 PRINT_DETAILS: ! r:
62020   let jp=0
62040   for j=1 to 4
62060     if j=1 and trim$(servicename$(j))<>"Water" then goto L930
62080     if j=2 and trim$(servicename$(j))<>"Sewer" then goto L930
62100     if j=3 and trim$(servicename$(j))<>"Electric" then goto L930
62120     if j=4 and trim$(servicename$(j))<>"Gas" then goto L930
62140     let jp=jp+1
62160     let p1=jp*19+40
62180     let depdate(jp)=c(j)
62200     amount(jp)=b(j+7)
62220     let r(j)=r(j)+b(j+7)
62240     s(j)=s(j)+b(j+7)
62260     let deposit$(jp)=cnvrt$("pic(zzz/zz/zz)",depdate(jp))&" "&cnvrt$("nz 8.2",amount(jp))
62280 L930: ! 
62282   next j
62300   mat deposit$(jp)
62320   pr #255,using L960: z$,e$(2)(1:22),e$(1)(1:22),mat deposit$ pageoflow NEWPGE
62340 L960: form c 12,2*c 23,x 2,jp*c 18
62360   return  ! /r
64000 PRINT_SUB_TOTALS: ! r:
64020   pr #255: "{\b Sub-totals:}"
64040   for j=1 to 4
64060     if trim$(servicename$(j))<>"" then pr #255,using F_TOTALS: servicename$(j),s(j)
64080     form pos 7,c 30,n 10.2
64100   next j
64120   mat s=(0)
64140   return  ! /r
64160 ! ______________________________________________________________________
66000 PRINT_GRAND_TOTALS: ! r:
66020   pr #255: "{\b Totals:}"
66040   for j=1 to 4
66060     if trim$(servicename$(j))<>"" then pr #255,using F_TOTALS: servicename$(j),r(j)
66080 F_TOTALS: form pos 7,c 30,n 10.2
66100   next j
66120   close #1: ioerr ignore
66140   fncloseprn
66160   goto XIT ! /r
66180 XIT: let fnxit
66200 ! ______________________________________________________________________
68000 NEWPGE: ! r:
68020   pr #255: newpage
68040   gosub HEADER
68060   continue  ! /r
68080 ! ______________________________________________________________________
70000 ! <Updateable Region: ERTN>
70020 ERTN: let fnerror(program$,err,line,act$,"xit")
70040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
70060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
70080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
70100 ERTN_EXEC_ACT: execute act$ : goto ERTN
70120 ! /region
70140 IGNORE: continue 
