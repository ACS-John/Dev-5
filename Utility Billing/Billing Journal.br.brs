00010 ! formerly S:\acsUB\ubBilJrn
00020 ! -- Print Billing Journal
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnacs,fnlbl,fntxt,fnwait,fncmbrt2,fntos,fnerror,fnopenprn,fncloseprn,fndate_mmddyy_to_ccyymmdd,fnd1,fncmdset,fnchk,fnfra,fnopt,fnpause,fncreg_read,fncreg_write,fnget_services,fnindex_it,fnstatus_pause
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim z$*10,e$(4)*30,g(12)
00090   dim a(7),t1(10,200,3),st$(10)
00092   dim de$*40
00094   dim x2(5),d(15),extra(23)
00100   dim hd1$*400,hd2$*400
00102   dim px$(99)*30,px(99),tx(99),gx(99)
00104   dim resp$(20)*128
00110   dim servicename$(10)*20,tax_code$(10)*1
00112   dim tg(11),usages(3)
00120 ! ______________________________________________________________________
00130   let fntop(program$)
00170   let fnd1(billing_date)
00200   fncreg_read('Route Low',bkno1$) : route_number=val(bkno1$)
00230   fnget_services(mat servicename$,mat service$,mat tax_code$,mat penalty$)
00260   let hd1$="Account                             "
00270   let hd2$="{\ul Number   }  {\ul Name                   }  "
00280   for j=1 to 10
00290     let x2=pos(trim$(servicename$(j))," ",1)
00300     if x2>0 then let servicename$(j)=servicename$(j)(1:2)&"-"&servicename$(j)(x2+1:len(servicename$(j)))
00310     if trim$(servicename$(j))<>"" then 
00320       let x1=pos (servicename$(j)," ",1)
00330       let x1=min(x1,7)
00340       let hd1$=hd1$&"---------"
00350       let hd2$=hd2$&"{\ul "&lpad$(trim$(servicename$(j)(1:x1)),8)&"} "
00360       let sz1=sz1+1 : let px$(sz1)=servicename$(j)
00370     end if 
00380   next j
00390   open #h_trans:=2: "Name="&env$('Q')&"\UBmstr\UBTransVB.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\UBTrIndx.h"&env$('cno')&",Shr",internal,input,keyed 
00400   open #8: "Name="&env$('Q')&"\UBmstr\ubData\RateMst.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubData\RateIdx1.h"&env$('cno')&",Shr",internal,input,keyed 
00402 ! r: get default sequence
00404   let fncreg_read('ubBilJrn.Sort_Option',sequence$)
00406   let seq=val(sequence$) conv ignore
00408   if seq<1 then let seq=1
00410 ! /r
00420 MAIN: ! r: Screen 1
00430   let fntos(sn$="UBBilJrn")
00440   let respc=0
00480   let fnlbl(2,1,"Billing Date:",25,1)
00490   let fntxt(2,27,8,0,1,"1")
00500   let resp$(respc+=1)=str$(billing_date)
00510   let fnfra(3,1,4,65,"Sort Order","The billing journal can be printed if route number sequence, account sequence or Alpha Sort Sequence.",0)
00520   let fnopt(1,2,"Route/Sequence Number (includes Subtotals by Route)",0,1)
00530   if seq=1 then let resp$(respc+=1)="True" else let resp$(respc+=1)="False"
00540   let fnopt(2,2,"Account",0,1)
00550   if seq=2 then let resp$(respc+=1)="True" else let resp$(respc+=1)="False"
00560   let fnopt(3,2,"Alpha Sort",0,1)
00570   if seq=3 then let resp$(respc+=1)="True" else let resp$(respc+=1)="False"
00572   let fnopt(4,2,"Customer Name",0,1)
00574   if seq=4 then let resp$(respc+=1)="True" else let resp$(respc+=1)="False"
00580   let fnlbl(9,1,"Route Number:",25,1)
00590   let fncmbrt2(9,27)
00600   let resp$(resp_route:=respc+=1)="[All]"
00610   let fnchk(10,27,"Print Usages:",1)
00612   let fncreg_read('ubBilJrn.Print Usages',resp$(resp_print_usages:=respc+=1))
00620   let fncmdset(3)
00630   let fnacs(sn$,0,mat resp$,ckey)
00650   if ckey=5 then goto XIT
00670   let billing_date=val(resp$(1))
00680   if resp$(2)="True" then let seq=1 ! route sequence
00690   if resp$(3)="True" then let seq=2 ! account sequence
00700   if resp$(4)="True" then let seq=3 ! Alpha Sort Sequence
00701   if resp$(5)="True" then let seq=4 ! Customer Name Sequence
00702   if uprc$(resp$(resp_route)) = uprc$("[All]") then let resp$(resp_route) = "0"
00710   let prtbkno=val(resp$(resp_route))
00720   let prtusage$=resp$(resp_print_usages)(1:1)
00722   let fncreg_write('ubBilJrn.Sort_Option',str$(seq))
00726   let fncreg_write('ubBilJrn.Print Usages',resp$(resp_print_usages))
00732 ! /r
00740   if seq=0 or seq=1 then ! route number
00742     open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndx5.h"&env$('cno')&",Shr",internal,input,keyed 
00744   else if seq=2 then            ! account
00745     open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,input,keyed 
00746   else if seq=3 then ! Alpha Sort Sequence
00747     open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\UBIndx2.h"&env$('cno')&",Shr",internal,input,keyed  
00748   else if seq=4 then ! Customer Name
00750     let fnindex_it(env$('Q')&"\UBmstr\Customer.h"&env$('cno'), env$('temp')&"\customer_name"&session$&".h"&env$('cno'),"41 30")
00758     open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('temp')&"\customer_name"&session$&".h"&env$('cno')&",Shr",internal,input,keyed  
00760   end if
00770   if trim$(servicename$(1))="Water" then let services=services+1 : let water=1
00780   if trim$(servicename$(3))="Electric" or trim$(service$(3))="LM" then let services=services+1
00782   if servicename$(3)(1:5)="Re-Se" then let reduc=1 : let services+=1
00790   if trim$(servicename$(4))="Gas" then let services=services+1 : let gas=1
00800   mat usages(services)
00810   let hd1$=hd1$&"---------    Prior  Current"
00820   let hd2$=hd2$&"{\ul    Total} {\ul  Balance} {\ul  Balance}  "
00830   let x1=int((len(hd1$)-43)/2)+27
00840   let hd1$(x1:x1+17)=" Current Billing "
00850   let px$(sz1+=1)="   Total"
00860   let px$(sz1+=1)="Previous Balance"
00870   let px$(sz1+=1)="Current Balance"
00880   mat px$(sz1) : mat tx(sz1) : mat gx(sz1) : mat px(sz1)
00890   if prtusage$="T" and trim$(servicename$(1))="Water" then let hd2$=hd2$&" {\ul  W-Usage}"
00900   if prtusage$="T" and trim$(servicename$(3))="Electric" then let hd2$=hd2$&" {\ul  E-Usage}"
00910   if prtusage$="T" and trim$(service$(3))="LM" then let hd2$=hd2$&" {\ul LM-Usage}"
00920   if prtusage$="T" and trim$(servicename$(4))="Gas" then let hd2$=hd2$&"{ \ul  G-Usage}"
00930   if prtusage$="T" then let hd2$=hd2$&"{ \ul Meter Address}"
00950   let fnopenprn
00952   gosub HDR
00954   if prtbkno<>0 and seq=1 then 
00956     let prtbkno$=rpad$(lpad$(str$(prtbkno),2),kln(1))
00958     let startcd=1
00960     restore #1,key>=prtbkno$: nokey MAIN
00962   end if
00970 ! ______________________________________________________________________
00980 READ_CUSTOMER: ! 
00990   read #1,using L680: z$,mat e$,mat a,mat d,bal,f,mat g,route,estimatedate,mat extra eof TOTAL_GRAND
00995 L680: form pos 1,c 10,4*c 30,pos 143,7*pd 2,pos 217,15*pd 5,pos 292,pd 4.2,pd 4,12*pd 4.2,pos 1741,n 2,pos 1831,n 9,pos 1741,n 2,n 7,2*n 6,n 9,pd 5.2,n 3,3*n 9,3*n 2,3*n 3,n 1,3*n 9,3*pd 5.2
01000   if env$('client')="Gilbertown" and f<>billing_date and bal<>0 then mat g=(0): goto L730
01005   if f=billing_date then goto L730
01015   if f><billing_date then no_match_found=fn_pull_from_history else no_match_found=0
01020   if no_match_found=0 then goto READ_CUSTOMER
01022 L730: !
01023   if seq=1 then  !  route subtotals 
01024     if startcd=1 and prtbkno<>route then 
01025       goto TOTAL_GRAND
01026     end if
01029     if (route_number>0 and route_number<prtbkno) or route_number=route then goto L790
01030     gosub TOTAL_BOOK
01032     print #255: newpage : gosub HDR
01033     L790: !
01034     let route_number=route
01036   end if
01037   gosub L910
01038   let tbp=tbp+1
01040   goto READ_CUSTOMER
01044 HDR: ! r:
01046   print #255: "\qc  {\f181 \fs18 \b "&env$('cnam')&"}"
01048   print #255: "\qc  {\f181 \fs24 \b "&env$('cap')&"}"
01050   print #255: "\qc  {\f181 \fs16 \b Billing Date "&cnvrt$("pic(zz/zz/zz)",billing_date)&"  Page "&str$(pge+=1)&"}"
01052   print #255: "\qc  {\f181 \fs16 \b "&date$("Month DD, CCYY")&"}"
01054   print #255: "\ql   "
01056   print #255: hd1$
01058   print #255: hd2$
01060   return  ! /r
01064 L910: ! r:
01066   let e=bal-g(11) : let j1=0
01068   for j=1 to 10
01070     if trim$(servicename$(j))<>"" then let px(j1+=1)=g(j)
01072   next j
01074   let px(j1+1)=g(11) : let px(j1+2)=e : let px(j1+3)=bal
01076   mat tx=tx+px : mat gx=gx+px
01078   let x=0
01080   if water=1 then let x=x+1 : let usages(x)=d(3)
01083   if reduc=1 then let x=x+1 : let usages(x)=d(3)-d(7)
01084   if gas=1 then let x=x+1 : let usages(x)=d(11)
01086   if estimatedate=billing_date then let est$="E" else let est$=""
01088   if prtusage$="T" then print #255,using L1020: z$,e$(2)(1:23),mat px,est$,mat usages,e$(1)(1:25) pageoflow PGOF else print #255,using L1020: z$,e$(2)(1:23),mat px,est$ pageoflow PGOF
01090 L1020: form pos 1,c 10,x 2,c 23,sz1*n 9.2,x 1,c 1,services*n 9,x 1,c 25
01092   gosub TOT1
01094 ! If CODEMIS=1 Then
01096 ! Print #255: " * The Previous Record has a charge, but does not have a matching code!"
01098 ! end if
01100   let codemis=0
01102   return  ! /r
01106 PGOF: ! r:
01108   print #255: newpage
01110   gosub HDR
01112   continue  ! /r
01116 TOTAL_BOOK: ! r:
01118   print #255: ""
01120   print #255: tab(27);"{\ul Totals for Route Number "&str$(route_number)&"}"
01122   for j=1 to sz1
01124 !   if trim$(px$(j))<>"Penalty" then ! don't allow any penalties go thur totals
01126     print #255,using "Form POS 1,C 30,N 15.2": px$(j),tx(j)
01128 !   end if  ! trim$(px$(j))<>"Penalty"
01130   next j
01132   mat tx=(0)
01134   return  ! /r
01138 TOTAL_GRAND: ! r:
01140   close #1: ioerr L1220
01142   if seq<>1 then 
01144     print #255: ""
01146     print #255: tab(27);"{\ul Totals for All Routes }       {\ul Grand Totals}"
01148     goto L1230
01150   end if 
01220 L1220: ! 
01230   print #255: ""
01240   print #255: tab(27);"{\ul Totals for Route Number "&str$(route_number)&"}       {\ul Grand Totals}"
01245 L1230: !
01246   for j=1 to sz1
01250 ! if trim$(px$(j))<>"Penalty" then ! don't allow any penalties go thur totals
01255     print #255,using "Form POS 1,C 30,N 15.2,X 6,N 15.2": px$(j),tx(j),gx(j)
01260 ! end if  ! trim$(px$(j))<>"Penalty" then
01265   next j
01270   gosub PRINT1
01275   goto DONE ! /r
01285 DONE: close #1: ioerr ignore
01310   let fncloseprn
01320 XIT: let fnxit
01330 IGNORE: continue 
01420 TOT1: ! r: ACCUMULATE TOTALS BY CODE
01430   for j=1 to 10
01440     if trim$(servicename$(j))="" or uprc$(penalty$(j))="Y" then goto L1720 ! don't allow any penalties go thur totals
01460     let x2=1 : let u2=0
01462     if j<=4 then 
01464       let x2=a(j)
01466       if j=1 or j=2 then 
01468         let u2=d(3)
01470       else if j=3 and (trim$(servicename$(3))="Electric" or trim$(servicename$(3))="Lawn Meter") then 
01472         let u2=d(7)
01474       else if j=4 and trim$(servicename$(4))="Gas" then 
01476         let u2=d(11)
01478       end if 
01480     end if 
01482     if j=9 or j=10 then 
01484       let x2=a(j-3)
01486     else if j=6 or j=7 or j=8 then 
01488       let x2=extra(j+5)
01490     else if j=5 then 
01492       if env$('client')="French Settlement" then 
01494         let x2=a(4)
01496       else 
01498         let x2=a(5)
01500       end if 
01502     end if 
01504 ! r: calculate U2 - U2 is the temporary Tax Base accumulator
01506     if env$('client')="French Settlement" and j=8 then let u2=0 : goto L1690 ! if Other Charge do not add to tax base
01508     if env$('client')="Carrizo" and j=3 then let u2=0 : goto L1520 ! allow canister rental to go thru tax
01510     if j=2 and reduc=1 then let u2=d(3)-d(7)
01512     if j<5 then goto L1690 ! don't allow real usage to go thru taxable routine
01520 L1520: ! 
01522     for j3=1 to 10
01530       if env$('client')="Carrizo" then 
01540         if j=7 and extra(12)<>9 then let u2=u2+g(3)+g(5)+g(6) : goto L1690
01550         if j=3 and extra(12)<>9 then let u2=u2+g(3): goto L1690
01560         if j=5 and extra(12)<>9 then let u2=u2+g(5): goto L1690
01570         if j=6 and extra(12)<>9 then let u2=u2+g(6): goto L1690
01580       else if env$('client')="Franklinton" then 
01590         if j=5 then let u2=0 : goto L1690
01600         if j=6 then let u2=u2+g(6) : goto L1690
01610         if j=7 then let u2=u2+g(1) : goto L1690
01620         if j=9 then let u2=u2+g(4) : goto L1690
01630       else if env$('client')="Bethany" then 
01640         if j=7 then let u2=u2+g(3) : goto L1690 ! bethany
01650         if j=9 then let u2=u2+g(4) : goto L1690 ! gas taxable
01660         goto L1680 ! bethany & franklinton
01670       end if 
01672       if uprc$(tax_code$(j3))="Y" then let u2=u2+g(j3) ! if service is taxable, add to total taxable dollars
01680 L1680: ! 
01682     next j3
01684 ! /r
01690 L1690: ! 
01692     if g(j)><0 and x2=0 then let x2=200: let codemis=1
01700     if x2>200 then let x2=200
01702     if env$('client')="Billings" and j>5 and j<9 then goto L1720
01704     if x2<>0 then 
01706       let t1(j,x2,1)=t1(j,x2,1)+1
01708       let t1(j,x2,2)=t1(j,x2,2)+g(j)
01710       let t1(j,x2,3)=t1(j,x2,3)+u2 : let u2=0
01712     end if 
01714 L1720: ! 
01716   next j
01730   return  ! /r
01732 PRINT1: ! r: PRINT TOTALS BY CODE
01734   for st_item=1 to 10
01736     let st$(st_item)=service$(st_item)
01738   next st_item
01754   print #255: ""
01756   print #255: "{\ul Service             }  {\ul Code}  {\ul Description                             }  {\ul Billed}  {\ul     Amount}  {\ul     Tax Base}  {\ul          Usage}"
01760   for j1=1 to 9
01765     for j2=1 to 200
01770       if t1(j1,j2,1)=0 and t1(j1,j2,3)=0 then goto L1960
01775       let de$=""
01780       if j2>99 then goto L1930 ! no codes >99
01785       let k$=st$(j1)(1:2)&cnvrt$("N 2",j2)
01790       let de$=""
01795       read #8,using "Form POS 5,C 40",key=k$,release: de$ nokey L1890,ioerr L1890
01800       if env$('client')="Carrizo" and j1=3 then goto L1930
01805       if j1>4 then goto L1930
01890 L1890: ! print #255,using L1950: st$(j1)(1:13),j2,de$,t1(j1,j2,1),t1(j1,j2,2),t1(j1,j2,3)
01892       print #255,using L1950: servicename$(j1)(1:13),j2,de$,t1(j1,j2,1),t1(j1,j2,2),t1(j1,j2,3)
01900       if env$('client')="Sangamon" and st$(j1)="WA" then let waterdollars+=t1(j1,j2,2) : let waterusage+=t1(j1,j2,3)
01910       if env$('client')="Sangamon" and st$(j1)="SW" then let sewerdollars+=t1(j1,j2,2) : let sewerusage+=t1(j1,j2,3)
01920       goto L1960
01930 L1930: ! 
01932       print #255,using L1940: st$(j1)(1:13),j2,de$,t1(j1,j2,1),t1(j1,j2,2),t1(j1,j2,3)
01940 L1940: form pos 1,c 21,n 4,x 3,c 40,n 8,n 12.2,pic(---,---,---.--)
01950 L1950: form pos 1,c 21,n 4,x 3,c 40,n 8,n 12.2,x 14,pic(----,---,---,---)
01960 L1960: ! 
01962     next j2
01970   next j1
01980   for j=1 to 10
01990     if t1(j,200,1)>0 or t1(j1,200,2)>0 then 
01992       print #255: "   * All charges billed without a service code are summarized as Code 200"
01994       goto L2010
01998     end if 
02000   next j
02010 L2010: ! 
02012   if env$('client')="Sangamon" then print #255,using "form pos 1,c 13,pos 69,n 12.2,x 14,pic(----,---,---,---)": "Water Totals",waterdollars,waterusage
02020   if env$('client')="Sangamon" then print #255,using "form pos 1,c 13,pos 69,n 12.2,x 14,pic(----,---,---,---)": "Sewer Totals",sewerdollars,sewerusage
02030   print #255: " ": print #255: "    Total # of Bills To Be Printed: "&str$(tbp)
02040   return  ! /r
02060 def fn_pull_from_history
02070   mat g=(0) : mat d=(0) : let bal=0
02072   let pfh_return=0
02080   restore #h_trans,key>=z$&"         ": nokey PFH_XIT
02085   do
02086     read #h_trans,using 'form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1': p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof PFH_XIT
02100     if z$<>p$ then goto PFH_XIT
02110     if tcode=1 then 
02120       let x=billing_date: let x=fndate_mmddyy_to_ccyymmdd(x)
02130       if x=tdate then goto PFH_MATCH_FOUND ! FOUND MATCH
02132     end if
02140   loop
02150 PFH_MATCH_FOUND: !
02152   let pfh_return=1
02160   let d(1)=wr : let d(3)=wu : let d(5)=er
02162   let d(7)=eu : let d(9)=gr : let d(11)=gu 
02164   let bal=tbal
02170   for j=1 to 11 : let g(j)=tg(j) : next j
02180 PFH_XIT: !
02182   fn_pull_from_history=pfh_return
02190 fnend
02200 ! <Updateable Region: ERTN>
02210 ERTN: let fnerror(program$,err,line,act$,"xit")
02220   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
02230   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
02240   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
02250 ERTN_EXEC_ACT: execute act$ : goto ERTN
02260 ! /region
