00100 ! Replace S:\acsUB\Sewer ! dont forget to change lines 510 &  640
00120 ! ______________________________________________________________________
00140   library 'S:\Core\Library': fntop,fnxit, fnacs,fnwait,fnopenprn,fncloseprn,fnerror,fnmsgbox,fntxt,fnlbl,fntos,fngethandle,fncreg_read,fncreg_write,fncmdkey,fnapply_default_rates
00160 ! ______________________________________________________________________
00180   on error goto ERTN
00200 ! ______________________________________________________________________
00220   dim cd1(8),cap$*128,x(13),message$(5)*80,message$*60,tg(11),extra(23)
00240 ! ______________________________________________________________________
00280   let fntop(program$,cap$="Calculate Sewer Average")
00300 ! ______________________________________________________________________
00320   open #h_trans:=fngethandle: "Name="&env$('Q')&"\UBmstr\ubTransvb.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubTrIndx.h"&env$('cno')&",Shr",internal,input,keyed 
00340   open #h_customer:=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,outin,keyed 
00360   read #h_customer,using L500: x$,customer_sewer_rate_code,oldavg eof DONE
00370   gosub APPLY_DEFAULT_RATE
00380   restore #h_trans,key>=x$&"         ": nokey L220
00400 L160: ! 
00420   read #h_trans,using F_TRANS: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof L220
00440   if p$<>x$ then goto L220 ! history record must belong to this customer
00460   if tcode<>1 then goto L160 ! charge transaction
00480   let j=j+1
00500   if j>8 then goto L220
00520   let resp$(j)=str$(tdate)
00540   goto L160
00560 L220: ! 
00580   restore #h_customer: 
00600 ! ______________________________________________________________________
00620 SCR1: ! 
00640   let fntos(sn$:="ubsewer-1b")
00660   let mylen=47 : let mypos=49
00680   let fnlbl(1,1,"Billing Dates for Months to Average:",mylen,1)
00700   for j=1 to 8
00720     let fntxt(j,mypos,10,0,0,"3")
00740     let fncreg_read(sn$&'.billing date.'&str$(j),resp$(j))
00760   next j
00780   let fnlbl(10,1,"Sewer code to average:",mylen,1)
00800   let fntxt(10,mypos,2,2,0,"20")
00820   let fncreg_read(sn$&'.sewer code to average',resp$(9))
00840 ! let fncmdset(2)
00842   let fncmdkey("&Clear Sewer Code Averages",3,0)
00843   let fncmdkey("&Next",1,1)
00844   let fncmdkey("&Cancel",5,0,1)
00860   let fnacs(sn$,0,mat resp$,ckey)
00870   if ckey=5 then goto XIT
00880   if ckey=3 then clear_averages=1 else clear_averages=0
00900   for j=1 to 8
00920 L310: ! 
00940     let x=pos(resp$(j),"/",1)
00960     if x>0 then 
00980       let resp$(j)(x:x)=""
01000       goto L310
01020     end if 
01040   next j
01060   let filter_sewer_code=val(resp$(9)) conv SCR1
01080   if filter_sewer_code=0 and ~clear_averages then 
01100     mat message$(1)
01120     let message$(1)="You must enter at least one date!"
01140     let fnmsgbox(mat message$,resp$,cap$,0)
01160     goto SCR1
01180   end if 
01200   for j=1 to 8
01220     cd1(j)=val(resp$(j)) conv SCR1
01240     let fncreg_write(sn$&'.billing date.'&str$(j),resp$(j))
01260   next j
01280   let fncreg_write(sn$&'.sewer code to average',resp$(9))
01300   if cd1(1)=0 and ~clear_averages then 
01320     mat message$(1)
01340     let message$(1)="You must enter at least one date!"
01360     let fnmsgbox(mat message$,resp$,cap$,0)
01380     goto SCR1
01400   end if 
01420 ! ______________________________________________________________________
01440   let fnopenprn
01460   let message$="Calculating: please wait..."
01480   let fnwait(0,cap$,message$,1)
01500   gosub HDR
01520 L480: ! 
01540   read #h_customer,using L500: x$,customer_sewer_rate_code,oldavg eof DONE
01550   gosub APPLY_DEFAULT_RATE
01560   if customer_sewer_rate_code<>filter_sewer_code then goto L480 ! only average certain rate codes
01580 L500: form pos 1,c 10,pos 145,pd 2,pos 1822,n 9
01582 ! r: calculate average
01584   if clear_averages then 
01586     let t3=0
01588   else 
01600     let t1=t2=t3=x=0
01620     mat x=(0)
01640     restore #h_trans,key>=x$&"         ": nokey L480
01660 READ_TRANS: ! 
01680     read #h_trans,using F_TRANS: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof EO_TRANS
01700 F_TRANS: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
01720     if p$<>x$ then goto EO_TRANS
01740     if tcode<>1 then goto READ_TRANS ! only charge transactions
01760 ! if trim$(x$)='306100.00' then pause
01780     for j1=1 to 8
01800       if cd1(j1)=tdate then 
01820         let t1=t1+1
01840         let t2=t2+wu
01860         let x=x+1
01880         let x(x)=wu
01900         goto READ_TRANS
01920       end if 
01940     next j1
01960     goto READ_TRANS
01980 ! ______________________________________________________________________
02000 EO_TRANS: ! 
02020     if t1>0 then let t3=int((t2+.5)/t1) else let t3=0
02040     if env$('client')="Monticello" and t3=0 then goto L670 ! skip record if no dates found  (multiple cycles)
02060     if t1=1 and env$('client')="Monticello" then let t3=8 ! if only one month use 8
02062   end if 
02070 ! /r
02080   rewrite #h_customer,using "Form POS 1822,N 9": t3
02100   pr #255,using L660: x$,oldavg,t3,x(1),x(2),x(3),x(4) pageoflow PAGE
02120 L660: form pos 1,c 12,6*nz 9
02140 L670: ! 
02160   goto L480
02180 ! ______________________________________________________________________
02200 DONE: ! 
02220   close #h_customer: 
02240   let fncloseprn
02260 XIT: let fnxit
02280 ! ______________________________________________________________________
02300 PAGE: pr #255: newpage
02320   gosub HDR
02340   continue 
02360 ! ______________________________________________________________________
02380 HDR: ! 
02400   let p1=p1+1
02420   pr #255,using "Form POS 20,CC 40,POS 70,C 5,N 4": env$('cnam'),"Page ",p1
02430   pr #255,using "Form POS 20,CC 40": "Calculate Sewer Average"
02440   pr #255,using "Form POS 20,CC 40": "Sewer Averages for Sewer Code "&ltrm$(str$(filter_sewer_code))
02460   pr #255: ""
02480   pr #255: " Acct.Num.    Old Avg  New Avg "&cnvrt$("pic(zzzzz/zz/zz)",cd1(1))&cnvrt$("pic(zzzzz/zz/zz)",cd1(2))&cnvrt$("pic(zzzzz/zz/zz)",cd1(3))&cnvrt$("pic(zzzzz/zz/zz)",cd1(4))
02490 ! pr #255: "__________    _______  _______ ___________ ___________ ___________ ___________"
02500   return 
02520 ! ______________________________________________________________________
02540 ! <Updateable Region: ERTN>
02560 ERTN: let fnerror(program$,err,line,act$,"NO")
02580   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
02600   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
02620   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
02640 ERTN_EXEC_ACT: execute act$ : goto ERTN
02660 ! /region
02680 ! ______________________________________________________________________
14000 APPLY_DEFAULT_RATE: ! r:
14020   a(2)=customer_sewer_rate_code
14040   fnapply_default_rates(mat extra, mat a)
14060   customer_sewer_rate_code=a(2)
14080 return ! /r
