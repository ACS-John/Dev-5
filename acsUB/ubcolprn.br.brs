00010 ! Replace S:\acsUB\UBColPrn
00020 ! -- Cash Receipts Journal
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnlbl,fntxt,fntos,fnopenprn,fncloseprn,fnerror,fncno,fndat,fnchk,fnacs,fncmdset,fnmsgbox,fngethandle
00060 ! ______________________________________________________________________
00070   dim cnam$*40,dat$*20,scr1$(10)*30,alloc(10),nam$*30,route(200)
00080   dim r(20,4),hd1$*255,cap$*128,servicename$(10)*20,tg(11),resp$(7)*40
00081   dim ml$(3)*90
00090 ! ______________________________________________________________________
00100   fntop("S:\acsUB\UBColPrn",cap$="Cash Receipts Journal")
00110   fncno(cno,cnam$)
00120 ! skip_header=1 ! <--  this is really a developer only option.
00130   fndat(dat$,1)
00140   open #20: "Name="&env$('Q')&"\UBmstr\ubData\Service.h"&str$(cno)&",Shr",internal,input,relative 
00142   read #20,using "Form POS 1,10*C 20",rec=1: mat servicename$
00144   close #20: 
00150   gosub SCREEN1
00160   hd1$="{\ul  Account  }  {\ul    Total}    {\ul    Date   }"
00170   for j=1 to 10
00180     let x2=pos(trim$(servicename$(j))," ",1)
00182     if x2>0 then servicename$(j)=servicename$(j)(1:2)&"-"&servicename$(j)(x2+1:len(servicename$(j))) ! if service name two words long, use part of both
00190     if trim$(servicename$(j))<>"" then 
00192       scr1$(sz1+=1)=servicename$(j)
00194       hd1$=hd1$&"  {\ul "&lpad$(rtrm$(servicename$(j)(1:7)),7)&"}"
00196     end if 
00200   next j
00210   hd1$=hd1$&"  {\ul Customer Name               }"
00220   mat scr1$(sz1)
00230   mat alloc(sz1)
00240   open #h_customer:=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,input,keyed 
00250   open #h_trans:=2: "Name="&env$('Q')&"\UBmstr\UBTransVB.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\UBTrIndx.h"&str$(cno)&",Shr",internal,input,keyed 
00260 ! ______________________________________________________________________
00280   fnopenprn
00290   gosub HDR
00300 ! 
40000 MAIN_LOOP_TOP: ! 
40020   read #h_customer,using 'Form POS 1,C 10,POS 41,C 28,pos 1741,n 2',release: z$,nam$,extra1 eof PRTOTALS
40040   restore #h_trans,key>=z$&"         ": nokey MAIN_LOOP_TOP
40060 READ_TRANS: ! 
40080   read #h_trans,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1': p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof MAIN_LOOP_TOP
40100   if p$<>z$ then goto MAIN_LOOP_TOP
40120   if ld1<>0 and tdate<ld1 then goto READ_TRANS
40140   if hd1<>0 and tdate>hd1 then goto READ_TRANS
40160   if tamount=0 then goto READ_TRANS
40180   if tcode<3 or tcode>5 then goto READ_TRANS ! don't pr charges or penalties
40200   if tcode=3 then ti2=1 ! REG.COLLECTION
40220   if tcode=4 then ti2=2 ! CREDIT MEMO
40240   if tcode=5 then ti2=3 ! DEBIT MEMO
40260   if ti2=3 then r(1,1)-=tamount else r(1,1)+=tamount
40280   r(1,ti2+1)+=tamount
40300   let x=0
40320   for j=1 to 10
40340     if trim$(servicename$(j))<>"" then 
40360       alloc(x+=1)=tg(j)
40380       if ti2=3 then r(x+3,1)-=tg(j) else r(x+3,1)+=tg(j)
40400       r(x+3,ti2+1)+=tg(j)
40420     end if 
40440   next j
40460   c$=" "
40480   if tcode=4 then 
40500     c$="CM"
40520   else if tcode=5 then 
40540     c$="DM"
40560   end if 
40580   if ti1$="True" then 
40600     pr #255,using 'Form POS 1,C 10,N 10.2,C 4,PIC(ZZZZ/ZZ/ZZ),SZ1*N 9.2,X 3,C 30': z$,tamount,c$,tdate,mat alloc,nam$(1:25) pageoflow PGOF
40620   end if 
40640   if extra1<0 or extra1>200 then extra1=200
40660   if sum(alloc)<>tamount then 
40700     mat ml$(3)
40720     ml$(1)="The breakdown on a collection transaction dated "&str$(tdate)& " for customer "&z$
40740     ml$(2)="does not balance.  Your totals will be off by "& trim$(cnvrt$("pic($$$,$$$.## cr)",tamount-sum(alloc)))&"."
40750     ml$(3)="(transaction record number: "&str$(rec(h_trans))&')'
40760     fnmsgbox(mat ml$,resp$,cap$,49)
40780   end if 
40800   route(extra1)+=tamount
40820   if resp$="Cancel" then goto XIT
40840   goto READ_TRANS
40860 ! ______________________________________________________________________
64000 PGOF: ! r:
64020   pr #255: newpage
64040   gosub HDR
64060   continue  ! /r
66000 PRTOTALS: ! r:
66020   pr #255: ""
66040   pr #255: "    ************ Totals ************"
66060   pr #255: tab(34);"{\ul       Total}  {\ul    Reg.Col}  {\ul   Cr.Memos}  {\ul   Db.Memos}"
66080   for j=1 to sz1
66100     pr #255,using 'Form POS 4,C 30,N 11.2,3*N 12.2': scr1$(j),r(j+3,1),r(j+3,2),r(j+3,3),r(j+3,4) pageoflow PGOF
66120   next j
66140   pr #255: ""
66160   pr #255,using 'Form POS 4,C 30,N 11.2,3*N 12.2': "Total      ",r(1,1),r(1,2),r(1,3),r(1,4)
66180   if routetotals<>0 then 
66200     pr #255,using "form skip 2,c 20": "Route Totals"
66220     for j=1 to 200
66240       if route(j)<>0 then 
66260         pr #255,using "form pos 1,c 10,pic(zzz,zzz,zzz.##)": "Route "&cnvrt$("pic(zzz)",j),route(j) pageoflow PGOF
66280       end if 
66300     next j
66320   end if 
66330   fncloseprn
66340   goto XIT ! /r
66350 XIT: fnxit
66360 IGNORE: continue 
68000 SCREEN1: ! r:
68020   fntos(sn$="UBColPrn")
68040   mylen=33 : mypos=mylen+2
68060   fnlbl(1,1,"Report Heading Date:",mylen,1)
68080   fntxt(1,mypos,20)
68100   resp$(1)=dat$
68120   fnlbl(2,1,"Starting Date (blank for all):",mylen,1)
68140   fntxt(2,mypos,10,0,1,"3",0,"First day of the period to be printed. (ccyymmdd format)")
68160   resp$(2)=str$(ld1)
68180   fnlbl(3,1,"Ending Date (blank for all):",mylen,1)
68200   fntxt(3,mypos,10,0,1,"3",0,"Last day of the period to be printed. (ccyymmdd format)")
68220   resp$(3)=str$(hd1)
68240   fnchk(4,mypos,"Include Details:",1)
68260   resp$(4)="True"
68280   fnchk(5,mypos,"Show Totals by Route:",1)
68300   resp$(5)="False"
68320   fncmdset(3)
68340   fnacs(sn$,win,mat resp$,ck)
68360   if ck=5 then goto XIT
68380   dat$=resp$(1)
68400   ld1=val(resp$(2))
68420   hd1=val(resp$(3))
68440   ti1$=resp$(4)
68460   if resp$(5)="True" then routetotals=1
68480   fndat(dat$,2)
68500   return  ! /r
70000 HDR: ! r:
70010   if ~skip_header then 
70020 ! need date$,time$
70040     pr #255: "\qc  {\f181 \fs20 \b "&env$('cnam')&"}"
70060     pr #255: "\qc  {\f181 \fs28 \b "&env$('program_caption')&"}"
70080     if ld1<>0 and hd1<>0 then 
70100       pr #255: "\qc  {\f181 \fs18 \b From "&cnvrt$("pic(zzzz/zz/zz)",ld1)& "  To "&cnvrt$("pic(zzzz/zz/zz)",hd1)&"}"
70120     end if 
70140     pr #255: ""
70160     pr #255: "\ql "&hd1$
70162   end if 
70180   return  ! /r
72000 ! <Updateable Region: ERTN>
72020 ERTN: fnerror(program$,err,line,act$,"xit")
72040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
72060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
72080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
72100 ERTN_EXEC_ACT: execute act$ : goto ERTN
72120 ! /region
