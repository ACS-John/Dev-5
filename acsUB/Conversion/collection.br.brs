00010 ! Replace S:\acsUB\conversion\collection
00020 ! try converting colletion for 402 when they do not have allocations on them in the transaction history.
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnopenprn,fncloseprn,fnerror,fncno,fndat,fnxit,fntop,fnindex_it
00050   fntop("S:\acsUB\conversion\collection",cap$="Convert Collections")
00060 ! ______________________________________________________________________
00070   dim cnam$*40,dat$*20,a$(61)*30,u(61),scr1$(10)*30,alloc(10),nam$*30,o(2)
00080   dim r(20,4),hd1$*190,hd2$*190,cap$*128,message$*40
00090 ! ______________________________________________________________________
00100   fncno(cno,cnam$)
00110 ! 
00120   cap$="Convert Collections"
00130 ! ______________________________________________________________________
00140   def fndate_mmddyy_to_ccyymmdd(x)
00150     let x2=(x-int(x*.01)*100)*10000+int(x*.01)
00160     if int(x2*.0001)<90 then let x2=x2+20000000 else let x2=x2+19000000
00170     fndate_mmddyy_to_ccyymmdd=x2
00180   fnend 
00190 ! ______________________________________________________________________
00210   open #6: "Name="&env$('Q')&"\UBmstr\Collect.h"&str$(cno),internal,outin,relative 
00220   open #2: "Name="&env$('Q')&"\UBmstr\UBTransVB.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\UBTrIndx.h"&str$(cno)&",Shr",internal,outin,keyed 
00230 L230: ! 
00240 L240: read #6,using L280: x$,m,n,mat o,adrnxt,rcpt$,mat alloc eof L440
00250   for j=1 to udim(alloc)
00260     if alloc(j)<-20202 then alloc(j)=0
00270   next j
00280 L280: form pos 1,c 10,pd 4.2,pd 4,2*n 1,pd 3,c 9,10*pd 4.2
00310   if m=0 then goto L230
00320   if o(1)<1 or o(1)>4 then o(1)=1
00330   if o(1)=3 then let ti2=1 : let tcode=3 ! REG.COLLECTION
00340   if o(1)=4 then let ti2=2 : let tcode=5 ! CREDIT MEMO
00350   if o(1)=1 and o(2)=4 then let ti2=3 : let tocde=6 ! DEBIT MEMO
00360   let tdate=fndate_mmddyy_to_ccyymmdd(n)
00370   let tamount=m
00380   for j=1 to 10 : let tg(j)=alloc(j): next j
00390   read #2,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1',key=x$&cnvrt$("pic(########)",tdate)&str$(tcode): p$ nokey L420
00400   rewrite #2,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1',key=x$&cnvrt$("pic(########)",tdate)&str$(tcode): x$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode
00410   goto L430
00420 L420: write #2,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1': x$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode
00430 L430: goto L240
00440 L440: close #2: 
00450   fnindex_it(env$('Q')&"\UBmstr\UBTransvb.h"&str$(cno),env$('Q')&"\UBmstr\UBTrindx.h"&str$(cno),"1 19")
00460 XIT: let fnxit
