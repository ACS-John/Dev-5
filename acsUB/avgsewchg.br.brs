00010 ! calculates average sewer charges
00011 ! calculates average water usage for selected date range
00012 ! uses the calculated water usage to calculate and store a  standard sewer charge
00020 ! ______________________________________________________________________
00030   library 'S:\Core\Library': fntop,fnxit, fnacs,fnwait,fnopenprn,fncloseprn,fnerror,fnmsgbox,fntxt,fnlbl,fntos,fnxit,fncmdset,fntop
00050   on error goto ERTN
00070   dim cap$*128,txt$*60,message$(5)*80,tt$*80,message$*60,tg(11),ttg(11),e2$*30,rt(10,3)
00100   let fntop(program$,cap$="Set Sewer Standard Charges from Average Water Usage")
00102   open #ratemst:=8: "Name="&env$('Q')&"\UBmstr\ubData\RateMst.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubData\RateIdx1.h"&env$('cno')&",Shr",internal,input,keyed 
00120   open #2: "Name="&env$('Q')&"\UBmstr\UBTransVB.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\UBTrIndx.h"&str$(cno)&",Shr",internal,input,keyed 
00130   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,outin,keyed 
00142   mat resp$=("")
00150 SCR1: ! 
00160   let sn$="avgsewchg-1" !:
        let fntos(sn$)
00170   let txt$="Average consumption for billing dates within range" !:
        let mylen=len(txt$)+4: let fnlbl(1,5,txt$,mylen,0)
00172   let txt$="Charge will be moved into Sewer Standard charge" !:
        let mylen=len(txt$)+4: let fnlbl(2,5,txt$,mylen,0)
00180   let mylen=12
00190   let txt$="Date From: " !:
        let fnlbl(3,6,txt$,mylen,1)
00200   let txt$="Date To: " !:
        let fnlbl(4,6,txt$,mylen,1)
00202   let fnlbl(5,4,"Sewer Rate Code: ",16,1)
00210   for j=1 to 2 !:
          let fntxt(j+2,20,8,0,0,"3") !:
        next j
00212   let fntxt(5,22,2,0,0,"30")
00220   let fncmdset(2): let fnacs(sn$,0,mat resp$,ckey)
00230   if ckey=5 then goto XIT
00240   for j=1 to 8
00250 L250: let x=pos(resp$(j),"/",1)
00260     if x>0 then let resp$(j)(x:x)="": goto L250
00270   next j
00280   let sd1=val(resp$(1)) : let sd2=val(resp$(2))
00290   if sd1=0 or sd2=0 or sd2<sd1 then goto SCR1
00292   let swcde=val(resp$(3))
00296   read #ratemst,using L298,key="SW"&lpad$(str$(swcde),2): mc1,mu1,mat rt nokey SCR1
00298 L298: form pos 55,32*g 10
00299   close #ratemst: 
00300 ! ______________________________________________________________________
00310   let fnopenprn
00330   gosub HDR
00340 L340: read #1,using L350: x$,e2$,a2,oldavg,extra14,fbc eof DONE
00350 L350: form pos 1,c 10,x 30,c 30,pos 145,pd 2,pos 1822,n 9,pos 1880,n 3,pos 1818,n 3
00352   if a2><swcde then goto L340
00354   if fbc>0 then goto L340
00360   restore #2,key>=x$&"         ": nokey L450
00370 L370: read #2,using L380: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof L450
00380 L380: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
00390   if p$<>x$ then goto L450 ! check account
00400   if tcode<>1 then goto L370 ! charge transaction
00410   if tdate<sd1 or tdate>sd2 then goto L370 ! check date range
00420   let ttg=ttg+1
00430   mat ttg=ttg+tg
00432   let twu=twu+wu ! total water used
00440   goto L370
00450 L450: if ttg=0 then goto L340 ! no transactions in date range
00460   mat g1=(0)
00470   let j2=0
00480   for j=1 to 9
00490 ! if trim$(servicename$(j))="" then goto L520
00500     let j2=j2+1
00510     let g1(j2)=ttg(j)/ttg
00520 L520: next j
00530 ! let g1(sz1)=ttg(11)/ttg
00532   let su1=twu/ttg
00534   gosub SEWER_CALK
00540   pr #255,using L550: x$,e2$(1:24),g1(1),g1(2),su1,swchg pageoflow NEWPGE
00550 L550: form pos 1,c 11,c 24,2*n 9.2,n 9,n 9.2,skip 1
00560   let ttg=0 : mat ttg=(0)
00562   let twu=0
00570   let tg2=tg2+1 : mat g2=g2+g1
00580   goto L340
00590 DONE: ! 
00600   pr #255: 
00610 L610: form pos 5,c 20,n 9
00620   pr #255,using L610: "Total Customers",tg2
00630   let j2=0
00640   for j=1 to 9
00650     if trim$(servicename$(j))<>"" then 
00660       let j2=j2+1
00670       pr #255,using L610: servicename$(j),g2(j2)
00672     end if  ! trim$(servicename$(j))<>""
00680   next j
00690 ! pr #255,using L610: "Total",g2(sz1)
00700   close #1: 
00710   let fncloseprn
00720 XIT: let fnxit
00730 ! ______________________________________________________________________
00740 NEWPGE: ! 
00742   pr #255: newpage
00750   gosub HDR
00760   continue 
00770 ! ______________________________________________________________________
00780 HDR: ! 
00790   let p1=p1+1
00800   pr #255,using "Form POS 20,CC 40,POS 70,C 5,N 4": env$('cnam'),"Page ",p1
00810   pr #255,using "Form POS 20,C 23,pic(####/##/##),c 6,pic(####/##/##)": "Average Charges From:",sd1,"  To:",sd2
00820   pr #255: "                                    <--------AVERAGE--------->    NEW  "
00830   pr #255: "Account    Customer Name             water    sewer    usage    std chg"
00840   pr #255: "__________ ________________________ ________ ________ ________ ________"
00850   return 
00860 ! ______________________________________________________________________
00870 ! <Updateable Region: ERTN>
00880 ERTN: let fnerror(program$,err,line,act$,"NO")
00890   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00900   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00910   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00920 ERTN_EXEC_ACT: execute act$ : goto ERTN
00930 ! /region
00940 ! ______________________________________________________________________
00950   dim hd1$*400,hd2$*400,g1(11),g2(11)
00960   dim servicename$(10)*20,services$(10)*2,tax_code$(10)*1,tg(11),usages(3)
01090 SEWER_CALK: ! calculate standard sewer charge
01100   let swchg=mc1*max(1,extra14) ! units per meter - sewer (default to one)
01120   if su1<=mu1 then goto SEWER_COMPLETED else let mu2=mu1
01150   for j=1 to 10
01160     if rt(j,1)>su1 then goto SEWER_COMPLETED
01180     if su1<rt(j,2) then let w1=su1-mu2 else let w1=rt(j,2)-mu2
01200     let swchg=swchg+round(w1*rt(j,3),2)
01220     if rt(j,2)>su1 then goto SEWER_COMPLETED
01240     let mu2=rt(j,2)
01260   next j
01280 SEWER_COMPLETED: ! 
01282   rewrite #1,using L1284,key=x$: swchg
01284 L1284: form pos 161,pd 4.2
01286   return 
