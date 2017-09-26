00010 ! calculates average charges for date range
00020 ! ______________________________________________________________________
00030   library 'S:\Core\Library': fntop,fnxit, fnacs,fnwait,fnopenprn,fncloseprn,fnerror,fnmsgbox,fntxt,fnlbl,fntos,fnxit,fncmdset,fntop
00040 ! ______________________________________________________________________
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cap$*128,txt$*60,message$(5)*80,tt$*80,message$*60,tg(11),ttg(11),e2$*30
00080 ! ______________________________________________________________________
00100   let fntop("S:\acsUB\ubSewer",cap$="Calculate Average Charges for Date Range")
00110 ! ______________________________________________________________________
00120   open #2: "Name="&env$('Q')&"\UBmstr\UBTransVB.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\UBTrIndx.h"&env$('cno')&",Shr",internal,input,keyed 
00130   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,outin,keyed 
00140   gosub BLDHDR
00150 SCR1: ! 
00160   let sn$="ubsewer-1" !:
        let fntos(sn$)
00170   let txt$="Billing Dates for Months to be Averaged:" !:
        let mylen=len(txt$)+4: let fnlbl(2,5,txt$,mylen,0)
00180   let mylen=12
00190   let txt$="Date From: " !:
        let fnlbl(3,6,txt$,mylen,1)
00200   let txt$="Date To: " !:
        let fnlbl(4,6,txt$,mylen,1)
00210   for j=1 to 2 !:
          let fntxt(j+2,20,8,0,0,"3") !:
          let resp$(j)="" !:
        next j
00220   let fncmdset(2): let fnacs(sn$,0,mat resp$,ckey)
00230   if ckey=5 then goto XIT
00240   for j=1 to 8
00250 L250: let x=pos(resp$(j),"/",1)
00260     if x>0 then let resp$(j)(x:x)="": goto L250
00270   next j
00280   let sd1=val(resp$(1)) : let sd2=val(resp$(2))
00290   if sd1=0 or sd2=0 or sd2<sd1 then goto SCR1
00300 ! ______________________________________________________________________
00310   let fnopenprn
00320   let message$="Calculating: please wait..." !:
        let fnwait(0,cap$,message$,1)
00330   gosub HDR
00340 L340: read #1,using L350: x$,e2$,oldavg eof DONE
00350 L350: form pos 1,c 10,x 30,c 30,pos 1822,n 9
00360   restore #2,key>=x$&"         ": nokey L450
00370 L370: read #2,using L380: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof L450
00380 L380: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
00390   if p$<>x$ then goto L450 ! check account
00400   if tcode<>1 then goto L370 ! charge transaction
00410   if tdate<sd1 or tdate>sd2 then goto L370 ! check date range
00420   let ttg=ttg+1
00430   mat ttg=ttg+tg
00440   goto L370
00450 L450: if ttg=0 then goto L340 ! no transactions in date range
00460   mat g1=(0)
00470   let j2=0
00480   for j=1 to 9
00490     if trim$(servicename$(j))="" then goto L520
00500     let j2=j2+1
00510     let g1(j2)=ttg(j)/ttg
00520 L520: next j
00530   let g1(sz1)=ttg(11)/ttg
00540   print #255,using L550: x$,e2$(1:24),mat g1 pageoflow NEWPGE
00550 L550: form pos 1,c 11,c 24,sz1*n 9.2,skip 1
00560   let ttg=0 : mat ttg=(0)
00570   let tg2=tg2+1 : mat g2=g2+g1
00580   goto L340
00590 DONE: ! 
00600   print #255: 
00610 L610: form pos 5,c 20,pic(zzz,zzz,zzz.##cr),skip 1
00620   print #255,using L610: "Total Customers",tg2
00630   let j2=0
00640   for j=1 to 9
00650     if trim$(servicename$(j))="" then goto L680
00660     let j2=j2+1
00670     print #255,using L610: servicename$(j),g2(j2)
00680 L680: next j
00690   print #255,using L610: "Total",g2(sz1)
00700   close #1: 
00710   let fncloseprn
00720 XIT: let fnxit
00730 ! ______________________________________________________________________
00740 NEWPGE: print #255: newpage
00750   gosub HDR
00760 continue 
00770 ! ______________________________________________________________________
00780 HDR: ! r:
00790   let p1=p1+1
00800   print #255,using "Form POS 20,CC 40,POS 70,C 5,N 4": env$('cnam'),"Page ",p1
00810   print #255,using "Form POS 20,C 23,pic(####/##/##),c 6,pic(####/##/##)": "Average Charges From:",sd1,"  To:",sd2
00820   print #255: ""
00830   print #255: hd1$
00840   print #255: hd2$
00850 return ! /r
00860 ! ______________________________________________________________________
00870 ! <Updateable Region: ERTN>
00880 ERTN: let fnerror(program$,err,line,act$,"NO")
00890   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00900   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00910   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00920 ERTN_EXEC_ACT: execute act$ : goto ERTN
00930 ! /region
00940 ! ______________________________________________________________________
00950   dim hd1$*400,hd2$*400,g1(11),g2(11)
00960   dim servicename$(10)*20,services$(10)*2,tax_code$(10)*1,tg(11),usages(3)
00970 BLDHDR: ! r: build print headings
00980   open #20: "Name="&env$('Q')&"\UBmstr\ubData\Service.h"&env$('cno')&",Shr",internal,input,relative  !:
        read #20,using "Form POS 1,10*C 20,10*C 2,10*C 1,10*c 1",rec=1: mat servicename$,mat service$,mat tax_code$,mat penalty$ !:
        close #20: 
00990   let hd1$="Account                             " !:
        let hd2$="{\ul Number   }  {\ul Name                   }  "
01000   for j=1 to 9 ! skip penalty
01010     let x2=pos(trim$(servicename$(j))," ",1) !:
          if x2>0 then let servicename$(j)=servicename$(j)(1:2)&"-"&servicename$(j)(x2+1:len(servicename$(j)))
01020     if trim$(servicename$(j))<>"" then !:
            let x1=pos (servicename$(j)," ",1) !:
            let x1=min(x1,7) !:
            let hd1$=hd1$&"---------" !:
            let hd2$=hd2$&"{\ul "&lpad$(trim$(servicename$(j)(1:x1)),8)&"} " !:
            let sz1=sz1+1
01030   next j
01040   let sz1=sz1+1
01050   let hd2$=hd2$&"{\ul    TOTAL} "
01060   mat g1(sz1)
01070   mat g2(sz1)
01080 return ! /r
