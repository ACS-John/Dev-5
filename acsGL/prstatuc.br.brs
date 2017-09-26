00010 ! Replace S:\acsGL\PRSTATUC
00020 ! Quarterly UC Report (From the after-the-fact payroll files in gl)
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fncno,fnerror,fnpedat$,fnprocess,fntos,fnlbl,fntxt,fnacs,fncmdset
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim k(1),k$(3)*25,l$(1)*11,d(14),m(36),n(2),cap$*128
00080   dim fa$(3),sa$(3)*40,cnam$*40
00090   dim a$(3)*40,b$(2)*12,c$*5,e(2),e$(2)*11,pedat$*20
00100 ! ______________________________________________________________________
00110   let fntop(program$,cap$="Print State UC Report")
00120   let fncno(cno,cnam$)
00140   open #1: "Name="&env$('Q')&"\GLmstr\Company.h"&str$(cno)&",Shr",internal,input 
00150   read #1,using 'Form POS 1,3*C 40,2*C 12,C 5,POS 188,PD 7.2,POS 658,10*N 1': mat a$,mat b$,c$,ucm,mat deduc
00160   close #1: 
00170   if fnprocess=1 then goto L240
00180 ! 
00190   let fntos(sn$="Prstatuc")
00200   let mylen=35: let mypos=mylen+3 : let right=1
00210   let fnlbl(1,1,"Quarterly Period Ending Date:",mylen,right)
00220   let fntxt(1,mypos,20,0,left,"",0,"Enter the last day of the quarter.",0 )
00230   let resp$(1)=""
00240   let fncmdset(2)
00250   let fnacs(sn$,0,mat resp$,ckey)
00260   if ckey=5 then goto XIT
00270   let pedat$=resp$(1)
00275 L240: open #2: "Name="&env$('Q')&"\GLmstr\PRmstr.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\PRIndex.h"&str$(cno)&",Shr",internal,input,keyed 
00280   let fnopenprn
00285   gosub HDR
00340 L340: read #2,using L350: mat k,mat k$,mat l$,mat m eof L720
00350 L350: form pos 1,n 4,3*c 25,c 11,36*pd 5.2,2*n 5
00360   if m(2)=0 or k(1)=0 then goto L530
00370   let deducy=deducq=0
00380   for j=1 to 10
00390     if deduc(j)=1 then let deducy=deducy+m(j*2+9)
00400     if deduc(j)=1 then let deducq=deducq+m(j*2+10)
00410   next j
00420   let m(1)=m(1)-deducy
00430   let m(2)=m(2)-deducq
00440   if p1<57 then goto L480
00450   gosub L920
00460   print #255: newpage
00470   gosub HDR
00480 L480: gosub L790
00490   let t1=t1+m(2)
00500   let t2=t2+h3
00510   let t3=t3+h2
00520   let t4=t4+m(34)
00530 L530: goto L340
00540 ! ______________________________________________________________________
00550 HDR: ! 
00560   print #255,using L570: "PAGE ",p2+=1
00570 L570: form pos 70,c 5,pic(zzz),skip 2
00580   print #255,using L590: "SCHEDULE A - EMPLOYER'S REPORT OF WAGE'S PAID TO EACH EMPLOYEE"
00590 L590: form pos 10,c 63
00600   print #255,using L610: "AS OF ",fnpedat$,"FED ID",b$(1)
00610 L610: form pos 26,c 6,c 20,pos 65,c 6,pos 75,c 40
00620   print #255,using L630: "RATE",a$(1),"STATE ID",b$(2)
00630 L630: form pos 6,c 9,pos 22,c 40,pos 65,c 8,pos 75,c 12
00640   print #255,using L650: c$,a$(2),a$(3)
00650 L650: form pos 6,c 5,pos 22,c 40,skip 1,pos 22,c 40,skip 2
00660   print #255: tab(39);"TOTAL WAGES  EXCESS WAGES    TAXABLE   WEEKS"
00670   print #255,using L680: "SS NUMBER   NAME                      FOR QUARTER   OVER  $"&ltrm$(str$(ucm)),"WAGES   WORKED"
00680 L680: form pos 1,c 68,pos 69,c 18
00690   let p1=16
00700   return 
00710 ! ______________________________________________________________________
00720 L720: gosub L920
00730   close #2: 
00740   let fncloseprn
00750   goto XIT
00760 ! ______________________________________________________________________
00770 XIT: let fnxit
00780 ! ______________________________________________________________________
00790 L790: let p3=p3+1
00800   if m(1)<ucm then goto L860
00810   if m(1)-m(2)>ucm then goto L840
00820   let h2=ucm-(m(1)-m(2))
00830   goto L870
00840 L840: let h2=0
00850   goto L870
00860 L860: let h2=m(2)
00870 L870: let h3=m(2)-h2
00880   print #255,using L890: l$(1),k$(1),m(2),h3,h2,m(34)
00890 L890: form pos 1,c 11,pos 13,c 25,pos 39,n 11.2,pos 53,n 11.2,n 11.2,n 8,skip 2
00900   let p1=p1+2
00910   return 
00920 L920: let j1=58-p1
00930   print #255,using L940: "----------   -----------  ---------  ------"
00940 L940: form skip j1,pos 40,c 48
00950   print #255,using L960: "EMPLOYEES ON THIS PAGE "&ltrm$(str$(p3))&"    PAGE TOTALS",t1,t2,t3,t4
00960 L960: form pos 1,c 42,pos 39,n 11.2,pos 53,n 11.2,n 11.2,n 8.2
00970   let p3=0
00980   let t1=0
00990   let t2=0
01000   let t3=0
01010   let t4=0
01020   return 
01030 ! ______________________________________________________________________
01040 ! <Updateable Region: ERTN>
01050 ERTN: let fnerror(program$,err,line,act$,"xit")
01060   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
01070   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01080   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
01090 ERTN_EXEC_ACT: execute act$ : goto ERTN
01100 ! /region
01110 ! ______________________________________________________________________
