00010 ! Replace S:\acsUB\ubmeans.br
00020 ! ______________________________________________________________________
00030   library 'S:\Core\Library': fntop,fnxit, fnacs,fnwait,fnopenprn,fncloseprn,fnerror,fnmsgbox,fntxt,fnlbl,fntos,fncno,fnxit,fncmdset,fntop
00040 ! ______________________________________________________________________
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cd1(8),rw(8,13),cap$*128,x(13),cnam$*40,txt$*60,message$(5)*80,tt$*80,message$*60,tg(11)
00080 ! ______________________________________________________________________
00090   let fncno(cno,cnam$) !:
        ! 
00100   let fntop("S:\acsUB\ubMeans",cap$="Median Sewer Charge")
00110 ! ______________________________________________________________________
00120   open #2: "Name="&env$('Q')&"\UBmstr\UBTransVB.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\UBTrIndx.h"&str$(cno)&",Shr",internal,input,keyed 
00130   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,outin,keyed 
00140   open #5: "Name="&env$('Q')&"\UBmstr\MEANs.h"&str$(cno)&",RecL=22,REPLACE",internal,output 
00150   read #1,using L490: x$,a2 eof DONE
00160   restore #2,key>=x$&"         ": nokey L230
00170 L170: read #2,using L540: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof L230
00180   if p$<>x$ then goto L230 !:
          ! history record must belong to this customer
00190   if tcode<>1 then goto L170 ! charge transaction
00200   let j=j+1 !:
        if j>8 then goto L230
00210   let resp$(j)=str$(tdate)
00220   goto L170
00230 L230: restore #1: 
00240 ! ______________________________________________________________________
00250 SCR1: ! 
00260   let sn$="Means-1" !:
        let fntos(sn$) !:
        let mylen=62 : let mypos=50
00270   let txt$="Billing Dates for Months to be Considered:" !:
        let fnlbl(1,1,txt$,mylen,1)
00280   for j=1 to 8 !:
          let fntxt(j+1,mypos,10,0,0,"3") !:
          let resp$(j)="" !:
        next j
00290   let fncmdset(2): let fnacs(sn$,0,mat resp$,ckey)
00300   if ckey=5 then goto XIT
00310   for j=1 to 8
00320 L320: let x=pos(resp$(j),"/",1)
00330     if x>0 then let resp$(j)(x:x)="": goto L320
00340   next j
00350   for j=1 to 8 !:
          let cd1(j)=val(resp$(j)) conv SCR1 !:
        next j
00360   if cd1(1)=0 then !:
          mat message$(1): let mytype=0 !:
          let message$(1)="You must enter at least one date!" !:
          let fnmsgbox(mat message$,resp$,cap$,mytype) !:
          goto SCR1
00370 ! ______________________________________________________________________
00380 SCR2: ! 
00390   let sn$="Means-2" !:
        let fntos(sn$)
00400   let txt$="Sewer code to analyze:" !:
        let fnlbl(1,1,txt$,22,1)
00410   let fntxt(1,24,2,2,0,"20") !:
        let resp$(1)=""
00420   let fncmdset(2): let fnacs(sn$,0,mat resp$,ckey)
00430   if ckey=5 then goto SCR1
00440   let sewcode=val(resp$(1)) conv SCR2
00450   if sewcode=0 then !:
          mat message$(1): let mytype=0 !:
          let message$(1)="You must enter at least one date!" !:
          let fnmsgbox(mat message$,resp$,cap$,mytype) !:
          goto SCR2
00460   let fnopenprn
00470 L470: read #1,using L490: x$,a2 eof DONE
00480   if a2<>sewcode then goto L470 ! only average certain rate codes
00490 L490: form pos 1,c 10,pos 145,pd 2,pos 1822,n 9
00500   let t1=t2=t3=x=0
00510   mat x=(0)
00520   restore #2,key>=x$&"         ": nokey L470
00530 L530: read #2,using L540: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof L620
00540 L540: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
00550   if p$<>x$ then goto L620
00560   if tcode<>1 then goto L530 ! only charge transactions
00570   for j1=1 to 8
00580     if cd1(j1)=tdate then let t1=t1+1: let t2=t2+wu !:
            let x=x+1: let x(x)=wu : goto L530
00590   next j1
00600   goto L530
00610 ! ______________________________________________________________________
00620 L620: if t1>0 then let t3=int(t2/t1) else let t3=0
00630   write #5,using "Form POS 1,C 10,N 12.2": x$,t3
00640   goto L470
00650 ! ______________________________________________________________________
00660 DONE: ! 
00670   gosub PRINT_REPORT
00680   let fncloseprn
00690 XIT: let fnxit
00700 ! ______________________________________________________________________
00710 ! ______________________________________________________________________
00720 ! <Updateable Region: ERTN>
00730 ERTN: let fnerror(program$,err,line,act$,"NO")
00740   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00750   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00760   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00770 ERTN_EXEC_ACT: execute act$ : goto ERTN
00780 ! /region
00790 ! ______________________________________________________________________
00800 PRINT_REPORT: ! 
00810   close #1: ioerr L820
00820 L820: close #2: ioerr L830
00830 L830: close #5: ioerr L840
00840 L840: execute "Index "&env$('Q')&"\UBmstr\MEANs.h"&str$(cno)&","&env$('Q')&"\UBmstr\MEANIDX.h"&str$(cno)&" 11,12,REPLACE,DupKeys -n"
00850   open #5: "Name="&env$('Q')&"\UBmstr\MEANs.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\MEANIDX.h"&str$(cno)&",Shr",internal,outin,keyed 
00860   gosub HEADER
00870   let means=int(lrec(5)/2)
00880 L880: read #5,using "Form POS 1,C 10,N 12.2": z$,t3 eof L940
00890   let x=x+1
00900   print #255,using L910: z$,t3 pageoflow L950
00910 L910: form pos 11,c 10,x 3,n 12.2,skip 1
00920   if means=x then print #255: "This should be the halfway point"
00930   goto L880
00940 L940: return 
00950 L950: print #255: newpage
00960   gosub HEADER
00970   continue 
00980 HEADER: ! 
00990   print #255,using L1000: date$,cnam$,time$,"Median Sewer Charge"
01000 L1000: form skip 2,pos 1,c 10,pos 20,cc 40,skip 1,pos 1,c 10,pos 20,cc 40,skip 2
01010   return 
