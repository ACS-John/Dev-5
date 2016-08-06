00010 ! Replace R:\acsGL\acglBalY
00020 ! G/L BALANCE SHEET with comparison on months
00030 ! ______________________________________________________________________
00040   library 'R:\Core\Library': fnxit,fntop, fnopenprn,fncloseprn,fnpglen,fncno,fnerror,fnprocess,fngl_number_use_dept,fnps,fnpedat$,fnactpd,fnpriorcd,fnglfs,fnactpd$,fnfscode
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim fl1$*256,pedat$*20,m1$(13)*9,m2$(13)*8,total(13),p$(20)*50
00080   dim cch$*20,by(13),bp(13),sc1$(2)*20,udf$*256,cap$*128
00090   dim cnam$*40,b$*3,a$(8)*30,oldtrans$*16,g(8),accum(9,13)
00100   dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*12
00110 ! ______________________________________________________________________
00120   let fntop(program$,cap$="Period Comparison Balance Sheet")
00130   let fncno(cno,cnam$)
00135   let actpd$=fnactpd$ !:
        let actpd=fnactpd
00140   let udf$=env$('temp')&'\'
00142   let fnfscode
00143   let fnpriorcd
00150   if fnglfs=5 then goto XIT !:
          ! sets fnps,fnpriorcd,fnfscode (primary/secondary,current year/Prior,             period to print)
00152   let fnfscode
00153   let fnpriorcd
00160   open #1: "Name=Q:\GLmstr\Company.h"&str$(cno)&",Shr",internal,input,relative: read #1,using "Form pos 384,N 2",rec=1: nap : close #1: 
00170   open #1: "Name=Q:\GLmstr\Company.h"&str$(cno)&",Shr",internal,input,relative: read #1,using "Form pos 296,N 2",rec=1: lmu : close #1: 
00180   let m1$(1)="  January" : let m1$(2)=" February" !:
        let m1$(3)="    March" : let m1$(4)="    April" !:
        let m1$(5)="      May" : let m1$(6)="     June" !:
        let m1$(7)="     July" : let m1$(8)="   August" !:
        let m1$(9)="September" : let m1$(10)="  October" !:
        let m1$(11)=" November" : let m1$(12)=" December" !:
        let m1$(13)=""
00190   let m2$(1)="     One" : let m2$(2)="     Two" : let m2$(3)="   Three" !:
        let m2$(4)="    Four" : let m2$(5)="    Five": let m2$(6)="     Six" !:
        let m2$(7)="   Seven": let m2$(8)="   Eight": let m2$(9)="    Nine" !:
        let m2$(10)="     Ten": let m2$(11)="  Eleven": let m2$(12)="  Twelve" !:
        let m2$(13)="Thirteen"
00200   let mp1=63
00210   if fnps=2 then let mp1=mp1+3
00220   let fl1$="Name=Q:\GLmstr\ACGLFNSB.h"&str$(cno)&",KFName=Q:\GLmstr\FnSBIndx.h"&str$(cno)&",Shr"
00230   if fnps=2 then let fl1$="Name=Q:\GLmstr\AcGLFnSc.h"&str$(cno)&",KFName=Q:\GLmstr\FnScIndx.h"&str$(cno)&",Shr"
00240   open #1: fl1$,internal,input,keyed 
00250   if fnprocess=1 or fngl_number_use_dept=0 then goto L330
00260   goto L370 ! Print NEWPAGE
00270   close #101: ioerr L280
00280 L280: open #101: "SROW=9,SCOL=4,EROW=12,ECOL=75,BORDER=DR,CAPTION=PRINT BALANCE SHEET",display,outin 
00290   print fields "13,32,C 16,R,N": "Press F5 to stop"
00300   print fields "10,5,c 70,n": "ENTER THE COST CENTER OR DEPT # IF YOU WISH TO ONLY PRINT A STATEMENT"
00310 L310: print fields "11,5,c 65,n": "ON ONE DEPARTMENT; ELSE ENTER 0 TO PRINT ALL DEPARTMENTS"
00320   input fields "11,70,N 3,eu,N": costcntr conv L310
00330 L330: ! Print NEWPAGE
00340   print fields "10,10,Cc 60,n": " BALANCE SHEET IN PROCESS"
00350   print fields "12,34,C 11,B,5": "Cancel (F5)"
00360   on fkey 5 goto L2120
00370 L370: let fnopenprn !:
        if file$(255)(1:4)<>"PRN:" then let redir=1 else let redir=0
00380   if fnps=2 then goto L410 ! secondary
00390   execute "Index Q:\GLmstr\GLmstr.h"&str$(cno)&" "&udf$&"fsindex.H"&str$(cno)&" 63 3 Replace DupKeys -N"
00400   goto L420
00410 L410: execute "Index Q:\GLmstr\GLmstr.h"&str$(cno)&" "&udf$&"fsindex.H"&str$(cno)&" 66 3 Replace DupKeys -N"
00420 L420: open #3: "Name=Q:\GLmstr\GLmstr.h"&str$(cno)&",KFName="&udf$&"fsindex.h"&str$(cno)&",Shr",internal,input,keyed 
00430   let report$=cap$
00440 L440: read #1,using L480: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof L2120
00450   if ltrm$(r$)="" or ltrm$(r$)="0" then goto L440
00460   if costcntr=0 then goto L480
00470   if costcntr><fc then goto L440
00480 L480: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
00490   if te$="S" or te$="F" then goto L510
00500   if heading=0 and te$><"R" then gosub L1970
00510 L510: on pos ("RFHDTSPE",te$,1) goto L1380,L1430,L520,L580,L1080,L1380,L1080,L580 none L440 ! 8/4/88
00520 L520: print #255,using L530: d$
00530 L530: form pos sp,c 50,skip 1
00540   gosub L1600
00550   gosub L1520
00560   goto L440
00570 ! ______________________________________________________________________
00580 L580: if notrans=1 then goto L820 ! 8/4/88
00590   if br>=val(r$) and val(r$)><0 then goto L630
00600 L600: ! READ GENERAL LEDGER MASTER FILE FOR AMOUNTS
00610 L610: read #3,using 'Form POS MP1,PD 3,POS 87,27*PD 6.2': br,cb,mat by,mat bp eof L810
00620   if br=0 then goto L610 ! SKIP IF NO REFERENCE #
00630 L630: if br=val(r$) then goto L640 else goto L790
00640 L640: if fnpriorcd=2 then goto L770
00650   for j=1 to 13
00660     if j=1 and actpd=1 then let total(j)+=cb else goto L680
00670     goto L740 ! 2/15/89
00680 L680: if j=1 then let total(j)=total(j)+by(j) else goto L700
00690     goto L740
00700 L700: if j>nap then goto L740 ! 7/21/88
00710     if j<=lmu then let total(j)=total(j)+by(j) else goto L730
00720     goto L740
00730 L730: if actpd<>lmu and j=actpd then !:
            let total(j)+=cb
00740 L740: next j
00750   goto L600
00760 ! ______________________________________________________________________
00770 L770: for j=1 to 13 : let total(j)+=bp(j) : next j
00780   goto L600
00790 L790: if br<val(r$) then goto L600
00800   if br>val(r$) then goto L860
00810 L810: let notrans=1
00820 L820: if te$="E" then goto L830 else goto L860 ! 8/4/88
00830 L830: for k=1 to 13 ! 8/4/88
00840     let total(k)=-accum(ap,k) ! 8/4/88
00850   next k ! 8/4/88
00860 L860: for j=1 to 9
00870     if ac(j)<>9 then !:
            for k=1 to 13 : let accum(j,k)=accum(j,k)+total(k) : next k
00880   next j
00890   for j=1 to 13
00900     if rs=1 then let total(j)=-total(j)
00910   next j
00920   if ds=1 then let dollar$="$" else let dollar$=" "
00930   let dollar=27+14*bc
00940   goto L960
00950   if ls+ul+ds+ic>0 then goto L960 else goto L440
00960 L960: let sp2=dollar-sp-1
00970   if nap=13 then goto L1010
00975   if ul=1 then print #255,using L991: d$(1:sp2),"{\ul ",total(1),"}","{\ul ",total(2),"}","{\ul ",total(3),"}","{\ul ",total(4),"}","{\ul ",total(5),"}","{\ul ",total(6),"}","{\ul ",total(7),"}","{\ul ",total(8),"}","{\ul ",total(9),"}","{\ul ",total(10),"}","{\ul ",total(11),"}","{\ul",total(12),"}" pageoflow L1760 : goto L990
00980   print #255,using L990: d$(1:sp2),total(1),total(2),total(3),total(4),total(5),total(6),total(7),total(8),total(9),total(10),total(11),total(12) pageoflow L1760
00990 L990: form pos sp,c sp2,pos 39,12*n 12.2,skip redir
00991 L991: form pos sp,c sp2,pos 39,c 5,n 12.2,c 1,c 5,n 12.2,c 1,c 5,n 12.2,c 1,c 5,n 12.2,c 1,c 5,n 12.2,c 1,c 5,n 12.2,c 1,c 5,n 12.2,c 1,c 5,n 12.2,c 1,c 5,n 12.2,c 1,c 5,n 12.2,c 1,c 5,n 12.2,c 1,c 5,n 12.2,c 1,skip redir
01000   goto L1030
01010 L1010: print #255,using L1020: d$(1:sp2),mat total pageoflow L1760
01020 L1020: form pos sp,c sp2,pos 39,13*n 12.2,skip redir
01030 L1030: mat total=(0) ! 6/03/88
01040   gosub L1520
01045   if ul=1 then goto L1060
01050   gosub L1780
01060 L1060: gosub L1600
01070   goto L440
01080 L1080: if ap=0 then let ap=1
01090   let dollar=27+14*bc
01100   let sp2=dollar-sp-1
01110   if rs=1 then let accum1=-accum(ap,1) else let accum1=accum(ap,1)
01120   if rs=1 then let accum2=-accum(ap,2) else let accum2=accum(ap,2)
01130   if rs=1 then let accum3=-accum(ap,3) else let accum3=accum(ap,3)
01140   if rs=1 then let accum4=-accum(ap,4) else let accum4=accum(ap,4)
01150   if rs=1 then let accum5=-accum(ap,5) else let accum5=accum(ap,5)
01160   if rs=1 then let accum6=-accum(ap,6) else let accum6=accum(ap,6)
01170   if rs=1 then let accum7=-accum(ap,7) else let accum7=accum(ap,7)
01180   if rs=1 then let accum8=-accum(ap,8) else let accum8=accum(ap,8)
01190   if rs=1 then let accum9=-accum(ap,9) else let accum9=accum(ap,9)
01200   if rs=1 then let accum10=-accum(ap,10) else let accum10=accum(ap,10)
01210   if rs=1 then let accum11=-accum(ap,11) else let accum11=accum(ap,11)
01220   if rs=1 then let accum12=-accum(ap,12) else let accum12=accum(ap,12)
01230   if rs=1 then let accum13=-accum(ap,13) else let accum13=accum(ap,13)
01240   if nap=13 then goto L1270
01245   if ul=1 then print #255,using L991: d$(1:sp2),"{\ul ",accum1,"}","{\ul ",m2,"}","{\ul ",m3,"}","{\ul ",m4,"}","{\ul ",m5,"}","{\ul ",m6,"}","{\ul ",m7,"}","{\ul ",m8,"}","{\ul ",um9,"}","{\ul ",accum10,"}","{\ul ",accum11,"}","{\ul ",accum12,"}" pageoflow L1760 : goto L1260
01250   print #255,using L990: d$(1:sp2),accum1,accum2,accum3,accum4,accum5,accum6,accum7,accum8,accum9,accum10,accum11,accum12 pageoflow L1760
01260 L1260: goto L1280
01270 L1270: print #255,using L1020: d$(1:sp2),accum1,accum2,accum3,accum4,accum5,accum6,accum7,accum8,accum9,accum10,accum11,accum12,accum13 pageoflow L1760
01280 L1280: gosub L1520
01285   if ul=1 then goto L1300
01290   gosub L1780
01300 L1300: gosub L1600
01310   if te$><"P" then goto L1370
01320   for j=1 to 9
01330     for k=1 to 13
01340       let accum(j,k)=accum(j,k)-accum(ap,k)
01350     next k
01360   next j
01370 L1370: goto L440
01380 L1380: if te$="R" then let report$=d$
01390   if te$="S" then let secondr$=d$
01400   gosub L1600
01410   goto L440
01420 ! ______________________________________________________________________
01430 L1430: if foot1=1 then goto L1490
01440   let tabnote=sp
01450   let foot1=1
01460   let foot$=d$
01470   goto L440
01480 ! ______________________________________________________________________
01490 L1490: let foot$=rtrm$(foot$)&d$
01500   goto L440
01510 ! ______________________________________________________________________
01520 L1520: for j=1 to 9
01530     if ac(j)=0 or ac(j)=9 then goto L1570 ! 10/14/87
01540     for k=1 to 13
01550       let accum(j,k)=0
01560     next k
01570 L1570: next j
01580   return 
01590 ! ______________________________________________________________________
01600 L1600: if ls=0 then goto L1740
01610   if ls=99 then goto L1650
01620   print #255,using L1630: " "
01630 L1630: form pos 1,c 1,skip ls
01640   goto L1740
01650 L1650: let fnpglen(pglen)
01660   if pglen<>42 then let pglen=58
01670   let sk=pglen-krec(255): let fl=len(rtrm$(foot$))
01680   if pglen=42 then let sk=sk+1
01690   print #255,using L1700: rtrm$(foot$)
01700 L1700: form skip sk,pos tabnote,c fl,skip 1
01710   if eofcode=1 then goto L1740
01720   print #255: newpage
01730   gosub L1970
01740 L1740: return 
01750 ! ______________________________________________________________________
01760 L1760: gosub L1650: continue 
01770 ! ______________________________________________________________________
01780 L1780: if ul=0 then goto L1930
01790   if ul=1 then goto L1870
01800   let underlin$=" ==========="
01810   if nap=13 then goto L1840
01820   print #255,using L1850: underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,""
01830   goto L1930
01840 L1840: print #255,using L1850: underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$
01850 L1850: form pos 39,13*c 12,skip redir
01860   goto L1930
01870 L1870: let underlin$=" ___________"
01880   if nap=13 then goto L1910
01890   print #255,using L1920: underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,""
01900   goto L1930
01910 L1910: print #255,using L1920: underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$
01920 L1920: form skip 0,pos 39,13*c 12,skip redir
01930 L1930: if redir=0 then print #255,using L1940: " "
01940 L1940: form skip 1,c 1,skip 0
01950   return 
01960 ! ______________________________________________________________________
01970 L1970: let heading=1
01980   print #255: "\qc  {\f181 \fs24 \b "&trim$(cnam$)&"}"
01990   print #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
02000   if trim$(secondr$)<>"" then print #255: "\qc  {\f181 \fs18 \b "&trim$(secondr$)&"}"
02010   print #255: "\qc  {\f181 \fs16 \b "&trim$(fnpedat$)&"}"
02020   print #255: "\qL "
02030   print #255: 
02040   print #255: 
02050   if nap=13 then goto L2080
02060   print #255,using L2090: mat m1$
02070   goto L2100
02080 L2080: print #255,using L2090: mat m2$
02090 L2090: form pos 42,13*c 12,skip 2
02100 L2100: return 
02110 ! ______________________________________________________________________
02120 L2120: let eofcode=1
02130   gosub L1650
02135   let fnfscode(actpd)
02136   let fnpriorcd(1)
02140   let fncloseprn
02150   goto XIT
02160 ! ______________________________________________________________________
02170 XIT: let fnxit
02180 ! ______________________________________________________________________
02190 ! <Updateable Region: ERTN>
02200 ERTN: let fnerror(cap$,err,line,act$,"xit")
02210   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
02220   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
02230   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
02240 ERTN_EXEC_ACT: execute act$ : goto ERTN
02250 ! /region
02260 ! ______________________________________________________________________
