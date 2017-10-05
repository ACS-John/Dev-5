00010 ! Replace S:\acsGL\AcGlIncY
00020 ! -- INCOME STATEMENT FOR THIRTEEN PERIODS
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnpglen,fnerror,fnprocess,fncno,fnUseDeptNo,fnpedat$,fnps,fnpriorcd,fnfscode,fnactpd$,fncch$,fnglfs,fnactpd,fntos,fntxt,fncmdkey,fnacs,fnlbl
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim p$(20)*50
00080   dim fl1$*256,actpd$*6,pedat$*20,m1$(13)*9,m2$(13)*8,total(13)
00090   dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*12
00100   dim cnam$*40,b$*3,a$(8)*30,oldtrans$*16,g(8),accum(9,13)
00110   dim by(13),bp(13),cap$*128,udf$*256
00120 ! ______________________________________________________________________
00130   fntop(program$,cap$="Income Statement with Period Comparison")
00140   on fkey 5 goto L2360
00150   fncno(cno,cnam$)
00160   data "     ONE","     TWO","   THREE","    FOUR","    FIVE","     SIX","   SEVEN","   EIGHT","    NINE","     TEN","  ELEVEN","  TWELVE",""
00170   read mat m1$
00180   data "     ONE","     TWO","   THREE","    FOUR","    FIVE","     SIX","   SEVEN","   EIGHT","    NINE","     TEN","  ELEVEN","  TWELVE","THIRTEEN"
00190   read mat m2$
00200 ! 
00210   let udf$=env$('temp')&'\'
00220   actpd=fnactpd
00230   actpd$=fnactpd$
00240 ! 
00250   if fnglfs=5 then goto XIT !:
          ! sets fnps,fnpriorcd,fnfscode (primary/secondary,current year/Prior,period to print)
00260   open #20: "Name="&env$('Q')&"\GLmstr\Company.h"&str$(cno)&",Shr",internal,outin,relative  !:
        read #20,using 'Form Pos 384,n 2',rec=1: nap : close #20: 
00270   open #20: "Name="&env$('Q')&"\GLmstr\Company.h"&str$(cno)&",Shr",internal,input,relative: read #20,using "Form pos 296,N 2",rec=1: lmu : close #20: 
00280 ! ______________________________________________________________________
00290   pors=1
00300   mp1=69
00310   if fnps=2 then mp1=mp1+3
00320   fl1$="Name="&env$('Q')&"\GLmstr\ACGLFNSI.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\FNSIINDX.h"&str$(cno)&",Shr"
00330   if fnps=2 then fl1$="Name="&env$('Q')&"\GLmstr\ACGLFNSJ.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\FNSJINDX.h"&str$(cno)&",Shr"
00340   form c 9,skip 0
00350 L350: form pos mp1,pd 3,pos 81,41*pd 6.2
00360   form c 7,skip 0
00370   nametab=int(95-len(rtrm$(cnam$))/2)
00380   open #1: fl1$,internal,input,keyed 
00390   if fnprocess=1 or fnUseDeptNo=0 then goto L490
00400   fntos(sn$="Acglincy") !:
        mylen=30: mypos=mylen+3 : right=1
00410   fnlbl(1,1,"Cost Center or Department #:",mylen,right)
00420   fntxt(1,mypos,3,0,right,"30",0,"Enter the cost center or department number if you wish to pr only one department, else leave blank for all.",0 ) !:
        resp$(1)=""
00430   fnlbl(2,1,"(Blank for all Departments)",mylen,right)
00440   fncmdkey("&Next",1,1,0,"Prints the financial statement.")
00450   fncmdkey("&Cancel",5,0,1,"Returns to menu without posting.")
00460   fnacs(sn$,0,mat resp$,ckey)
00470   if ckey=5 then goto XIT
00480   costcntr=val(resp$(1))
00490 L490: cnam$=rtrm$(cnam$)
00500   pf1=len(cnam$)+int((43-len(cnam$))/2)
00510   close #101: ioerr L520
00520 L520: open #101: "SROW=08,SCOL=18,EROW=12,ECOL=58,BORDER=DR,CAPTION= INCOME STATEMENT WITH MONTHLY COMPARISONS ",display,outin 
00530   pr f "08,18,C 41,H,N": lpad$(cnam$,pf1)
00540   pr f "09,18,C 41,H,N": "            COMPANY NUMBER "&str$(cno)
00550   pr f "11,18,C 41,R,N": "              IN PROCESS"
00560   pr f "13,30,C 16,R,N": "PRESS F5 TO STOP"
00570   if cmdkey=5 then goto L2380 ! jb
00580   report$="STATEMENT OF INCOME AND EXPENSES"
00590   fnopenprn
00600   if fnps=2 then goto L630 ! secondary
00610   execute "Index "&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&" "&udf$&"fsindex.H"&str$(cno)&" 69 3 Replace DupKeys -N"
00620   goto L640
00630 L630: execute "Index "&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&" "&udf$&"fsindex.H"&str$(cno)&" 72 3 Replace DupKeys -N"
00640 L640: open #3: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&",KFName="&udf$&"fsindex.h"&str$(cno)&",Shr",internal,input,keyed 
00650   redir=0: if file$(255)(1:4)<>"PRN:" then redir=1
00660 L660: read #1,using L710: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof L2360
00670   if ltrm$(r$)="" or ltrm$(r$)="0" then goto L660
00680   if costcntr=0 then goto L710
00690   if fc=0 and te$="F" then goto L720
00700   if costcntr><fc then goto L660
00710 L710: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
00720 L720: if te$="S" or te$="F" then goto L740
00730   if heading=0 and te$><"R" then gosub L2160
00740 L740: on pos ("RFHDTS",te$,1) goto L1630,L1670,L750,L800,L1350,L1630 none L660
00750 L750: pr #255,using L760: d$(1:40)
00760 L760: form pos sp,c 40,skip 1
00770   gosub L1810
00780   gosub L1740
00790   goto L660
00800 L800: if notrans=1 then goto L1120
00810   if ir=val(r$) and val(r$)><0 then goto L860
00820   if ir>val(r$) then goto L860
00830 L830: ! read amounts from gl master
00840 L840: read #3,using L350: ir,bb,cb,mat by,mat bp eof L1110
00850   if ir=0 then goto L840
00860 L860: if ir=val(r$) then goto L870 else goto L1090
00870 L870: if fnpriorcd=2 then goto L1020
00880   for j=1 to 13
00890     if j=1 and actpd=1 then total(j)=total(j)+cb else goto L910
00900     goto L1000
00910 L910: if j=1 then total(j)=total(j)+by(j) else goto L930
00920     goto L1000
00930 L930: if j>actpd then goto L1000
00940     if j=<lmu then total(j)=total(j)+by(j)-by(j-1) else goto L960
00950     goto L1000
00960 L960: if lmu=actpd then goto L970 else goto L990
00970 L970: total(j)=total(j)+by(j)-by(j-1)
00980     goto L1000
00990 L990: total(j)=total(j)+cb-by(j-1)
01000 L1000: next j
01010   goto L830
01020 L1020: for j=1 to 13
01030     if j>nap then goto L1070
01040     if j=1 then total(j)=total(j)+bp(j) else goto L1060
01050     goto L1070
01060 L1060: total(j)=total(j)+bp(j)-bp(j-1)
01070 L1070: next j
01080   goto L830
01090 L1090: if ir<val(r$) then goto L830
01100   if ir>val(r$) then goto L1120
01110 L1110: notrans=1
01120 L1120: for j=1 to 9
01130     if ac(j)=9 then goto L1170
01140     for k=1 to 13
01150       accum(j,k)=accum(j,k)+total(k)
01160     next k
01170 L1170: next j
01180   for j=1 to 13
01190     if rs=1 then total(j)=-total(j)
01200   next j
01210   goto L1230 ! IF SUM(TOTAL)><0 THEN GOTO 920
01220   if ls+ul+ds+ic>0 then goto L1230 else goto L660
01230 L1230: sp2=48-sp-1
01240   if nap=13 then goto L1280
01250   pr #255,using L1260: d$(1:sp2),sum(total),total(1),total(2),total(3),total(4),total(5),total(6),total(7),total(8),total(9),total(10),total(11),total(12) pageoflow L1960
01260 L1260: form pos sp,c sp2,pos 38,13*n 12.2,skip redir
01270   goto L1300
01280 L1280: pr #255,using L1290: d$(1:sp2),sum(total),mat total pageoflow L1960
01290 L1290: form pos sp,c sp2,pos 38,14*n 12.2,skip redir
01300 L1300: mat total=(0)
01310   gosub L1740
01320   gosub L1970
01330   gosub L1810
01340   goto L660
01350 L1350: if ap=0 then ap=1
01360   if rs=1 then accum1=-accum(ap,1) else accum1=accum(ap,1)
01370   if rs=1 then accum2=-accum(ap,2) else accum2=accum(ap,2)
01380   if rs=1 then accum3=-accum(ap,3) else accum3=accum(ap,3)
01390   if rs=1 then accum4=-accum(ap,4) else accum4=accum(ap,4)
01400   if rs=1 then accum5=-accum(ap,5) else accum5=accum(ap,5)
01410   if rs=1 then accum6=-accum(ap,6) else accum6=accum(ap,6)
01420   if rs=1 then accum7=-accum(ap,7) else accum7=accum(ap,7)
01430   if rs=1 then accum8=-accum(ap,8) else accum8=accum(ap,8)
01440   if rs=1 then accum9=-accum(ap,9) else accum9=accum(ap,9)
01450   if rs=1 then accum10=-accum(ap,10) else accum10=accum(ap,10)
01460   if rs=1 then accum11=-accum(ap,11) else accum11=accum(ap,11)
01470   if rs=1 then accum12=-accum(ap,12) else accum12=accum(ap,12)
01480   if rs=1 then accum13=-accum(ap,13) else accum13=accum(ap,13)
01490   accumt=0
01500   for j=1 to 13
01510     if rs=1 then accumt=accumt-accum(ap,j) else accumt=accumt+accum(ap,j)
01520   next j
01530   sp2=48-sp-1
01540   if nap=13 then goto L1580
01550   pr #255,using L1560: d$(1:sp2),accumt,accum1,accum2,accum3,accum4,accum5,accum6,accum7,accum8,accum9,accum10,accum11,accum12 pageoflow L1960
01560 L1560: form pos sp,c sp2,pos 38,13*n 12.2,skip redir
01570   goto L1590
01580 L1580: pr #255,using L1290: d$(1:sp2),accumt,accum1,accum2,accum3,accum4,accum5,accum6,accum7,accum8,accum9,accum10,accum11,accum12,accum13
01590 L1590: gosub L1740
01600   gosub L1970
01610   gosub L1810
01620   goto L660
01630 L1630: if te$="R" then report$=d$
01640   if te$="S" then secondr$=d$
01650   gosub L1810
01660   goto L660
01670 L1670: if foot1=1 then goto L1720
01680   tabnote=sp
01690   let foot1=1
01700   let foot$=d$
01710   goto L660
01720 L1720: let foot$=rtrm$(foot$)&d$
01730   goto L660
01740 L1740: for j=1 to 9
01750     if ac(j)=0 or ac(j)=9 then goto L1790
01760     for k=1 to 13
01770       accum(j,k)=0
01780     next k
01790 L1790: next j
01800   return 
01810 L1810: if ls=0 then goto L1950
01820   if ls=99 then goto L1860
01830   pr #255,using L1840: " "
01840 L1840: form pos 1,c 1,skip ls
01850   goto L1950
01860 L1860: fnpglen(pglen)
01870 ! If PGLEN<>42 Then pGLEN=58
01880   sk=pglen-krec(255): fl=len(rtrm$(foot$))
01890 ! If PGLEN=42 Then sK=SK+1
01900   pr #255,using L1910: rtrm$(foot$)
01910 L1910: form skip sk,pos tabnote,c fl,skip 1
01920   if eofcode=1 then goto L1950
01930   pr #255: newpage
01940   gosub L2160
01950 L1950: return 
01960 L1960: gosub L1860: continue 
01970 L1970: if ul=0 then goto L2120
01980   if ul=1 then goto L2060
01990   let underlin$=" ==========="
02000   if nap=13 then goto L2030
02010   pr #255,using L2040: underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,""
02020   goto L2120
02030 L2030: pr #255,using L2040: underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$
02040 L2040: form skip 1,pos 38,14*c 12,skip redir
02050   goto L2120
02060 L2060: let underlin$=" ___________"
02070   if nap=13 then goto L2100
02080   pr #255,using L2110: underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,""
02090   goto L2120
02100 L2100: pr #255,using L2110: underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$
02110 L2110: form skip redir,pos 38,14*c 12,skip redir
02120 L2120: if redir=0 then pr #255,using L2130: " "
02130 L2130: form skip 1,c 1,skip 0
02140   return 
02150 ! ______________________________________________________________________
02160 L2160: heading=1
02170   pr #255,using L2180: cnam$
02180 L2180: form skip 2,pos 10,cc 70,skip 1
02190   p1=95-len(rtrm$(report$))/2
02200   pr #255,using L2210: rtrm$(report$)
02210 L2210: form pos 10,cc 70
02220   if rtrm$(secondr$)="" then goto L2250
02230   p1=95-len(rtrm$(secondr$))/2
02240   pr #255,using L2210: rtrm$(secondr$)
02250 L2250: p1=81-len(rtrm$(actpd$))/2-len(rtrm$(pedat$))/2
02260   pr #255,using L2210: "For the "&rtrm$(actpd$)&" month period ended "&rtrm$(fnpedat$)
02270   pr #255: 
02280   pr #255: 
02290   if nap=13 then goto L2320
02300   pr #255,using L2330: "YTD-TOTAL",mat m1$
02310   goto L2340
02320 L2320: pr #255,using L2330: "YTD-TOTAL",mat m2$
02330 L2330: form pos 42,14*c 12,skip 2
02340 L2340: return 
02350 ! ______________________________________________________________________
02360 L2360: eofcode=1
02370   gosub L1860
02380 L2380: ! 
02390 ! 
02400   fncloseprn
02410 ! 
02420   goto XIT
02430 ! ______________________________________________________________________
02440 XIT: fnxit
02450 ! ______________________________________________________________________
02460 ! <updateable region: ertn>
02470 ERTN: fnerror(program$,err,line,act$,"xit")
02480   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
02490   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
02500   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
02510 ERTN_EXEC_ACT: execute act$ : goto ERTN
02520 ! /region
