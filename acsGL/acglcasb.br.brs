00010 ! Replace S:\acsGL\acglCasB
00020 ! CASH FLOW STATEMENT  WITH BUDGET
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnxit,fntop, fnopenprn,fncloseprn,fnpglen,fnerror,fncno,fncch$,fnprocess,fnUseDeptNo,fnactpd$,fnactpd,fnpedat$,fnfscode,fnpriorcd,fnps,fnglfs,fntos,fnlbl,fntxt,fncmdkey,fnacs
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim bm(13),bp(13),by(13),sc1$(2)*20,fl1$*256,in3$(4),p$(20)*50,cap$*128
00080   dim cnam$*40,b$*3,a$(8)*30,oldtrans$*16,g(8),accum(9,7),udf$*256,cch$*15
00090   dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*14
00100 ! ______________________________________________________________________
00110   fntop(program$,cap$="Cash Flow Statement With Budget")
00120   fncno(cno,cnam$)
00130   udf$=env$('temp')&'\'
00135   actpd$=fnactpd$ !:
        actpd=fnactpd !:
        fnfscode !:
        fnpriorcd
00140   if fnglfs=5 then goto XIT !:
          ! sets fnps,fnpriorcd,fnfscode (primary/secondary,current year/Prior,period to print)
00142   fscode=fnfscode !:
        priorcd=fnpriorcd
00150   open #20: "Name="&env$('Q')&"\GLmstr\Company.h"&str$(cno)&",Shr",internal,input,relative  !:
        read #20,using 'Form Pos 384,n 2',rec=1: nap : close #20: 
00160   actpd=fnactpd : fscode=fnfscode
00170   if nap<12 or nap> 13 then nap=12
00180   pors=1
00190   fnopenprn !:
        if file$(255)(1:4)<>"PRN:" then redir=1 else redir=0
00200   on fkey 5 goto L2170
00210   in3$(1)="8,5,N 12.2,UT,N"
00220   in3$(2)="8,25,N 12.2,UT,N"
00230   in3$(3)="8,45,N 12.2,UT,N"
00240   in3$(4)="8,65,N 12.2,UT,N"
00250   mp1=75
00260   if fnps=2 then mp1=mp1+3
00270   fl1$="Name="&env$('Q')&"\GLmstr\ACGLFNSF.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\FNSFIndx.h"&str$(cno)&",Shr"
00280   if fnps=2 then fl1$="Name="&env$('Q')&"\GLmstr\ACGLFNSG.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\FNSGIndx.h"&str$(cno)&",Shr"
00290   form c 7,skip 0
00300   open #1: fl1$,internal,input,keyed 
00310   if fnprocess=1 or fnUseDeptNo=0 then goto L410
00320   fntos(sn$="ACglcasb") !:
        mylen=30: mypos=mylen+3 : right=1
00330   fnlbl(1,1,"Cost Center or Department #:",mylen,right)
00340   fntxt(1,mypos,3,0,right,"30",0,"Enter the cost center or department number if you wish to pr only one department, else leave blank for all.",0 ) !:
        resp$(1)=""
00350   fnlbl(2,1,"(Blank for all Departments)",mylen,right)
00360   fncmdkey("&Next",1,1,0,"Prints the financial statement.")
00370   fncmdkey("&Cancel",5,0,1,"Returns to menu without posting.")
00380   fnacs(sn$,0,mat resp$,ckey)
00390   if ckey=5 then goto XIT
00400   costcntr=val(resp$(1))
00410 L410: report$=cap$
00420 L420: read #1,using L460: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof L2170
00430   if ltrm$(r$)="" or ltrm$(r$)="0" then goto L420
00440   if costcntr=0 then goto L460
00450   if costcntr><fc then goto L420
00460 L460: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
00470   if te$="S" or te$="F" then goto L490
00480   if heading=0 and te$><"R" then gosub L2040
00490 L490: on pos ("RFHDTSBC",te$,1) goto L1480,L1530,L500,L550,L1320,L1480,L550,L2240 none L420
00500 L500: pr #255,using L510: d$(1:40)
00510 L510: form pos sp,c 40,skip 1
00520   gosub L1730
00530   gosub L1610
00540   goto L420
00550 L550: if te$="B" and ap>0 then goto L1320 ! ENDING BANK BALANCE
00560   if notrans=1 then goto L940
00570   if ir>=val(r$) and val(r$)><0 then goto L740
00572   close #3: ioerr ignore
00574   if fnps=2 then 
00580     execute "Index "&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&" "&udf$&"fsindex.H"&str$(cno)&" 78 3 Replace DupKeys -N"
00590   else 
00600     execute "Index "&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&" "&udf$&"fsindex.H"&str$(cno)&" 75 3 Replace DupKeys -N"
00610   end if 
00620   open #3: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&",KFName="&udf$&"fsindex.h"&str$(cno)&",Shr",internal,input,keyed 
00630 L630: ! read amounts from gl master file
00640 L640: read #3,using L730: ir,bb,cb,mat by,mat bp,mat bm eof L940
00650   if ir=0 then goto L640
00660   if fscode=0 or (fscode=actpd and priorcd=1) then goto L730
00670   if fscode<1 or fscode>13 then fscode=1
00680   if fnpriorcd=1 then cb=by(fscode) else cb=bp(fscode)
00690   if fnpriorcd=2 then goto L720
00700   if fscode>1 then bb=by(fscode-1) else bb=0
00710   goto L730
00720 L720: if fscode>1 then bb=bp(fscode-1) else bb=0
00730 L730: form pos mp1,pd 3,pos 81,41*pd 6.2
00740 L740: if ir=val(r$) then total=total+(cb-bb) else goto L900
00750   if te$="B" then total=total-(cb-bb): total=total - bb: total2=total2-bp(nap) : goto L770
00760   total2=total2+cb
00770 L770: for z=1 to 13
00780     annualb=annualb+bm(z)
00790   next z
00800   if fscode=0 then monthb=monthb+bm(actpd) else monthb=monthb+bm(fscode)
00810   if fscode=0 then goto L820 else goto L860
00820 L820: for j=1 to actpd
00830     ytdb=ytdb+bm(j)
00840   next j
00850   goto L630
00860 L860: for j=1 to fscode
00870     ytdb=ytdb+bm(j)
00880   next j
00890   goto L630
00900 L900: if ir<val(r$) then goto L630
00910   if ir>val(r$) then goto L940
00920   notrans=1
00930   gosub L2240
00940 L940: overundr=ytdb-total2
00950   unexpend=annualb-total2
00960   for j=1 to 9
00970     if ac(j)=9 then goto L1050
00980     accum(j,1)+=total
00990     accum(j,2)+=total2
01000     accum(j,3)=accum(j,3)+annualb
01010     accum(j,4)=accum(j,4)+monthb
01020     accum(j,5)=accum(j,5)+ytdb
01030     accum(j,6)=accum(j,6)+overundr
01040     accum(j,7)=accum(j,7)+unexpend
01050 L1050: next j
01060   if rs=1 then total=-total else goto L1130
01070   total2=-total2
01080   annualb=-annualb
01090   monthb=-monthb
01100   ytdb=-ytdb
01110   overundr=overundr
01120   unexpend=unexpend
01130 L1130: if ds=1 then dollar$="$" else dollar$=" "
01140   if annualb><0 or total2><0 then goto L1170
01150   if total<>0 then goto L1170
01160   if ls+ds+ul+ic>0 then goto L1170 else goto L420
01170 L1170: sp2=24-sp-1
01180   if te$="B" then total=-total: total2=-total2 ! REVERSE SIGN ON BEGINNING BANK BALANCE
01185   if ul=1 then pr #255,using L1201: d$(1:sp2),dollar$,"{\ul ",monthb,"}",dollar$,"{\ul ",total,"}",dollar$,"{\ul ",total2,"}",dollar$,"{\ul ",annualb,"}" pageoflow L1900 : goto L1200
01190   pr #255,using L1200: d$(1:sp2),dollar$,monthb,dollar$,total,dollar$,total2,dollar$,annualb pageoflow L1900
01200 L1200: form pos sp,c sp2,pos 24,c 1,pic(--,---,---.##),x 1,c 1,pic(--,---,---.##),x 1,c 1,pic(--,---,---.##),x 1,c 1,pic(--,---,---.##),skip redir
01201 L1201: form pos sp,c sp2,pos 24,c 1,c 5,pic(--,---,---.##),c 1,x 1,c 1,c 5,pic(--,---,---.##),c 1,x 1,c 1,c 5,pic(--,---,---.##),c 1,x 1,c 1,c 5,pic(--,---,---.##),c 1,skip redir
01210   total=0
01220   total2=0
01230   annualb=0
01240   monthb=0
01250   ytdb=0
01260   overundr=0
01270   unexpend=0
01280   gosub L1610
01285   if ul=1 then goto L1300
01290   gosub L1910
01300 L1300: gosub L1730
01310   goto L420
01320 L1320: if ap=0 then ap=1
01330   if rs=1 then accum1=-accum(ap,1) else accum1=accum(ap,1)
01340   if rs=1 then accum2=-accum(ap,2) else accum2=accum(ap,2)
01350   if rs=1 then accum3=-accum(ap,3) else accum3=accum(ap,3)
01360   if rs=1 then accum4=-accum(ap,4) else accum4=accum(ap,4)
01370   if rs=1 then accum5=-accum(ap,5) else accum5=accum(ap,5)
01380   if rs=1 then accum6=accum(ap,6) else accum6=accum(ap,6)
01390   if rs=1 then accum7=accum(ap,7) else accum7=accum(ap,7)
01400   if ds=1 then dollar$="$" else dollar$=" "
01410   sp2=24-sp-1
01420   if te$="B" then accum2=accum3=accum4=0
01425   if ul=1 then pr #255,using L1201: d$(1:sp2),dollar$,"{\ul ",accum4,"}",dollar$,"{\ul ",accum1,"}",dollar$,"{\ul ",accum2,"}",dollar$,"{\ul ",accum3,"}" pageoflow L1900 : goto L1440
01430   pr #255,using L1200: d$(1:sp2),dollar$,accum4,dollar$,accum1,dollar$,accum2,dollar$,accum3 pageoflow L1900
01440 L1440: gosub L1610
01445   if ul=1 then goto L1460
01450   gosub L1910
01460 L1460: gosub L1730
01470   goto L420
01480 L1480: if te$="R" then report$=d$
01490   if te$="S" then secondr$=d$
01500   gosub L1730
01510   goto L420
01520 ! ______________________________________________________________________
01530 L1530: if foot1=1 then goto L1590
01540   tabnote=sp
01550   foot1=1
01560   foot$=d$
01570   goto L420
01580 ! ______________________________________________________________________
01590 L1590: foot$=rtrm$(foot$)&d$ : goto L420
01600 ! ______________________________________________________________________
01610 L1610: for j=1 to 9
01620     if ac(j)=0 or ac(j)=9 then goto L1700
01630     accum(j,1)=0
01640     accum(j,2)=0
01650     accum(j,3)=0
01660     accum(j,4)=0
01670     accum(j,5)=0
01680     accum(j,6)=0
01690     accum(j,7)=0
01700 L1700: next j
01710   return 
01720 ! ______________________________________________________________________
01730 L1730: if ls=0 then goto L1880
01740   if ls=99 then goto L1790
01750   pr #255,using L1760: " "
01760 L1760: form pos 1,c 1,skip ls
01770   goto L1880
01780 ! ______________________________________________________________________
01790 L1790: fnpglen(pglen)
01800 ! If PGLEN<>42 Then pGLEN=58
01810   sk=pglen-krec(255): fl=len(rtrm$(foot$))
01820 ! If PGLEN=42 Then sK=SK+1
01830   pr #255,using L1840: rtrm$(foot$),"Page "&str$(pt1)
01840 L1840: form skip sk,pos tabnote,c fl,pos 75,c 8,skip 1
01850   if eofcode=1 then goto L1880
01860   pr #255: newpage
01870   gosub L2040
01880 L1880: return 
01890 ! ______________________________________________________________________
01900 L1900: gosub L1790: continue 
01910 L1910: if ul=0 then goto L2000
01920   if ul=1 then goto L1970
01930   underlin$="=============="
01950   goto L1980
01960   goto L2000
01970 L1970: underlin$="______________"
01980 L1980: pr #255,using L1990: underlin$,underlin$,underlin$,underlin$
01990 L1990: form pos 24,4*c 15,skip redir
02000 L2000: ! If REDIR=0 Then pr #255,Using 2010: " "
02010   form c 1,skip 1
02020   return 
02030 ! ______________________________________________________________________
02040 L2040: heading=1
02050   pt1+=1
02060   pr #255: "\qc  {\f181 \fs24 \b "&env$('cnam')&"}"
02070   pr #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
02080   if trim$(secondr$)<>"" then pr #255: "\qc  {\f181 \fs18 \b "&trim$(secondr$)&"}"
02090   pr #255: "\qc  {\f181 \fs16 \b For the "&rtrm$(fnactpd$)&" month period ended "&rtrm$(fnpedat$)&"}"
02100   pr #255: "\ql "
02110   pr #255: 
02115   cch$=lpad$(rtrm$(fncch$),15)
02120   pr #255: tab(31);"MONTHLY";tab(38);cch$;tab(61);"YEAR TO";tab(77);"ANNUAL"
02130   pr #255: tab(32);"BUDGET";tab(44);"       ";tab(62);"DATE";tab(77);"BUDGET"
02140   pr #255: 
02150   return 
02160 ! ______________________________________________________________________
02170 L2170: eofcode=1
02180   gosub L1790
02190   fnfscode(actpd)
02192   fnpriorcd(1)
02210   fncloseprn
02220   goto XIT
02230 ! ______________________________________________________________________
02240 L2240: fntos(sn$="ACglchgs2") !:
        mylen=30: mypos=mylen+3 : right=1
02250   fnlbl(1,1,d$,mylen,right)
02260   fnlbl(3,1,"Monthy Budget:",mylen,right)
02270   fntxt(3,mypos,12,0,right,"10",0,"Enter the monthly budget.",0 ) !:
        resp$(1)=str$(monthb)
02280   fnlbl(4,1,"Total for the Month:",mylen,right)
02290   fntxt(4,mypos,12,0,right,"10",0,"Enter the total for the month.",0 ) !:
        resp$(2)=str$(total)
02300   fnlbl(5,1,"Total Year to Date:",mylen,right)
02310   fntxt(5,mypos,12,0,right,"10",0,"Enter the total for the year.",0 ) !:
        resp$(3)=str$(total2)
02320   fnlbl(6,1,"Total Budget for the Year:",mylen,right)
02330   fntxt(6,mypos,12,0,right,"10",0,"Enter the total budget for the  year.",0 ) !:
        resp$(4)=str$(annualb)
02340   fncmdkey("&Next",1,1,0,"Accept the answer.")
02350   fncmdkey("&Cancel",5,0,1,"Returns to menu without posting.")
02360   fnacs(sn$,0,mat resp$,ckey)
02370   if ckey=5 then goto XIT
02380   monthb=val(resp$(1))
02390   total=val(resp$(2))
02400   total2=val(resp$(3))
02410   annualb=val(resp$(4))
02420   goto L940
02430 XIT: fnxit
02440 IGNORE: continue 
02450 ! <Updateable Region: ERTN>
02460 ERTN: fnerror(program$,err,line,act$,"xit")
02470   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
02480   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
02490   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
02500 ERTN_EXEC_ACT: execute act$ : goto ERTN
02510 ! /region
02520 ! ______________________________________________________________________
