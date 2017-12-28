00010 ! Replace S:\acsGL\AcGlInc6b
00020 ! -- INCOME STATEMENT WITH BUDGET (month compared to month and year compared to year to date
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnpglen,fnerror,fncno,fnglfs,fncch$,fnpedat$,fnactpd$,fnactpd,fnfscode,fnUseDeptNo,fnpriorcd,fnTos,fnprocess,fnLbl,fnTxt,fnCmdKey,fnAcs,fnps
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim fl1$*256,p$(20)*50
00080   dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*14
00090   dim cnam$*40,b$*3,a$(8)*30,oldtrans$*16,g(8),accum(9,6)
00100   dim pedat$*20,actpd$*6,bm(13),bp(13),by(13),cap$*128,udf$*256
00110 ! ______________________________________________________________________
00120   fntop(program$,cap$="Income Statement-Monthly & Year Budgets")
00130   on fkey 5 goto L2160
00140   fncno(cno,cnam$)
00150   udf$=env$('temp')&'\'
00160   actpd=fnactpd
00170   actpd$=fnactpd$
00180   if fnglfs=5 then goto XIT !:
          ! sets fnps,fnpriorcd,fnfscode (primary/secondary,current year/Prior,period to print)
00190 ! ______________________________________________________________________
00200   pr newpage
00210   pors=1
00220   mp1=69
00230   if fnps=2 then mp1=mp1+3
00240   fl1$="Name="&env$('Q')&"\GLmstr\ACGLFNSI.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\FNSIINDX.h"&env$('cno')&",Shr"
00250   if fnps=2 then fl1$="Name="&env$('Q')&"\GLmstr\ACGLFNSJ.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\FNSJINDX.h"&env$('cno')&",Shr"
00260   form c 9,skip 0
00270   form c 7,skip 0
00280   nametab=int(44-len(rtrm$(cnam$))/2)
00290   open #1: fl1$,internal,input,keyed 
00300   if fnprocess=1 or fnUseDeptNo=0 then goto L390
00310   fnTos(sn$="ACglincb") !:
        mylen=30: mypos=mylen+3 : right=1
00320   fnLbl(1,1,"Cost Center or Department #:",mylen,right)
00330   fnTxt(1,mypos,3,0,right,"30",0,"Enter the cost center or department number if you wish to pr only one department, else leave blank for all.",0 ) !:
        resp$(1)=""
00340   fnLbl(2,1,"(Blank for all Departments)",mylen,right)
00350   fnCmdKey("&Next",1,1,0,"Prints the financial statement.")
00360   fnCmdKey("&Cancel",5,0,1,"Returns to menu without posting.")
00370   fnAcs(sn$,0,mat resp$,ckey)
00380   if ckey=5 then goto XIT
00390 L390: costcntr=val(resp$(1))
00400   cnam$=rtrm$(cnam$)
00410   pf1=len(cnam$)+int((43-len(cnam$))/2)
00420   report$="STATEMENT OF INCOME AND EXPENSES"
00430   fnopenprn(cp,58,220,process)
00440   redir=0: if file$(255)(1:4)<>"PRN:" then redir=1
00450   if fnps=2 then goto L480 ! secondary
00460   execute "Index "&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&" "&udf$&"fsindex.H"&env$('cno')&" 69 3 Replace DupKeys -N"
00470   goto L490
00480 L480: execute "Index "&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&" "&udf$&"fsindex.H"&env$('cno')&" 72 3 Replace DupKeys -N"
00490 L490: open #3: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&",KFName="&udf$&"fsindex.h"&env$('cno')&",Shr",internal,input,keyed 
00500 L500: read #1,using L550: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof L2160
00510   if ltrm$(r$)="" or ltrm$(r$)="0" then goto L500
00520   if costcntr=0 then goto L550
00530   if fc=0 and te$="F" then goto L560 ! 5/08/1989
00540   if costcntr><fc then goto L500
00550 L550: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
00560 L560: if te$="S" or te$="F" then goto L580
00570   if heading=0 and te$><"R" then gosub L2030
00580 L580: on pos ("RFHDTS",te$,1) goto L1490,L1530,L590,L640,L1330,L1490 none L500
00590 L590: pr #255,using L600: d$(1:40)
00600 L600: form pos sp,c 40,skip 1
00610   gosub L1710
00620   gosub L1600
00630   goto L500
00640 L640: if notrans=1 then goto L950
00650   if ir>=val(r$) and val(r$)><0 then goto L770
00660 L660: ! read amounts from gl master file
00670 L670: read #3,using L760: ir,bb,cb,mat by,mat bp,mat bm eof L940
00680   if ir=0 then goto L670 ! skip accounts with no income reference #
00690   if fnfscode=0 then goto L760
00700   if fnfscode<1 or fnfscode>13 then let fnfscode=1
00710   if fnpriorcd=1 then cb=by(fnfscode) else cb=bp(fnfscode)
00720   if fnpriorcd=2 then goto L750
00730   if fnfscode>1 then bb=by(fnfscode-1) else bb=0
00740   goto L760
00750 L750: if fnfscode>1 then bb=bp(fnfscode-1) else bb=0
00760 L760: form pos mp1,pd 3,pos 81,41*pd 6.2
00770 L770: if ir=val(r$) then total=total+(cb-bb) else goto L920
00780   total2=total2+cb
00790   for z=1 to 13
00800     annualb=annualb+bm(z)
00810   next z
00820   if fnfscode=0 then monthb=monthb+bm(actpd) else monthb=monthb+bm(fnfscode) ! 11/24/86
00830   if fnfscode=0 then goto L840 else goto L880 ! 11/24/86
00840 L840: for j=1 to actpd
00850     ytdb=ytdb+bm(j)
00860   next j
00870   goto L660
00880 L880: for j=1 to fnfscode ! 11/24/86
00890     ytdb=ytdb+bm(j) ! 11/24/86
00900   next j ! 11/24/86
00910   goto L660 ! 11/24/86
00920 L920: if ir<val(r$) then goto L660
00930   if ir>val(r$) then goto L950
00940 L940: notrans=1
00950 L950: overundr=ytdb-total2
00955   oumonth=monthb-total
00960 ! uNEXPEND=ANNUALB-TOTAL2
00970   for j=1 to 9
00980     if ac(j)=9 then goto L1060 ! 10/14/87
00990     accum(j,1)=accum(j,1)+monthb
01000     accum(j,2)=accum(j,2)+total
01010     accum(j,3)=accum(j,3)+oumonth
01020     accum(j,4)=accum(j,4)+ytdb
01030     accum(j,5)=accum(j,5)+total2
01040     accum(j,6)=accum(j,6)+overundr
01060 L1060: next j
01070   if rs=1 then total=-total else goto L1140
01080   total2=-total2
01090   annualb=-annualb
01100   monthb=-monthb
01110   ytdb=-ytdb
01120   overundr=overundr
01130   unexpend=unexpend
01140 L1140: if ds=1 then dollar$="$" else dollar$=" "
01150   goto L1190 ! pr all accounts even if zero balance  (if budget ever nets to zero, it messes the monthly budget column up
01160   if annualb><0 or total2><0 then goto L1190
01170   if total<>0 then goto L1190
01180   if ls+ds+ul+ic>0 then goto L1190 else goto L500
01190 L1190: sp2=26-sp-1
01195   if ul=1 then pr #255,using L1211: d$(1:sp2),dollar$,"{\UL ",monthb,"}",dollar$,"{\UL ",total,"}",dollar$,"{\UL ",oumonth,"}",dollar$,"{\UL ",ytdb,"}",dollar$,"{\UL ",total2,"}",dollar$,"{\UL ",overundr,"}" pageoflow L1890 : goto L1210
01200   pr #255,using L1210: d$(1:sp2),dollar$,monthb,dollar$,total,dollar$,oumonth,dollar$,ytdb,dollar$,total2,dollar$,overundr pageoflow L1890
01210 L1210: form pos sp,c sp2,pos 26,c 1,n 13.2,x 1,c 1,n 11.2,x 1,c 1,n 11.2,x 1,c 1,n 13.2,x 1,c 1,n 13.2,x 1,c 1,n 13.2,x 1,c 1,n 13.2,skip redir
01211 L1211: form pos sp,c sp2,pos 26,c 1,c 5,n 13.2,c 1,x 1,c 1,c 5,n 11.2,c 1,x 1,c 1,c 5,n 11.2,c 1,x 1,c 1,c 5,n 13.2,c 1,x 1,c 1,c 5,n 13.2,c 1,x 1,c 1,c 5,n 13.2,c 1,x 1,c 1,c 5,n 13.2,c 1,skip redir
01220   total=0
01230   total2=0
01240   annualb=0
01245   oumonth=0
01250   monthb=0
01260   ytdb=0
01270   overundr=0
01290   gosub L1600
01295   if ul=1 then goto L1310
01300   gosub L1900
01310 L1310: gosub L1710
01320   goto L500
01330 L1330: if ap=0 then ap=1
01340   if rs=1 then accum1=-accum(ap,1) else accum1=accum(ap,1)
01350   if rs=1 then accum2=-accum(ap,2) else accum2=accum(ap,2)
01360   if rs=1 then accum3=-accum(ap,3) else accum3=accum(ap,3)
01370   if rs=1 then accum4=-accum(ap,4) else accum4=accum(ap,4)
01380   if rs=1 then accum5=-accum(ap,5) else accum5=accum(ap,5)
01390   if rs=1 then accum6=accum(ap,6) else accum6=accum(ap,6)
01410   if ds=1 then dollar$="$" else dollar$=" "
01420   sp2=26-sp-1
01425   if ul=1 then pr #255,using L1211: d$(1:sp2),dollar$,"{\UL ",accum1,"}",dollar$,"{\UL ",accum2,"}",dollar$,"{\UL ",accum3,"}",dollar$,"{\UL ",accum4,"}",dollar$,"{\UL ",accum5,"}",dollar$,"{\UL ",accum6,"}" pageoflow L1890 : goto L1440
01430   pr #255,using L1210: d$(1:sp2),dollar$,accum1,dollar$,accum2,dollar$,accum3,dollar$,accum4,dollar$,accum5,dollar$,accum6 pageoflow L1890
01440 L1440: ft1=0
01450   gosub L1600
01455   if ul=1 then goto L1470
01460   gosub L1900
01470 L1470: gosub L1710
01480   goto L500
01490 L1490: if te$="R" then report$=d$
01500   if te$="S" then secondr$=d$
01510   gosub L1710
01520   goto L500
01530 L1530: if foot1=1 then goto L1580
01540   tabnote=sp
01550   foot1=1
01560   foot$=d$
01570   goto L500
01580 L1580: foot$=rtrm$(foot$)&d$
01590   goto L500
01600 L1600: for j=1 to 9
01610     if ac(j)=0 or ac(j)=9 then goto L1690 ! 10/14/87
01620     accum(j,1)=0
01630     accum(j,2)=0
01640     accum(j,3)=0
01650     accum(j,4)=0
01660     accum(j,5)=0
01670     accum(j,6)=0
01690 L1690: next j
01700   return 
01710 L1710: if ls=0 then goto L1870
01720   if ls=99 then goto L1760
01730   pr #255,using L1740: " "
01740 L1740: form pos 1,c 1,skip ls
01750   goto L1870
01760 L1760: ! If FT1=1 Then Goto 1870
01770   fnpglen(pglen)
01780 ! If PGLEN<>42 Then pGLEN=58
01790   sk=pglen-krec(255): fl=len(rtrm$(foot$))
01800 ! If PGLEN=42 Then sK=SK+1
01810   pr #255,using L1820: rtrm$(foot$),"Page "&str$(pt1)
01820 L1820: form skip sk,pos tabnote,c fl,pos 100,c 8,skip 1
01830 ! ft1=1
01840   if eofcode=1 then goto L1870
01850   pr #255: newpage
01860   gosub L2030
01870 L1870: return 
01880 ! ______________________________________________________________________
01890 L1890: gosub L1760: continue 
01900 L1900: if ul=0 then goto L1990
01910   if ul=1 then goto L1960
01920   underlin$="=============="
01940   goto L1970
01950   goto L1990
01960 L1960: underlin$="______________"
01970 L1970: pr #255,using L1980: underlin$,underlin$(1:12),underlin$(1:12),underlin$,underlin$,underlin$
01980 L1980: form pos 26,c 15,2*c 13,4*c 15,skip redir
01990 L1990: if redir=0 then pr #255,using L2000: " " pageoflow L1890
02000 L2000: form c 1
02010   return 
02020 ! ______________________________________________________________________
02030 L2030: heading=1
02040   pt1+=1
02050   pr #255: "\qc  {\f181 \fs24 \b "&env$('cnam')&"}"
02060   pr #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
02070   if trim$(secondr$)<>"" then pr #255: "\qc  {\f181 \fs18 \b "&trim$(secondr$)&"}"
02080   pr #255: "\qc  {\f181 \fs16 \b For the "&rtrm$(actpd$)&" month period ended "&rtrm$(fnpedat$)&"}"
02090   pr #255: "\ql "
02100   pr #255: 
02110   pr #255: tab(31);"<-------";fncch$;"------>";tab(71);" <------------YEAR TO DATE ------------>"
02120   pr #255: tab(34);"BUDGET";tab(45);"ACTIVITY";tab(57);"OVER/UNDER";tab(75);"BUDGET";tab(87);"ACTIVITY";tab(101);"OVER/UNDER"
02130   pr #255: 
02140   return 
02150 ! ______________________________________________________________________
02160 L2160: eofcode=1
02170   gosub L1760
02180 ! 
02190 ! 
02200   fncloseprn
02210 ! 
02220 XIT: fnxit
02230 ! ______________________________________________________________________
02240 ! <updateable region: ertn>
02250 ERTN: fnerror(program$,err,line,act$,"xit")
02260   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
02270   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
02280   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
02290 ERTN_EXEC_ACT: execute act$ : goto ERTN
02300 ! /region
