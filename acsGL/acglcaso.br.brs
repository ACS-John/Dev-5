00010 ! Replace S:\acsGL\acglCasO
00020 ! Cash Flow with YTD Budget Comparison
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnxit,fntop, fnwait,fnopenprn,fncloseprn,fnpglen,fncno,fnerror,fnprocess,fnactpd$,fnpedat$,fnactpd,fnfscode,fnUseDeptNo,fnpriorcd,fnps,fnglfs,fntos,fnlbl,fntxt,fncmdkey,fnacs
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim bm(13),bp(13),by(13)
00080   dim cnam$*40,b$*3,a$(8)*30,oldtrans$*16,g(8),accum(9,7),fl1$*256,cap$*128
00090   dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*14
00100 ! ______________________________________________________________________
00110   fntop(program$,cap$="Cash Flow with YTD Budget Comparison")
00120   report$=cap$
00125   actpd$=fnactpd$ !:
        actpd=fnactpd !:
        fnfscode !:
        fnpriorcd
00130   if fnglfs=5 then goto XIT
00135   fscode=fnfscode !:
        priorcd=fnpriorcd
00140   fncno(cno,cnam$)
00150   fnopenprn
00160   open #20: "Name="&env$('Q')&"\GLmstr\Company.h"&env$('cno')&",Shr",internal,input,relative: read #20,using 'Form Pos 384,n 2',rec=1: nap : close #20: 
00170   fscode=fnfscode
00180   if nap<12 or nap>13 then nap=12
00190   in3$(1)="8,05,N 12.2,UT,N" : in3$(2)="8,25,N 12.2,UT,N" !:
        in3$(3)="8,45,N 12.2,UT,N" : in3$(4)="8,65,N 12.2,UT,N"
00200   if fnps=2 then mp1=78 else mp1=75
00210   fl1$="Name="&env$('Q')&"\GLmstr\ACGLFNSF.H"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\FNSFINDX.H"&env$('cno')&",Shr"
00220   if fnps=2 then fl1$="Name="&env$('Q')&"\GLmstr\ACGLFNSG.H"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\FNSGINDX.H"&env$('cno')&",Shr"
00230   open #1: fl1$,internal,input,keyed 
00240   if fnprocess=1 or fnUseDeptNo=0 then goto L340
00250   fntos(sn$="Acglcaso") !:
        mylen=30: mypos=mylen+3 : right=1
00260   fnlbl(1,1,"Cost Center or Department #:",mylen,right)
00270   fntxt(1,mypos,3,0,right,"30",0,"Enter the cost center or department number if you wish to pr only one department, else leave blank for all.",0 ) !:
        resp$(1)=""
00280   fnlbl(2,1,"(Blank for all Departments)",mylen,right)
00290   fncmdkey("&Next",1,1,0,"Prints the financial statement.")
00300   fncmdkey("&Cancel",5,0,1,"Returns to menu without posting.")
00310   fnacs(sn$,0,mat resp$,ckey)
00320   if ckey=5 then goto XIT
00330   costcntr=val(resp$(1))
00340 L340: open #3: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\fsindex.h"&env$('cno')&",Shr",internal,input,keyed 
00350 L350: read #1,using L390: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof DONE
00360   if ltrm$(r$)="" or ltrm$(r$)="0" then goto L350
00370   if costcntr=0 then goto L390
00380   if costcntr><fc then goto L350
00390 L390: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
00400   if te$="S" or te$="F" then goto L420
00410   if heading=0 and te$><"R" then gosub L1890
00420 L420: on pos ("RFHDTSBC",te$,1) goto L1330,L1380,L440,L500,L1170,L1330,L500,L2070 none L350
00430 ! ______________________________________________________________________
00440 L440: pr #255,using L450: d$(1:40)
00450 L450: form pos sp,c 40
00460   gosub L1590
00470   gosub L1470
00480   goto L350
00490 ! ______________________________________________________________________
00500 L500: if te$="B" and ap>0 then goto L1170 ! ENDING BANK BALANCE
00510   if notrans=1 then goto L780
00520   if ir>=val(r$) and val(r$)><0 then goto L630
00530 READ_3: read #3,using L620: ir,bb,cb,mat by,mat bp,mat bm eof L770
00540   if ir=0 then goto L640
00550   if fscode=0 or (fscode=actpd and priorcd=1) then goto L620
00560   if fscode<1 or fscode>13 then fscode=1
00570   if fnpriorcd=1 then cb=by(fscode) else cb=bp(fscode)
00580   if fnpriorcd=2 then goto L610
00590   if fscode>1 then bb=by(fscode-1) else bb=0
00600   goto L620
00610 L610: if fscode>1 then bb=bp(fscode-1) else bb=0
00620 L620: form pos mp1,pd 3,pos 81,41*pd 6.2
00630 L630: if ir=val(r$) then total+=(cb-bb) else goto L750
00640 L640: if te$="B" then !:
          total-=(cb-bb): total-=bb: total2-=bp(nap) !:
          goto L660
00650   total2+=cb
00660 L660: for z=1 to 13 : annualb+=bm(z) : next z
00670   if fnfscode=0 then monthb+=bm(fnactpd) else !:
          monthb+=bm(fnfscode)
00680   if fscode=0 then goto L690 else goto L720
00690 L690: for j=1 to fnactpd : ytdb+=bm(j) : next j
00700   goto READ_3
00710 ! ______________________________________________________________________
00720 L720: for j=1 to fscode : ytdb+=bm(j) : next j
00730   goto READ_3
00740 ! ______________________________________________________________________
00750 L750: if ir<val(r$) then goto READ_3
00760   if ir>val(r$) then goto L780
00770 L770: notrans=1
00780 L780: overundr=ytdb-total2
00790   unexpend=annualb-total2
00800   for j=1 to 9
00810     if ac(j)=9 then goto L890
00820     accum(j,1)+=total
00830     accum(j,2)+=total2
00840     accum(j,3)+=annualb
00850     accum(j,4)+=monthb
00860     accum(j,5)+=ytdb
00870     accum(j,6)+=overundr
00880     accum(j,7)+=unexpend
00890 L890: next j
00900   if rs=1 then total=-total else goto L970
00910   total2=-total2
00920   annualb=-annualb
00930   monthb=-monthb
00940   ytdb=-ytdb
00950   overundr=overundr
00960   unexpend=unexpend
00970 L970: if ds=1 then dollar$="$" else dollar$=" "
00980   if annualb><0 or total2><0 then goto L1010
00990   if total<>0 then goto L1010
01000   if ls+ds+ul+ic>0 then goto L1010 else goto L350
01010 L1010: sp2=36-sp-1
01020   if te$="B" then total=-total: total2=-total2: unexpend=0: ! REVERSE SIGN ON BEGINNING BANK BALANCE
01030   pr #255,using L1040: d$(1:sp2),dollar$,total2,dollar$,annualb,dollar$,unexpend pageoflow L1760
01040 L1040: form pos sp,c sp2,pos 37,c 1,pic(--,---,---.##),x 1,c 1,pic(--,---,---.##),x 1,c 1,pic(--,---,---.##),skip redir
01050   total=0
01060   total2=0
01070   annualb=0
01080   monthb=0
01090   ytdb=0
01100   overundr=0
01110   unexpend=0
01120   gosub L1470
01130   gosub L1770
01140   gosub L1590
01150   goto L350
01160 ! ______________________________________________________________________
01170 L1170: if ap=0 then ap=1
01180   if rs=1 then accum1=-accum(ap,1) else accum1=accum(ap,1)
01190   if rs=1 then accum2=-accum(ap,2) else accum2=accum(ap,2)
01200   if rs=1 then accum3=-accum(ap,3) else accum3=accum(ap,3)
01210   if rs=1 then accum4=-accum(ap,4) else accum4=accum(ap,4)
01220   if rs=1 then accum5=-accum(ap,5) else accum5=accum(ap,5)
01230   if rs=1 then accum6=accum(ap,6) else accum6=accum(ap,6)
01240   if rs=1 then accum7=-accum(ap,7) else accum7=accum(ap,7)
01250   if ds=1 then dollar$="$" else dollar$=" "
01260   sp2=36-sp-1
01270   if te$="B" then accum3=accum4=accum7=0
01280   pr #255,using L1040: d$(1:sp2),dollar$,accum2,dollar$,accum3,dollar$,accum7 pageoflow L1760
01290   gosub L1470
01300   gosub L1770
01310   gosub L1590
01320   goto L350
01330 L1330: if te$="R" then report$=d$
01340   if te$="S" then secondr$=d$
01350   gosub L1590
01360   goto L350
01370 ! ______________________________________________________________________
01380 L1380: if foot1=1 then goto L1440
01390   tabnote=sp
01400   foot1=1
01410   foot$=d$
01420   goto L350
01430 ! ______________________________________________________________________
01440 L1440: foot$=rtrm$(foot$)&d$
01450   goto L350
01460 ! ______________________________________________________________________
01470 L1470: for j=1 to 9
01480     if ac(j)=0 or ac(j)=9 then goto L1560
01490     accum(j,1)=0
01500     accum(j,2)=0
01510     accum(j,3)=0
01520     accum(j,4)=0
01530     accum(j,5)=0
01540     accum(j,6)=0
01550     accum(j,7)=0
01560 L1560: next j
01570   return 
01580 ! ______________________________________________________________________
01590 L1590: if ls=0 then goto L1740
01600   if ls=99 then goto L1650
01610   pr #255,using L1620: " "
01620 L1620: form pos 1,c 1,skip ls
01630   goto L1740
01640 ! ______________________________________________________________________
01650 L1650: fnpglen(pglen)
01660 ! If PGLEN<>42 Then pGLEN=58
01670   sk=pglen-krec(255): fl=len(rtrm$(foot$))
01680 ! If PGLEN=42 Then sK=SK+1
01690   pr #255,using L1700: rtrm$(foot$),"Page "&str$(pt1)
01700 L1700: form skip sk,pos tabnote,c fl,pos 75,c 8
01710   if eofcode=1 then goto L1740
01720   pr #255: newpage
01730   gosub L1890
01740 L1740: return 
01750 ! ______________________________________________________________________
01760 L1760: gosub L1650: continue 
01770 L1770: if ul=0 then goto L1860
01780   if ul=1 then goto L1830
01790   underlin$="=============="
01800   pr #255: ""
01810   goto L1840
01820 ! ______________________________________________________________________
01830 L1830: underlin$="______________"
01840 L1840: pr #255,using L1850: underlin$,underlin$,underlin$
01850 L1850: form skip redir,pos 37,3*c 15,skip redir
01860 L1860: if redir=0 then pr #255: ""
01870   return 
01880 ! ______________________________________________________________________
01890 L1890: heading=1
01900   pt1+=1
01910   pr #255: "\qc  {\f181 \fs24 \b "&env$('cnam')&"}"
01920   pr #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
01930   if trim$(secondr$)<>"" then pr #255: "\qc  {\f181 \fs18 \b "&trim$(secondr$)&"}"
01940   pr #255: "\qc  {\f181 \fs16 \b For the "&rtrm$(fnactpd$)&" month period ended "&rtrm$(fnpedat$)&"}"
01950   pr #255: "\ql "
01960   pr #255: ''
01970   pr #255: tab(44);"Year To";tab(60);"Annual";tab(71);"Over/Under"
01980   pr #255: tab(45);"Date";tab(60);"Budget";tab(73);"Budget"
01990   return 
02000 ! ______________________________________________________________________
02010 DONE: eofcode=1
02020   gosub L1650
02022   fnfscode(actpd)
02023   fnpriorcd(1)
02030   fncloseprn
02040   goto XIT
02050 ! ______________________________________________________________________
02060   fntos(sn$="ACglchgs2") !:
        mylen=30: mypos=mylen+3 : right=1
02070 L2070: fnlbl(1,10,d$)
02080   fnlbl(3,1,"Total Amount YTD:",mylen,right)
02090   fntxt(3,mypos,12,0,right,"10",0,"Enter the total for the year.",0 ) !:
        resp$(1)=str$(total2)
02100   fnlbl(4,1,"Total Budget Year to Date:",mylen,right)
02110   fntxt(4,mypos,12,0,right,"10",0,"Enter the annual budget.",0 ) !:
        resp$(2)=str$(annualb)
02120   fncmdkey("&Next",1,1,0,"Accept the answer.")
02130   fncmdkey("&Cancel",5,0,1,"Returns to menu without posting.")
02140   fnacs(sn$,0,mat resp$,ckey)
02150   if ckey=5 then goto XIT
02160   total2=val(resp$(1))
02170   annualb=val(resp$(2))
02180   return 
02190 XIT: fnxit
02200 ! ______________________________________________________________________
02210 ! <Updateable Region: ERTN>
02220 ERTN: fnerror(program$,err,line,act$,"xit")
02230   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
02240   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
02250   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
02260 ERTN_EXEC_ACT: execute act$ : goto ERTN
02270 ! /region
02280 ! ______________________________________________________________________
