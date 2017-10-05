00010 ! formerly S:\acsGL\AcGlInc4
00020 ! -- Four Column Budget Income Statement 
00030 ! r: library, on error and dims
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnpglen,fnerror,fnpedat$,fnps,fnpriorcd,fnfscode,fnactpd$,fncch$,fnglfs,fnactpd,fnindex_it,fngethandle
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim actpd$*6,cch$*20
00080   dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*14
00090   dim accum(9,7)
00100   dim actpd$*6,bm(13),bp(13),by(13),cap$*128
00110 ! /r
00120   fntop(program$,cap$="Four Column Budget Income Statement")
00130   report$="Statement of Income and Expenses"
00150   if fnglfs=5 then goto XIT ! ! sets fnps,fnpriorcd,fnfscode (primary/secondary,current year/Prior,period to print)
00152   on fkey 5 goto FINIS
00154 ! r: setup files, etc
00160   cch$=fncch$
00180   actpd$=fnactpd$
00190   actpd=fnactpd
00200   fscode=fnfscode
00210   priorcd=fnpriorcd
00220 ! ______________________________________________________________________
00240   if fnps=2 then 
00242     mp1=72 
00244     open #hAcGlFnsX:=fngethandle:"Name="&env$('Q')&"\GLmstr\ACGLFNSJ.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\FNSJINDX.h"&env$('cno')&",Shr" ,internal,input,keyed 
00246   else 
00248     mp1=69 
00250     open #hAcGlFnsX:=fngethandle:"Name="&env$('Q')&"\GLmstr\ACGLFNSI.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\FNSIINDX.h"&env$('cno')&",Shr",internal,input,keyed 
00252   end if
00254   fnindex_it(env$('Q')&"\GLmstr\GLmstr.h"&env$('cno'),env$('temp')&"\fsindex.H"&env$('cno'),str$(mp1)&" 3")
00260   fnopenprn
00290   open #hGlMstr:=fngethandle: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&",KFName="&env$('temp')&"\fsindex.h"&env$('cno')&",Shr",internal,input,keyed 
00300   fHlMstr: form pos mp1,pd 3,pos 81,41*pd 6.2
00320   ! /r
00350 ReadFinStmtLayout: ! r:
00352   read #hAcGlFnsX,using L400: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof FINIS
00360   if ltrm$(r$)="" or ltrm$(r$)="0" then goto ReadFinStmtLayout
00370   if costcntr=0 then goto L400
00380   if fc=0 and te$="F" then goto L410
00390   if costcntr><fc then goto ReadFinStmtLayout
00400   L400: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
00410   L410: if te$="S" or te$="F" then goto L430
00420   if heading=0 and te$><"R" then gosub PrHeaderPrimary
00430   L430: !
00432 on pos ("RFHDTS",te$,1) goto FinRecRandS,FinRecF,FinRecH,FinRecD,FinRecT,FinRecRandS none ReadFinStmtLayout ! /r
00440 FinRecH: ! r:
00442   pr #255,using L450: d$(1:40)
00450   L450: form pos sp,c 40
00460   gosub PrFootnotesA
00470   gosub AccumReset
00480 goto ReadFinStmtLayout ! /r
00490 ! ______________________________________________________________________
00500 FinRecD: ! r:
00502   if notrans=1 then goto L720
00510   if ir>=val(r$) and val(r$)><0 then goto L630
00520   FdReadGlMstr: ! read amounts for gl master file
00530   read #hGlMstr,using fHlMstr: ir,bb,cb,mat by,mat bp,mat bm eof L710
00540   if ir=0 then goto FdReadGlMstr
00550   if fscode=0 or (fscode=actpd and priorcd=1) then goto L620
00560   if fscode<1 or fscode>13 then fscode=1
00570   if priorcd=1 then cb=by(fscode) else cb=bp(fscode)
00580   if priorcd=2 then goto L610
00590   if fscode>1 then bb=by(fscode-1) else bb=0
00600   goto L620
00610   L610: if fscode>1 then bb=bp(fscode-1) else bb=0
00620   L620: !
00630   L630: if ir=val(r$) then total=total+(cb-bb) else goto L690
00640   total2+=cb
00650   for z=1 to 13 : annualb+=bm(z) : next z
00652   if fscode=0 then 
00654     monthb=monthb+bm(actpd) 
00656   else 
00658     monthb=monthb+bm(fscode)
00660   end if
00662   if fscode=0 then 
00664     for j=1 to actpd : ytdb+=bm(j) : next j 
00666   else 
00668     for j=1 to fscode : ytdb+=bm(j) : next j 
00670   end if
00672 goto FdReadGlMstr

00690   L690: !
00692   if ir<val(r$) then goto FdReadGlMstr
00700   if ir>val(r$) then goto L720
00710   L710: !
00712   notrans=1
00720   L720: !
00722   unexpend=annualb-total2
00730   for j=1 to 9
00740     if ac(j)<>9 then 
00742       accum(j,1)+=annualb : accum(j,2)+=total 
00744       accum(j,3)+=total2 : accum(j,4)+=unexpend
00746     end if
00750   next j
00760   if rs=1 then 
00762     total=-total 
00770     total2=-total2 : annualb=-annualb : unexpend=unexpend
00776   end if
00778   if ds=1 then dollar$="$" else dollar$=" "
00790   if annualb><0 or total2><0 or total<>0 or ls+ds+ul+ic>0 then 
00800     sp2=22-sp-1
00825     if ul=1 then pr #255,using L841: d$(1:sp2),dollar$,"{\ul ",annualb,"}",dollar$,"{\ul ",total,"}",dollar$,"{\ul ",total2,"}",dollar$,"{\ul ",unexpend,"}" pageoflow PGOF : goto L840
00830     pr #255,using L840: d$(1:sp2),dollar$,annualb,dollar$,total,dollar$,total2,dollar$,unexpend pageoflow PGOF
00840     L840: form pos sp,c sp2,pos 22,c 1,pic(--,---,---.##),x 1,c 1,pic(--,---,---.##),x 1,c 1,pic(--,---,---.##),x 1,c 1,pic(---,---,---.##),skip 1
00841     L841: form pos sp,c sp2,pos 22,c 1,c 5,pic(--,---,---.##),c 1,x 1,c 1,c 5,pic(--,---,---.##),c 1,x 1,c 1,c 5,pic(--,---,---.##),c 1,x 1,c 1,c 5,pic(---,---,---.##),c 1,skip 1
00850     total=total2=annualb=unexpend=0
00860     gosub AccumReset
00865     if ul<>1 then gosub PrUnderlines
00880     gosub PrFootnotesA
00882   end if
00890   goto ReadFinStmtLayout ! /r
00900 FinRecT: ! r:
00902   if ap=0 then ap=1
00910   if rs=1 then accum1=-accum(ap,1) else accum1=accum(ap,1)
00920   if rs=1 then accum2=-accum(ap,2) else accum2=accum(ap,2)
00930   if rs=1 then accum3=-accum(ap,3) else accum3=accum(ap,3)
00940   if rs=1 then accum4=accum(ap,4) else accum4=accum(ap,4)
00950   if ds=1 then dollar$="$" else dollar$=" "
00960   sp2=22-sp-1
00964   if ul=1 then 
00966     pr #255,using L841: d$(1:sp2),dollar$,"{\ul ",accum1,"}",dollar$,"{\ul ",accum2,"}",dollar$,"{\ul ",accum3,"}",dollar$,"{\ul ",accum4,"}" pageoflow PGOF
00968   else
00970     pr #255,using L840: d$(1:sp2),dollar$,accum1,dollar$,accum2,dollar$,accum3,dollar$,accum4 pageoflow PGOF
00972   end if
00980   gosub AccumReset
00985   if ul<>1 then gosub PrUnderlines
01000   gosub PrFootnotesA
01010 goto ReadFinStmtLayout ! /r
01030 FinRecRandS: ! r:
01032   if te$="R" then 
01034     report$=d$
01040   else if te$="S" then 
01042     secondr$=d$
01044   end if
01050   gosub PrFootnotesA
01060 goto ReadFinStmtLayout ! /r
01080 FinRecF: ! r:
01082   if foot1=1 then 
01084     foot$=rtrm$(foot$)&d$
01086   else
01090     tabnote=sp
01100     foot1=1
01110     foot$=d$
01120   end if
01150 goto ReadFinStmtLayout ! /r
01170 AccumReset: ! r: reset mat accum
01172   for j=1 to 9
01180     if ac(j)<>0 and ac(j)<>9 then 
01190       accum(j,1)=accum(j,2)=accum(j,3)=accum(j,4)=0
01200     end if
01202   next j
01210 return ! /r
01230 PrFootnotesA: ! r: footnotes
01232   if ls<>0 then 
01240     if ls=99 then 
01242       gosub PrHeaderSecondary
01244     else
01250       pr #255,using L1260: " "
01260       L1260: form pos 1,c 1,skip ls
01270     end if
01272   end if
01274 return ! /r
01290 PrHeaderSecondary: ! r:
01300   fnpglen(pglen)
01310   sk=pglen-krec(255): fl=len(rtrm$(foot$))
01330   pr #255,using L1340: rtrm$(foot$),"Page "&str$(pt1)
01340   L1340: form skip sk,pos tabnote,c fl,pos 74,c 8,skip 1
01350   if eofcode<>1 then 
01360     pr #255: newpage
01370     gosub PrHeaderPrimary
01372   end if
01390 return ! /r
01400 PGOF: gosub PrHeaderSecondary: continue 
01410 PrUnderlines: ! r:
01412   if ul=0 then goto L1500
01420   if ul=1 then goto L1470
01430   underlin$="=============="
01450   goto L1480
01460   ! 
01470   L1470: underlin$="______________"
01480   L1480: pr #255,using L1490: underlin$,underlin$,underlin$,underlin$
01490   L1490: form pos 22,3*c 15,x 1,c 15,skip 1
01500   L1500: ! if redir=0 then pr #255: ""
01510 return ! /r
01530 PrHeaderPrimary: ! r:
01532   heading=1
01540   pt1+=1
01550   pr #255: "\qc  {\f181 \fs24 \b "&env$('cnam')&"}"
01560   pr #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
01570   if trim$(secondr$)<>"" then pr #255: "\qc  {\f181 \fs18 \b "&trim$(secondr$)&"}"
01580   pr #255: "\qc  {\f181 \fs16 \b For the "&rtrm$(actpd$)&" month period ended "&rtrm$(fnpedat$)&"}"
01590   pr #255: "\ql "
01600   pr #255: 
01610   pr #255,using L1620: "Annual",lpad$(rtrm$(cch$),20),"Year To Date"," Budget"
01620   L1620: form pos 29,c 6,pos 35,cc 20,pos 55,c 15,pos 73,c 7,skip 1
01630   pr #255: tab(29);"Budget";tab(41);"Balance";tab(56);"  Balance";tab(72);"Over/Under"
01640   pr #255: 
01650 return ! /r
01670 FINIS: ! r:
01672   eofcode=1
01680   gosub PrHeaderSecondary
01690   fnfscode(actpd)
01691   fnpriorcd(1)
01700   fncloseprn
01710 goto XIT ! /r
01730 XIT: fnxit
01750 ! <updateable region: ertn>
01760 ERTN: fnerror(program$,err,line,act$,"xit")
01770   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
01780   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01790   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01800 ERTN_EXEC_ACT: execute act$ : goto ERTN
01810 ! /region
