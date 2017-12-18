00010 ! Replace S:\acsGL\AcGlInc4p
00020 ! -- pr INCOME STATEMENT WITH BUDGET and percent remaining
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnpglen,fnerror,fnprocess,fncno,fnpedat$,fnps,fnpriorcd,fnfscode,fnactpd$,fncch$,fnglfs,fnactpd,fnconsole
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim fl1$*256,actpd$*6,cogl$(3)*12,pedat$*20,cch$*20,p$(20)*50
00080   dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*14
00090   dim cnam$*40,b$*3,a$(8)*30,oldtrans$*16,g(8),accum(9,7)
00100   dim pedat$*20,actpd$*6,bm(13),d(2),bp(13),by(13),cap$*128,udf$*256
00110 ! ______________________________________________________________________
00120   fntop(program$,cap$="Four Column Budget With Percent")
00130   fncno(cno,cnam$)
00140   udf$=env$('temp')&'\'
00141   fscode=fnfscode
00142   priorcd=fnpriorcd
00150   if fnglfs=5 then goto XIT !:
          ! sets fnps,fnpriorcd,fnfscode (primary/secondary,current year/Prior,period to print)
00160   cch$=fncch$
00170   pedat$=fnpedat$
00180   actpd$=fnactpd$
00190   actpd=fnactpd
00200   fscode=fnfscode
00210   priorcd=fnpriorcd
00220 ! ______________________________________________________________________
00230   pors=1
00240   if fnps=2 then mp1=72 !:
          fl1$="Name="&env$('Q')&"\GLmstr\ACGLFNSJ.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\FNSJINDX.h"&env$('cno')&",Shr" else !:
          mp1=69 !:
          fl1$="Name="&env$('Q')&"\GLmstr\ACGLFNSI.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\FNSIINDX.h"&env$('cno')&",Shr"
00250   open #1: fl1$,internal,input,keyed 
00260   fnopenprn !:
        if file$(255)(1:4)<>"PRN:" then redir=1 else redir=0
00270   on fkey 5 goto L1670
00280   report$="Statement of Income and Expenses"
00290   if fnps=2 then goto L320 ! secondary
00300   execute "Index "&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&" "&udf$&"fsindex.H"&env$('cno')&" 69 3 Replace DupKeys -N"
00310   goto L340
00320 L320: execute "Index "&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&" "&udf$&"fsindex.H"&env$('cno')&" 72 3 Replace DupKeys -N"
00330   fnconsole(off=0)
00340 L340: open #3: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&",KFName="&udf$&"fsindex.h"&env$('cno')&",Shr",internal,input,keyed 
00350 L350: read #1,using L400: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof L1670
00360   if ltrm$(r$)="" or ltrm$(r$)="0" then goto L350
00370   if costcntr=0 then goto L400
00380   if fc=0 and te$="F" then goto L410
00390   if costcntr><fc then goto L350
00400 L400: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
00410 L410: if te$="S" or te$="F" then goto L430
00420   if heading=0 and te$><"R" then gosub HDR
00430 L430: on pos ("RFHDTS",te$,1) goto L1030,L1080,L440,L500,L900,L1030 none L350
00440 L440: pr #255,using L450: d$(1:40)
00450 L450: form pos sp,c 40
00460   gosub L1230
00470   gosub L1170
00480   goto L350
00490 ! ______________________________________________________________________
00500 L500: if notrans=1 then goto L720
00510   if ir>=val(r$) and val(r$)><0 then goto L630
00520 L520: ! read amounts for gl master file
00530 L530: read #3,using L620: ir,bb,cb,mat by,mat bp,mat bm eof L710
00540   if ir=0 then goto L530
00550   if fscode=0 or (fscode=actpd and priorcd=1) then goto L620
00560   if fscode<1 or fscode>13 then fscode=1
00570   if priorcd=1 then cb=by(fscode) else cb=bp(fscode)
00580   if priorcd=2 then goto L610
00590   if fscode>1 then bb=by(fscode-1) else bb=0
00600   goto L620
00610 L610: if fscode>1 then bb=bp(fscode-1) else bb=0
00620 L620: form pos mp1,pd 3,pos 81,41*pd 6.2
00630 L630: if ir=val(r$) then total=total+(cb-bb) else goto L690
00640   total2+=cb
00650   for z=1 to 13 : annualb+=bm(z) : next z
00660   if fscode=0 then monthb=monthb+bm(actpd) else !:
          monthb=monthb+bm(fscode)
00670   if fscode=0 then !:
          for j=1 to actpd : ytdb+=bm(j) : next j : goto L520 !:
        else !:
          for j=1 to fscode : ytdb+=bm(j) : next j : goto L520
00680   goto L520
00690 L690: if ir<val(r$) then goto L520
00700   if ir>val(r$) then goto L720
00710 L710: notrans=1
00720 L720: unexpend=annualb-total2
00730   for j=1 to 9
00740     if ac(j)<>9 then !:
            accum(j,1)+=annualb : accum(j,2)+=total !:
            accum(j,3)+=total2 : accum(j,4)+=unexpend
00750   next j
00760   if rs=1 then total=-total else goto L780
00770   total2=-total2 : annualb=-annualb : unexpend=unexpend
00780 L780: if ds=1 then dollar$="$" else dollar$=" "
00790   if annualb><0 or total2><0 then goto L820
00800   if total<>0 then goto L820
00810   if ls+ds+ul+ic>0 then goto L820 else goto L350
00820 L820: sp2=22-sp-1
00825   if ul=1 then pr #255,using L841: d$(1:sp2),dollar$,"{\ul ",annualb,"}",dollar$,"{\ul ",total,"}",dollar$,"{\ul ",total2,"}",dollar$,"{\ul ",unexpend,"}",percnt pageoflow L1400 : goto L840
00830   pr #255,using L840: d$(1:sp2),dollar$,annualb,dollar$,total,dollar$,total2,dollar$,unexpend,percnt pageoflow L1400
00840 L840: form pos sp,c sp2,pos 22,c 1,pic(--,---,---.##),x 1,c 1,pic(--,---,---.##),x 1,c 1,pic(--,---,---.##),x 1,c 1,pic(---,---,---.##),pic(--------.--),skip redir
00841 L841: form pos sp,c sp2,pos 22,c 1,c 5,pic(--,---,---.##),c 1,x 1,c 1,c 5,pic(--,---,---.##),c 1,x 1,c 1,c 5,pic(--,---,---.##),c 1,x 1,c 1,c 5,pic(---,---,---.##),c 1,pic(--------.--),skip redir
00850   total=total2=annualb=unexpend=0
00860   gosub L1170
00865   if ul=1 then goto L880
00870   gosub L1410
00880 L880: gosub L1230
00890   goto L350
00900 L900: if ap=0 then ap=1
00910   if rs=1 then accum1=-accum(ap,1) else accum1=accum(ap,1)
00920   if rs=1 then accum2=-accum(ap,2) else accum2=accum(ap,2)
00930   if rs=1 then accum3=-accum(ap,3) else accum3=accum(ap,3)
00940   if rs=1 then accum4=accum(ap,4) else accum4=accum(ap,4)
00950   if ds=1 then dollar$="$" else dollar$=" "
00960   sp2=22-sp-1
00965   if ul=1 then pr #255,using L841: d$(1:sp2),dollar$,"{\ul ",accum1,"}",dollar$,"{\ul ",accum2,"}",dollar$,"{\ul ",accum3,"}",dollar$,"{\ul ",accum4,"}",percnt pageoflow L1400 : goto L980
00970   pr #255,using L840: d$(1:sp2),dollar$,accum1,dollar$,accum2,dollar$,accum3,dollar$,accum4,percnt pageoflow L1400
00980 L980: gosub L1170
00985   if ul=1 then goto L1000
00990   gosub L1410
01000 L1000: gosub L1230
01010   goto L350
01020 ! ______________________________________________________________________
01030 L1030: if te$="R" then report$=d$
01040   if te$="S" then secondr$=d$
01050   gosub L1230
01060   goto L350
01070 ! ______________________________________________________________________
01080 L1080: if foot1=1 then goto L1140
01090   tabnote=sp
01100   foot1=1
01110   foot$=d$
01120   goto L350
01130 ! ______________________________________________________________________
01140 L1140: foot$=rtrm$(foot$)&d$
01150   goto L350
01160 ! ______________________________________________________________________
01170 L1170: for j=1 to 9
01180     if ac(j)=0 or ac(j)=9 then goto L1200 ! 10/14/87
01190     accum(j,1)=accum(j,2)=accum(j,3)=accum(j,4)=0
01200 L1200: next j
01210   return 
01220 ! ______________________________________________________________________
01230 L1230: if ls=0 then goto L1380
01240   if ls=99 then goto L1290
01250   pr #255,using L1260: " "
01260 L1260: form pos 1,c 1,skip ls
01270   goto L1380
01280 ! ______________________________________________________________________
01290 L1290: fnpglen(pglen)
01300 ! If PGLEN<>42 Then pGLEN=58
01310   sk=pglen-krec(255): fl=len(rtrm$(foot$))
01320 ! If PGLEN=42 Then sK=SK+1
01330   pr #255,using L1340: rtrm$(foot$),"Page "&str$(pt1)
01340 L1340: form skip sk,pos tabnote,c fl,pos 74,c 8,skip 1
01350   if eofcode=1 then goto L1380
01360   pr #255: newpage
01370   gosub HDR
01380 L1380: return 
01390 ! ______________________________________________________________________
01400 L1400: gosub L1290: continue 
01410 L1410: if ul=0 then goto L1500
01420   if ul=1 then goto L1470
01430   underlin$="=============="
01440   pr #255: 
01450   goto L1480
01460 ! ______________________________________________________________________
01470 L1470: underlin$="______________"
01480 L1480: pr #255,using L1490: underlin$,underlin$,underlin$,underlin$ ! ,UNDERLIN$(1:10)
01490 L1490: form pos 22,3*c 15,x 1,c 15,c 15,skip redir
01500 L1500: if redir=0 then pr #255: ""
01510   return 
01520 ! ______________________________________________________________________
01530 HDR: heading=1
01540   pt1+=1
01550   pr #255: "\qc  {\f181 \fs24 \b "&env$('cnam')&"}"
01560   pr #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
01570   if trim$(secondr$)<>"" then pr #255: "\qc  {\f181 \fs18 \b "&trim$(secondr$)&"}"
01580   pr #255: "\qc  {\f181 \fs16 \b For the "&rtrm$(actpd$)&" month period ended "&rtrm$(fnpedat$)&"}"
01590   pr #255: "\ql "
01600   pr #255: 
01610   pr #255,using L1620: "Annual",lpad$(rtrm$(cch$),20),"Year To Date"," Budget","Percent"
01620 L1620: form pos 29,c 6,pos 35,cc 20,pos 55,c 15,pos 73,c 7,pos 84,c 7,skip 1
01630   pr #255: tab(29);"Budget";tab(41);"Balance";tab(56);"  Balance";tab(72);"Over/Under";tab(83);"Remaining"
01640   pr #255: 
01650   return 
01660 ! ______________________________________________________________________
01670 L1670: eofcode=1
01680   gosub L1290
01690 ! 
01700   fncloseprn
01705   fnfscode(actpd)
01706   fnpriorcd(1)
01710   goto XIT
01720 ! ______________________________________________________________________
01730 XIT: fnxit
01740 ! ______________________________________________________________________
01750 ! <updateable region: ertn>
01760 ERTN: fnerror(program$,err,line,act$,"xit")
01770   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
01780   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01790   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01800 ERTN_EXEC_ACT: execute act$ : goto ERTN
01810 ! /region
