00010 ! Replace S:\acsGL\AcGlIncO
00020 ! -- gasb 4 column budget statement with original and final budget
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnpglen,fnerror,fnprocess,fncno,fnUseDeptNo,fnpedat$,fnps,fnpriorcd,fnfscode,fnactpd$,fncch$,fnGlAskFormatPriorCdPeriod,fnactpd,fnTos,fnLbl,fnTxt,fnCmdKey,fnAcs
00050   on error goto Ertn
00060 ! ______________________________________________________________________
00070   dim fl1$*256,actpd$*6,cogl$(3)*12,pedat$*20,cch$*20,p$(20)*50
00080   dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*14
00090   dim cnam$*40,b$*3,a$(8)*30,oldtrans$*16,g(8),accum(9,7)
00100   dim pedat$*20,actpd$*6,bm(13),d(2),bp(13),by(13),revb(13),cap$*128,udf$*256
00110 ! ______________________________________________________________________
00120   fntop(program$,cap$="Income Statement with GASB Budget")
00130   fncno(cno,cnam$)
00140   udf$=env$('temp')&'\'
00150   if fnGlAskFormatPriorCdPeriod=5 then goto XIT !:
          ! sets fnps,fnpriorcd,fnfscode (primary/secondary,current year/Prior,period to print)
00160   cch$=fncch$
00170   pedat$=fnpedat$
00180   actpd=fnactpd
00190   actpd$=fnactpd$
00200   fscode=fnfscode
00210   priorcd=fnpriorcd
00220 ! ______________________________________________________________________
00230   pors=1
00240   if fnps=2 then fl1$="Name=[Q]\GLmstr\ACGLFNSJ.h[cno],KFName=[Q]\GLmstr\FNSJINDX.h[cno],Shr" : mp1=72 else !:
          fl1$="Name=[Q]\GLmstr\ACGLFNSI.h[cno],KFName=[Q]\GLmstr\FNSIINDX.h[cno],Shr" : mp1=69
00250   open #1: fl1$,internal,input,keyed 
00260   if fnprocess=1 or fnUseDeptNo=0 then goto L360
00270   fnTos(sn$="ACglinco") !:
        mylen=30: mypos=mylen+3 : right=1
00280   fnLbl(1,1,"Cost Center or Department #:",mylen,right)
00290   fnTxt(1,mypos,3,0,right,"30",0,"Enter the cost center or department number if you wish to pr only one department, else leave blank for all.",0 ) !:
        resp$(1)=""
00300   fnLbl(2,1,"(Blank for all Departments)",mylen,right)
00310   fnCmdKey("&Next",1,1,0,"Prints the financial statement.")
00320   fnCmdKey("&Cancel",5,0,1,"Returns to menu without posting.")
00330   fnAcs(sn$,0,mat resp$,ckey)
00340   if ckey=5 then goto XIT
00350   costcntr=val(resp$(1))
00360 L360: fnopenprn !:
        if file$(255)(1:4)<>"PRN:" then redir=1 else redir=0
00370   report$="Budgetary Comparison Schedule"
00380   if fnps=2 then goto L410 ! secondary
00390   execute "Index [Q]\GLmstr\GLmstr.h[cno] "&udf$&"fsindex.H[cno] 69 3 Replace DupKeys -N"
00400   goto L420
00410 L410: execute "Index [Q]\GLmstr\GLmstr.h[cno] "&udf$&"fsindex.H[cno] 72 3 Replace DupKeys -N"
00420 L420: open #3: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName="&udf$&"fsindex.h[cno],Shr",internal,input,keyed 
00430 L430: read #1,using L480: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof L1920
00440   if ltrm$(r$)="" or ltrm$(r$)="0" then goto L430
00450   if costcntr=0 then goto L480
00460   if fc=0 and te$="F" then goto L490
00470   if costcntr><fc then goto L430
00480 L480: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
00490 L490: if te$="S" or te$="F" then goto L510
00500   if heading=0 and te$><"R" then gosub L1750
00510 L510: on pos ("RFHDTS",te$,1) goto L1280,L1320,L520,L570,L1160,L1280 none L430
00520 L520: pr #255,using L530: d$(1:40)
00530 L530: form pos sp,c 40,skip 1
00540   gosub L1470
00550   gosub L1390
00560   goto L430
00570 L570: if notrans=1 then goto L890
00580   if ir>=val(r$) and val(r$)><0 then goto L700
00590 L590: ! read amounts from gl master file
00600 L600: read #3,using L690: ir,bb,cb,mat by,mat bp,mat bm,mat revb eof L880
00610   if ir=0 then goto L600
00620   if fscode=0 or (fscode=actpd and priorcd=1) then goto L690
00630   if fscode<1 or fscode>13 then fscode=1
00640   if priorcd=1 then cb=by(fscode) else cb=bp(fscode)
00650   if priorcd=2 then goto L680
00660   if fscode>1 then bb=by(fscode-1) else bb=0
00670   goto L690
00680 L680: if fscode>1 then bb=bp(fscode-1) else bb=0
00690 L690: form pos mp1,pd 3,pos 81,41*pd 6.2,pos 339,13*pd 6.2
00700 L700: if ir=val(r$) then goto L710 else goto L860
00710 L710: total2=total2+cb
00720   for z=1 to 13
00730     annualb=annualb+bm(z)
00740     finalb=finalb+revb(z)
00750   next z
00760   if fscode=0 then monthb=monthb+bm(actpd) else monthb=monthb+bm(fscode) ! 11/24/86
00770   if fscode=0 then goto L780 else goto L820 ! 11/24/86
00780 L780: for j=1 to actpd
00790     ytdb=ytdb+bm(j)
00800   next j
00810   goto L590
00820 L820: for j=1 to fscode ! 11/24/86
00830     ytdb=ytdb+bm(j) ! 11/24/86
00840   next j ! 11/24/86
00850   goto L590 ! 11/24/86
00860 L860: if ir<val(r$) then goto L590
00870   if ir>val(r$) then goto L890
00880 L880: notrans=1
00890 L890: unexpend=finalb-total2
00900   for j=1 to 9
00910     if ac(j)=9 then goto L960 ! 10/14/87
00920     accum(j,1)=accum(j,1)+annualb
00930     accum(j,2)=accum(j,2)+finalb
00940     accum(j,3)=accum(j,3)+total2
00950     accum(j,4)=accum(j,4)+unexpend
00960 L960: next j
00970   if rs=1 then finalb=-finalb else goto L1010
00980   total2=-total2
00990   annualb=-annualb
01000   unexpend=unexpend
01010 L1010: if ds=1 then dollar$="$" else dollar$=" "
01020   if annualb><0 or total2><0 then goto L1050
01030   if finalb<>0 then goto L1050
01040   if ls+ds+ul+ic>0 then goto L1050 else goto L430
01050 L1050: sp2=22-sp-1
01059   if ul=1 then pr #255,using L1071: d$(1:sp2),dollar$,"{\ul ",annualb,"}",dollar$,"{\ul ",finalb,"}",dollar$,"{\ul ",total2,"}",dollar$,"{\ul ",unexpend,"}" pageoflow L1620 : goto L1070
01060   pr #255,using L1070: d$(1:sp2),dollar$,annualb,dollar$,finalb,dollar$,total2,dollar$,unexpend pageoflow L1620
01070 L1070: form pos sp,c sp2,pos 22,c 1,pic(--,---,---.##),x 1,c 1,pic(--,---,---.##),x 1,c 1,pic(--,---,---.##),x 1,c 1,pic(--,---,---.##),skip redir
01071 L1071: form pos sp,c sp2,pos 22,c 1,c 5,pic(--,---,---.##),c 1,x 1,c 1,c 5,pic(--,---,---.##),c 1,x 1,c 1,c 5,pic(--,---,---.##),c 1,x 1,c 1,c 5,pic(--,---,---.##),c 1,skip redir
01080   finalb=0
01090   total2=0
01100   annualb=0
01110   unexpend=0
01120   gosub L1390
01125   if ul=1 then goto L1140
01130   gosub L1630
01140 L1140: gosub L1470
01150   goto L430
01160 L1160: if ap=0 then ap=1
01170   if rs=1 then accum1=-accum(ap,1) else accum1=accum(ap,1)
01180   if rs=1 then accum2=-accum(ap,2) else accum2=accum(ap,2)
01190   if rs=1 then accum3=-accum(ap,3) else accum3=accum(ap,3)
01200   if rs=1 then accum4=accum(ap,4) else accum4=accum(ap,4)
01210   if ds=1 then dollar$="$" else dollar$=" "
01220   sp2=22-sp-1
01228   if ul=1 then pr #255,using L1071: d$(1:sp2),dollar$,"{\ul ",accum1,"}",dollar$,"{\ul ",accum2,"}",dollar$,"{\ul ",accum3,"}",dollar$,"{\ul ",accum4,"}" pageoflow L1620 : goto L1240
01230   pr #255,using L1070: d$(1:sp2),dollar$,accum1,dollar$,accum2,dollar$,accum3,dollar$,accum4 pageoflow L1620
01240 L1240: gosub L1390
01245   if ul=1 then goto L1260
01250   gosub L1630
01260 L1260: gosub L1470
01270   goto L430
01280 L1280: if te$="R" then report$=d$
01290   if te$="S" then secondr$=d$
01300   gosub L1470
01310   goto L430
01320 L1320: if foot1=1 then goto L1370
01330   tabnote=sp
01340   foot1=1
01350   foot$=d$
01360   goto L430
01370 L1370: foot$=rtrm$(foot$)&d$
01380   goto L430
01390 L1390: for j=1 to 9
01400     if ac(j)=0 or ac(j)=9 then goto L1450 ! 10/14/87
01410     accum(j,1)=0
01420     accum(j,2)=0
01430     accum(j,3)=0
01440     accum(j,4)=0
01450 L1450: next j
01460   return 
01470 L1470: if ls=0 then goto L1610
01480   if ls=99 then goto L1520
01490   pr #255,using L1500: " "
01500 L1500: form pos 1,c 1,skip ls
01510   goto L1610
01520 L1520: fnpglen(pglen)
01530 ! If PGLEN<>42 Then pGLEN=58
01540   sk=pglen-krec(255): fl=len(rtrm$(foot$))
01550 ! If PGLEN=42 Then sK=SK+1
01560   pr #255,using L1570: rtrm$(foot$),"Page "&str$(pt1)
01570 L1570: form skip sk,pos tabnote,c fl,pos 75,c 8,skip 1
01580   if eofcode=1 then goto L1610
01590   pr #255: newpage
01600   gosub L1750
01610 L1610: return 
01620 L1620: gosub L1520: continue 
01630 L1630: if ul=0 then goto L1720
01640   if ul=1 then goto L1690
01650   underlin$="=============="
01660 ! pr #255:
01670   goto L1700
01680   goto L1720
01690 L1690: underlin$="______________"
01700 L1700: pr #255,using L1710: underlin$,underlin$,underlin$,underlin$
01710 L1710: form pos 22,4*c 15,skip redir
01720 L1720: if redir=0 then pr #255,using L1730: " "
01730 L1730: form skip 1,c 1,skip 0
01740   return 
01750 L1750: heading=1
01760   pt1+=1
01770   pr #255: "\qc  {\f181 \fs24 \b "&env$('cnam')&"}"
01780   pr #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
01790   if trim$(secondr$)<>"" then pr #255: "\qc  {\f181 \fs18 \b "&trim$(secondr$)&"}"
01800   pr #255: "\qc  {\f181 \fs16 \b For the "&rtrm$(actpd$)&" month period ended "&rtrm$(fnpedat$)&"}"
01810   pr #255: "\ql "
01820   pr #255: 
01830   pr #255: tab(67);"Varience with"
01840   pr #255: tab(68);"Final Budget"
01850   pr #255: tab(25);" Original";tab(40);"  Final";tab(56);"";tab(70);"Positive"
01860   pr #255,using L1870: "  Budget","  Budget"," Actual","- Negative"
01870 L1870: form pos 25,c 10,pos 40,c 10,pos 56,c 10,pos 69,c 10,skip redir
01880   pr #255: tab(24);"____________   ____________  _____________   ____________"
01890   pr #255: 
01900   return 
01910 ! ______________________________________________________________________
01920 L1920: eofcode=1
01930   gosub L1520
01940 ! 
01948   fnfscode(actpd)
01949   fnpriorcd(1)
01950   fncloseprn
01960   goto XIT
01970 ! ______________________________________________________________________
01980 XIT: fnxit
01990 ! ______________________________________________________________________
02000 ! <updateable region: ertn>
02010 ERTN: fnerror(program$,err,line,act$,"xit")
02020   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
02030   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
02040   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
02050 ERTN_EXEC_ACT: execute act$ : goto ERTN
02060 ! /region
