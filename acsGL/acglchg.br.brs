00010 ! Replace S:\acsGL\acglChg
00020 ! Statement of Change in Financial Position with Comparrison  !:
        ! FOR 8 1/2 * 11
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnxit,fntop, fnopenprn,fncloseprn,fnpglen,fncno,fnerror,fnpedat$,fnactpd$,fnprocess,fnUseDeptNo,fnps,fnpriorcd,fnactpd,fnfscode,fnGlAskFormatPriorCdPeriod,fnTos,fnLbl,fnTxt,fnCmdKey,fnAcs
00050   on error goto Ertn
00060 ! ______________________________________________________________________
00070   dim fl1$*256,actpd$*6,cogl$(3)*12,pedat$*20,p$(20)*50,cap$*128
00080   dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*14
00090   dim cnam$*40,accum(9,2),acct$*12,bp(13),by(13),udf$*256
00100 ! ______________________________________________________________________
00110   fntop(program$,cap$="Comparative (FP)")
00120   if fnGlAskFormatPriorCdPeriod=5 then goto XIT
00130   fncno(cno,cnam$)
00140   udf$=env$('temp')&'\'
00150   open #20: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,input,relative: read #20,using 'Form Pos 152,3*C 12',rec=1: mat cogl$ : close #20: 
00160   actpd=fnactpd : fscode=fnfscode
00170   pors=1
00180   on fkey 5 goto L1830
00190   mp1=75
00200   if fnps=2 then mp1=mp1+3
00210   fl1$="Name=[Q]\GLmstr\ACGLFNSF.h[cno],KFName=[Q]\GLmstr\FNSFIndx.h[cno],Shr"
00220   if fnps=2 then fl1$="Name=[Q]\GLmstr\ACGLFNSG.h[cno],KFName=[Q]\GLmstr\FNSGIndx.h[cno],Shr"
00230 L230: form pos mp1,pd 3,pos 81,28*pd 6.2,pos 327,pd 6.2
00240 L240: form pos 1,c 12,pos 87,27*pd 6.2
00250   open #1: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno],Shr",internal,input,keyed 
00260 L260: read #1,using L240: acct$,cb,mat by,mat bp eof L340
00270   if acct$>cogl$(3) then goto L340
00280   if fnpriorcd=2 then income=income-bp(fscode) else goto L310
00290   pincome=0
00300   goto L330
00310 L310: if fscode<=0 or fscode>12 then !:
          income=income-cb else !:
          income=income-by(fscode)
00320   if fscode<=0 or fscode>12 then !:
          pincome=pincome-bp(actpd) else !:
          pincome=pincome-bp(fscode)
00330 L330: goto L260
00340 L340: close #1: 
00350   open #1: fl1$,internal,input,keyed 
00360   if fnprocess=1 or fnUseDeptNo=0 then goto L460
00370   fnTos(sn$="ACglchg") !:
        mylen=30: mypos=mylen+3 : right=1
00380   fnLbl(1,1,"Cost Center or Department #:",mylen,right)
00390   fnTxt(1,mypos,3,0,right,"30",0,"Enter the cost center or department number if you wish to pr only one department, else leave blank for all.",0 ) !:
        resp$(1)=""
00400   fnLbl(2,1,"(Blank for all Departments)",mylen,right)
00410   fnCmdKey("&Next",1,1,0,"Prints the financial statement.")
00420   fnCmdKey("&Cancel",5,0,1,"Returns to menu without posting.")
00430   fnAcs(sn$,0,mat resp$,ckey)
00440   if ckey=5 then goto XIT
00450   costcntr=val(resp$(1))
00460 L460: fnopenprn !:
        redir=0: if file$(255)(1:4)<>"PRN:" then redir=1
00470   report$="STATEMENT OF CHANGES IN FINANCIAL POSITION"
00480   if fnps=2 then goto L510 ! secondary
00490   execute "Index [Q]\GLmstr\GLmstr.h[cno] "&udf$&"fsindex.H[cno] 75 3 Replace DupKeys -N"
00500   goto L520
00510 L510: execute "Index [Q]\GLmstr\GLmstr.h[cno] "&udf$&"fsindex.H[cno] 78 3 Replace DupKeys -N"
00520 L520: open #3: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName="&udf$&"fsindex.h[cno],Shr",internal,input,keyed 
00530 L530: read #1,using L570: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof L1830
00540   if ltrm$(r$)="" or ltrm$(r$)="0" then goto L530
00550   if costcntr=0 then goto L570
00560   if costcntr><fc then goto L530
00570 L570: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
00580   if te$="S" or te$="F" then goto L600
00590   if heading=0 and te$><"R" then gosub L1620
00600 L600: on pos ("RFHDTSP",te$,1) goto L1110,L1160,L610,L660,L1000,L1110,L1890 none L530
00610 L610: pr #255,using L620: d$
00620 L620: form pos sp,c 50
00630   gosub L1320
00640   gosub L1250
00650   goto L530
00660 L660: if notrans=1 then goto L840
00670   if fr=val(r$) and val(r$)><0 then goto L750
00680   if fr>val(r$) then goto L750
00690 L690: ! read amounts from gl master file
00700 L700: read #3,using L230: fr,bb,cb,mat by,mat bp,pbp eof L830
00710   if fr=0 then goto L700
00720   if fscode=0 then goto L750
00730   if fscode<1 or fscode>12 then fscode=1
00740   if fnpriorcd=2 then cb=bp(fscode) else cb=by(fscode)
00750 L750: if fr=val(r$) then goto L760 else goto L810
00760 L760: if fnpriorcd=2 then !:
          total=total+(cb-pbp) else !:
          total=total+(cb-bp(12))
00770   if fnpriorcd=2 then total2=0 else goto L790
00780   goto L800
00790 L790: if fscode<=0 or fscode>12 then !:
          total2=total2+(bp(actpd)-pbp) else !:
          total2=total2+(bp(fscode)-pbp)
00800 L800: goto L690
00810 L810: if fr<val(r$) then goto L690
00820   if fr>val(r$) then goto L840
00830 L830: notrans=1
00840 L840: for j=1 to 9 : accum(j,1)+=total : accum(j,2)+=total2 : next j
00850   if rs=1 then total=-total else goto L870
00860   total2=-total2
00870 L870: if ds=1 then dollar$="$" else dollar$=" "
00880   if total><0 or total2><0 then goto L900
00890   if ls+ul+ds+ic>0 then goto L900 else goto L530
00900 L900: sp2=49-sp-1
00910   pr #255,using L920: d$(1:sp2),dollar$,total,dollar$,total2 pageoflow L1480
00920 L920: form pos sp,c sp2,pos 49,c 1,pic(--,---,---.##),pos 67,c 1,pic(--,---,---.##),skip redir
00930   total=0
00940   total2=0
00950   gosub L1250
00960   gosub L1490
00970   gosub L1320
00980   goto L530
00990 ! ______________________________________________________________________
01000 L1000: if ap=0 then ap=1
01010   if rs=1 then accum1=-accum(ap,1) else accum1=accum(ap,1)
01020   if rs=1 then accum2=-accum(ap,2) else accum2=accum(ap,2)
01030   if ds=1 then dollar$="$" else dollar$=" "
01040   sp2=49-sp-1
01050   pr #255,using L920: d$(1:sp2),dollar$,accum1,dollar$,accum2 pageoflow L1480
01060   gosub L1250
01070   gosub L1490
01080   gosub L1320
01090   goto L530
01100 ! ______________________________________________________________________
01110 L1110: if te$="R" then report$=d$
01120   if te$="S" then secondr$=d$
01130   gosub L1320
01140   goto L530
01150 ! ______________________________________________________________________
01160 L1160: if foot1=1 then goto L1220
01170   tabnote=sp
01180   foot1=1
01190   foot$=d$
01200   goto L530
01210 ! ______________________________________________________________________
01220 L1220: foot$=rtrm$(foot$)&d$
01230   goto L530
01240 ! ______________________________________________________________________
01250 L1250: for j=1 to 9
01260     if ac(j)=0 then goto L1290
01270     accum(j,1)=0
01280     accum(j,2)=0
01290 L1290: next j
01300   return 
01310 ! ______________________________________________________________________
01320 L1320: if ls=0 then goto L1460
01330   if ls=99 then goto L1370
01340   pr #255,using L1350: " "
01350 L1350: form pos 1,c 1,skip ls
01360   goto L1460
01370 L1370: fnpglen(pglen)
01380 ! If PGLEN<>42 Then pGLEN=58
01390   sk=pglen-krec(255): fl=len(rtrm$(foot$))
01400 ! If PGLEN=42 Then sK=SK+1
01410   pr #255,using L1420: rtrm$(foot$),"Page "&str$(pt1)
01420 L1420: form skip sk,pos tabnote,c fl,pos 75,c 8,skip 1
01430   if eofcode=1 then goto L1460
01440   pr #255: newpage
01450   gosub L1620
01460 L1460: return 
01470 ! ______________________________________________________________________
01480 L1480: gosub L1370: continue 
01490 L1490: if ul=0 then goto L1580
01500   if ul=1 then goto L1550
01510   underlin$="=============="
01520   pr #255,using L1530: underlin$,underlin$
01530 L1530: form skip 1,pos 49,c 14,pos 67,c 14,skip redir
01540   goto L1580
01550 L1550: underlin$="______________"
01560   pr #255,using L1570: underlin$,underlin$
01570 L1570: form pos 49,c 14,pos 67,c 14,skip redir
01580 L1580: if redir=0 then pr #255,using L1590: " "
01590 L1590: form skip 1,c 1,skip 0
01600   return 
01610 ! ______________________________________________________________________
01620 L1620: heading=1
01630   pt1+=1
01640   pr #255: "\qc  {\f181 \fs24 \b "&env$('cnam')&"}"
01650   pr #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
01660   if trim$(secondr$)<>"" then pr #255: "\qc  {\f181 \fs18 \b "&trim$(secondr$)&"}"
01670   pr #255: "\qc  {\f181 \fs16 \b For the "&rtrm$(fnactpd$)&" month period ended "&rtrm$(fnpedat$)&"}"
01680   pr #255: "\ql "
01690   pr #255: 
01700   on error goto L1780
01710   a=len(rtrm$(fnpedat$))
01720   b=val(rtrm$(fnpedat$(a-4:a)))
01730   c=b-1
01740   pr #255,using L1750: b,c
01750 L1750: form pos 52,pic(-----),pos 71,pic(-----),skip 2
01760   on error goto Ertn
01770   goto L1810
01780 L1780: pr #255: tab(49);"CURRENT YEAR";tab(68);"PRIOR YEAR"
01790   on error goto Ertn
01800   pr #255: 
01810 L1810: return 
01820 ! ______________________________________________________________________
01830 L1830: eofcode=1
01840   gosub L1370
01850 ! 
01860   fncloseprn
01870   goto XIT
01880 ! ______________________________________________________________________
01890 L1890: total=income
01900   total2=pincome
01910   goto L840
01920 ! ______________________________________________________________________
01930 XIT: fnxit
01940 ! ______________________________________________________________________
01950 ! <Updateable Region: ERTN>
01960 ERTN: fnerror(program$,err,line,act$,"xit")
01970   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
01980   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01990   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
02000 ERTN_EXEC_ACT: execute act$ : goto ERTN
02010 ! /region
