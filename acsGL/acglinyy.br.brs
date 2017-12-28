00010 ! Replace S:\acsGL\AcGlinyy
00020 ! Income Statement with Year Comparison
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnpglen,fnerror,fncno,fncch$,fnpedat$,fnactpd$,fnactpd,fnfscode,fnUseDeptNo,fnpriorcd,fnps,fnprocess,fnglfs,fnTos,fnLbl,fnTxt,fnCmdKey,fnAcs
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim fl1$*256,actpd$*6,cogl$(3)*12,pedat$*20,cch$*20,p$(20)*50
00080   dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*22
00090   dim cnam$*40,b$*3,a$(8)*30,oldtrans$*16,g(8),accum(9,4),cap$*128,udf$*256
00100   dim bp(13),d(2),by(13),tp1(4)
00110 ! ______________________________________________________________________
00120   fntop(program$,cap$="Income Statement with Year Comparison")
00130   on fkey 5 goto L2590
00140   fncno(cno,cnam$)
00150   udf$=env$('temp')&'\'
00155   fscode=fnfscode
00156   priorcd=fnpriorcd
00160   if fnglfs=5 then goto XIT !:
          ! sets fnps,fnpriorcd,fnfscode (primary/secondary,current year/Prior,period to print)
00170   cch$=fncch$
00180   pedat$=fnpedat$
00190   actpd=fnactpd
00200   actpd$=fnactpd$
00210   fscode=fnfscode
00220   priorcd=fnpriorcd
00230 ! ______________________________________________________________________
00250   pors=1
00260   fnopenprn(cp,58,220,process)
00270   redir=0: if file$(255)(1:4)<>"PRN:" then redir=1
00280   mp1=69
00290   if fnps=2 then mp1=mp1+3
00300   fl1$="Name="&env$('Q')&"\GLmstr\ACGLFNSI.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\FNSIINDX.h"&env$('cno')&",Shr"
00310   if fnps=2 then fl1$="Name="&env$('Q')&"\GLmstr\ACGLFNSJ.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\FNSJINDX.h"&env$('cno')&",Shr"
00320   form c 9,skip 0
00330 L330: form pos mp1,pd 3,pos 81,41*pd 6.2
00340   form c 7,skip 0
00350   nametab=int(44-len(rtrm$(cnam$))/2)
00360   pas=1 : open #4: "Name="&env$('temp')&"\Work."&session$&",KFName=IDX."&wsid$&",Replace,RecL=33,KPS=1,KLN=5",internal,outIn,keyed 
00370   if actpd>0 and actpd<14 then goto L430
00390   pr f "10,2,C 78": "THIS PROGRAM CANNOT PROCESS WITHOUT THE NUMBER OF THE ACCOUNTING PERIOD END"
00400   pr f "12,2,C 60,N": "USE OPTION 1 ON THE MENU TO ENTER THIS INFORMATION"
00410   input fields "23,2,C 1,E,N": pause$
00420   goto XIT
00430 L430: open #1: fl1$,internal,input,keyed 
00440   if fnprocess=1 or fnUseDeptNo=0 then goto L550
00450   if percent=1 then goto L550
00460   fnTos(sn$="ACGlinyy") !:
        mylen=30: mypos=mylen+3 : right=1
00470   fnLbl(1,1,"Cost Center or Department #:",mylen,right)
00480   fnTxt(1,mypos,3,0,right,"30",0,"Enter the cost center or department number if you wish to pr only one department, else leave blank for all.",0 ) !:
        resp$(1)=""
00490   fnLbl(2,1,"(Blank for all Departments)",mylen,right)
00500   fnCmdKey("&Next",1,1,0,"Prints the financial statement.")
00510   fnCmdKey("&Cancel",5,0,1,"Returns to menu without posting.")
00520   fnAcs(sn$,0,mat resp$,ckey)
00530   if ckey=5 then goto XIT
00540   costcntr=val(resp$(1))
00550 L550: cnam$=rtrm$(cnam$)
00560   pf1=len(cnam$)+int((43-len(cnam$))/2)
00570   close #101: ioerr L580
00580 L580: open #101: "SROW=08,SCOL=18,EROW=12,ECOL=58,BORDER=DR,CAPTION= COMPARATIVE INCOME STATEMENT ",display,outIn 
00590   pr f "08,18,C 41,H,N": lpad$(cnam$,pf1)
00600   pr f "09,18,C 41,H,N": "            COMPANY NUMBER "&env$('cno')
00610   pr f "11,18,C 41,R,N": "              IN PROCESS"
00620   pr f "13,30,C 16,R,N": "PRESS F5 TO STOP"
00630   if cmdkey=5 then goto L2610
00640   report$="STATEMENT OF INCOME AND EXPENSES"
00650   fnopenprn(cp,58,220,process)
00660   redir=0: if file$(255)(1:4)<>"PRN:" then redir=1
00670   if fnps=2 then goto L700 ! secondary
00680   execute "Index "&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&" "&udf$&"fsindex.H"&env$('cno')&" 69 3 Replace DupKeys -N"
00690   goto L710
00700 L700: execute "Index "&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&" "&udf$&"fsindex.H"&env$('cno')&" 72 3 Replace DupKeys -N"
00710 L710: open #3: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&",KFName="&udf$&"fsindex.h"&env$('cno')&",Shr",internal,input,keyed 
00720 L720: ! 
00730 L730: read #1,using L780: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc,rnp eof L2510
00740   if ltrm$(r$)="" or ltrm$(r$)="0" then goto L720
00750   if costcntr=0 then goto L780
00760   if fc=0 and te$="F" then goto L790 ! 5/08/1989
00770   if costcntr><fc then goto L720
00780 L780: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3,n 5
00790 L790: if te$="S" or te$="F" then goto L810
00800   if heading=0 and te$><"R" then gosub L2310
00810 L810: on pos ("RFHDTS",te$,1) goto L1820,L1860,L820,L880,L1540,L1820 none L720
00820 L820: if percent=0 then goto L860
00830   pr #255,using L840: d$(1:40)
00840 L840: form pos sp,c 40,skip 1
00850   gosub L2010
00860 L860: gosub L1930
00870   goto L720
00880 L880: if notrans=1 then goto L1120
00890   if ir>=val(r$) and val(r$)><0 then goto L1010
00900 L900: ! read amounts from gl master file
00910   form pd 3
00920 L920: read #3,using L330: ir,bb,cb,mat by,mat bp eof L1110
00930   if ir=0 then goto L920
00940   if fscode=0 or (fscode=actpd and priorcd=1) then goto L1010
00950   if fscode<1 or fscode>13 then fscode=1 ! 6/8/88
00960   if priorcd=1 then cb=by(fscode) else cb=bp(fscode)
00970   if priorcd=2 then goto L1000
00980   if fscode>1 then bb=by(fscode-1) else bb=0
00990   goto L1010
01000 L1000: if fscode>1 then bb=bp(fscode-1) else bb=0
01010 L1010: if ir=val(r$) then total=total+(cb-bb) else goto L1090
01020   total2=total2+cb
01030   if fscode>1 then goto L1040 else goto L1060
01040 L1040: total3=total3+(bp(fscode)-bp(fscode-1))
01050   goto L1070
01060 L1060: total3=total3+bp(fscode)
01070 L1070: total4=total4+bp(fscode)
01080   goto L900
01090 L1090: if ir<val(r$) then goto L900
01100   if ir>val(r$) then goto L1120
01110 L1110: notrans=1
01120 L1120: for j=1 to 9
01130     if ac(j)=9 then goto L1180 ! 10/14/87
01140     accum(j,1)=accum(j,1)+total
01150     accum(j,2)=accum(j,2)+total2
01160     accum(j,3)=accum(j,3)+total3
01170     accum(j,4)=accum(j,4)+total4
01180 L1180: next j
01190   if rs=1 then total=-total else goto L1230
01200   total2=-total2
01210   total3=-total3
01220   total4=-total4
01230 L1230: if ds=1 then dollar$="$" else dollar$=" "
01240   if ds=1 then percent$="%" else percent$=" "
01250   if total2><0 or total4><0 then goto L1280
01260   if total><0 or total3><0 then goto L1280
01270   if ls+ds+ul>0 then goto L1280 else goto L730
01280 L1280: if percent=0 then goto L1450
01290   sp2=31-sp-1
01300   if pas=2 then gosub PAS2
01310   if percent1=0 then pdpct=0 else pdpct=total/percent1*100
01320   if percent2=0 then ytdpct=0 else ytdpct=total2/percent2*100
01330   if percent3=0 then pppd=0 else pppd=total3/percent3*100
01340   if percent4=0 then ppyear=0 else ppyear=total4/percent4*100
01350   if pdpct<-999.99 then pdpct=-999.99
01360   if pdpct>999.99 then pdpct=999.99
01370   if ytdpct<-999.99 then ytdpct=-999.99
01380   if ytdpct>999.99 then ytdpct=999.99
01390   if ppyear<-999.99 then ppyear=-999.99
01400   if ppyear>999.99 then ppyear=999.99
01410   if pppd<-999.99 then pppd=-999.99
01420   if pppd>999.99 then pppd=999.99
01425   if ul=1 then pr #255,using L1441: d$(1:sp2),dollar$,"{\ul ",total2,"}",ytdpct,percent$,dollar$,"{\ul ",total4,"}",ppyear,percent$ pageoflow L2170 : goto L1440
01430   pr #255,using L1440: d$(1:sp2),dollar$,total2,ytdpct,percent$,dollar$,total4,ppyear,percent$ pageoflow L2170
01440 L1440: form pos sp,c sp2,pos 31,c 1,pic(---,---,---.##),pic(----.##),c 2,c 1,pic(---,---,---.##),pic(----.##),c 2,skip redir
01441 L1441: form pos sp,c sp2,pos 31,c 1,c 5,pic(---,---,---.##),c 1,pic(----.##),c 2,c 1,c 5,pic(---,---,---.##),c 1,pic(----.##),c 2,skip redir
01450 L1450: if pas=1 then tp1=total : tp2=total2 : tp3=total3 : tp4=total4 : gosub PAS1
01460   total=0
01470   total2=0
01480   total3=0
01490   total4=0
01500   gosub L1930
01505   if ul=1 then goto L1520
01510   gosub L2180
01520 L1520: gosub L2010
01530   goto L720
01540 L1540: if ap=0 then ap=1
01550   if rs=1 then accum1=-accum(ap,1) else accum1=accum(ap,1)
01560   if rs=1 then accum2=-accum(ap,2) else accum2=accum(ap,2)
01570   if rs=1 then accum3=-accum(ap,3) else accum3=accum(ap,3)
01580   if rs=1 then accum4=-accum(ap,4) else accum4=accum(ap,4)
01590   if ds=1 then dollar$="$" else dollar$=" "
01600   if ds=1 then percent$="%" else percent$=" "
01610   if pas=2 then gosub PAS2
01620   if percent=0 then goto L1770
01630   if percent1=0 then pdpct=0 else pdpct=accum1/percent1*100
01640   if percent2=0 then ytdpct=0 else ytdpct=accum2/percent2*100
01650   if percent3=0 then pppd=0 else pppd=accum3/percent3*100
01660   if percent4=0 then ppyear=0 else ppyear=accum4/percent4*100
01670   if pdpct<-999.99 then pdpct=-999.99
01680   if pdpct>999.99 then pdpct=999.99
01690   if ytdpct<-999.99 then ytdpct=-999.99
01700   if ytdpct>999.99 then ytdpct=999.99
01710   if pppd<-999.99 then pppd=-999.99
01720   if pppd>999.99 then pppd=999.99
01730   if ppyear<-999.99 then ppyear=-999.99
01740   if ppyear>999.99 then ppyear=999.99
01750   sp2=31-sp-1
01759   if ul=1 then pr #255,using L1441: d$(1:sp2),dollar$,"{\ul ",accum2,"}",ytdpct,percent$,dollar$,"{\ul ",accum4,"}",ppyear,percent$ pageoflow L2170 : goto L1770
01760   pr #255,using L1440: d$(1:sp2),dollar$,accum2,ytdpct,percent$,dollar$,accum4,ppyear,percent$ pageoflow L2170
01770 L1770: if pas=1 then tp1=accum1: tp2=accum2 : tp3=accum3 : tp4=accum4 : gosub PAS1
01780   gosub L1930
01785   if ul=1 then goto L1800
01790   gosub L2180
01800 L1800: gosub L2010
01810   goto L720
01820 L1820: if te$="R" then report$=d$
01830   if te$="S" then secondr$=d$
01840   gosub L2010
01850   goto L720
01860 L1860: if foot1=1 then goto L1910
01870   tabnote=sp
01880   foot1=1
01890   foot$=d$
01900   goto L720
01910 L1910: foot$=rtrm$(foot$)&d$
01920   goto L720
01930 L1930: for j=1 to 9
01940     if ac(j)=0 or ac(j)=9 then goto L1990 ! 10/14/87
01950     accum(j,1)=0
01960     accum(j,2)=0
01970     accum(j,3)=0
01980     accum(j,4)=0
01990 L1990: next j
02000   return 
02010 L2010: if percent=0 then goto L2160
02020   if ls=0 then goto L2160
02030   if ls=99 then goto L2070
02040   pr #255,using L2050: " "
02050 L2050: form pos 1,c 1,skip ls
02060   goto L2160
02070 L2070: fnpglen(pglen)
02080 ! If PGLEN<>42 Then pGLEN=58
02090   sk=pglen-krec(255): fl=len(rtrm$(foot$))
02100 ! If PGLEN=42 Then sK=SK+1
02110   pr #255,using L2120: rtrm$(foot$),"Page "&str$(pt1)
02120 L2120: form skip sk,pos tabnote,c fl,pos 70,c 8,skip 1
02130   if eofcode=1 then goto L2160
02140   pr #255: newpage
02150   gosub L2310
02160 L2160: return 
02170 L2170: gosub L2070: continue 
02180 L2180: if percent=0 then goto L2300
02190   if ul=0 then goto L2280
02200   if ul=1 then goto L2250
02210   underlin$="=============== ======"
02220   pr #255,using L2230: underlin$,underlin$
02230 L2230: form pos 31,c 22,pos 55,c 22,skip redir
02240   goto L2280
02250 L2250: underlin$="_______________ ______"
02260   pr #255,using L2270: underlin$,underlin$
02270 L2270: form skip redir,pos 31,c 22,pos 55,c 22,pos 79,c 22,pos 103,c 22,skip redir
02280 L2280: if redir=0 then pr #255,using L2290: " "
02290 L2290: form skip 1,c 1,skip 0
02300 L2300: return 
02310 L2310: heading=1
02311   pt1+=1
02312   pr #255: "\qc  {\f181 \fs18 \b "&env$('cnam')&"}"
02313   pr #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
02314   if trim$(secondr$)<>"" then pr #255: "\qc  {\f181 \fs24 \b "&trim$(secondr$)&"}"
02315   pr #255: "\qc  {\f181 \fs16 \b For the "&rtrm$(actpd$)&" month period ended "&rtrm$(fnpedat$)&"}"
02316   pr #255: "\ql "
02420   pr #255: 
02430   pr #255: tab(36);"CURRENT YEAR";tab(60);"PRIOR YEAR"
02440   pr #255,using L2450: "     YEAR TO DATE","     YEAR TO DATE"
02450 L2450: form pos 32,c 20,pos 55,c 22,skip redir
02460 L2460: form pos 31,c 22,pos 55,c 22,skip 1
02470   pr #255,using L2460: "_____________________","______________________"
02480   pr #255: 
02490   return 
02500 ! ______________________________________________________________________
02510 L2510: if pas=2 then goto L2590
02520   pas=2
02530   percent=1
02540   percent1=0
02550   percent2=0
02560   percent3=0
02570   percent4=0
02580   goto L2670
02590 L2590: eofcode=1
02600   gosub L2070
02610 L2610: ! 
02620 ! 
02628   fnfscode(actpd)
02629   fnpriorcd(1)
02630   fncloseprn
02640 ! 
02650   goto XIT
02660 ! ______________________________________________________________________
02670 L2670: close #1: ioerr L2680
02680 L2680: close #3: ioerr L2690
02690 L2690: total=0
02700   total2=0
02710   total3=0
02720   total4=0
02730   mat accum=(0)
02740   foot1=0
02750   foot$=" "
02760   notrans=ir=0
02770   goto L430
02780 PAS1: if rnp=0 then goto L2890
02790   mat tp1=(0)
02800   read #4,using L2810,key=r$: k4$,mat tp1 nokey L2880
02810 L2810: form pos 1,g 5,4*pd 7.2
02820   tp1(1)=tp1(1)+tp1
02830   tp1(2)=tp1(2)+tp2
02840   tp1(3)=tp1(3)+tp3
02850   tp1(4)=tp1(4)+tp4
02860   rewrite #4,using L2810: k4$,mat tp1
02870   goto L2890
02880 L2880: write #4,using L2810: r$,tp1,tp2,tp3,tp4
02890 L2890: return 
02900 PAS2: mat tp1=(0)
02910   if rnp=0 then goto L2980
02920   k4$=lpad$(str$(rnp),5)
02930   read #4,using L2810,key=k4$: k4$,mat tp1 nokey L2940
02940 L2940: percent1=tp1(1)
02950   percent2=tp1(2)
02960   percent3=tp1(3)
02970   percent4=tp1(4)
02980 L2980: return 
02990 XIT: fnxit
03000 ! ______________________________________________________________________
03010 ! <updateable region: ertn>
03020 ERTN: fnerror(program$,err,line,act$,"xit")
03030   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
03040   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
03050   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
03060 ERTN_EXEC_ACT: execute act$ : goto ERTN
03070 ! /region
