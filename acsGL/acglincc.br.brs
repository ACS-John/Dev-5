00010 ! Replace S:\acsGL\AcGlIncC
00020 ! -- ! COMPARATIVE INCOME STATEMENT FOR 14 7/8*11 PAPER WITH PERCENTAGES
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnpglen,fnerror,fncno,fncch$,fnpedat$,fnactpd$,fnactpd,fnfscode,fnUseDeptNo,fnpriorcd,fnps,fnprocess,fnglfs,fntos,fnlbl,fntxt,fncmdkey,fnacs
00050   fntop(program$,cap$="Comparative Income Statement")
00060 ! ______________________________________________________________________
00070   on error goto ERTN
00090 ! ______________________________________________________________________
00100   dim fl1$*256,p$(20)*50,cap$*128
00110   dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*22
00120   dim cnam$*40,b$*3,a$(8)*30,oldtrans$*16,g(8),accum(9,4),udf$*256
00130   dim bp(13),by(13),tp1(4)
00140 ! ______________________________________________________________________
00150   fncno(cno,cnam$)
00160   udf$=env$('temp')&'\'
00185   actpd$=fnactpd$ !:
        actpd=fnactpd !:
        fnfscode !:
        fnpriorcd
00190   if fnglfs=5 then goto XIT !:
          ! sets fnps,fnpriorcd,fnfscode (primary/secondary,current year/Prior,period to print)
00195   fscode=fnfscode !:
        priorcd=fnpriorcd
00200 ! ______________________________________________________________________
00210   pr newpage
00220   pors=1
00230   mp1=69
00240   if fnps=2 then mp1=mp1+3
00250   fl1$="Name="&env$('Q')&"\GLmstr\ACGLFNSI.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\FNSIINDX.h"&env$('cno')&",Shr"
00260   if fnps=2 then fl1$="Name="&env$('Q')&"\GLmstr\ACGLFNSJ.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\FNSJINDX.h"&env$('cno')&",Shr"
00270   form c 9,skip 0
00280 L280: form pos mp1,pd 3,pos 81,41*pd 6.2
00290   form c 7,skip 0
00300   nametab=int(44-len(rtrm$(cnam$))/2)
00310   pas=1 : open #4: "Name="&env$('temp')&"\Work."&session$&",KFName="&env$('temp')&"\IDX."&wsid$&",Replace,RecL=33,KPS=1,KLN=5",internal,outin,keyed 
00320   if actpd>0 and actpd<14 then goto L380
00330   pr newpage
00340   pr f "10,2,C 78": "THIS PROGRAM CANNOT PROCESS WITHOUT THE NUMBER OF THE ACCOUNTING PERIOD END"
00350   pr f "12,2,C 70,N": "USE THE SELECT DATE ROUTINE TO ENTER THIS INFORMATION"
00360   input fields "23,2,C 1,E,N": pause$
00370   goto XIT
00380 L380: open #1: fl1$,internal,input,keyed 
00390   if fnprocess=1 or fnUseDeptNo=0 then goto L510
00400   if percent=1 then goto L510
00410   fntos(sn$="Acglincc") !:
        mylen=30: mypos=mylen+3 : right=1
00420   fnlbl(1,1,"Cost Center or Department #:",mylen,right)
00430   fntxt(1,mypos,3,0,right,"30",0,"Enter the cost center or department number if you wish to pr only one department, else leave blank for all.",0 ) !:
        resp$(1)=""
00440   fnlbl(2,1,"(Blank for all Departments)",mylen,right)
00450   fncmdkey("&Next",1,1,0,"Prints the financial statement.")
00460   fncmdkey("&Cancel",5,0,1,"Returns to menu without posting.")
00470   fnacs(sn$,0,mat resp$,ckey)
00480   if ckey=5 then goto XIT
00490   costcntr=val(resp$(1))
00500   cnam$=rtrm$(cnam$)
00510 L510: pf1=len(cnam$)+int((43-len(cnam$))/2)
00520   fnopenprn
00530   redir=0: if file$(255)(1:4)<>"PRN:" then redir=1
00540   report$="STATEMENT OF INCOME AND EXPENSES"
00550   if fnps=2 then goto L580 ! secondary
00560   execute "Index "&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&" "&udf$&"fsindex.H"&env$('cno')&" 69 3 Replace DupKeys -N"
00570   goto L590
00580 L580: execute "Index "&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&" "&udf$&"fsindex.H"&env$('cno')&" 72 3 Replace DupKeys -N"
00590 L590: open #3: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&",KFName="&udf$&"fsindex.h"&env$('cno')&",Shr",internal,input,keyed 
00600 L600: ! 
00610 L610: read #1,using L660: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc,rnp eof L2330
00620   if ltrm$(r$)="" or ltrm$(r$)="0" then goto L600
00630   if costcntr=0 then goto L660
00640   if fc=0 and te$="F" then goto L670 ! 5/08/1989
00650   if costcntr><fc then goto L600
00660 L660: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3,n 5
00670 L670: if te$="S" or te$="F" then goto L690
00680   if heading=0 and te$><"R" then gosub L2180
00690 L690: on pos ("RFHDTS",te$,1) goto L1690,L1730,L700,L760,L1410,L1690 none L600
00700 L700: if percent=0 then goto L740
00710   pr #255,using L720: d$(1:40)
00720 L720: form pos sp,c 40,skip 1
00730   gosub L1880
00740 L740: gosub L1800
00750   goto L600
00760 L760: if notrans=1 then goto L990
00770   if ir>=val(r$) and val(r$)><0 then goto L880
00780 L780: ! read amounts from gl master
00790 L790: read #3,using L280: ir,bb,cb,mat by,mat bp eof L980
00800   if ir=0 then goto L790
00810   if fnfscode=0 or (fnfscode=actpd and fnpriorcd=1) then goto L880
00820   if fnfscode<1 or fnfscode>13 then let fnfscode=1 ! 6/8/88
00830   if fnpriorcd=1 then cb=by(fnfscode) else cb=bp(fnfscode)
00840   if fnpriorcd=2 then goto L870
00850   if fnfscode>1 then bb=by(fnfscode-1) else bb=0
00860   goto L880
00870 L870: if fnfscode>1 then bb=bp(fnfscode-1) else bb=0
00874   if cb=0 then goto L880
00880 L880: if ir=val(r$) then total=total+(cb-bb) else goto L960
00890   total2=total2+cb
00900   if fnfscode>1 then goto L910 else goto L930
00910 L910: total3=total3+(bp(fnfscode)-bp(fnfscode-1))
00920   goto L940
00930 L930: total3=total3+bp(fnfscode)
00940 L940: total4=total4+bp(fnfscode)
00950   goto L780
00960 L960: if ir<val(r$) then goto L780
00970   if ir>val(r$) then goto L990
00980 L980: notrans=1
00990 L990: for j=1 to 9
01000     if ac(j)=9 then goto L1050 ! 10/14/87
01010     accum(j,1)=accum(j,1)+total
01020     accum(j,2)=accum(j,2)+total2
01030     accum(j,3)=accum(j,3)+total3
01040     accum(j,4)=accum(j,4)+total4
01050 L1050: next j
01060   if rs=1 then total=-total else goto L1100
01070   total2=-total2
01080   total3=-total3
01090   total4=-total4
01100 L1100: if ds=1 then dollar$="$" else dollar$=" "
01110   if ds=1 then percent$="%" else percent$=" "
01120   if total2><0 or total4><0 then goto L1150
01130   if total><0 or total3><0 then goto L1150
01140   if ls+ds+ul>0 then goto L1150 else goto L610
01150 L1150: if percent=0 then goto L1320
01160   sp2=31-sp-1
01170   if pas=2 then gosub PAS2
01180   if percent1=0 then pdpct=0 else pdpct=total/percent1*100
01190   if percent2=0 then ytdpct=0 else ytdpct=total2/percent2*100
01200   if percent3=0 then pppd=0 else pppd=total3/percent3*100
01210   if percent4=0 then ppyear=0 else ppyear=total4/percent4*100
01220   if pdpct<-999.99 then pdpct=-999.99
01230   if pdpct>999.99 then pdpct=999.99
01240   if ytdpct<-999.99 then ytdpct=-999.99
01250   if ytdpct>999.99 then ytdpct=999.99
01260   if ppyear<-999.99 then ppyear=-999.99
01270   if ppyear>999.99 then ppyear=999.99
01280   if pppd<-999.99 then pppd=-999.99
01290   if pppd>999.99 then pppd=999.99
01295   if ul=1 then pr #255,using L1311: d$(1:sp2),dollar$,"{\ul ",total,"}",pdpct,percent$,dollar$,"{\ul ",total2,"}",ytdpct,percent$,dollar$,"{\ul ",total3,"}",pppd,percent$,dollar$,"{\ul ",total4,"}",ppyear,percent$ pageoflow L2040 : goto L1320
01300   pr #255,using L1310: d$(1:sp2),dollar$,total,pdpct,percent$,dollar$,total2,ytdpct,percent$,dollar$,total3,pppd,percent$,dollar$,total4,ppyear,percent$ pageoflow L2040
01310 L1310: form pos sp,c sp2,pos 31,c 1,pic(---,---,---.##),pic(----.##),c 2,c 1,pic(---,---,---.##),pic(----.##),c 2,c 1,pic(---,---,---.##),pic(----.##),c 2,c 1,pic(---,---,---.##),pic(----.##),c 1,skip redir
01311 L1311: form pos sp,c sp2,pos 31,c 1,c 5,pic(---,---,---.##),c 1,pic(----.##),c 2,c 1,c 5,pic(---,---,---.##),c 1,pic(----.##),c 2,c 1,c 5,pic(---,---,---.##),c 1,pic(----.##),c 2,c 1,c 5,pic(---,---,---.##),c 1,pic(----.##),c 1,skip redir
01320 L1320: if pas=1 then tp1=total : tp2=total2 : tp3=total3 : tp4=total4 : gosub PAS1
01330   total=0
01340   total2=0
01350   total3=0
01360   total4=0
01370 L1370: gosub L1800
01375   if ul=1 then goto L1390
01380   gosub L2050
01390 L1390: gosub L1880
01400   goto L600
01410 L1410: if ap=0 then ap=1
01420   if rs=1 then accum1=-accum(ap,1) else accum1=accum(ap,1)
01430   if rs=1 then accum2=-accum(ap,2) else accum2=accum(ap,2)
01440   if rs=1 then accum3=-accum(ap,3) else accum3=accum(ap,3)
01450   if rs=1 then accum4=-accum(ap,4) else accum4=accum(ap,4)
01460   if ds=1 then dollar$="$" else dollar$=" "
01470   if ds=1 then percent$="%" else percent$=" "
01480   if pas=2 then gosub PAS2
01490   if percent=0 then goto L1640
01500   if percent1=0 then pdpct=0 else pdpct=accum1/percent1*100
01510   if percent2=0 then ytdpct=0 else ytdpct=accum2/percent2*100
01520   if percent3=0 then pppd=0 else pppd=accum3/percent3*100
01530   if percent4=0 then ppyear=0 else ppyear=accum4/percent4*100
01540   if pdpct<-999.99 then pdpct=-999.99
01550   if pdpct>999.99 then pdpct=999.99
01560   if ytdpct<-999.99 then ytdpct=-999.99
01570   if ytdpct>999.99 then ytdpct=999.99
01580   if pppd<-999.99 then pppd=-999.99
01590   if pppd>999.99 then pppd=999.99
01600   if ppyear<-999.99 then ppyear=-999.99
01610   if ppyear>999.99 then ppyear=999.99
01620   sp2=31-sp-1
01625   if ul=1 then pr #255,using L1311: d$(1:sp2),dollar$,"{\ul ",accum1,"}",pdpct,percent$,dollar$,"{\ul ",accum2,"}",ytdpct,percent$,dollar$,"{\ul ",accum3,"}",pppd,percent$,dollar$,"{\ul ",accum4,"}",ppyear,percent$ pageoflow L2040 : goto L1640
01630   pr #255,using L1310: d$(1:sp2),dollar$,accum1,pdpct,percent$,dollar$,accum2,ytdpct,percent$,dollar$,accum3,pppd,percent$,dollar$,accum4,ppyear,percent$ pageoflow L2040
01640 L1640: if pas=1 then tp1=accum1: tp2=accum2 : tp3=accum3 : tp4=accum4 : gosub PAS1
01650   gosub L1800
01655   if ul=1 then goto L1370
01660   gosub L2050
01670   gosub L1880
01680   goto L600
01690 L1690: if te$="R" then report$=d$
01700   if te$="S" then secondr$=d$
01710   gosub L1880
01720   goto L600
01730 L1730: if foot1=1 then goto L1780
01740   tabnote=sp
01750   foot1=1
01760   foot$=d$
01770   goto L600
01780 L1780: foot$=rtrm$(foot$)&d$
01790   goto L600
01800 L1800: for j=1 to 9
01810     if ac(j)=0 or ac(j)=9 then goto L1860 ! 10/14/87
01820     accum(j,1)=0
01830     accum(j,2)=0
01840     accum(j,3)=0
01850     accum(j,4)=0
01860 L1860: next j
01870   return 
01880 L1880: if percent=0 then goto L2030
01890   if ls=0 then goto L2030
01900   if ls=99 then goto L1940
01910   pr #255,using L1920: " "
01920 L1920: form pos 1,c 1,skip ls
01930   goto L2030
01940 L1940: fnpglen(pglen)
01950 ! If PGLEN<>42 Then pGLEN=58
01960   sk=pglen-krec(255): fl=len(rtrm$(foot$))
01970 ! If PGLEN=42 Then sK=SK+1
01980   pr #255,using L1990: rtrm$(foot$),"Page "&str$(pt1)
01990 L1990: form skip sk,pos tabnote,c fl,pos 120,c 8,skip 1
02000   if eofcode=1 then goto L2030
02010   pr #255: newpage
02020   gosub L2180
02030 L2030: return 
02040 L2040: gosub L1940: continue 
02050 L2050: if percent=0 then goto L2170
02060   if ul=0 then goto L2150
02070   if ul=1 then goto L2120
02080   underlin$="============== ======"
02090   pr #255,using L2100: underlin$,underlin$,underlin$,underlin$
02100 L2100: form pos 32,c 22,pos 56,c 22,pos 80,c 22,pos 104,c 22,skip redir
02110   goto L2150
02120 L2120: underlin$="______________ _______"
02130   pr #255,using L2140: underlin$,underlin$,underlin$,underlin$
02140 L2140: form skip redir,pos 31,c 22,pos 55,c 22,pos 79,c 22,pos 103,c 22,skip redir
02150 L2150: if redir=0 then pr #255,using L2160: " "
02160 L2160: form skip 1,c 1,skip 0
02170 L2170: return 
02180 L2180: heading=1
02190   pt1+=1
02200   pr #255: "\qc  {\f181 \fs24 \b "&env$('cnam')&"}"
02210   pr #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
02220   if trim$(secondr$)<>"" then pr #255: "\qc  {\f181 \fs18 \b "&trim$(secondr$)&"}"
02230   pr #255: "\qc  {\f181 \fs16 \b For the "&rtrm$(actpd$)&" month period ended "&rtrm$(fnpedat$)&"}"
02240   pr #255: "\ql "
02250   pr #255: 
02260   pr #255: tab(48);"CURRENT YEAR";tab(97);"PRIOR YEAR"
02270   pr #255,using L2280: fncch$,"     YEAR TO DATE",fncch$,"     YEAR TO DATE"
02280 L2280: form pos 32,c 20,pos 55,c 22,pos 79,c 22,pos 103,c 22,skip redir
02290 L2290: form pos 31,c 22,pos 55,c 22,pos 79,c 22,pos 103,c 22,skip 1
02300   pr #255,using L2290: "______________________","______________________","_____________________","______________________"
02310   pr #255: 
02320   return 
02330 L2330: if pas=2 then goto L2410
02340   pas=2
02350   percent=1
02360   percent1=0
02370   percent2=0
02380   percent3=0
02390   percent4=0
02400   goto L2490
02410 L2410: eofcode=1
02420   gosub L1940
02430 ! 
02440 ! 
02450   fncloseprn
02460   fnfscode(actpd)
02465   fnpriorcd(1)
02470   goto XIT
02480 ! ______________________________________________________________________
02490 L2490: close #1: ioerr L2500
02500 L2500: close #3: ioerr L2510
02510 L2510: total=0
02520   total2=0
02530   total3=0
02540   total4=0
02550   mat accum=(0)
02560   foot1=0
02570   foot$=" "
02580   notrans=ir=0
02590   goto L380
02600 PAS1: if rnp=0 then goto L2710
02610   mat tp1=(0)
02620   read #4,using L2630,key=r$: k4$,mat tp1 nokey L2700
02630 L2630: form pos 1,g 5,4*pd 7.2
02640   tp1(1)=tp1(1)+tp1
02650   tp1(2)=tp1(2)+tp2
02660   tp1(3)=tp1(3)+tp3
02670   tp1(4)=tp1(4)+tp4
02680   rewrite #4,using L2630: k4$,mat tp1
02690   goto L2710
02700 L2700: write #4,using L2630: r$,tp1,tp2,tp3,tp4
02710 L2710: return 
02720 PAS2: mat tp1=(0)
02730   if rnp=0 then goto L2800
02740   k4$=lpad$(str$(rnp),5)
02750   read #4,using L2630,key=k4$: k4$,mat tp1 nokey L2760
02760 L2760: percent1=tp1(1)
02770   percent2=tp1(2)
02780   percent3=tp1(3)
02790   percent4=tp1(4)
02800 L2800: return 
02810 XIT: fnxit
02820 ! ______________________________________________________________________
02830 ! <updateable region: ertn>
02840 ERTN: fnerror(program$,err,line,act$,"xit")
02850   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
02860   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
02870   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
02880 ERTN_EXEC_ACT: execute act$ : goto ERTN
02890 ! /region
