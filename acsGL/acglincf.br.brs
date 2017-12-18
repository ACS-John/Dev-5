00010 ! Replace S:\acsGL\AcGlIncF
00020 ! -- INCOME STATEMENT COMPARING UP TO 10 FUNDS  - USES 2ND I/C DESIGN
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnpglen,fnerror,fnprocess,fncno,fnchain,fnUseDeptNo,fnpedat$,fnps,fnpriorcd,fnfscode,fnactpd$,fncch$,fnglfs,fnactpd$,fnactpd,fntos,fnlbl,fntxt,fncmdkey,fnacs,fnopt
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim fl1$*256,actpd$*6,cogl$(3)*12,pedat$*20,cch$*20
00080   dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*14
00090   dim cnam$*40,b$*3,a$(8)*30,oldtrans$*16,g(8),accum(10,9,2)
00100   dim by(13),bp(13),resp$(30)*50
00110   dim sendto$*80,bigul$*140,heading$*140,cap$*128,udf$*256
00120   dim fundnum(10),funddesc$(10)*20,io1$(20),dolcol$*140,accumcol$*140
00130   dim choices$(2)*21,io5$(2)
00140 ! ______________________________________________________________________
00150   fntop(program$,cap$="Income Statemet - Fund Comparison")
00160   on fkey 5 goto L2170
00170   fncno(cno,cnam$)
00180   udf$=env$('temp')&'\'
00190   if fnglfs=5 then goto XIT !:
          ! sets fnps,fnpriorcd,fnfscode (primary/secondary,current year/Prior,period to print)
00200   actpd$=fnactpd$
00210   pedat$=fnpedat$
00220   actpd$=fnactpd$
00230   actpd=fnactpd
00240   fscode=fnfscode
00250   priorcd=fnpriorcd
00260 ! ______________________________________________________________________
00270   monthly=1 ! use 2 for ytd figures (set default for automatic processing to monthly information
00280   gosub L2510
00290   pors=1
00300   if fnps=2 then mp1=72 else mp1=69
00310   fl1$="Name="&env$('Q')&"\GLmstr\ACGLFNSI.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\FNSIINDX.h"&env$('cno')&",Shr"
00320   if fnps=2 then fl1$="Name="&env$('Q')&"\GLmstr\ACGLFNSJ.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\FNSJINDX.h"&env$('cno')&",Shr"
00330   form c 9,skip 0
00340 L340: form pos 1,n 3,n 6,n 3,pos mp1,pd 3,pos mp2,pd 3,pos 81,41*pd 6.2
00350   form c 7,skip 0
00360   nametab=int(44-len(rtrm$(cnam$))/2)
00370   open #1: fl1$,internal,input,keyed 
00380   if fnprocess=1 or fnUseDeptNo=0 then goto L480
00390   fntos(sn$="ACglincf") !:
        mylen=30: mypos=mylen+3 : right=1
00400   fnlbl(1,1,"Cost Center or Department #:",mylen,right)
00410   fntxt(1,mypos,3,0,right,"30",0,"Enter the cost center or department number if you wish to pr only one department, else leave blank for all.",0 ) !:
        resp$(1)=""
00420   fnlbl(2,1,"(Blank for all Departments)",mylen,right)
00430   fncmdkey("&Next",1,1,0,"Prints the financial statement.")
00440   fncmdkey("&Cancel",5,0,1,"Returns to menu without posting.")
00450   fnacs(sn$,0,mat resp$,ckey)
00460   if ckey=5 then goto XIT
00470   costcntr=val(resp$(1))
00480 L480: report$="STATEMENT OF INCOME AND EXPENSES - FUND COMPARISON"
00490   fnopenprn
00500   redir=0: if file$(255)(1:4)<>"PRN:" then redir=1
00510   if fnps=2 then goto L540 ! secondary
00520   execute "Index "&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&" "&udf$&"fsindex.H"&env$('cno')&" 69 3 Replace DupKeys -N"
00530   goto L550
00540 L540: execute "Index "&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&" "&udf$&"fsindex.H"&env$('cno')&" 72 3 Replace DupKeys -N"
00550 L550: open #3: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&",KFName="&udf$&"fsindex.h"&env$('cno')&",Shr",internal,input,keyed 
00560 L560: read #1,using L610: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof L2170
00570   if ltrm$(r$)="" or ltrm$(r$)="0" then goto L560
00580   if costcntr=0 then goto L610
00590   if fc=0 and te$="F" then goto L620
00600   if costcntr><fc then goto L560
00610 L610: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
00620 L620: if te$="S" or te$="F" then goto L640
00630   if heading=0 and te$><"R" then gosub L1970
00640 L640: on pos ("RFHDTS",te$,1) goto L1430,L1470,L650,L700,L1280,L1430 none L560
00650 L650: pr #255,using L660: d$(1:40)
00660 L660: form pos sp,c 40,skip 1
00670   gosub L1640
00680   gosub L1540
00690   goto L560
00700 L700: if notrans=1 then goto L950
00710   if ir=val(r$) and val(r$)><0 then goto L830
00720   if ir>val(r$) then goto L830
00730 L730: ! read amounts from gl master file
00740 L740: read #3,using L340: dno,ano,sno,ir,pcr,bb,cb,mat by,mat bp eof L940
00750   if ir=0 then goto L740
00760   if fscode=0 then goto L830
00770   if fscode<1 or fscode>13 then fscode=1
00780   if priorcd=1 then cb=by(fscode) else cb=bp(fscode)
00790   if priorcd=2 then goto L820
00800   if fscode>1 then bb=by(fscode-1) else bb=0
00810   goto L830
00820 L820: if fscode>1 then bb=bp(fscode-1) else bb=0
00830 L830: if ir=val(r$) then goto L840 else goto L880
00840 L840: for x=1 to 10
00850     if dno=fundnum(x) then fund=x ! put in certain column
00860     if fundnum(x)>0 then totcol=x ! total columns needed
00870   next x
00880 L880: if ir=val(r$) then total(fund)=total(fund)+(cb-bb) else goto L920
00890   total2(fund)=total2(fund)+cb
00900   k$=cnvrt$("N 5",pcr)
00910   goto L730
00920 L920: if ir<val(r$) then goto L730
00930   if ir>val(r$) then goto L950
00940 L940: notrans=1
00950 L950: for j=1 to 9
00960     if ac(j)=9 then goto L1010 ! 10/14/87
00970     for j2=1 to 10
00980       accum(j2,j,1)=accum(j2,j,1)+total(j2)
00990       accum(j2,j,2)=accum(j2,j,2)+total2(j2)
01000     next j2
01010 L1010: next j
01020   if rs=1 then goto L1030 else goto L1070
01030 L1030: for j=1 to 10
01040     total(j)=-total(j)
01050     total2(j)=-total2(j)
01060   next j
01070 L1070: if ds=1 then dollar$="$" else dollar$=" "
01080   if sum(total)><0 or sum(total2)><0 then goto L1100
01090   if ls+ul+ds+ic>0 then goto L1100 else goto L560
01100 L1100: sp2=49-sp-1
01110 ! pr #255,USING 1070: D$(1:SP2),DOLLAR$,TOTAL(FUND),DOLLAR$,TOTAL2(FUND) PAGEOFLOW 1560
01120   dolcol$=""
01130   if monthly=2 then mat total=total2 ! substitute ytd figures if ytd stmt
01140   for j=1 to totcol
01150     dolcol$=dolcol$&" "&dollar$&cnvrt$("PIC(-,---,---.##)",total(j))
01160   next j
01170   pr #255,using L1180: d$(1:sp2),rtrm$(dolcol$) pageoflow L1790
01180 L1180: form pos sp,c sp2,pos 49,c big,skip redir
01190 ! IF PC0=1 THEN GOSUB BLDPCT2
01200 ! IF PC3>0 OR PC4>0 THEN pr #255,USING 1100: PC3,PC4
01210   form pos 63,n 4,pos 82,n 4,skip redir
01220   mat total=(0)
01230   mat total2=(0)
01240   gosub L1540
01250   gosub L1810
01260   gosub L1640
01270   goto L560
01280 L1280: accumcol$=""
01290   for j=1 to totcol
01300     if ap=0 then ap=1
01310     if rs=1 then accum1=-accum(j,ap,1) else accum1=accum(j,ap,1)
01320     if rs=1 then accum2=-accum(j,ap,2) else accum2=accum(j,ap,2)
01330     if ds=1 then dollar$="$" else dollar$=" "
01340     if monthly=2 then accum1=accum2
01350     accumcol$=accumcol$&" "&dollar$&cnvrt$("pic(-,---,---.##)",accum1)
01360   next j
01370   sp2=49-sp-1
01380   pr #255,using L1180: d$(1:sp2),rtrm$(accumcol$) pageoflow L1790
01390   gosub L1540
01400   gosub L1810
01410   gosub L1640
01420   goto L560
01430 L1430: if te$="R" then report$=d$
01440   if te$="S" then secondr$=d$
01450   gosub L1640
01460   goto L560
01470 L1470: if foot1=1 then goto L1520
01480   tabnote=sp
01490   foot1=1
01500   foot$=d$
01510   goto L560
01520 L1520: foot$=rtrm$(foot$)&d$
01530   goto L560
01540 L1540: for j=1 to 9
01550     if ac(j)=0 or ac(j)=9 then goto L1620 ! 10/14/87
01560     for j2=1 to 10
01570 ! aCCUM(FUND,J,1)=0
01580       accum(j2,j,1)=0
01590 ! aCCUM(FUND,J,2)=0
01600       accum(j2,j,2)=0
01610     next j2
01620 L1620: next j
01630   return 
01640 L1640: if ls=0 then goto L1780
01650   if ls=99 then goto L1690
01660   pr #255,using L1670: " "
01670 L1670: form pos 1,c 1,skip ls
01680   goto L1780
01690 L1690: fnpglen(pglen)
01700 ! If PGLEN<>42 Then pGLEN=58
01710   sk=pglen-krec(255): fl=len(rtrm$(foot$))
01720 ! If PGLEN=42 Then sK=SK+1
01730   pr #255,using L1740: rtrm$(foot$)
01740 L1740: form skip sk,pos tabnote,c fl,skip 1
01750   if eofcode=1 then goto L1780
01760   pr #255: newpage
01770   gosub L1970
01780 L1780: return 
01790 L1790: gosub L1690
01800   continue 
01810 L1810: if ul=0 then goto L1940
01820   if ul=1 then goto L1850
01830   underlin$="  ============"
01840   goto L1860
01850 L1850: underlin$="  ____________"
01860 L1860: bigul$=""
01870   for j=1 to totcol
01880     bigul$=bigul$&underlin$
01890   next j
01900   if ul=1 then pr #255,using L1920: bigul$
01910   if ul=2 then pr #255,using L1930: bigul$
01920 L1920: form skip redir,pos 49,c big,skip redir
01930 L1930: form skip 1,pos 49,c big,skip redir
01940 L1940: if redir=0 then pr #255,using L1950: " "
01950 L1950: form skip 1,c 1,skip redir
01960   return 
01970 L1970: heading=1
01980   if pt1=0 then pt1=1 else pt1=pt1+1
01990   pr #255,using L2000: cnam$,"PAGE "&str$(pt1)
02000 L2000: form skip 2,pos 15,cc 60,pos 73,c 7
02010   p1=44-len(rtrm$(report$))/2
02020   pr #255,using L2030: rtrm$(report$)
02030 L2030: form pos 15,cc 60
02040   if rtrm$(secondr$)="" then goto L2070
02050   p1=44-len(rtrm$(secondr$))/2
02060   pr #255,using L2030: rtrm$(secondr$)
02070 L2070: p1=30-len(rtrm$(actpd$))/2-len(rtrm$(pedat$))/2
02080   if monthly=1 then pr #255,using L2100: "For the one month period ended "&rtrm$(pedat$)
02090   if monthly=2 then pr #255,using L2100: "For the "&rtrm$(actpd$)&" month period ended "&rtrm$(pedat$)
02100 L2100: form pos 15,cc 60
02110   pr #255: 
02120   pr #255: 
02130   pr #255,using L2140: heading$
02140 L2140: form pos 49,c big,skip 2
02150   return 
02160 ! ______________________________________________________________________
02170 L2170: eofcode=1
02180   gosub L1690
02190 ! 
02200 ! 
02210   fncloseprn
02220   goto XIT
02230 ! ______________________________________________________________________
02240 XIT: fnxit
02250 ! ______________________________________________________________________
02260 BLDPCT1: open #10: "Name="&env$('temp')&"\Work."&session$&",KFName="&env$('Temp')&"\Addr."&session$&",Replace,RecL=17,KPS=1,KLN=5",internal,outin,keyed 
02270   for j=1 to lrec(3)
02280     read #3,using L2290,rec=j: pc1,bb,cb norec L2380
02290 L2290: form pos mp1,pd 3,pos 81,2*pd 6.2
02300     k$=cnvrt$("N 5",pc1)
02310     read #10,using L2320,key=k$: pc1,pc2,yt2 nokey L2370
02320 L2320: form pos 1,g 5,2*pd 6.2
02330     pc2=pc2+cb-bb
02340     yt2=yt2+cb
02350     rewrite #10,using L2320: pc1,pc2,yt2
02360     goto L2380
02370 L2370: write #10,using L2320: pc1,cb-bb,cb
02380 L2380: next j
02390   pc0=1
02400   return 
02410 BLDPCT2: ! 
02420   pc3=pc4=0
02430   if val(k$)=0 then goto L2510
02440   read #10,using L2320,key=k$: pc1,pc2,yt2 nokey L2510
02450   if total(fund)=0 then goto L2480
02460   pc3=round(((total(fund)-pc2)/total(fund))*100,0)
02470   if pc3<-999 or pc3>9999 then pc3=0
02480 L2480: if total2(fund)=0 then goto L2510
02490   pc4=round(((total2(fund)-yt2)/total(fund))*100,0)
02500   if pc4<-999 or pc4>9999 then pc4=0
02510 L2510: open #5: "Name="&env$('Q')&"\GLmstr\GLfund.h"&env$('cno')&",RecL=230,use",internal,outin,relative 
02520   read #5,using L2530: mat fundnum,mat funddesc$ ioerr L2530
02530 L2530: form pos 1,10*n 3,10*c 20
02540   fntos(sn$="ACglcasf3") !:
        mylen=1: mypos=mylen+3
02550   fnlbl(1,4,"Fund                 Description ")
02560   for j=1 to 10
02570     fntxt(j+1,mypos,3,0,right,"30",0,"Enter the fund number.") !:
          resp$(j*2-1)=str$(fundnum(j))
02580     fntxt(j+1,mypos+10,20,0,0,"",0,"Enter the fund description.") !:
          resp$(j*2)=funddesc$(j)
02590   next j
02600   fncmdkey("&Next",1,1,0,"Continues with financial statement.")
02610   fncmdkey("&Cancel",5,0,1,"Returns to menu without posting.")
02620   fnacs(sn$,0,mat resp$,ckey)
02630   if ckey=5 then goto XIT
02640   for j=1 to 10
02650     fundnum(j)=val(resp$(j*2-1))
02660     funddesc$(j)=resp$(j*2)
02670   next j
02680   rewrite #5,using L2530: mat fundnum,mat funddesc$ ioerr L2700
02690   goto L2710
02700 L2700: write #5,using L2530: mat fundnum,mat funddesc$
02710 L2710: close #5: 
02720   for j=1 to 10
02730     if fundnum(j)>0 then heading$=heading$&" "&lpad$(rtrm$(funddesc$(j)(1:13)),13): totcol=totcol+1
02740   next j
02750   big=totcol*14
02760   return 
02770 ASK_MONTHLY: ! ask monthly info or ytd info
02780   fntos(sn$="ACglcasf2") !:
        mylen=30: mypos=mylen+3 : right=1
02790   fnopt(1,2,"Print Monthly Figures" ,0,0) !:
        resp$(2)="False"
02800   fnopt(2,2,"Print Year to Date Figures" ,0,0) !:
        resp$(2)="True"
02810   fncmdkey("&Next",1,1,0,"Prints the financial statement.")
02820   fncmdkey("&Cancel",5,0,1,"Returns to menu without posting.")
02830   fnacs(sn$,0,mat resp$,ckey)
02840   if ckey=5 then goto XIT
02850   if resp$(1)="True" then monthly=1
02860   if resp$(2)="True" then monthly=2
02870   return 
02880 ! ______________________________________________________________________
02890 ! <updateable region: ertn>
02900 ERTN: fnerror(program$,err,line,act$,"xit")
02910   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
02920   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
02930   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
02940 ERTN_EXEC_ACT: execute act$ : goto ERTN
02950 ! /region
