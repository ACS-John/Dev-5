00010 ! Replace S:\acsGL\acglCasF
00020 ! Cash Flow Statement with Fund Comparisons
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnxit,fntop, fnopenprn,fncloseprn,fnpglen,fncno,fnerror,fnprocess,fnactpd$,fnpedat$,fnactpd,fnfscode,fnUseDeptNo,fnpriorcd, fnps,fnglfs,fnTos,fnLbl,fnTxt,fnCmdKey,fnAcs,fnOpt
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim choices$(2)*21,io5$(2),bigul$*140,heading$*140
00080   dim fundnum(10),funddesc$(10)*20,io1$(20),dolcol$*140,accumcol$*140
00090   dim bm(13),bp(13),by(13),cap$*128,fl1$*256,in3$(4),sc1$(2)*20,udf$*256
00100   dim cnam$*40,b$*3,a$(8)*30,oldtrans$*16,g(8),accum(10,9,7),resp$(30)*50
00110   dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*14
00120 ! ______________________________________________________________________
00130   fntop(program$,cap$="Cash Flow with Fund Comparison")
00131   report$=cap$
00132   actpd$=fnactpd$ !:
        actpd=fnactpd !:
        fnfscode !:
        fnpriorcd
00140   if fnglfs=5 then goto XIT
00150   fncno(cno,cnam$)
00160   fscode=fnfscode !:
        priorcd=fnpriorcd
00170   udf$=env$('temp')&'\'
00180   monthly=1 ! default to monthly information
00190   open #20: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,input,relative: read #20,using 'Form Pos 384,n 2',rec=1: nap : close #20: 
00200   fscode=fnfscode
00210   pors=1
00220   gosub L2370
00230   form pos 1,n 2,c 40,pos 89,2*n 1,pos 141,6*n 1,3*n 2,c 6,3*c 12,2*c 20,pos 237,n 2
00240   in3$(1)="8,25,N 12.2,UT,N" : in3$(2)="8,45,N 12.2,UT,N"
00250   mp1=75
00260   if fnps=2 then mp1=mp1+3
00270   fl1$="Name=[Q]\GLmstr\ACGLFNSF.h[cno],KFName=[Q]\GLmstr\FNSFIndx.h[cno],Shr"
00280   if fnps=2 then fl1$="Name=[Q]\GLmstr\ACGLFNSG.h[cno],KFName=[Q]\GLmstr\FNSGIndx.h[cno],Shr"
00290   open #1: fl1$,internal,input,keyed 
00300   if fnprocess=1 or fnUseDeptNo=0 then goto L410
00310   fnTos(sn$="ACglcasf") !:
        mylen=30: mypos=mylen+3 : right=1
00320   fnLbl(1,1,"Cost Center or Department #:",mylen,right)
00330   fnTxt(1,mypos,3,0,right,"30",0,"Enter the cost center or department number if you wish to pr only one department, else leave blank for all.",0 ) !:
        resp$(1)=""
00340   fnLbl(2,1,"(Blank for all Departments)",mylen,right)
00350   fnCmdKey("&Next",1,1,0,"Prints the financial statement.")
00360   fnCmdKey("&Cancel",5,0,1,"Returns to menu without posting.")
00370   fnAcs(sn$,0,mat resp$,ckey)
00380   if ckey=5 then goto XIT
00390   costcntr=val(resp$(1))
00400   gosub ASK_MONTHLY
00410 L410: on fkey 5 goto L2250
00420   if fnps=2 then goto L450 ! secondary
00425   close #3: ioerr L430
00430 L430: execute "Index [Q]\GLmstr\GLmstr.h[cno] "&udf$&"fsindex.H[cno] 75 3 Replace DupKeys -N"
00440   goto L460
00450 L450: execute "Index [Q]\GLmstr\GLmstr.h[cno] "&udf$&"fsindex.H[cno] 78 3 Replace DupKeys -N"
00460 L460: open #3: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName="&udf$&"fsindex.h[cno],Shr",internal,input,keyed 
00470   fnopenprn !:
        if file$(255)(1:4)<>"PRN:" then redir=1 else redir=0
00480 L480: read #1,using L520: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof L2250
00490   if ltrm$(r$)="" or ltrm$(r$)="0" then goto L480
00500   if costcntr=0 then goto L520
00510   if costcntr><fc then goto L480
00520 L520: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
00530   if te$="S" or te$="F" then goto L550
00540   if heading=0 and te$><"R" then gosub L2130
00550 L550: on pos ("RFHDTSBC",te$,1) goto L1550,L1600,L560,L620,L1390,L1550,L620,L2300 none L480
00560 L560: pr #255,using L570: d$(1:40)
00570 L570: form pos sp,c 40,skip 1
00580   gosub L1780
00590   gosub L1690
00600   goto L480
00610 ! ______________________________________________________________________
00620 L620: if te$="B" and ap>0 then goto L1390 ! ENDING BANK BALANCE
00630   if notrans=1 then goto L1040
00640   if ir>=val(r$) and val(r$)><0 then goto L770
00650 L650: ! read amounts from gl master file
00660 L660: read #3,using L760: dno,ano,sno,ir,bb,cb,mat by,mat bp,mat bm eof L1030
00670   if ir=0 then goto L660
00680 !  cB=5: bB=1: bP(12)=100.00
00690   if fscode=0 or (fscode=actpd and priorcd=1) then goto L760
00700   if fscode<1 or fscode>13 then fscode=1
00710   if fnpriorcd=1 then cb=by(fscode) else cb=bp(fscode)
00720   if fnpriorcd=2 then goto L750
00730   if fscode>1 then bb=by(fscode-1) else bb=0
00740   goto L760
00750 L750: if fscode>1 then bb=bp(fscode-1) else bb=0
00760 L760: form pos 1,n 3,n 6,n 3,pos mp1,pd 3,pos 81,41*pd 6.2
00770 L770: if ir=val(r$) then goto L780 else goto L840
00780 L780: fund=0
00790   for x=1 to 10
00800     if dno=fundnum(x) then fund=x ! put in certain column
00810     if fundnum(x)>0 then totcol=x ! total columns needed
00820   next x
00830   if fund=0 then goto L650 ! no matching fund # - skip
00840 L840: if ir=val(r$) then total(fund)=total(fund)+(cb-bb) else goto L1010
00850   if te$="B" then total(fund)=total(fund)-(cb-bb) : total(fund)=total(fund) - bb: total2(fund)=total2(fund)-bp(nap) : goto L870
00860   total2(fund)=total2(fund)+cb
00870 L870: for z=1 to 13
00880     annualb=annualb+bm(z)
00890   next z
00900   if fscode=0 then monthb=monthb+bm(fnactpd) else !:
          monthb=monthb+bm(fscode)
00910   if fscode=0 then goto L920 else goto L960
00920 L920: for j=1 to fnactpd
00930     ytdb=ytdb+bm(j)
00940   next j
00950   goto L650
00960 L960: for j=1 to fscode
00970     ytdb=ytdb+bm(j)
00980   next j
00990   goto L650
01000 ! ______________________________________________________________________
01010 L1010: if ir<val(r$) then goto L650
01020   if ir>val(r$) then goto L1040
01030 L1030: notrans=1
01040 L1040: for j=1 to 9
01050     if ac(j)=9 then goto L1100
01060     for j2= 1 to 10
01070       accum(j2,j,1)=accum(j2,j,1)+total(j2)
01080       accum(j2,j,2)=accum(j2,j,2)+total2(j2)
01090     next j2
01100 L1100: next j
01110   if rs=1 then goto L1120 else goto L1160
01120 L1120: for j=1 to 10
01130     total(j)=-total(j)
01140     total2(j)=-total2(j)
01150   next j
01160 L1160: ! 
01170   if ds=1 then dollar$="$" else dollar$=" "
01180   if sum(total)<>0 or sum(total2)<>0 then goto L1200
01190   if ls+ds+ul+ic>0 then goto L1200 else goto L480
01200 L1200: sp2=30-sp-1
01210   for j=1 to 10
01220     if te$="B" then total(j)=-total(j): total2(j)=-total2(j) ! REVERSE SIGN ON BEGINNING BANK BALANCE
01230   next j
01240   dolcol$=""
01250   if monthly=2 then mat total=total2 ! substitute ytd figures if ytd stmt
01260   for j=1 to totcol
01270     dolcol$=dolcol$&" "&dollar$&cnvrt$("PIC(-,---,---.##)",total(j))
01280   next j
01290   pr #255,using L1300: d$(1:sp2),rtrm$(dolcol$) pageoflow L1940
01300 L1300: form pos sp,c sp2,pos 49,c big,skip redir
01310   form pos sp,c sp2,pos 52,c 1,pic(--,---,---.##),x 1,c 1,pic(--,---,---.##),skip redir
01320   mat total=(0)
01330   mat total2=(0)
01340   gosub L1690
01350   gosub L1960
01360   gosub L1780
01370   goto L480
01380 ! ______________________________________________________________________
01390 L1390: accumcol$=""
01400   for j=1 to totcol
01410     if ap=0 then ap=1
01420     if rs=1 then accum1=-accum(j,ap,1) else accum1=accum(j,ap,1)
01430     if rs=1 then accum2=-accum(j,ap,2) else accum2=accum(j,ap,2)
01440     if ds=1 then dollar$="$" else dollar$=" "
01450     if monthly=2 then accum1=accum2
01460     accumcol$=accumcol$&" "&dollar$&cnvrt$("pic(-,---,---.##)",accum1)
01470   next j
01480   sp2=30-sp-1
01490   pr #255,using L1300: d$(1:sp2),rtrm$(accumcol$) pageoflow L1940
01500   gosub L1690
01510   gosub L1960
01520   gosub L1780
01530   goto L480
01540 ! ______________________________________________________________________
01550 L1550: if te$="R" then report$=d$
01560   if te$="S" then secondr$=d$
01570   gosub L1780
01580   goto L480
01590 ! ______________________________________________________________________
01600 L1600: if foot1=1 then goto L1660
01610   tabnote=sp
01620   foot1=1
01630   foot$=d$
01640   goto L480
01650 ! ______________________________________________________________________
01660 L1660: foot$=rtrm$(foot$)&d$
01670   goto L480
01680 ! ______________________________________________________________________
01690 L1690: for j=1 to 9
01700     if ac(j)=0 or ac(j)=9 then goto L1750
01710     for j2=1 to 10
01720       accum(j2,j,1)=0
01730       accum(j2,j,2)=0
01740     next j2
01750 L1750: next j
01760   return 
01770 ! ______________________________________________________________________
01780 L1780: if ls=0 then goto L1920
01790   if ls=99 then goto L1830
01800   pr #255,using L1810: " "
01810 L1810: form pos 1,c 1,skip ls
01820   goto L1920
01830 L1830: fnpglen(pglen)
01840 ! If PGLEN<>42 Then pGLEN=58
01850   sk=pglen-krec(255): fl=len(rtrm$(foot$))
01860 ! If PGLEN=42 Then sK=SK+1
01870   pr #255,using L1880: rtrm$(foot$),"Page "&str$(pt1)
01880 L1880: form skip sk,pos tabnote,c fl,pos 75,c 8,skip 1
01890   if eofcode=1 then goto L1920
01900   pr #255: newpage
01910   gosub L2130
01920 L1920: return 
01930 ! ______________________________________________________________________
01940 L1940: gosub L1830: continue 
01950 ! ______________________________________________________________________
01960 L1960: if ul=0 then goto L2090
01970   if ul=1 then goto L2000
01980   underlin$="  ============"
01990   goto L2010
02000 L2000: underlin$="  ____________"
02010 L2010: bigul$=""
02020   for j=1 to totcol
02030     bigul$=bigul$&underlin$
02040   next j
02050   if ul=1 then pr #255,using L2070: bigul$
02060   if ul=2 then pr #255,using L2080: bigul$
02070 L2070: form skip redir,pos 49,c big,skip redir
02080 L2080: form skip 1,pos 49,c big,skip redir
02090 L2090: if redir=0 then pr #255,using L2100: " "
02100 L2100: form skip 1,c 1,skip 0
02110   return 
02120 ! ______________________________________________________________________
02130 L2130: heading=1
02140   pt1+=1
02150   pr #255: "\qc  {\f181 \fs24 \b "&env$('cnam')&"}"
02160   pr #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
02170   if trim$(secondr$)<>"" then pr #255: "\qc  {\f181 \fs18 \b "&trim$(secondr$)&"}"
02180   if monthly=2 then pr #255: "\qc  {\f181 \fs16 \b For the "&rtrm$(actpd$)&" month period ended "&rtrm$(fnpedat$)&"}"
02190   if monthly=1 then pr #255: "\qc  {\f181 \fs16 \b For the one month period ended "&rtrm$(fnpedat$)&"}"
02200   pr #255: 
02210   pr #255,using L2220: heading$
02220 L2220: form pos 49,c big,skip 2
02230   return 
02240 ! ______________________________________________________________________
02250 L2250: eofcode=1
02260   gosub L1830
02270   fnfscode(actpd)
02280   fncloseprn
02290   goto XIT
02300 L2300: ! 
02310 ! 
02320 ! 
02330 ! need total,total2  current month, year to date
02340 ! ______________________________________________________________________
02350 XIT: fnxit
02360 ! ______________________________________________________________________
02370 L2370: open #5: "Name=[Q]\GLmstr\GLfund.h[cno],RecL=230,use",internal,outIn,relative 
02380   read #5,using L2390: mat fundnum,mat funddesc$ ioerr L2400
02390 L2390: form pos 1,10*n 3,10*c 20
02400 L2400: fnTos(sn$="ACglcasf3") !:
        mylen=1: mypos=mylen+3
02410   fnTos(sn$="ACglcasf3") !:
        mylen=1: mypos=mylen+3
02420   fnLbl(1,4,"Fund                 Description ")
02430   for j=1 to 10
02440     fnTxt(j+1,mypos,3,0,right,"30",0,"Enter the fund number.") !:
          resp$(j*2-1)=str$(fundnum(j))
02450     fnTxt(j+1,mypos+10,40,0,0,"",0,"Enter the fund description.") !:
          resp$(j*2)=funddesc$(j)
02460   next j
02470   fnCmdKey("&Next",1,1,0,"Continues with financial statement.")
02480   fnCmdKey("&Cancel",5,0,1,"Returns to menu without posting.")
02490   fnAcs(sn$,0,mat resp$,ckey)
02500   if ckey=5 then goto XIT
02510   for j=1 to 10
02520     fundnum(j)=val(resp$(j*2-1))
02530     funddesc$(j)=resp$(j*2)
02540   next j
02550   rewrite #5,using L2390: mat fundnum,mat funddesc$ ioerr L2570
02560   goto L2580
02570 L2570: write #5,using L2390: mat fundnum,mat funddesc$
02580 L2580: close #5: 
02590   for j=1 to 10
02600     if fundnum(j)>0 then !:
            heading$=heading$&" "&lpad$(rtrm$(funddesc$(j)(1:13)),13) !:
            totcol+=1
02610   next j
02620   big=totcol*14
02630   return 
02640 ! ______________________________________________________________________
02650 ASK_MONTHLY: ! ask monthly info or ytd info
02660   fnTos(sn$="ACglcasf2") !:
        mylen=30: mypos=mylen+3 : right=1
02670   fnOpt(1,2,"Print Monthly Figures" ,0,0) !:
        resp$(2)="False"
02680   fnOpt(2,2,"Print Year to Date Figures" ,0,0) !:
        resp$(2)="True"
02690   fnCmdKey("&Next",1,1,0,"Prints the financial statement.")
02700   fnCmdKey("&Cancel",5,0,1,"Returns to menu without posting.")
02710   fnAcs(sn$,0,mat resp$,ckey)
02720   if ckey=5 then goto XIT
02730   if resp$(1)="True" then monthly=1
02740   if resp$(2)="True" then monthly=2
02750   return 
02760 ! ______________________________________________________________________
02770 ! <Updateable Region: ERTN>
02780 ERTN: fnerror(program$,err,line,act$,"xit")
02790   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
02800   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
02810   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
02820 ERTN_EXEC_ACT: execute act$ : goto ERTN
02830 ! /region
