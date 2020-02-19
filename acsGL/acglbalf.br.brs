00020 ! G/L BALANCE SHEET -  STANDARD FOR 8 1/2 * 11 PAPER
00030 !
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnpglen,fnerror,fncno,fnprocess,fnpedat$,fnfscode,fnUseDeptNo,fnpriorcd,fnps,fnGlAskFormatPriorCdPeriod,fnTos,fnLbl,fnTxt,fnCmdKey,fnAcs,fnactpd$,fnactpd
00050   on error goto Ertn
00060 !
00070   dim sc1$(2)*20,bigul$*140,heading$*140,cch$*20,by(13),bp(13)
00080   dim p$(20)*50,accum(10,9,2),total(10),fl1$*256,pedat$*20,cap$*128,udf$*256
00090   dim fundnum(10),funddesc$(10)*20,io1$(20),dolcol$*300,accumcol$*300
00100   dim cnam$*40,b$*3,a$(8)*30,oldtrans$*16,g(8)
00110   dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*14
00115   dim resp$(80)*50
00120 !
00130   fntop(program$,cap$="Fund Comparison Balance Sheet")
00140   fncno(cno,cnam$)
00150   udf$=env$('temp')&'\'
00155   actpd$=fnactpd$ !:
        actpd=fnactpd !:
        fnfscode !:
        fnpriorcd
00160   if fnGlAskFormatPriorCdPeriod=5 then goto XIT !:
          ! sets fnps,fnpriorcd,fnfscode (primary/secondary,current year/Prior,period to print)
00165   fnfscode !:
        fnpriorcd
00170   pors=1
00180   if fnprocess=0 then gosub L2010
00190   mp1=63 : if fnps=2 then mp1=mp1+3
00200   if fnps=2 then !:
          fl1$="Name=[Q]\GLmstr\AcGLFnSc.h[cno]," !:
          fl1$=fl1$&"KFName=[Q]\GLmstr\FnScIndx.h[cno],Shr" else !:
          fl1$="Name=[Q]\GLmstr\ACGLFNSB.h[cno]," !:
          fl1$=fl1$&"KFName=[Q]\GLmstr\FnSBIndx.h[cno],Shr"
00210 L210: form pos 1,n 3,n 6,n 3,pos mp1,pd 3,pos 87,27*pd 6.2
00220   open #1: fl1$,internal,input,keyed 
00230   if fnprocess=1 or fnUseDeptNo=0 then goto L320
00240   goto L350 ! pr NEWPAGE
00250   close #101: ioerr L260
00260 L260: open #101: "SROW=9,SCOL=4,EROW=12,ECOL=75,BORDER=DR,CAPTION=PRINT BALANCE SHEET",display,outIn 
00270   pr f "13,32,C 16,B,5": "Cancel (F5)"
00280   pr f "10,5,c 70,n": "ENTER THE COST CENTER OR DEPT # IF YOU WISH TO ONLY pr A STATEMENT"
00290 L290: pr f "11,5,c 65,n": "ON ONE DEPARTMENT; ELSE ENTER 0 TO pr ALL DEPARTMENTS"
00300   input fields "11,70,N 3,eu,N": costcntr conv L290
00310   if cmdkey=5 then goto XIT
00320 L320: pr newpage
00330   pr f "10,20,c 34,h,n": " BALANCE SHEET IN PROCESS"
00340   pr f "12,2,C 18,B,5": " Press F5 to stop"
00350 L350: fnopenprn
00360   if fnps=2 then goto L390 ! secondary
00370   execute "Index [Q]\GLmstr\GLmstr.h[cno] "&udf$&"fsindex.H[cno] 63 3 Replace DupKeys -N"
00380   goto L400
00390 L390: execute "Index [Q]\GLmstr\GLmstr.h[cno] "&udf$&"fsindex.H[cno] 66 3 Replace DupKeys -N"
00400 L400: open #3: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName="&udf$&"fsindex.h[cno],Shr",internal,input,keyed 
00410   if file$(255)(1:4)<>"PRN:" then redir=1 else redir=0
00420   on fkey 5 goto L1940
00430   report$=cap$
00440 L440: read #1,using L480: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof L1940
00450   if ltrm$(r$)="" or ltrm$(r$)="0" then goto L440
00460   if costcntr=0 then goto L480
00470   if costcntr><fc then goto L440
00480 L480: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
00490   if te$="S" or te$="F" then goto L510
00500   if heading=0 and te$><"R" then gosub L1820
00510 L510: on pos ("RFHDTSPE",te$,1) goto L1290,L1340,L520,L570,L1080,L1290,L1080,L570 none L440
00520 L520: pr #255,using L530: d$
00530 L530: form pos sp,c 50,skip 1
00540   gosub L1480
00550   gosub L1410
00560   goto L440
00570 L570: if notrans=1 then goto L750
00580   if br>=val(r$) and val(r$)><0 then goto L660
00590 L590: ! read gl master file for amounts
00600 L600: read #3,using L210: dno,ano,sno,br,cb,mat by,mat bp eof L740
00610   if br=0 then goto L600
00630   if fnfscode=0 or (fnfscode=actpd and priorcd=1) then goto L660
00640   if fnfscode<1 or fnfscode>12 then let fnfscode(1)
00650   if fnpriorcd=1 then cb=by(fnfscode) else cb=bp(fnfscode)
00660 L660: for x=1 to 10
00670     if dno=fundnum(x) then fund=x ! put in certain column
00680     if fundnum(x)>0 then totcol=x ! total columns needed
00690   next x
00700   if br=val(r$) then total(fund)=total(fund)+cb else goto L720
00710   goto L590
00720 L720: if br<val(r$) then goto L590
00730   if br>val(r$) then goto L750
00740 L740: notrans=1
00750 L750: for j=1 to 10
00760     if te$="E" then total(j)=-accum(j,ap,1)
00770   next j
00780   for j=1 to 9
00790     if ac(j)=9 then goto L830 ! 10/14/87
00800     for j2= 1 to 10
00810       accum(j2,j,1)=accum(j2,j,1)+total(j2)
00820     next j2
00830 L830: next j
00840   if rs=1 then goto L850 else goto L880
00850 L850: for j=1 to 10
00860     total(j)=-total(j)
00870   next j
00880 L880: if ds=1 then dollar$="$" else dollar$=" "
00890 ! If CP=1 Then dOLLAR=50+14*BC Else dOLLAR=24+14*BC
00900   if sum(total)><0 then goto L920
00910   if ls+ul+ds+ic>0 then goto L920 else goto L440
00920 L920: sp2=49-sp-1
00930 ! pr #255,Using 990: D$(1:SP2),DOLLAR$,TOTAL(FUND) Pageoflow 1470
00940   dolcol$=""
00950   for j=1 to totcol
00955     if ul=1 then dolcol$=dolcol$&" "&dollar$&"{\ul "&cnvrt$("PIC(-,---,---.##)",total(j))&" }" : goto L970
00960     dolcol$=dolcol$&" "&dollar$&cnvrt$("PIC(-,---,---.##)",total(j))
00970 L970: next j
00975   if ul=1 then pr #255,using L991: d$(1:sp2),rtrm$(dolcol$) pageoflow L1640 : goto L990
00980   pr #255,using L990: d$(1:sp2),rtrm$(dolcol$) pageoflow L1640
00990 L990: form pos sp,c sp2,pos 49,c big,skip 1
00991 L991: form pos sp,c sp2,pos 49,c 150,skip 1
01000   form pos sp,c sp2,pos dollar,c 1,pic(--,---,---.##),skip redir
01010   form pos sp,c sp2,pos 49,c big,skip redir
01020   mat total=(0)
01030   gosub L1410
01035   if ul=1 then goto L1050
01040   gosub L1670
01050 L1050: gosub L1480
01060   goto L440
01070 !
01080 L1080: if ap=0 then ap=1
01090   accumcol$=""
01100   for j=1 to totcol
01110     if rs=1 then accum1=-accum(j,ap,1) else accum1=accum(j,ap,1)
01120     if ds=1 then dollar$="$" else dollar$=" "
01125     if ul=1 then accumcol$=accumcol$&" "&dollar$&"{\ul "&cnvrt$("pic(-,---,---.##)",accum1)&"}" : goto L1140
01130     accumcol$=accumcol$&" "&dollar$&cnvrt$("pic(-,---,---.##)",accum1)
01140 L1140: next j
01150   sp2=49-sp-1
01155   if ul=1 then pr #255,using L991: d$(1:sp2),rtrm$(accumcol$) pageoflow L1640 : goto L1170
01160   pr #255,using L1170: d$(1:sp2),rtrm$(accumcol$) pageoflow L1640
01170 L1170: form pos sp,c sp2,pos 49,c big,skip redir
01180   gosub L1410
01185   if ul=1 then goto L1200
01190   gosub L1670
01200 L1200: gosub L1480
01210   if te$><"P" then goto L1270
01220   for j=1 to 9
01230     for j2=1 to 10
01240       accum(j2,j,1)=accum(j2,j,1)-accum(j2,ap,1)
01250     next j2
01260   next j
01270 L1270: goto L440
01280 !
01290 L1290: if te$="R" then report$=d$
01300   if te$="S" then secondr$=d$
01310   gosub L1480
01320   goto L440
01330 !
01340 L1340: if foot1=1 then goto L1390
01350   tabnote=sp
01360   foot1=1
01370   foot$=d$
01380   goto L440
01390 L1390: foot$=rtrm$(foot$)&d$
01400   goto L440
01410 L1410: for j=1 to 9
01420     if ac(j)=0 or ac(j)=9 then goto L1460 ! 10/14/87
01430     for j2=1 to 10
01440       accum(j2,j,1)=0
01450     next j2
01460 L1460: next j
01470   return 
01480 L1480: if ls=0 then goto L1630
01490   if ls=99 then goto L1540
01500   pr #255,using L1510: " "
01510 L1510: form pos 1,c 1,skip ls
01520   goto L1630
01530 !
01540 L1540: fnpglen(pglen)
01550 ! If PGLEN<>42 Then pGLEN=58
01560   sk=pglen-krec(255): fl=len(rtrm$(foot$))
01570 ! If PGLEN=42 Then sK=SK+1
01580   pr #255,using L1590: rtrm$(foot$)
01590 L1590: form skip sk,pos tabnote,c fl,skip 1
01600   if eofcode=1 then goto L1630
01610   pr #255: newpage
01620   gosub L1820
01630 L1630: return 
01640 L1640: gosub L1540
01650   continue 
01660 !
01670 L1670: if ul=0 then goto L1800
01680   if ul=1 then goto L1710
01690   underlin$="  ============"
01700   goto L1720
01710 L1710: underlin$="  ____________"
01720 L1720: bigul$=""
01730   for j=1 to totcol : bigul$=bigul$&underlin$ : next j
01740   if ul=1 then pr #255,using L1760: bigul$
01750   if ul=2 then pr #255,using L1770: bigul$
01760 L1760: form skip redir,pos 49,c big,skip redir
01770 L1770: form pos 49,c big,skip redir
01780   if redir=0 then pr #255,using L1790: " "
01790 L1790: form skip 1,c 1,skip redir
01800 L1800: return 
01810 !
01820 L1820: heading=1
01830   pr #255: "\qc  {\f181 \fs24 \b "&env$('cnam')&"}"
01840   pr #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
01850   if trim$(secondr$)<>"" then pr #255: "\qc  {\f181 \fs18 \b "&trim$(secondr$)&"}"
01860   pr #255: "\qc  {\f181 \fs16 \b "&trim$(fnpedat$)&"}"
01870   pr #255: "\ql "
01880   pr #255: 
01890   pr #255: 
01900   pr #255,using L1910: heading$
01910 L1910: form pos 49,c big,skip 2
01920   return 
01930 !
01940 L1940: eofcode=1
01950   gosub L1540
01960   if pors=2 then goto XIT else goto DONE
01970 XIT: fnxit
01980 DONE: ! 
01982   fnfscode(actpd)
01983   fnpriorcd(1)
01984   fncloseprn
01990   goto XIT
02000 !
02010 L2010: pr newpage ! determine fund #s
02020   for j=1 to 10
02030     io1$(j*2-1)=str$(j+4)&",22,NZ 3,UT,n" !:
          io1$(j*2)=str$(j+4)&",28,C 20,UT,N"
02040   next j
02050   open #5: "Name=[Q]\GLmstr\GLfund.h[cno],RecL=230,use",internal,outIn,relative 
02060   read #5,using L2070: mat fundnum,mat funddesc$ ioerr L2080
02070 L2070: form pos 1,10*n 3,10*c 20
02080 L2080: fnTos(sn$="ACglcasf3") !:
        mylen=1: mypos=mylen+3
02090   fnLbl(1,4,"Fund                 Description ")
02100   for j=1 to 10
02110     fnTxt(j+1,mypos,3,0,right,"30",0,"Enter the fund number.") !:
          resp$(j*2-1)=str$(fundnum(j))
02120     fnTxt(j+1,mypos+10,20,0,0,"",0,"Enter the fund description.") !:
          resp$(j*2)=funddesc$(j)
02130   next j
02140   fnCmdKey("&Next",1,1,0,"Continues with financial statement.")
02150   fnCmdKey("&Cancel",5,0,1,"Returns to menu without posting.")
02160   fnAcs(sn$,0,mat resp$,ckey)
02170   if ckey=5 then goto XIT
02180   for j=1 to 10
02190     fundnum(j)=val(resp$(j*2-1))
02200     funddesc$(j)=resp$(j*2)
02210   next j
02220   rewrite #5,using L2070: mat fundnum,mat funddesc$ ioerr L2240
02230   goto L2250
02240 L2240: write #5,using L2070: mat fundnum,mat funddesc$
02250 L2250: close #5: 
02260   for j=1 to 10
02270     if fundnum(j)>0 then !:
            heading$=heading$&" "&lpad$(rtrm$(funddesc$(j)(1:13)),13) !:
            totcol+=1
02280   next j
02290   big=totcol*14
02300   return 
02310 !
02320 ! <Updateable Region: ERTN>
02330 ERTN: fnerror(program$,err,line,act$,"xit")
02340   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
02350   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
02360   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
02370 ERTN_EXEC_ACT: execute act$ : goto ERTN
02380 ! /region
02390 !
