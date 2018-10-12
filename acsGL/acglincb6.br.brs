00010 ! Replace S:\acsGL\AcGlIncB6
00020 ! -- INCOME STATEMENT WITH BUDGET (six columns)
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnpglen,fnerror,fncno,fnGlAskFormatPriorCdPeriod,fncch$,fnpedat$,fnactpd$,fnactpd,fnfscode,fnUseDeptNo,fnpriorcd,fnTos,fnprocess,fnLbl,fnTxt,fnCmdKey,fnAcs,fnps
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim fl1$*256,p$(20)*50
00080   dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*14
00090   dim cnam$*40,b$*3,a$(8)*30,oldtrans$*16,g(8),accum(9,7)
00100   dim pedat$*20,actpd$*6,bm(13),bp(13),by(13),cap$*128,udf$*256
00110 ! ______________________________________________________________________
00120   fntop(program$,cap$="Six Column Income Statement with Budget")
00130   on fkey 5 goto L2350
00140   fncno(cno,cnam$)
00150   udf$=env$('temp')&'\'
00160   actpd=fnactpd
00170   actpd$=fnactpd$
00180   pedat$=rtrm$(fnpedat$)
00190   x=pos(pedat$," ",1)
00200   curmonth$=pedat$(1:x)
00210   curyear$=pedat$(len(rtrm$(pedat$))-4:len(rtrm$(pedat$)))
00220   curyear=val(curyear$) conv L230
00230 L230: prioryr=curyear-1
00240   if fnGlAskFormatPriorCdPeriod=5 then goto XIT !:
          ! sets fnps,fnpriorcd,fnfscode (primary/secondary,current year/Prior,period to print)
00250 ! ______________________________________________________________________
00260   pr newpage
00270   pors=1
00280   mp1=69
00290   if fnps=2 then mp1=mp1+3
00300   fl1$="Name=[Q]\GLmstr\ACGLFNSI.h[cno],KFName=[Q]\GLmstr\FNSIINDX.h[cno],Shr"
00310   if fnps=2 then fl1$="Name=[Q]\GLmstr\ACGLFNSJ.h[cno],KFName=[Q]\GLmstr\FNSJINDX.h[cno],Shr"
00320   form c 9,skip 0
00330   form c 7,skip 0
00340   nametab=int(44-len(rtrm$(cnam$))/2)
00350   open #1: fl1$,internal,input,keyed 
00360   if fnprocess=1 or fnUseDeptNo=0 then goto L450
00370   fnTos(sn$="ACglincb") !:
        mylen=30: mypos=mylen+3 : right=1
00380   fnLbl(1,1,"Cost Center or Department #:",mylen,right)
00390   fnTxt(1,mypos,3,0,right,"30",0,"Enter the cost center or department number if you wish to pr only one department, else leave blank for all.",0 ) !:
        resp$(1)=""
00400   fnLbl(2,1,"(Blank for all Departments)",mylen,right)
00410   fnCmdKey("&Next",1,1,0,"Prints the financial statement.")
00420   fnCmdKey("&Cancel",5,0,1,"Returns to menu without posting.")
00430   fnAcs(sn$,0,mat resp$,ckey)
00440   if ckey=5 then goto XIT
00450 L450: costcntr=val(resp$(1))
00460   cnam$=rtrm$(cnam$)
00470   pf1=len(cnam$)+int((43-len(cnam$))/2)
00480   report$="STATEMENT OF INCOME AND EXPENSES"
00490   fnopenprn(cp,58,220,process)
00500   redir=0: if file$(255)(1:4)<>"PRN:" then redir=1
00510   if fnps=2 then goto L540 ! secondary
00520   execute "Index [Q]\GLmstr\GLmstr.h[cno] "&udf$&"fsindex.H[cno] 69 3 Replace DupKeys -N"
00530   goto L550
00540 L540: execute "Index [Q]\GLmstr\GLmstr.h[cno] "&udf$&"fsindex.H[cno] 72 3 Replace DupKeys -N"
00550 L550: open #3: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName="&udf$&"fsindex.h[cno],Shr",internal,input,keyed 
00560 L560: read #1,using L610: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof L2350
00570   if ltrm$(r$)="" or ltrm$(r$)="0" then goto L560
00580   if costcntr=0 then goto L610
00590   if fc=0 and te$="F" then goto L620 ! 5/08/1989
00600   if costcntr><fc then goto L560
00610 L610: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
00620 L620: if te$="S" or te$="F" then goto L640
00630   if heading=0 and te$><"R" then gosub L2190
00640 L640: on pos ("RFHDTS",te$,1) goto L1640,L1680,L650,L700,L1430,L1640 none L560
00650 L650: pr #255,using L660: d$(1:40)
00660 L660: form pos sp,c 40,skip 1
00670   gosub L1860
00680   gosub L1750
00690   goto L560
00700 L700: if notrans=1 then goto L1010
00710   if ir>=val(r$) and val(r$)><0 then goto L830
00720 L720: ! read amounts from gl master file
00730 L730: read #3,using L820: ir,bb,cb,mat by,mat bp,mat bm eof L1000
00740   if ir=0 then goto L730 ! skip accounts with no income reference #
00750   if fnfscode=0 or (fnfscode=actpd and fnpriorcd=1) then goto L820
00760   if fnfscode<1 or fnfscode>13 then let fnfscode=1
00770   if fnpriorcd=1 then cb=by(fnfscode) else cb=bp(fnfscode)
00780   if fnpriorcd=2 then goto L810
00790   if fnfscode>1 then bb=by(fnfscode-1) else bb=0
00800   goto L820
00810 L810: if fnfscode>1 then bb=bp(fnfscode-1) else bb=0
00820 L820: form pos mp1,pd 3,pos 81,41*pd 6.2
00830 L830: if ir=val(r$) then total=total+(cb-bb) else goto L980
00840   total2=total2+cb
00850   for z=1 to 13
00860     annualb=annualb+bm(z)
00870   next z
00880   if fnfscode=0 then pmonth=pmonth+bm(actpd) else pmonth=pmonth+bm(fnfscode) ! 11/24/86
00890   if fnfscode=0 then goto L900 else goto L940 ! 11/24/86
00900 L900: for j=1 to actpd
00910     lastyr=lastyr+bm(j)
00920   next j
00930   goto L720
00940 L940: for j=1 to fnfscode ! 11/24/86
00950     lastyr=lastyr+bm(j) ! 11/24/86
00960   next j ! 11/24/86
00970   goto L720 ! 11/24/86
00980 L980: if ir<val(r$) then goto L720
00990   if ir>val(r$) then goto L1010
01000 L1000: notrans=1
01010 L1010: overundr=lastyr-total2
01020   unexpend=annualb-total2
01030   for j=1 to 9
01040     if ac(j)=9 then goto L1120 ! 10/14/87
01050     accum(j,1)=accum(j,1)+total
01060     accum(j,2)=accum(j,2)+total2
01070     accum(j,3)=accum(j,3)+annualb
01080     accum(j,4)=accum(j,4)+pmonth
01090     accum(j,5)=accum(j,5)+lastyr
01100     accum(j,6)=accum(j,6)+overundr
01110     accum(j,7)=accum(j,7)+unexpend
01120 L1120: next j
01130   if rs=1 then total=-total else goto L1200
01140   total2=-total2
01150   annualb=-annualb
01160   pmonth=-pmonth
01170   lastyr=-lastyr
01180   overundr=overundr
01190   unexpend=unexpend
01200 L1200: if ds=1 then dollar$="$" else dollar$=" "
01210   goto L1250 ! pr all accounts even if zero balance  (if budget ever nets to zero, it messes the monthly budget column up
01220   if annualb><0 or total2><0 then goto L1250
01230   if total<>0 then goto L1250
01240   if ls+ds+ul+ic>0 then goto L1250 else goto L560
01250 L1250: sp2=26-sp-1
01260   if ul=1 then pr #255,using L1570: d$(1:sp2),dollar$,"{\ul ",annualb,"}",ar$,"{\ul ",total,"}",ar$,"{\ul ",pmonth,"}",dollar$,"{\ul ",total2,"}",dollar$,"{\ul ",lastyr,"}",dollar$,"{\ul ",unexpend,"}" pageoflow L2040 : goto L1300
01280   pr #255,using L1290: d$(1:sp2),dollar$,annualb,dollar$,total,dollar$,pmonth,dollar$,total2,dollar$,lastyr,dollar$,unexpend pageoflow L2040
01290 L1290: form pos sp,c sp2,pos 26,c 1,n 13.2,x 1,c 1,n 11.2,x 1,c 1,n 11.2,x 1,c 1,n 13.2,x 1,c 1,n 13.2,x 1,c 1,n 13.2,x 1,skip 1
01300 L1300: form pos sp,c sp2,pos 26,c 1,n 13.2,x 1,c 1,n 11.2,x 1,c 1,n 11.2,x 1,c 1,n 13.2,x 1,c 1,n 13.2,x 1,c 1,n 13.2,x 1,c 1,n 13.2,skip redir
01310   total=0
01320   total2=0
01330   annualb=0
01340   pmonth=0
01350   lastyr=0
01360   overundr=0
01370   unexpend=0
01380   gosub L1750
01390   if ul=1 then goto L1410
01400   gosub L2050
01410 L1410: gosub L1860
01420   goto L560
01430 L1430: if ap=0 then ap=1
01440   if rs=1 then accum1=-accum(ap,1) else accum1=accum(ap,1)
01450   if rs=1 then accum2=-accum(ap,2) else accum2=accum(ap,2)
01460   if rs=1 then accum3=-accum(ap,3) else accum3=accum(ap,3)
01470   if rs=1 then accum4=-accum(ap,4) else accum4=accum(ap,4)
01480   if rs=1 then accum5=-accum(ap,5) else accum5=accum(ap,5)
01490   if rs=1 then accum6=accum(ap,6) else accum6=accum(ap,6)
01500   if rs=1 then accum7=accum(ap,7) else accum7=accum(ap,7)
01510   if ds=1 then dollar$="$" else dollar$=" "
01520   sp2=26-sp-1
01530   if ul=1 then pr #255,using L1570: d$(1:sp2),dollar$,"{\ul ",accum3,"}",dollar$,"{\ul ",accum1,"}",dollar$,"{\ul ",accum4,"}",dollar$,"{\ul ",accum2,"}",dollar$,"{\ul ",accum5,"}",dollar$,"{\ul ",accum7,"}" pageoflow L2040 : goto L1580
01540 ! pr #255,Using 1210: D$(1:SP2),DOLLAR$,ACCUM3,DOLLAR$,ACCUM1,DOLLAR$,ACCUM4,DOLLAR$,ACCUM2,DOLLAR$,ACCUM5,DOLLAR$,ACCUM6,DOLLAR$,ACCUM7 Pageoflow 1890
01550   pr #255,using L1560: d$(1:sp2),dollar$,accum3,dollar$,accum1,dollar$,accum4,dollar$,accum2,dollar$,accum5,dollar$,accum7 pageoflow L2040
01560 L1560: form pos sp,c sp2,pos 26,c 1,n 13.2,x 1,c 1,n 11.2,x 1,c 1,n 11.2,x 1,c 1,n 13.2,x 1,c 1,n 13.2,x 1,c 1,n 13.2,x 1
01570 L1570: form pos sp,c sp2,pos 26,c 1,c 5,n 13.2,c 1,x 1,c 1,c 5,n 11.2,c 1,x 1,c 1,c 5,n 11.2,c 1,x 1,c 1,c 5,n 13.2,c 1,x 1,c 1,c 5,n 13.2,c 1,x 1,c 1,c 5,n 13.2,c 1,x 1
01580 L1580: ft1=0
01590   gosub L1750
01600   if ul=1 then goto L1620
01610   gosub L2050
01620 L1620: gosub L1860
01630   goto L560
01640 L1640: if te$="R" then report$=d$
01650   if te$="S" then secondr$=d$
01660   gosub L1860
01670   goto L560
01680 L1680: if foot1=1 then goto L1730
01690   tabnote=sp
01700   foot1=1
01710   foot$=d$
01720   goto L560
01730 L1730: foot$=rtrm$(foot$)&d$
01740   goto L560
01750 L1750: for j=1 to 9
01760     if ac(j)=0 or ac(j)=9 then goto L1840 ! 10/14/87
01770     accum(j,1)=0
01780     accum(j,2)=0
01790     accum(j,3)=0
01800     accum(j,4)=0
01810     accum(j,5)=0
01820     accum(j,6)=0
01830     accum(j,7)=0
01840 L1840: next j
01850   return 
01860 L1860: if ls=0 then goto L2020
01870   if ls=99 then goto L1910
01880   pr #255,using L1890: " "
01890 L1890: form pos 1,c 1,skip ls
01900   goto L2020
01910 L1910: ! If FT1=1 Then Goto 1870
01920   fnpglen(pglen)
01930 ! If PGLEN<>42 Then pGLEN=58
01940   sk=pglen-krec(255): fl=len(rtrm$(foot$))
01950 ! If PGLEN=42 Then sK=SK+1
01960   pr #255,using L1970: rtrm$(foot$),"Page "&str$(pt1)
01970 L1970: form skip sk,pos tabnote,c fl,pos 115,c 8,skip 1
01980 ! ft1=1
01990   if eofcode=1 then goto L2020
02000   pr #255: newpage
02010   gosub L2190
02020 L2020: return 
02030 ! ______________________________________________________________________
02040 L2040: gosub L1910: continue 
02050 L2050: if ul=0 then goto L2150
02060   if ul=1 then goto L2100
02070   underlin$="=============="
02080   goto L2110
02090   goto L2150
02100 L2100: underlin$="______________"
02110 L2110: ! pr #255,Using 1980: UNDERLIN$,UNDERLIN$(1:12),UNDERLIN$(1:12),UNDERLIN$,UNDERLIN$,UNDERLIN$,UNDERLIN$
02120   pr #255,using L2130: underlin$,underlin$(1:12),underlin$(1:12),underlin$,underlin$,underlin$
02130 L2130: form pos 30,c 15,2*c 13,4*c 15,skip 0
02140   form skip redir,pos 26,c 15,2*c 13,4*c 15,skip redir
02150 L2150: if redir=0 then pr #255,using L2160: " " pageoflow L2040
02160 L2160: form skip 1,c 1,skip 0
02170   return 
02180 ! ______________________________________________________________________
02190 L2190: heading=1
02200   pt1+=1
02210   pr #255: "\qc  {\f181 \fs24 \b "&env$('cnam')&"}"
02220   pr #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
02230   if trim$(secondr$)<>"" then pr #255: "\qc  {\f181 \fs18 \b "&trim$(secondr$)&"}"
02240   pr #255: "\qc  {\f181 \fs16 \b For the "&rtrm$(actpd$)&" month period ended "&rtrm$(fnpedat$)&"}"
02250   pr #255: "\ql "
02260   pr #255: 
02270 ! pr #255: TAB(33);"ANNUAL";TAB(40);"<--";FNCCH$;"-->";TAB(66);" <--     YEAR TO DATE      -->";TAB(97);"<--     BUDGET TO DATE    -->"
02280   pr #255,using L2290: "ANNUAL",curmonth$,curmonth$,"BAL YTD","BAL YTD","UNEXPENDED"
02290 L2290: form pos 27,cr 13,cr 13,cr 13,cr 15,cr 15,cr 15,skip 1
02300   pr #255,using L2290: "BUDGET",str$(curyear),str$(prioryr),str$(curyear),str$(prioryr),str$(curyear)
02310 ! pr #255: TAB(33);"BUDGET";TAB(45);"BALANCE";TAB(60);"BUDGET";TAB(73);"BALANCE";TAB(90);"BUDGET";TAB(101);"OVER/UNDER";TAB(116);"UNEXPENDED"
02320   pr #255: 
02330   return 
02340 ! ______________________________________________________________________
02350 L2350: eofcode=1
02360   gosub L1910
02370   fnfscode(actpd)
02380   fnpriorcd(1)
02388   fnfscode(actpd)
02389   fnpriorcd(1)
02390   fncloseprn
02400 ! 
02410 XIT: fnxit
02420 ! ______________________________________________________________________
02430 ! <updateable region: ertn>
02440 ERTN: fnerror(program$,err,line,act$,"xit")
02450   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
02460   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
02470   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
02480 ERTN_EXEC_ACT: execute act$ : goto ERTN
02490 ! /region
