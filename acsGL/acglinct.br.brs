00010 ! Replace S:\acsGL\acglinct
00020 ! -- pr Income Statement !:
        ! FOR 8 1/2 * 11 PAPER WITHOUT PERCENTAGES
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnpglen,fnerror,fnprocess,fncno,fnchain,fnUseDeptNo,fnpedat$,fnps,fnpriorcd,fnfscode,fnactpd$,fncch$,fnGlAskFormatPriorCdPeriod,fnTos,fnLbl,fnTxt,fnCmdKey,fnAcs
00050   on error goto Ertn
00060 ! ______________________________________________________________________
00070   dim fl1$*256,cch$*20,by(13),bp(13),cap$*128,udf$*256
00080   dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*14
00090   dim cnam$*40,b$*3,a$(8)*30,oldtrans$*16,g(8),accum(9,2),sc1$(2)*20
00100 ! ______________________________________________________________________
00110   fntop(program$,cap$="Income Statement")
00120   fncno(cno,cnam$)
00130   udf$=env$('temp')&'\'
00140   actpd$=fnactpd$
00150   if fnGlAskFormatPriorCdPeriod=5 then goto XIT !:
          ! sets fnps,fnpriorcd,fnfscode (primary/secondary,current year/Prior,period to print)
00160   pors=1
00170   mp1=69
00180   if fnps=2 then mp1=mp1+3
00190   if fnps=2 then mp2=78 else mp2=75
00200   if fnps=2 then fl1$="Name=[Q]\GLmstr\ACGLFNSJ.h[cno],KFName=[Q]\GLmstr\FNSJINDX.h[cno],Shr" else !:
          fl1$="Name=[Q]\GLmstr\ACGLFNSI.h[cno],KFName=[Q]\GLmstr\FNSIINDX.h[cno],Shr"
00210   open #1: fl1$,internal,input,keyed 
00220   if fnprocess=1 or fnUseDeptNo=0 then goto L320
00230   fnTos(sn$="GLInput") !:
        mylen=30: mypos=mylen+3 : right=1
00240   fnLbl(1,1,"Cost Center or Department #:",mylen,right)
00250   fnTxt(1,mypos,3,0,right,"30",0,"Enter the cost center or department number if you wish to pr only one department, else leave blank for all.",0 ) !:
        resp$(1)=""
00260   fnLbl(2,1,"(Blank for all Departments)",mylen,right)
00270   fnCmdKey("&Next",1,1,0,"Prints the financial statement.")
00280   fnCmdKey("&Cancel",5,0,1,"Returns to menu without posting.")
00290   fnAcs(sn$,0,mat resp$,ckey)
00300   if ckey=5 then goto XIT
00310   costcntr=val(resp$(1))
00320 L320: fnopenprn !:
        if file$(255)(1:4)<>"PRN:" then redir=1 else redir=0
00330   if fnps=2 then goto L360 ! secondary
00340   execute "Index [Q]\GLmstr\GLmstr.h[cno] "&udf$&"fsindex.H[cno] 69 3 Replace DupKeys -N"
00350   goto L370
00360 L360: execute "Index [Q]\GLmstr\GLmstr.h[cno] "&udf$&"fsindex.H[cno] 72 3 Replace DupKeys -N"
00370 L370: open #3: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName="&udf$&"fsindex.h[cno],Shr",internal,input,keyed 
00380   report$="Statement of Income and Expenses"
00390 ! GOSUB BLDPCT1 ! BUILD % BASED ON REF # IN PRIMARY FUND # IN G/L ACCOUNT
00400 L400: read #1,using L450: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof L1650
00410   if ltrm$(r$)="" or ltrm$(r$)="0" then goto L400
00420   if costcntr=0 then goto L450
00430   if fc=0 and te$="F" then goto L460 ! 5/8/89
00440   if costcntr><fc then goto L400
00450 L450: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
00460 L460: if te$="S" or te$="F" then goto L480
00470   if heading=0 and te$><"R" then gosub L1530
00480 L480: on pos ("RFHDTS",te$,1) goto L1070,L1110,L490,L540,L970,L1070 none L400
00490 L490: pr #255,using L500: d$(1:40)
00500 L500: form pos sp,c 40,skip 1
00510   gosub L1240
00520   gosub L1180
00530   goto L400
00540 L540: if notrans=1 then goto L750
00550   if ir=val(r$) and val(r$)><0 then goto L680
00560   if ir>val(r$) then goto L680
00570 L570: ! read gl master file for amounts
00580 L580: read #3,using L600: ir,pcr,bb,cb,mat by,mat bp eof L740
00585   cb=1
00590   if ir=0 then goto L580 ! skip any gl accounts not pointed to ic
00600 L600: form pos mp1,pd 3,pos mp2,pd 3,pos 81,41*pd 6.2
00610   if fnfscode=0 then goto L680
00620   if fnfscode<1 or fnfscode>13 then let fnfscode(1)
00630 ! If FNPRIORCD=1 Then cB=BY(FNFSCODE) Else cB=BP(FNFSCODE)
00640   if fnpriorcd=2 then goto L670
00650   if fnfscode>1 then bb=by(fnfscode-1) else bb=0
00660   goto L680
00670 L670: if fnfscode>1 then bb=bp(fnfscode-1) else bb=0
00680 L680: if ir=val(r$) then total=total+(cb-bb) else goto L720
00690   total2=total2+cb
00700   k$=cnvrt$("N 5",pcr)
00710   goto L570
00720 L720: if ir<val(r$) then goto L570
00730   if ir>val(r$) then goto L750
00740 L740: notrans=1
00750 L750: for j=1 to 9
00760     if ac(j)=9 then goto L790 ! 10/14/87
00770     accum(j,1)=accum(j,1)+total
00780     accum(j,2)=accum(j,2)+total2
00790 L790: next j
00800   if rs=1 then total=-total else goto L820
00810   total2=-total2
00820 L820: if ds=1 then dollar$="$" else dollar$=" "
00830   if total><0 or total2><0 then goto L850
00840   if ls+ul+ds+ic>0 then goto L850 else goto L400
00850 L850: sp2=49-sp-1
00860   pr #255,using L870: d$(1:sp2),dollar$,total,dollar$,total2 pageoflow L1390
00870 L870: form pos sp,c sp2,pos 49,c 1,pic(-----,---,---.##),pos 67,c 1,pic(-------,---,---.##),skip redir
00880   if pc0=1 then gosub BLDPCT2
00890   if pc3>0 or pc4>0 then pr #255,using L900: pc3,pc4
00900 L900: form pos 63,n 4,pos 82,n 4,skip redir
00910   total=0
00920   total2=0
00930   gosub L1180
00940   gosub L1410
00950   gosub L1240
00960   goto L400
00970 L970: if ap=0 then ap=1
00980   if rs=1 then accum1=-accum(ap,1) else accum1=accum(ap,1)
00990   if rs=1 then accum2=-accum(ap,2) else accum2=accum(ap,2)
01000   if ds=1 then dollar$="$" else dollar$=" "
01010   sp2=49-sp-1
01020   pr #255,using L870: d$(1:sp2),dollar$,accum1,dollar$,accum2 pageoflow L1390
01030   gosub L1180
01040   gosub L1410
01050   gosub L1240
01060   goto L400
01070 L1070: if te$="R" then report$=d$
01080   if te$="S" then secondr$=d$
01090   gosub L1240
01100   goto L400
01110 L1110: if foot1=1 then goto L1160
01120   tabnote=sp
01130   foot1=1
01140   foot$=d$
01150   goto L400
01160 L1160: foot$=rtrm$(foot$)&d$
01170   goto L400
01180 L1180: for j=1 to 9
01190     if ac(j)=0 or ac(j)=9 then goto L1220 ! 10/14/87
01200     accum(j,1)=0
01210     accum(j,2)=0
01220 L1220: next j
01230   return 
01240 L1240: if ls=0 then goto L1380
01250   if ls=99 then goto L1290
01260   pr #255,using L1270: " "
01270 L1270: form pos 1,c 1,skip ls
01280   goto L1380
01290 L1290: fnpglen(pglen)
01300 ! If PGLEN<>42 Then pGLEN=58
01310   sk=pglen-krec(255): fl=len(rtrm$(foot$))
01320 ! If PGLEN=42 Then sK=SK+1
01330   pr #255,using L1340: rtrm$(foot$),"Page "&str$(pt1)
01340 L1340: form skip sk,pos tabnote,c fl,pos 80,c 8,skip 1
01350   if eofcode=1 then goto L1380
01360   pr #255: newpage
01370   gosub L1530
01380 L1380: return 
01390 L1390: gosub L1290
01400   continue 
01410 L1410: if ul=0 then goto L1500
01420   if ul=1 then goto L1470
01430   underlin$="============="
01440   pr #255,using L1450: underlin$&"====",underlin$&"======"
01450 L1450: form skip 1,pos 49,c 17,pos 67,c 19,skip redir
01460   goto L1500
01470 L1470: underlin$="______________"
01480   pr #255,using L1490: underlin$&"___",underlin$&"_____"
01490 L1490: form skip redir,pos 49,c 18,pos 67,c 19,skip redir
01500 L1500: if redir=0 then pr #255,using L1510: " "
01510 L1510: form skip 1,c 1,skip redir
01520   return 
01530 L1530: heading=1
01540   if pt1=0 then pt1=1 else pt1=pt1+1
01550   pr #255: "\qc  {\f181 \fs18 \b "&env$('cnam')&"}"
01560   pr #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
01570   if trim$(secondr$)<>"" then pr #255: "\qc  {\f181 \fs24 \b "&trim$(secondr$)&"}"
01580   pr #255: "\qc  {\f181 \fs16 \b For the "&rtrm$(actpd$)&" month period ended "&rtrm$(fnpedat$)&"}"
01590   pr #255: "\ql "
01600   pr #255: 
01610   pr #255,using L1620: lpad$(rtrm$(fncch$),20),"Year To Date"
01620 L1620: form pos 45,c 20,pos 73,c 12,skip 2
01630   return 
01640 ! ______________________________________________________________________
01650 L1650: eofcode=1
01660   gosub L1290
01670 ! 
01680   fncloseprn
01690   goto XIT
01700 ! ______________________________________________________________________
01710 BLDPCT1: open #10: "Name="&env$('temp')&"\Work."&session$&",KFName="&env$('Temp')&"\Addr."&session$&",Replace,RecL=17,KPS=1,KLN=5",internal,outIn,keyed 
01720   for j=1 to lrec(3)
01730     read #3,using L1740,rec=j: pc1,bb,cb noRec L1830
01735     cb=1
01740 L1740: form pos mp1,pd 3,pos 81,2*pd 6.2
01750     k$=cnvrt$("N 5",pc1)
01760     read #10,using L1770,key=k$: pc1,pc2,yt2 nokey L1820
01770 L1770: form pos 1,g 5,2*pd 6.2
01780     pc2=pc2+cb-bb
01790     yt2=yt2+cb
01800     rewrite #10,using L1770: pc1,pc2,yt2
01810     goto L1830
01820 L1820: write #10,using L1770: pc1,cb-bb,cb
01830 L1830: next j
01840   pc0=1
01850   return 
01860 ! ______________________________________________________________________
01870 BLDPCT2: ! 
01880   pc3=pc4=0
01890   if val(k$)=0 then goto L1970
01900   read #10,using L1770,key=k$: pc1,pc2,yt2 nokey L1970
01910   if total=0 then goto L1940
01920   pc3=round(((total-pc2)/total)*100,0)
01930   if pc3<-999 or pc3>9999 then pc3=0
01940 L1940: if total2=0 then goto L1970
01950   pc4=round(((total2-yt2)/total2)*100,0)
01960   if pc4<-999 or pc4>9999 then pc4=0
01970 L1970: return 
01980 ! ______________________________________________________________________
01990 XIT: fnxit
02000 ! ______________________________________________________________________
02010 ! <Updateable Region: ERTN>
02020 ERTN: fnerror(program$,err,line,act$,"xit")
02030   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
02040   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
02050   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
02060 ERTN_EXEC_ACT: execute act$ : goto ERTN
02070 ! /region
