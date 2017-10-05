00010 ! Replace S:\acsGL\ACGLIncP
00020 ! -- INCOME STATEMENT FOR 8 1/2 * 11 PAPER WITH PERCENTAGES
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnpglen,fncno,fnerror,fnprocess,fncch$,fnpedat$,fnactpd$,fnfscode,fnUseDeptNo,fnpriorcd,fnps,fnglfs,fnactpd,fntos,fnlbl,fntxt,fncmdkey,fnacs
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim fl1$*256,tp1(4),by(13),cap$*128
00080   dim cnam$*40,b$*3,a$(8)*30,oldtrans$*16,g(8),accum(9,2),bp(13),udf$*256
00090   dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*25
00100 ! ______________________________________________________________________
00110   fntop(program$,cap$="Income Statement with Percents")
00120   fncno(cno,cnam$)
00130   let udf$=env$('temp')&'\'
00140   actpd=fnactpd
00150   actpd$=fnactpd$
00155   let fscode=fnfscode
00156   priorcd=fnpriorcd
00160   if fnglfs=5 then goto XIT !:
          ! sets fnps,priorcd,fnfscode (primary/secondary,current year/Prior,period to print)
00165   priorcd=fnpriorcd
00166   let fscode=fnfscode
00170   if fnps=2 then mp1=72 else mp1=69
00180   if fnps=2 then fl1$="Name="&env$('Q')&"\GLmstr\ACGLFNSJ.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\FNSJINDX.h"&str$(cno)&",Shr" else !:
          fl1$="Name="&env$('Q')&"\GLmstr\ACGLFNSI.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\FNSIINDX.h"&str$(cno)&",Shr"
00190 L190: form pos mp1,pd 3,pos 81,41*pd 6.2
00200   pas=1 : open #hwork:=4: "Name="&env$('temp')&"\Work."&session$&",KFName=IDX."&wsid$&",Replace,RecL=33,KPS=1,KLN=5",internal,outin,keyed 
00210 L210: acglfnsi=1 !:
        open #acglfnsi: fl1$,internal,outin,keyed 
00220   if fnprocess=1 or fnUseDeptNo=0 then goto L330
00230   if percent=1 then goto L330
00240   fntos(sn$="ACGlincp") !:
        mylen=30: mypos=mylen+3 : right=1
00250   fnlbl(1,1,"Cost Center or Department #:",mylen,right)
00260   fntxt(1,mypos,3,0,right,"30",0,"Enter the cost center or department number if you wish to pr only one department, else leave blank for all.",0 ) !:
        resp$(1)=""
00270   fnlbl(2,1,"(Blank for all Departments)",mylen,right)
00280   fncmdkey("&Next",1,1,0,"Prints the financial statement.")
00290   fncmdkey("&Cancel",5,0,1,"Returns to menu without posting.")
00300   fnacs(sn$,0,mat resp$,ckey)
00310   if ckey=5 then goto XIT
00320   costcntr=val(resp$(1))
00330 L330: on fkey 5 goto L1910
00340   fnopenprn !:
        if file$(255)(1:4)<>"PRN:" then redir=1 else redir=0
00350   if fnps=2 then goto L380 ! secondary
00360   execute "Index "&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&" "&udf$&"fsindex.H"&str$(cno)&" 69 3 Replace DupKeys -N"
00370   goto L390
00380 L380: execute "Index "&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&" "&udf$&"fsindex.H"&str$(cno)&" 72 3 Replace DupKeys -N"
00390 L390: open #3: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&",KFName="&udf$&"fsindex.h"&str$(cno)&",Shr",internal,input,keyed 
00400   report$="Statement of Income and Expenses"
00410 READ_ACGLFNSI: ! 
00420 L420: read #acglfnsi,using L470: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc,rnp eof L1880,conv L2060
00430   if ltrm$(r$)="" or ltrm$(r$)="0" then goto READ_ACGLFNSI
00440   if costcntr=0 then goto L470
00450   if fc=0 and te$="F" then goto L480 ! 5/08/89
00460   if costcntr><fc then goto READ_ACGLFNSI
00470 L470: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3,n 5
00480 L480: if te$="S" or te$="F" then goto L500
00490   if heading=0 and te$><"R" then gosub L1730
00500 L500: on pos ("RFHDTS",te$,1) goto L1250,L1300,L510,L570,L1040,L1250 none READ_ACGLFNSI
00510 L510: if percent=0 then goto L550
00520   pr #255,using L530: d$(1:40)
00530 L530: form pos sp,c 40,skip 1
00540   gosub L1420
00550 L550: gosub L1370
00560   goto READ_ACGLFNSI
00570 L570: if notrans=1 then goto L750
00580   if ir>=val(r$) and val(r$)><0 then goto L690
00590 L590: ! read balances from gl master file
00600 L600: read #3,using L190: ir,bb,cb,mat by,mat bp eof L740
00610   if ir=0 then goto L600
00620   if fscode=0 or fscode=actpd then goto L690
00630   if fscode<1 or fscode>13 then let fscode=1
00640   if priorcd=1 then cb=by(fscode) else cb=bp(fscode)
00650   if priorcd=2 then goto L680
00660   if fscode>1 then bb=by(fscode-1) else bb=0
00670   goto L690
00680 L680: if fscode>1 then bb=bp(fscode-1) else bb=0
00690 L690: if ir=val(r$) then total=total+(cb-bb) else goto L720
00700   total2=total2+cb
00710   goto L590
00720 L720: if ir<val(r$) then goto L590
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
00830   if ds=1 then percent$="%" else percent$=" "
00840   if total><0 or total2><0 then goto L860
00850   if ls+ds+ul>0 then goto L860 else goto L420
00860 L860: if percent=0 then goto L970
00870   if pas=2 then gosub PAS2
00880   sp2=31-sp-1
00890   if percent1><0 then monthpct=total/percent1*100 else monthpct=0
00900   if percent2><0 then let ytdpct=total2/percent2*100 else let ytdpct=0
00910   if monthpct<-999.99 then monthpct=-999.99
00920   if monthpct>999.99 then monthpct=999.99
00930   if ytdpct<-999.99 then let ytdpct=-999.99
00940   if ytdpct>999.99 then let ytdpct=999.99
00945   if ul=1 then pr #255,using L961: d$(1:sp2),dollar$,"{\UL ",total,"}",monthpct,percent$,dollar$,"{\UL ",total2,"}",ytdpct,percent$ pageoflow L1590 : goto L960
00950   pr #255,using L960: d$(1:sp2),dollar$,total,monthpct,percent$,dollar$,total2,ytdpct,percent$ pageoflow L1590
00960 L960: form pos sp,c sp2,pos 31,c 1,pic(-----,---,---.##),pic(-----.##),c 2,x 3,c 1,pic(-----,---,---.##),pic(-----.##),c 1,skip redir
00961 L961: form pos sp,c sp2,pos 31,c 1,c 5,pic(-----,---,---.##),c 1,pic(-----.##),c 2,x 3,c 1,c 5,pic(-----,---,---.##),c 1,pic(-----.##),c 1,skip redir
00970 L970: if pas=1 then tp1=total : tp2=total2 : gosub PAS1
00980   total=0
00990   total2=0
01000   gosub L1370
01005   if ul=1 then goto L1020
01010   gosub L1600
01020 L1020: gosub L1420
01030   goto READ_ACGLFNSI
01040 L1040: if ap=0 then ap=1
01050   if rs=1 then accum1=-accum(ap,1) else accum1=accum(ap,1)
01060   if rs=1 then accum2=-accum(ap,2) else accum2=accum(ap,2)
01070   if ds=1 then dollar$="$" else dollar$=" "
01080   if ds=1 then percent$="%" else percent$=" "
01090   if percent=0 then goto L1180
01100   if percent1><0 then monthpct=accum1/percent1*100 else monthpct=0
01110   if percent2><0 then let ytdpct=accum2/percent2*100 else let ytdpct=0
01120   if monthpct<-999.99 then monthpct=-999.99
01130   if monthpct>999.99 then monthpct=999.99
01140   if ytdpct<-999.99 then let ytdpct=-999.99
01150   if ytdpct>999.99 then let ytdpct=999.99
01160   sp2=31-sp-1
01165   if ul=1 then pr #255,using L961: d$(1:sp2),dollar$,"{\UL ",accum1,"}",monthpct,percent$,dollar$,"{\UL ",accum2,"}",ytdpct,percent$ pageoflow L1590 : goto L1180
01170   pr #255,using L960: d$(1:sp2),dollar$,accum1,monthpct,percent$,dollar$,accum2,ytdpct,percent$ pageoflow L1590
01180 L1180: if pas=1 then tp1=accum1 : tp2=accum2 : gosub PAS1
01190   gosub L1370
01200   if pas=2 then gosub PAS2
01205   if ul=1 then goto L1220
01210   gosub L1600
01220 L1220: gosub L1420
01230   goto READ_ACGLFNSI
01240 ! ______________________________________________________________________
01250 L1250: if te$="R" then report$=d$
01260   if te$="S" then secondr$=d$
01270   gosub L1420
01280   goto READ_ACGLFNSI
01290 ! ______________________________________________________________________
01300 L1300: if foot1=1 then goto L1340
01310   tabnote=sp : let foot1=1 : let foot$=d$
01320   goto READ_ACGLFNSI
01330 ! ______________________________________________________________________
01340 L1340: let foot$=rtrm$(foot$)&d$
01350   goto READ_ACGLFNSI
01360 ! ______________________________________________________________________
01370 L1370: ! r:
01372   for j=1 to 9
01380     if ac(j)=0 or ac(j)=9 then 
01382       goto L1390 
01384     else 
01386       accum(j,1)=0 : accum(j,2)=0
01388     end if
01390 L1390: !
01392     next j
01400   return ! /r
01410 ! ______________________________________________________________________
01420 L1420: !
01422   if percent=0 then goto L1570
01430   if ls=0 then goto L1570
01440   if ls=99 then goto L1480
01450   pr #255,using L1460: " "
01460 L1460: form pos 1,c 1,skip ls
01470   goto L1570
01480 L1480: fnpglen(pglen)
01490 ! If PGLEN<>42 Then pGLEN=58
01500   sk=pglen-krec(255): fl=len(rtrm$(foot$))
01510 ! If PGLEN=42 Then sK+=1
01520   pr #255,using L1530: rtrm$(foot$),"Page "&str$(pt1)
01530 L1530: form skip sk,pos tabnote,c fl,pos 80,c 8
01540   if eofcode=1 then goto L1570
01550   pr #255: newpage
01560   gosub L1730
01570 L1570: !
01572 return 
01580 ! ______________________________________________________________________
01590 L1590: gosub L1480: continue 
01600 L1600: if percent=0 then goto L1710
01610   if ul=0 then goto L1700
01620   if ul=1 then goto L1670
01630   let underlin$="================= ======="
01640   pr #255,using L1650: underlin$,underlin$
01650 L1650: form pos 31,c 25,pos 61,c 25,skip redir
01660   goto L1700
01670 L1670: let underlin$="_________________ _______"
01680   pr #255,using L1690: underlin$,underlin$
01690 L1690: form skip redir,pos 31,c 25,pos 61,c 25,skip redir
01700 L1700: if redir=0 then pr #255: ""
01710 L1710: return 
01720 ! ______________________________________________________________________
01730 L1730: heading=1
01740   pt1+=1
01750   pr #255: "\qc  {\f181 \fs24 \b "&env$('cnam')&"}"
01760   pr #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
01770   if trim$(secondr$)<>"" then pr #255: "\qc  {\f181 \fs18 \b "&trim$(secondr$)&"}"
01780   pr #255: "\qc  {\f181 \fs16 \b For the "&rtrm$(fnactpd$)&" month period ended "&rtrm$(fnpedat$)&"}"
01790   pr #255: "\ql "
01800   pr #255: 
01810   pr #255,using L1840: fncch$,"       Year To Date"
01820   pr #255,using L1830: "_________________________","_________________________"
01830 L1830: form pos 31,c 25,pos 61,c 25,skip redir
01840 L1840: form pos 38,c 20,pos 61,c 25,skip redir
01850   pr #255: 
01860   return 
01870 ! ______________________________________________________________________
01880 L1880: if pas=2 then goto L1910
01890   pas=2 : percent=1 : percent1=percent2=0
01900   goto L1970
01910 L1910: eofcode=1
01920   gosub L1480
01930 ! ______________________________________________________________________
01940   fncloseprn
01945   fnfscode(actpd)
01946   fnpriorcd(1)
01950   goto XIT
01960 ! ______________________________________________________________________
01970 L1970: close #acglfnsi: 
01980   close #3: 
01990   total=total2=0
02000   mat accum=(0)
02010   let foot1=0
02020   let foot$=" "
02030   ir=notrans=0
02040   goto L210
02050 ! ______________________________________________________________________
02060 L2060: read #acglfnsi,using 'Form Pos 1,C 5': r$
02070   delete #acglfnsi: 
02080   goto L420
02090 ! ______________________________________________________________________
02100 PAS1: if rnp=0 then goto L2170
02110   read #hwork,using L2120,key=r$: k4$,mat tp1 nokey L2160
02120 L2120: form pos 1,g 5,4*pd 7.2
02130   tp1(1)=tp1 : tp1(2)=tp2
02140   rewrite #hwork,using L2120: k4$,mat tp1
02150   goto L2170
02160 L2160: write #hwork,using L2120: r$,tp1,tp2,0,0
02170 L2170: return 
02180 ! ______________________________________________________________________
02190 PAS2: mat tp1=(0)
02200   if rnp=0 then goto L2250
02210   k4$=lpad$(str$(rnp),5)
02220   read #hwork,using L2120,key=k4$: k4$,mat tp1 nokey L2230
02230 L2230: percent1=tp1(1)
02240   percent2=tp1(2)
02250 L2250: return 
02260 ! ______________________________________________________________________
02270 XIT: fnxit
02280 ! ______________________________________________________________________
02290 ! <updateable region: ertn>
02300 ERTN: fnerror(program$,err,line,act$,"xit")
02310   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
02320   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
02330   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
02340 ERTN_EXEC_ACT: execute act$ : goto ERTN
02350 ! /region
