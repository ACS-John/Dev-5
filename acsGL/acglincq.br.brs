00010 ! Replace S:\acsGL\ACGLINCQ
00020 ! -- INCOME STATEMENT FOR 8 1/2 * 11 PAPER WITHOUT PERCENTAGES
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnpglen,fncno,fnerror,fncch$,fnpedat$,fnprocess,fnUseDeptNo,fnps,fnpriorcd,fnfscode,fnactpd$,fnglfs,fntos,fnlbl,fntxt,fncmdkey,fnacs
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim fl1$*256,bp(13)
00080   dim sc1$(2)*20,cap$*128,udf$*256
00090   dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*14
00100   dim cnam$*40,b$*3,a$(8)*30,oldtrans$*16,g(8),accum(9,2),by(13)
00110 ! ______________________________________________________________________
00120   fntop(program$,cap$="Quarterly Income Statement")
00130   on fkey 5 goto L1720
00140   fncno(cno,cnam$)
00150   let udf$=env$('temp')&'\'
00160   actpd$=fnactpd$
00170   if fnglfs=5 then goto XIT !:
          ! sets fnps,fnpriorcd,fnfscode (primary/secondary,current year/Prior,period to print)
00180 ! ______________________________________________________________________
00190 ! pr NEWPAGE
00200   let redir=0: if file$(255)(1:4)<>"PRN:" then let redir=1: goto L210
00210 L210: if fnps=2 then let mp1=72 else let mp1=69
00220   fl1$="Name="&env$('Q')&"\GLmstr\ACGLFNSI.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\FNSIINDX.h"&str$(cno)&",Shr"
00230   if fnps=2 then fl1$="Name="&env$('Q')&"\GLmstr\ACGLFNSJ.H"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\FNSJINDX.h"&str$(cno)&",Shr"
00240   form c 9,skip 0
00250 L250: form pos mp1,pd 3,pos mp2,pd 3,pos 81,41*pd 6.2
00260   form c 7,skip 0
00270   let nametab=int(44-len(rtrm$(cnam$))/2)
00280   open #1: fl1$,internal,input,keyed 
00290   if fnprocess=1 or fnUseDeptNo=0 then goto L390
00300   fntos(sn$="ACglincq") !:
        let mylen=30: let mypos=mylen+3 : let right=1
00310   fnlbl(1,1,"Cost Center or Department #:",mylen,right)
00320   fntxt(1,mypos,3,0,right,"30",0,"Enter the cost center or department number if you wish to pr only one department, else leave blank for all.",0 ) !:
        let resp$(1)=""
00330   fnlbl(2,1,"(Blank for all Departments)",mylen,right)
00340   fncmdkey("&Next",1,1,0,"Prints the financial statement.")
00350   fncmdkey("&Cancel",5,0,1,"Returns to menu without posting.")
00360   fnacs(sn$,0,mat resp$,ckey)
00370   if ckey=5 then goto XIT
00380   costcntr=val(resp$(1))
00390 L390: let fnopenprn
00400   if fnps=2 then goto L430 ! secondary
00410   execute "Index "&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&" "&udf$&"fsindex.H"&str$(cno)&" 69 3 Replace DupKeys -N"
00420   goto L440
00430 L430: execute "Index "&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&" "&udf$&"fsindex.H"&str$(cno)&" 72 3 Replace DupKeys -N"
00440 L440: open #3: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&",KFName="&udf$&"fsindex.h"&str$(cno)&",Shr",internal,input,keyed 
00450   let report$="STATEMENT OF INCOME AND EXPENSES"
00460 ! GOSUB BLDPCT1 ! BUILD % BASED ON REF # IN PRIMARY FUND # IN G/L ACCOUNT
00470 L470: read #1,using L520: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof L1720
00480   if ltrm$(r$)="" or ltrm$(r$)="0" then goto L470
00490   if costcntr=0 then goto L520
00500   if fc=0 and te$="F" then goto L530 ! 5/8/89
00510   if costcntr><fc then goto L470
00520 L520: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
00530 L530: if te$="S" or te$="F" then goto L550
00540   if heading=0 and te$><"R" then gosub L1600
00550 L550: on pos ("RFHDTS",te$,1) goto L1130,L1170,L560,L610,L1030,L1130 none L470
00560 L560: pr #255,using L570: d$(1:40)
00570 L570: form pos sp,c 40,skip 1
00580   gosub L1300
00590   gosub L1240
00600   goto L470
00610 L610: if notrans=1 then goto L810
00620   if ir=val(r$) and val(r$)><0 then goto L740
00630   if ir>val(r$) then goto L740
00640 L640: ! read balances from general ledger
00650   if ir=0 then goto L660
00660 L660: read #3,using L250: ir,pcr,bb,cb,mat by,mat bp eof L800
00670   if fscode=0 or (fscode=actpd and priorcd=1) then goto L740
00680   if fscode<1 or fscode>13 then let fscode=1
00690   if fnpriorcd=1 then cb=by(fnfscode) else cb=bp(fnfscode)
00700   if fnpriorcd=2 then goto L730
00710   if fscode>1 then bb=by(fscode-1) else bb=0
00720   goto L740
00730 L730: if fscode>1 then bb=bp(fscode-1) else bb=0
00740 L740: if ir=val(r$) then let total=total+(cb-bb) else goto L780
00750   let total2=total2+cb
00760   let k$=cnvrt$("N 5",pcr)
00770   goto L640
00780 L780: if ir<val(r$) then goto L640
00790   if ir>val(r$) then goto L810
00800 L800: let notrans=1
00810 L810: for j=1 to 9
00820     if ac(j)=9 then goto L850 ! 10/14/87
00830     accum(j,1)=accum(j,1)+total
00840     accum(j,2)=accum(j,2)+total2
00850 L850: next j
00860   if rs=1 then let total=-total else goto L880
00870   let total2=-total2
00880 L880: if ds=1 then let dollar$="$" else let dollar$=" "
00890   if total><0 or total2><0 then goto L910
00900   if ls+ul+ds+ic>0 then goto L910 else goto L470
00910 L910: sp2=49-sp-1
00915   if ul=1 then pr #255,using L931: d$(1:sp2),dollar$,"{\UL ",total,"}",dollar$,"{\UL ",total2,"}" pageoflow L1450 : goto L930
00920   pr #255,using L930: d$(1:sp2),dollar$,total,dollar$,total2 pageoflow L1450
00930 L930: form pos sp,c sp2,pos 49,c 1,pic(--,---,---.##),pos 67,c 1,pic(--,---,---.##),skip redir
00931 L931: form pos sp,c sp2,pos 49,c 1,c 5,pic(--,---,---.##),c 1,pos 73,c 1,c 5,pic(--,---,---.##),c 1,skip redir
00940   if pc0=1 then gosub BLDPCT2
00950   if pc3>0 or pc4>0 then pr #255,using L960: pc3,pc4
00960 L960: form pos 63,n 4,pos 82,n 4,skip redir
00970   let total=0
00980   let total2=0
00990   gosub L1240
00995   if ul=1 then goto L1010
01000   gosub L1470
01010 L1010: gosub L1300
01020   goto L470
01030 L1030: if ap=0 then ap=1
01040   if rs=1 then accum1=-accum(ap,1) else accum1=accum(ap,1)
01050   if rs=1 then accum2=-accum(ap,2) else accum2=accum(ap,2)
01060   if ds=1 then let dollar$="$" else let dollar$=" "
01070   sp2=49-sp-1
01075   if ul=1 then pr #255,using L931: d$(1:sp2),dollar$,"{\UL ",accum1,"}",dollar$,"{\UL ",accum2,"}" pageoflow L1450 : goto L1090
01080   pr #255,using L930: d$(1:sp2),dollar$,accum1,dollar$,accum2 pageoflow L1450
01090 L1090: gosub L1240
01095   if ul=1 then goto L1110
01100   gosub L1470
01110 L1110: gosub L1300
01120   goto L470
01130 L1130: if te$="R" then let report$=d$
01140   if te$="S" then secondr$=d$
01150   gosub L1300
01160   goto L470
01170 L1170: if foot1=1 then goto L1220
01180   let tabnote=sp
01190   let foot1=1
01200   let foot$=d$
01210   goto L470
01220 L1220: let foot$=rtrm$(foot$)&d$
01230   goto L470
01240 L1240: for j=1 to 9
01250     if ac(j)=0 or ac(j)=9 then goto L1280 ! 10/14/87
01260     accum(j,1)=0
01270     accum(j,2)=0
01280 L1280: next j
01290   return 
01300 L1300: if ls=0 then goto L1440
01310   if ls=99 then goto L1350
01320   pr #255,using L1330: " "
01330 L1330: form pos 1,c 1,skip ls
01340   goto L1440
01350 L1350: let fnpglen(pglen)
01360 ! If PGLEN<>42 Then Let PGLEN=58
01370   sk=pglen-krec(255): fl=len(rtrm$(foot$))
01380 ! If PGLEN=42 Then sK=SK+1
01390   pr #255,using L1400: rtrm$(foot$),"Page "&str$(pt1)
01400 L1400: form skip sk,pos tabnote,c fl,pos 75,c 8,skip 1
01410   if eofcode=1 then goto L1440
01420   pr #255: newpage
01430   gosub L1600
01440 L1440: return 
01450 L1450: gosub L1350
01460   continue 
01470 L1470: if ul=0 then goto L1560
01480   if ul=1 then goto L1530
01490   let underlin$="=============="
01500   pr #255,using L1510: underlin$,underlin$
01510 L1510: form pos 49,c 14,pos 67,c 14,skip redir
01520   goto L1560
01530 L1530: let underlin$="______________"
01540   pr #255,using L1550: underlin$,underlin$
01550 L1550: form skip redir,pos 49,c 14,pos 67,c 14,skip redir
01560 L1560: if redir=0 then pr #255,using L1570: " "
01570 L1570: form skip 1,c 1,skip 0
01580   return 
01590 ! ______________________________________________________________________
01600 L1600: let heading=1
01610   if pt1=0 then let pt1=1 else let pt1=pt1+1
01620   pr #255: "\qc  {\f181 \fs24 \b "&env$('cnam')&"}"
01630   pr #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
01640   if trim$(secondr$)<>"" then pr #255: "\qc  {\f181 \fs18 \b "&trim$(secondr$)&"}"
01650   pr #255: "\qc  {\f181 \fs16 \b For the quarter ended "&rtrm$(fnpedat$)&"}"
01660   pr #255: "\ql "
01670   pr #255: 
01680   pr #255,using L1690: lpad$(rtrm$(fncch$),20),"Year To DATE"
01690 L1690: form pos 43,c 20,pos 69,c 12,skip 2
01700   return 
01710 ! ______________________________________________________________________
01720 L1720: eofcode=1
01730   gosub L1350
01740   fncloseprn
01742   fnfscode(actpd)
01750   fnpriorcd(1)
01760   goto XIT
01770 ! ______________________________________________________________________
01780 BLDPCT1: open #10: "Name="&env$('temp')&"\Work."&session$&",KFName="&env$('Temp')&"\Addr."&session$&",Replace,RecL=17,KPS=1,KLN=5",internal,outin,keyed 
01790   for j=1 to lrec(3)
01800     read #3,using L1810,rec=j: pc1,bb,cb norec L1900
01810 L1810: form pos mp1,pd 3,pos 81,2*pd 6.2
01820     let k$=cnvrt$("N 5",pc1)
01830     read #10,using L1840,key=k$: pc1,pc2,yt2 nokey L1890
01840 L1840: form pos 1,g 5,2*pd 6.2
01850     let pc2=pc2+cb-bb
01860     let yt2=yt2+cb
01870     rewrite #10,using L1840: pc1,pc2,yt2
01880     goto L1900
01890 L1890: write #10,using L1840: pc1,cb-bb,cb
01900 L1900: next j
01910   let pc0=1
01920   return 
01930 ! ______________________________________________________________________
01940 BLDPCT2: ! 
01950   let pc3=pc4=0
01960   if val(k$)=0 then goto L2040
01970   read #10,using L1840,key=k$: pc1,pc2,yt2 nokey L2040
01980   if total=0 then goto L2010
01990   let pc3=round(((total-pc2)/total)*100,0)
02000   if pc3<-999 or pc3>9999 then let pc3=0
02010 L2010: if total2=0 then goto L2040
02020   let pc4=round(((total2-yt2)/total2)*100,0)
02030   if pc4<-999 or pc4>9999 then let pc4=0
02040 L2040: return 
02050 ! ______________________________________________________________________
02060 XIT: let fnxit
02070 ! ______________________________________________________________________
02080 ! <updateable region: ertn>
02090 ERTN: let fnerror(program$,err,line,act$,"xit")
02100   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
02110   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
02120   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
02130 ERTN_EXEC_ACT: execute act$ : goto ERTN
02140 ! /region
02150 ! ______________________________________________________________________
