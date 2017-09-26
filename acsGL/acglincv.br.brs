00010 ! Replace S:\acsGL\AcGlIncV
00020 ! COMPARATIVE INCOME STATEMENT WITH PERCENTAGES & VARIANCES
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnpglen,fnerror,fnprocess,fncno,fnUseDeptNo,fnpedat$,fnps,fnpriorcd,fnfscode,fnactpd$,fncch$,fnglfs,fnactpd,fntos,fnlbl,fntxt,fncmdkey,fnacs
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim fl1$*256,actpd$*6,cogl$(3)*12,pedat$*20,cch$*20
00080   dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*22
00090   dim cnam$*40,b$*3,a$(8)*30,oldtrans$*16,g(8),accum(9,6)
00100   dim bp(13),by(13),cap$*128,udf$*256
00110 ! ______________________________________________________________________
00120   let fntop(program$,cap$="Income Statement with Varience")
00130   on fkey 5 goto L2500
00140   let fncno(cno,cnam$)
00150   let udf$=env$('temp')&'\'
00155   let fnfscode
00156   let fnpriorcd
00160   if fnglfs=5 then goto XIT !:
          ! sets fnps,fnpriorcd,fnfscode (primary/secondary,current year/Prior,period to print)
00165   let fscode=fnfscode
00166   let fnpriorcd
00170   let cch$=fncch$
00180   let pedat$=fnpedat$
00190   let actpd$=fnactpd$
00200   let actpd=fnactpd
00210   let priorcd=fnpriorcd
00220 ! ______________________________________________________________________
00230   let fnopenprn !:
        if file$(255)(1:4)<>"PRN:" then let redir=1 else let redir=0
00240   let mp1=69
00250   if fnps=2 then let mp1=mp1+3
00260   if fnps=2 then let fl1$="Name="&env$('Q')&"\GLmstr\ACGLFNSJ.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\FNSJINDX.h"&str$(cno)&",Shr" else !:
          let fl1$="Name="&env$('Q')&"\GLmstr\ACGLFNSI.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\FNSIINDX.h"&str$(cno)&",Shr"
00270 L270: form pos mp1,pd 3,pos 81,41*pd 6.2
00280   if actpd>0 and actpd<14 then goto L330
00290   print fields "10,2,C 78": "THIS PROGRAM CANNOT PROCESS WITHOUT THE NUMBER OF THE ACCOUNTING PERIOD END"
00300   print fields "12,2,C 60,N": "USE OPTION 1 ON THE MENU TO ENTER THIS INFORMATION"
00310   input fields "23,2,C 1,E,N": pause$
00320   goto L2530
00330 L330: open #1: fl1$,internal,input,keyed 
00340   if fnprocess=1 or fnUseDeptNo=0 or percent=1 then goto L440
00350   let fntos(sn$="ACglincv") !:
        let mylen=30: let mypos=mylen+3 : let right=1
00360   let fnlbl(1,1,"Cost Center or Department #:",mylen,right)
00370   let fntxt(1,mypos,3,0,right,"30",0,"Enter the cost center or department number if you wish to print only one department, else leave blank for all.",0 ) !:
        let resp$(1)=""
00380   let fnlbl(2,1,"(Blank for all Departments)",mylen,right)
00390   let fncmdkey("&Next",1,1,0,"Prints the financial statement.")
00400   let fncmdkey("&Cancel",5,0,1,"Returns to menu without posting.")
00410   let fnacs(sn$,0,mat resp$,ckey)
00420   if ckey=5 then goto XIT
00430   let costcntr=val(resp$(1))
00440 L440: let report$="Statement of Income and Expenses"
00450   if fnps=2 then goto L480 ! secondary
00460   execute "Index "&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&" "&udf$&"fsindex.H"&str$(cno)&" 69 3 Replace DupKeys -N"
00470   goto L490
00480 L480: execute "Index "&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&" "&udf$&"fsindex.H"&str$(cno)&" 72 3 Replace DupKeys -N"
00490 L490: open #3: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&",KFName="&udf$&"fsindex.h"&str$(cno)&",Shr",internal,input,keyed 
00500   goto TOP_OF_LOOP
00510 ! ______________________________________________________________________
00520 TOP_OF_LOOP: ! 
00530   if ic><2 then goto L550
00540   let percent1=percent2=percent3=percent4=percent5=percent6=0 !:
        let percent=2
00550 L550: read #1,using L600: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof L2450
00560   if ltrm$(r$)="" or ltrm$(r$)="0" then goto TOP_OF_LOOP
00570 L570: if costcntr=0 then goto L600
00580   if fc=0 and te$="F" then goto L610 ! 5/08/1989
00590   if costcntr><fc then goto TOP_OF_LOOP
00600 L600: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
00610 L610: if te$="S" or te$="F" then goto L630
00620   if heading=0 and te$><"R" then gosub L2290
00630 L630: on pos ("RFHDTS",te$,1) goto L1880,L1910,L640,L700,L1510,L1880 none TOP_OF_LOOP
00640 L640: if percent=0 then goto L680
00650   print #255,using L660: d$(1:40)
00660 L660: form pos sp,c 40,skip 1
00670   gosub EOPAGE
00680 L680: gosub L1940
00690   goto TOP_OF_LOOP
00700 L700: if notrans=1 then goto L950
00710   if ir>=val(r$) and val(r$)><0 then goto L820
00720 L720: ! read amounts from gl master file
00730 L730: read #3,using L270: ir,bb,cb,mat by,mat bp eof L940
00740   if ir=0 then goto L730
00750   if fscode=0 or (fscode=actpd and fnpriorcd=1) then goto L820
00760   if fscode<1 or fscode>13 then let fscode=1 ! 6/8/88
00770   if priorcd=1 then let cb=by(fscode) else let cb=bp(fscode)
00780   if priorcd=2 then goto L810
00790   if fscode>1 then let bb=by(fscode-1) else let bb=0
00800   goto L820
00810 L810: if fscode>1 then let bb=bp(fscode-1) else let bb=0
00820 L820: if ir=val(r$) then let total=total+(cb-bb) else goto L920
00830   let total2=total2+cb
00840   if actpd>1 then let pripd=actpd-1 else goto L870
00850   let total3=total3+(bp(actpd)-bp(pripd))
00860   goto L880
00870 L870: let total3=total3+bp(actpd)
00880 L880: let total4=total4+bp(actpd)
00890   let total5=total-total3
00900   let total6=total2-total4
00910   goto L720
00920 L920: if ir<val(r$) then goto L720
00930   if ir>val(r$) then goto L950
00940 L940: let notrans=1
00950 L950: for j=1 to 9
00960     if ac(j)=9 then goto L1030 ! 10/14/87
00970     let accum(j,1)=accum(j,1)+total
00980     let accum(j,2)=accum(j,2)+total2
00990     let accum(j,3)=accum(j,3)+total3
01000     let accum(j,4)=accum(j,4)+total4
01010     let accum(j,5)=accum(j,1)-accum(j,3)
01020     let accum(j,6)=accum(j,2)-accum(j,4)
01030 L1030: next j
01040   if rs=1 then let total=-total else goto L1100
01050   let total2=-total2
01060   let total3=-total3
01070   let total4=-total4
01080   let total5=-total5
01090   let total6=-total6
01100 L1100: if ds=1 then let dollar$="$" else let dollar$=" "
01110   if ds=1 then let percent$="%" else let percent$=" "
01120   if total2><0 or total4><0 then goto L1150
01130   if total><0 or total3><0 then goto L1150
01140   if ls+ds+ul+ic>0 then goto L1150 else goto L550
01150 L1150: if percent=0 then goto L1380
01160   let sp2=31-sp-1
01170   if percent=2 and ic=1 then gosub L2560
01180   if percent1=0 then let pdpct=0 else let pdpct=total/percent1*100
01190   if percent2=0 then let ytdpct=0 else let ytdpct=total2/percent2*100
01200   if percent3=0 then let pppd=0 else let pppd=total3/percent3*100
01210   if percent4=0 then let ppyear=0 else let ppyear=total4/percent4*100
01220   if percent5=0 then let ppcvar=0 else let ppcvar=total5/percent5*100
01230   if percent6=0 then let ppyvar=0 else let ppyvar=total6/percent6*100
01240   if pdpct<-999.99 then let pdpct=-999.99
01250   if pdpct>999.99 then let pdpct=999.99
01260   if ytdpct<-999.99 then let ytdpct=-999.99
01270   if ytdpct>999.99 then let ytdpct=999.99
01280   if ppyear<-999.99 then let ppyear=-999.99
01290   if ppyear>999.99 then let ppyear=999.99
01300   if pppd<-999.99 then let pppd=-999.99
01310   if pppd>999.99 then let pppd=999.99
01320   if ppcvar<-999.99 then let ppcvar=999.99
01330   if ppyvar<-999.99 then let ppyvar=999.99
01340   if ul=1 then print #255,using L1361: d$(1:sp2),dollar$,"{\UL ",total,"}",pdpct,percent$,dollar$,"{\UL ",total2,"}",ytdpct,percent$,dollar$,"{\UL ",total3,"}",pppd,percent$,dollar$,"{\UL ",total4,"}",ppyear,percent$,dollar$,"{\UL ",total5,"}",ppcvar,percent$,dollar$,"{\UL ",total6,"}",ppyvar,percent$ pageoflow PGOF : goto L1360
01350   print #255,using L1360: d$(1:sp2),dollar$,total,pdpct,percent$,dollar$,total2,ytdpct,percent$,dollar$,total3,pppd,percent$,dollar$,total4,ppyear,percent$,dollar$,total5,ppcvar,percent$,dollar$,total6,ppyvar,percent$ pageoflow PGOF
01360 L1360: form pos sp,c sp2,pos 31,c 1,pic(--,---,---.##),pic(-----.##),c 2,c 1,pic(--,---,---.##),pic(-----.##),c 2,c 1,pic(--,---,---.##),pic(-----.##),c 2,c 1,pic(--,---,---.##),pic(-----.##),c 2,c 1,pic(--,---,---.##),pic(-----.##),c 2,c 1,pic(--,---,---.##),pic(-----.##),c 1,skip redir
01361 L1361: form pos sp,c sp2,pos 31,c 1,c 5,pic(--,---,---.##),c 1,pic(-----.##),c 2,c 1,c 5,pic(--,---,---.##),c 1,pic(-----.##),c 2,c 1,c 5,pic(--,---,---.##),c 1,pic(-----.##),c 2,c 1,c 5,pic(--,---,---.##),c 1,pic(-----.##),c 2,c 1,c 5,pic(--,---,---.##),c 1,pic(-----.##),c 2,c 1,c 5,pic(--,---,---.##) ,c 1,pic(-----.##),c 1
01370 L1370: form pos sp,c sp2,pos 31,c 1,c 5,pic(--,---,---.##),c 1,pic(-----.##),c 2,c 1,c 5,pic(--,---,---.##),c 1,pic(-----.##),c 2,c 1,c 5,pic(--,---,---.##),c 1,pic(-----.##),c 2,c 1,c 5,pic(--,---,---.##),c 1,pic(-----.##),c 2,c 1,c 5,pic(--,---,---.##),c 1,pic(-----.##),c 2,c 1,c 5,pic(--,---,---.##),c 1,pic(-----.##),skip redir
01380 L1380: if percent=0 and ic=1 then goto L2550
01390   let total=0
01400   let total2=0
01410   let total3=0
01420   let total4=0
01430   let total5=0
01440   let total6=0
01450   gosub L1940
01460   if ul=1 then goto L1480
01470   gosub L2160
01480 L1480: gosub EOPAGE
01490   goto TOP_OF_LOOP
01500 ! ______________________________________________________________________
01510 L1510: if ap=0 then let ap=1
01520   if rs=1 then let accum1=-accum(ap,1) else let accum1=accum(ap,1)
01530   if rs=1 then let accum2=-accum(ap,2) else let accum2=accum(ap,2)
01540   if rs=1 then let accum3=-accum(ap,3) else let accum3=accum(ap,3)
01550   if rs=1 then let accum4=-accum(ap,4) else let accum4=accum(ap,4)
01560   if rs=1 then let accum5=-accum(ap,5) else let accum5=accum(ap,5)
01570   if rs=1 then let accum6=-accum(ap,6) else let accum6=accum(ap,6)
01580   if ds=1 then let dollar$="$" else let dollar$=" "
01590   if ds=1 then let percent$="%" else let percent$=" "
01600   if percent=2 and ic=1 then gosub L2640
01610   if percent=0 then goto L1830
01620   if percent1=0 then let pdpct=0 else let pdpct=accum1/percent1*100
01630   if percent2=0 then let ytdpct=0 else let ytdpct=accum2/percent2*100
01640   if percent3=0 then let pppd=0 else let pppd=accum3/percent3*100
01650   if percent4=0 then let ppyear=0 else let ppyear=accum4/percent4*100
01660   if percent5=0 then let ppcvar=0 else let ppcvar=accum5/percent5*100
01670   if percent6=0 then let ppyvar=0 else let ppyvar=accum6/percent6*100
01680   if pdpct<-999.99 then let pdpct=-999.99
01690   if pdpct>999.99 then let pdpct=999.99
01700   if ytdpct<-999.99 then let ytdpct=-999.99
01710   if ytdpct>999.99 then let ytdpct=999.99
01720   if pppd<-999.99 then let pppd=-999.99
01730   if pppd>999.99 then let pppd=999.99
01740   if ppyear<-999.99 then let ppyear=-999.99
01750   if ppyear>999.99 then let ppyear=999.99
01760   if ppyvar<-999.99 then let ppyvar=-999.99
01770   if ppyvar>999.99 then let ppyvar=999.99
01780   if ppcvar<-999.99 then let ppcvar=-999.99
01790   if ppcvar>999.99 then let ppcvar=999.99
01800   let sp2=31-sp-1
01810   if ul=1 then print #255,using L1370: d$(1:sp2),dollar$,"{\UL ",accum1,"}",pdpct,percent$,dollar$,"{\UL ",accum2,"}",ytdpct,percent$,dollar$,"{\UL ",accum3,"}",pppd,percent$,dollar$,"{\UL ",accum4,"}",ppyear,percent$,dollar$,"{\UL ",accum5,"}",ppcvar,percent$,dollar$,"{\UL ",accum6,"}",ppyvar,percent$ pageoflow PGOF : goto L1830
01820   print #255,using L1360: d$(1:sp2),dollar$,accum1,pdpct,percent$,dollar$,accum2,ytdpct,percent$,dollar$,accum3,pppd,percent$,dollar$,accum4,ppyear,percent$,dollar$,accum5,ppcvar,percent$,dollar$,accum6,ppyvar,percent$ pageoflow PGOF
01830 L1830: if percent=0 and ic=1 then !:
          goto L2630
01840   gosub L1940
01845   if ul=1 then goto L1860
01850   gosub L2160
01860 L1860: gosub EOPAGE : goto TOP_OF_LOOP
01870 ! ______________________________________________________________________
01880 L1880: if te$="R" then let report$=d$ else !:
          if te$="S" then let secondr$=d$
01890   gosub EOPAGE : goto TOP_OF_LOOP
01900 ! ______________________________________________________________________
01910 L1910: if foot1=1 then let foot$=rtrm$(foot$)&d$ else !:
          let tabnote=sp : let foot1=1 : let foot$=d$
01920   goto TOP_OF_LOOP
01930 ! ______________________________________________________________________
01940 L1940: for j=1 to 9
01950     if ac(j)<>0 and ac(j)<>9 then !:
            let accum(j,1)=accum(j,2)=accum(j,3)=accum(j,4)=0
01960   next j
01970   return 
01980 ! ______________________________________________________________________
01990 EOPAGE: ! i think that's what this is... could be wrong
02000   if percent=0 then goto L2130
02010   if ls=0 then goto L2130
02020   if ls=99 then goto FOOTER
02030   print #255,using L2040: " "
02040 L2040: form pos 1,c 1,skip ls
02050   goto L2130
02060 FOOTER: let sk=58-krec(255): let fl=len(rtrm$(foot$))
02070 ! If PGLEN=42 Then Let SK=SK+1
02080   print #255,using L2090: rtrm$(foot$),"Page "&str$(pt1)
02090 L2090: form skip sk,pos tabnote,c fl,pos 165,c 8,skip 1
02100   if eofcode=1 then goto L2130
02110   print #255: newpage
02120   gosub L2290
02130 L2130: return 
02140 ! ______________________________________________________________________
02150 PGOF: gosub FOOTER: continue 
02160 L2160: if percent=0 then goto L2280
02170   if ul=0 then goto L2260
02180   if ul=1 then goto L2230
02190   let underlin$="============== ======="
02200   print #255,using L2210: underlin$,underlin$,underlin$,underlin$,underlin$,underlin$
02210 L2210: form pos 31,c 22,pos 55,c 22,pos 79,c 22,pos 103,c 22,pos 127,c 22,pos 151,c 22,skip redir
02220   goto L2260
02230 L2230: let underlin$="______________ _______"
02240   print #255,using L2250: underlin$,underlin$,underlin$,underlin$,underlin$,underlin$
02250 L2250: form skip redir,pos 31,c 22,pos 55,c 22,pos 79,c 22,pos 103,c 22,pos 127,c 22,pos 151,c 22,skip redir
02260 L2260: if redir=0 then print #255,using L2270: " "
02270 L2270: form skip 1,c 1,skip 0
02280 L2280: return 
02290 L2290: let heading=1
02300   let pt1+=1
02310   print #255: "\qc  {\f181 \fs24 \b "&env$('cnam')&"}"
02320   print #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
02330   if trim$(secondr$)<>"" then print #255: "\qc  {\f181 \fs18 \b "&trim$(secondr$)&"}"
02340   print #255: "\qc  {\f181 \fs16 \b For the "&rtrm$(actpd$)&" month period ended "&rtrm$(fnpedat$)&"}"
02350   print #255: "\ql "
02360   print #255: 
02370   print #255: tab(48);"CURRENT YEAR";tab(97);"PRIOR YEAR";tab(147);"VARIANCE"
02380   print #255,using L2390: cch$,"     YEAR TO DATE",cch$,"     YEAR TO DATE",cch$,"     YEAR TO DATE"
02390 L2390: form pos 32,c 20,pos 55,c 22,pos 79,c 22,pos 103,c 22,pos 127,c 22,pos 151,c 22,skip redir
02400 L2400: form pos 31,c 22,pos 55,c 22,pos 78,c 22,pos 103,c 22,pos 127,c 22,pos 151,c 22,skip redir
02410   print #255,using L2400: "______________________","______________________","______________________","______________________","______________________","_____________________"
02420   print #255: 
02430   return 
02440 ! ______________________________________________________________________
02450 L2450: if percent><0 then goto L2500
02460   let percent=1
02470   let percent1=percent2=percent3=percent4=percent5=percent6=0
02480   goto L570
02490 ! ______________________________________________________________________
02500 L2500: let eofcode=1
02510   gosub FOOTER
02520   let fncloseprn
02525   let fnfscode(actpd)
02526   let fnpriorcd(1)
02530 L2530: goto XIT
02540 ! ______________________________________________________________________
02550 L2550: let percent=1
02560 L2560: let percent1=total
02570   let percent2=total2
02580   let percent3=total3
02590   let percent4=total4
02600   let percent5=total5
02610   let percent6=total6
02620   goto L2700
02630 L2630: let percent=1
02640 L2640: let percent1=accum1
02650   let percent2=accum2
02660   let percent3=accum3
02670   let percent4=accum4
02680   let percent5=accum5
02690   let percent6=accum6
02700 L2700: if percent=2 then return 
02710   close #1: ioerr L2720
02720 L2720: close #3: ioerr L2730
02730 L2730: let total=0
02740   let total2=0
02750   let total3=0
02760   let total4=0
02770   let total5=0
02780   let total6=0
02790   mat accum=(0)
02800   let foot1=0
02810   let foot$=" "
02820   let ir=0
02830   goto L330
02840 ! ______________________________________________________________________
02850 XIT: let fnxit
02860 ! ______________________________________________________________________
02870 ! <updateable region: ertn>
02880 ERTN: let fnerror(program$,err,line,act$,"xit")
02890   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
02900   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
02910   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
02920 ERTN_EXEC_ACT: execute act$ : goto ERTN
02930 ! /region
