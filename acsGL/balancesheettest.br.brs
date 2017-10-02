00010 ! Replace S:\acsGL\BalanceSheetTest
00020 ! Balance Sheet !:
        ! Standard 8.5x11
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit,fnopenprn,fncloseprn,fncno,fnerror,fnprocess,fnpedat$,fnpriorcd,fnps,fnfscode,fnUseDeptNo,fnglfs,fnpglen,fntos,fnlbl,fntxt,fncmdkey,fnacs
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim fl1$*256,cap$*128
00080   dim cnam$*40,b$*3,a$(8)*30,oldtrans$*16,g(8),d(2),by(13),bp(13)
00090   dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*14
00100 ! ______________________________________________________________________
00110   let fntop(program$,cap$="Balance Sheet")
00120   let fncno(cno,cnam$)
00140   if fnglfs=5 then goto XIT !:
          ! sets fnps,fnpriorcd,fnfscode (primary/secondary,current year/Prior,period to print)
00150   if fnps=2 then let mp1=66 !:
          let fl1$="Name="&env$('Q')&"\GLmstr\acglFnSC.h"&str$(cno) !:
          let fl1$=fl1$&",KFName="&env$('Q')&"\GLmstr\fnSCIndx.h"&str$(cno)&",Shr" else !:
          let mp1=63 !:
          let fl1$="Name="&env$('Q')&"\GLmstr\acglFnSB.h"&str$(cno) !:
          let fl1$=fl1$&",KFName="&env$('Q')&"\GLmstr\FNSBIndx.h"&str$(cno)&",Shr"
00160   open #1: fl1$,internal,input,keyed 
00170   if fnprocess=1 or fnUseDeptNo=0 then goto L280 else goto L190
00180 ! ______________________________________________________________________
00190 L190: let fntos(sn$="GLInput") !:
        let mylen=30: let mypos=mylen+3 : let right=1
00200   let fnlbl(1,1,"Cost Center or Department #:",mylen,right)
00210   let fntxt(1,mypos,3,0,right,"30",0,"Enter the cost center or department number if you wish to pr only one department, else leave blank for all.",0 ) !:
        let resp$(1)=""
00220   let fnlbl(2,1,"(Blank for all Departments)",mylen,right)
00230   let fncmdkey("&Next",1,1,0,"Prints the financial statement.")
00240   let fncmdkey("&Cancel",5,0,1,"Returns to menu without posting.")
00250   let fnacs(sn$,0,mat resp$,ckey)
00260   if ckey=5 then goto XIT
00270   costcntr=val(resp$(1))
00280 L280: if fnps=2 then goto L310 ! secondary
00290   execute "Index "&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&' '&env$('Q')&"\GLmstr\fsindex.H"&str$(cno)&" 63 3 Replace DupKeys -N"
00300   goto L320
00310 L310: execute "Index "&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&' '&env$('Q')&"\GLmstr\fsindex.H"&str$(cno)&" 66 3 Replace DupKeys -N"
00320 L320: open #3: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\fsindex.h"&str$(cno)&",Shr",internal,input,keyed 
00330   let fnopenprn
00340   if file$(255)(1:4)<>"PRN:" then let redir=1 else let redir=0
00350   let report$="Balance Sheet"
00360 READ_TOP: ! 
00370   read #1,using L380: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof DONE
00380 L380: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
00390   if ltrm$(r$)="" or ltrm$(r$)="0" then goto READ_TOP
00400   if costcntr=0 then goto L420
00410   if costcntr><fc then goto READ_TOP
00420 L420: if te$="S" or te$="F" then goto L440
00430   if heading=0 and te$><"R" then gosub HEADER
00440 L440: on pos ("RFHDTSPE",te$,1) goto L970,L1010,L460,L520,L840,L970,L840,L520 none READ_TOP
00450 ! ______________________________________________________________________
00460 L460: pr #255,using L470: d$
00470 L470: form pos sp,c 50,skip 1
00480   gosub FOOTER
00490   gosub SET_ACCUM
00500   goto READ_TOP
00510 ! ______________________________________________________________________
00520 L520: if notrans=1 then goto L660
00530   if br>=val(r$) and val(r$)><0 then goto L610
00540 L540: ! read general ledger master file for amounts
00550   form pd 3
00560 L560: read #3,using 'Form POS MP1,PD 3,POS 87,27*PD 6.2': br,cb,mat by,mat bp eof L650
00565   cb=1
00570   if br=0 then goto L560
00580   if fnfscode=0 then goto L610
00590   if fnfscode<1 or fnfscode>12 then let fnfscode(1)
00600 ! If FNPRIORCD=1 Then cB=BY(FNFSCODE) Else cB=BP(FNFSCODE)
00610 L610: if br=val(r$) then let total=total+cb else goto L630
00620   goto L540
00630 L630: if br<val(r$) then goto L540
00640   if br>val(r$) then goto L660
00650 L650: let notrans=1
00660 L660: if te$="E" then let total=-accum(ap)
00670   for j=1 to 9
00680     if ac(j)=9 then goto L690 else accum(j)=accum(j)+total
00690 L690: next j
00700   if rs=1 then let total=-total
00710   if ds=1 then let dollar$="$" else let dollar$=" "
00720   let dollar=24+14*bc ! If CP=1 Then Let DOLLAR=50+14*BC Else Let DOLLAR=24+14*BC
00730   if total><0 then goto L750
00740   if ls+ul+ds+ic>0 then goto L750 else goto READ_TOP
00750 L750: let sp2=dollar-sp-1
00760   pr #255,using L770: d$(1:sp2),dollar$,total pageoflow PGOF
00770 L770: form pos sp,c sp2,pos dollar,c 1,pic(---,---,---.##),skip redir
00780   let total=0
00790   gosub SET_ACCUM
00800   gosub UNDERLINE
00810   gosub FOOTER
00820   goto READ_TOP
00830 ! ______________________________________________________________________
00840 L840: if ap=0 then ap=1
00850   if rs=1 then accum1=-accum(ap) else accum1=accum(ap)
00860   if ds=1 then let dollar$="$" else let dollar$=" "
00870   let dollar=24+14*bc ! if  CP=1 Then Let DOLLAR=50+14*BC Else Let DOLLAR=24+14*BC
00880   let sp2=dollar-sp-1
00890   pr #255,using L770: d$(1:sp2),dollar$,accum1 pageoflow PGOF
00900   gosub SET_ACCUM
00910   gosub UNDERLINE
00920   gosub FOOTER
00930   if te$><"P" then goto L950
00940   for j=1 to 9 !:
          accum(j)=accum(j)-accum(ap) !:
        next j
00950 L950: goto READ_TOP
00960 ! ______________________________________________________________________
00970 L970: if te$="R" then let report$=d$
00980   if te$="S" then let secondr$=d$
00990   gosub FOOTER
01000   goto READ_TOP
01010 L1010: if foot1=1 then goto L1070
01020   let tabnote=sp
01030   let foot1=1
01040   let foot$=d$
01050   goto READ_TOP
01060 ! ______________________________________________________________________
01070 L1070: let foot$=rtrm$(foot$)&d$
01080   goto READ_TOP
01090 ! ______________________________________________________________________
01100 SET_ACCUM: ! 
01110   for j=1 to 9
01120     if ac(j)=0 or ac(j)=9 then goto L1130 else accum(j)=0
01130 L1130: next j
01140   return 
01150 ! ______________________________________________________________________
01160 FOOTER: ! 
01170   if ls=0 then goto EO_FOOTER
01180   if ls=99 then goto L1220
01190   pr #255,using L1200: " "
01200 L1200: form pos 1,c 1,skip ls
01210   goto EO_FOOTER
01220 L1220: let fnpglen(pglen)
01230 ! If PGLEN<>42 Then Let PGLEN=58
01240   let sk=pglen-krec(255): let fl=len(rtrm$(foot$))
01250 ! If PGLEN=42 Then Let SK+=1
01260   pr #255,using L1270: rtrm$(foot$)
01270 L1270: form skip sk,pos tabnote,c fl,skip 1
01280   if eofcode=1 then goto EO_FOOTER
01290   pr #255: newpage
01300   gosub HEADER
01310 EO_FOOTER: return 
01320 ! ______________________________________________________________________
01330 PGOF: ! 
01340   gosub L1220
01350   continue 
01360 ! ______________________________________________________________________
01370 UNDERLINE: ! 
01380   if ul=0 then goto L1480
01390   let underlin=25+14*bc ! if CP=1 Then Let UNDERLIN=51+14*BC Else Let UNDERLIN=25+14*BC
01400   if ul=1 then goto L1450
01410   let underlin$="=============="
01420   pr #255,using L1430: underlin$
01430 L1430: form skip 1,pos underlin,c 14,skip redir
01440   goto L1480
01450 L1450: let underlin$="______________"
01460   pr #255,using L1470: underlin$
01470 L1470: form pos underlin,c 14,skip redir
01480 L1480: if redir=0 then pr #255,using L1490: " "
01490 L1490: form skip 1,c 1,skip 0
01500   return 
01510 ! ______________________________________________________________________
01520 HEADER: ! 
01530   let heading=1
01540   pr #255: "\qc  {\f181 \fs24 \b "&env$('cnam')&"}"
01543   pr #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
01544   if trim$(secondr$)<>"" then pr #255: "\qc  {\f181 \fs18 \b "&trim$(secondr$)&"}"
01545   pr #255: "\qc  {\f181 \fs16 \b "&trim$(fnpedat$)&"}"
01546   pr #255: "\ql "
01590   return 
01600 ! ______________________________________________________________________
01610 DONE: ! 
01620   let eofcode=1
01630   gosub L1220
01640   if pors<>2 then let fncloseprn
01650   goto XIT
01660 ! ______________________________________________________________________
01670 ! <Updateable Region: ERTN>
01680 ERTN: let fnerror(program$,err,line,act$,"xit")
01690   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
01700   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01710   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01720 ERTN_EXEC_ACT: execute act$ : goto ERTN
01730 ! /region
01740 ! ______________________________________________________________________
01750 XIT: let fnxit
01760 ! ______________________________________________________________________
