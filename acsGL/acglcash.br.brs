00010 ! Replace S:\acsGL\acglCash
00020 ! Cash Flow Statement
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnxit,fntop, fnopenprn,fncloseprn,fnpglen,fncno,fnerror,fnprocess,fncch$,fnactpd$,fnpedat$,fnactpd,fnps,fnfscode,fnUseDeptNo,fnpriorcd,fnglfs,fntos,fnlbl,fntxt,fncmdkey,fnacs,fnactpd$
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim fl1$*256,actpd$*6,pedat$*20,cch$*20 ,in3$(4)
00080   dim pedat$*20,actpd$*6,bm(13),bp(13),by(13)
00090   dim cnam$*40,b$*3,a$(8)*30,oldtrans$*16,g(8),accum(9,7),cap$*128
00100   dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*14
00110 ! ______________________________________________________________________
00120   fntop(program$,cap$="Cash Flow Statement")
00130   report$=cap$
00140   fncno(cno,cnam$)
00144   actpd$=fnactpd$
00145   pedat=val(actpd$)
00150   open #20: "Name="&env$('Q')&"\GLmstr\Company.h"&str$(cno)&",Shr",internal,input,relative  !:
        read #20,using 'Form Pos 384,n 2',rec=1: nap : close #20: 
00160   let fscode=fnfscode
00165   fnfscode
00166   fnpriorcd
00170   if fnglfs=5 then goto XIT !:
          ! sets fnps,fnpriorcd,fnfscode (primary/secondary,current year/Prior,period to print)
00175   let fscode=fnfscode
00176   priorcd=fnpriorcd
00180 ! ______________________________________________________________________
00190   pors=1
00200   in3$(1)="8,25,N 12.2,UT,N" : in3$(2)="8,45,N 12.2,UT,N"
00210   mp1=75
00220   if fnps=2 then mp1=mp1+3
00230   fl1$="Name="&env$('Q')&"\GLmstr\ACGLFNSF.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\FNSFIndx.h"&str$(cno)&",Shr"
00240   if fnps=2 then fl1$="Name="&env$('Q')&"\GLmstr\ACGLFNSG.h"&str$(cno)&"," !:
          fl1$=fl1$&"KFName="&env$('Q')&"\GLmstr\FNSGIndx.h"&str$(cno)&",Shr"
00250   open #1: fl1$,internal,input,keyed 
00260   if fnprocess=1 or fnUseDeptNo=0 then goto L360
00270   fntos(sn$="ACglcash") !:
        mylen=30: mypos=mylen+3 : right=1
00280   fnlbl(1,1,"Cost Center or Department #:",mylen,right)
00290   fntxt(1,mypos,3,0,right,"30",0,"Enter the cost center or department number if you wish to pr only one department, else leave blank for all.",0 ) !:
        resp$(1)=""
00300   fnlbl(2,1,"(Blank for all Departments)",mylen,right)
00310   fncmdkey("&Next",1,1,0,"Prints the financial statement.")
00320   fncmdkey("&Cancel",5,0,1,"Returns to menu without posting.")
00330   fnacs(sn$,0,mat resp$,ckey)
00340   if ckey=5 then goto XIT
00350   costcntr=val(resp$(1))
00360 L360: if fnps=2 then goto L390 ! secondary
00365   close #3: ioerr L370
00370 L370: execute "Index "&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&" "&env$('Temp')&"\fsindex.H"&str$(cno)&" 75 3 Replace DupKeys -N"
00380   goto L400
00390 L390: execute "Index "&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&" "&env$('Temp')&"\fsindex.H"&str$(cno)&" 78 3 Replace DupKeys -N"
00400 L400: open #3: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&",KFName="&env$('Temp')&"\fsindex.h"&str$(cno)&",Shr",internal,input,keyed 
00410   fnopenprn !:
        if file$(255)(1:4)<>"PRN:" then redir=1 else redir=0
00420 L420: read #1,using L460: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof L1820
00430   if ltrm$(r$)="" or ltrm$(r$)="0" then goto L420
00440   if costcntr=0 then goto L460
00450   if costcntr><fc then goto L420
00460 L460: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
00470   if te$="S" or te$="F" then goto L490
00480   if heading=0 and te$><"R" then gosub HDR
00490 L490: on pos ("RFHDTSBC",te$,1) goto L1190,L1240,L500,L560,L1080,L1190,L560,L1880 none L420
00500 L500: pr #255,using L510: d$(1:40)
00510 L510: form pos sp,c 40
00520   gosub L1390
00530   gosub L1320
00540   goto L420
00550 ! ______________________________________________________________________
00560 L560: if te$="B" and ap>0 then accum1=-accum1: accum2=-accum2: goto L1080 ! ENDING BANK BALANCE
00570   if notrans=1 then goto L870
00580   if ir>=val(r$) and val(r$)><0 then goto L700
00590 L590: ! read amounts from gl master
00600 L600: read #3,using L690: ir,bb,cb,mat by,mat bp,mat bm eof L860
00610   if ir=0 then goto L600
00620   if fscode=0 or (fscode=pedat and priorcd=1) then goto L690
00630   if fscode<1 or fscode>13 then let fscode=1
00640   if fnpriorcd=1 then cb=by(fscode) else cb=bp(fscode)
00650   if fnpriorcd=2 then goto L680
00660   if fscode>1 then bb=by(fscode-1) else bb=0
00670   goto L690
00680 L680: if fscode>1 then bb=bp(fscode-1) else bb=0
00690 L690: form pos mp1,pd 3,pos 81,41*pd 6.2
00700 L700: if ir=val(r$) then total+=(cb-bb) else goto L840
00710   if te$="B" then !:
          total-=(cb-bb): total-=bb: total2-=bp(nap) : goto L730
00720   total2+=cb
00730 L730: for z=1 to 13 : annualb+=bm(z) : next z
00740   if fscode=0 then monthb=monthb+bm(fnactpd) else !:
          monthb=monthb+bm(fscode)
00750   if fscode=0 then goto L760 else goto L800
00760 L760: for j=1 to fnactpd
00770     let ytdb=ytdb+bm(j)
00780   next j
00790   goto L590
00800 L800: for j=1 to fscode
00810     let ytdb=ytdb+bm(j)
00820   next j
00830   goto L590
00840 L840: if ir<val(r$) then goto L590
00850   if ir>val(r$) then goto L870
00860 L860: notrans=1
00870 L870: for j=1 to 9
00880     if ac(j)<>9 then !:
            accum(j,1)+=total : accum(j,2)+=total2
00890   next j
00900   if rs=1 then total=-total else goto L930
00910   total2=-total2
00920   let ytdb=-ytdb
00930 L930: if ds=1 then dollar$="$" else dollar$=" "
00940   if total><0 or total2><0 then goto L970
00950   if total<>0 then goto L970
00960   if ls+ds+ul+ic>0 then goto L970 else goto L420
00970 L970: sp2=30-sp-1
00980   if te$="B" then total=-total: total2=-total2 !:
          ! Reverse sign on beginning bank balance
00985   if ul=1 then pr #255,using L1001: d$(1:sp2),dollar$,"{\ul ",total,"}",dollar$,"{\ul ",total2,"}" pageoflow L1550 : goto L1000
00990   pr #255,using L1000: d$(1:sp2),dollar$,total,dollar$,total2 pageoflow L1550
01000 L1000: form pos sp,c sp2,pos 52,c 1,pic(--,---,---.##),x 1,c 1,pic(--,---,---.##),skip redir
01001 L1001: form pos sp,c sp2,pos 52,c 1,c 5,pic(--,---,---.##),c 1,x 1,c 1,c 5,pic(--,---,---.##),c 1,skip redir
01010   total=0
01020   total2=0
01030   gosub L1320
01035   if ul=1 then goto L1050
01040   gosub L1560
01050 L1050: gosub L1390
01060   goto L420
01070 ! ______________________________________________________________________
01080 L1080: if ap=0 then ap=1
01090   if rs=1 then accum1=-accum(ap,1) else accum1=accum(ap,1)
01100   if rs=1 then accum2=-accum(ap,2) else accum2=accum(ap,2)
01110   if ds=1 then dollar$="$" else dollar$=" "
01120   sp2=30-sp-1
01121   if te$="B" then accum2=0
01125   if ul=1 then pr #255,using L1001: d$(1:sp2),dollar$,"{\ul ",accum1,"}",dollar$,"{\ul ",accum2,"}" pageoflow L1550 : goto L1140
01130   pr #255,using L1000: d$(1:sp2),dollar$,accum1,dollar$,accum2 pageoflow L1550
01140 L1140: gosub L1320
01145   if ul=1 then goto L1160
01150   gosub L1560
01160 L1160: gosub L1390
01170   goto L420
01180 ! ______________________________________________________________________
01190 L1190: if te$="R" then report$=d$
01200   if te$="S" then secondr$=d$
01210   gosub L1390
01220   goto L420
01230 ! ______________________________________________________________________
01240 L1240: if foot1=1 then goto L1290
01250   tabnote=sp
01260   let foot1=1
01270   let foot$=d$
01280   goto L420
01290 L1290: let foot$=rtrm$(foot$)&d$
01300   goto L420
01310 ! ______________________________________________________________________
01320 L1320: for j=1 to 9
01330     if ac(j)=0 or ac(j)=9 then goto L1360
01340     accum(j,1)=0
01350     accum(j,2)=0
01360 L1360: next j
01370   return 
01380 ! ______________________________________________________________________
01390 L1390: if ls=0 then goto L1530
01400   if ls=99 then goto L1440
01410   pr #255,using L1420: " "
01420 L1420: form pos 1,c 1,skip ls
01430   goto L1530
01440 L1440: fnpglen(pglen)
01450 ! If PGLEN<>42 Then pGLEN=58
01460   sk=pglen-krec(255): fl=len(rtrm$(foot$))
01470 ! If PGLEN=42 Then sK=SK+1
01480   pr #255,using L1490: rtrm$(foot$),"Page "&str$(pt1)
01490 L1490: form skip sk,pos tabnote,c fl,pos 75,c 8,skip 1
01500   if eofcode=1 then goto L1530
01510   pr #255: newpage
01520   gosub HDR
01530 L1530: return 
01540 ! ______________________________________________________________________
01550 L1550: gosub L1440: continue 
01560 L1560: if ul=0 then goto L1650
01570   if ul=1 then goto L1620
01580   let underlin$="=============="
01600   goto L1630
01610   goto L1650
01620 L1620: let underlin$="______________"
01630 L1630: pr #255,using L1640: underlin$,underlin$
01640 L1640: form skip redir,pos 52,2*c 15,skip redir
01650 L1650: if redir=0 then pr #255,using L1660: " "
01660 L1660: form c 1,skip 1
01670   return 
01680 ! ______________________________________________________________________
01690 HDR: heading=1
01700   pt1+=1
01710   pr #255: "\qc  {\f181 \fs24 \b "&env$('cnam')&"}"
01720   pr #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
01730   if trim$(secondr$)<>"" then pr #255: "\qc  {\f181 \fs18 \b "&trim$(secondr$)&"}"
01740   pr #255: "\qc  {\f181 \fs16 \b For the "&rtrm$(fnactpd$)&" month period ended "&rtrm$(fnpedat$)&"}"
01750   pr #255: "\ql "
01760   pr #255: ""
01770   pr #255: tab(50);lpad$(trim$(fncch$),15);tab(74);"Year To"
01780   pr #255: tab(56);"       ";tab(75);"Date"
01790   pr #255: ""
01800   return 
01810 ! ______________________________________________________________________
01820 L1820: eofcode=1
01830   gosub L1440
01840   fnfscode(pedat)
01841   fnpriorcd(1)
01850   fncloseprn
01860   goto XIT
01870 ! ______________________________________________________________________
01880 L1880: fntos(sn$="ACglcash2") !:
        mylen=25: mypos=mylen+3 : right=1
01890   fnlbl(1,1,"Total Current Month:",mylen,right)
01900   fntxt(1,mypos,12,0,right,"10",0,"Enter the total for the current month.",0 ) !:
        resp$(1)=str$(total)
01910   fnlbl(2,1,"Total Year to Date:",mylen,right)
01920   fntxt(2,mypos,12,0,right,"10",0,"Enter the total for the year.",0 ) !:
        resp$(2)=str$(total2)
01930   fncmdkey("&Next",1,1,0,"Accept the answer.")
01940   fncmdkey("&Cancel",5,0,1,"Returns to menu without posting.")
01950   fnacs(sn$,0,mat resp$,ckey)
01960   if ckey=5 then goto XIT
01970   total=val(resp$(1))
01980   total2=val(resp$(2))
01990   if ckey=5 then goto XIT
02000 XIT: fnxit
02010 ! ______________________________________________________________________
02020 ! <Updateable Region: ERTN>
02030 ERTN: fnerror(program$,err,line,act$,"xit")
02040   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
02050   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
02060   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
02070 ERTN_EXEC_ACT: execute act$ : goto ERTN
02080 ! /region
