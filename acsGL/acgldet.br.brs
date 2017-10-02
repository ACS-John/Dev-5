00010 ! Replace S:\acsGL\AcGlDet
00020 ! -- Modified Cash Flow Statement (Detailed Transactions, Not Balances)
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnpglen,fnerror,fncno,fnchain, fnprocess,fnps,fnpriorcd,fnUseDeptNo,fnfscode,fnpedat$,fnactpd$,fnactpd,fncch$,fntos,fnlbl,fntxt,fncmdkey,fnacs,fnglfs
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim fl1$*256,in3$(4)
00080   dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*14
00090   dim cnam$*40,b$*3,a$(8)*30,oldtrans$*16,g(8),accum(9,7),tr(7),cap$*128
00100   dim bm(13),bp(13),by(13),tr$*12,td$*30,ta(2)
00110 ! ______________________________________________________________________
00120   let fntop(program$,cap$="Cash Flow Statement - Detail") ! not on menu
00130   let report$=cap$
00140   let fncno(cno,cnam$)
00145   actpd$=fnactpd$ !:
        actpd=fnactpd !:
        let fnfscode !:
        let fnpriorcd
00150   if fnglfs=5 then goto XIT !:
          ! sets fnps,fnpriorcd,fnfscode (primary/secondary,current year/Prior,period to print)
00151   let fscode=fnfscode !:
        let priorcd=fnpriorcd
00160   on fkey 5 goto L1870
00170   open #20: "Name="&env$('Q')&"\GLmstr\Company.h"&str$(cno)&",Shr",internal,outin,relative  !:
        read #20,using 'form pos 384,n 2': nap : close #20: 
00180   let pors=1
00190   let fnopenprn
00200   let in3$(1)="8,25,N 12.2,UT,N"
00210   let in3$(2)="8,45,N 12.2,UT,N"
00220   let mp1=75
00230   if fnps=2 then let mp1+=3
00240   let fl1$="Name="&env$('Q')&"\GLmstr\ACGLFNSF.H"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\FNSFINDX.H"&str$(cno)&",Shr"
00250   if fnps=2 then !:
          let fl1$="Name="&env$('Q')&"\GLmstr\ACGLFNSG.H"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\FNSGINDX.H"&str$(cno)&",Shr"
00260   let nametab=int(44-len(rtrm$(cnam$))/2)
00270   open #1: fl1$,internal,input,keyed 
00280   open #3: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&",Shr",internal,input,relative 
00300   open #4: "Name="&env$('Q')&"\GLmstr\GLTRANS.H"&str$(cno)&",Shr",internal,outin,relative 
00310   if fnprocess=1 or fnUseDeptNo=0 then goto L410
00320   let fntos(sn$="GLInput") !:
        let mylen=30: let mypos=mylen+3 : let right=1
00330   let fnlbl(1,1,"Cost Center or Department #:",mylen,right)
00340   let fntxt(1,mypos,3,0,right,"30",0,"Enter the cost center or department number if you wish to pr only one department, else leave blank for all.",0 ) !:
        let resp$(1)=""
00350   let fnlbl(2,1,"(Blank for all Departments)",mylen,right)
00360   let fncmdkey("&Next",1,1,0,"Prints the financial statement.")
00370   let fncmdkey("&Cancel",5,0,1,"Returns to menu without posting.")
00380   let fnacs(sn$,0,mat resp$,ckey)
00390   if ckey=5 then goto XIT
00400   costcntr=val(resp$(1))
00410 L410: read #1,using L450: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof L1870
00420   if ltrm$(r$)="" or ltrm$(r$)="0" then goto L410
00430   if costcntr=0 then goto L450
00440   if costcntr><fc then goto L410
00450 L450: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
00460   if te$="S" or te$="F" then goto L480
00470   if heading=0 and te$><"R" then gosub L1740
00480 L480: on pos ("RFHDTSBC",te$,1) goto L1270,L1310,L490,L540,L1170,L1270,L540,L1940 none L410
00490 L490: pr #255,using L500: d$(1:40)
00500 L500: form pos sp,c 40,skip 1
00510   gosub L1450
00520   gosub L1380
00530   goto L410
00540 L540: if te$="B" and ap>0 then goto L1170 ! ENDING BANK BALANCE
00550   if notrans=1 then goto L950
00560   if ir>=val(r$) and val(r$)><0 then goto L680
00570 L570: ! Read #2,Using 580: RECORD Eof 940
00580 ! Form PD 3
00590   read #3,using L670: ir,bb,cb,mat by,mat bp,mat bm,mat ta eof L940
00600   if fnfscode=0 or (fscode=actpd and priorcd=1) then goto L670
00610   if fnfscode<1 or fnfscode>13 then let fnfscode=1
00620   if fnpriorcd=1 then cb=by(fnfscode) else cb=bp(fnfscode)
00630   if fnpriorcd=2 then goto L660
00640   if fnfscode>1 then bb=by(fnfscode-1) else bb=0
00650   goto L670
00660 L660: if fnfscode>1 then bb=bp(fnfscode-1) else bb=0
00670 L670: form pos mp1,pd 3,pos 81,41*pd 6.2,pos 333,2*pd 3
00680 L680: if ir=val(r$) then let total=total+(cb-bb) else goto L920
00690   if te$="B" then let total-=(cb-bb): let total-=bb: let total2-=bp(nap) : goto L710
00700   let total2=total2+cb
00710 L710: for z=1 to 13
00720     annualb=annualb+bm(z)
00730   next z
00740   if fnfscode=0 then let monthb+=bm(fnactpd) else let monthb+=bm(fnfscode)
00750   if fnfscode=0 then goto L760 else goto L800
00760 L760: for j=1 to fnactpd
00770     let ytdb=ytdb+bm(j)
00780   next j
00790   goto L840
00800 L800: for j=1 to fnfscode
00810     let ytdb=ytdb+bm(j)
00820   next j
00830   goto L840
00840 L840: ! pr DETAILS
00850   let nta=ta(1)
00860 L860: if nta=0 then goto L910
00870   read #4,using L880,rec=nta,release: mat tr,tr$,td$,nta
00880 L880: form pos 1,n 3,n 6,n 3,n 6,pd 6.2,2*n 2,c 12,c 30,pd 3
00890   if rs=1 then pr #255,using L1100: td$(1:sp2)," ",-tr(5) else pr #255,using L1100: td$(1:sp2),"",tr(5)
00900   goto L860
00910 L910: goto L570
00920 L920: if ir<val(r$) then goto L570
00930   if ir>val(r$) then goto L950
00940 L940: let notrans=1
00950 L950: for j=1 to 9
00960     if ac(j)=9 then goto L990
00970     accum(j,1)=accum(j,1)+total
00980     accum(j,2)=accum(j,2)+total2
00990 L990: next j
01000   if rs=1 then let total=-total else goto L1030
01010   let total2=-total2
01020   let ytdb=-ytdb
01030 L1030: if ds=1 then let dollar$="$" else let dollar$=" "
01040   if total><0 or total2><0 then goto L1070
01050   if total<>0 then goto L1070
01060   if ls+ds+ul+ic>0 then goto L1070 else goto L410
01070 L1070: let sp2=30-sp-1
01080   if te$="B" then let total=-total: let total2=-total2 ! REVERSE SIGN ON BEGINNING BANK BALANCE
01090   if total=0 and total2=0 and ls+ds+ul+ic>0 then pr #255,using L1100: "",dollar$,0 pageoflow L1600
01100 L1100: form pos sp,c sp2,pos 52,c 1,pic(--,---,---.##),x 1,c 1,pic(--,---,---.##),skip 1
01110   let total=0
01120   let total2=0
01130   gosub L1380
01140   gosub L1610
01150   gosub L1450
01160   goto L410
01170 L1170: if ap=0 then ap=1
01180   if rs=1 then accum1=-accum(ap,1) else accum1=accum(ap,1)
01190   if rs=1 then accum2=-accum(ap,2) else accum2=accum(ap,2)
01200   if ds=1 then let dollar$="$" else let dollar$=" "
01210   let sp2=30-sp-1
01220   pr #255,using L1100: d$(1:sp2),dollar$,accum1 pageoflow L1600
01230   gosub L1380
01240   gosub L1610
01250   gosub L1450
01260   goto L410
01270 L1270: if te$="R" then let report$=d$
01280   if te$="S" then let secondr$=d$
01290   gosub L1450
01300   goto L410
01310 L1310: if foot1=1 then goto L1360
01320   let tabnote=sp
01330   let foot1=1
01340   let foot$=d$
01350   goto L410
01360 L1360: let foot$=rtrm$(foot$)&d$
01370   goto L410
01380 L1380: for j=1 to 9
01390     if ac(j)=0 or ac(j)=9 then goto L1420
01400     accum(j,1)=0
01410     accum(j,2)=0
01420 L1420: next j
01430   return 
01440 ! ______________________________________________________________________
01450 L1450: if ls=0 then goto L1580
01460   if ls=99 then goto L1500
01470   pr #255,using L1480: " "
01480 L1480: form pos 1,c 1,skip ls
01490   goto L1580
01500 L1500: let fnpglen(pglen)
01510   if pglen<>42 then let pglen=58
01520   let sk=pglen-krec(255): let fl=len(rtrm$(foot$))
01530   pr #255,using L1540: rtrm$(foot$),"Page "&str$(pt1)
01540 L1540: form skip sk,pos tabnote,c fl,pos 70,c 8,skip 1
01550   if eofcode=1 then goto L1580
01560   pr #255: newpage
01570   gosub L1740
01580 L1580: return 
01590 ! ______________________________________________________________________
01600 L1600: gosub L1500: continue 
01610 L1610: if ul=0 then goto L1700
01620   if ul=1 then goto L1670
01630   let underlin$="=============="
01640   pr #255: 
01650   goto L1680
01660   goto L1700
01670 L1670: let underlin$="______________"
01680 L1680: pr #255,using L1690: underlin$
01690 L1690: form skip 1,pos 52,2*c 15,skip 1
01700 L1700: ! f REDIR=0 Then pr #255,Using 1560: " "
01710   form skip 1,c 1,skip 0
01720   return 
01730 ! ______________________________________________________________________
01740 L1740: let heading=1
01750   let pt1+=1
01760   pr #255: "\qc  {\f181 \fs18 \b "&env$('cnam')&"}"
01770   pr #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
01780   if trim$(secondr$)<>"" then pr #255: "\qc  {\f181 \fs24 \b "&trim$(secondr$)&"}"
01790   pr #255: "\qc  {\f181 \fs16 \b For the "&rtrm$(fnactpd$)&" month period ended "&rtrm$(fnpedat$)&"}"
01800   pr #255: "\ql "
01810   pr #255: 
01820   pr #255: tab(50);fncch$
01830   pr #255: tab(56);"       "
01840   pr #255: 
01850   return 
01860 ! ______________________________________________________________________
01870 L1870: let eofcode=1
01880   gosub L1500
01890   let fnfscode(actpd)
01900   let fnpriorcd(1)
01910   let fncloseprn
01920 ! 
01930   goto XIT
01940 L1940: ! 
01950   pr fields "2,5,C 75,N": "ENTER THE FOLLOWING INFORMATION FOR "& rtrm$(d$)
01960   pr fields "6,5,C 70,N": "                    CURRENT         "
01970   pr fields "7,5,C 70,N": "                     MONTH       "
01980 L1980: input fields mat in3$: total conv L1980
01990   goto L950
02000 ! ______________________________________________________________________
02010 XIT: let fnxit
02020 ! ______________________________________________________________________
02030 ! <updateable region: ertn>
02040 ERTN: let fnerror(program$,err,line,act$,"xit")
02050   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
02060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
02070   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
02080 ERTN_EXEC_ACT: execute act$ : goto ERTN
02090 ! /region
02100 ! ______________________________________________________________________
