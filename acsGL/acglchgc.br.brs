00010 ! Replace S:\acsGL\ACGLCHGC
00020 ! STATEMENT OF CHANGES IN FINANCIAL POSITION FOR 8 1/2 * 11 PAPER WITH            COMPARSION
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnxit,fntop, fnerror,fnwait,fncno,fnopenprn,fncloseprn,fnpglen,fnpedat$,fnactpd$,fnactpd,fnfscode,fnUseDeptNo,fnpriorcd,fnps,fnprocess,fnglfs,fntos,fnlbl,fntxt,fncmdkey,fnacs
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim fl1$*256,cogl$(3)*12,cnam$*40,acct$*12,bp(13),by(13),udf$*256
00080   dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*14
00090   dim cnam$*40,b$*3,a$(8)*30,oldtrans$*16,g(8),accum(9,2),by(13),cap$*128
00100 ! ______________________________________________________________________
00110   fntop(program$,cap$="Comparative Change Amount")
00120   if fnglfs=5 then goto XIT
00130   let udf$=env$('temp')&'\'
00140   fncno(cno,cnam$)
00150   open #20: "Name="&env$('Q')&"\GLmstr\Company.h"&str$(cno)&",Shr",internal,input,relative: read #20,using 'Form Pos 152,3*C 12',rec=1: mat cogl$ : close #20: 
00160   actpd=fnactpd : let fscode=fnfscode : priorcd=fnpriorcd
00170   on fkey 5 goto L2060
00180   mp1=75
00190   if fnps=2 then mp1=mp1+3
00200   fl1$="Name="&env$('Q')&"\GLmstr\ACGLFNSF.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\FNSFIndx.h"&str$(cno)&",Shr"
00210   if fnps=2 then fl1$="Name="&env$('Q')&"\GLmstr\ACGLFNSG.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\FNSGIndx.h"&str$(cno)&",Shr"
00220   flo$(1)="8,3,C 50,N" !:
        flo$(2)="8,55,N 10.2,N" !:
        flo$(3)="8,69,N 10.2,N"
00230   fli$(1)="08,03,C 50,UT,N" !:
        fli$(2)="08,55,N 10.2,UT,N" !:
        fli$(3)="08,69,N 10.2,UT,N"
00240   open #1: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\GLIndex.h"&str$(cno)&",Shr",internal,input,keyed 
00250 L250: read #1,using L260: acct$,cb,mat by,mat bp eof L350
00260 L260: form pos 1,c 12,pos 87,27*pd 6.2
00270   if acct$>cogl$(3) then goto L350
00280   if priorcd=2 then income=income-bp(fscode) else goto L310
00290   pincome=0
00300   goto L330
00310 L310: if fscode<=0 or fscode>12 then !:
          income=income-cb else income=income-by(fscode)
00320   if fscode<=0 or fscode>12 then !:
          pincome=pincome-bp(actpd) else pincome=pincome-bp(fscode)
00330 L330: goto L250
00340 ! ___________________________
00350 L350: close #1: 
00360   open #1: fl1$,internal,input,keyed 
00370   if fnprocess=1 or fnUseDeptNo=0 then goto L480
00380 ! ___________________________
00390   fntos(sn$="ACglchgc") !:
        mylen=30: mypos=mylen+3 : right=1
00400   fnlbl(1,1,"Cost Center or Department #:",mylen,right)
00410   fntxt(1,mypos,3,0,right,"30",0,"Enter the cost center or department number if you wish to pr only one department, else leave blank for all.",0 ) !:
        resp$(1)=""
00420   fnlbl(2,1,"(Blank for all Departments)",mylen,right)
00430   fncmdkey("&Next",1,1,0,"Prints the financial statement.")
00440   fncmdkey("&Cancel",5,0,1,"Returns to menu without posting.")
00450   fnacs(sn$,0,mat resp$,ckey)
00460   if ckey=5 then goto XIT
00470   costcntr=val(resp$(1))
00480 L480: fnopenprn !:
        if file$(255)(1:4)<>"PRN:" then redir=1 else redir=0
00490   report$="Statement of Changes in Financial Position"
00500   if fnps=2 then goto L530 ! secondary
00510   execute "Index "&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&" "&udf$&"fsindex.H"&str$(cno)&" 75 3 Replace DupKeys -N"
00520   goto L540
00530 L530: execute "Index "&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&" "&udf$&"fsindex.H"&str$(cno)&" 78 3 Replace DupKeys -N"
00540 L540: open #3: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&",KFName="&udf$&"fsindex.h"&str$(cno)&",Shr",internal,input,keyed 
00550 L550: read #1,using L590: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof L2060
00560   if ltrm$(r$)="" or ltrm$(r$)="0" then goto L550
00570   if costcntr=0 then goto L590
00580   if costcntr><fc then goto L550
00590 L590: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
00600   if te$="S" or te$="F" then goto L620
00610   if heading=0 and te$><"R" then gosub L1840
00620 L620: on pos ("RFHDTSP",te$,1) goto L1310,L1360,L630,L680,L1200,L1310,L2110 none L550
00630 L630: pr #255,using L640: d$
00640 L640: form pos sp,c 50,skip 1
00650   gosub L1510
00660   gosub L1440
00670   goto L550
00680 L680: if notrans=1 then goto L1020
00690   if fr=val(r$) and val(r$)><0 then goto L780
00700   if fr>val(r$) then goto L780
00710 L710: ! read amounts from g/ master file
00720 L720: read #3,using L740: fr,bb,cb,mat by,mat bp,pbp eof L860
00730   if fr=0 then goto L720
00740 L740: form pos mp1,pd 3,pos 81,28*pd 6.2,pos 327,pd 6.2
00750   if fscode=0 then goto L780
00760   if fscode<1 or fscode>12 then let fscode=1
00770   if priorcd=2 then cb=bp(fscode) else cb=by(fscode)
00780 L780: if fr=val(r$) then goto L790 else goto L840
00790 L790: if priorcd=2 then total=total+(cb-pbp) !:
        else total=total+(cb-bp(12))
00800   if priorcd=2 then total2=0 else goto L820
00810   goto L830
00820 L820: if fscode<=0 or fscode>12 then total2=total2+(bp(actpd)-pbp) else !:
          total2=total2+(bp(fscode)-pbp)
00830 L830: goto L710
00840 L840: if fr<val(r$) then goto L710
00850   if fr>val(r$) then goto L870
00860 L860: notrans=1
00870 L870: fntos(sn$="ACglchgs2") !:
        mylen=30: mypos=mylen+3 : right=1
00880   fnlbl(1,1,"Description:",mylen,right)
00890   fntxt(1,mypos,50,0,right,"",0,"Enter the description if not accurate.",0 ) !:
        resp$(1)=d$
00900   fnlbl(2,1,"Total Year to Date:",mylen,right)
00910   fntxt(2,mypos,12,0,right,"10",0,"Enter the total for the year.",0 ) !:
        resp$(2)=str$(total)
00920   fnlbl(3,1,"Total Last Year to Date:",mylen,right)
00930   fntxt(3,mypos,12,0,right,"10",0,"Enter the total for last year.",0 ) !:
        resp$(3)=str$(total2)
00940   fncmdkey("&Next",1,1,0,"Accept the answer.")
00950   fncmdkey("&Cancel",5,0,1,"Returns to menu without posting.")
00960   fnacs(sn$,0,mat resp$,ckey)
00970   if ckey=5 then goto XIT
00980   d$=resp$(1)
00990   total=val(resp$(2))
01000   total2=val(resp$(3))
01010 ! 
01020 L1020: for j=1 to 9
01030     accum(j,1)=accum(j,1)+total
01040     accum(j,2)=accum(j,2)+total2
01050   next j
01060   if rs=1 then total=-total else goto L1080
01070   total2=-total2
01080 L1080: if ds=1 then dollar$="$" else dollar$=" "
01090   if total><0 or total2><0 then goto L1110
01100   if ls+ul+ds+ic>0 then goto L1110 else goto L550
01110 L1110: sp2=49-sp-1
01120   pr #255,using L1130: d$(1:sp2),dollar$,total,dollar$,total2 pageoflow L1680
01130 L1130: form pos sp,c sp2,pos 49,c 1,pic(--,---,---.##),pos 67,c 1,pic(--,---,---.##),skip redir
01140   total=0
01150   total2=0
01160   gosub L1440
01170   gosub L1700
01180   gosub L1510
01190   goto L550
01200 L1200: if ap=0 then ap=1
01210   if rs=1 then accum1=-accum(ap,1) else accum1=accum(ap,1)
01220   if rs=1 then accum2=-accum(ap,2) else accum2=accum(ap,2)
01230   sp2=49-sp-1
01240   if ds=1 then dollar$="$" else dollar$=" "
01250   pr #255,using L1130: d$(1:sp2),dollar$,accum1,dollar$,accum2 pageoflow L1680
01260   gosub L1440
01270   gosub L1700
01280   gosub L1510
01290   goto L550
01300 ! ______________________________________________________________________
01310 L1310: if te$="R" then report$=d$
01320   if te$="S" then secondr$=d$
01330   gosub L1510
01340   goto L550
01350 ! ______________________________________________________________________
01360 L1360: if foot1=1 then goto L1420
01370   tabnote=sp
01380   let foot1=1
01390   let foot$=d$
01400   goto L550
01410 ! ______________________________________________________________________
01420 L1420: let foot$=rtrm$(foot$)&d$
01430   goto L550
01440 L1440: for j=1 to 9
01450     if ac(j)=0 then goto L1480
01460     accum(j,1)=0
01470     accum(j,2)=0
01480 L1480: next j
01490   return 
01500 ! ______________________________________________________________________
01510 L1510: if ls=0 then goto L1660
01520   if ls=99 then goto L1570
01530   pr #255,using L1540: " "
01540 L1540: form pos 1,c 1,skip ls
01550   goto L1660
01560 ! ______________________________________________________________________
01570 L1570: fnpglen(pglen)
01580 ! If PGLEN<>42 Then pGLEN=58
01590   sk=pglen-krec(255): fl=len(rtrm$(foot$))
01600 ! If PGLEN=42 Then sK=SK+1
01610   pr #255,using L1620: rtrm$(foot$),"Page "&str$(pt1)
01620 L1620: form skip sk,pos tabnote,c fl,pos 75,c 8,skip 1
01630   if eofcode=1 then goto L1660
01640   pr #255: newpage
01650   gosub L1840
01660 L1660: return 
01670 ! ______________________________________________________________________
01680 L1680: gosub L1570: continue 
01690 ! ______________________________________________________________________
01700 L1700: if ul=0 then goto L1800
01710   if ul=1 then goto L1770
01720   let underlin$="=============="
01730   pr #255,using L1740: underlin$,underlin$
01740 L1740: form skip 1,pos 49,c 14,pos 67,c 14,skip redir
01750   goto L1800
01760 ! ______________________________________________________________________
01770 L1770: let underlin$="______________"
01780   pr #255,using L1790: underlin$,underlin$
01790 L1790: form pos 49,c 14,pos 67,c 14,skip redir
01800 L1800: if redir=0 then pr #255,using L1810: " "
01810 L1810: form skip 1,c 1,skip 0
01820   return 
01830 ! ______________________________________________________________________
01840 L1840: heading=1
01850   pt1+=1
01860   pr #255: "\qc  {\f181 \fs24 \b "&env$('cnam')&"}"
01870   pr #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
01880   if trim$(secondr$)<>"" then pr #255: "\qc  {\f181 \fs18 \b "&trim$(secondr$)&"}"
01890   pr #255: "\qc  {\f181 \fs16 \b For the "&rtrm$(fnactpd$)&" month period ended "&rtrm$(fnpedat$)&"}"
01900   pr #255: "\ql "
01910   pr #255: ""
01920   on error goto L2010
01930   a=len(rtrm$(fnpedat$))
01940   b=val(rtrm$(fnpedat$(a-4:a)))
01950   c=b-1
01960   pr #255,using L1970: b,c
01970 L1970: form pos 52,pic(-----),pos 71,pic(-----),skip 2
01980   on error goto ERTN
01990   goto L2040
02000 ! ______________________________________________________________________
02010 L2010: pr #255: tab(49);"Current Year";tab(68);"Prior Year"
02020   on error goto ERTN
02030   pr #255: 
02040 L2040: return 
02050 ! ______________________________________________________________________
02060 L2060: eofcode=1
02070   gosub L1570
02080   fncloseprn
02090   goto XIT
02100 ! ______________________________________________________________________
02110 L2110: total=income
02120   total2=pincome
02130   goto L1020
02140 ! ______________________________________________________________________
02150 XIT: fnxit
02160 ! ______________________________________________________________________
02170 ERTN: fnerror(program$,err,line,act$,"xit")
02180   if lwrc$(act$)<>"pause" then goto L2210
02190   execute "list -"&str$(line) !:
        pause  !:
        goto L2210
02200   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause 
02210 L2210: execute act$
02220   goto ERTN
