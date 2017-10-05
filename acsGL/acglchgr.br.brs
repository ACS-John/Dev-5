00010 ! Replace S:\acsGL\acglChgR
00020 ! Statement of Change in Financial Position with Comparison !:
        ! for Letter size paper
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnpglen,fnerror,fnprocess,fncno,fnUseDeptNo,fnpedat$,fnps,fnpriorcd,fnfscode,fnactpd$,fncch$,fnglfs,fnwait,fnactpd,fntos,fnlbl,fntxt,fncmdkey,fnacs
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim fl1$*256,actpd$*6,cogl$(3)*12,pedat$*20
00080   dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*14
00090   dim cnam$*40,b$*3,a$(8)*30,oldtrans$*16,g(8),accum(9,2),udf$*256
00100   dim acct$*12,bp(13),lastact$*12,d(2),by(13),cap$*128,message$*40
00110 ! ______________________________________________________________________
00120   fntop(program$,cap$="Change in Financial Position")
00130   fncno(cno,cnam$)
00140   let udf$=env$('temp')&'\'
00150   if fnglfs=5 then goto XIT !:
          ! sets fnps,fnpriorcd,fnfscode (primary/secondary,current year/Prior,period to print)
00160   pedat$=fnpedat$
00170   actpd$=fnactpd$
00180   actpd=fnactpd
00190   let fscode=fnfscode
00200   priorcd=fnpriorcd
00210 ! ______________
00220   mp1=75
00230   if fnps=2 then mp1=mp1+3
00240   fl1$="Name="&env$('Q')&"\GLmstr\ACGLFNSF.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\FNSFIndx.h"&str$(cno)&",Shr"
00250   if fnps=2 then fl1$="Name="&env$('Q')&"\GLmstr\ACGLFNSG.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\FNSGIndx.h"&str$(cno)&",Shr"
00260 ! ______________
00270   open #1: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\GLIndex.h"&str$(cno)&",Shr",internal,input,keyed 
00280 L280: read #1,using L290: acct$,cb,mat by,mat bp eof L350
00290 L290: form pos 1,c 12,pos 87,27*pd 6.2
00300   if acct$>cogl$(3) then goto L350
00310   if fscode=0 then income=income-cb else goto L330
00320   goto L340
00330 L330: if priorcd=2 then income=income-bp(fscode) !:
        else income=income-by(fscode)
00340 L340: goto L280
00350 L350: close #1: 
00360   open #1: fl1$,internal,input,keyed 
00370   if fnprocess=1 or fnUseDeptNo=0 then goto BEGIN_PRINTING
00380   fntos(sn$="GLInput") !:
        mylen=30: mypos=mylen+3 : right=1
00390   fnlbl(1,1,"Cost Center or Department #:",mylen,right)
00400   fntxt(1,mypos,3,0,right,"30",0,"Enter the cost center or department number if you wish to pr only one department, else leave blank for all.",0 ) !:
        resp$(1)=""
00410   fnlbl(2,1,"(Blank for all Departments)",mylen,right)
00420   fncmdkey("&Next",1,1,0,"Prints the financial statement.")
00430   fncmdkey("&Cancel",5,0,1,"Returns to menu without posting.")
00440   fnacs(sn$,0,mat resp$,ckey)
00450   if ckey=5 then goto XIT
00460   costcntr=val(resp$(1))
00470 BEGIN_PRINTING: ! 
00480   on fkey 5 goto L1840
00490   fnopenprn !:
        if file$(255)(1:4)<>"PRN:" then redir=1 else redir=0
00500   report$="STATEMENT OF CHANGES IN FINANCIAL POSITION"
00510   fnopenprn !:
        if file$(255)(1:4)<>"PRN:" then redir=1 else redir=0
00520   if fnps=2 then goto L550 ! secondary
00530   execute "Index "&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&" "&udf$&"fsindex.H"&str$(cno)&" 75 3 Replace DupKeys -N"
00540   goto L560
00550 L550: execute "Index "&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&" "&udf$&"fsindex.H"&str$(cno)&" 78 3 Replace DupKeys -N"
00560 L560: open #3: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&",KFName="&udf$&"fsindex.h"&str$(cno)&",Shr",internal,input,keyed 
00570 L570: read #1,using L610: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof DONE
00580   if ltrm$(r$)="" or ltrm$(r$)="0" then goto L570
00590   if costcntr=0 then goto L610
00600   if costcntr><fc then goto L570
00610 L610: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
00620   if te$="S" or te$="F" then goto L640
00630   if heading=0 and te$><"R" then gosub L1610
00640 L640: on pos ("RFHDTSP",te$,1) goto L1120,L1170,L650,L710,L1020,L1120,L1870 none L570
00650 L650: pr #255,using L660: d$
00660 L660: form pos sp,c 50,skip 1
00670   gosub L1310
00680   gosub L1260
00690   goto L570
00700 ! ______________________________________________________________________
00710 L710: if notrans=1 then goto L880
00720   if fr=val(r$) and val(r$)><0 then goto L810
00730   if fr>val(r$) then goto L810
00740 L740: ! read amounts form gl maste file
00750 L750: read #3,using L770: fr,bb,cb,mat by,mat bp,pbp norec L740 eof L870
00760   if fr=0 then goto L750
00770 L770: form pos mp1,pd 3,pos 81,28*pd 6.2,pos 327,pd 6.2
00780   if fscode=0 then goto L810
00790   if fscode<1 or fscode>12 then let fscode=1
00800   if priorcd=2 then cb=bp(fscode) else cb=by(fscode)
00810 L810: if fr=val(r$) then goto L820 else goto L850
00820 L820: if priorcd=2 then total+=(cb-pbp) else total+=(cb-bp(12))
00830   goto L740
00840 ! ______________________________________________________________________
00850 L850: if fr<val(r$) then goto L740
00860   if fr>val(r$) then goto L880
00870 L870: notrans=1
00880 L880: for j=1 to 9 : accum(j,1)+=total : next j
00890   if rs=1 then total=-total else goto L900
00900 L900: if ds=1 then dollar$="$" else dollar$=" "
00910   if total><0 then goto L930
00920   if ls+ul+ds+ic>0 then goto L930 else goto L570
00930 L930: sp2=67-sp-1
00940   pr #255,using L950: d$(1:sp2),dollar$,total pageoflow L1470
00950 L950: form pos sp,c sp2,pos 67,c 1,pic(--,---,---.##),skip redir
00960   total=0
00970   gosub L1260
00980   gosub L1480
00990   gosub L1310
01000   goto L570
01010 ! ______________________________________________________________________
01020 L1020: if ap=0 then ap=1
01030   if rs=1 then accum1=-accum(ap,1) else accum1=accum(ap,1)
01040   if ds=1 then dollar$="$" else dollar$=" "
01050   sp2=67-sp-1
01060   pr #255,using L950: d$(1:sp2),dollar$,accum1 pageoflow L1470
01070   gosub L1260
01080   gosub L1480
01090   gosub L1310
01100   goto L570
01110 ! ______________________________________________________________________
01120 L1120: if te$="R" then report$=d$
01130   if te$="S" then secondr$=d$
01140   gosub L1310
01150   goto L570
01160 ! ______________________________________________________________________
01170 L1170: if foot1=1 then goto L1230
01180   tabnote=sp
01190   let foot1=1
01200   let foot$=d$
01210   goto L570
01220 ! ______________________________________________________________________
01230 L1230: let foot$=rtrm$(foot$)&d$
01240   goto L570
01250 ! ______________________________________________________________________
01260 L1260: for j=1 to 9
01270     if ac(j)<>0 then accum(j,1)=0
01280   next j
01290   return 
01300 ! ______________________________________________________________________
01310 L1310: if ls=0 then goto L1450
01320   if ls=99 then goto L1360
01330   pr #255,using L1340: " "
01340 L1340: form pos 1,c 1,skip ls
01350   goto L1450
01360 L1360: fnpglen(pglen)
01370 ! If PGLEN<>42 Then pGLEN=58
01380   sk=pglen-krec(255): fl=len(rtrm$(foot$))
01390 ! If PGLEN=42 Then sK=SK+1
01400   pr #255,using L1410: rtrm$(foot$),"Page "&str$(pt1)
01410 L1410: form skip sk,pos tabnote,c fl,pos 75,c 8,skip 1
01420   if eofcode=1 then goto L1450
01430   pr #255: newpage
01440   gosub L1610
01450 L1450: return 
01460 ! ______________________________________________________________________
01470 L1470: gosub L1360: continue 
01480 L1480: if ul=0 then goto L1570
01490   if ul=1 then goto L1540
01500   let underlin$="=============="
01510   pr #255,using L1520: underlin$
01520 L1520: form skip 1,pos 67,c 14,skip redir
01530   goto L1570
01540 L1540: let underlin$="______________"
01550   pr #255,using L1560: underlin$
01560 L1560: form pos 67,c 14,skip redir
01570 L1570: if redir=0 then pr #255,using L1580: " "
01580 L1580: form skip 1,c 1,skip 0
01590   return 
01600 ! ______________________________________________________________________
01610 L1610: heading=1
01620   pt1+=1
01630   pr #255: "\qc  {\f181 \fs24 \b "&env$('cnam')&"}"
01640   pr #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
01650   if trim$(secondr$)<>"" then pr #255: "\qc  {\f181 \fs18 \b "&trim$(secondr$)&"}"
01660   pr #255: "\qc  {\f181 \fs16 \b For the "&rtrm$(fnactpd$)&" month period ended "&rtrm$(fnpedat$)&"}"
01670   pr #255: "\ql "
01680   pr #255: ""
01690   on error goto L1760
01700   a=len(rtrm$(pedat$))
01710   b=val(rtrm$(pedat$(a-4:a)))
01720   pr #255,using L1730: b
01730 L1730: form pos 72,pic(zzzz),skip 2
01740   on error goto ERTN
01750   goto L1790
01760 L1760: pr #255: tab(68);"Current Year"
01770   on error goto ERTN
01780   pr #255: 
01790 L1790: return 
01800 ! ______________________________________________________________________
01810 DONE: ! 
01820   eofcode=1
01830   gosub L1360
01840 L1840: fncloseprn
01850   goto XIT
01860 ! ______________________________________________________________________
01870 L1870: total=income
01880   goto L880
01890 ! ______________________________________________________________________
01900 XIT: fnxit
01910 ! ______________________________________________________________________
01920 ! <Updateable Region: ERTN>
01930 ERTN: fnerror(program$,err,line,act$,"xit")
01940   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
01950   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01960   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01970 ERTN_EXEC_ACT: execute act$ : goto ERTN
01980 ! /region
