00010 ! Replace S:\acsGL\acglChg
00020 ! Statement of Change in Financial Position with Comparrison  !:
        ! FOR 8 1/2 * 11
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnxit,fntop, fnopenprn,fncloseprn,fnpglen,fncno,fnerror,fnpedat$,fnactpd$,fnprocess,fnUseDeptNo,fnps,fnpriorcd,fnactpd,fnfscode,fnglfs,fntos,fnlbl,fntxt,fncmdkey,fnacs
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim fl1$*256,actpd$*6,cogl$(3)*12,pedat$*20,p$(20)*50,cap$*128
00080   dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*14
00090   dim cnam$*40,accum(9,2),acct$*12,bp(13),by(13),udf$*256
00100 ! ______________________________________________________________________
00110   let fntop(program$,cap$="Comparative (FP)")
00120   if fnglfs=5 then goto XIT
00130   let fncno(cno,cnam$)
00140   let udf$=env$('temp')&'\'
00150   open #20: "Name="&env$('Q')&"\GLmstr\Company.h"&str$(cno)&",Shr",internal,input,relative: read #20,using 'Form Pos 152,3*C 12',rec=1: mat cogl$ : close #20: 
00160   let actpd=fnactpd : let fscode=fnfscode
00170   let pors=1
00180   on fkey 5 goto L1830
00190   let mp1=75
00200   if fnps=2 then let mp1=mp1+3
00210   let fl1$="Name="&env$('Q')&"\GLmstr\ACGLFNSF.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\FNSFIndx.h"&str$(cno)&",Shr"
00220   if fnps=2 then let fl1$="Name="&env$('Q')&"\GLmstr\ACGLFNSG.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\FNSGIndx.h"&str$(cno)&",Shr"
00230 L230: form pos mp1,pd 3,pos 81,28*pd 6.2,pos 327,pd 6.2
00240 L240: form pos 1,c 12,pos 87,27*pd 6.2
00250   open #1: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\GLIndex.h"&str$(cno)&",Shr",internal,input,keyed 
00260 L260: read #1,using L240: acct$,cb,mat by,mat bp eof L340
00270   if acct$>cogl$(3) then goto L340
00280   if fnpriorcd=2 then let income=income-bp(fscode) else goto L310
00290   let pincome=0
00300   goto L330
00310 L310: if fscode<=0 or fscode>12 then !:
          let income=income-cb else !:
          let income=income-by(fscode)
00320   if fscode<=0 or fscode>12 then !:
          let pincome=pincome-bp(actpd) else !:
          let pincome=pincome-bp(fscode)
00330 L330: goto L260
00340 L340: close #1: 
00350   open #1: fl1$,internal,input,keyed 
00360   if fnprocess=1 or fnUseDeptNo=0 then goto L460
00370   let fntos(sn$="ACglchg") !:
        let mylen=30: let mypos=mylen+3 : let right=1
00380   let fnlbl(1,1,"Cost Center or Department #:",mylen,right)
00390   let fntxt(1,mypos,3,0,right,"30",0,"Enter the cost center or department number if you wish to print only one department, else leave blank for all.",0 ) !:
        let resp$(1)=""
00400   let fnlbl(2,1,"(Blank for all Departments)",mylen,right)
00410   let fncmdkey("&Next",1,1,0,"Prints the financial statement.")
00420   let fncmdkey("&Cancel",5,0,1,"Returns to menu without posting.")
00430   let fnacs(sn$,0,mat resp$,ckey)
00440   if ckey=5 then goto XIT
00450   let costcntr=val(resp$(1))
00460 L460: let fnopenprn !:
        let redir=0: if file$(255)(1:4)<>"PRN:" then let redir=1
00470   let report$="STATEMENT OF CHANGES IN FINANCIAL POSITION"
00480   if fnps=2 then goto L510 ! secondary
00490   execute "Index "&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&" "&udf$&"fsindex.H"&str$(cno)&" 75 3 Replace DupKeys -N"
00500   goto L520
00510 L510: execute "Index "&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&" "&udf$&"fsindex.H"&str$(cno)&" 78 3 Replace DupKeys -N"
00520 L520: open #3: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&",KFName="&udf$&"fsindex.h"&str$(cno)&",Shr",internal,input,keyed 
00530 L530: read #1,using L570: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof L1830
00540   if ltrm$(r$)="" or ltrm$(r$)="0" then goto L530
00550   if costcntr=0 then goto L570
00560   if costcntr><fc then goto L530
00570 L570: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
00580   if te$="S" or te$="F" then goto L600
00590   if heading=0 and te$><"R" then gosub L1620
00600 L600: on pos ("RFHDTSP",te$,1) goto L1110,L1160,L610,L660,L1000,L1110,L1890 none L530
00610 L610: print #255,using L620: d$
00620 L620: form pos sp,c 50
00630   gosub L1320
00640   gosub L1250
00650   goto L530
00660 L660: if notrans=1 then goto L840
00670   if fr=val(r$) and val(r$)><0 then goto L750
00680   if fr>val(r$) then goto L750
00690 L690: ! read amounts from gl master file
00700 L700: read #3,using L230: fr,bb,cb,mat by,mat bp,pbp eof L830
00710   if fr=0 then goto L700
00720   if fscode=0 then goto L750
00730   if fscode<1 or fscode>12 then let fscode=1
00740   if fnpriorcd=2 then let cb=bp(fscode) else let cb=by(fscode)
00750 L750: if fr=val(r$) then goto L760 else goto L810
00760 L760: if fnpriorcd=2 then !:
          let total=total+(cb-pbp) else !:
          let total=total+(cb-bp(12))
00770   if fnpriorcd=2 then let total2=0 else goto L790
00780   goto L800
00790 L790: if fscode<=0 or fscode>12 then !:
          let total2=total2+(bp(actpd)-pbp) else !:
          let total2=total2+(bp(fscode)-pbp)
00800 L800: goto L690
00810 L810: if fr<val(r$) then goto L690
00820   if fr>val(r$) then goto L840
00830 L830: let notrans=1
00840 L840: for j=1 to 9 : let accum(j,1)+=total : let accum(j,2)+=total2 : next j
00850   if rs=1 then let total=-total else goto L870
00860   let total2=-total2
00870 L870: if ds=1 then let dollar$="$" else let dollar$=" "
00880   if total><0 or total2><0 then goto L900
00890   if ls+ul+ds+ic>0 then goto L900 else goto L530
00900 L900: let sp2=49-sp-1
00910   print #255,using L920: d$(1:sp2),dollar$,total,dollar$,total2 pageoflow L1480
00920 L920: form pos sp,c sp2,pos 49,c 1,pic(--,---,---.##),pos 67,c 1,pic(--,---,---.##),skip redir
00930   let total=0
00940   let total2=0
00950   gosub L1250
00960   gosub L1490
00970   gosub L1320
00980   goto L530
00990 ! ______________________________________________________________________
01000 L1000: if ap=0 then let ap=1
01010   if rs=1 then let accum1=-accum(ap,1) else let accum1=accum(ap,1)
01020   if rs=1 then let accum2=-accum(ap,2) else let accum2=accum(ap,2)
01030   if ds=1 then let dollar$="$" else let dollar$=" "
01040   let sp2=49-sp-1
01050   print #255,using L920: d$(1:sp2),dollar$,accum1,dollar$,accum2 pageoflow L1480
01060   gosub L1250
01070   gosub L1490
01080   gosub L1320
01090   goto L530
01100 ! ______________________________________________________________________
01110 L1110: if te$="R" then let report$=d$
01120   if te$="S" then let secondr$=d$
01130   gosub L1320
01140   goto L530
01150 ! ______________________________________________________________________
01160 L1160: if foot1=1 then goto L1220
01170   let tabnote=sp
01180   let foot1=1
01190   let foot$=d$
01200   goto L530
01210 ! ______________________________________________________________________
01220 L1220: let foot$=rtrm$(foot$)&d$
01230   goto L530
01240 ! ______________________________________________________________________
01250 L1250: for j=1 to 9
01260     if ac(j)=0 then goto L1290
01270     let accum(j,1)=0
01280     let accum(j,2)=0
01290 L1290: next j
01300   return 
01310 ! ______________________________________________________________________
01320 L1320: if ls=0 then goto L1460
01330   if ls=99 then goto L1370
01340   print #255,using L1350: " "
01350 L1350: form pos 1,c 1,skip ls
01360   goto L1460
01370 L1370: let fnpglen(pglen)
01380 ! If PGLEN<>42 Then Let PGLEN=58
01390   let sk=pglen-krec(255): let fl=len(rtrm$(foot$))
01400 ! If PGLEN=42 Then Let SK=SK+1
01410   print #255,using L1420: rtrm$(foot$),"Page "&str$(pt1)
01420 L1420: form skip sk,pos tabnote,c fl,pos 75,c 8,skip 1
01430   if eofcode=1 then goto L1460
01440   print #255: newpage
01450   gosub L1620
01460 L1460: return 
01470 ! ______________________________________________________________________
01480 L1480: gosub L1370: continue 
01490 L1490: if ul=0 then goto L1580
01500   if ul=1 then goto L1550
01510   let underlin$="=============="
01520   print #255,using L1530: underlin$,underlin$
01530 L1530: form skip 1,pos 49,c 14,pos 67,c 14,skip redir
01540   goto L1580
01550 L1550: let underlin$="______________"
01560   print #255,using L1570: underlin$,underlin$
01570 L1570: form pos 49,c 14,pos 67,c 14,skip redir
01580 L1580: if redir=0 then print #255,using L1590: " "
01590 L1590: form skip 1,c 1,skip 0
01600   return 
01610 ! ______________________________________________________________________
01620 L1620: let heading=1
01630   let pt1+=1
01640   print #255: "\qc  {\f181 \fs24 \b "&env$('cnam')&"}"
01650   print #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
01660   if trim$(secondr$)<>"" then print #255: "\qc  {\f181 \fs18 \b "&trim$(secondr$)&"}"
01670   print #255: "\qc  {\f181 \fs16 \b For the "&rtrm$(fnactpd$)&" month period ended "&rtrm$(fnpedat$)&"}"
01680   print #255: "\ql "
01690   print #255: 
01700   on error goto L1780
01710   let a=len(rtrm$(fnpedat$))
01720   let b=val(rtrm$(fnpedat$(a-4:a)))
01730   let c=b-1
01740   print #255,using L1750: b,c
01750 L1750: form pos 52,pic(-----),pos 71,pic(-----),skip 2
01760   on error goto ERTN
01770   goto L1810
01780 L1780: print #255: tab(49);"CURRENT YEAR";tab(68);"PRIOR YEAR"
01790   on error goto ERTN
01800   print #255: 
01810 L1810: return 
01820 ! ______________________________________________________________________
01830 L1830: let eofcode=1
01840   gosub L1370
01850 ! 
01860   let fncloseprn
01870   goto XIT
01880 ! ______________________________________________________________________
01890 L1890: let total=income
01900   let total2=pincome
01910   goto L840
01920 ! ______________________________________________________________________
01930 XIT: let fnxit
01940 ! ______________________________________________________________________
01950 ! <Updateable Region: ERTN>
01960 ERTN: let fnerror(program$,err,line,act$,"xit")
01970   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
01980   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01990   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
02000 ERTN_EXEC_ACT: execute act$ : goto ERTN
02010 ! /region
