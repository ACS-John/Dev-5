00010 ! Replace S:\acsGL\acglChgS
00020 ! STATEMENT OF CHANGES IN FINANCIAL POSITION WITH COMPARRISON !:
        ! FOR 8 1/2 * 11 PAPER
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnxit,fntop, fnopenprn,fncloseprn,fnpglen,fncno,fnerror,fnpedat$,fnactpd$,fnprocess,fnUseDeptNo,fnps,fnpriorcd,fnfscode,fnglfs,fntos,fnlbl,fntxt,fncmdkey,fnacs
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim fl1$*256,cogl$(3)*12,cap$*128,udf$*256
00080   dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*14
00090   dim cnam$*40,b$*3,a$(8)*30,oldtrans$*16,g(8),accum(9,2)
00100   dim acct$*12,bp(13),fli$(2),flo$(2),by(13)
00110   dim p$(20)*50
00120 ! ______________________________________________________________________
00130   fntop(program$,cap$="Change Amount")
00140   fncno(cno,cnam$)
00150   if fnglfs=5 then goto XIT
00160   udf$=env$('temp')&'\'
00170   open #20: "Name="&env$('Q')&"\GLmstr\Company.h"&env$('cno')&",Shr",internal,input,relative: read #20,using 'Form Pos 152,3*C 12',rec=1: mat cogl$ : close #20: 
00180   fscode=fnfscode
00190   pors=1
00200   mp1=75
00210   if fnps=2 then mp1=mp1+3
00220   fl1$="Name="&env$('Q')&"\GLmstr\ACGLFNSF.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\FNSFIndx.h"&env$('cno')&",Shr"
00230   if fnps=2 then fl1$="Name="&env$('Q')&"\GLmstr\ACGLFNSG.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\FNSGIndx.h"&env$('cno')&",Shr"
00240 L240: form pos 1,c 12,pos 87,27*pd 6.2
00250   flo$(1)="8,5,C 50,N"
00260   flo$(2)="8,58,N 10.2,N"
00270   fli$(1)="8,5,C 50,UT,N"
00280   fli$(2)="8,58,N 10.2,UT,N"
00290   open #1: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\GLIndex.h"&env$('cno')&",Shr",internal,input,keyed 
00300 L300: read #1,using L240: acct$,cb,mat by,mat bp eof L360
00310   if acct$>cogl$(3) then goto L360
00320   if fscode=0 then income=income-cb else goto L340
00330   goto L350
00340 L340: if fnpriorcd=2 then income-=bp(fscode) else income-=by(fscode)
00350 L350: goto L300
00360 L360: close #1: 
00370   open #1: fl1$,internal,input,keyed 
00380   if fnprocess=1 or fnUseDeptNo=0 then goto L480
00390   fntos(sn$="ACglchgs") !:
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
00480 L480: on fkey 5 goto L1970 !:
        fnopenprn !:
        if file$(255)(1:4)<>"PRN:" then redir=1 else redir=0
00490 ! 
00500   report$="Statement of Changes in Financial Position"
00510   if fnps=2 then goto L540 ! secondary
00520   execute "Index "&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&" "&udf$&"fsindex.H"&env$('cno')&" 75 3 Replace DupKeys -N"
00530   goto L550
00540 L540: execute "Index "&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&" "&udf$&"fsindex.H"&env$('cno')&" 78 3 Replace DupKeys -N"
00550 L550: open #3: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&",KFName="&udf$&"fsindex.h"&env$('cno')&",Shr",internal,input,keyed 
00560 L560: read #1,using L600: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof L1970
00570   if ltrm$(r$)="" or ltrm$(r$)="0" then goto L560
00580   if costcntr=0 then goto L600
00590   if costcntr><fc then goto L560
00600 L600: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
00610   if te$="S" or te$="F" then goto L630
00620   if heading=0 and te$><"R" then gosub L1760
00630 L630: on pos ("RFHDTSP",te$,1) goto L1240,L1290,L640,L700,L1140,L1240,L2040 none L560
00640 L640: pr #255,using L650: d$
00650 L650: form pos sp,c 50
00660   gosub L1430
00670   gosub L1370
00680   goto L560
00690 ! ______________________________________________________________________
00700 L700: if notrans=1 then goto L860
00710   if fr=val(r$) and val(r$)><0 then goto L800
00720   if fr>val(r$) then goto L800
00730 L730: ! read amounts from gl master file
00740 L740: read #3,using L760: fr,bb,cb,mat by,mat bp,pbp eof L850
00750   if fr=0 then goto L740
00760 L760: form pos mp1,pd 3,pos 81,28*pd 6.2,pos 327,pd 6.2
00770   if fscode=0 then goto L800
00780   if fscode<1 or fscode>12 then fscode=1
00790   if fnpriorcd=2 then cb=bp(fscode) else cb=by(fscode)
00800 L800: if fr=val(r$) then goto L810 else goto L830
00810 L810: if fnpriorcd=2 then total+=(cb-pbp) else total+=(cb-bp(12))
00820   goto L730
00830 L830: if fr<val(r$) then goto L730
00840   if fr>val(r$) then goto L860
00850 L850: notrans=1
00860 L860: fntos(sn$="ACglchgs2") !:
        mylen=30: mypos=mylen+3 : right=1
00870   fnlbl(1,1,"Description:",mylen,right)
00880   fntxt(1,mypos,50,0,right,"",0,"Enter the description if not accurate.",0 ) !:
        resp$(1)=d$
00890   fnlbl(2,1,"Total Year to Date:",mylen,right)
00900   fntxt(2,mypos,12,0,right,"10",0,"Enter the total for the year.",0 ) !:
        resp$(2)=str$(total)
00910   fncmdkey("&Next",1,1,0,"Accept the answer.")
00920   fncmdkey("&Cancel",5,0,1,"Returns to menu without posting.")
00930   fnacs(sn$,0,mat resp$,ckey)
00940   if ckey=5 then goto XIT
00950   d$=resp$(1)
00960   total=val(resp$(2))
00970 ! 
00980 L980: for j=1 to 9
00990     accum(j,1)=accum(j,1)+total
01000   next j
01010   if rs=1 then total=-total else goto L1020
01020 L1020: if ds=1 then dollar$="$" else dollar$=" "
01030   if total><0 then goto L1050
01040   if ls+ds+ul+ic>0 then goto L1050 else goto L560
01050 L1050: sp2=67-sp-1
01060   pr #255,using L1070: d$(1:sp2),dollar$,total pageoflow L1600
01070 L1070: form pos sp,c sp2,pos 67,c 1,pic(--,---,---.##),skip redir
01080   total=0
01090   gosub L1370
01100   gosub L1620
01110   gosub L1430
01120   goto L560
01130 ! ______________________________________________________________________
01140 L1140: if ap=0 then ap=1
01150   if rs=1 then accum1=-accum(ap,1) else accum1=accum(ap,1)
01160   sp2=67-sp-1
01170   if ds=1 then dollar$="$" else dollar$=" "
01180   pr #255,using L1070: d$(1:sp2),dollar$,accum1 pageoflow L1600
01190   gosub L1370
01200   gosub L1620
01210   gosub L1430
01220   goto L560
01230 ! ______________________________________________________________________
01240 L1240: if te$="R" then report$=d$
01250   if te$="S" then secondr$=d$
01260   gosub L1430
01270   goto L560
01280 ! ______________________________________________________________________
01290 L1290: if foot1=1 then goto L1340
01300   tabnote=sp
01310   foot1=1
01320   foot$=d$
01330   goto L560
01340 L1340: foot$=rtrm$(foot$)&d$
01350   goto L560
01360 ! ______________________________________________________________________
01370 L1370: for j=1 to 9
01380     if ac(j)=0 then goto L1400
01390     accum(j,1)=0
01400 L1400: next j
01410   return 
01420 ! ______________________________________________________________________
01430 L1430: if ls=0 then goto L1580
01440   if ls=99 then goto L1490
01450   pr #255,using L1460: " "
01460 L1460: form pos 1,c 1,skip ls
01470   goto L1580
01480 ! ______________________________________________________________________
01490 L1490: fnpglen(pglen)
01500 ! If PGLEN<>42 Then pGLEN=58
01510   sk=pglen-krec(255): fl=len(rtrm$(foot$))
01520 ! If PGLEN=42 Then sK=SK+1
01530   pr #255,using L1540: rtrm$(foot$),"Page "&str$(pt1)
01540 L1540: form skip sk,pos tabnote,c fl,pos 75,c 8,skip 1
01550   if eofcode=1 then goto L1580
01560   pr #255: newpage
01570   gosub L1760
01580 L1580: return 
01590 ! ______________________________________________________________________
01600 L1600: gosub L1490: continue 
01610 ! ______________________________________________________________________
01620 L1620: if ul=0 then goto L1720
01630   if ul=1 then goto L1690
01640   underlin$="=============="
01650   pr #255,using L1660: underlin$
01660 L1660: form skip 1,pos 67,c 14,skip redir
01670   goto L1720
01680 ! ______________________________________________________________________
01690 L1690: underlin$="______________"
01700   pr #255,using L1710: underlin$
01710 L1710: form pos 67,c 14,skip redir
01720 L1720: if redir=0 then pr #255,using L1730: " "
01730 L1730: form skip 1,c 1,skip 0
01740   return 
01750 ! ______________________________________________________________________
01760 L1760: heading=1
01770   pt1+=1
01780   pr #255: "\qc  {\f181 \fs24 \b "&env$('cnam')&"}"
01790   pr #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
01800   if trim$(secondr$)<>"" then pr #255: "\qc  {\f181 \fs18 \b "&trim$(secondr$)&"}"
01810   pr #255: "\qc  {\f181 \fs16 \b For the "&rtrm$(fnactpd$)&" month period ended "&rtrm$(fnpedat$)&"}"
01820   pr #255: "\ql "
01830   pr #255: 
01840   on error goto L1920
01850   a=len(rtrm$(fnpedat$))
01860   b=val(rtrm$(fnpedat$(a-4:a)))
01870   pr #255,using L1880: b
01880 L1880: form pos 72,pic(zzzz),skip 2
01890   on error system 
01900   goto L1950
01910 ! ______________________________________________________________________
01920 L1920: pr #255: tab(68);"CURRENT YEAR"
01930   on error system 
01940   pr #255: 
01950 L1950: return 
01960 ! ______________________________________________________________________
01970 L1970: eofcode=1
01980   gosub L1490
01990 ! 
02000 ! 
02010   fncloseprn
02020   goto XIT
02030 ! ______________________________________________________________________
02040 L2040: total=income
02050   goto L980
02060 XIT: fnxit
02070 ! ______________________________________________________________________
02080 ! <Updateable Region: ERTN>
02090 ERTN: fnerror(program$,err,line,act$,"xit")
02100   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
02110   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
02120   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
02130 ERTN_EXEC_ACT: execute act$ : goto ERTN
02140 ! /region
