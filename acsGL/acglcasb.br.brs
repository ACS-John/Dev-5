00010 ! Replace R:\acsGL\acglCasB
00020 ! CASH FLOW STATEMENT  WITH BUDGET
00030 ! ______________________________________________________________________
00040   library 'R:\Core\Library': fnxit,fntop, fnopenprn,fncloseprn,fnpglen,fnerror,fncno,fncch$,fnprocess,fngl_number_use_dept,fnactpd$,fnactpd,fnpedat$,fnfscode,fnpriorcd,fnps,fnglfs,fntos,fnlbl,fntxt,fncmdkey,fnacs
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim bm(13),bp(13),by(13),sc1$(2)*20,fl1$*256,in3$(4),p$(20)*50,cap$*128
00080   dim cnam$*40,b$*3,a$(8)*30,oldtrans$*16,g(8),accum(9,7),udf$*256,cch$*15
00090   dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*14
00100 ! ______________________________________________________________________
00110   let fntop(program$,cap$="Cash Flow Statement With Budget")
00120   let fncno(cno,cnam$)
00130   let udf$=env$('temp')&'\'
00135   let actpd$=fnactpd$ !:
        let actpd=fnactpd !:
        let fnfscode !:
        let fnpriorcd
00140   if fnglfs=5 then goto XIT !:
          ! sets fnps,fnpriorcd,fnfscode (primary/secondary,current year/Prior,period to print)
00142   let fscode=fnfscode !:
        let priorcd=fnpriorcd
00150   open #20: "Name=Q:\GLmstr\Company.h"&str$(cno)&",Shr",internal,input,relative  !:
        read #20,using 'Form Pos 384,n 2',rec=1: nap : close #20: 
00160   let actpd=fnactpd : let fscode=fnfscode
00170   if nap<12 or nap> 13 then let nap=12
00180   let pors=1
00190   let fnopenprn !:
        if file$(255)(1:4)<>"PRN:" then let redir=1 else let redir=0
00200   on fkey 5 goto L2170
00210   let in3$(1)="8,5,N 12.2,UT,N"
00220   let in3$(2)="8,25,N 12.2,UT,N"
00230   let in3$(3)="8,45,N 12.2,UT,N"
00240   let in3$(4)="8,65,N 12.2,UT,N"
00250   let mp1=75
00260   if fnps=2 then let mp1=mp1+3
00270   let fl1$="Name=Q:\GLmstr\ACGLFNSF.h"&str$(cno)&",KFName=Q:\GLmstr\FNSFIndx.h"&str$(cno)&",Shr"
00280   if fnps=2 then let fl1$="Name=Q:\GLmstr\ACGLFNSG.h"&str$(cno)&",KFName=Q:\GLmstr\FNSGIndx.h"&str$(cno)&",Shr"
00290   form c 7,skip 0
00300   open #1: fl1$,internal,input,keyed 
00310   if fnprocess=1 or fngl_number_use_dept=0 then goto L410
00320   let fntos(sn$="ACglcasb") !:
        let mylen=30: let mypos=mylen+3 : let right=1
00330   let fnlbl(1,1,"Cost Center or Department #:",mylen,right)
00340   let fntxt(1,mypos,3,0,right,"30",0,"Enter the cost center or department number if you wish to print only one department, else leave blank for all.",0 ) !:
        let resp$(1)=""
00350   let fnlbl(2,1,"(Blank for all Departments)",mylen,right)
00360   let fncmdkey("&Next",1,1,0,"Prints the financial statement.")
00370   let fncmdkey("&Cancel",5,0,1,"Returns to menu without posting.")
00380   let fnacs(sn$,0,mat resp$,ckey)
00390   if ckey=5 then goto XIT
00400   let costcntr=val(resp$(1))
00410 L410: let report$=cap$
00420 L420: read #1,using L460: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof L2170
00430   if ltrm$(r$)="" or ltrm$(r$)="0" then goto L420
00440   if costcntr=0 then goto L460
00450   if costcntr><fc then goto L420
00460 L460: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
00470   if te$="S" or te$="F" then goto L490
00480   if heading=0 and te$><"R" then gosub L2040
00490 L490: on pos ("RFHDTSBC",te$,1) goto L1480,L1530,L500,L550,L1320,L1480,L550,L2240 none L420
00500 L500: print #255,using L510: d$(1:40)
00510 L510: form pos sp,c 40,skip 1
00520   gosub L1730
00530   gosub L1610
00540   goto L420
00550 L550: if te$="B" and ap>0 then goto L1320 ! ENDING BANK BALANCE
00560   if notrans=1 then goto L940
00570   if ir>=val(r$) and val(r$)><0 then goto L740
00572   close #3: ioerr ignore
00574   if fnps=2 then 
00580     execute "Index Q:\GLmstr\GLmstr.h"&str$(cno)&" "&udf$&"fsindex.H"&str$(cno)&" 78 3 Replace DupKeys -N"
00590   else 
00600     execute "Index Q:\GLmstr\GLmstr.h"&str$(cno)&" "&udf$&"fsindex.H"&str$(cno)&" 75 3 Replace DupKeys -N"
00610   end if 
00620   open #3: "Name=Q:\GLmstr\GLmstr.h"&str$(cno)&",KFName="&udf$&"fsindex.h"&str$(cno)&",Shr",internal,input,keyed 
00630 L630: ! read amounts from gl master file
00640 L640: read #3,using L730: ir,bb,cb,mat by,mat bp,mat bm eof L940
00650   if ir=0 then goto L640
00660   if fscode=0 or (fscode=actpd and priorcd=1) then goto L730
00670   if fscode<1 or fscode>13 then let fscode=1
00680   if fnpriorcd=1 then let cb=by(fscode) else let cb=bp(fscode)
00690   if fnpriorcd=2 then goto L720
00700   if fscode>1 then let bb=by(fscode-1) else let bb=0
00710   goto L730
00720 L720: if fscode>1 then let bb=bp(fscode-1) else let bb=0
00730 L730: form pos mp1,pd 3,pos 81,41*pd 6.2
00740 L740: if ir=val(r$) then let total=total+(cb-bb) else goto L900
00750   if te$="B" then let total=total-(cb-bb): let total=total - bb: let total2=total2-bp(nap) : goto L770
00760   let total2=total2+cb
00770 L770: for z=1 to 13
00780     let annualb=annualb+bm(z)
00790   next z
00800   if fscode=0 then let monthb=monthb+bm(actpd) else let monthb=monthb+bm(fscode)
00810   if fscode=0 then goto L820 else goto L860
00820 L820: for j=1 to actpd
00830     let ytdb=ytdb+bm(j)
00840   next j
00850   goto L630
00860 L860: for j=1 to fscode
00870     let ytdb=ytdb+bm(j)
00880   next j
00890   goto L630
00900 L900: if ir<val(r$) then goto L630
00910   if ir>val(r$) then goto L940
00920   let notrans=1
00930   gosub L2240
00940 L940: let overundr=ytdb-total2
00950   let unexpend=annualb-total2
00960   for j=1 to 9
00970     if ac(j)=9 then goto L1050
00980     let accum(j,1)+=total
00990     let accum(j,2)+=total2
01000     let accum(j,3)=accum(j,3)+annualb
01010     let accum(j,4)=accum(j,4)+monthb
01020     let accum(j,5)=accum(j,5)+ytdb
01030     let accum(j,6)=accum(j,6)+overundr
01040     let accum(j,7)=accum(j,7)+unexpend
01050 L1050: next j
01060   if rs=1 then let total=-total else goto L1130
01070   let total2=-total2
01080   let annualb=-annualb
01090   let monthb=-monthb
01100   let ytdb=-ytdb
01110   let overundr=overundr
01120   let unexpend=unexpend
01130 L1130: if ds=1 then let dollar$="$" else let dollar$=" "
01140   if annualb><0 or total2><0 then goto L1170
01150   if total<>0 then goto L1170
01160   if ls+ds+ul+ic>0 then goto L1170 else goto L420
01170 L1170: let sp2=24-sp-1
01180   if te$="B" then let total=-total: let total2=-total2 ! REVERSE SIGN ON BEGINNING BANK BALANCE
01185   if ul=1 then print #255,using L1201: d$(1:sp2),dollar$,"{\ul ",monthb,"}",dollar$,"{\ul ",total,"}",dollar$,"{\ul ",total2,"}",dollar$,"{\ul ",annualb,"}" pageoflow L1900 : goto L1200
01190   print #255,using L1200: d$(1:sp2),dollar$,monthb,dollar$,total,dollar$,total2,dollar$,annualb pageoflow L1900
01200 L1200: form pos sp,c sp2,pos 24,c 1,pic(--,---,---.##),x 1,c 1,pic(--,---,---.##),x 1,c 1,pic(--,---,---.##),x 1,c 1,pic(--,---,---.##),skip redir
01201 L1201: form pos sp,c sp2,pos 24,c 1,c 5,pic(--,---,---.##),c 1,x 1,c 1,c 5,pic(--,---,---.##),c 1,x 1,c 1,c 5,pic(--,---,---.##),c 1,x 1,c 1,c 5,pic(--,---,---.##),c 1,skip redir
01210   let total=0
01220   let total2=0
01230   let annualb=0
01240   let monthb=0
01250   let ytdb=0
01260   let overundr=0
01270   let unexpend=0
01280   gosub L1610
01285   if ul=1 then goto L1300
01290   gosub L1910
01300 L1300: gosub L1730
01310   goto L420
01320 L1320: if ap=0 then let ap=1
01330   if rs=1 then let accum1=-accum(ap,1) else let accum1=accum(ap,1)
01340   if rs=1 then let accum2=-accum(ap,2) else let accum2=accum(ap,2)
01350   if rs=1 then let accum3=-accum(ap,3) else let accum3=accum(ap,3)
01360   if rs=1 then let accum4=-accum(ap,4) else let accum4=accum(ap,4)
01370   if rs=1 then let accum5=-accum(ap,5) else let accum5=accum(ap,5)
01380   if rs=1 then let accum6=accum(ap,6) else let accum6=accum(ap,6)
01390   if rs=1 then let accum7=accum(ap,7) else let accum7=accum(ap,7)
01400   if ds=1 then let dollar$="$" else let dollar$=" "
01410   let sp2=24-sp-1
01420   if te$="B" then let accum2=accum3=accum4=0
01425   if ul=1 then print #255,using L1201: d$(1:sp2),dollar$,"{\ul ",accum4,"}",dollar$,"{\ul ",accum1,"}",dollar$,"{\ul ",accum2,"}",dollar$,"{\ul ",accum3,"}" pageoflow L1900 : goto L1440
01430   print #255,using L1200: d$(1:sp2),dollar$,accum4,dollar$,accum1,dollar$,accum2,dollar$,accum3 pageoflow L1900
01440 L1440: gosub L1610
01445   if ul=1 then goto L1460
01450   gosub L1910
01460 L1460: gosub L1730
01470   goto L420
01480 L1480: if te$="R" then let report$=d$
01490   if te$="S" then let secondr$=d$
01500   gosub L1730
01510   goto L420
01520 ! ______________________________________________________________________
01530 L1530: if foot1=1 then goto L1590
01540   let tabnote=sp
01550   let foot1=1
01560   let foot$=d$
01570   goto L420
01580 ! ______________________________________________________________________
01590 L1590: let foot$=rtrm$(foot$)&d$ : goto L420
01600 ! ______________________________________________________________________
01610 L1610: for j=1 to 9
01620     if ac(j)=0 or ac(j)=9 then goto L1700
01630     let accum(j,1)=0
01640     let accum(j,2)=0
01650     let accum(j,3)=0
01660     let accum(j,4)=0
01670     let accum(j,5)=0
01680     let accum(j,6)=0
01690     let accum(j,7)=0
01700 L1700: next j
01710   return 
01720 ! ______________________________________________________________________
01730 L1730: if ls=0 then goto L1880
01740   if ls=99 then goto L1790
01750   print #255,using L1760: " "
01760 L1760: form pos 1,c 1,skip ls
01770   goto L1880
01780 ! ______________________________________________________________________
01790 L1790: let fnpglen(pglen)
01800 ! If PGLEN<>42 Then Let PGLEN=58
01810   let sk=pglen-krec(255): let fl=len(rtrm$(foot$))
01820 ! If PGLEN=42 Then Let SK=SK+1
01830   print #255,using L1840: rtrm$(foot$),"Page "&str$(pt1)
01840 L1840: form skip sk,pos tabnote,c fl,pos 75,c 8,skip 1
01850   if eofcode=1 then goto L1880
01860   print #255: newpage
01870   gosub L2040
01880 L1880: return 
01890 ! ______________________________________________________________________
01900 L1900: gosub L1790: continue 
01910 L1910: if ul=0 then goto L2000
01920   if ul=1 then goto L1970
01930   let underlin$="=============="
01950   goto L1980
01960   goto L2000
01970 L1970: let underlin$="______________"
01980 L1980: print #255,using L1990: underlin$,underlin$,underlin$,underlin$
01990 L1990: form pos 24,4*c 15,skip redir
02000 L2000: ! If REDIR=0 Then Print #255,Using 2010: " "
02010   form c 1,skip 1
02020   return 
02030 ! ______________________________________________________________________
02040 L2040: let heading=1
02050   let pt1+=1
02060   print #255: "\qc  {\f181 \fs24 \b "&trim$(cnam$)&"}"
02070   print #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
02080   if trim$(secondr$)<>"" then print #255: "\qc  {\f181 \fs18 \b "&trim$(secondr$)&"}"
02090   print #255: "\qc  {\f181 \fs16 \b For the "&rtrm$(fnactpd$)&" month period ended "&rtrm$(fnpedat$)&"}"
02100   print #255: "\qL "
02110   print #255: 
02115   let cch$=lpad$(rtrm$(fncch$),15)
02120   print #255: tab(31);"MONTHLY";tab(38);cch$;tab(61);"YEAR TO";tab(77);"ANNUAL"
02130   print #255: tab(32);"BUDGET";tab(44);"       ";tab(62);"DATE";tab(77);"BUDGET"
02140   print #255: 
02150   return 
02160 ! ______________________________________________________________________
02170 L2170: let eofcode=1
02180   gosub L1790
02190   let fnfscode(actpd)
02192   let fnpriorcd(1)
02210   let fncloseprn(nw)
02220   goto XIT
02230 ! ______________________________________________________________________
02240 L2240: let fntos(sn$="ACglchgs2") !:
        let mylen=30: let mypos=mylen+3 : let right=1
02250   let fnlbl(1,1,d$,mylen,right)
02260   let fnlbl(3,1,"Monthy Budget:",mylen,right)
02270   let fntxt(3,mypos,12,0,right,"10",0,"Enter the monthly budget.",0 ) !:
        let resp$(1)=str$(monthb)
02280   let fnlbl(4,1,"Total for the Month:",mylen,right)
02290   let fntxt(4,mypos,12,0,right,"10",0,"Enter the total for the month.",0 ) !:
        let resp$(2)=str$(total)
02300   let fnlbl(5,1,"Total Year to Date:",mylen,right)
02310   let fntxt(5,mypos,12,0,right,"10",0,"Enter the total for the year.",0 ) !:
        let resp$(3)=str$(total2)
02320   let fnlbl(6,1,"Total Budget for the Year:",mylen,right)
02330   let fntxt(6,mypos,12,0,right,"10",0,"Enter the total budget for the  year.",0 ) !:
        let resp$(4)=str$(annualb)
02340   let fncmdkey("&Next",1,1,0,"Accept the answer.")
02350   let fncmdkey("&Cancel",5,0,1,"Returns to menu without posting.")
02360   let fnacs(sn$,0,mat resp$,ckey)
02370   if ckey=5 then goto XIT
02380   let monthb=val(resp$(1))
02390   let total=val(resp$(2))
02400   let total2=val(resp$(3))
02410   let annualb=val(resp$(4))
02420   goto L940
02430 XIT: let fnxit
02440 IGNORE: continue 
02450 ! <Updateable Region: ERTN>
02460 ERTN: let fnerror(cap$,err,line,act$,"xit")
02470   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
02480   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
02490   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
02500 ERTN_EXEC_ACT: execute act$ : goto ERTN
02510 ! /region
02520 ! ______________________________________________________________________
