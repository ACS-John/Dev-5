00020 ! -- INCOME STATEMENT WITH YEAR TO DATE COLUMN ONLY
00030 ! ______________________________________________________________________
00040   library 'R:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnpglen,fnerror,fnprocess,fncno,fngl_number_use_dept,fnpedat$,fnps,fnpriorcd,fnfscode,fnactpd$,fncch$,fnglfs,fnwait,fnactpd,fntos,fnlbl,fntxt,fncmdkey,fnacs
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim fl1$*256,actpd$*6,cogl$(3)*12,pedat$*20,cch$*20
00080   dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*14
00090   dim cnam$*40,b$*3,a$(8)*30,oldtrans$*16,g(8),accum(9,2)
00100   dim by(13),bp(13),cap$*128,message$*40,cap$*128,udf$*256
00110 ! ______________________________________________________________________
00120   let fntop(program$,cap$="Income Statement showing YTD Only")
00130   let fncno(cno,cnam$)
00140   let udf$=env$('temp')&'\'
00145   let fnfscode
00146   let fnpriorcd
00150   if fnglfs=5 then goto XIT !:
          ! sets fnps,fnpriorcd,fnfscode (primary/secondary,current year/Prior,period to print)
00155   let fscode=fnfscode
00156   let priorcd=fnpriorcd
00160   let cch$=fncch$
00170   let pedat$=fnpedat$
00180   let actpd$=fnactpd$
00190   let actpd=fnactpd
00200   let priorcd=fnpriorcd
00210 ! ______________________________________________________________________
00220   let mp1=69
00230   let fl1$="Name=Q:\GLmstr\ACGLFNSI.h"&str$(cno)&",KFName=Q:\GLmstr\FNSIIndx"
00240   if fnps=2 then let mp1=mp1+3 !:
          let fl1$="Name=Q:\GLmstr\ACGLFNSJ.h"&str$(cno)&",KFName=Q:\GLmstr\FNSJIndx"
00250   open #1: fl1$&".H"&str$(cno)&",Shr",internal,input,keyed 
00260   if fnprocess=1 or fngl_number_use_dept=0 then goto L370
00270 ! ______________________________________________________________________
00280   let fntos(sn$="ACglinc1") !:
        let mylen=30: let mypos=mylen+3 : let right=1
00290   let fnlbl(1,1,"Cost Center or Department #:",mylen,right)
00300   let fntxt(1,mypos,3,0,right,"30",0,"Enter the cost center or department number if you wish to print only one department, else leave blank for all.",0 ) !:
        let resp$(1)=""
00310   let fnlbl(2,1,"(Blank for all Departments)",mylen,right)
00320   let fncmdkey("&Next",1,1,0,"Prints the financial statement.")
00330   let fncmdkey("&Cancel",5,0,1,"Returns to menu without posting.")
00340   let fnacs(sn$,0,mat resp$,ckey)
00350   if ckey=5 then goto XIT
00360   let costcntr=val(resp$(1))
00370 L370: on fkey 5 goto L1760
00380   let fnopenprn
00390   if file$(255)(1:4)<>"PRN:" then let redir=1 else let redir=0
00400   let report$="Statement of Income and Expenses" ! cap$
00410   if fnps=2 then goto L440 ! secondary
00420   execute "Index Q:\GLmstr\GLmstr.h"&str$(cno)&" "&udf$&"fsindex.H"&str$(cno)&" 69 3 Replace DupKeys -N"
00430   goto L450
00440 L440: execute "Index Q:\GLmstr\GLmstr.h"&str$(cno)&" "&udf$&"fsindex.H"&str$(cno)&" 72 3 Replace DupKeys -N"
00450 L450: open #3: "Name=Q:\GLmstr\GLmstr.h"&str$(cno)&",KFName="&udf$&"fsindex.h"&str$(cno)&",Shr",internal,input,keyed 
00460 L460: read #1,using L510: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof L1760
00470   if ltrm$(r$)="" or ltrm$(r$)="0" then goto L460
00480   if costcntr=0 then goto L510
00490   if fc=0 and te$="F" then goto L520
00500   if costcntr><fc then goto L460
00510 L510: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
00520 L520: if te$="S" or te$="F" then goto L540
00530   if heading=0 and te$><"R" then gosub L1640
00540 L540: on pos ("RFHDTS",te$,1) goto L1130,L1180,L560,L620,L1020,L1130 none L460
00550 ! ______________________________________________________________________
00560 L560: print #255,using L570: d$(1:40)
00570 L570: form pos sp,c 40,skip 1
00580   gosub L1320
00590   gosub L1250
00600   goto L460
00610 ! ______________________________________________________________________
00620 L620: if notrans=1 then goto L820
00630   if ir=val(r$) and val(r$)><0 then goto L760
00640   if ir>val(r$) then goto L760
00650 L650: ! read amounts from gl master file
00660 L660: read #3,using L680: ir,bb,cb,mat by,mat bp eof L810
00670   if ir=0 then goto L660
00680 L680: form pos mp1,pd 3,pos 81,41*pd 6.2
00690   if fscode=0 or (fscode=actpd and fnpriorcd=1) then goto L760
00700   if fscode<1 or fscode>13 then let fscode=1
00710   if priorcd=1 then let cb=by(fscode) else let cb=bp(fscode)
00720   if priorcd=2 then goto L750
00730   if fscode>1 then let bb=by(fscode-1) else let bb=0
00740   goto L760
00750 L750: if fscode>1 then let bb=bp(fscode-1) else let bb=0
00760 L760: if ir=val(r$) then let total=total+(cb-bb) else goto L790
00770   let total2=total2+cb
00780   goto L650
00790 L790: if ir<val(r$) then goto L650
00800   if ir>val(r$) then goto L820
00810 L810: let notrans=1
00820 L820: for j=1 to 9
00830     if ac(j)=9 then goto L860
00840     let accum(j,1)=accum(j,1)+total
00850     let accum(j,2)=accum(j,2)+total2
00860 L860: next j
00870   if rs=1 then let total=-total else goto L890
00880   let total2=-total2
00890 L890: if ds=1 then let dollar$="$" else let dollar$=" "
00900   if total><0 or total2><0 then goto L920
00910   if ls+ul+ds+ic>0 then goto L920 else goto L460
00920 L920: let sp2=49-sp-1
00925   if ul=1 then print #255,using L941: d$(1:sp2),dollar$,"{\UL ",total2,"}" pageoflow L1490 : goto L940
00930   print #255,using L940: d$(1:sp2),dollar$,total2 pageoflow L1490
00940 L940: form pos sp,c sp2,pos 67,c 1,pic(--,---,---.##),skip redir
00941 L941: form pos sp,c sp2,pos 67,c 1,c 5,pic(--,---,---.##),c 1,skip redir
00950   let total=0
00960   let total2=0
00970   gosub L1250
00975   if ul=1 then goto L990
00980   gosub L1500
00990 L990: gosub L1320
01000   goto L460
01010 ! ______________________________________________________________________
01020 L1020: if ap=0 then let ap=1
01030   if rs=1 then let accum1=-accum(ap,1) else let accum1=accum(ap,1)
01040   if rs=1 then let accum2=-accum(ap,2) else let accum2=accum(ap,2)
01050   if ds=1 then let dollar$="$" else let dollar$=" "
01060   let sp2=49-sp-1
01065   print #255,using L941: d$(1:sp2),dollar$,"{\UL ",accum2,"}" pageoflow L1490: goto L1080
01070   print #255,using L940: d$(1:sp2),dollar$,accum2 pageoflow L1490
01080 L1080: gosub L1250
01085   if ul=1 then goto L1100
01090   gosub L1500
01100 L1100: gosub L1320
01110   goto L460
01120 ! ______________________________________________________________________
01130 L1130: if te$="R" then let report$=d$
01140   if te$="S" then let secondr$=d$
01150   gosub L1320
01160   goto L460
01170 ! ______________________________________________________________________
01180 L1180: if foot1=1 then goto L1230
01190   let tabnote=sp
01200   let foot1=1
01210   let foot$=d$
01220   goto L460
01230 L1230: let foot$=rtrm$(foot$)&d$
01240   goto L460
01250 L1250: for j=1 to 9
01260     if ac(j)=0 or ac(j)=9 then goto L1290
01270     let accum(j,1)=0
01280     let accum(j,2)=0
01290 L1290: next j
01300   return 
01310 ! ______________________________________________________________________
01320 L1320: if ls=0 then goto L1470
01330   if ls=99 then goto L1380
01340   print #255,using L1350: " "
01350 L1350: form pos 1,c 1,skip ls
01360   goto L1470
01370 ! ______________________________________________________________________
01380 L1380: let fnpglen(pglen)
01390 ! If PGLEN<>42 Then Let PGLEN=58
01400   let sk=pglen-krec(255): let fl=len(rtrm$(foot$))
01410 ! If PGLEN=42 Then Let SK=SK+1
01420   print #255,using L1430: rtrm$(foot$),"Page "&str$(pt1)
01430 L1430: form skip sk,pos tabnote,c fl,pos 75,c 8,skip 1
01440   if eofcode=1 then goto L1470
01450   print #255: newpage
01460   gosub L1640
01470 L1470: return 
01480 ! ______________________________________________________________________
01490 L1490: gosub L1380: continue 
01500 L1500: if ul=0 then goto L1600
01510   if ul=1 then goto L1570
01520   let underlin$="=============="
01530   print #255,using L1540: underlin$
01540 L1540: form pos 67,c 14,skip redir
01550   goto L1600
01560 ! ______________________________________________________________________
01570 L1570: let underlin$="______________"
01580   print #255,using L1590: underlin$
01590 L1590: form skip redir,pos 67,c 14,skip redir
01600 L1600: if redir=0 then print #255,using L1610: " "
01610 L1610: form skip 1,c 1,skip 0
01620   return 
01630 ! ______________________________________________________________________
01640 L1640: let heading=1
01650   let pt1+=1
01660   print #255: "\qc  {\f181 \fs24 \b "&trim$(cnam$)&"}"
01670   print #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
01680   if trim$(secondr$)<>"" then print #255: "\qc  {\f181 \fs18 \b "&trim$(secondr$)&"}"
01690   print #255: "\qc  {\f181 \fs16 \b For the "&rtrm$(actpd$)&" month period ended "&rtrm$(fnpedat$)&"}"
01700   print #255: "\qL "
01710   print #255: 
01720   print #255,using L1730: "Year To Date"
01730 L1730: form pos 69,c 12,skip 2
01740   return 
01750 ! ______________________________________________________________________
01760 L1760: let eofcode=1
01770   gosub L1380
01780   let fncloseprn
01781   let fnfscode(actpd)
01782   let fnpriorcd(1)
01790 ! 
01800   goto XIT
01810 ! ______________________________________________________________________
01820 XIT: let fnxit
01830 ! ______________________________________________________________________
01840 ! <updateable region: ertn>
01850 ERTN: let fnerror(cap$,err,line,act$,"xit")
01860   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
01870   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01880   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
01890 ERTN_EXEC_ACT: execute act$ : goto ERTN
01900 ! /region
