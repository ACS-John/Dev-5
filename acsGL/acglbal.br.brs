00010 ! Replace R:\acsGL\acglBal
00020 ! Balance Sheet !:
        ! Standard 8.5x11
00030 ! ______________________________________________________________________
00040   library 'R:\Core\Library': fntop,fnxit, fnxit,fntop,fnopenprn,fncloseprn,fncno,fnerror,fnprocess,fnchain,fnpedat$,fnpriorcd,fnps,fnfscode,fngl_number_use_dept,fnglfs,fnpglen
00050   let fntop(program$,cap$="Balance Sheet")
00060   on error goto ERTN
00070 ! ______________________________________________________________________
00080   dim fl1$*256,cap$*128
00090   dim cnam$*40,b$*3,a$(8)*30,oldtrans$*16,g(8),d(2),by(13),bp(13),udf$*256
00100   dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*14
00110 ! ______________________________________________________________________
00120   let fncno(cno,cnam$)
00130   let udf$=env$('temp')&'\'
00140   if fnglfs=5 then goto XIT !:
          ! sets fnps,fnpriorcd,fnfscode (primary/secondary,current year/Prior,period to print)
00150   if fnps=2 then let mp1=66 !:
          let fl1$="Name=Q:\GLmstr\acglFnSC.h"&str$(cno) !:
          let fl1$=fl1$&",KFName=Q:\GLmstr\fnSCIndx.h"&str$(cno)&",Shr" else !:
          let mp1=63 !:
          let fl1$="Name=Q:\GLmstr\acglFnSB.h"&str$(cno) !:
          let fl1$=fl1$&",KFName=Q:\GLmstr\FNSBIndx.h"&str$(cno)&",Shr"
00160   open #1: fl1$,internal,input,keyed 
00170   if fnprocess=1 or fngl_number_use_dept=0 then goto L270
00180 ! ______________________________________________________________________
00190   print newpage
00200   close #101: ioerr L210
00210 L210: open #101: "SROW=9,SCOL=4,EROW=11,ECOL=75,BORDER=DR,CAPTION=<"&cap$,display,outin 
00220   print fields "12,29,C 10,B,1": "Print (F1)" !:
        print fields "12,40,C 12,B,99": "Cancel (Esc)"
00230   print fields "10,5,C 49,N": "Cost Center or Department Number (blank for all):"
00240 L240: input fields "10,55,Nz 3,UT,N": costcntr conv L240
00250   if cmdkey=5 or cmdkey=99 then goto XIT
00260 ! ______________________________________________________________________
00270 L270: print newpage !:
        print fields "10,20,Cc 40,N": "Printing Balance Sheet..." !:
        print fields "12,34,C 18,B,99": "Cancel (Esc)"
00280   on fkey 99 goto DONE
00290   if fnps=2 then goto L320 ! secondary
00300   execute "Index Q:\GLmstr\GLmstr.h"&str$(cno)&" "&udf$&"fsindex.H"&str$(cno)&" 63 3 Replace DupKeys -N"
00310   goto L330
00320 L320: execute "Index Q:\GLmstr\GLmstr.h"&str$(cno)&" "&udf$&"fsindex.H"&str$(cno)&" 66 3 Replace DupKeys -N"
00330 L330: open #3: "Name=Q:\GLmstr\GLmstr.h"&str$(cno)&",KFName="&udf$&"fsindex.h"&str$(cno)&",Shr",internal,input,keyed 
00340   let fnopenprn
00350   if file$(255)(1:4)<>"PRN:" then let redir=1 else let redir=0
00360   let report$="Balance Sheet"
00370 READ_TOP: ! 
00380   read #1,using L390: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof DONE
00390 L390: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
00400   if ltrm$(r$)="" or ltrm$(r$)="0" then goto READ_TOP
00410   if costcntr=0 then goto L430
00420   if costcntr><fc then goto READ_TOP
00430 L430: if te$="S" or te$="F" then goto L450
00440   if heading=0 and te$><"R" then gosub HEADER
00450 L450: on pos ("RFHDTSPE",te$,1) goto L980,L1020,L470,L530,L850,L980,L850,L530 none READ_TOP
00460 ! ______________________________________________________________________
00470 L470: print #255,using L480: d$
00480 L480: form pos sp,c 50,skip 1
00490   gosub FOOTER
00500   gosub L1110
00510   goto READ_TOP
00520 ! ______________________________________________________________________
00530 L530: if notrans=1 then goto L670
00540   if br>=val(r$) and val(r$)><0 then goto L620
00550 L550: ! read general ledger master file for amounts
00560   form pd 3
00570 L570: read #3,using 'Form POS MP1,PD 3,POS 87,27*PD 6.2': br,cb,mat by,mat bp eof L660
00580   if br=0 then goto L570
00590   if fnfscode=0 then goto L620
00600   if fnfscode<1 or fnfscode>12 then let fnfscode(1)
00610   if fnpriorcd=1 then let cb=by(fnfscode) else let cb=bp(fnfscode)
00620 L620: if br=val(r$) then let total=total+cb else goto L640
00630   goto L550
00640 L640: if br<val(r$) then goto L550
00650   if br>val(r$) then goto L670
00660 L660: let notrans=1
00670 L670: if te$="E" then let total=-accum(ap)
00680   for j=1 to 9
00690     if ac(j)=9 then goto L700 else let accum(j)=accum(j)+total
00700 L700: next j
00710   if rs=1 then let total=-total
00720   if ds=1 then let dollar$="$" else let dollar$=" "
00730   if cp=1 then let dollar=50+14*bc else let dollar=24+14*bc
00740   if total><0 then goto L760
00750   if ls+ul+ds+ic>0 then goto L760 else goto READ_TOP
00760 L760: let sp2=dollar-sp-1
00770   print #255,using L780: d$(1:sp2),dollar$,total pageoflow PGOF
00780 L780: form pos sp,c sp2,pos dollar,c 1,pic(---,---,---.##),skip redir
00790   let total=0
00800   gosub L1110
00810   gosub UNDERLINE
00820   gosub FOOTER
00830   goto READ_TOP
00840 ! ______________________________________________________________________
00850 L850: if ap=0 then let ap=1
00860   if rs=1 then let accum1=-accum(ap) else let accum1=accum(ap)
00870   if ds=1 then let dollar$="$" else let dollar$=" "
00880   if cp=1 then let dollar=50+14*bc else let dollar=24+14*bc
00890   let sp2=dollar-sp-1
00900   print #255,using L780: d$(1:sp2),dollar$,accum1 pageoflow PGOF
00910   gosub L1110
00920   gosub UNDERLINE
00930   gosub FOOTER
00940   if te$><"P" then goto L960
00950   for j=1 to 9 !:
          let accum(j)=accum(j)-accum(ap) !:
        next j
00960 L960: goto READ_TOP
00970 ! ______________________________________________________________________
00980 L980: if te$="R" then let report$=d$
00990   if te$="S" then let secondr$=d$
01000   gosub FOOTER
01010   goto READ_TOP
01020 L1020: if foot1=1 then goto L1080
01030   let tabnote=sp
01040   let foot1=1
01050   let foot$=d$
01060   goto READ_TOP
01070 ! ______________________________________________________________________
01080 L1080: let foot$=rtrm$(foot$)&d$
01090   goto READ_TOP
01100 ! ______________________________________________________________________
01110 L1110: for j=1 to 9
01120     if ac(j)=0 or ac(j)=9 then goto L1130 else let accum(j)=0
01130 L1130: next j
01140   return 
01150 ! ______________________________________________________________________
01160 FOOTER: ! 
01170   if ls=0 then goto L1310
01180   if ls=99 then goto L1220
01190   print #255,using L1200: " "
01200 L1200: form pos 1,c 1,skip ls
01210   goto L1310
01220 L1220: let fnpglen(pglen)
01230 ! If PGLEN<>42 Then Let PGLEN=58
01240   let sk=pglen-krec(255): let fl=len(rtrm$(foot$))
01250 ! If PGLEN=42 Then Let SK+=1
01260   print #255,using L1270: rtrm$(foot$)
01270 L1270: form skip sk,pos tabnote,c fl,skip 1
01280   if eofcode=1 then goto L1310
01290   print #255: newpage
01300   gosub HEADER
01310 L1310: return 
01320 ! ______________________________________________________________________
01330 PGOF: ! 
01340   gosub L1220
01350   continue 
01360 ! ______________________________________________________________________
01370 UNDERLINE: ! 
01380   if ul=0 then goto L1480
01390   if cp=1 then let underlin=51+14*bc else let underlin=25+14*bc
01400   if ul=1 then goto L1450
01410   let underlin$="=============="
01420   print #255,using L1430: underlin$
01430 L1430: form skip 1,pos underlin,c 14,skip redir
01440   goto L1480
01450 L1450: let underlin$="______________"
01460   print #255,using L1470: underlin$
01470 L1470: form pos underlin,c 14,skip redir
01480 L1480: if redir=0 then print #255,using L1490: " "
01490 L1490: form skip 1,c 1,skip 0
01500   return 
01510 ! ______________________________________________________________________
01520 HEADER: ! 
01530   let heading=1
01540   print #255,using L1550: cnam$
01550 L1550: form pos 20,cc 40,skip 1
01560   print #255,using L1570: rtrm$(report$)
01570 L1570: form pos 15,cc 50
01580   if rtrm$(secondr$)="" then goto L1600
01590   print #255,using L1570: rtrm$(secondr$)
01600 L1600: print #255,using L1570: trim$(fnpedat$)
01610   print #255: ""
01620   return 
01630 ! ______________________________________________________________________
01640 DONE: ! 
01650   let eofcode=1
01660   gosub L1220
01670   if pors=2 then goto L1680 else goto L1690
01680 L1680: let fnxit
01690 L1690: let fncloseprn
01700   goto XIT
01710 ! ______________________________________________________________________
01720 ! <Updateable Region: ERTN>
01730 ERTN: let fnerror(cap$,err,line,act$,"xit")
01740   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
01750   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01760   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
01770 ERTN_EXEC_ACT: execute act$ : goto ERTN
01780 ! /region
01790 ! ______________________________________________________________________
01800 XIT: let fnchain("R:\acsGL\acglAuto")
01810 ! ______________________________________________________________________
