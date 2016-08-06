00010 ! Replace R:\acsGL\AcGLBalC
00020 ! Comparative Balance Sheet
00030 ! ______________________________________________________________________
00040   library 'R:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnpglen,fncno,fnerror,fnprocess,fnpedat$,fnps,fnpriorcd,fnfscode,fngl_number_use_dept,fnactpd,fnglfs,fntos,fnlbl,fntxt,fncmdkey,fnacs,fnactpd$
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim fl1$*256,cogl$(3)*12,accum(9,2),bp(13),by(13)
00080   dim cnam$*40,cap$*128,udf$*256
00090   dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*56
00100 ! ______________________________________________________________________
00110   let fntop(program$,cap$="Comparative Balance Sheet")
00120   let fncno(cno,cnam$)
00130   let udf$=env$('temp')&'\'
00135   let actpd$=fnactpd$ !:
        let actpd=fnactpd !:
        let fnfscode !:
        let fnpriorcd
00140   if fnglfs=5 then goto XIT ! sets fnps,fnpriorcd,fnfscode (primary/secondary,current year/Pprior,period to print)
00146   let fnfscode
00147   let fnpriorcd
00150   print newpage
00160   if fnps=2 then let mp1=66 !:
          let fl1$="Name=Q:\GLmstr\AcGLFnSc.h"&str$(cno)&"," !:
          let fl1$=fl1$&"KFName=Q:\GLmstr\FnScIndx.h"&str$(cno)&",Shr" else !:
          let mp1=63 !:
          let fl1$="Name=Q:\GLmstr\ACGLFNSB.h"&str$(cno)&"," !:
          let fl1$=fl1$&"KFName=Q:\GLmstr\FnSBIndx.h"&str$(cno)&",Shr"
00170   if actpd>0 and actpd<13 then goto L230
00180   print newpage
00190   print fields "10,2,C 78,N": "THIS PROGRAM CANNOT PROCESS WITHOUT THE NUMBER OF THE ACCOUNTING MONTH END"
00200   print fields "12,2,c 78,n": "USE OPTION 1 ON THE CURRENT PERIOD PROCESSING MENU TO ENTER THIS INFORMATION"
00210   input fields "23,2,c 1,e,n": pause$
00220   goto XIT
00230 L230: open #1: fl1$,internal,input,keyed 
00240   if fnprocess=1 or fngl_number_use_dept=0 then goto L320
00250   let fntos(sn$="Acglbalc") !:
        let mylen=30: let mypos=mylen+3 : let right=1
00260   let fnlbl(1,1,"Cost Center or Department #:",mylen,right)
00270   let fntxt(1,mypos,3,0,right,"30",0,"Enter the cost center or department number if you wish to print only one department, else leave blank for all.",0 ) !:
        let resp$(1)=""
00272   let fnlbl(2,1,"(Blank for all Departments)",mylen,right)
00280   let fncmdkey("&Next",1,1,0,"Prints the financial statement.")
00290   let fncmdkey("&Cancel",5,0,1,"Returns to menu without posting.")
00300   let fnacs(sn$,0,mat resp$,ckey)
00310   if ckey=5 then goto XIT
00320 L320: let costcntr=val(resp$(1))
00350   on fkey 5 goto L1660
00360   if fnps=2 then goto L390 ! secondary
00370   execute "Index Q:\GLmstr\GLmstr.h"&str$(cno)&" "&udf$&"\fsindex.H"&str$(cno)&" 63 3 Replace DupKeys -N"
00380   goto L400
00390 L390: execute "Index Q:\GLmstr\GLmstr.h"&str$(cno)&" "&udf$&"\fsindex.H"&str$(cno)&" 66 3 Replace DupKeys -N"
00400 L400: open #3: "Name=Q:\GLmstr\GLmstr.h"&str$(cno)&",KFName="&udf$&"fsindex.h"&str$(cno)&",Shr",internal,input,keyed 
00410   let fnopenprn !:
        if file$(255)(1:4)<>"PRN:" then let redir=1 else let redir=0
00420   let report$=cap$
00430 L430: read #1,using L470: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof L1660
00440   if ltrm$(r$)="" or ltrm$(r$)="0" then goto L430
00450   if costcntr=0 then goto L470
00460   if costcntr><fc then goto L430
00470 L470: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
00480   if te$="S" or te$="F" then goto L500
00490   if heading=0 and te$><"R" then gosub L1520
00500 L500: on pos ("RFHDTSPE",te$,1) goto L1030,L1060,L510,L560,L890,L1030,L890,L560 none L430
00510 L510: print #255,using L520: d$
00520 L520: form pos sp,c 50,skip 1
00530   gosub L1180
00540   gosub L1120
00550   goto L430
00560 L560: if notrans=1 then goto L710
00570   if br>=val(r$) and val(r$)><0 then goto L650
00580 L580: ! read general ledger master file for amounts
00590 L590: read #3,using 'Form POS MP1,PD 3,POS 87,27*PD 6.2': br,cb,mat by,mat bp eof L700
00600   if br=0 then goto L590
00610   if fnfscode=0 or (fnfscode=actpd and fnpriorcd=1) then goto L650
00620   if fnfscode<1 or fnfscode>13 then let fnfscode(1)
00630   if fnpriorcd=1 then let cb=by(fnfscode)
00650 L650: if br=val(r$) then let total=total+cb else goto L680
00660   let total2+=bp(fnfscode)
00670   goto L580
00680 L680: if br<val(r$) then goto L580
00690   if br>val(r$) then goto L710
00700 L700: let notrans=1
00710 L710: if te$="E" then let total=-accum(ap,1) : let total2=-accum(ap,2)
00720   for j=1 to 9
00730     if ac(j)<>9 then !:
            let accum(j,1)=accum(j,1)+total : let accum(j,2)=accum(j,2)+total2
00740   next j
00750   if rs=1 then let total=-total : let total2=-total2
00760   if ds=1 then let dollar$="$" else let dollar$=" "
00770   let dollar=24+14*bc
00780   if total><0 or total2><0 then goto L800
00790   if ls+ul+ds+ic>0 then goto L800 else goto L430
00800 L800: let sp2=dollar-sp-1
00805   if ul=1 then print #255,using L816: d$(1:sp2),dollar$,"{\ul ",total,"}",dollar$,"{\ul ",total2,"}" pageoflow L1340 : goto L830
00810   print #255,using L820: d$(1:sp2),dollar$,total,dollar$,total2 pageoflow L1340
00816 L816: form pos sp,c sp2,pos dollar,c 1,c 5,pic(--,---,---.##),c 1,x 28,c 1,c 5,pic(--,---,---.##),c 1,skip redir
00820 L820: form pos sp,c sp2,pos dollar,c 1,pic(--,---,---.##),x 28,c 1,pic(--,---,---.##),skip redir
00830 L830: let total=0
00840   let total2=0
00850   gosub L1120
00855   if ul=1 then goto L870
00860   gosub L1370
00870 L870: gosub L1180
00880   goto L430
00890 L890: if ap=0 then let ap=1
00900   if rs=1 then let accum1=-accum(ap,1) else let accum1=accum(ap,1)
00910   if rs=1 then let accum2=-accum(ap,2) else let accum2=accum(ap,2)
00920   if ds=1 then let dollar$="$" else let dollar$=" "
00930   let dollar=24+14*bc
00940   let sp2=dollar-sp-1
00945   if ul=1 then print #255,using L816: d$(1:sp2),dollar$,"{\ul ",accum1,"}",dollar$,"{\ul ",accum2,"}" pageoflow L1340 : goto L960
00950   print #255,using L820: d$(1:sp2),dollar$,accum1,dollar$,accum2 pageoflow L1340
00960 L960: gosub L1120
00965   if ul=1 then goto L980
00970   gosub L1370
00980 L980: gosub L1180
00990   if te$><"P" then goto L1010
01000   for j=1 to 9 !:
          let accum(j,1)=accum(j,1)-accum(ap,1) !:
          let accum(j,2)=accum(j,2)-accum(ap,2) !:
        next j
01010 L1010: goto L430
01020 ! ______________________________________________________________________
01030 L1030: if te$="R" then let report$=d$ else !:
          if te$="S" then let secondr$=d$
01040   gosub L1180
01050   goto L430
01060 L1060: if foot1=1 then goto L1100
01070   let tabnote=sp : let foot1=1 : let foot$=d$
01080   goto L430
01090 ! ______________________________________________________________________
01100 L1100: let foot$=rtrm$(foot$)&d$ !:
        goto L430
01110 ! ______________________________________________________________________
01120 L1120: for j=1 to 9
01130     if ac(j)=0 or ac(j)=9 then goto L1150
01140     let accum(j,1)=0 : let accum(j,2)=0
01150 L1150: next j
01160   return 
01170 ! ______________________________________________________________________
01180 L1180: if ls=0 then goto L1320
01190   if ls=99 then goto L1230
01200   print #255,using L1210: " "
01210 L1210: form pos 1,c 1,skip ls
01220   goto L1320
01230 L1230: let fnpglen(pglen)
01240 ! If PGLEN<>42 Then Let PGLEN=58
01250   let sk=pglen-krec(255): let fl=len(rtrm$(foot$))
01260 ! If PGLEN=42 Then Let SK=SK+1
01270   if trim$(foot$)<>'' then print #255,using L1280: rtrm$(foot$)
01280 L1280: form skip sk,pos tabnote,c fl,skip 1
01290   if eofcode=1 then goto L1320
01300   print #255: newpage
01310   gosub L1520
01320 L1320: return 
01330 ! ______________________________________________________________________
01340 L1340: gosub L1230
01350   continue 
01360 ! ______________________________________________________________________
01370 L1370: if ul=0 then goto L1480
01380   let underlin=24+14*bc
01390   if ul=1 then goto L1450
01400   let underlin$="==============                            =============="
01410   print #255,using L1420: underlin$
01420 L1420: form pos underlin,c 56,skip redir
01430   goto L1480
01440 ! ______________________________________________________________________
01450 L1450: let underlin$="______________                            ______________"
01460   print #255,using L1470: underlin$
01470 L1470: form pos underlin,c 56,skip redir
01480 L1480: if redir=0 then print #255,using L1490: " "
01490 L1490: form skip 1,c 1,skip redir
01500   return 
01510 ! ______________________________________________________________________
01520 L1520: let heading=1
01521   print #255: "\qc  {\f181 \fs24 \b "&trim$(cnam$)&"}"
01524   print #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
01525   if trim$(secondr$)<>"" then print #255: "\qc  {\f181 \fs18 \b "&trim$(secondr$)&"}"
01526   print #255: "\qc  {\f181 \fs16 \b "&trim$(fnpedat$)&"}"
01527   print #255: "\qL "
01590   print #255: ""
01600   print #255: "{\f181                                                                                                                             Current Year                                                                    Prior Year }"
01610   print #255,using L1630: "__________________________________________"," _________________________________________"
01620   if redir=0 then print #255: ""
01630 L1630: form pos 38,cc 42,x 1,cc 42,skip redir
01640   return 
01650 ! ______________________________________________________________________
01660 L1660: let eofcode=1
01670   gosub L1230
01677   let fnfscode(actpd)
01678   let fnpriorcd(1)
01680   let fncloseprn(nw)
01690   goto XIT
01700 ! ______________________________________________________________________
01710 XIT: let fnxit
01720 ! ______________________________________________________________________
01730 ! <Updateable Region: ERTN>
01740 ERTN: let fnerror(cap$,err,line,act$,"xit")
01750   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
01760   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01770   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
01780 ERTN_EXEC_ACT: execute act$ : goto ERTN
01790 ! /region
