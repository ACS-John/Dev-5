00010 ! Replace S:\acsPR\newprRpt4
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnwait,fnopenprn,fncloseprn,fncno,fnerror,fntop,fnxit
00050   on error goto ERTN
00060 ! 
00070   dim rt$*78,ch$(2)*132,psc(100),inp(20),pp(20),ti(20),cnam$*40
00080   dim message$*40,cap$*128
00090 ! ______________________________________________________________________
00100   fntop(program$,cap$="User Designed Reports Proof List")
00110   fncno(cno,cnam$)
00120   open #1: "Name="&env$('Q')&"\PRmstr\PRReport.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\prrptidx.h"&env$('cno')&",Shr",internal,input,keyed 
00130   fnopenprn
00160 ! ______________________________________________________________________
00170   do 
00180     read #1,using 'form pos 1,n 2,c 78,2*c 132,n 3,2*n 1,100*pd 6.3,40*pd 2,20*n 1': rn,rt$,mat ch$,ips,sd,cp,mat psc,mat inp,mat pp,mat ti eof L560
00190     pr #255,using "form pos 1,c 25": "Page "&str$(pgno+=1)&" "&date$
00200     pr #255: "\qc  {\f221 \fs22 \b "&env$('cnam')&"}"
00210     pr #255: "\qc  {\f201 \fs20 \b "&env$('program_caption')&"}"
00220     pr #255: "\qc  {\f181 \fs16 \b "&trim$(dat$)&"}"
00230     pr #255: "\ql   "
00240 ! form skip 2,pos 1,c 8,pos nametab,c 40,skip 1,pos 1,c 8,pos 51,c 30,skip 2
00250     pr #255,using 'form pos 1,c 13,pos 20,pic(zz)': "Report Number",rn
00270     pr #255,using 'form pos 1,c 12,pos 20,c 78': "Report Title",rt$
00290     pr #255,using 'form pos 1,c 15,skip 2,c 132': "Column Headings",ch$(1)
00310     pr #255,using 'form pos 1,c 132': ch$(2)
00312     pr #255: ''
00330     pr #255,using 'form pos 1,c 25,pos 30,pic(zz#)': "Item Number for pr Sel",ips
00350     pr #255,using 'form pos 1,c 26,pos 32,pic(#)': "Summarize Departments",sd
00370 ! pr #255,Using 360: "Use Condensed Print",CP
00380     for j=1 to 100
00390       if psc(j)=0 then goto L490
00400       if j><48 then goto L450
00410       pr #255: newpage
00412       pr #255: ''
00414       pr #255: ''
00420       pr #255,using L460: "Print Selection Criteria",psc(j)
00440       goto L470
00450 L450: ! 
00452       pr #255,using L460: "PRINT SELECTION CRITERIA",psc(j)
00460 L460: form pos 1,c 24,pos 30,pic(---------.###)
00470 L470: ! 
00472     next j
00480     pr #255: newpage
00490 L490: ! 
00492     for j=1 to 20
00500       if inp(j)+pp(j)+ti(j)=0 then goto L540
00510       pr #255,using 'form pos 1,c 20,pos 25,pic(zzz),pos 35,c 14,pos 49,pic(zzz),pos 65,c 15,pos 85,pic(#)': "Item Number to Print",inp(j), "Print Position",pp(j), "Total this Item",ti(j)
00530     next j
00540 L540: ! 
00542     pr #255: newpage
00550   loop 
00560 L560: ! 
00562   close #1: ioerr ignore
00570 L570: ! 
00580   fncloseprn
00590 XIT: fnxit
00600 IGNORE: continue 
00610 ! <Updateable Region: ERTN>
00620 ERTN: fnerror(program$,err,line,act$,"xit")
00630   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00640   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00650   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00660 ERTN_EXEC_ACT: execute act$ : goto ERTN
00670 ! /region
00680 ! ______________________________________________________________________
