00010 ! Replace S:\acsPR\prRpt4
00020 ! Report File - Proof List
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnwait,fnopenprn,fncloseprn,fncno,fnerror,fntop,fnxit
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim rt$*78,ch$(2)*132,psc(100),inp(20),pp(20),ti(20),cnam$*40
00080   dim message$*40,cap$*128
00090 ! ______________________________________________________________________
00100   fntop("S:\acsPR\prRpt4",cap$="Report File - Proof List")
00110   fncno(cno,cnam$)
00120   open #1: "Name=[Q]\PRmstr\PRReport.h[cno],KFName=[Q]\PRmstr\prrptidx.h[cno],Shr",internal,input,keyed 
00130   fnopenprn(cp,58,220,process)
00140   nametab=66-len(rtrm$(cnam$))/2
00150 ! ______________________________________________________________________
00160   pr newpage
00170   message$="Printing:  please wait..."
00180   on fkey 5 goto L560
00190   fnwait(message$,1)
00200 ! ______________________________________________________________________
00210 L210: read #1,using L220: rn,rt$,mat ch$,ips,sd,cp,mat psc,mat inp,mat pp,mat ti eof L560
00220 L220: form pos 1,n 2,c 78,2*c 132,n 3,2*n 1,100*pd 6.3,40*pd 2,20*n 1
00230   pr #255,using L240: date$,cnam$,time$,"Payroll Report File Proof List"
00240 L240: form skip 2,pos 1,c 8,pos nametab,c 40,skip 1,pos 1,c 8,pos 51,c 30,skip 2
00250   pr #255,using L260: "Report Number",rn
00260 L260: form pos 1,c 13,pos 20,pic(zz),skip 1
00270   pr #255,using L280: "Report Title",rt$
00280 L280: form pos 1,c 12,pos 20,c 78,skip 1
00290   pr #255,using L300: "Column Headings",ch$(1)
00300 L300: form pos 1,c 15,skip 2,c 132,skip 1
00310   pr #255,using L320: ch$(2)
00320 L320: form pos 1,c 132,skip 2
00330   pr #255,using L340: "Item # for Prt Sel",ips
00340 L340: form pos 1,c 18,pos 30,pic(zz#),skip 1
00350   pr #255,using L360: "Summarize Departments",sd
00360 L360: form pos 1,c 26,pos 32,pic(#),skip 1
00370   pr #255,using L360: "Use Condensed Print",cp
00380   for j=1 to 100
00390     if psc(j)=0 then goto L490
00400     if j><48 then goto L450
00410     pr #255: newpage
00420     pr #255,using L430: "Print Selection Criteria",psc(j)
00430 L430: form skip 3,pos 1,c 24,pos 30,pic(---------.###),skip 1
00440     goto L470
00450 L450: pr #255,using L460: "PRINT SELECTION CRITERIA",psc(j)
00460 L460: form pos 1,c 24,pos 30,pic(---------.###),skip 1
00470 L470: next j
00480   pr #255: newpage
00490 L490: for j=1 to 20
00500     if inp(j)+pp(j)+ti(j)=0 then goto L540
00510     pr #255,using L520: "Item Number to Print",inp(j), "Print Position",pp(j), "Total this Item",ti(j)
00520 L520: form pos 1,c 20,pos 25,pic(zzz),pos 35,c 14,pos 49,pic(zzz),pos 65,c 15,pos 85,pic(#),skip 1
00530   next j
00540 L540: pr #255: newpage
00550   goto L210
00560 L560: close #1: ioerr L570
00570 L570: fncloseprn
00580 ! ______________________________________________________________________
00590 XIT: fnxit
00600 ! ______________________________________________________________________
00610 ! <Updateable Region: ERTN>
00620 ERTN: fnerror(program$,err,line,act$,"xit")
00630   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00640   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00650   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00660 ERTN_EXEC_ACT: execute act$ : goto ERTN
00670 ! /region
00680 ! ______________________________________________________________________
