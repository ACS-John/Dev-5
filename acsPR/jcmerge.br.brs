00010 ! Replace S:\acsPR\jcMerge
00020 ! Posting to Jobs...
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnwait,fncno,fnerror,fndate_mmddyy_to_ccyymmdd,fntop,fnxit
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim h(7),ji1(6),jn$*6,ji2(6),cn$*11,l(13),ta(2),tr(9),empnum$*12
00080   dim empnam$*30,cap$*128,message$*40
00090 ! ______________________________________________________________________
00100   fntop("S:\acsPR\jcMerge",cap$="Job Cost Merge")
00110   fncno(cno)
00120 ! 
00170 ! ______________________________________________________________________
00180   open #2: "Name="&env$('Q')&"\PRmstr\JCCAT.H"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\CatIndx.h"&str$(cno)&",Shr",internal,outin,keyed 
00190   open #3: "Name="&env$('temp')&"\Work."&session$,internal,input 
00200   open #4: "Name="&env$('Q')&"\PRmstr\JCPRH1.H"&str$(cno)&",Shr",internal,output 
00210   open #5: "Name="&env$('Q')&"\PRmstr\JCTRANS.h"&str$(cno)&",Shr",internal,outin,relative 
00220 L220: read #3,using L230: mat ji1,jn$,mat ji2,empnam$,sal eof L720
00230 L230: form pos 1,n 8,n 1,pd 4,pd 2,2*pd 4.2,c 6,2*pd 3,pd 5.2,n 2,2*pd 4.2,c 30,pd 4.2
00240   if ji1(1)=-1 then goto L220
00250   if ji2(4)<1 or ji2(4)>10 then goto L280
00260   ji2(6)=ji2(3)
00270   ji2(3)=0
00280 L280: jn$=lpad$(rtrm$(jn$),6)
00290   if h(7)<1 or h(7)>11 then goto L300 else goto L340
00300 L300: if h(1)=ji1(1) and h(3)=ji1(4) and dt2=ji1(3) then goto L400
00310   if h(1)=0 then goto L360
00320   h(6)=sal
00330   dt2=fndate_mmddyy_to_ccyymmdd(ji1(3))
00340 L340: write #4,using L350: mat h,dt2,jn$
00350 L350: form pos 1,n 8,n 1,pd 2,2*pd 4.2,pd 5.2,n 2,n 8,c 6
00360 L360: mat h=(0)
00370   h(1)=ji1(1)
00380   h(2)=ji1(2)
00390   h(3)=ji1(4)
00400 L400: h(4)=h(4)+ji1(5)
00410   h(5)=h(5)+ji1(6)
00420   if ji2(4)<1 or ji2(4)>10 then h(6)=ji2(3) else h(6)=ji2(6)
00430   h(7)=ji2(4)
00440   if ltrm$(jn$)="" or rtrm$(ltrm$(jn$))="0" then goto L220
00450   cn$=jn$&lpad$(str$(ji2(1)),5)
00460   read #2,using L470,key=cn$: mat l,mat ta nokey L600
00470 L470: form pos 37,11*pd 7.2,2*pd 2,2*pd 3
00480   if ji1(5)+ji1(6)=0 then goto L560
00490   l(4)=l(4)+ji2(3)
00500   l(7)=l(7)+ji2(3)
00510   l(5)=l(5)+ji1(5)+ji1(6)
00520   l(8)=l(8)+ji1(5)+ji1(6)
00530   l(6)=l(6)+ji2(6)
00540   l(9)=l(9)+ji2(6)
00550   goto L580
00560 L560: l(6)=l(6)+ji2(3)+ji2(6)
00570   l(9)=l(9)+ji2(3)+ji2(6)
00580 L580: l(10)=l(10)+ji2(5)
00590   goto L600
00600 L600: read #5,using L610,rec=1,reserve: ot5
00610 L610: form pos 86,pd 3
00620   empnum$=lpad$(rtrm$(str$(ji1(1))),12)
00630 L630: ot5=lrec(5)+1
00640   write #5,using L650,rec=ot5,reserve: empnum$,jn$,ji2(1),ji2(2),ji1(4),ji1(3),ji1(5),ji1(6),ji2(5),ji2(6),ji2(3),empnam$,0 duprec L630
00650 L650: form pos 1,c 12,c 6,n 5,pd 3,pd 2,n 6,4*pd 4.2,pd 5.2,c 30,pd 3
00660   if ta(2)=0 then ta(1)=ot5 else rewrite #5,using L610,rec=ta(2),reserve: ot5
00670   rewrite #5,using L610,rec=1,release: ot5
00680   ta(2)=ot5
00690   rewrite #2,using L470,key=cn$: mat l,mat ta
00700   goto L220
00710 ! ______________________________________________________________________
00720 L720: dt2=fndate_mmddyy_to_ccyymmdd(ji1(3))
00730   if h(1)><0 then write #4,using L350: mat h,dt2,jn$
00740   goto XIT
00750 ! ______________________________________________________________________
00760 ! <Updateable Region: ERTN>
00770 ERTN: fnerror(program$,err,line,act$,"xit")
00780   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00790   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00800   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00810 ERTN_EXEC_ACT: execute act$ : goto ERTN
00820 ! /region
00830 ! ______________________________________________________________________
00840 XIT: ! fnXIT
00841   close #3,free: 
00842   fnxit
00850 ! ______________________________________________________________________
