00010 ! Replace S:\acsPR\newjcMrgC
00020 ! JOB COST MERGE CHARGES
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim rn$*12,jn$*6,ji2(3),cn$*11,l(13),ta(2),tr(9),empnum$*12,empnam$*30
00080 ! ______________________________________________________________________
00090   fncno(cno)
00100 ! 
00110 ! ______________________________________________________________________
00140   open #2: "Name=[Q]\PRmstr\JCCAT.H[cno],KFName=[Q]\PRmstr\CatIndx.h[cno],Shr",internal,outIn,keyed 
00150   open #3: "Name=jccharges."&wsid$,internal,input 
00160   open #5: "Name=[Q]\PRmstr\JCTRANS.h[cno],Shr",internal,outIn,relative 
00170 L170: read #3,using L180: rn$,dat,jn$,mat ji2,empnam$ eof L400
00180 L180: form pos 1,c 12,pd 4,c 6,2*pd 3,pd 5.2,c 30
00190   if ltrm$(rtrm$(rn$))="-1" or ji2(3)=0 then goto L170
00200   jn$=lpad$(rtrm$(jn$),6)
00210   cn$=jn$&lpad$(str$(ji2(1)),5)
00220   read #2,using L230,key=cn$: mat l,mat ta nokey L280
00230 L230: form pos 37,11*pd 7.2,2*pd 2,2*pd 3
00240   nc1=0
00250   l(6)=l(6)+ji2(3)
00260   l(9)=l(9)+ji2(3)
00270   goto L290
00280 L280: nc1=1
00290 L290: read #5,using L300,rec=1,reserve: ot5
00300 L300: form pos 86,pd 3
00310   empnum$=lpad$(rtrm$(rn$),12)
00320 L320: ot5=lrec(5)+1
00330   write #5,using L340,rec=ot5,reserve: empnum$,jn$,ji2(1),ji2(2),0,dat,0,0,0,0,ji2(3),empnam$,0 duprec L320
00340 L340: form pos 1,c 12,c 6,n 5,pd 3,pd 2,n 6,4*pd 4.2,pd 5.2,c 30,pd 3
00350   if ta(2)=0 then ta(1)=ot5 else rewrite #5,using L300,rec=ta(2): ot5
00360   rewrite #5,using L300,rec=1,release: ot5
00370   ta(2)=ot5
00380   if nc1=0 then rewrite #2,using L230,key=cn$: mat l,mat ta
00390   goto L170
00400 L400: close #2: 
00410   close #3: 
00420   close #5: 
00430 XIT: fnxit
00440 ! ______________________________________________________________________
00450 ERTN: fnerror(program$,err,line,act$,"NO")
00460   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00470   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00480   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00490 ERTN_EXEC_ACT: execute act$ : goto ERTN
00500 ! ______________________________________________________________________
