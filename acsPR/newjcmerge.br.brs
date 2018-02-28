00010 ! Replace S:\acsPR\newjcMerge
00020 ! Posting to Jobs...
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnwait,fncno,fnerror,fndate_mmddyy_to_ccyymmdd,fntop,fnxit
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim h(7),ji1(6),jn$*6,ji2(6),cn$*11,l(13),ta(2),tr(9),empnum$*12
00080   dim empnam$*30,cap$*128,message$*40,k$*25
00090 ! ______________________________________________________________________
00100   fntop("S:\acsPR\jcMerge",cap$="Job Cost Merge")
00110   fncno(cno)
00120 ! 
00130 ! ______________________________________________________________________
00140   open #2: "Name=[Q]\PRmstr\JCCAT.H[cno],KFName=[Q]\PRmstr\CatIndx.h[cno],Shr",internal,outIn,keyed 
00150   open #3: "Name=jcWork."&session$,internal,input 
00160   open #4: "Name=[Q]\PRmstr\JCPRH1.H[cno],RecL=128,use",internal,output 
00170   open #5: "Name=[Q]\PRmstr\JCTRANS.h[cno],Shr",internal,outIn,relative 
00180   open #9: "Name=[Q]\PRmstr\Category.H[cno],KFName=[Q]\PRmstr\categoryIDX.H[cno],Shr",internal,outIn,keyed 
00190 L190: read #3,using L210: mat ji1,jn$,mat ji2,empnam$,sal eof L750
00210 L210: form pos 1,n 8,n 1,pd 4,pd 2,2*pd 4.2,c 6,2*pd 3,pd 5.2,n 2,2*pd 4.2,c 30,pd 4.2
00220   if ji1(1)=-1 then goto L190
00230   if ji2(4)<1 or ji2(4)>20 then goto L260
00240   ji2(6)=ji2(3)
00250   ji2(3)=0
00260 L260: jn$=lpad$(rtrm$(jn$),6)
00270   if h(7)<1 or h(7)>11 then goto L280 else goto L320
00280 L280: if h(1)=ji1(1) and h(3)=ji1(4) and dt2=ji1(3) then goto L380
00290   if h(1)=0 then goto L340
00300   h(6)=sal
00310   dt2=fndate_mmddyy_to_ccyymmdd(ji1(3))
00320 L320: write #4,using L330: mat h,dt2,jn$
00330 L330: form pos 1,n 8,n 1,pd 2,2*pd 4.2,pd 5.2,n 2,n 8,c 6
00340 L340: mat h=(0)
00350   h(1)=ji1(1)
00360   h(2)=ji1(2)
00370   h(3)=ji1(4)
00380 L380: h(4)=h(4)+ji1(5)
00390   h(5)=h(5)+ji1(6)
00400   if ji2(4)<1 or ji2(4)>20 then h(6)=ji2(3) else h(6)=ji2(6)
00410   h(7)=ji2(4)
00420   if ltrm$(jn$)="" or rtrm$(ltrm$(jn$))="0" then goto L190
00430   cn$=jn$&lpad$(str$(ji2(1)),5)
00440   read #9,using 'form pos 6,c 25',key=cnvrt$("pic(zzzzz)",val(cn$(7:11))): k$ nokey L450
00450 L450: read #2,using L451,key=cn$: mat l,mat ta nokey L470
00451 L451: form pos 37,11*pd 7.2,2*pd 2,2*pd 3
00460   goto L500
00470 L470: mat l=(0): mat ta=(0)
00480   write #2,using L490: cn$,k$,mat l,mat ta
00490 L490: form pos 1,c 11,c 25,pos 37,11*pd 7.2,2*pd 2,2*pd 3
00500 L500: form pos 1,c 11,c 25,pos 37,11*pd 7.2,2*pd 2,2*pd 3
00510   if ji1(5)+ji1(6)=0 then goto L590
00520   l(4)=l(4)+ji2(3)
00530   l(7)=l(7)+ji2(3)
00540   l(5)=l(5)+ji1(5)+ji1(6)
00550   l(8)=l(8)+ji1(5)+ji1(6)
00560   l(6)=l(6)+ji2(6)
00570   l(9)=l(9)+ji2(6)
00580   goto L610
00590 L590: l(6)=l(6)+ji2(3)+ji2(6)
00600   l(9)=l(9)+ji2(3)+ji2(6)
00610 L610: l(10)=l(10)+ji2(5)
00620   goto L630
00630 L630: read #5,using L640,rec=1,reserve: ot5
00640 L640: form pos 86,pd 3
00650   empnum$=lpad$(rtrm$(str$(ji1(1))),12)
00660 L660: ot5=lrec(5)+1
00670   write #5,using L680,rec=ot5,reserve: empnum$,jn$,ji2(1),ji2(2),ji1(4),ji1(3),ji1(5),ji1(6),ji2(5),ji2(6),ji2(3),empnam$,0 duprec L660
00680 L680: form pos 1,c 12,c 6,n 5,pd 3,pd 2,n 6,4*pd 4.2,pd 5.2,c 30,pd 3
00690   if ta(2)=0 then ta(1)=ot5 else rewrite #5,using L640,rec=ta(2),reserve: ot5
00700   rewrite #5,using L640,rec=1,release: ot5
00710   ta(2)=ot5
00720   rewrite #2,using L500,key=cn$: cn$,k$,mat l,mat ta
00730   goto L190
00740 ! ______________________________________________________________________
00750 L750: dt2=fndate_mmddyy_to_ccyymmdd(ji1(3))
00760   if h(1)><0 then write #4,using L330: mat h,dt2,jn$
00770   goto XIT
00780 ! ______________________________________________________________________
00790 ! <Updateable Region: ERTN>
00800 ERTN: fnerror(program$,err,line,act$,"xit")
00810   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00820   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00830   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00840 ERTN_EXEC_ACT: execute act$ : goto ERTN
00850 ! /region
00860 ! ______________________________________________________________________
00870 XIT: ! fnXIT
00880   close #3,free: 
00890   fnxit
00900 ! ______________________________________________________________________
