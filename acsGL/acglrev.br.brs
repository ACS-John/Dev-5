00010 ! Replace S:\acsGL\acglRev
00020 ! Used to reverse specific entries !:
        ! enter the date and reference under "youref' and 'yourdate'
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnerror,fnwin3,fncno,fnerror
00050   fntop(program$,"Special Reversing")
00060   on error goto ERTN
00070 ! ______________________________________________________________________
00080   dim adr(2),ta(2),prg$*20
00090   dim t$*12,n(2),l$*12,p$*30
00100   dim cnam$*40
00110 ! ______________________________________________________________________
00120   fncno(cno,cnam$)
00130   open #20: "Name=CNo.H"&wsid$,internal,outIn,relative  !:
        read #20,using 'form POS 43,C 20,POS 137,N 2,POS 141,N 1',rec=1: prg$,systype,process !:
        close #20: 
00140 ! 
00150   open #1: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno],Shr",internal,outIn,keyed 
00160   open #2: "Name=[Q]\GLmstr\GLTrans.h[cno],Shr",internal,outIn,relative 
00170   open #3: "Name=[Q]\GLmstr\GL_Work_"&env$('acsUserId')&".h[cno],NoShr",internal,outIn 
00180   pr newpage
00190   x=lrec(2)
00200   for j=1 to x
00210 L210: read #2,using L220,rec=j: t$,s,k,mat n,l$,p$,ven$ eof L420 noRec L410
00220 L220: form pos 1,c 12,n 6,pd 6.2,n 2,n 2,c 12,c 30,c 8
00230     if s=yourdate and n(1)=3 and rtrm$(ltrm$(tr$))="yourref#" then goto L240 else goto L410
00240 L240: k=-k
00250     s=newdate
00260     if k=0 and uprc$(ltrm$(rtrm$(p$)))<>"VOID" then goto L210
00270     if val(t$(1:3))=0 and val(t$(4:9))=0 and val(t$(10:12))=0 and k=0 then goto L210
00280     if t$(3:3)=" " then t$(3:3)="0"
00290     if t$(12:12)=" " then t$(12:12)="0"
00300     read #1,using L310,key=t$: cb,mat ta nokey L410
00310 L310: form pos 87,pd 6.2,pos 333,2*pd 3
00320 L320: lr2=lrec(2)+1
00330     write #2,using L390,rec=lr2: t$,s,k,mat n,l$,p$,0 duprec L320
00340     if ta(1)=0 then ta(1)=lr2
00350     if ta(2)>0 then rewrite #2,using L400,rec=ta(2): lr2
00360     ta(2)=lr2
00370     cb=cb+k
00380     rewrite #1,using L310,key=t$: cb,mat ta
00390 L390: form pos 1,c 12,n 6,pd 6.2,n 2,n 2,c 12,c 30,pd 3
00400 L400: form pos 71,pd 3
00410 L410: next j
00420 L420: ! second pass
00430 L430: read #3,using L440: t$,s,k,mat n,l$,p$,ven$ eof XIT
00440 L440: form pos 1,c 12,n 6,pd 6.2,n 2,n 2,c 12,c 30,c 8
00450   if s=yourdate and n(1)=3 and rtrm$(ltrm$(tr$))="yourref#" then goto L460 else goto L630
00460 L460: k=-k
00470   s=newdate
00480   if k=0 and uprc$(ltrm$(rtrm$(p$)))<>"VOID" then goto L430
00490   if val(t$(1:3))=0 and val(t$(4:9))=0 and val(t$(10:12))=0 and k=0 then goto L430
00500   if t$(3:3)=" " then t$(3:3)="0"
00510   if t$(12:12)=" " then t$(12:12)="0"
00520   read #1,using L530,key=t$: cb,mat ta nokey L630
00530 L530: form pos 87,pd 6.2,pos 333,2*pd 3
00540 L540: lr2=lrec(2)+1
00550   write #2,using L610,rec=lr2: t$,s,k,mat n,l$,p$,0 duprec L540
00560   if ta(1)=0 then ta(1)=lr2
00570   if ta(2)>0 then rewrite #2,using L620,rec=ta(2): lr2
00580   ta(2)=lr2
00590   cb=cb+k
00600   rewrite #1,using L530,key=t$: cb,mat ta
00610 L610: form pos 1,c 12,n 6,pd 6.2,n 2,n 2,c 12,c 30,pd 3
00620 L620: form pos 71,pd 3
00630 L630: goto L430
00640 XIT: fnxit
00650 ! ______________________________________________________________________
00660 ERTN: fnerror(program$,err,line,act$,"NO")
00670   if lwrc$(act$)<>"pause" then goto L700
00680   execute "list -"&str$(line) !:
        pause  !:
        goto L700
00690   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause 
00700 L700: execute act$
00710   goto ERTN
00720 ! ______________________________________________________________________
