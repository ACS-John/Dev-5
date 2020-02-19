00010 ! Replace S:\acsUB\conversion\booktitle
00020 ! this program converts a field from ALL CAPITAL LETTERS !:
        ! to Book Title Capitalization
00030 !
00040   library 'S:\Core\Library': fnxit,fnerror,fncno
00050   on error goto Ertn
00060 !
00070   dim nam$*30
00080 !
00090   def fnbooktitle$*80(x$*80)
00100     x$=lwrc$(trim$(x$)) : olda=0
00110     x$(1:1)=uprc$(x$(1:1))
00120 ! capitalize anthing after a SPACE
00130 L130: a=pos(x$," ",olda) !:
          if a<>0 then !:
            a+=1 : x$(a:a)=uprc$(x$(a:a)) : olda=a : goto L130
00140     a=olda=0
00150 L150: a=pos(x$,"-",olda) !:
          if a<>0 then !:
            a+=1 : x$(a:a)=uprc$(x$(a:a)) : olda=a : goto L150
00160     a=olda=0
00170 L170: a=pos(x$,"/",olda) !:
          if a<>0 then !:
            a+=1 : x$(a:a)=uprc$(x$(a:a)) : olda=a : goto L170
00180     a=olda=0
00190 L190: a=pos(x$,"\",olda) !:
          if a<>0 then !:
            a+=1 : x$(a:a)=uprc$(x$(a:a)) : olda=a : goto L190
00200     a=olda=0
00210 L210: a=pos(x$,".",olda) !:
          if a<>0 then !:
            a+=1 : x$(a:a)=uprc$(x$(a:a)) : olda=a : goto L210
00220     fnbooktitle$=x$
00230   fnend 
00240 !
00250   fncno(cno)
00260   pr newpage
00270 L270: pr f "8,20,C 30,R,N": "Book Title Capital"
00280   pr f "10,1,Cr 38": "Company Number to Convert (0 to Stop):"
00290 ! 
00300   io1$(1)="10,40,N 2,UT,N"
00310 L310: rinput fields mat io1$: cno conv L310
00320   if cno=0 or cmdkey=5 or cmdkey=99 then goto XIT
00330   open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubindex.h[cno],Shr",internal,outIn,keyed 
00340   for j=1 to lrec(1)
00350     read #1,using "Form Pos 71,c 30",rec=j: nam$ noRec L390
00360     nam$=fnbooktitle$(nam$)
00370     pr nam$
00380     rewrite #1,using "Form Pos 71,c 30",rec=j: nam$ noRec L390
00390 L390: next j
00400   goto DONE
00410 !
00420 DONE: close #1: 
00430   pr "company number [cno] completed successfully"
00440   goto L270
00450 XIT: stop 
00460 !
00470 ! <Updateable Region: ERTN>
00480 ERTN: fnerror(program$,err,line,act$,"xit")
00490   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00500   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00510   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00520 ERTN_EXEC_ACT: execute act$ : goto ERTN
00530 ! /region
00540 !
