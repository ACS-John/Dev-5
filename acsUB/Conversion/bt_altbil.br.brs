00010 ! Replace S:\acsUB\conversion\booktitle_altbil
00020 ! this program converts a field from ALL CAPITAL LETTERS !:
        ! to Book Title Capitalization
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnxit,fnerror,fncno
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim nam$*30,ab$(4)*30
00080 ! ______________________________________________________________________
00090   def fnbooktitle$*80(x$*80)
00100     let x$=lwrc$(trim$(x$)) : let olda=0
00110     let x$(1:1)=uprc$(x$(1:1))
00120 ! capitalize anthing after a SPACE
00130 L130: let a=pos(x$," ",olda) !:
          if a<>0 then !:
            let a+=1 : let x$(a:a)=uprc$(x$(a:a)) : let olda=a : goto L130
00140     let a=olda=0
00150 L150: let a=pos(x$,"-",olda) !:
          if a<>0 then !:
            let a+=1 : let x$(a:a)=uprc$(x$(a:a)) : let olda=a : goto L150
00160     let a=olda=0
00170 L170: let a=pos(x$,"/",olda) !:
          if a<>0 then !:
            let a+=1 : let x$(a:a)=uprc$(x$(a:a)) : let olda=a : goto L170
00180     let a=olda=0
00190 L190: let a=pos(x$,"\",olda) !:
          if a<>0 then !:
            let a+=1 : let x$(a:a)=uprc$(x$(a:a)) : let olda=a : goto L190
00200     let a=olda=0
00210 L210: let a=pos(x$,".",olda) !:
          if a<>0 then !:
            let a+=1 : let x$(a:a)=uprc$(x$(a:a)) : let olda=a : goto L210
00220     let fnbooktitle$=x$
00230   fnend 
00240 ! ______________________________________________________________________
00250   let fncno(cno)
00260   print newpage
00270 L270: print fields "8,20,C 30,R,N": "Book Title Capital"
00280   print fields "10,1,Cr 38": "Company Number to Convert (0 to Stop):"
00290 ! 
00300   let io1$(1)="10,40,N 2,UT,N"
00310 L310: rinput fields mat io1$: cno conv L310
00320   if cno=0 or cmdkey=5 or cmdkey=99 then goto XIT
00330   open #1: "Name="&env$('Q')&"\UBmstr\ubadrbil.h"&str$(cno),internal,outin,relative 
00340   for j=1 to lrec(1)
00350     read #1,using "Form Pos 11,4*c 30",rec=j: mat ab$ norec L390
00360     for x=1 to 4 : let ab$(x)=fnbooktitle$(ab$(x)) : next x
00370 ! Print NAM$
00380     rewrite #1,using "Form Pos 11,4*C 30",rec=j: mat ab$ norec L390
00390 L390: next j
00400   goto DONE
00410 ! ______________________________________________________________________
00420 DONE: close #1: 
00430   print "company number "&str$(cno)&" completed successfully"
00440   goto L270
00450 XIT: let fnxit
00460 ! ______________________________________________________________________
00470 ! <Updateable Region: ERTN>
00480 ERTN: let fnerror(program$,err,line,act$,"xit")
00490   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00500   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00510   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00520 ERTN_EXEC_ACT: execute act$ : goto ERTN
00530 ! /region
00540 ! ______________________________________________________________________
