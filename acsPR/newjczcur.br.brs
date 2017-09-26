00010 ! Replace S:\acsPR\newjcZCur
00020 ! Zero Current Periond Information
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnerror,fnxit,fntop,fncno,fnxit,fnmsgbox
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim jn$*6,n$*40,a$(3)*30,b(4),cn$*11,k$*25,l(13),ta(2),tn$*6
00080   dim eno$*12,jno$*6,tr(9),pd$*30,cap$*128,ml$(2)*60
00090   dim response$(5)*1
00100 ! ______________________________________________________________________
00110   let fntop("S:\acsPR\jczcur",cap$="Zero Current Period Info")
00120   let fncno(cno)
00130 ! 
00140 ! ______________________________________________________________________
00150   mat ml$(2) !:
        let ml$(1)="Are you sure you want to zero all" !:
        let ml$(2)="current period information?." !:
        let fnmsgbox(mat ml$,resp$,cap$,36)
00160 ! ______________________________________________________________________
00170   open #1: "Name="&env$('Q')&"\PRmstr\Company.h"&str$(cno)&",Shr",internal,input,relative 
00180   read #1,using L190,rec=1: kt
00190 L190: form pos 745,n 1
00200   close #1: 
00210   if kt=1 then goto L290
00220 ! ______________________________________________________________________
00230   open #1: "Name="&env$('Q')&"\PRmstr\JCTRANS.h"&str$(cno),internal,output 
00240   restore #1: 
00250   write #1,using L260: " "," ",mat tr," ",1
00260 L260: form pos 1,c 12,c 6,n 5,pd 3,pd 2,n 6,4*pd 4.2,pd 5.2,c 30,pd 3
00270   close #1: 
00280 ! ______________________________________________________________________
00290 L290: open #2: "Name="&env$('Q')&"\PRmstr\JCCAT.H"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\CatIndx.h"&str$(cno),internal,outin,keyed 
00300   if kt=0 then goto L360
00310 L310: read #2,using L330: a1,a2,a3 eof DONE
00320   rewrite #2,using L330: 0,0,0
00330 L330: form pos 79,3*pd 7.2
00340   goto L310
00350 ! ______________________________________________________________________
00360 L360: read #2,using L380: a1,a2,a3,a4,a5 eof DONE
00370   rewrite #2,using L380: 0,0,0,0,0
00380 L380: form pos 79,3*pd 7.2,pos 118,2*pd 3
00390   goto L360
00400 ! ______________________________________________________________________
00410 DONE: ! 
00420   close #2: 
00430 XIT: let fnxit
00440 ! ______________________________________________________________________
00450 ! <Updateable Region: ERTN>
00460 ERTN: let fnerror(program$,err,line,act$,"xit")
00470   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00480   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00490   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00500 ERTN_EXEC_ACT: execute act$ : goto ERTN
00510 ! /region
00520 ! ______________________________________________________________________
