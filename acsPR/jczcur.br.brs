00010 ! Replace S:\acsPR\jcZCur
00020 ! Zero Current Periond Information
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnwin3b,fnconsole,fnerror,fnwait,fnoldmsgbox,fnxit,fntop,fncno,fnchain
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim jn$*6,n$*40,a$(3)*30,b(4),cn$*11,k$*25,l(13),ta(2),tn$*6
00080   dim eno$*12,jno$*6,tr(9),pd$*30,cap$*128,message$*40,msgline$(2)*60
00090   dim response$(5)*1
00100 ! ______________________________________________________________________
00110   let fntop("S:\acsPR\jczcur",cap$="Zero Current Period Info")
00120   let fncno(cno)
00130 ! 
00135   let fnconsole(1)
00140 ! ______________________________________________________________________
00150   print newpage
00160   let msgline$(1)="Are you sure you wish to zero all"
00170   let msgline$(2)="current period information? (Y/N)"
00180   let fnoldmsgbox(mat response$,cap$,mat msgline$,2)
00190   if response$(1)="N" then goto XIT
00200 ! ______________________________________________________________________
00210   print newpage
00220   let fnwait(101,cap$,message$="Zeroing: please wait...",0)
00230 ! ______________________________________________________________________
00240   open #1: "Name="&env$('Q')&"\PRmstr\Company.h"&str$(cno)&",Shr",internal,input,relative 
00250   read #1,using L260,rec=1: kt
00260 L260: form pos 745,n 1
00270   close #1: 
00280   if kt=1 then goto L360
00290 ! ______________________________________________________________________
00300   open #1: "Name="&env$('Q')&"\PRmstr\JCTRANS.h"&str$(cno),internal,output 
00310   restore #1: 
00320   write #1,using L330: " "," ",mat tr," ",1
00330 L330: form pos 1,c 12,c 6,n 5,pd 3,pd 2,n 6,4*pd 4.2,pd 5.2,c 30,pd 3
00340   close #1: 
00350 ! ______________________________________________________________________
00360 L360: open #2: "Name="&env$('Q')&"\PRmstr\JCCAT.H"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\CatIndx.h"&str$(cno),internal,outin,keyed 
00370   if kt=0 then goto L430
00380 L380: read #2,using L400: a1,a2,a3 eof DONE
00390   rewrite #2,using L400: 0,0,0
00400 L400: form pos 79,3*pd 7.2
00410   goto L380
00420 ! ______________________________________________________________________
00430 L430: read #2,using L450: a1,a2,a3,a4,a5 eof DONE
00440   rewrite #2,using L450: 0,0,0,0,0
00450 L450: form pos 79,3*pd 7.2,pos 118,2*pd 3
00460   goto L430
00470 ! ______________________________________________________________________
00480 DONE: ! 
00490   close #2: 
00520 XIT: let fnxit
00530 ! ______________________________________________________________________
00540 ! <Updateable Region: ERTN>
00550 ERTN: let fnerror(program$,err,line,act$,"xit")
00560   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00570   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00580   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00590 ERTN_EXEC_ACT: execute act$ : goto ERTN
00600 ! /region
00610 ! ______________________________________________________________________
