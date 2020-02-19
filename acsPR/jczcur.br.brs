00010 ! Replace S:\acsPR\jcZCur
00020 ! Zero Current Periond Information
00030 !
00040   library 'S:\Core\Library': fnwin3b,fnconsole,fnerror,fnwait,fnoldmsgbox,fnxit,fntop,fncno,fnchain
00050   on error goto Ertn
00060 !
00070   dim jn$*6,n$*40,a$(3)*30,b(4),cn$*11,k$*25,l(13),ta(2),tn$*6
00080   dim eno$*12,jno$*6,tr(9),pd$*30,cap$*128,message$*40,msgline$(2)*60
00090   dim response$(5)*1
00100 !
00110   fntop("S:\acsPR\jczcur",cap$="Zero Current Period Info")
00120   fncno(cno)
00130 ! 
00135   fnconsole(1)
00140 !
00150   pr newpage
00160   msgline$(1)="Are you sure you wish to zero all"
00170   msgline$(2)="current period information? (Y/N)"
00180   fnoldmsgbox(mat response$,cap$,mat msgline$,2)
00190   if response$(1)="N" then goto XIT
00200 !
00210   pr newpage
00220   fnwait(message$="Zeroing: please wait...",0)
00230 !
00240   open #1: "Name=[Q]\PRmstr\Company.h[cno],Shr",internal,input,relative 
00250   read #1,using L260,rec=1: kt
00260 L260: form pos 745,n 1
00270   close #1: 
00280   if kt=1 then goto L360
00290 !
00300   open #1: "Name=[Q]\PRmstr\JCTRANS.h[cno]",internal,output 
00310   restore #1: 
00320   write #1,using L330: " "," ",mat tr," ",1
00330 L330: form pos 1,c 12,c 6,n 5,pd 3,pd 2,n 6,4*pd 4.2,pd 5.2,c 30,pd 3
00340   close #1: 
00350 !
00360 L360: open #2: "Name=[Q]\PRmstr\JCCAT.H[cno],KFName=[Q]\PRmstr\CatIndx.h[cno]",internal,outIn,keyed 
00370   if kt=0 then goto L430
00380 L380: read #2,using L400: a1,a2,a3 eof DONE
00390   rewrite #2,using L400: 0,0,0
00400 L400: form pos 79,3*pd 7.2
00410   goto L380
00420 !
00430 L430: read #2,using L450: a1,a2,a3,a4,a5 eof DONE
00440   rewrite #2,using L450: 0,0,0,0,0
00450 L450: form pos 79,3*pd 7.2,pos 118,2*pd 3
00460   goto L430
00470 !
00480 DONE: ! 
00490   close #2: 
00520 XIT: fnxit
00530 !
00540 ! <Updateable Region: ERTN>
00550 ERTN: fnerror(program$,err,line,act$,"xit")
00560   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00570   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00580   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00590 ERTN_EXEC_ACT: execute act$ : goto ERTN
00600 ! /region
00610 !
