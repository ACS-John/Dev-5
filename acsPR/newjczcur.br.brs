00010 ! Replace S:\acsPR\newjcZCur
00020 ! Zero Current Periond Information
00030 !
00040   library 'S:\Core\Library': fnerror,fnxit,fntop,fncno,fnxit,fnmsgbox
00050   on error goto Ertn
00060 !
00070   dim jn$*6,n$*40,a$(3)*30,b(4),cn$*11,k$*25,l(13),ta(2),tn$*6
00080   dim eno$*12,jno$*6,tr(9),pd$*30,cap$*128,ml$(2)*60
00090   dim response$(5)*1
00100 !
00110   fntop("S:\acsPR\jczcur",cap$="Zero Current Period Info")
00120   fncno(cno)
00130 ! 
00140 !
00150   mat ml$(2) !:
        ml$(1)="Are you sure you want to zero all" !:
        ml$(2)="current period information?." !:
        fnmsgbox(mat ml$,resp$,cap$,36)
00160 !
00170   open #1: "Name=[Q]\PRmstr\Company.h[cno],Shr",internal,input,relative 
00180   read #1,using L190,rec=1: kt
00190 L190: form pos 745,n 1
00200   close #1: 
00210   if kt=1 then goto L290
00220 !
00230   open #1: "Name=[Q]\PRmstr\JCTRANS.h[cno]",internal,output 
00240   restore #1: 
00250   write #1,using L260: " "," ",mat tr," ",1
00260 L260: form pos 1,c 12,c 6,n 5,pd 3,pd 2,n 6,4*pd 4.2,pd 5.2,c 30,pd 3
00270   close #1: 
00280 !
00290 L290: open #2: "Name=[Q]\PRmstr\JCCAT.H[cno],KFName=[Q]\PRmstr\CatIndx.h[cno]",internal,outIn,keyed 
00300   if kt=0 then goto L360
00310 L310: read #2,using L330: a1,a2,a3 eof DONE
00320   rewrite #2,using L330: 0,0,0
00330 L330: form pos 79,3*pd 7.2
00340   goto L310
00350 !
00360 L360: read #2,using L380: a1,a2,a3,a4,a5 eof DONE
00370   rewrite #2,using L380: 0,0,0,0,0
00380 L380: form pos 79,3*pd 7.2,pos 118,2*pd 3
00390   goto L360
00400 !
00410 DONE: ! 
00420   close #2: 
00430 XIT: fnxit
00440 !
00450 ! <Updateable Region: ERTN>
00460 ERTN: fnerror(program$,err,line,act$,"xit")
00470   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00480   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00490   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00500 ERTN_EXEC_ACT: execute act$ : goto ERTN
00510 ! /region
00520 !
