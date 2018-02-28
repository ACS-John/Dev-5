00010 ! Replace S:\acsPR\Conversion\prCkHist-Fix
00020 ! CONVERT FOR CC CHG
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnerror
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim tdc(6),tc2(22)
00080 ! ______________________________________________________________________
00090   def fncd(x)=(x-int(x*.01)*100)*10000+int(x*.01)
00100 ! ______________________________________________________________________
00110   pr newpage
00120   pr f "10,15,C 50": "ENTER COMPANY # TO CONVERT OR 0 TO STOP:"
00130 L130: input fields "10,55,N 2,UE,N": cno conv L130
00140   if cno=0 then goto XIT
00150 ! 
00160   open #4: "Name=[Q]\PRmstr\PRCkHist.h[cno],RecL=150,USE",internal,outIn,relative 
00170 L170: r1+=1
00180   read #4,using L190,rec=r1: eno,prd,ckno,mat tdc,mat tc2 eof L330,conv L220,noRec L330
00190 L190: form pos 1,n 8,pd 6,n 7,5*pd 3.2,pd 4.2,22*pd 5.2
00200   goto L170
00210 ! ______________________________________________________________________
00220 L220: read #4,using L230,rec=r1: eno,prd,ckno,mat tdc,mat tc2 eof L330,conv L290,noRec L330
00230 L230: form pos 1,n 8,n 6,n 7,5*pd 3.2,pd 4.2,22*pd 5.2
00240   prd=19000000+fncd(prd)
00250   rewrite #4,using L260,rec=r1: prd
00260 L260: form pos 9,pd 6
00270   goto L170
00280 ! ______________________________________________________________________
00290 L290: read #4,using L230,rec=r1: eno conv L300
00300 L300: delete #4,rec=r1: 
00310   goto L170
00320 ! ______________________________________________________________________
00330 L330: if r1<lrec(4) then goto L170
00340   close #4: 
00350   execute "Index [Q]\PRmstr\PRCkHist.h[cno]"&' '&"[Q]\PRmstr\PRCKINDX.h[cno] 1 14 Replace DupKeys"
00360 XIT: stop 
00370 ! ______________________________________________________________________
00380 ! <updateable region: ertn>
00390 ERTN: fnerror(program$,err,line,act$,"xit")
00400   if uprc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00410   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00420   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00430 ERTN_EXEC_ACT: execute act$ : goto ERTN
00440 ! /region
00450 ! ______________________________________________________________________
