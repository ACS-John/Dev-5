00010 ! Replace acsPR\Conversion\prCHevil-Cnv
00020 ! CONVERT second field from PD 6 to N 6                        !:
        ! this is not a program to convert towards standard !!!!!
00030 !
00040   library 'Core\Library': fntop,fnxit, fnerror
00050   on error goto Ertn
00060 !
00070   def fncd(x)=(x-int(x*.01)*100)*10000+int(x*.01)
00080 !
00090   def fndate_mmddyy_to_ccyymmdd(x)
00100     x2=(x-int(x*.01)*100)*10000+int(x*.01)
00110     if int(x2*.0001)<90 then x2=x2+20000000 else x2=x2+19000000
00120     fndate_mmddyy_to_ccyymmdd=x2
00130   fnend 
00140 !
00150   pr newpage
00160   pr f "10,15,C 50": "ENTER COMPANY # TO CONVERT OR 0 TO STOP:"
00170 L170: input fields "10,55,N 2,UE,N": cno conv L170
00180   if cno=0 then goto XIT
00190 ! 
00200 !
00210   open #4: "Name=[Q]\PRmstr\PRCkHist.h[cno],RecL=150,USE",internal,outIn 
00220 L220: read #4,using L230: d1 eof L350,conv L320
00230 L230: form pos 9,pd 6
00240   if d1<1000000 then goto L270
00250   d1=val(str$(d1)(5:6)&str$(d1)(7:8)&str$(d1)(3:4))
00260 ! pr D1
00270 L270: ! 
00280   rewrite #4,using L290: d1
00290 L290: form pos 9,n 6
00300   goto L220
00310 !
00320 L320: read #4,using L290: d1 eof L350
00330   goto L220
00340 !
00350 L350: close #4: 
00360   execute "Index [Q]\PRmstr\PRCkHist.h[cno]"&' '&"[Q]\PRmstr\PRCKINDX.h[cno] 1 14 Replace DupKeys -n"
00370 XIT: stop  ! Chain "PRMENU/acsPR"
00380 !
00390 ! <updateable region: ertn>
00400 ERTN: fnerror(program$,err,line,act$,"xit")
00410   if uprc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00420   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00430   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00440 ERTN_EXEC_ACT: execute act$ : goto ERTN
00450 ! /region
00460 !
