00010 ! Replace S:\acsGL\FixRef
00020 ! trying to place income statement reference numbers back into a chart of accounts, might be handy, but don't need on menu
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnerror,fncno
00050   on error goto Ertn
00060 ! ______________________________________________________________________
00070   dim io1$(9),gln(2,3),ta(2),ac(18),te$*1,cap$*128
00080   dim d$*50,bc(13),bp(13),bm(13),rf(6),dn$*3,an$*6,sn$*3,glk$*12,fsk$*5
00090 ! ______________________________________________________________________
00100   fntop(program$,"Fix Reference Numbers")
00110   fncno(cno)
00120   open #1: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno],Shr",internal,outIn,keyed 
00130   fil$(1)="ACGLFNSB" : idx$(1)="FNSBINDX"
00140   fil$(2)="ACGLFNSc" : idx$(2)="FNScINDX"
00150   fil$(3)="ACGLFNSi" : idx$(3)="FNSiINDX"
00160   fil$(4)="ACGLFNSj" : idx$(4)="FNSjINDX"
00170   fil$(5)="ACGLfNSf" : idx$(5)="FNSfINDX"
00180   fil$(6)="ACGLfNSg" : idx$(6)="FNSGINDX"
00190 ! ______________________________________________________________________
00200   on fkey 5 goto XIT
00210   pr newpage
00220   close #2: ioerr L230
00230 L230: open #2: "Name=[Q]\GLmstr\"&fil$(6)&"&.h1,KFName=[Q]\GLmstr\"&idx$(6)&".h1",internal,outIn,keyed 
00240 L240: read #1,using L250: dno,ano,sno,d$,mat rf eof L350
00250 L250: form pos 1,n 3,n 6,n 3,c 50,6*pd 3,42*pd 6.2,2*pd 3
00260   restore #2: 
00270 L270: read #2,using L300: rno,refd$,type$ eof L240
00280   if type$<>"D" then goto L270
00290   if d$(1:15)=refd$(1:15) then goto L310 else goto L270 ! try to find match on descriptions
00300 L300: form pos 1,n 5,c 50,c 1,2*n 2,15*n 1,n 3
00310 L310: rf(6)=rno ! rewrite new reference back into g/l account  ?
00320   rewrite #1,using L330: mat rf
00330 L330: form pos 63,6*pd 3
00340   goto L240
00350 L350: close #2: 
00370   stop 
00380 ! ______________________________________________________________________
00390 XIT: fnxit
00400 ! ______________________________________________________________________
00410 ! <Updateable Region: ERTN>
00420 ERTN: fnerror(program$,err,line,act$,"xit")
00430   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00440   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00450   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00460 ERTN_EXEC_ACT: execute act$ : goto ERTN
00470 ! /region
00480 ! ______________________________________________________________________
