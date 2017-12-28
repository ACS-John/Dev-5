00020 ! 
00030   dim dat$*20,cnam$*40,vnam$*30,de$*35,bn$*30
00040   pr newpage
00050   pr "FIX DATE CLEARED: ENTER COMPANY NUMBER:"
00060   input cno
00070 ! 
00080   open #5: "Name="&env$('Q')&"\CLmstr\TRMSTR.H"&env$('cno')&",Shr",internal,outIn,relative 
00090 L90: read #5,using L100: bcde,tcde,ck$,clr eof END1
00100 L100: form pos 1,n 2,n 1,c 8,pos 72,n 6
00102   if tcde><1 then goto L90
00103   ck1=val(ck$) conv L90
00104   if ck1<40718 or ck1>49000 then goto L90
00110   if clr><123107 then goto L90
00120   rewrite #5,using L122: 0
00122 L122: form pos 72,n 6
00130   goto L90
00140 END1: stop 
