00010 ! REPLACE S:\acsCL\conversion\trmstr-cnv
00020   library 'S:\Core\Library': fncd,fncno,fnCopy,fnindex_it ! def fncd(x)=(x-int(x*.01)*100)*10000+int(x*.01)
00030   fncno(cno) ! pr newpage
00040 ! pr f "10,5,C 60": "      COMPANY NUMBER:"
00050 ! pr f "12,12,C 16,B,5": "PRESS F5 TO STOP"
00060 ! L60: input fields "10,30,N 2,UE,N": cno conv L60
00070 ! 
00080 !  fnCopy(env$('Q')&"\CLmstr\TRMSTR.H"&str$(cno),env$('Q')&"\CLmstr\TRMSTR.H"&str$(cno),92)
00110   open #trmstr=1: "Name="&env$('Q')&"\CLmstr\TRMSTR.H"&str$(cno),internal,outin,relative 
00120   for j=1 to lrec(1)
00130     read #trmstr,using L140,rec=j: d1 norec NEXT_J
00140 L140: form pos 12,n 6
00150     d1=fncd(d1)
00160     rewrite #trmstr,using 'Form POS 85,N 2,N 6',rec=j: 19,d1
00162 NEXT_J: ! 
00170   next j
00180   close #trmstr: 
00190   fnindex_it(env$('Q')&"\CLmstr\TRMSTR.H"&str$(cno),env$('Q')&"\CLmstr\TRIDX1.H"&str$(cno),"1 11")
00200   fnindex_it(env$('Q')&"\CLmstr\TRMSTR.H"&str$(cno),env$('Q')&"\CLmstr\TRIDX2.H"&str$(cno),"28/1 8/11")
