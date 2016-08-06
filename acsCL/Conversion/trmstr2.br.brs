00020 ! this pgm will search the check history file and delete any records with conversion errors
00030   library 'R:\Core\Library': fncno,fnxit
00040   let fncno(cno)
00070   form c 9,skip 0
00080 ! 
00100   dim adr(2),gl(3),sf1$*28,pr$(4)*30,whgl$(5)*12,whgl(5,3)
00110   dim tr$(5)*35,tr(2),de$*30,bn$*40,ladr(12),sk$*12,sn$*50,flh$(20),ink$(20),ck$*11
00120   open #1: "Name=Q:\CLmstr\TRMSTR.H"&str$(cno)&",KFName=Q:\CLmstr\TRIDX1.H"&str$(cno)&",Shr",internal,outin,keyed 
00130   open #2: "Name=Q:\CLmstr\TRMSTR.H"&str$(cno)&",KFName=Q:\CLmstr\TRIDX2.H"&str$(cno)&",Shr",internal,outin,keyed 
00140 L140: read #1,using L150: ck$,tr$(2),tr$(3),tr$(4),tr$(5),pcde,clr,scd,mat tr eof L200 conv L170
00150 L150: form pos 1,c 11,g 6,g 10.2,c 8,c 35,n 1,n 6,n 1,2*pd 3
00160   goto L140
00170 L170: reread #1,using L150: ck$ eof L200
00180   delete #1,key=ck$: 
00190   goto L140
00200 L200: close #1: 
00210   close #2: 
00220   execute "INDEX Q:\CLmstr\TRMSTR.H"&str$(cno)&" Q:\CLmstr\TRIDX2.H"&str$(cno)&" 28/1 8/11 Replace DupKeys"
00230   execute "INDEX Q:\CLmstr\TRMSTR.H"&str$(cno)&" Q:\CLmstr\TRIDX1.H"&str$(cno)&" 1 11 Replace DupKeys"
00240   let fnxit
