! REPLACE S:\acsCL\conversion\trmstr-cnv
	autoLibrary
	fncno(cno) ! pr newpage
! pr f "10,5,C 60": "      COMPANY NUMBER:"
! pr f "12,12,C 16,B,5": "PRESS F5 TO STOP"
! L60: input fields "10,30,N 2,UE,N": cno conv L60
 
!  fnCopy("[Q]\CLmstr\TRMSTR.H[cno]","[Q]\CLmstr\TRMSTR.H[cno]",92)
	open #trmstr=1: "Name=[Q]\CLmstr\TRMSTR.H[cno]",internal,outIn,relative
	for j=1 to lrec(1)
		read #trmstr,using L140,rec=j: d1 noRec NEXT_J
L140: form pos 12,n 6
		d1=fncd(d1)
		rewrite #trmstr,using 'Form POS 85,N 2,N 6',rec=j: 19,d1
NEXT_J: !
	next j
	close #trmstr:
	fnIndex("[Q]\CLmstr\TRMSTR.H[cno]","[Q]\CLmstr\TRIDX1.H[cno]","1 11")
	fnIndex("[Q]\CLmstr\TRMSTR.H[cno]","[Q]\CLmstr\TRIDX2.H[cno]","28/1 8/11")
