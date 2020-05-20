! REPLACE S:\acsCL\conversion\fnTrMstr_v0_to_v1
! converts the CL TRmstr file to version 1 !:
	! meainging the amount changes from G 10.2 to PD 10.2
def library fntrmstr_v0_to_v1
	autoLibrary

	fnStatus("Checkbook update Trans to v1: Updating Transaction file.")
	open #trmstr=fngethandle: "Name=[Q]\CLmstr\TrMstr.h[cno]",internal,outIn,relative 
	if version(trmstr)=1 then pr "trmstr is already version 1" 
		pr "press enter to continue" 
		input fields "1,1,C 1,N": pause$ 
	else 
		version(trmstr,1)
		for j=1 to lrec(trmstr)
			read #trmstr,using 'form pos 18,n 10.2': amt eof L240
			rewrite #trmstr,using 'form pos 18,pd 10.2': amt
		next j
		L240: !
		close #trmstr: 
	end if
fnend 
