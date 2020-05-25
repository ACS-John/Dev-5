! Replace S:\Core\Print1099
! pr 1099 Forms (From a File) : _
	! fnAdd1099 - adds a 1099 to be printed/exported : _
	! fnPrint1099 - Prints all added 1099s : _
	! Chain to this program - does the same as fnPrint1099
!_
!_
 
! this is the part of the program that tells it what to do when you : _
	! chain to i - or just load and run it.
	library 'S:\Core\Library': fnprint1099,fnXit,fnTop
	dim cap$*128
	fnTop("S:\Core\Print1099",cap$="Print 1099s")
	fnprint1099(lz1$)
	fnXit
!_
!_
!_
def library fnadd1099(mat cinfo$, mat einfo$, mat box)
		dim oldbox(22)
		library 'S:\Core\Library': fngethandle,fnpause
! ___________________________
		if einfo$(1)='' then let fnpause
		open #dave=fngethandle: 'Name=1099-'&session$&'.dat,RecL=810,Use,Version=1,KFName=S:\Core\Data\Print1099.Idx,KPs=241,KLn=40',internal,outIn,keyed
		read #dave,using 'Form Pos 481,22*N 15.2',key=rpad$(einfo$(1),40),reserve: mat oldbox nokey DAVENOKEY
		mat box=box+oldbox
		rewrite #dave,using 'Form Pos 481,22*N 15.2',same: mat box
		pr 'fnAdd1099 - REWritting for '&einfo$(1) ! XXX
		goto ADD1099DONE
! ___________________________
DAVENOKEY: ! : _
		write #dave,using 'Form Pos 1,6*C 40,6*C 40,22*N 15.2': mat cinfo$,mat einfo$,mat box
		pr 'fnAdd1099 - Writting for '&einfo$(1) ! XXX
		goto ADD1099DONE
! ___________________________
ADD1099DONE: close #dave:
! reset all variables (used in this function) : _
		mat einfo$=('') : mat box=(0) : dave=0 : mat oldbox=(0) : _
		! don't reset Mat cInfo$, because it's annoying to loose these values
fnend
!_
!_
!_
def library fnprint1099(; lz1$)
		library 'S:\Core\Library': fnTop,fnopenprn,fncloseprn,fncno,fnerror,fnTop,fnXit,fncursys$,fngethandle
		on error goto PRINT1099ERTN
! ___________________________
		dim cinfo$(6)*40,einfo$(6)*40,box(22)
! cInfo$(1)*40 ! Company Name
! cInfo$(2)*40 ! Company Address (1)
! cInfo$(3)*40 ! Company Address (2)
! cInfo$(4)*40 ! Company City State Zip
! cInfo$(5)*12 ! Payer/Company Phone Number
! cInfo$(6)*12 ! Federal Identification Number
! LZ1$*1 ! 'D', 'L', or 'E' -[D]ot Matrix, [L]aser, [E]xport
! ___________________________
		if lz1$='' then lz1$='D'
! fnwait : _
		on fkey 5 goto EODAVE
		open #dave=fngethandle: 'Name=1099-'&session$&'.dat',internal,outIn ioerr Xit
		if lz1$='E' then : _
			open #exportfile=fngethandle: 'Name=\1099Etc.Wrk\W2Data\1099Dat.Prn,Replace',display,output else : _
			fnopenprn
READ_DAVE: !
		read #dave, using 'Form Pos 1,12*C 40,22*N 15.2': mat cinfo$,mat einfo$,mat box eof EODAVE
		if lz1$='E' then gosub EXPORT1099 else : _
			if lz1$='L' then gosub PRINT1099LASER else : _
				if lz1$='D' then gosub PRINT1099DOT
		goto READ_DAVE
! ___________________________
EODAVE: !
		if lz1$='E' then : _
			close #exportfile: else : _
			fncloseprn : _
			pr 'closing prn' ! XXX
		pr str$(lrec(dave))&' 1099s should have been printed.'
		close #dave,free:
		goto Xit
 
PRINT1099DOT: ! Dot Matrix 1099
		pr #255: '' : _
		pr #255: '' : _
		pr #255: '' : _
		pr #255: ''
		pr #255,using 'Form POS 7,C 30,N 11.2': cinfo$(1)(1:30),box(1)
		pr #255,using 'Form POS 7,C 30': cinfo$(2)(1:30)
		pr #255,using 'Form POS 7,C 30': cinfo$(4)(1:30)
		pr #255,using 'Form POS 37,N 11.2': box(2)
		pr #255: '' : _
		pr #255: ''
		pr #255,using 'Form POS 24,C 12,POS 37,N 11.2,N 13.2': cinfo$(5)(1:12),box(3),box(4)
		pr #255: '' : _
		pr #255: ''
		pr #255,using 'Form POS 7,2*C 15,N 11.2,N 13.2': cinfo$(6)(1:15),einfo$(1)(1:15),box(5),box(6)
		pr #255: '' : _
		pr #255: '' : _
		pr #255,using 'Form POS 7,C 30': einfo$(2)(1:30) : _
		pr #255: ''
		pr #255,using 'Form POS 7,N 11.2,N 13.2': box(7),box(8)
		pr #255: ''
		pr #255,using 'Form POS 7,C 32': einfo$(3)(1:32)
		pr #255: '' : _
		pr #255: ''
		pr #255,using 'Form POS 7,C 32': einfo$(5)(1:32)
		pr #255: '' : _
		pr #255: ''
		pr #255,using 'Form POS 7,C 8': einfo$(6)(1:8)
		for j=1 to 6 : _
			pr #255: '' : _
		next j
return
 
PRINT1099LASER: ! Laser 1099
		pr #255: ''
		pr #255,using 'Form POS 7,C 30,N 11.2': cinfo$(1)(1:30),box(1)
		pr #255,using 'Form POS 7,C 30': cinfo$(2)(1:30)
		pr #255,using 'Form POS 7,C 30': cinfo$(4)(1:30)
		pr #255,using 'Form POS 37,N 11.2': box(2)
		pr #255: '' : _
		pr #255: ''
		pr #255,using 'Form POS 24,C 12,POS 37,N 11.2,N 13.2': cinfo$(5)(1:12),box(3),box(4)
		pr #255: '' : _
		pr #255: ''
		pr #255,using 'Form POS 7,2*C 15,N 11.2,N 13.2': cinfo$(6)(1:15),einfo$(1)(1:15),box(5),box(6)
		pr #255: '' : _
		pr #255: '' : _
		pr #255: ''
		pr #255,using 'Form POS 7,C 30,N 11.2,N 13.2': einfo$(2)(1:30),box(7),box(8)
		pr #255: '' : _
		pr #255: ''
		pr #255,using 'Form POS 7,C 32': einfo$(3)(1:32)
		pr #255: '' : _
		pr #255: ''
		pr #255,using 'Form POS 7,C 32': einfo$(5)(1:32)
		pr #255: '' : _
		pr #255: ''
		pr #255,using 'Form POS 7,C 32': einfo$(6)(1:32)
		x+=1 : if x=2 then : _
			pr #255: newpage : x=0 else : _
			for j=1 to 10 : pr #255: '' : next j
return
 
EXPORT1099: !
		pr #exportfile: "01 ";" "
		pr #exportfile: "02 ";cinfo$(5)
		pr #exportfile: "03 ";cinfo$(1)
		pr #exportfile: "04 ";box(1)
		pr #exportfile: "05 ";" "
		pr #exportfile: "06 ";cinfo$(2)
		pr #exportfile: "07 ";box(2)
		pr #exportfile: "08 ";cinfo$(4)
		pr #exportfile: "09 ";box(3)
		pr #exportfile: "10 ";box(4)
		pr #exportfile: "11 ";b$(1)
		pr #exportfile: "12 ";einfo$(1)
		pr #exportfile: "13 ";box(5)
		pr #exportfile: "14 ";box(6)
		pr #exportfile: "15 ";box(7)
		pr #exportfile: "16 ";box(8)
		pr #exportfile: "17 ";einfo$(2)
		pr #exportfile: "18 ";" "
		pr #exportfile: "19 ";" "
		pr #exportfile: "20 ";box(10)
		pr #exportfile: "21 ";ad$(1)
		pr #exportfile: "22 ";ad$(2)
		pr #exportfile: "23 ";" "
		pr #exportfile: "24 ";0
		pr #exportfile: "25 ";einfo$(6)
		pr #exportfile: "26 ";" "
		pr #exportfile: "27 ";0
		pr #exportfile: "28 ";" "
		pr #exportfile: "29 ";0
		pr #exportfile: "30 ";" "
! pr #exportfile: "31 ";" "
! pr #exportfile: "32 ";0
		pr #exportfile: "*"
return
 
PRINT1099ERTN: fnerror(program$,err,line,act$,"Xit")
		if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
		execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
		pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
ERTN_EXEC_ACT: execute act$ : goto PRINT1099ERTN
 
Xit: !
fnend
 
 
 
