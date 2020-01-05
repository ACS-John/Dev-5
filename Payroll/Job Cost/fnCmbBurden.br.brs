! Replace S:\acsPR\CmbBurden.br
! creates a screen ace combobox for personnel burden
	def library fncmbburden(myline,mypos; addall,container,indexfile$*200)
! ______________________________________________________________________
		library 'S:\Core\Library': fncno,fncombof,fnerror
		on error goto Ertn
! ______________________________________________________________________
		dim df$*200,if$*200
! ______________________________________________________________________
		if addall<>1 then addall=0
		fncno(cno)
		if addall=0 then 
			fen$="Cburden.h[cno]" 
		else 
			fen$="CburdenALL.h[cno]"
		end if
		if indexfile$="" then 
			if$="[Q]\PRmstr\burdenidx.h[cno]" 
		else 
			if$=indexfile$
		end if
		fncombof(fen$,myline,mypos,43,"[Q]\PRmstr\burden.h[cno]",1,8,9,30,if$,1+addall,0,"Select from the list of personnel burden records. To add a personnel burden record, take the Add option.",container)
		indexfile$=""
		goto XIT
	XIT: !
fnend
include: ertn

