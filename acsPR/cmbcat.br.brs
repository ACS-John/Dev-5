! Replace S:\acsPR\CmbCat.br
! creates a screen ace combobox for category records
def library fncmbcat(myline,mypos; addall,container,indexfile$*200)
 
		autoLibrary
		on error goto Ertn
 
		dim df$*200,if$*200
 
		if addall<>1 then addall=0
		fncno(cno)
		if addall=0 then fen$="Ccat.h[cno]" else : _
			fen$="CcatALL.h[cno]"
		if indexfile$="" then if$="[Q]\PRmstr\catindx.h[cno]" else : _
			if$=indexfile$
		fncombof(fen$,myline,mypos,43,"[Q]\PRmstr\jccat.h[cno]",1,11,12,25,if$,1+addall,1,"Select from the list of categories. To add a category record, go to the Category File.",container)
		indexfile$=""
		goto Xit
 
include: ertn
 
Xit: fnend
 
