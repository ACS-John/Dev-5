! Replace S:\acsPR\CmbJob.br
! creates a screen ace combobox for job records
def library fncmbjob(myline,mypos; addall,container,indexfile$*200)
 
		autoLibrary
		on error goto Ertn
 
		dim df$*200,if$*200
 
		if addall<>1 then addall=0
		fncno(cno)
		if addall=0 then fen$="CJob.h[cno]" else : _
			fen$="CJobALL.h[cno]"
		if indexfile$="" then if$="[Q]\PRmstr\jcindx.h[cno]" else : _
			if$=indexfile$
		fnComboF(fen$,myline,mypos,43,"[Q]\PRmstr\jcmstr.h[cno]",1,6,7,25,if$,1+addall,1,"Select from the list of jobs. To add a job, go to the Job Cost File.",container)
		indexfile$=""
		goto Xit

Xit: fnend
include: ertn
