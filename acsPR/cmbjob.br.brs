def library fncmbjob(myline,mypos; addall,container,indexfile$*200,___,if$*200)
	autoLibrary
	if addall<>1 then addall=0
	if addall=0 then fen$='CJob.h[cno]' else fen$='CJobALL.h[cno]'
	if indexfile$='' then if$='[Q]\PRmstr\jcindx.h[cno]' else if$=indexfile$
	fncmbjob=fnComboF(fen$,myline,mypos,43,'[Q]\PRmstr\jcmstr.h[cno]',1,6,7,25,if$,1+addall,1,'Select from the list of jobs. To add a job, go to the Job Cost File.',container)
	indexfile$=''
fnend
