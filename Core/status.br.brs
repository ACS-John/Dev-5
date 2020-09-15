def fn_setup
	setup=1
	autoLibrary
	on error goto Ertn
fnend
def library fnStatus(text$*512)
	if ~setup then fn_setup
	fnStatus=fn_status(text$)
fnend
def fn_status(text$*512)
	if ~status_initialized or file$(hStatusWin)<>':CON:' or hStatusWin=0 then
		status_initialized=1
		dim headings$(1)*40,widths(1),forms$(1)*40,status_gridspec$*80
		open #hStatusWin:=fnH: 'SRow=1,SCol=1,Rows=20,Cols=80,Parent=None,Caption=Status',display,output
		status_gridspec$='#'&str$(hStatusWin)&',1,1,List 20/80'
		headings$(1)='Status'
		widths(1)=80
		forms$(1)='C 512'
		pr f status_gridspec$&",headers,[gridheaders]": (mat headings$,mat widths, mat forms$)
	end if
	text$=fnSrepEnv$(text$)
	if env$('ACSDeveloper')<>'' then
		pr f status_gridspec$&",+": text$(1:512)
	else
		pr f status_gridspec$&",+": text$(1:512) error ignore
	end if

	input fields status_gridspec$&",rowcnt,all,nowait": grid_rows
	curfld(1,grid_rows+1)

fnend

def library fnStatusPause
	if ~setup then fn_setup
	fn_status('Press any key to continue.')
	kstat$(1)
fnend

def library fnStatusClose
	if ~setup then fn_setup
	close #hStatusWin: ioerr ignore
	hStatusWin=0
	status_initialized=0
Xit: ! necessary for ertn
fnend
include: Ertn