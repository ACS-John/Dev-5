fn_setup

dim gl$(0)*128,glN(0)
hGl=fn_openFio('GL Account',mat gl$,mat glN)

! r: get key lists
restore #hGl:
dim acctUnique$(0)*12
dim acctAll$(0)*12
mat acctUnique$(0)
mat acctAll$(0)
do
	read #hGl,using form$(hGL): mat gl$,mat glN eof GklEoGL
	fnAddOneC(mat acctUnique$,gl$(gl_account), 0,1)
	fnAddOneC(mat acctAll$,gl$(gl_account))
	fnAddOneN(mat recAll,rec(hGl))
loop
GklEoGL: !
! /r
dupesDetected=udim(mat acctAll$)-udim(mat acctUnique$)
if dupesDetected then
	dim mb$(0)*128
	mat mb$(0)
	fnAddOneC(mat mb$,str$(dupesDetected)&' duplicate accounts detected.')
	fnAddOneC(mat mb$,'Would you like to fix them now?')
	if fnMb(mat mb$, mb_question+mb_yesno)=mb_yes then
		! r: fix duplicates
		! record number order ! restore #hGl:
		acctSize=record=0
		dim acct$(0)*12
		mat acct$(0)
		do
			FixNext: !
			record+=1
			read #hGl,using form$(hGL),rec=record: mat gl$,mat glN noRec FixNext eof FixEoGL
			if srch(mat acct$,gl$(gl_account))<=0 then ! not yet encountered
				mat acct$(acctSize+=1)
				acct$(acctSize)=gl$(gl_account)
			else ! it already exists, delete it
				delete #hGl, rec=record:
				fixCount+=1
			end if
		loop until record=>lrec(hGl)
		FixEoGL: !
		mat mb$(0)
		fnAddOneC(mat mb$,str$(fixCount)&' duplicate accounts deleted.')
		fnMb(mat mb$, mb_information)
		! /r
		fnCloseFile(hGl,'GL Account')
		fnindex_sys( val(env$('cno')))
	end if
end if
goto Finis

Finis: !
goto Xit

Xit: fnXit

include: fn_open
include: fn_setup
