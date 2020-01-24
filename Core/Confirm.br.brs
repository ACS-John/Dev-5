def fn_setup
	if ~setup then
		setup=1
		library 'S:\Core\Library': fnMsgBox
		library 'S:\Core\Library': fnAddOneC
		library 'S:\Core\Library': fnTos,fnLbl,fnTxt,fnCmdSet,fnAcs
		gosub Enum
	end if
fnend
def library fnConfirm(Verb$*40; textAddition$*2048,Confirm_Dont_Ask_Again_Key$*28)
	if ~setup then let fn_setup
	fnConfirm=fn_confirm(Verb$, textAddition$,Confirm_Dont_Ask_Again_Key$)
fnend
def fn_confirm(Verb$*40; textAddition$*2048,Confirm_Dont_Ask_Again_Key$*28,___,returnN,confirmButtonAdd,confirmResponse$)
	! Verb$ - something like "confirm" or "cancel" or "delete" or "complete"
	confirmFkeyPrior=Fkey
	if verb$='delete' then
		confirmButtonAdd=mb_button2_default
	else
		confirmButtonAdd=mb_button1_default
	end if

	dim confirmText$(0)*2048
	mat confirmText$(0)
	fnAddOneC(mat confirmText$,'Do you really want to '&Lwrc$(Verb$)&'?')
	if textAddition$<>'' then
		fnAddOneC(mat confirmText$,'')
		fnAddOneC(mat confirmText$,textAddition$)
	end if


	fnMsgBox(mat confirmText$, confirmResponse$,'',mb_Question+mb_Yesno+confirmButtonAdd)

	fkey(confirmFkeyPrior)
	if confirmResponse$='Yes' then returnN=1
	! if mb_Repsonse=mb_Yes then returnN=1
	fn_confirm=returnN
fnend
def library fnConfirmDelete(What_You_Deleting$*60; Confirm_Dont_Ask_Again_Key$*28)
	if ~setup then let fn_setup
	fnConfirmDelete=fn_confirm('delete',What_You_Deleting$,Confirm_Dont_Ask_Again_Key$)
fnend
def library fnConfirmDeleteHard(whatYouAreDeletingGeneral$*20,whatYouAreDeletingSpecific$*60; ___,returnN)
	if ~setup then let fn_setup
	companyDeleteReturn=0
	fnTos
	fnLbl(1,1,"**** WARNING ****",40,1,5)
	fnLbl(3,1,"You have chosen to delete "&whatYouAreDeletingGeneral$&":",60,2)
	fnLbl(4,1,whatYouAreDeletingSpecific$,60,2)
	fnLbl(6,1,"This action is irreversable.",60,2)
	fnLbl(9,11,"Enter ERASE to continue:",24,1)
	fnTxt(9,36,5)
	resp$(1)=''
	fnCmdSet(2)
	fnAcs(sn$,0,mat resp$,ckey)
	e$=uprc$(trim$(resp$(1)))
	if ckey<>5 and uprc$(trim$(resp$(1)))="ERASE" then
		returnN=1
	end if
	fnConfirmDeleteHard=returnN
fnend
include: enum