def fn_setup
	if ~setup then
		setup=1
		autoLibrary
		gosub Enum
	end if
fnend
def library fnConfirm(cVerb$*40; textAddition$*2048,Confirm_Dont_Ask_Again_Key$*28)
	if ~setup then fn_setup
	fnConfirm=fn_confirm(cVerb$, textAddition$,Confirm_Dont_Ask_Again_Key$)
fnend
def fn_confirm(Verb$*40; textAddition$*2048,Confirm_Dont_Ask_Again_Key$*28,___,returnN,confirmButtonAdd,confirmResponse$)
	! cVerb$ - something like "confirm" or "cancel" or "delete" or "complete"
	confirmFkeyPrior=Fkey
	if cVerb$='delete' then
		confirmButtonAdd=mb_button2_default
	else
		confirmButtonAdd=mb_button1_default
	end if

	dim confirmText$(0)*2048
	mat confirmText$(0)
	fnAddOneC(mat confirmText$,'Do you really want to '&Lwrc$(cVerb$)&'?')
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
	if ~setup then fn_setup
	fnConfirmDelete=fn_confirm('delete',What_You_Deleting$,Confirm_Dont_Ask_Again_Key$)
fnend
def library fnConfirmDeleteHard(whatYouAreDeletingGeneral$*20,whatYouAreDeletingSpecific$*60; ___,returnN)
	if ~setup then fn_setup
	fnConfirmDeleteHard=fn_confirmHard('delete',whatYouAreDeletingGeneral$,whatYouAreDeletingSpecific$)
fnend
def library fnConfirmHard(chVerb$,whatYouAreVerbingGeneral$*20,whatYouAreVerbingSpecific$*60; ___,returnN,typeWord$,verb$)
	if ~setup then fn_setup
	fnConfirmHard=fn_confirmHard(chVerb$,whatYouAreVerbingGeneral$,whatYouAreVerbingSpecific$)
fnend
def fn_confirmHard(chVerb$,whatYouAreVerbingGeneral$*20,whatYouAreVerbingSpecific$*60; ___,returnN,typeWord$,verb$)
	fnTos
	fnLbl(1,1,"**** WARNING ****",40,1,+455)
	fnLbl(3,1,'You have '&chVerb$&' '&whatYouAreVerbingGeneral$&":",60,2)
	fnLbl(4,1,whatYouAreVerbingSpecific$,60,2)
	fnLbl(6,1,"This action is irreversable.",60,2)
	if chVerb$='delete' then typeWord$='ERASE' else typeWord$=uprc$(chVerb$)
	col1_pos=11
	col1_len=len('Enter '&typeWord$&' to continue:')
	col2_pos=col1_pos+col1_len+1
	col2_len=len(typeWord$)
	fnLbl(9,col1_pos,'Enter '&typeWord$&' to continue:',col1_len,1)
	fnTxt(9,col2_pos,col2_len)
	resp$(1)=''
	fnCmdSet(2)
	fnAcs2(mat resp$,ckey)
	e$=uprc$(trim$(resp$(1)))
	if ckey<>5 and uprc$(trim$(resp$(1)))=typeWord$ then
		returnN=1
	end if
	fn_confirmHard=returnN
fnend
include: enum