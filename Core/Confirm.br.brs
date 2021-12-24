def library fnConfirm(cVerb$*64; textAddition$*2048,Confirm_Dont_Ask_Again_Key$*28)
	if ~setup then fn_setup
	fnConfirm=fn_confirm(cVerb$, textAddition$,Confirm_Dont_Ask_Again_Key$)
fnend
def fn_confirm(Verb$*64; textAddition$*2048,Confirm_Dont_Ask_Again_Key$*28,___,returnN,confirmButtonAdd,confirmResponse$)
	! Verb$ - something like "confirm" or "cancel" or "delete" or "complete"
	confirmFkeyPrior=Fkey
	if Verb$='delete' then
		confirmButtonAdd=mb_button2_default
	else
		confirmButtonAdd=mb_button1_default
	end if

	dim confirmText$(0)*2048
	mat confirmText$(0)
	fnAddOneC(mat confirmText$,'Do you really want to '&lwrC$(Verb$)&'?')
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
def library fnConfirmDeleteHard(whatYouAreDeletingGeneral$*32,whatYouAreDeletingSpecific$*60; ___,returnN)
	if ~setup then fn_setup
	fnConfirmDeleteHard=fn_confirmHard('delete',whatYouAreDeletingGeneral$,whatYouAreDeletingSpecific$)
fnend
def library fnConfirmHard(chVerb$,whatYouAreVerbingGeneral$*32,whatYouAreVerbingSpecific$*60; ___,returnN,typeWord$,verb$)
	if ~setup then fn_setup
	fnConfirmHard=fn_confirmHard(chVerb$,whatYouAreVerbingGeneral$,whatYouAreVerbingSpecific$)
fnend
def fn_confirmHard(chVerb$,whatYouAreVerbingGeneral$*32,whatYouAreVerbingSpecific$*60; ___,returnN,typeWord$,verb$)
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
	ckey=fnAcs(mat resp$)
	! e$=uprc$(trim$(resp$(1)))
	if ckey<>5 and uprc$(trim$(resp$(1)))=typeWord$ then
		returnN=1
	end if
	fn_confirmHard=returnN
fnend

def library fnConfirmOpen(filename$*256,filterDesc$*128; ___,returnN,lc)
	if ~setup then fn_setup
	fnTos
	fnLbl(lc+=1,1,"**** WARNING ****",60,2)
	lc+=1
	fnLbl(lc+=1,1,'You choosen to open ',60,2)
	fnLbl(lc+=1,1,filterDesc$,60,2)
	fnLbl(lc+=1,1,'from',60,2)
	fnLbl(lc+=1,1,filename$,60,2)
	lc+=1
	fnLbl(lc+=1,1,'This action is irreversable without technical support.',60,2)
	fnLbl(lc+=1,1,'Any '&filterDesc$&' data currently open will be overwritten',60,2)
	fnLbl(lc+=1,1,'and permantly lost if not previously saved elsewhere.',60,2)
	lc+=1
	fnLbl(lc+=1,11,'Enter OPEN to continue:',23,1)
	fnTxt(lc,35,4)
	coResp$(1)=''
	fnCmdSet(2)
	ckey=fnAcs(mat coResp$)
	dim coResp$(1)*64
	if ckey<>5 and uprc$(trim$(coResp$(1)))='OPEN' then returnN=1
	fnConfirmOpen=returnN
fnend

include: fn_setup
