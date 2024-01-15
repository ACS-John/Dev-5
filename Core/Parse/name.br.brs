def library fnNameParse(fullname$*128,&nameFirst$,&nameMiddle$,&nameLast$,&nameSuffix$)
	autoLibrary
	fnNameParse=fn_nameParse(fullname$,nameFirst$,nameMiddle$,nameLast$,nameSuffix$)
fnend
def fn_nameParse(fullname$*128,&nameFirst$,&nameMiddle$,&nameLast$,&nameSuffix$)
	dim nameFormat$*20
	dim optNameFormat$(2)*20
	optNameFormat$(1)='First Name First'
	optNameFormat$(2)='Last Name First'
	if env$('cursys')='GL' then 
		fncreg_read('Payee Name Format',nameFormat$,optNameFormat$(1))
	else env$('cursys')='PR' then 
		fncreg_read('Employee Name Format',nameFormat$,optNameFormat$(1))
	end if
	fullname$=uprc$(rtrm$(fullname$)): ! nameFormat$='s'
	npPosSpace1=pos(fullname$,' ',1)
	npPosSpace2=pos(fullname$,' ',npPosSpace1+1)
	if nameFormat$=optNameFormat$(1) then
		! r: first name first
		nameFirst$=fullname$(1:max(min(15,npPosSpace1-1),1))
		if npPosSpace2>0 then
			nameMiddle$=fullname$(npPosSpace1+1:npPosSpace2-1)
			nameLast$=fullname$(npPosSpace2+1:len(fullname$))
		end if
		if npPosSpace2=0 then
			nameLast$=fullname$(npPosSpace1+1:len(fullname$))
			nameMiddle$=''
		end if
		! /r
	else  ! last name first
		! r: last name first
		! npPosComma=pos(fullname$,',')
		! if npPosComma then
		!   ! r: last name, first name
		!   dim fullNameCopy$*256
		!   fullNameCopy$=fullname$
		!   nameLast$=fullNameCopy$(1:npPosComma-1)
		!   fullNameCopy$(1:npPosComma)=''
		!   fullNameCopy$=trim$(fullNameCopy$)
		!   npFncPosSpace1=pos(fullNameCopy$,' ')
		!   if npFncPosSpace1<=0 then npFncPosSpace1=len(fullNameCopy$)
		!   nameFirst$=trim$(fullNameCopy$(1:npFncPosSpace1))
		!   fullNameCopy$(1:npFncPosSpace1)=''
		!   nameMiddle$=trim$(fullNameCopy$)
		!   fullNameCopy$=''
		!   ! /r
		! else
		!   ! r: last name [space] first name
		!   ! /r
		! ! end if
		if npPosSpace1>0 and fullname$(npPosSpace1-1:npPosSpace1-1)=',' then
			nameLast$=fullname$(1:npPosSpace1-2)
		else
			nameLast$=fullname$(1:max(npPosSpace1-1,1))
		end if
		if npPosSpace2>0 then
			nameFirst$=fullname$(npPosSpace1+1:npPosSpace2-1): nameMiddle$=fullname$(npPosSpace2+1:len(fullname$))
		else ! if npPosSpace2=0 then
			nameFirst$=fullname$(npPosSpace1+1:len(fullname$)): nameMiddle$=''
		end if
		! /r
	end if
	nameFirst$=rtrm$(nameFirst$,',')
	! r: nameSuffix$ process ! JR
	nameSuffix$='' err npNsFinis
	if namefirst$='II' or namefirst$='III' then
		namesuffix$=namefirst$
		namefirst$=namemiddle$
		namemiddle$=''
	end if
	if uprc$(nameFirst$&'*')<>uprc$(srep$(nameFirst$&'*',',JR*','')) then
		nameSuffix$='JR'
		nameFirst$=rtrm$(nameFirst$)
		nameFirst$=nameFirst$(1:len(nameFirst$)-3)
	end if
	if uprc$(nameLast$&'*')<>uprc$(srep$(nameLast$&'*',',JR*','')) then
		nameSuffix$='JR'
		nameLast$=rtrm$(nameLast$)
		nameLast$=nameLast$(1:len(nameLast$)-3)
	end if
	if uprc$(nameLast$&'*')<>uprc$(srep$(nameLast$&'*',',JR.*','')) then
		nameSuffix$='JR'
		nameLast$=rtrm$(nameLast$)
		nameLast$=nameLast$(1:len(nameLast$)-4)
	end if
	! SR and II and III
	if uprc$(nameFirst$&'*')<>uprc$(srep$(nameFirst$&'*',',SR*','')) then
		nameSuffix$='SR'
		nameFirst$=rtrm$(nameFirst$)
		nameFirst$=nameFirst$(1:len(nameFirst$)-3)
	end if
	if uprc$(nameLast$&'*')<>uprc$(srep$(nameLast$&'*',',SR*','')) then
		nameSuffix$='SR'
		nameLast$=rtrm$(nameLast$)
		nameLast$=nameLast$(1:len(nameLast$)-3)
	end if
	if uprc$(nameLast$&'*')<>uprc$(srep$(nameLast$&'*',',SR.*','')) then
		nameSuffix$='SR'
		nameLast$=rtrm$(nameLast$)
		nameLast$=nameLast$(1:len(nameLast$)-4)
	end if
	if uprc$(nameFirst$&'*')<>uprc$(srep$(nameFirst$&'*',',II*','')) then
		nameSuffix$='II'
		nameFirst$=rtrm$(nameFirst$)
		nameFirst$=nameFirst$(1:len(nameFirst$)-3)
	end if
	if uprc$(nameLast$&'*')<>uprc$(srep$(nameLast$&'*',',II*','')) then
		nameSuffix$='II'
		nameLast$=rtrm$(nameLast$)
		nameLast$=nameLast$(1:len(nameLast$)-3)
	end if
	if uprc$(nameLast$&'*')<>uprc$(srep$(nameLast$&'*',',II.*','')) then
		nameSuffix$='II'
		nameLast$=rtrm$(nameLast$)
		nameLast$=nameLast$(1:len(nameLast$)-4)
	end if
	if uprc$(nameFirst$&'*')<>uprc$(srep$(nameFirst$&'*',',III*','')) then
		nameSuffix$='III'
		nameFirst$=rtrm$(nameFirst$)
		nameFirst$=nameFirst$(1:len(nameFirst$)-4)
	end if
	if uprc$(nameLast$&'*')<>uprc$(srep$(nameLast$&'*',' III*','')) then
		nameSuffix$='III'
		nameLast$=rtrm$(nameLast$)
		nameLast$=nameLast$(1:len(nameLast$)-4)
	end if
	if uprc$(nameLast$&'*')<>uprc$(srep$(nameLast$&'*',',III.*','')) then
		nameSuffix$='III'
		nameLast$=rtrm$(nameLast$)
		nameLast$=nameLast$(1:len(nameLast$)-5)
	end if
	npNsFinis: !
	! /r
fnend

