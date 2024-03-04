! r: test zone
	autoLibrary
	dim nameFormat$*20
	dim optNameFormat$(2)*20
	optNameFormat$(1)='First Name First'
	optNameFormat$(2)='Last Name First'
	nameFormat$=optNameFormat$(2)
	fncreg_write(env$('cursys')&' Name Format',nameFormat$)
	dim n1$*64,n2$*64,n3$*64
	pr 'name format: "'&nameFormat$&'"'
	dim nFull$(0)*256
	fnAddOneC(mat nFull$,'JOINER,JR JOHNNY C.')
	fnAddOneC(mat nFull$,'CRUZ,  BRYAN EDUARDO DEL')
	fnAddOneC(mat nFull$,'SAN NICOLAS, CHRISTOPHER')
	fnAddOneC(mat nFull$,'GARCIA,JR JESUS')
	fnAddOneC(mat nFull$,'CHARLES A. WADE')
	fnAddOneC(mat nFull$,'DOUG DUESTORHOFT, JR')
	fnAddOneC(mat nFull$,'JOEL J ROZNER')
	fnAddOneC(mat nFull$,'MONICA MAREK')
	fnAddOneC(mat nFull$,'SANTOS MARTINEZ, ALEX J.')
	fnAddOneC(mat nFull$,'JOHNSON,II. DAVID E.')
	fnAddOneC(mat nFull$,'GIBSON III, JOHN L.')
	fnAddOneC(mat nFull$,'GUZMAN MONTES, JUAN B.')
	fnAddOneC(mat nFull$,'first last')
	fnAddOneC(mat nFull$,'first m last')
	fnAddOneC(mat nFull$,'first middle last')
	fnAddOneC(mat nFull$,'last,first middle')
	fnAddOneC(mat nFull$,'last,first m')
	fnAddOneC(mat nFull$,'last,first m.')
	fnAddOneC(mat nFull$,'last, first middle')
	fnAddOneC(mat nFull$,'last, first m')
	fnAddOneC(mat nFull$,'last, first m.')
	fnAddOneC(mat nFull$,'last,  first  m.')


	for nFullItem=1 to udim(mat nFull$)
		pr rpad$('source name: "'&nFull$(nFullItem)&'"',50);
		fn_nameParse2(nFull$(nFullItem),n1$,n2$,n3$,n4$)
		pr 'becomes  ['&n1$&'] ['&n2$&'] ['&n3$&'] ['&n4$&']'
	nex nFullItem
end ! /r


def library fnNameParse(fullname$*128,&nameFirst$,&nameMiddle$,&nameLast$,&nameSuffix$)
	autoLibrary
	fnNameParse=fn_nameParse2(fullname$,nameFirst$,nameMiddle$,nameLast$,nameSuffix$)
	! fnNameParse=fn_nameParse(fullname$,nameFirst$,nameMiddle$,nameLast$,nameSuffix$)
fnend
def fn_nameParse2(name$*256,&first$,&m$,&last$,&suffix$; ___,posComma,lastFirst,which,x)
	posComma=pos(name$,',')
	if posComma then lastFirst=1
	first$=m$=last$=suffix$=''
	name$=srep$(name$,'.','')
	name$=srep$(name$,',',', ')
	do while pos(name$,'  ')>0
		name$=srep$(name$,'  ',' ')
	loop
	dim word$(0)*256
	str2mat(srep$(srep$(name$,',',' '),'  ',' '),mat word$, ' ')
	

	
	! r: find suffix$ and remove it from mat word$
		dim sfxs$(0)*10
		str2mat('JR SR II III IV',mat sfxs$,' ')
		for x=1 to udim(mat sfxs$)
			which=srch(mat word$,sfxs$(x))
			if which>0 then
				suffix$=word$(which)
				fnArrayItemRemoveC(mat word$,which)
				goto NpForXxit
			end if
		nex x
		NpForXxit: !
	! /r

	if lastFirst then
		name$=fn_mat2str$(mat word$)

		if udim(mat word$)=2 then
			first$=word$(2)
			last$=word$(1)
			m$=''
			goto NpFinis
		else if udim(mat word$)=3 then
			last$=word$(1)
			first$=word$(2)
			m$=word$(3)
			goto NpFinis
		else if udim(mat word$)=4 then
			if posComma=>(len(word$(1))+len(word$(2))) then
				last$=word$(1)&' '&word$(2)
				first$=word$(3)
			else
				last$=word$(1)
				first$=word$(2)&' '&word$(3)
			end if
			m$=word$(4)
			goto NpFinis
		else if udim(mat word$)=5 then
			last$=word$(1)&' '&word$(2)
			first$=word$(3)&' '&word$(4)
			m$=word$(5)
			goto NpFinis
		else
			pr 'udim(mat word$)=';udim(mat word$)
			pause
		end if
pr '...'
pr name$
pause
	else
		overrideNameFormat$='First Name First'
		! first$=m$=last$=suffix$='x' ! 
		fn_nameParse(name$,first$,m$,last$,suffix$)
		overrideNameFormat$=''
	end if
	
	first$=trim$(first$)
	
	if m$(len(m$):len(m$))='.' then m$(len(m$):len(m$))=''
	NpFinis: !
fnend
def fn_mat2str$*256(mat word$; delim$,return$*256)
	if delim$='' then delim$=' '
	mat2str(mat word$,return$, delim$)
	fn_mat2str$=return$
fnend
dim overrideNameFormat$*20
def fn_nameParse(fullname$*128,&nameFirst$,&nameMiddle$,&nameLast$,&nameSuffix$; ___,nl,nameFormat$*20,posComma)
	dim optNameFormat$(2)*20
	optNameFormat$(1)='First Name First'
	optNameFormat$(2)='Last Name First'
	if setupSys$<>env$('cursys') then
		setupSys$=env$('cursys')
		fncreg_read(env$('cursys')&' Name Format',nameFormat$,optNameFormat$(1))
	end if
	if overrideNameFormat$<>'' then nameFormat$=overrideNameFormat$
	! pr 'fullname$='&fullname$&' (nameFormat$='&nameFormat$&')'
	! pause

	fullname$=uprc$(fullname$)
	! fullname$=uprc$(rtrm$(fullname$)): ! nameFormat$='s'
	fullname$=trim$(fullname$)
	do
		fullname$=srep$(fullname$,'  ',' ')
	loop while pos(fullname$,'  ')>0
	str2mat(fullname$,mat word$, ' ')
	npPosSpace1=pos(fullname$,' ',1)
	npPosSpace2=pos(fullname$,' ',npPosSpace1+1)
	posComma=pos(fullname$,',')

	! r: if last name first, lets try just turn it around
	! if nameFormat$=optNameFormat$(2) and posComma>0 then
	! 	nameFormat$=optNameFormat$(1)
	! 	fullname$=fullname$(posComma+1:len(rtrm$(fullname$)))&' '&fullname$(1:posComma-1)
	! end if
	! pr fullname$ : pause
	! /r

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
	else ! nameFormat$=optNameFormat$(2) then ! last name first
		! r: last name first
			npPosComma=pos(fullname$,',')
			if npPosComma then
				! r: last name, first name
				dim fullNameCopy$*256
				fullNameCopy$=fullname$
				nameLast$=fullNameCopy$(1:npPosComma-1)
				fullNameCopy$(1:npPosComma)=''
				fullNameCopy$=trim$(fullNameCopy$)
				npFncPosSpace1=pos(fullNameCopy$,' ')
				if npFncPosSpace1<=0 then npFncPosSpace1=len(fullNameCopy$)
				nameFirst$=trim$(fullNameCopy$(1:npFncPosSpace1))
				fullNameCopy$(1:npFncPosSpace1)=''
				nameMiddle$=trim$(fullNameCopy$)
				fullNameCopy$=''
			end if
		! /r
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
	nameFirst$=rtrm$(nameFirst$)

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
	nl=len(nameFirst$)
	if uprc$(nameFirst$)(nl-3:nl)=',JR.' then
		nameSuffix$='JR'
		nameFirst$(nl-3:nl)=''
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
	if uprc$(nameFirst$&'*')<>uprc$(srep$(nameFirst$&'*',',II.*','')) then
		nameSuffix$='II'
		nameFirst$=rtrm$(nameFirst$)
		nameFirst$=nameFirst$(1:len(nameFirst$)-4)
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

	if lwrc$(nameLast$)='jr' or lwrc$(nameLast$)='sr' or lwrc$(nameLast$)='ii' or lwrc$(nameLast$)='iii' then ! in case of suffix but not a middle name
		nameSuffix$=nameLast$
		nameLast$=nameMiddle$
		nameMiddle$=''
	end if
	nameLast$=rtrm$(nameLast$,',')
	nameMiddle$=srep$(nameMiddle$,',','')
	nameMiddle$=srep$(nameMiddle$,'.','')
	npNsFinis: !
	! /r
fnend



