! r: test zone
	autoLibrary
	dim nameFormat$*20
	dim optNameFormat$(2)*20
	optNameFormat$(1)='First Name First'
	optNameFormat$(2)='Last Name First'
	nameFormat$=optNameFormat$(1)
	fncreg_write(env$('cursys')&' Name Format',nameFormat$)
	dim n1$*64,n2$*64,n3$*64
	pr 'name format: "'&nameFormat$&'"'
	dim nFull$(4)*256
	! nFull$(1)='JOINER,JR JOHNNY C.'
	! nFull$(2)='CRUZ,  BRYAN EDUARDO DEL'
	! nFull$(3)='SAN NICOLAS, CHRISTOPHER'
	! nFull$(4)='GARCIA,JR JESUS'

	nFull$(1)='CHARLES A. WADE'
	nFull$(2)='DOUG DUESTORHOFT, JR'
	nFull$(3)='JOEL J ROZNER'
	nFull$(4)='MONICA MAREK'


	for nFullItem=1 to udim(mat nFull$)
		fn_nameParse(nFull$(nFullItem),n1$,n2$,n3$,n4$)
		pr 'source name: "'&nFull$(nFullItem)&'"'
		pr '     first: "'&n1$&'"'
		pr '    middle: "'&n2$&'"'
		pr '      last: "'&n3$&'"'
		pr '    suffix: "'&n4$&'"'
	nex nFullItem
end ! /r


def library fnNameParse(fullname$*128,&nameFirst$,&nameMiddle$,&nameLast$,&nameSuffix$)
	autoLibrary
	fnNameParse=fn_nameParse(fullname$,nameFirst$,nameMiddle$,nameLast$,nameSuffix$)
fnend
def fn_nameParse(fullname$*128,&nameFirst$,&nameMiddle$,&nameLast$,&nameSuffix$; ___,nl,nameFormat$*20)
	dim optNameFormat$(2)*20
	optNameFormat$(1)='First Name First'
	optNameFormat$(2)='Last Name First'
	fncreg_read(env$('cursys')&' Name Format',nameFormat$,optNameFormat$(1))

	! pr 'fullname$='&fullname$&' (nameFormat$='&nameFormat$&')'
	! pause

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

	npNsFinis: !
	! /r
fnend

