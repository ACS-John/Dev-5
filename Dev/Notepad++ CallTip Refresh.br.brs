fn_setup
fntop(program$)
fn_gatherLibraryFunctions
fn_gatherLibraryKeywords
fn_sortFunctions

fn_updateNppCallTipFile
xit: fnxit
def fn_setup
	library 'S:\Core\Library': fngethandle,fntop,fnxit,fnAddOneC,fnCopy
	dim functions$(0)*512,line$*512
fnend
def fn_gatherLibraryKeywords
	fnAddOneC(mat functions$,'str2mat(Source$,mat parsed$; mat delimiter$)')
	fnAddOneC(mat functions$,'mat2str(mat Source$,&returned$; mat delimiter$)')
	fnAddOneC(mat functions$,'abs(numeric_arg)')
	fnAddOneC(mat functions$,'aidx(mat arrayname[$])')
	fnAddOneC(mat functions$,'atn(angle)')
	fnAddOneC(mat functions$,'bell')
	fnAddOneC(mat functions$,'br_filename$(os_filename$)')
	fnAddOneC(mat functions$,'ceil(numeric_arg)')
	fnAddOneC(mat functions$,'cform$(form_statement$)')
	fnAddOneC(mat functions$,'chr$(integer_arg)')
	fnAddOneC(mat functions$,'cmdkey(; new_value)')
	fnAddOneC(mat functions$,'cnt')
	fnAddOneC(mat functions$,'cnvrt$(spec$,num_to_convert)')
	fnAddOneC(mat functions$,'code')
	fnAddOneC(mat functions$,'cos(angle)')
	fnAddOneC(mat functions$,'curCol')
	fnAddOneC(mat functions$,'curFld(; newCurField,Attribute$/newCurRow/newCurCell)')
	fnAddOneC(mat functions$,'curRow')
	fnAddOneC(mat functions$,'curTab(; window_number,activate_tab)')
	fnAddOneC(mat functions$,'curWindow(; window_number)')
	fnAddOneC(mat functions$,'date[$](; days,[*]format$)')
	fnAddOneC(mat functions$,'days(date; format$)')
	fnAddOneC(mat functions$,'debug_str(message_level,string_value$)')
	fnAddOneC(mat functions$,'decrypt$(data$; key$,encryption_type$,initialization_vector$)')
	fnAddOneC(mat functions$,'didx(mat array_name[$])')
	fnAddOneC(mat functions$,'encrypt$(data$; key$,encryption_type$,initialization_vector$)')
	fnAddOneC(mat functions$,'env$(env_var_name$)')
	fnAddOneC(mat functions$,'err')
	fnAddOneC(mat functions$,'exists(filename$)')
	fnAddOneC(mat functions$,'exp(power)')
	fnAddOneC(mat functions$,'file(filehandle)')
	fnAddOneC(mat functions$,'file$(filehandle)')
	! fnAddOneC(mat functions$,'filenum')
	! fnAddOneC(mat functions$,'fkey(; new_value)')
	! fnAddOneC(mat functions$,'fp(numeric_arg)')
	! fnAddOneC(mat functions$,'freesp(filehandle)')
	! fnAddOneC(mat functions$,'help$([*]topic$; filename$,mark)')
	! fnAddOneC(mat functions$,'hex$(string_to_hex$)')
	! fnAddOneC(mat functions$,'inf')
	! fnAddOneC(mat functions$,'int(numeric_arg)')
	! fnAddOneC(mat functions$,'ip(numeric_arg)')
	! fnAddOneC(mat functions$,'kln(filehandle; split_key_num)')
	! fnAddOneC(mat functions$,'kps(filehandle; split_key_num)')
	! fnAddOneC(mat functions$,'krec(filehandle)')
	! fnAddOneC(mat functions$,'kstat$(; num_keystrokes,wait_time)')
	! fnAddOneC(mat functions$,'len(string_to_measure$)')
	! fnAddOneC(mat functions$,'line')
	! fnAddOneC(mat functions$,'lines(filehandle)')
	! fnAddOneC(mat functions$,'linespp(filehandle)')
	! fnAddOneC(mat functions$,'log(numeric_arg)')
	! fnAddOneC(mat functions$,'login_name$')
	! fnAddOneC(mat functions$,'lpad$(string_to_pad$,desired_length; pad_char$)')
	! fnAddOneC(mat functions$,'lrec(filehandle)')
	! fnAddOneC(mat functions$,'ltrm$(string_to_trim$; trim_char$)')
	! fnAddOneC(mat functions$,'lwrc$(string_to_convert$)')
	! fnAddOneC(mat functions$,'mat2str(mat array_name$,result$; mat delimiter$, quote_type$:trim_type$)')
	! fnAddOneC(mat functions$,'max(numeric_arg1,numeric_arg2,etc,numeric_argn)')
	! fnAddOneC(mat functions$,'max$(string_arg1$,string_arg2$,etc,string_argn$)')
	! fnAddOneC(mat functions$,'min(numeric_arg1,numeric_arg2,etc,numeric_argn)')
	! fnAddOneC(mat functions$,'min$(string_arg1$,string_arg2$,etc,string_argn$)')
	! fnAddOneC(mat functions$,'mod(numerator,denominator)')
	! fnAddOneC(mat functions$,'msg("kb", string$)')
	! fnAddOneC(mat functions$,'msg$(message_text$)')
	! fnAddOneC(mat functions$,'msgbox(prompt$; title$,buttons$,icon$)')
	! fnAddOneC(mat functions$,'newpage')
	! fnAddOneC(mat functions$,'ord(char$)')
	! fnAddOneC(mat functions$,'os_filename$(filename$)')
	! fnAddOneC(mat functions$,'pi')
	! fnAddOneC(mat functions$,'pic$(currency_symbol$)')
	! fnAddOneC(mat functions$,'pos(string_arg$,[^]search_key$; [-]start_pos)')
	! fnAddOneC(mat functions$,'printer_list(mat printer_names$)')
	! fnAddOneC(mat functions$,'procin')
	! fnAddOneC(mat functions$,'program$')
	! fnAddOneC(mat functions$,'rec(filehandle)')
	! fnAddOneC(mat functions$,'rem(numerator,denominator)')
	! fnAddOneC(mat functions$,'rln(filehandle; new_rec_length)')
	! fnAddOneC(mat functions$,'rnd(; reset_flag)')
	! fnAddOneC(mat functions$,'round(num_to_round,decimals)')
	! fnAddOneC(mat functions$,'rpad$(string_to_pad$,new_length; pad_char$)')
	! fnAddOneC(mat functions$,'rpt$(string_to_repeat$,number_of_repetitions)')
	! fnAddOneC(mat functions$,'rtrm$(string_to_trim$; trim_char$)')
	! fnAddOneC(mat functions$,'serial')
	! fnAddOneC(mat functions$,'session$')
	! fnAddOneC(mat functions$,'setenv(env_var_name$,new_value$)')
	! fnAddOneC(mat functions$,'sgn(numeric_arg)')
	! fnAddOneC(mat functions$,'shift')
	! fnAddOneC(mat functions$,'sin(angle)')
	! fnAddOneC(mat functions$,'sleep(seconds)')
	! fnAddOneC(mat functions$,'sqr(numeric_arg)')
	! fnAddOneC(mat functions$,'srch(mat array_name,search_word; start_row)')
	! fnAddOneC(mat functions$,'srch(mat array_name$,search_word$; start_row)')
	! fnAddOneC(mat functions$,'srep$(orig_string$,replace_what$,replace_with$)')
	! fnAddOneC(mat functions$,'srep$(orig_string$,start_pos,replace_what$,replace_with$)')
	! fnAddOneC(mat functions$,'str$(numeric_arg)')
	! fnAddOneC(mat functions$,'str2mat(string$,mat result_array$; mat delimiter$,quote_type$:trim_type$)')
	! fnAddOneC(mat functions$,'sum(mat num_array)')
	! fnAddOneC(mat functions$,'tab(column)')
	! fnAddOneC(mat functions$,'tan(angle)')
	! fnAddOneC(mat functions$,'timer')
	! fnAddOneC(mat functions$,'time$')
	! fnAddOneC(mat functions$,'trim$(string_to_trim$; trim_char$)')
	! fnAddOneC(mat functions$,'udim(mat array_name; dimension)')
	! fnAddOneC(mat functions$,'udim(mat array_name$; dimension)')
	! fnAddOneC(mat functions$,'unhex$(string_to_unhex$$)')
	! fnAddOneC(mat functions$,'uprc$(string_to_upper$)')
	! fnAddOneC(mat functions$,'userid$')
	! fnAddOneC(mat functions$,'val(a$)')
	! fnAddOneC(mat functions$,'variable$')
	! fnAddOneC(mat functions$,'version(filehandle,new_version)')
	! fnAddOneC(mat functions$,'wbplatform$')
	! fnAddOneC(mat functions$,'wbversion$')
	! fnAddOneC(mat functions$,'wsid$')
	! fnAddOneC(mat functions$,'xlate$(a$,b$; x)')
fnend
def fn_gatherLibraryFunctions
	open #hGlf:=fngethandle: 'name=S:\Core\Library.br.brs',d,i
	do
		linput #hGlf: line$ eof eoGlf
		line$=lwrc$(line$)
		! if pos(lwrc$(line$),'def library ')>0 then pr line$ : pause
		! r: remove line number if present
			posSpace1=pos(line$,' ')
			! if val(line$(1:posSpace1))=40320 then pr 40320 : pause
			lineVal=val(line$(1:posSpace1)) conv RemvoeLineNumberFinis
			if lineVal>0 then line$(1:posSpace1)=''
		RemvoeLineNumberFinis: ! /r
		! remove all excess spaces
		do while pos(line$,'  ')>0 
			line$=srep$(line$,'  ',' ')
		loop
		if pos(line$,'!')>0 then line$=line$(1:pos(line$,'!')-1) ! remove any trailing comments (! only)
		line$=trim$(line$)
		if lwrc$(line$(1:12))='def library ' then
			line$=line$(13:len(line$))
			fnAddOneC(mat functions$,line$)
		end if
		!
	loop
	eoGlf: !
	close #hGlf:
fnend
def fn_sortFunctions
	dim functionsTmp$(0)*512
	mat functionsTmp$(udim(mat functions$))
	mat Sorted(udim(mat functions$))
	mat functionsTmp$=functions$
	mat Sorted=aidx(functions$)
	for Index=1 to udim(mat functions$)
		functions$(Index)=functionsTmp$(Sorted(Index))
	next Index
fnend
def fn_updateNppCallTipFile
	!
	! pr 'fn_updateNppCallTipFile'
	open #hNpp:=fngethandle: 'name='&env$('temp')&'\BRSource.xml,recl=1024,replace',d,o
	pr #hNpp: '<?xml version="1.0" encoding="Windows-1252" ?>'
	pr #hNpp: '<NotepadPlus>'
	pr #hNpp: '  <AutoComplete language="BR! Source">'
	pr #hNpp: '    <Environment ignoreCase="yes" startFunc="(" stopFunc=")" paramSeparator="," />'
	for functionItem=1 to udim(mat functions$)
		dim funName$*32
		dim funReturnValue$*32
		dim funParms$(0)*64
		fn_functionParse(functions$(functionItem),funName$,funReturnValue$,mat funParms$)
		if udim(mat funParms$)=0 then
			pr #hNpp: '    <KeyWord name="'&lwrc$(funName$)&'"/>'
		else
			pr #hNpp: '    <KeyWord name="'&lwrc$(funName$)&'" func="yes">'
			pr #hNpp: '      <Overload retVal="'&funReturnValue$&'" >'
			for parmItem=1 to udim(mat funParms$)
				pr #hNpp: '        <Param name="'&funParms$(parmItem)&'" />'
			nex parmItem
			pr #hNpp: '      </Overload>'
			pr #hNpp: '    </KeyWord>'
		end if
		! pr #hNpp: '    <KeyWord name="'&funName$&'('&funReturnValue$&')"/>'
		! pr #hNpp: '    <KeyWord name="'&funName$&' ('&funReturnValue$&')" func="yes">'
		! ! pr #hNpp: '      <Overload retVal="'&funReturnValue$&'" >'
		! ! pr #hNpp: '        <Param name="int i" />'
		! ! pr #hNpp: '      </Overload>'
		! pr #hNpp: '    </KeyWord>'
	next functionItem
	pr #hNpp: '  </AutoComplete>'
	pr #hNpp: '</NotepadPlus>'
	pr #hNpp: ''
	close #hNpp:
	fnCopy(env$('temp')&'\BRSource.xml','C:\ACS\Program\Notepad++\plugins\APIs\BR! Source.xml')
	fnCopy(env$('temp')&'\BRSource.xml','C:\ACS\Program\N++ Cloud Settings\BR! Source.xml')
fnend
def fn_functionParse(funLine$*512,&funName$,&funReturnValue$,mat funParms$)
	! funLine$=srep$(funLine$,'mat ','mat#')
	! funLine$=srep$(funLine$,' ','')
	! funLine$=srep$(funLine$,'mat#','mat ')
	posOpenParenthesis=pos(funLine$,'(')
	posAstrisk1=pos(funLine$,'*')
	if posOpenParenthesis>0 and posAstrisk1>0 then
		posEndName=min(posOpenParenthesis,posAstrisk1)-1
		!
	else if posOpenParenthesis>0 then
		posEndName=posOpenParenthesis-1
	else if posAstrisk1>0 then
		posEndName=posAstrisk1-1
	else
		posEndName=len(funLine$)
	end if
	!
	funName$=funLine$(1:posEndName)
	!
	if funLine$(posEndName:posEndName)='$' then
		funReturnValue$='String'
	else
		funReturnValue$='Number'
	end if
	if posOpenParenthesis>0 and posAstrisk1>0 and posOpenParenthesis>posAstrisk1 then
		funReturnValue$=funReturnValue$&' '&funLine$(posAstrisk1+1:posOpenParenthesis-1)
	end if
	pr 'funName="'&funName$&'" retVal="'&funReturnValue$&'"'
	if posOpenParenthesis<=0 then
		mat funParms$(0)
	else
		funLine$=funline$(posOpenParenthesis+1:len(funLine$)-1) 
		funLine$=srep$(funLine$,';',';,')
		str2mat(funLine$,mat funParms$,',')
	end if
fnend
