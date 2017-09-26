10000 let fn_setup
10020 fntop(program$)
10040 fn_gatherLibraryFunctions
10060 fn_gatherLibraryKeywords
10080 fn_sortFunctions

10100 let fn_updateNppCallTipFile
10120 xit: fnxit
16000 def fn_setup
16020   library 'S:\Core\Library': fngethandle,fntop,fnxit,fnAddOneC,fnCopy
16040   dim functions$(0)*512,line$*512
16060 fnend
22000 def fn_gatherLibraryKeywords
22020   fnAddOneC(mat functions$,'str2mat(Source$,mat parsed$; mat delimiter$)')
22030   fnAddOneC(mat functions$,'mat2str(mat Source$,&returned$; mat delimiter$)')
22040  fnAddOneC(mat functions$,'abs(numeric_arg)')
22050  fnAddOneC(mat functions$,'aidx(mat arrayname[$])')
22070 fnAddOneC(mat functions$,'atn(angle)')
22080 fnAddOneC(mat functions$,'bell')
22090 fnAddOneC(mat functions$,'br_filename$(os_filename$)')
22100 fnAddOneC(mat functions$,'ceil(numeric_arg)')
22110 fnAddOneC(mat functions$,'cform$(form_statement$)')
22120 fnAddOneC(mat functions$,'chr$(integer_arg)')
22130 fnAddOneC(mat functions$,'cmdkey(; new_value)')
22140 fnAddOneC(mat functions$,'cnt')
22150 fnAddOneC(mat functions$,'cnvrt$(spec$,num_to_convert)')
22160 fnAddOneC(mat functions$,'code')
22170 fnAddOneC(mat functions$,'cos(angle)')
22180 fnAddOneC(mat functions$,'curCol')
22190 fnAddOneC(mat functions$,'curFld(; newCurField,Attribute$/newCurRow/newCurCell)')
22220 fnAddOneC(mat functions$,'curRow')
22230 fnAddOneC(mat functions$,'curTab(; window_number,activate_tab)')
22240 fnAddOneC(mat functions$,'curWindow(; window_number)')
22250 fnAddOneC(mat functions$,'date[$](; days,[*]format$)')
22270 fnAddOneC(mat functions$,'days(date; format$)')
22280 fnAddOneC(mat functions$,'debug_str(message_level,string_value$)')
22290 fnAddOneC(mat functions$,'decrypt$(data$; key$,encryption_type$,initialization_vector$)')
22300 fnAddOneC(mat functions$,'didx(mat array_name[$])')
22320 fnAddOneC(mat functions$,'encrypt$(data$; key$,encryption_type$,initialization_vector$)')
22330 fnAddOneC(mat functions$,'env$(env_var_name$)')
22340 fnAddOneC(mat functions$,'err')
22350 fnAddOneC(mat functions$,'exists(filename$)')
22360 fnAddOneC(mat functions$,'exp(power)')
22370 fnAddOneC(mat functions$,'file(filehandle)')
22380 fnAddOneC(mat functions$,'file$(filehandle)')
22390 ! fnAddOneC(mat functions$,'filenum')
22400 ! fnAddOneC(mat functions$,'fkey(; new_value)')
22410 ! fnAddOneC(mat functions$,'fp(numeric_arg)')
22420 ! fnAddOneC(mat functions$,'freesp(filehandle)')
22430 ! fnAddOneC(mat functions$,'help$([*]topic$; filename$,mark)')
22440 ! fnAddOneC(mat functions$,'hex$(string_to_hex$)')
22450 ! fnAddOneC(mat functions$,'inf')
22460 ! fnAddOneC(mat functions$,'int(numeric_arg)')
22470 ! fnAddOneC(mat functions$,'ip(numeric_arg)')
22480 ! fnAddOneC(mat functions$,'kln(filehandle; split_key_num)')
22490 ! fnAddOneC(mat functions$,'kps(filehandle; split_key_num)')
22500 ! fnAddOneC(mat functions$,'krec(filehandle)')
22510 ! fnAddOneC(mat functions$,'kstat$(; num_keystrokes,wait_time)')
22520 ! fnAddOneC(mat functions$,'len(string_to_measure$)')
22530 ! fnAddOneC(mat functions$,'line')
22540 ! fnAddOneC(mat functions$,'lines(filehandle)')
22550 ! fnAddOneC(mat functions$,'linespp(filehandle)')
22560 ! fnAddOneC(mat functions$,'log(numeric_arg)')
22570 ! fnAddOneC(mat functions$,'login_name$')
22580 ! fnAddOneC(mat functions$,'lpad$(string_to_pad$,desired_length; pad_char$)')
22590 ! fnAddOneC(mat functions$,'lrec(filehandle)')
22600 ! fnAddOneC(mat functions$,'ltrm$(string_to_trim$; trim_char$)')
22610 ! fnAddOneC(mat functions$,'lwrc$(string_to_convert$)')
22620 ! fnAddOneC(mat functions$,'mat2str(mat array_name$,result$; mat delimiter$, quote_type$:trim_type$)')
22630 ! fnAddOneC(mat functions$,'max(numeric_arg1,numeric_arg2,etc,numeric_argn)')
22640 ! fnAddOneC(mat functions$,'max$(string_arg1$,string_arg2$,etc,string_argn$)')
22650 ! fnAddOneC(mat functions$,'min(numeric_arg1,numeric_arg2,etc,numeric_argn)')
22660 ! fnAddOneC(mat functions$,'min$(string_arg1$,string_arg2$,etc,string_argn$)')
22670 ! fnAddOneC(mat functions$,'mod(numerator,denominator)')
22680 ! fnAddOneC(mat functions$,'msg("kb", string$)')
22690 ! fnAddOneC(mat functions$,'msg$(message_text$)')
22700 ! fnAddOneC(mat functions$,'msgbox(prompt$; title$,buttons$,icon$)')
22710 ! fnAddOneC(mat functions$,'newpage')
22720 ! fnAddOneC(mat functions$,'ord(char$)')
22730 ! fnAddOneC(mat functions$,'os_filename$(filename$)')
22740 ! fnAddOneC(mat functions$,'pi')
22750 ! fnAddOneC(mat functions$,'pic$(currency_symbol$)')
22760 ! fnAddOneC(mat functions$,'pos(string_arg$,[^]search_key$; [-]start_pos)')
22770 ! fnAddOneC(mat functions$,'printer_list(mat printer_names$)')
22780 ! fnAddOneC(mat functions$,'procin')
22790 ! fnAddOneC(mat functions$,'program$')
22800 ! fnAddOneC(mat functions$,'rec(filehandle)')
22810 ! fnAddOneC(mat functions$,'rem(numerator,denominator)')
22820 ! fnAddOneC(mat functions$,'rln(filehandle; new_rec_length)')
22830 ! fnAddOneC(mat functions$,'rnd(; reset_flag)')
22840 ! fnAddOneC(mat functions$,'round(num_to_round,decimals)')
22850 ! fnAddOneC(mat functions$,'rpad$(string_to_pad$,new_length; pad_char$)')
22860 ! fnAddOneC(mat functions$,'rpt$(string_to_repeat$,number_of_repetitions)')
22870 ! fnAddOneC(mat functions$,'rtrm$(string_to_trim$; trim_char$)')
22880 ! fnAddOneC(mat functions$,'serial')
22890 ! fnAddOneC(mat functions$,'session$')
22900 ! fnAddOneC(mat functions$,'setenv(env_var_name$,new_value$)')
22910 ! fnAddOneC(mat functions$,'sgn(numeric_arg)')
22920 ! fnAddOneC(mat functions$,'shift')
22930 ! fnAddOneC(mat functions$,'sin(angle)')
22940 ! fnAddOneC(mat functions$,'sleep(seconds)')
22950 ! fnAddOneC(mat functions$,'sqr(numeric_arg)')
22960 ! fnAddOneC(mat functions$,'srch(mat array_name,search_word; start_row)')
22970 ! fnAddOneC(mat functions$,'srch(mat array_name$,search_word$; start_row)')
22980 ! fnAddOneC(mat functions$,'srep$(orig_string$,replace_what$,replace_with$)')
22990 ! fnAddOneC(mat functions$,'srep$(orig_string$,start_pos,replace_what$,replace_with$)')
23000 ! fnAddOneC(mat functions$,'str$(numeric_arg)')
23010 ! fnAddOneC(mat functions$,'str2mat(string$,mat result_array$; mat delimiter$,quote_type$:trim_type$)')
23020 ! fnAddOneC(mat functions$,'sum(mat num_array)')
23030 ! fnAddOneC(mat functions$,'tab(column)')
23040 ! fnAddOneC(mat functions$,'tan(angle)')
23050 ! fnAddOneC(mat functions$,'timer')
23060 ! fnAddOneC(mat functions$,'time$')
23070 ! fnAddOneC(mat functions$,'trim$(string_to_trim$; trim_char$)')
23080 ! fnAddOneC(mat functions$,'udim(mat array_name; dimension)')
23090 ! fnAddOneC(mat functions$,'udim(mat array_name$; dimension)')
23100 ! fnAddOneC(mat functions$,'unhex$(string_to_unhex$$)')
23110 ! fnAddOneC(mat functions$,'uprc$(string_to_upper$)')
23120 ! fnAddOneC(mat functions$,'userid$')
23130 ! fnAddOneC(mat functions$,'val(a$)')
23140 ! fnAddOneC(mat functions$,'variable$')
23150 ! fnAddOneC(mat functions$,'version(filehandle,new_version)')
23160 ! fnAddOneC(mat functions$,'wbplatform$')
23170 ! fnAddOneC(mat functions$,'wbversion$')
23180 ! fnAddOneC(mat functions$,'wsid$')
23190 ! fnAddOneC(mat functions$,'xlate$(a$,b$; x)')
23200 fnend
24000 def fn_gatherLibraryFunctions
24020   open #hGlf:=fngethandle: 'name=S:\Core\Library.br.brs',d,i
24040   do
24060     linput #hGlf: line$ eof eoGlf
24080     ! if pos(lwrc$(line$),'def library ')>0 then pr line$ : pause
24100     posSpace1=pos(line$,' ')
24120     ! remove line number if present
24140     ! if val(line$(1:posSpace1))=40320 then pr 40320 : pause
24160     if val(line$(1:posSpace1))>0 then line$(1:posSpace1)=''
24180     ! remove all excess spaces
24200     do while pos(line$,'  ')>0 
24220       line$=srep$(line$,'  ',' ')
24240     loop
24260     if pos(line$,'!')>0 then line$=line$(1:pos(line$,'!')-1) ! remove any trailing comments (! only)
24280     line$=trim$(line$)
24300     if lwrc$(line$(1:12))='def library ' then
24320       line$=line$(13:len(line$))
24360       fnAddOneC(mat functions$,line$)
24380     end if
24400     !
24420   loop
24440   eoGlf: !
24460   close #hGlf:
24480 fnend
30000 def fn_sortFunctions
30040   dim functionsTmp$(0)*512
30060   mat functionsTmp$(udim(mat functions$))
30080   mat Sorted(udim(mat functions$))
30100   mat functionsTmp$=functions$
30120   mat Sorted=aidx(functions$)
30140   for Index=1 to udim(mat functions$)
30160      let functions$(Index)=functionsTmp$(Sorted(Index))
30180   next Index
30240 fnend
42000 def fn_updateNppCallTipFile
42020   !
42040   ! pr 'fn_updateNppCallTipFile'
42060   open #hNpp:=fngethandle: 'name='&env$('temp')&'\BRSource.xml,recl=1024,replace',d,o
42080   pr #hNpp: '<?xml version="1.0" encoding="Windows-1252" ?>'
42100   pr #hNpp: '<NotepadPlus>'
42120   pr #hNpp: '  <AutoComplete language="BR! Source">'
42140   pr #hNpp: '    <Environment ignoreCase="yes" startFunc="(" stopFunc=")" paramSeparator="," />'
42160   for functionItem=1 to udim(mat functions$)
42180     dim funName$*32
42200     dim funReturnValue$*32
42220     dim funParms$(0)*64
42240     fn_functionParse(functions$(functionItem),funName$,funReturnValue$,mat funParms$)
42260     if udim(mat funParms$)=0 then
42280       pr #hNpp: '    <KeyWord name="'&lwrc$(funName$)&'"/>'
42300     else
42320       pr #hNpp: '    <KeyWord name="'&lwrc$(funName$)&'" func="yes">'
42340       pr #hNpp: '      <Overload retVal="'&funReturnValue$&'" >'
42360       for parmItem=1 to udim(mat funParms$)
42380         pr #hNpp: '        <Param name="'&funParms$(parmItem)&'" />'
42400       nex parmItem
42420       pr #hNpp: '      </Overload>'
42440       pr #hNpp: '    </KeyWord>'
42460     end if
42480     ! pr #hNpp: '    <KeyWord name="'&funName$&'('&funReturnValue$&')"/>'
42500     ! pr #hNpp: '    <KeyWord name="'&funName$&' ('&funReturnValue$&')" func="yes">'
42520     ! ! pr #hNpp: '      <Overload retVal="'&funReturnValue$&'" >'
42540     ! ! pr #hNpp: '        <Param name="int i" />'
42560     ! ! pr #hNpp: '      </Overload>'
42580     ! pr #hNpp: '    </KeyWord>'
42600   next functionItem
42620   pr #hNpp: '  </AutoComplete>'
42640   pr #hNpp: '</NotepadPlus>'
42660   pr #hNpp: ''
42680   close #hNpp:
42700   fnCopy(env$('temp')&'\BRSource.xml','C:\Program Files\Notepad++\plugins\APIs\BR! Source.xml')
42720 fnend
44000 def fn_functionParse(funLine$*512,&funName$,&funReturnValue$,mat funParms$)
44018   ! funLine$=srep$(funLine$,'mat ','mat#')
44020   ! funLine$=srep$(funLine$,' ','')
44022   ! funLine$=srep$(funLine$,'mat#','mat ')
44040   posOpenParenthesis=pos(funLine$,'(')
44060   posAstrisk1=pos(funLine$,'*')
44080   if posOpenParenthesis>0 and posAstrisk1>0 then
44100     posEndName=min(posOpenParenthesis,posAstrisk1)-1
44120     !
44140   else if posOpenParenthesis>0 then
44160     posEndName=posOpenParenthesis-1
44180   else if posAstrisk1>0 then
44200     posEndName=posAstrisk1-1
45000   else
45020     posEndName=len(funLine$)
45040   end if
45060   !
45080   funName$=funLine$(1:posEndName)
45100   !
45120   if funLine$(posEndName:posEndName)='$' then
45140     funReturnValue$='String'
45160   else
45180     funReturnValue$='Number'
45200   end if
45220   if posOpenParenthesis>0 and posAstrisk1>0 and posOpenParenthesis>posAstrisk1 then
45240     funReturnValue$=funReturnValue$&' '&funLine$(posAstrisk1+1:posOpenParenthesis-1)
45260   end if
45280   pr 'funName="'&funName$&'" retVal="'&funReturnValue$&'"'
45300   if posOpenParenthesis<=0 then
45320     mat funParms$(0)
45340   else
45360     funLine$=funline$(posOpenParenthesis+1:len(funLine$)-1) 
45380     funLine$=srep$(funLine$,';',';,')
45400     str2mat(funLine$,mat funParms$,',')
45420   end if
45440 fnend
