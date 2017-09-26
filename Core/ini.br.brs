10000 ! r: test zone
10010   ! on error goto ERTN
10020   fn_setup
10100   library program$: fnIniOpen,fnIniRead$,fnIniSet,fnIniWrite
10200   fnIniOpen('acs.ini')
10300   fnIniSet('core','version',wbversion$)
10400   fnIniSet('core','tom','tomset')
10500   fnIniSet('acsPR','dave','daveset')
10600   fnIniWrite
10700   print fnIniRead$('core','version')
10800   pause  ! fnIniOpen('acs.ini')
10900 ! fnIniSet('core','
11000   end 
11100 ! 
11300 ! ERTN: ! r:
11400 !   execute 'list -'&str$(line)
11500 !   print 'Error '&str$(err)&' here';bell
11600 !   pause 
11700 ! ! ! /r
11720 ! /r
30000 def fn_setup
30020   if ~setup then
30040     setup=1
30060     library 'S:\Core\Library': fngethandle,fnarray_item_insert$,fnAddOneC,fnsrch_case_insensitive,fnCopy
30080   end if
30100 fnend
40000 def library fnIniOpen(ii_file$*256) ! ,MAT INI_SECTION$, MAT INI_FIELD$, MAT INI_VALUE$)
40020   if ~setup then let fn_setup
40040   if ~setup_fnIniOpen then 
40060     dim ii_line$(1)*2048,ii_section$(1)*1024,ii_section$*1024,ii_value$*1024,ini_file$*256,ini_field$*256
40080     dim ini_section$(1)*1024, ini_field$(1)*256, ini_value$(1)*256
40100     let setup_fnIniOpen=1
40120   end if 
40140   let ini_file$=trim$(ii_file$)
40160   let ini_field_len_longest=0
40180   mat ii_line$(0)
40200   mat ini_section$(0) : mat ini_field$(0) : mat ini_value$(0)
40220   open #h_ini:=fngethandle: 'Name='&ini_file$,display,input ioerr INI_OPEN_FAIL
40240   do 
40260     mat ii_line$(udim(mat ii_line$)+1)
40280     linput #h_ini: ii_line$(udim(mat ii_line$)) eof INI_OPEN_EOF
40300   loop until file(h_ini)
40320   INI_OPEN_EOF: ! 
40340   close #h_ini: 
40360   mat ii_line$(udim(mat ii_line$)-1) ! if udim(mat ii_line$)=1 and ii_line$(1)="" then mat ii_line$(0)
40380   mat ii_section$(udim(ii_line$))=("")
40400   for ii_item=1 to udim(mat ii_line$)
40420     if ii_line$(ii_item)(1:1)='[' then 
40440       let ii_line$(ii_item)=trim$(ii_line$(ii_item))
40460       let ii_section$=uprc$(trim$(trim$(trim$(ii_line$(ii_item)),"["),"]"))
40480       let ii_section$(ii_item)=ii_section$
40500     else if ii_line$(ii_item)(1:1)='!' or ii_line$(ii_item)(1:1)=';' or ii_line$(ii_item)(1:1)='#' then 
40520       let ii_section$(ii_item)=ii_section$
40540     else 
40560       let ii_section$(ii_item)=ii_section$
40580       let fnAddOneC(mat ini_section$,ii_section$)
40600       let ii_pos_equal=pos(ii_line$(ii_item),"=")
40620       if ii_pos_equal=-1 then let ii_pos_equal=pos(ii_line$(ii_item),tab$) ! if no equals sign than use a tab instead
40640       let ini_field$=trim$(trim$(trim$(trim$(ii_line$(ii_item)(1:ii_pos_equal-1),tab$),'"')),tab$)
40660       let fnAddOneC(mat ini_field$,ini_field$)
40680       let ini_field_len_longest=max(ini_field_len_longest,ii_pos_equal-1)
40700       let ii_value$=trim$(trim$(ii_line$(ii_item)(ii_pos_equal+1:len(ii_line$(ii_item)))),tab$)
40720       if lwrc$(ii_value$)="blank" then let ii_value$=''
40740       let fnAddOneC(mat ini_value$,ii_value$)
40760     end if  ! II_Line$(II_Item)(1:1)=     '['     /  '!'   /     else 
40780   next ii_item
40800   INI_OPEN_FAIL: ! 
40820 fnend 
42000 def library fnIniRead$*256(il_section$*256,il_field$*256) ! shares MAT INI_Section$, MAT INI_Field$, MAT INI_Value$ with fnIniOpen
42020   if ~setup then let fn_setup
42040   let tab$=chr$(9)
42060   mat empty$(0)
42080   let fnIniRead$=fn_iniRead$(il_section$,il_field$)
42100 fnend  ! fnIniRead$
44000 def fn_iniRead$*256(il_section$*256,il_field$*256) ! shares MAT INI_Section$, MAT INI_Field$, MAT INI_Value$ with fnIniOpen
44020   dim set_fn_iniRead$*256
44040   set_fn_iniRead$=""
44060   il_field$=trim$(il_field$)
44080   if il_field$="" then il_field$='blank'
44100   il_section$=trim$(trim$(uprc$(il_section$),"]"),"[")
44120   il_section_first=fnsrch_case_insensitive(mat ini_section$, il_section$)
44140   if il_section_first>0 then 
44160     match=fnsrch_case_insensitive(mat ini_field$,il_field$,il_section_first)
44180     if match>0 then 
44200       if ini_section$(match)=il_section$ then 
44220         set_fn_iniRead$=trim$(ini_value$(match),tab$)
44240       end if  ! INI_Section$(Match)=IL_Section$
44260     else 
44280       set_fn_iniRead$=""
44300     end if  ! Match>0   /   else 
44320   else 
44340     il_field$=""
44360   end if  ! IL_Section_First>0   /   else 
44380   fn_iniRead$=set_fn_iniRead$
44400 fnend 
46000 def library fnIniSet(inis_section$*256,inis_field$*256,inis_value$*256)
46020   if ~setup then let fn_setup
46040   let fnIniSet=fn_iniSet(inis_section$,inis_field$,inis_value$)
46060 fnend
48000 def fn_iniSet(inis_section$*256,inis_field$*256,inis_value$*256)
48020   ! shares MAT INI_Section$, MAT INI_Field$, MAT INI_Value$, Mat II_Section$, INI_Field_Len_Longest with fnIniOpen
48040   inis_field$=trim$(inis_field$)
48060   if inis_field$="" then let inis_field$=''
48080   inis_section$=trim$(trim$(inis_section$,"]"),"[")
48100   inis_section_first=fnsrch_case_insensitive(mat ini_section$, inis_section$)
48120   if inis_section_first>0 then 
48140     inis_field_item=fnsrch_case_insensitive(mat ini_field$,inis_field$,inis_section_first)
48160     if inis_field_item>0 then 
48180       if ini_section$(inis_field_item)=uprc$(inis_section$) then 
48200         ini_value$(inis_field_item)=inis_value$
48220       end if  ! INI_Section$(INIS_Field_Item)=INIS_Section$
48240     else 
48260     !     pr ' ** need a new field (existing section) **' : pr 'Section='&INIS_SECTION$ : pr '  Field='&INIS_FIELD$ : pr '  Value='&INIS_Value$ : pr 'now go add it!!!' : pause ! insert into Mat II_Section$, Mat II_Line$, Mat INI_Section$, Mat INI_Field$, Mat INI_Value$
48280       insert_item_number=inis_section_first
48300       do  ! set Insert_Item_Number to be the last of each section
48320         insert_item_number+=1
48340       loop until insert_item_number>udim(ini_section$) or uprc$(inis_section$)<>ini_section$(insert_item_number)
48360       ini_field_len_longest=max(ini_field_len_longest,len(inis_field$))
48380       fnarray_item_insert$(mat ii_section$,inis_section$,insert_item_number)
48400       fnarray_item_insert$(mat ii_line$,rpad$(inis_field$,ini_field_len_longest)&" = "&inis_value$,insert_item_number)
48420       fnarray_item_insert$(mat ini_section$,uprc$(inis_section$),insert_item_number)
48440       fnarray_item_insert$(mat ini_field$,inis_field$,insert_item_number)
48460       fnarray_item_insert$(mat ini_value$,inis_value$,insert_item_number)
48480     end if  ! INIS_Field_Item>0   /   else 
48500   else 
48520     !      pr ' ** need a new section and a new field in it **' : pr 'Section='&INIS_SECTION$ : pr '  Field='&INIS_FIELD$ : pr '  Value='&INIS_Value$ ! insert into Mat II_Section$, Mat II_Line$, Mat INI_Section$, Mat INI_Field$, Mat INI_Value$
48540     ini_field_len_longest=max(ini_field_len_longest,len(inis_field$))
48560     fnAddOneC(mat ii_section$,inis_section$)
48580     fnAddOneC(mat ii_section$,inis_section$)
48600     fnAddOneC(mat ii_line$,"["&inis_section$&"]")
48620     fnAddOneC(mat ii_line$,rpad$(inis_field$,ini_field_len_longest)&" = "&inis_value$)
48640     fnAddOneC(mat ini_section$,uprc$(inis_section$))
48660     fnAddOneC(mat ini_field$,inis_field$)
48680     fnAddOneC(mat ini_value$,inis_value$)
48700   end if  ! INIS_Section_First>0   /   else 
48720 fnend 
50000 def library fnIniWrite
50020   if ~setup then let fn_setup
50040   !  uses (built by fnIniOpen): mat ii_line$, mat ini_section$, mat ini_field$, mat ini_value$, tab$, ini_file$
50060   dim inir_line$*2048
50080   dim inir_field$*256,inir_value$*256
50100   dim inir_section$*1024
50120   dim inir_temp_file$*1024
50140   open #inir_temp_handle:=fngethandle: "Name="&env$("Temp")&"\inir-"&session$&".ini,RecL=2048,Replace",display,output 
50160   let inir_temp_file$=file$(inir_temp_handle)
50180   if ini_file$="" then 
50200     if env$('ACSDeveloper')<>'' then 
50220       print "INI_File$ is empty.  fn_ini_write will abort.  You must specifying a file via fnIniOpen, before calling this function."
50240       pause 
50260     end if 
50280     goto inirXIT
50300   end if  ! INI_File$=""
50320   for ii_item=1 to udim(mat ii_line$)
50340     if ii_line$(ii_item)(1:1)='[' then 
50360       let inir_section$=uprc$(trim$(trim$(trim$(ii_line$(ii_item)),"["),"]"))
50380       let inir_line$=ii_line$(ii_item)
50400       gosub INIR_WRITE
50420     else if ii_line$(ii_item)(1:1)='!' or ii_line$(ii_item)(1:1)=';' or ii_line$(ii_item)(1:1)='#' or trim$(ii_line$(ii_item))="" then 
50440       let inir_line$=ii_line$(ii_item)
50460       gosub INIR_WRITE
50480     else 
50500       let fnAddOneC(mat ini_section$,ii_section$)
50520       let ii_pos_equal=pos(ii_line$(ii_item),"=")
50540       if ii_pos_equal=-1 then let ii_pos_equal=pos(ii_line$(ii_item),tab$) ! if no equals sign than use a tab instead
50560       let inir_field$=trim$(trim$(ii_line$(ii_item)(1:ii_pos_equal-1),'"'))
50580       let inir_value$=fn_iniRead$(inir_section$,inir_field$)
50600       if inir_value$='' then let inir_value$=""
50620       let inir_line$=ii_line$(ii_item)(1:ii_pos_equal)&' '&inir_value$
50640       gosub INIR_WRITE
50660       let fnAddOneC(mat ini_value$,ii_value$)
50680     end if  ! II_Line$(II_Item)(1:1)=     '['     /  '!'...   /     else 
50700   next ii_item
50720   close #inir_temp_handle: 
50780   fnCopy(inir_temp_file$,ini_file$)
50800   goto inirXIT
50820   ! 
50840   INIR_WRITE: ! r:
50860     if trim$(inir_line$)<>'' then 
50880       print #inir_temp_handle: inir_line$
50900     end if 
50920   return ! /r
50940   inirXIT: ! 
50960 fnend 

