def fn_setup
  if ~setup then
    setup=1
    library 'S:\Core\Library.br': fngethandle
  end if
fnend
def library fnarray_item_insert$(mat array$, insert_item$*1024, insert_item_number)
  fnarray_item_insert$=fn_array_item_insert$(mat array$, insert_item$, insert_item_number)
fnend
def fn_array_item_insert$(mat array$, insert_item$*1024, insert_item_number)
  array_item_count=udim(mat array$)
  if insert_item_number>array_item_count then
    mat array$(insert_item_number)
    array$(insert_item_number)=insert_item$
  else
    array_item_count+=1
    mat array$(array_item_count)
    mat array$(insert_item_number+1:array_item_count)=array$(insert_item_number:array_item_count-1)
    array$(insert_item_number)=insert_item$
  end if  ! Insert_Item_Number>Array_Item_Count   /   else
fnend
def library fnarray_item_insert(mat array, insert_item, insert_item_number)
  fnarray_item_insert=fn_array_item_insert(mat array, insert_item, insert_item_number)
fnend  ! fnARRAY_ITEM_INSERT
def fn_array_item_insert(mat array, insert_item, insert_item_number)
  array_item_count=udim(mat array)
  if insert_item_number>array_item_count then
    mat array(insert_item_number)
    array(insert_item_number)=insert_item
  else
    array_item_count+=1
    mat array(array_item_count)
    mat array(insert_item_number+1:array_item_count)=array(insert_item_number:array_item_count-1)
    array(insert_item_number)=insert_item
  end if  ! Insert_Item_Number>Array_Item_Count   /   else
fnend
def library fnsrch_case_insensitive(mat srch_array$,srch_for$*256; srch_start_ele)
  ! if ~setup then let fn_setup
  fnsrch_case_insensitive=fn_srch_case_insensitive(mat srch_array$,srch_for$, srch_start_ele)
fnend  ! fnsrch_case_insensitive
def fn_srch_case_insensitive(mat srch_array$,srch_for$*256; srch_start_ele)
  srch_array_count=udim(mat srch_array$)
  srch_return=0
  do
    srch_found=srch(mat srch_array$,'^'&srch_for$,srch_start_ele)
    if srch_found>0 and lwrc$(srch_for$)=lwrc$(srch_array$(srch_found)) then
      srch_return=srch_found
    else if srch_found>0 then
      srch_start_ele=srch_found+1
    else if srch_found<=0 then ! it's not there, anywhere - get outta here.
      srch_start_ele=srch_array_count+1
    end if
  loop until srch_start_ele>srch_array_count or srch_return
  fn_srch_case_insensitive=srch_return
fnend  ! fn_srch_case_insensitive
def library fnAddOneN(mat add_to, one; skip_zeros, skip_dupes)
  fnAddOneN=fn_addOneN(mat add_to, one, skip_zeros, skip_dupes)
fnend
def fn_addOneN(mat add_to, one; skip_zeros, skip_dupes)
  ! must dim an array to 0 before you can add a first item
  !    Mat Add_To - the array to add One item to
  !    One - the One item to add to Mat Add_To
  !    skip_Zeros - if =1 than only add One if One<>0
  !    skip_dupes - if =1 than only add One if One is not yet in Mat Add_To
  !    This function returns the number of items in the array after any add
  if skip_zeros=0 or (skip_zeros and one<>0) then
    if skip_dupes=0 or (skip_dupes and srch(mat add_to,one)=-1) then
      add_to_udim=udim(mat add_to) : mat add_to(add_to_udim+1) : add_to(add_to_udim+1)=one
    end if
  end if
  fn_addOneN=udim(mat add_to)
fnend
def library fnAddOneC(mat add_to$, one$*2048; skip_blanks, skip_dupes)
  fnAddOneC=fn_addOneC(mat add_to$, one$, skip_blanks, skip_dupes)
fnend
def fn_addOneC(mat add_to$, one$*2048; skip_blanks, skip_dupes)
  ! must dim an array to 0 before you can add a first item
  !    Mat Add_To$ - the array to add One$ item to
  !    One$ - the One$ item to add to Mat Add_To$
  !    skip_Blanks - if =1 than only add One$ if Trim$(One$)<>""
  !    skip_dupes - if =1 than only add One$ if One$ is not yet in Mat Add_To$
  !    This function returns the number of items in the array after any add
  if skip_blanks=0 or (skip_blanks and trim$(one$)<>"") then
    if skip_dupes=0 or (skip_dupes and srch(mat add_to$,one$)<=0) then
      add_to_udim=udim(mat add_to$) : mat add_to$(add_to_udim+1) : add_to$(add_to_udim+1)=one$
    end if
  end if
  fn_addOneC=udim(mat add_to$)
fnend
def library fnCountMatchesC(mat arrayToSearch$,valueToMatch$*256)
  cmcReturn=0
  cmcIndex=0
  do
    cmcIndex=srch(mat arrayToSearch$,valueToMatch$,cmcIndex+1)
    if cmcIndex>0 then cmcReturn+=1
  loop while cmcIndex>0
  fnCountMatchesC=cmcReturn
fnend
def library fnCountMatchesN(mat arrayToSearch,valueToMatch)
  cmcReturn=0
  cmcIndex=0
  do
    cmcIndex=srch(mat arrayToSearch,valueToMatch,cmcIndex+1)
    if cmcIndex>0 then cmcReturn+=1
  loop while cmcIndex>0
  fnCountMatchesN=cmcReturn
fnend
def library fnArrayMax(mat arrayToSearch)
  ! returns index (not value), if multiple = maxes it returns the first one.
  amReturn=0
  amMax=-99999
  if udim(mat arrayToSearch)=0 then
    amReturn=0
  else
    amMax=arrayToSearch(1)
    amReturn=1
    for x=2 to udim(mat arrayToSearch)
      if arrayToSearch(x)>amMax then
        amMax=arrayToSearch(x)
        amReturn=x
      end if
    nex x
  end if
  fnArrayMax=amReturn
fnend
def library fnFileTo2Arrays(ftaFile$*512,mat ftaArrayLeft$,mat ftaArrayRight$; ftaSkipFirstLine,ftaDelimiter$*1)
  if ~setup then let fn_setup
  dim ftaLine$*1024
  if ftaDelimiter$='' then ftaDelimiter$='='
  open #hFta:=fngethandle: 'name='&ftaFile$,d,i
  mat ftaArrayLeft$ (0)
  mat ftaArrayRight$(0)
  for ftaSkipFirstLineItem=1 to ftaSkipFirstLine
    linput #hFta: ftaLine$ eof FtaEof
  nex ftaSkipFirstLineItem
  do
    linput #hFta: ftaLine$ eof FtaEof
    ftaPosDelim=pos(ftaLine$,ftaDelimiter$)
    if ftaPosDelim<=0 then
      fn_addOneC(mat ftaArrayLeft$,trim$(ftaLine$))
      fn_addOneC(mat ftaArrayRight$,'')
    else
      fn_addOneC(mat ftaArrayLeft$,trim$(ftaLine$(1:ftaPosDelim-1)))
      fn_addOneC(mat ftaArrayRight$,trim$(ftaLine$(ftaPosDelim+1:len(ftaLine$))))
    end if
  loop
  FtaEof: !
  close #hFta:
  fnFileTo2Arrays=udim(mat ftaArrayLeft$)+ftaSkipFirstLine
fnend
def library fnArrayWasPassedC(mat array$)
  ! 1-D arrays only please
  on error goto AwpcFinis
  awpcReturn=0
  awpcUdim=udim(mat array$)
  mat array$(awpcUdim+1)
  mat array$(awpcUdim)
  awpcReturn=1
  AwpcFinis: !
  on error System
  fnArrayWasPassedC=awpcReturn
fnend
def library fnArrayWasPassedN(mat arrayN)
  ! 1-D arrays only please
  on error goto AwpnFinis
  awpnReturn=0
  awpnUdim=udim(mat arrayN)
  mat arrayN(awpnUdim+1)
  mat arrayN(awpnUdim)
  awpnReturn=1
  AwpnFinis: !
  on error System
  fnArrayWasPassedN=awpnReturn
fnend
def library fnArrayEmpty(mat ae$)
  arrayEmptyReturn=1
  for aeItem=1 to udim(mat ae$)
    if trim$(ae$(aeItem))<>'' then goto AeNotEmpty
  nex aeItem
  goto AeFinis
  AeNotEmpty: !
  arrayEmptyReturn=0
  AeFinis: !
  fnArrayEmpty=arrayEmptyReturn
fnend
def library fnRead1column(mat r1Return$,r1File$*256,r1ColumnNumber,r1Delimiter$)
  if ~setup then let fn_setup
  dim r1Line$*256
  dim r1LineItem$(0)*128
  mat r1Return$(0)
  open #hr1:=fngethandle: 'name='&r1File$,d,input ioerr EoR1
  linput #hr1: r1Line$ eof EoR1 ! just consume the headings
  do
    linput #hr1: r1Line$ eof EoR1
    str2mat(r1Line$,mat r1LineItem$,r1Delimiter$)
    if udim(mat r1LineItem$)=>r1ColumnNumber then
      fn_addonec(mat r1Return$,r1LineItem$(r1ColumnNumber))
    end if
  loop
  close #hr1:
  EoR1: !
  fnRead1column=udim(mat r1Return$)
fnend
def library fnRead2column(mat r2Return1$,mat r2Return2$,r2File$*256,r2ColumnNumber1,r2ColumnNumber2,r2Delimiter$)

  if ~setup then let fn_setup
  dim r2Line$*256
  dim r2LineItem$(0)*128
  mat r2Return1$(0)
  mat r2Return2$(0)
  open #hr2:=fngethandle: 'name='&r2File$,d,input ioerr Eor2
  linput #hr2: r2Line$ eof Eor2 ! just consume the headings
  do
    linput #hr2: r2Line$ eof Eor2
    str2mat(r2Line$,mat r2LineItem$,r2Delimiter$)
    if udim(mat r2LineItem$)=>r2ColumnNumber1 then
      fn_addonec(mat r2Return1$,r2LineItem$(r2ColumnNumber1))
    else
      fn_addonec(mat r2Return1$,'')
    end if
    if udim(mat r2LineItem$)=>r2ColumnNumber2 then
      fn_addonec(mat r2Return2$,r2LineItem$(r2ColumnNumber2))
    else
      fn_addonec(mat r2Return2$,'')
    end if
  loop
  close #hr2:
  Eor2: !
  fnRead2column=udim(mat r2Return1$)
fnend
def library fnRead3column(mat r3Return1$,mat r3Return2$,mat r3Return3$,r3File$*256,r3ColumnNumber1,r3ColumnNumber2,r3ColumnNumber3,r3Delimiter$)
  if ~setup then let fn_setup
  dim r3Line$*256
  dim r3LineItem$(0)*128
  mat r3Return1$(0)
  mat r3Return2$(0)
  open #hr3:=fngethandle: 'name='&r3File$,d,input ioerr Eor3
  linput #hr3: r3Line$ eof Eor3 ! just consume the headings
  do
    linput #hr3: r3Line$ eof Eor3
    str2mat(r3Line$,mat r3LineItem$,r3Delimiter$)
    if udim(mat r3LineItem$)<max(r3ColumnNumber3,r3ColumnNumber2,r3ColumnNumber1) then mat r3LineItem$(max(r3ColumnNumber3,r3ColumnNumber2,r3ColumnNumber1))
    fn_addonec(mat r3Return1$,r3LineItem$(r3ColumnNumber1))
    fn_addonec(mat r3Return2$,r3LineItem$(r3ColumnNumber2))
    fn_addonec(mat r3Return3$,r3LineItem$(r3ColumnNumber3))
  loop
  close #hr3:
  Eor3: !
  fnRead3column=udim(mat r3Return1$)
fnend
def library fnRead4column(mat r4Return1$,mat r4Return2$,mat r4Return3$,mat r4Return4$,r4File$*256,r4ColumnNumber1,r4ColumnNumber2,r4ColumnNumber3,r4ColumnNumber4,r4Delimiter$)
  if ~setup then let fn_setup
  dim r4Line$*256
  dim r4LineItem$(0)*128
  mat r4Return1$(0)
  mat r4Return2$(0)
  open #hr4:=fngethandle: 'name='&r4File$,d,input ioerr Eor4
  linput #hr4: r4Line$ eof Eor4 ! just consume the headings
  do
    linput #hr4: r4Line$ eof Eor4
    str2mat(r4Line$,mat r4LineItem$,r4Delimiter$)
    if udim(mat r4LineItem$)<max(r4ColumnNumber4,r4ColumnNumber2,r4ColumnNumber1) then mat r4LineItem$(max(r4ColumnNumber4,r4ColumnNumber2,r4ColumnNumber1))
    fn_addonec(mat r4Return1$,r4LineItem$(r4ColumnNumber1))
    fn_addonec(mat r4Return2$,r4LineItem$(r4ColumnNumber2))
    fn_addonec(mat r4Return3$,r4LineItem$(r4ColumnNumber3))
    fn_addonec(mat r4Return4$,r4LineItem$(r4ColumnNumber4))
  loop
  close #hr4:
  Eor4: !
  fnRead4column=udim(mat r4Return1$)
fnend

def library fnRead2columnFixedWidth(mat r2fReturn1$,mat r2fReturn2$,r2fFile$*256,r2fColumn1Width)
  ! for reading files into two arrays where the first X positions in the file are column1 and the rest of the line is column2
  ! a 1 character delimiter between the two columns is assumed.
  ! no headings is assumed
  if ~setup then let fn_setup
  dim r2fLine$*256
  mat r2fReturn1$(0)
  mat r2fReturn2$(0)
  open #hr2f:=fngethandle: 'name='&r2fFile$,d,input ioerr Eor2f
  do
    linput #hr2f: r2fLine$ eof Eor2f
    fn_addonec(mat r2fReturn1$,r2fLine$(1:r2fColumn1Width))
    fn_addonec(mat r2fReturn2$,r2fLine$(r2fColumn1Width+2:inf))
  loop
  close #hr2f:
  Eor2f: !
  fnRead2columnFixedWidth=udim(mat r2fReturn1$)
fnend

def library fnArrayAddC(mat array_combined$,mat arrayPartOne$,mat arrayPartTwo$)
	array_part_one_udim=udim(arrayPartOne$)
	array_part_two_udim=udim(arrayPartTwo$)
	array_combined_udim=array_part_one_udim+array_part_two_udim
	mat array_combined$(array_combined_udim)
	if array_part_one_udim=0 then
		mat array_combined$=arrayPartTwo$
	else if array_part_two_udim=0 then
		mat array_combined$=arrayPartOne$
	else if array_part_one_udim>0 and array_part_two_udim>0 then
		mat array_combined$(1:array_part_one_udim)=arrayPartOne$(1:array_part_one_udim)
		mat array_combined$(array_part_one_udim+1:array_combined_udim)=arrayPartTwo$(1:array_part_two_udim)
	end if
fnend

def library fnarrayaddn(mat array_combined,mat array_part_one,mat array_part_two)
	let array_part_one_udim=udim(array_part_one)
	let array_part_two_udim=udim(array_part_two)
	let array_combined_udim=array_part_one_udim+array_part_two_udim
	mat array_combined(array_combined_udim)
	if array_part_one_udim=0 then 
		mat array_combined=array_part_two
	else if array_part_two_udim=0 then 
		mat array_combined=array_part_one
	else if array_part_one_udim>0 and array_part_two_udim>0 then 
		mat array_combined(1:array_part_one_udim)=array_part_one(1:array_part_one_udim)
		mat array_combined(array_part_one_udim+1:array_combined_udim)=array_part_two(1:array_part_two_udim)
	end if 
fnend 
def library fnArrayReverseC(mat in$,mat out$)
	let in_udim=udim(in$)
	mat out$(in_udim)
	for in_item=1 to in_udim
		let out$(in_udim-in_item+1)=in$(in_item)
	next in_item
fnend 
def library fnChrCount(String_To_Search$*10480,Chr_To_Count$*1)
	Chr_Count=0
	DO 
		Cc_Pos_Chr=Pos(String_To_Search$,Chr_To_Count$,Cc_Pos_Chr+1)
		IF Cc_Pos_Chr>0 THEN Chr_Count+=1
	LOOP Until Cc_Pos_Chr<=0
	fnChrCount=Chr_Count
fnend