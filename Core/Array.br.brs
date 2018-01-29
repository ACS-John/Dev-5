12000 def fn_setup
12020   if ~setup then
12040     setup=1
12060     library 'S:\Core\Library.br': fngethandle
12080   end if
12100 fnend
28200 def library fnarray_item_insert$(mat array$, insert_item$*1024, insert_item_number)
28300   fnarray_item_insert$=fn_array_item_insert$(mat array$, insert_item$, insert_item_number)
28400 fnend 
28500 def fn_array_item_insert$(mat array$, insert_item$*1024, insert_item_number)
28600   array_item_count=udim(mat array$)
28700   if insert_item_number>array_item_count then 
28800     mat array$(insert_item_number)
28900     array$(insert_item_number)=insert_item$
29000   else 
29100     array_item_count+=1
29200     mat array$(array_item_count)
29300     mat array$(insert_item_number+1:array_item_count)=array$(insert_item_number:array_item_count-1)
29400     array$(insert_item_number)=insert_item$
29500   end if  ! Insert_Item_Number>Array_Item_Count   /   else 
29600 fnend 
29700 def library fnarray_item_insert(mat array, insert_item, insert_item_number)
29800   fnarray_item_insert=fn_array_item_insert(mat array, insert_item, insert_item_number)
29900 fnend  ! fnARRAY_ITEM_INSERT
30000 def fn_array_item_insert(mat array, insert_item, insert_item_number)
30100   array_item_count=udim(mat array)
30200   if insert_item_number>array_item_count then 
30300     mat array(insert_item_number)
30400     array(insert_item_number)=insert_item
30500   else 
30600     array_item_count+=1
30700     mat array(array_item_count)
30800     mat array(insert_item_number+1:array_item_count)=array(insert_item_number:array_item_count-1)
30900     array(insert_item_number)=insert_item
31000   end if  ! Insert_Item_Number>Array_Item_Count   /   else 
31100 fnend 
31200 def library fnsrch_case_insensitive(mat srch_array$,srch_for$*256; srch_start_ele)
31300   ! if ~setup then let fn_setup
31400   fnsrch_case_insensitive=fn_srch_case_insensitive(mat srch_array$,srch_for$, srch_start_ele)
31500 fnend  ! fnsrch_case_insensitive
31600 def fn_srch_case_insensitive(mat srch_array$,srch_for$*256; srch_start_ele)
31700   srch_array_count=udim(mat srch_array$)
31800   srch_return=0
31900   do 
32000     srch_found=srch(mat srch_array$,'^'&srch_for$,srch_start_ele)
32100     if srch_found>0 and lwrc$(srch_for$)=lwrc$(srch_array$(srch_found)) then 
32200       srch_return=srch_found
32300     else if srch_found>0 then 
32400       srch_start_ele=srch_found+1
32500     else if srch_found<=0 then ! it's not there, anywhere - get outta here.
32600       srch_start_ele=srch_array_count+1
32700     end if 
32800   loop until srch_start_ele>srch_array_count or srch_return
32900   fn_srch_case_insensitive=srch_return
33000 fnend  ! fn_srch_case_insensitive
33100 def library fnAddOneN(mat add_to, one; skip_zeros, skip_dupes)
33200   fnAddOneN=fn_addOneN(mat add_to, one, skip_zeros, skip_dupes)
33300 fnend 
33400 def fn_addOneN(mat add_to, one; skip_zeros, skip_dupes)
33500   ! must dim an array to 0 before you can add a first item
33600   !    Mat Add_To - the array to add One item to
33700   !    One - the One item to add to Mat Add_To
33800   !    skip_Zeros - if =1 than only add One if One<>0
33900   !    skip_dupes - if =1 than only add One if One is not yet in Mat Add_To
34000   !    This function returns the number of items in the array after any add
34100   if skip_zeros=0 or (skip_zeros and one<>0) then 
34200     if skip_dupes=0 or (skip_dupes and srch(mat add_to,one)=-1) then 
34300       add_to_udim=udim(add_to) : mat add_to(add_to_udim+1) : add_to(add_to_udim+1)=one
34400     end if 
34500   end if 
34600   fn_addOneN=udim(mat add_to)
34700 fnend 
34800 def library fnAddOneC(mat add_to$, one$*2048; skip_blanks, skip_dupes)
34900   fnAddOneC=fn_addOneC(mat add_to$, one$, skip_blanks, skip_dupes)
35000 fnend 
35100 def fn_addOneC(mat add_to$, one$*2048; skip_blanks, skip_dupes)
35200   ! must dim an array to 0 before you can add a first item
35300   !    Mat Add_To$ - the array to add One$ item to
35400   !    One$ - the One$ item to add to Mat Add_To$
35500   !    skip_Blanks - if =1 than only add One$ if Trim$(One$)<>""
35600   !    skip_dupes - if =1 than only add One$ if One$ is not yet in Mat Add_To$
35700   !    This function returns the number of items in the array after any add
35800   if skip_blanks=0 or (skip_blanks and trim$(one$)<>"") then 
35900     if skip_dupes=0 or (skip_dupes and srch(mat add_to$,one$)<=0) then 
36000       add_to_udim=udim(mat add_to$) : mat add_to$(add_to_udim+1) : add_to$(add_to_udim+1)=one$
36100     end if 
36200   end if 
36300   fn_addOneC=udim(mat add_to$)
36400 fnend 
37000 def library fnCountMatchesC(mat arrayToSearch$,valueToMatch$)
37020   cmcReturn=0
37040   cmcIndex=0
37060   do
37080     cmcIndex=srch(mat arrayToSearch$,valueToMatch$,cmcIndex+1)
37100     if cmcIndex>0 then cmcReturn+=1
37120   loop while cmcIndex>0
37140   fnCountMatchesC=cmcReturn
37160 fnend
38000 def library fnCountMatchesN(mat arrayToSearch,valueToMatch)
38020   cmcReturn=0
38040   cmcIndex=0
38060   do
38080     cmcIndex=srch(mat arrayToSearch,valueToMatch,cmcIndex+1)
38100     if cmcIndex>0 then cmcReturn+=1
38120   loop while cmcIndex>0
38140   fnCountMatchesN=cmcReturn
38160 fnend
40000 def library fnArrayMax(mat arrayToSearch)
40020   ! returns index (not value), if multiple = maxes it returns the first one.
40040   amReturn=0
40060   amMax=-99999
40080   if udim(mat arrayToSearch)=0 then
40100     amReturn=0
40120   else
40140     amMax=arrayToSearch(1)
40150     amReturn=1
40160     for x=2 to udim(mat arrayToSearch)
40180       if arrayToSearch(x)>amMax then 
40200         amMax=arrayToSearch(x)
40220         amReturn=x
40240       end if
40260     nex x
40280   end if
40300   fnArrayMax=amReturn
40320 fnend
44000 def library fnFileTo2Arrays(ftaFile$*512,mat ftaArrayLeft$,mat ftaArrayRight$; ftaSkipFirstLine,ftaDelimiter$*1)
44010   if ~setup then let fn_setup
44020   dim ftaLine$*1024
44040   if ftaDelimiter$='' then let ftaDelimiter$='='
44060   open #hFta:=fngethandle: 'name='&ftaFile$,d,i
44080   mat ftaArrayLeft$ (0)
44100   mat ftaArrayRight$(0)
44120   for ftaSkipFirstLineItem=1 to ftaSkipFirstLine
44140     linput #hFta: ftaLine$ eof FtaEof
44160   nex ftaSkipFirstLineItem
44180   do
44200     linput #hFta: ftaLine$ eof FtaEof
44220     ftaPosDelim=pos(ftaLine$,ftaDelimiter$)
44240     if ftaPosDelim<=0 then 
44260       fn_addOneC(mat ftaArrayLeft$,trim$(ftaLine$))
44280       fn_addOneC(mat ftaArrayRight$,'')
44300     else
44320       fn_addOneC(mat ftaArrayLeft$,trim$(ftaLine$(1:ftaPosDelim-1)))
44340       fn_addOneC(mat ftaArrayRight$,trim$(ftaLine$(ftaPosDelim+1:len(ftaLine$))))
44360     end if
44380   loop
44400   FtaEof: !
44420   close #hFta:
44440   fnFileTo2Arrays=udim(mat ftaArrayLeft$)+ftaSkipFirstLine
44460 fnend
48000 def library fnArrayWasPassedC(mat array$)
48020   ! 1-D arrays only please
48040   on error goto awpcFinis
48060   awpcReturn=0
48080   awpcUdim=udim(mat array$)
48100   mat array$(awpcUdim+1)
48120   mat array$(awpcUdim)
48140   awpcReturn=1
48160   AwpcFinis: !
48180   on error System
48200   fnArrayWasPassedC=awpcReturn
48220 fnend
48240 def library fnArrayWasPassedN(mat arrayN)
48260   ! 1-D arrays only please
48280   on error goto AwpnFinis
48300   awpnReturn=0
48320   awpnUdim=udim(mat arrayN)
48340   mat arrayN(awpnUdim+1)
48360   mat arrayN(awpnUdim)
48380   awpnReturn=1
48400   AwpnFinis: !
48420   on error System
48440   fnArrayWasPassedN=awpnReturn
48460 fnend
52000 def library fnArrayEmpty(mat ae$)
52010   arrayEmptyReturn=1
52020   for aeItem=1 to udim(mat ae$)
52030     if trim$(ae$(aeItem))<>'' then goto AeNotEmpty
52040   nex aeItem
52050   goto AeFinis
52060   AeNotEmpty: !
52070   arrayEmptyReturn=0
52080   AeFinis: !
52090   fnArrayEmpty=arrayEmptyReturn
52100 fnend
